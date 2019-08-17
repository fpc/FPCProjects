unit cnocStackBinaryListener;

{ CnocStack's server implementation for the binary-protocol

  Copyright (C) 2019 Joost van der Sluis (CNOC) joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Sysutils,
  StrUtils,
  ssockets,
  syncobjs,
  Sockets,
  fgl,
  TypInfo,
  TLoggerUnit,
  TLevelUnit,
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  cnocStackBinaryReaderThread,
  cnocStackInternalStack,
  cnocStackInternalSenderHandler;

Type
  TcnocStackBinaryClientConnectionList = specialize TFPGObjectList<TThread>;

  { TcnocStackBinaryListener }

  TcnocStackBinaryListener = class(TThread)
  private
    procedure Handleconnect(Sender: Tobject; Data: Tsocketstream);
  protected
    FHost: string;
    FPort: Word;
    FServer: TInetServer;
    FClientConnectionList: TcnocStackBinaryClientConnectionList;
    FStackList: TcnocStackInternalStackList;
    FSenderHandler: TcnocStackInternalSenderHandler;
    FSenderHash: TcnocStackSenderHash;
  public
    constructor Create(Host: string; Port: Word; StackList: TcnocStackInternalStackList; SenderHandler: TcnocStackInternalSenderHandler);
    destructor Destroy; Override;
    procedure Execute; Override;
  end;

implementation

type

  { TBinaryClientConnectionReader }

  TBinaryClientConnection = class;
  TBinaryClientConnectionReader = class(TcnocStackBinaryReaderThread)
  private
    FClientConnection: TBinaryClientConnection;
    FReceivedMessagesQueue: TcnocStackMessageTypesQueue;
  protected
    procedure HandleReceivedMessage(Message: PcnocStackMessage); override;
  public
    constructor Create(Stream: TSocketStream; ClientConnection: TBinaryClientConnection);
    destructor Destroy; override;
    procedure Execute; override;
    property ReceivedMessagesQueue: TcnocStackMessageTypesQueue read FReceivedMessagesQueue;
  end;

  { TBinaryClientConnection }

  TBinaryClientConnection = class(TThread, IStackListener, IStackSender)
  private
    FStream: TSocketStream;
    FStackList: TcnocStackInternalStackList;
    FWakeupEvent: PRTLEvent;
    FReader: TBinaryClientConnectionReader;
    FSubscribedStackList: TcnocStackInternalStackList;
    FSenderHandler: TcnocStackInternalSenderHandler;
    FSenderHash: TcnocStackSenderHash;
    FResponseMessagesQueue: TcnocStackMessageTypesQueue;

    FLogSockAddrText: string;
    function FlagsToString(Flags: TcnocStackFlags): string;
  protected
    procedure NotifyNewReaderMessage();
    procedure SendMessage(Message: PcnocStackMessage);
    procedure SendDirectAck(ErrCode: TcnocStackErrorCodes; StackName: string; AStoryId: LongWord; AResponseData: TBytes);
    function CheckForIncomingMessage: Boolean;
    function CheckForOutgoingMessage: Boolean;
    function CheckForIncomingResponse: Boolean;
  public
    constructor Create(Stream: TSocketStream; StackList: TcnocStackInternalStackList; SenderHandler: TcnocStackInternalSenderHandler);
    destructor Destroy; override;
    procedure Execute; Override;
    procedure NotifyNewMessage;
    function SendControlMessage(const AItem: PcnocStackMessage): TWaitResult;
  end;

{ TBinaryClientConnectionReade }

constructor TBinaryClientConnectionReader.Create(Stream: TSocketStream; ClientConnection: TBinaryClientConnection);
begin
  inherited Create(Stream);
  FClientConnection := ClientConnection;
  FReceivedMessagesQueue := TcnocStackMessageTypesQueue.create();
end;

procedure TBinaryClientConnectionReader.Execute;
begin
  inherited;
  // There is no socket anymore. So it is not possible for a client to determine
  // if a message is handled correctly or not (no ability to ACK).
  // In this case it is better to ignore any pending messages.
  FReceivedMessagesQueue.DoShutDown;
  FClientConnection.Terminate;
  FClientConnection.NotifyNewReaderMessage;
end;

destructor TBinaryClientConnectionReader.Destroy;
begin
  FReceivedMessagesQueue.Free;
  inherited Destroy;
end;

procedure TBinaryClientConnectionReader.HandleReceivedMessage(Message: PcnocStackMessage);
begin
  if FReceivedMessagesQueue.PushItem(Message) <> wrSignaled then
    raise Exception.Create('Not able to push message to stack?');
  FClientConnection.NotifyNewReaderMessage;
end;

{ TBinaryClientConnection }

constructor TBinaryClientConnection.Create(Stream: TSocketStream; StackList: TcnocStackInternalStackList;
  SenderHandler: TcnocStackInternalSenderHandler);
begin
  FStream := Stream;
  FStackList := StackList;
  FWakeupEvent := RTLEventCreate;
  FSubscribedStackList := TcnocStackInternalStackList.Create([]);

  FLogSockAddrText := SockAddToLogText(FStream.RemoteAddress);

  FSenderHandler := SenderHandler;
  FSenderHash := FSenderHandler.RegisterSender(Self);

  FResponseMessagesQueue := TcnocStackMessageTypesQueue.create();

  FreeOnTerminate := True;

  Inherited Create(False);
end;

procedure TBinaryClientConnection.Execute;
var
  IncomingMsg: PcnocStackMessage;
  wr: TWaitResult;
  Stack: tcnocStackinternalstack;
  HasHandledMessage: Boolean;
begin
  FReader := TBinaryClientConnectionReader.Create(FStream, Self);
  try
    repeat
      repeat
        RTLEventResetEvent(FWakeupEvent);
        HasHandledMessage := False;
        if not Terminated and CheckForIncomingMessage then
          HasHandledMessage := True;
        if not Terminated and CheckForOutgoingMessage then
          HasHandledMessage := True;
        if not Terminated and CheckForIncomingResponse then
          HasHandledMessage := True;
      until terminated or not HasHandledMessage;

      if not Terminated then
        RTLEventWaitFor(FWakeupEvent);
    until Terminated;

    for Stack in FSubscribedStackList.Values do
      Stack.RemoveListener(Self);

    repeat until not CheckForIncomingMessage;

  finally
    FReader.Terminate;

    for Stack in FSubscribedStackList.Values do
      Stack.RemoveListener(Self);

    FReader.WaitFor;
    FReader.Free;
  end;
end;

procedure TBinaryClientConnection.NotifyNewMessage;
begin
  RTLEventSetEvent(FWakeupEvent);
end;

procedure TBinaryClientConnection.NotifyNewReaderMessage();
begin
  RTLEventSetEvent(FWakeupEvent);
end;

destructor TBinaryClientConnection.Destroy;
begin
  RTLEventDestroy(FWakeupEvent);
  FSubscribedStackList.Free;
  FResponseMessagesQueue.Free;
  FSenderHandler.UnregisterSender(FSenderHash);
  inherited Destroy;
end;

procedure TBinaryClientConnection.SendMessage(Message: PcnocStackMessage);
begin
  if TLevelUnit.TRACE.IsGreaterOrEqual(TLogger.GetInstance.GetLevel()) then
    TLogger.GetInstance.Trace(PadRight('TCP: (' + FLogSockAddrText + ')', 28) + PadRight(ScnocStackMessageType[Message^.Header.MessageType], 16) + ' <= ' + PadRight(Message^.GetStackName,15) +
      GetMessageLogText(Message));
  try
    FStream.WriteBuffer(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
  except on e: Exception do
    begin
    TLogger.GetInstance.Warn('TCP: error while writing message, closing connection: ' + E.Message);
    Terminate;
    end;
  end;
end;

procedure TBinaryClientConnection.SendDirectAck(ErrCode: TcnocStackErrorCodes; StackName: string; AStoryId: LongWord; AResponseData: TBytes);
var
  M: PcnocStackMessage;
  MT: TcnocStackMessageType;
  Flags: TcnocStackFlags;
begin
  Flags := [];
  if ErrCode<>ecNone then
    MT := smtNack
  else
    MT := smtAck;
  M := TcnocStackMessage.CreateMessage(MT, ''{StackName}, Flags, AStoryId, nil, nil, nil, AResponseData);
  try
    M^.Header.ErrorCode := LongWord(ErrCode);
    SendMessage(M);
  finally
    TcnocStackMessage.DestroyMessage(M);
  end;
end;

function TBinaryClientConnection.SendControlMessage(const AItem: PcnocStackMessage): TWaitResult;
begin
  Result := FResponseMessagesQueue.PushItem(AItem);
  RTLEventSetEvent(FWakeupEvent);
end;

function TBinaryClientConnection.CheckForIncomingMessage: Boolean;
var
  IncomingMsg: PcnocStackMessage;
  ErrCode: TcnocStackErrorCodes;
  StackName: String;
  Stack: TcnocStackInternalStack;
begin
  Result := False;
  if FReader.FReceivedMessagesQueue.PopItemTimeout(IncomingMsg, 0)=wrSignaled then
    begin
    Result := True;
    try
      TLogger.GetInstance.Debug(Format('Received message %s, flags: [%s]', [ScnocStackMessageType[IncomingMsg^.Header.MessageType], FlagsToString(IncomingMsg^.Header.Flags)]));
      // Check validity of message
      ErrCode := ecNone;
      case IncomingMsg^.Header.MessageType of
        smtToStack:
          begin
          StackName := IncomingMsg^.GetStackName;
          if StackName='' then
            ErrCode := ecEmptyStackName
          else
            begin
            if not FStackList.TryGetValue(StackName, Stack) then
              ErrCode := ecStackDoesNotExist
            else
              begin
              if sfExpectDirectResponse in IncomingMsg^.Header.Flags then
                IncomingMsg^.Header.RoutingInfo := FSenderHash;

              case Stack.PushItem(IncomingMsg) of
                wrTimeout: ErrCode := ecPushMessageTimeout;
                wrAbandoned: ErrCode := ecPushMessageAbandoned;
                wrError: ErrCode := ecPushMessageError;
              end;
              end;
            end;
          end;
        smtResponseToStack:
          begin
          ErrCode := FSenderHandler.SendMessage(IncomingMsg);
          end;
        smtSubscribeToStack:
          begin
          StackName := IncomingMsg^.GetStackName;
          if StackName='' then
            ErrCode := ecEmptyStackName
          else
            begin
            if not FStackList.TryGetValue(StackName, Stack) then
              ErrCode := ecStackDoesNotExist
            else
              begin
              Stack.AddListener(Self);
              FSubscribedStackList.Add(StackName, Stack);
              end;
            end;
          end
      else
        ErrCode := ecInvalidMessageType;
      end;
    except on E: Exception do
      begin
      TLogger.GetInstance.Error('BinaryClientConnection: Exception while evaluating incoming message: ' + E.Message);
      ErrCode := ecException;
      end;
    end;
    if ErrCode <> ecNone then
      TLogger.GetInstance.Warn(cnocStackErrorMessages[ErrCode]);
    // Send (n)ack when requested
    if sfRequestDirectAck in IncomingMsg^.Header.Flags then
      SendDirectAck(ErrCode, IncomingMsg^.GetStackName, IncomingMsg^.Header.StoryId, IncomingMsg^.GetResponseData);
    end;

end;

function TBinaryClientConnection.CheckForOutgoingMessage: Boolean;
var
  wr: TWaitResult;
  Stack: tcnocStackinternalstack;
  IncomingMsg: PcnocStackMessage;
begin
  Result := False;
  for Stack in FSubscribedStackList.Values do
  begin
    wr := Stack.PopItem(IncomingMsg);
    if wr = wrSignaled then
      begin
      Result := True;
      SendMessage(IncomingMsg);
      IncomingMsg^.DestroyMessage(IncomingMsg);
      end;
  end;
end;

function TBinaryClientConnection.CheckForIncomingResponse: Boolean;
var
  IncomingMsg: PcnocStackMessage;
begin
  Result := False;
  // Check for incoming responses
  if FResponseMessagesQueue.PopItemTimeout(IncomingMsg, 0)=wrSignaled then
    begin
    Result := True;
    SendMessage(IncomingMsg);
    end;
end;

function TBinaryClientConnection.FlagsToString(Flags: TcnocStackFlags): string;
var
  Flag: TcnocStackFlag;
begin
  Result := '';
  for Flag in Flags do
    Result := Result + ',' + GetEnumName(TypeInfo(Flag), Ord(Flag));
  Result := Copy(Result,2);
end;

{ TcnocStackBinaryListener }

constructor TcnocStackBinaryListener.Create(Host: string; Port: Word; StackList: TcnocStackInternalStackList;
  SenderHandler: TcnocStackInternalSenderHandler);
begin
  FHost := Host;
  FPort := Port;
  FStackList := StackList;
  FSenderHandler := SenderHandler;
  Inherited Create(False);
end;

destructor TcnocStackBinaryListener.Destroy;
begin
  inherited Destroy;
end;

procedure TcnocStackBinaryListener.Execute;
var
  i: Integer;
begin
  FServer := TInetServer.Create(FHost, FPort);
  try
    // Set TCP_NODELAY, to avoid a latency of 40 mseconds on small
    // packat sizes.
    i := 1;
    FServer.SetSockopt(IPPROTO_TCP, TCP_NODELAY, integer(i), SizeOf(i));

    FServer.OnConnect := @Handleconnect;
    FServer.ReuseAddress := True;
    FServer.StartAccepting;
  finally
    FServer.Free;
  end;
end;

procedure TcnocStackBinaryListener.Handleconnect(Sender: Tobject; Data: TSocketStream);
begin
  TBinaryClientConnection.Create(Data, FStackList, FSenderHandler);
End;

end.

