unit cnocStackBinaryClient;

{ Client-library for CnocStack

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
{$interfaces corba}

interface

uses
  Classes,
  SysUtils,
  ssockets,
  StrUtils,
  syncobjs,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  sockets,
  TLoggerUnit,
  TLevelUnit,
  cnocStackBinaryReaderThread,
  cnocStackMessageTypes,
  cnocStackErrorCodes;

type

  { TcnocStackBinaryClientReader }

  TcnocStackBinaryClient = class;
  TcnocStackBinaryClientReader = class(TcnocStackBinaryReaderThread)
  private
    FBinaryClient: TcnocStackBinaryClient;
    FReceivedMessagesQueue: TcnocStackMessageTypesQueue;
    FReceivedControlMessagesQueue: TcnocStackMessageTypesQueue;
  protected
    procedure HandleReceivedMessage(Message: PcnocStackMessage); override;
    procedure HandleConnectionLost; override;
  public
    constructor Create(Stream: TSocketStream; BinaryClient: TcnocStackBinaryClient);
    destructor Destroy; override;
    property ReceivedMessagesQueue: TcnocStackMessageTypesQueue read FReceivedMessagesQueue;
    property ReceivedControlMessagesQueue: TcnocStackMessageTypesQueue read FReceivedControlMessagesQueue;
  end;

  { TcnocStackBinaryClient }

  TcnocStackBinaryClient = class
  private
    FReaderThread: TcnocStackBinaryClientReader;
    FSocket: TInetSocket;
    FHost: string;
    FPort: Word;
    FLogger: TLogger;
    FLogSockAddrText: string;
    function IntSendMessage(Message: PcnocStackMessage; out Response: PcnocStackMessage): TcnocStackErrorCodes;
    procedure CleanupConnection;
  public
    constructor Create(Host: string; Port: Word);
    destructor Destroy; override;
    procedure Connect();
    function IsConnected: Boolean;

    function SendResponseMessage(
      Message: PcnocStackMessage;
      Flags: TcnocStackFlags;
      Data: TBytes;
      ExtAccessKeys: TStringArray;
      IntAccessKeys: TStringArray;
      ResponseData: TBytes;
      out Response: PcnocStackMessage): TcnocStackErrorCodes; overload;

    function SendResponseMessage(
      Message: PcnocStackMessage;
      Flags: TcnocStackFlags;
      Data: TBytes;
      ExtAccessKeys: TStringArray = nil;
      IntAccessKeys: TStringArray = nil;
      ResponseData: TBytes = nil): TcnocStackErrorCodes; overload;

    function SendMessage(
      MessagType: TcnocStackMessageType;
      StackName: string;
      Flags: TcnocStackFlags;
      StoryId: LongWord;
      Data: TBytes;
      ExtAccessKeys: TStringArray;
      IntAccessKeys: TStringArray;
      ResponseData: TBytes;
      out Response: PcnocStackMessage): TcnocStackErrorCodes; overload;
    function SendMessage(
      MessagType: TcnocStackMessageType;
      StackName: string;
      Flags: TcnocStackFlags;
      StoryId: LongWord;
      Data: TBytes;
      ExtAccessKeys: TStringArray = nil;
      IntAccessKeys: TStringArray = nil;
      ResponseData: TBytes = nil): TcnocStackErrorCodes; overload;
    function SendMessage(
      MessagType: TcnocStackMessageType;
      StackName: string;
      Flags: TcnocStackFlags;
      StoryId: LongWord;
      Data: string;
      ExtAccessKeys: TStringArray = nil;
      IntAccessKeys: TStringArray = nil;
      ResponseData: TBytes = nil): TcnocStackErrorCodes; overload;


    function SendMessage(
      StackName: string;
      Data: TBytes;
      out ResponseData: TBytes;
      ExtAccessKeys: TStringArray = nil;
      AStoryId: LongWord = 0): TcnocStackErrorCodes; overload;
    function SendMessage(
      StackName: string;
      Data: string;
      out ResponseData: string;
      ExtAccessKeys: TStringArray = nil;
      AStoryId: LongWord = 0): TcnocStackErrorCodes; overload;


    function WaitForMessage(out Message: PcnocStackMessage; const Timeout: Integer = 0): TWaitResult;
    function SubscribeToStack(const StackName: string): TcnocStackErrorCodes;
  end;

implementation


{ TcnocStackBinaryClientReader }

constructor TcnocStackBinaryClientReader.Create(Stream: TSocketStream; BinaryClient: TcnocStackBinaryClient);
begin
  inherited Create(Stream);
  FBinaryClient := BinaryClient;
  FReceivedMessagesQueue := TcnocStackMessageTypesQueue.create(10,INFINITE,100);
  FReceivedControlMessagesQueue := TcnocStackMessageTypesQueue.create(10,INFINITE,100);
end;

destructor TcnocStackBinaryClientReader.Destroy;
begin
  FReceivedControlMessagesQueue.Free;
  FReceivedMessagesQueue.Free;
  inherited Destroy;
end;

procedure TcnocStackBinaryClientReader.HandleReceivedMessage(Message: PcnocStackMessage);
begin
  if Message^.Header.MessageType in [smtFromStack] then
    begin
    if FReceivedMessagesQueue.PushItem(Message) <> wrSignaled then
      raise Exception.Create('Not able to push message to stack?');
    end
  else
    begin
    if FReceivedControlMessagesQueue.PushItem(Message) <> wrSignaled then
      raise Exception.Create('Not able to push message to stack?');
    end;
end;

procedure TcnocStackBinaryClientReader.HandleConnectionLost;
begin
  inherited HandleConnectionLost;
  FReceivedControlMessagesQueue.DoShutDown;
  FReceivedMessagesQueue.DoShutDown;
end;

{ TcnocStackBinaryClient }

constructor TcnocStackBinaryClient.Create(Host: string; Port: Word);
begin
  FHost := Host;
  FPort := Port;
  FLogger := TLogger.GetInstance;
end;

procedure TcnocStackBinaryClient.Connect();
begin
  if IsConnected then
    Raise Exception.Create('Already connected');
  FSocket := TInetSocket.Create(FHost, FPort, 5000);
  {$IFDEF Linux}
  FSocket.WriteFlags := MSG_NOSIGNAL;
  {$ENDIF}
  FLogSockAddrText := SockAddToLogText(FSocket.RemoteAddress, FSocket.LocalAddress);
  FReaderThread := TcnocStackBinaryClientReader.Create(FSocket, Self);
end;

destructor TcnocStackBinaryClient.Destroy;
begin
  if Assigned(FReaderThread) then
    begin
    FReaderThread.Terminate;
    fpshutdown(FSocket.Handle, SHUT_RDWR);

    CleanupConnection;
    end;
  inherited Destroy;
end;

function TcnocStackBinaryClient.SendMessage(MessagType: TcnocStackMessageType; StackName: string;
  Flags: TcnocStackFlags; StoryId: LongWord; Data: TBytes; ExtAccessKeys: TStringArray;
  IntAccessKeys: TStringArray; ResponseData: TBytes; out Response: PcnocStackMessage): TcnocStackErrorCodes;
var
  Message: PcnocStackMessage;
begin
  Response := nil;
  if not Assigned(FSocket) then
    Result := ecNotConnected
  else
    begin
    Message := TcnocStackMessage.CreateMessage(
      MessagType,
      StackName,
      Flags,
      StoryId,
      Data,
      ExtAccessKeys,
      IntAccessKeys,
      ResponseData);
    try
      Result := IntSendMessage(Message, Response);
    finally
      TcnocStackMessage.DestroyMessage(Message);
    end;
    end;
end;

function TcnocStackBinaryClient.IntSendMessage(Message: PcnocStackMessage; out Response: PcnocStackMessage): TcnocStackErrorCodes;
var
  ControlMessage: PcnocStackMessage;
  wr: TWaitResult;
begin
  Response := nil;
  if not Assigned(FSocket) then
    Result := ecNotConnected
  else
    begin
    Result := ecNone;
    if TLevelUnit.TRACE.IsGreaterOrEqual(FLogger.GetLevel()) then
      FLogger.Trace(FLogSockAddrText + PadRight(ScnocStackMessageType[Message^.Header.MessageType], 16) + ' <= ' + PadRight(Message^.GetStackName,15) +
        GetMessageLogText(Message));
    try
      FSocket.WriteBuffer(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
    except
      Result := ecFailedToSendData;
      // In principle it would be possible to check
      // for FReaderThread.ReceivedMessagesQueue.Shutdown or FReaderThread.Terminated.
      // But it could be the case that our write triggered the disconnect. Then
      // the FReaderThread needs some time to process it.
      if WaitForThreadTerminate(FReaderThread.Handle, 10) = 0 then
        begin
        // The connection is gone.
        CleanupConnection;
        end;
      Exit;
    end;

    if sfRequestDirectAck in Message^.Header.Flags then
      begin
      wr := FReaderThread.ReceivedControlMessagesQueue.PopItemTimeout(ControlMessage, 1000);
      if wr <> wrSignaled then
        begin
        case wr of
          wrTimeout: Result := ecTimeoutWhileWaitingForDirectAck;
          wrAbandoned: Result := ecAbandonedWhileWaitingForDirectAck;
          wrError: Result := ecErrorWhileWaitingForDirectAck;
        end;
        end
      else
        begin
        case ControlMessage^.Header.MessageType of
          smtAck: Result := ecNone;
          smtNack: Result := ControlMessage^.GetErrorCode;
        else
          Result := ecInvalidControlFlow;
        end;
        TcnocStackMessage.DestroyMessage(ControlMessage);
        end;
      end
    else
      Result := ecNone;

    if result = ecNone then
      begin
      if sfExpectDirectResponse in Message^.Header.Flags then
        begin
        wr := FReaderThread.ReceivedControlMessagesQueue.PopItemTimeout(ControlMessage, 1000);
        if wr <> wrSignaled then
          begin
          case wr of
            wrTimeout: Result := ecTimeoutWhileWaitingForDirectResponse;
            wrAbandoned: Result := ecAbandonedWhileWaitingForDirectResponse;
            wrError: Result := ecErrorWhileWaitingForDirectResponse;
          end;
          end
        else
          begin
          case ControlMessage^.Header.MessageType of
            smtDirectResponse:
              begin
              Result := ecNone;
              Response := ControlMessage;
              end
          else
            begin
            Result := ecInvalidControlFlow;
            TcnocStackMessage.DestroyMessage(ControlMessage);
            end;
          end;
          end;
        end
      else
        Result := ecNone;
      end;
    end;
end;

function TcnocStackBinaryClient.WaitForMessage(out Message: PcnocStackMessage; const Timeout: Integer): TWaitResult;
begin
  if not Assigned(FSocket) then
    Result := wrAbandoned
  else
    begin
    if Timeout> 0 then
      Result := FReaderThread.ReceivedMessagesQueue.PopItemTimeout(Message, Timeout)
    else
      Result := FReaderThread.ReceivedMessagesQueue.PopItem(Message);

    if Result = wrAbandoned then
      begin
      // The connection has been lost. Free all resources
      CleanupConnection;
      end;
    end;
end;

function TcnocStackBinaryClient.SendMessage(MessagType: TcnocStackMessageType; StackName: string;
  Flags: TcnocStackFlags; StoryId: LongWord; Data: TBytes; ExtAccessKeys: TStringArray;
  IntAccessKeys: TStringArray; ResponseData: TBytes): TcnocStackErrorCodes;
var
  Msg: PcnocStackMessage;
begin
  Assert(not (sfExpectDirectResponse in Flags));
  Result :=SendMessage(MessagType, StackName, Flags, StoryId, Data, ExtAccessKeys, IntAccessKeys, ResponseData, Msg);
  assert(not Assigned(Msg));
end;

function TcnocStackBinaryClient.SubscribeToStack(const StackName: string): TcnocStackErrorCodes;
begin
  Result := SendMessage(smtSubscribeToStack, StackName, [sfRequestDirectAck], 0, nil);
end;

function TcnocStackBinaryClient.SendResponseMessage(Message: PcnocStackMessage; Flags: TcnocStackFlags; Data: TBytes;
  ExtAccessKeys: TStringArray; IntAccessKeys: TStringArray; ResponseData: TBytes): TcnocStackErrorCodes;
var
  Msg: PcnocStackMessage;
begin
  Assert(not (sfExpectDirectResponse in Flags));
  Result :=SendResponseMessage(Message, Flags, Data, ExtAccessKeys, IntAccessKeys, ResponseData, Msg);
  assert(not Assigned(Msg));
end;

function TcnocStackBinaryClient.SendResponseMessage(Message: PcnocStackMessage; Flags: TcnocStackFlags; Data: TBytes;
  ExtAccessKeys: TStringArray; IntAccessKeys: TStringArray; ResponseData: TBytes; out
  Response: PcnocStackMessage): TcnocStackErrorCodes;
var
  ResponseMessage: PcnocStackMessage;
begin
  Response := nil;
  if not Assigned(FSocket) then
    Result := ecNotConnected
  else
    begin
    ResponseMessage := Message^.CreateResponseMessage(
      Flags,
      Data,
      ExtAccessKeys,
      IntAccessKeys,
      ResponseData);
    try
      Result := IntSendMessage(ResponseMessage, Response);
    finally
      TcnocStackMessage.DestroyMessage(ResponseMessage);
    end;
    end;
end;

function TcnocStackBinaryClient.IsConnected: Boolean;
begin
  Result := Assigned(FSocket);
  if Result then
    begin
    if Assigned(FReaderThread) and FReaderThread.ReceivedMessagesQueue.ShutDown then
      begin
      Result := False;
      CleanupConnection;
      end;
    end;
end;

function TcnocStackBinaryClient.SendMessage(StackName: string; Data: string; out ResponseData: string;
  ExtAccessKeys: TStringArray; AStoryId: LongWord): TcnocStackErrorCodes;
var
  DataBytes: TBytes;
  ResponseBytes: TBytes;
begin
  SetLength(DataBytes, Length(Data));
  Move(Data[1], DataBytes[0], Length(Data));
  Result := SendMessage(StackName, DataBytes, ResponseBytes, ExtAccessKeys, AStoryId);
  SetLength(ResponseData, Length(ResponseBytes));
  Move(ResponseBytes[0], ResponseData[1], Length(ResponseBytes));
end;

function TcnocStackBinaryClient.SendMessage(StackName: string; Data: TBytes; out ResponseData: TBytes;
  ExtAccessKeys: TStringArray; AStoryId: LongWord): TcnocStackErrorCodes;
var
  Response: PcnocStackMessage;
begin
  Result := SendMessage(smtToStack, StackName, [sfRequestDirectAck, sfExpectDirectResponse], AStoryId, Data, ExtAccessKeys, nil, nil, Response);
  try
    if (Result = ecNone) and Assigned(Response) then
      begin
      ResponseData := Response^.GetContents;
      end
    else
      ResponseData := [];
  finally
    TcnocStackMessage.DestroyMessage(Response);
  end;
end;

function TcnocStackBinaryClient.SendMessage(MessagType: TcnocStackMessageType; StackName: string;
  Flags: TcnocStackFlags; StoryId: LongWord; Data: string; ExtAccessKeys: TStringArray;
  IntAccessKeys: TStringArray; ResponseData: TBytes): TcnocStackErrorCodes;
var
  DataBytes: TBytes;
begin
  SetLength(DataBytes, Length(Data));
  Move(Data[1], DataBytes[0], Length(Data));
  Result := SendMessage(MessagType, StackName, Flags, StoryId, DataBytes, ExtAccessKeys, IntAccessKeys, ResponseData);
end;

procedure TcnocStackBinaryClient.CleanupConnection;
begin
  // This seems silly, but TThread.WaitFor calls CheckSynchronize when it
  // is the main thread, with a timeout of 100msecs. By waiting just a short
  // period, this could be avoided. (Which means that the shutdown could be
  // 100 msecs faster)
  WaitForThreadTerminate(FReaderThread.Handle, 10);
  FReaderThread.WaitFor;

  FreeAndNil(FReaderThread);
  FreeAndNil(FSocket);
end;

end.
