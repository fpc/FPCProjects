unit cnocStackHandlerThread;

{ CnocStack handler

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
  syncobjs,
  generics.Collections,
  cnocStackbinaryclient,
  cnocStackMessageTypes;

type
  IcnocStackHandler = interface ['{5906EA51-8EF5-427F-AC49-ACE1AF8C47E9}']
  end;

  IcnocStackRespondToMessage = interface(IcnocStackHandler) ['{FA2AA839-AA16-42C8-A706-1509963A069D}']
    procedure RespondToMessage(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean);
  end;

  IcnocStackHandleMessage = interface ['{C64C602E-40EE-4674-966F-9BEC8E234A8A}']
    procedure HandleMessage(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
  end;


  TcnocStackHandlerMap = specialize TObjectHashMap<string, TObject>;

  TcnocStackRespondToMessage = procedure(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean);
  TcnocStackOnMessage = procedure(const BinaryClient: TcnocStackBinaryClient; const IncomingMessage: PcnocStackMessage; var Handled: Boolean);

  { TcnocStackHandlerThread }

  TcnocStackHandlerThread = class(TThread)
  private
    FHost: string;
    FPort: Word;
    FOnRespondToMessage: TcnocStackRespondToMessage;
    FOnMessage: TcnocStackOnMessage;
    FSubscribeToStacks: TStringArray;
  protected
    FBinaryClient: TcnocStackBinaryClient;
    FHandlerMap: TcnocStackHandlerMap;
    procedure DoRespondToMessage(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean); virtual;
    procedure DoHandleMessage(const IncomingMessage: PcnocStackMessage; var Handled: Boolean); virtual;
    procedure RespondToMessage(const IncomingMessage: PcnocStackMessage; out Response: TBytes); virtual;
    procedure HandleMessage(const IncomingMessage: PcnocStackMessage); virtual;
  public
    constructor Create(Host: string; Port: Word; SubscribeToStacks: TStringArray);
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddHandler(const Name: string; const AHandler: TObject);
    procedure RemoveHandler(const Name: string);
    property OnRespondToMessage: TcnocStackRespondToMessage read FOnRespondToMessage write FOnRespondToMessage;
    property OnMessage: TcnocStackOnMessage read FOnMessage write FOnMessage;
  end;

implementation


{ TcnocStackHandlerThread }

constructor TcnocStackHandlerThread.Create(Host: string; Port: Word; SubscribeToStacks: TStringArray);
begin
  Inherited Create(False);
  FHost := Host;
  FPort := Port;
  FSubscribeToStacks := SubscribeToStacks;
  FHandlerMap := TcnocStackHandlerMap.Create();
end;

destructor TcnocStackHandlerThread.Destroy;
begin
  FHandlerMap.Free;
  inherited Destroy;
end;

procedure TcnocStackHandlerThread.Execute;
var
  wr: TWaitResult;
  Message: PcnocStackMessage;
  Response: TBytes;
  i: Integer;
begin
  repeat
    try
      FBinaryClient := TcnocStackBinaryClient.Create(FHost, FPort);
      try
        try
          FBinaryClient.Connect();
        except
          on E: Exception do
            begin
            // Log. Retry in 10 seconds.
            Sleep(10000);
            end;
        end;
        for i := 0 to High(FSubscribeToStacks) do
          FBinaryClient.SubscribeToStack(FSubscribeToStacks[i]);
        if FBinaryClient.IsConnected then
          begin
          repeat
          wr := FBinaryClient.WaitForMessage(Message, 1000);
          if wr = wrSignaled then
            begin
            try
              try
                if sfExpectDirectResponse in Message^.Header.Flags then
                  begin
                  RespondToMessage(Message, Response);
                  end
                else
                  HandleMessage(Message);
              Except
                // Log and continue
              end;
            finally
              TcnocStackMessage.DestroyMessage(Message);
            end;
            end;
          until (not (wr in [wrSignaled, wrTimeout])) or (Terminated);
          end;
      finally
        FBinaryClient.Free;
      end;
    except
      // Continue... ToDo: Log?
    end;
  until Terminated;
end;

procedure TcnocStackHandlerThread.RespondToMessage(const IncomingMessage: PcnocStackMessage; out Response: TBytes);
var
  Handled: Boolean;
begin
  Handled := False;
  Response := [];
  if Assigned(FOnRespondToMessage) then
    FOnRespondToMessage(IncomingMessage, Response, Handled);

  DoRespondToMessage(IncomingMessage, Response, Handled);

  if Assigned(Response) then
    FBinaryClient.SendResponseMessage(IncomingMessage, [], Response);
  if Assigned(FOnMessage) then
    FOnMessage(FBinaryClient, IncomingMessage, Handled);
end;

procedure TcnocStackHandlerThread.HandleMessage(const IncomingMessage: PcnocStackMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnMessage) then
    FOnMessage(FBinaryClient, IncomingMessage, Handled);

  DoHandleMessage(IncomingMessage, Handled)
end;

procedure TcnocStackHandlerThread.AddHandler(const Name: string; const AHandler: TObject);
begin
  FHandlerMap.Add(Name, AHandler);
end;

procedure TcnocStackHandlerThread.RemoveHandler(const Name: string);
begin
  FHandlerMap.Remove(Name);
end;

procedure TcnocStackHandlerThread.DoHandleMessage(const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
var
  Handler: TObject;
  MessHandler: IcnocStackHandleMessage;
begin
  for Handler in FHandlerMap.Values do
    begin
    if supports(Handler, IcnocStackHandleMessage, MessHandler) then
      MessHandler.HandleMessage(FBinaryClient, IncomingMessage, Handled);
    end;
end;

procedure TcnocStackHandlerThread.DoRespondToMessage(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean);
var
  Handler: TObject;
  RespHandler: IcnocStackRespondToMessage;
  MessHandler: IcnocStackHandleMessage;
begin
  for Handler in FHandlerMap.Values do
    begin
    if supports(Handler, IcnocStackRespondToMessage, RespHandler) then
      RespHandler.RespondToMessage(IncomingMessage, Response, Handled);
    if supports(Handler, IcnocStackHandleMessage, MessHandler) then
      MessHandler.HandleMessage(FBinaryClient, IncomingMessage, Handled);
    end;
end;

end.

