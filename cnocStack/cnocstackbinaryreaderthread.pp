unit cnocStackBinaryReaderThread;

{ Basic thread-class for CnocStack

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
  SysUtils,
  Math,
  ssockets,
  sockets,
  StrUtils,
  cnocStackMessageTypes,
  TLoggerUnit,
  TLevelUnit;

type

  { TcnocStackBinaryReaderThread }

  TcnocStackBinaryReaderThread = class(TThread)
  private
    FStream: TSocketStream;

    FLogSockAddrText: string;
  protected
    FLogger: TLogger;
    procedure HandleReceivedMessage(Message: PcnocStackMessage); virtual; abstract;
  public
    constructor Create(Stream: TSocketStream);
    destructor Destroy; override;
    procedure Execute; override;
  end;

function SockAddToLogText(Addr: TSockAddr): string;
function GetMessageLogText(Message: PcnocStackMessage): string;

implementation

function SockAddToLogText(Addr: TSockAddr): string;
type
  TIPAddr= packed array[1..4] of byte;
var
  IPAddr: ^TIPAddr;
begin
  if Addr.sa_family = AF_INET then
    begin
    IPAddr := @Addr.sin_Addr.s_addr;
    Result := IntToStr(IPAddr^[1]) + '.' + IntToStr(IPAddr^[2]) + '.' + IntToStr(IPAddr^[3]) + '.' + IntToStr(IPAddr^[4]);
    Result := Result + ':' + IntToStr(Addr.sin_port);
    end
  else
    begin
    Result := '(NI)'
    end;
end;

function GetMessageLogText(Message: PcnocStackMessage): string;
const
  Width = 100;
var
  Contents: TBytes;
  j: Integer;

  procedure ObtainStr(StartInd, StopInd: Integer);
  var
    i: Integer;
    s: string;
  begin
    for i := StartInd to min(StopInd, Length(Contents) -1) do
      begin
      if Contents[i] in [32..126] then
        begin
        Result[j] := Chr(Contents[i]);
        Inc(j);
        end
      else
        begin
        s := IntToStr(Contents[i]);
        Move(s[1],Result[j],length(s));
        Inc(j,Length(s));
        end;
      end;
  end;

begin
  Contents := Message^.GetContents;
  SetLength(Result, min(Length(Contents),Width+10) * 4);
  j := 1;
  if Length(Contents) > Width then
    begin
    ObtainStr(0, (Width div 2) - 1);
    Result[31] := ' ';
    Result[32] := '.';
    Result[33] := '.';
    Result[34] := '.';
    Result[35] := ' ';
    Inc(J, 5);
    ObtainStr(Length(Contents)-(Width div 2), Length(Contents) -1);
    end
  else
    ObtainStr(0, Length(Contents) -1);
  SetLength(Result, j -1);
end;

{ TcnocStackBinaryReaderThread }

constructor TcnocStackBinaryReaderThread.Create(Stream: TSocketStream);
begin
  FStream := Stream;
  FLogger := TLogger.GetInstance;
  FLogSockAddrText := SockAddToLogText(FStream.RemoteAddress);

  Inherited Create(False);
end;

procedure TcnocStackBinaryReaderThread.Execute;

  function ReadBuf(Buf: pointer; Count: Integer): LongInt;
  var
    r: Integer;
    Len: LongInt;
  begin
    len:=0;
    repeat
      r:=FStream.Read(PByte(Buf)[Len],Count-Len);
      Inc(Len, r);
    until (Len=Count) or (r<=0);
    if r = 0 then
      begin
      if Assigned(FLogger) then
        FLogger.Debug('TCP: Connection closed');
      Terminate;
      end;
    if r < 0 then
      begin
      // In fact this is an error.. How to handle this?
      if Assigned(FLogger) then
        FLogger.Info('TCP: Connection closed due to error: ' + IntToStr(FStream.LastError));
      Terminate;
      end;
    Result:=Len;
  end;

var
  Buf: TBytes;
  BufSize: Integer;
  Message: PcnocStackMessage;
  Len: LongInt;
  ContentLength: LongWord;

begin
  BufSize := 4096;
  Buf := [];
  SetLength(Buf, BufSize);
  repeat
    ReadBuf(@Buf[0], SizeOf(TcnocStackMessageHeader));
    if not Terminated then
      begin
      ContentLength := PcnocStackMessage(Buf)^.Header.ContentLength;
      Message := TcnocStackMessage.AllocateMessage(ContentLength);
      Message^.Header := PcnocStackMessage(Buf)^.Header;
      if ContentLength > 0 then
        begin
        Len := ReadBuf(@(Message^.Contents[0]), ContentLength);
        if Len < ContentLength then
          // ToDo: log
          raise Exception.Create('Received incomplete message')
        end;
      if Assigned(FLogger) then
        if TLevelUnit.TRACE.IsGreaterOrEqual(TLogger.GetInstance.GetLevel()) then
          TLogger.GetInstance.Trace(PadRight('TCP: (' + FLogSockAddrText + ')',  28) + PadRight(ScnocStackMessageType[Message^.Header.MessageType], 16) + ' => ' + PadRight(Message^.GetStackName,15) +
            GetMessageLogText(Message));
      HandleReceivedMessage(Message);
      end;
  until Terminated;
end;

destructor TcnocStackBinaryReaderThread.Destroy;
begin
  inherited Destroy;
end;

end.

