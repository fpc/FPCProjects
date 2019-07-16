unit cnocStackMessageTypes;

{ CnocStack's basic message-type

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

{$mode objfpc}{$H+}{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes,
  Sysutils,
  cnocQueue,
  cnocStackErrorCodes;

type
  TcnocStackMessageType = (
    smtToStack  = 0,
    smtSubscribeToStack = 1,
    smtFromStack = 2,
    smtAck = 3,
    smtNack = 4,
    smtDirectResponse = 5,
    smtResponseToStack = 6
  );
  TcnocStackFlag = (
    sfRequestDirectAck,
    //sfRequestAAck,
    sfExpectDirectResponse
  );
  TcnocStackFlags = set of TcnocStackFlag;

  PcnocStackMessage = ^TcnocStackMessage;

  { TcnocStackMessageHeader }

  TcnocStackMessageHeader = packed record
    Version: Word;
    ContentLength: LongWord;
    IntAccessKeyCount: Byte;
    ExtAccessKeyCount: Byte;
    StoryId: LongWord;
    ResponseDataIndex: LongWord;   // (Also the size of the stack-name)
    IntAccessKeyIndex: LongWord;
    ExtAccessKeyIndex: LongWord;
    ContentIndex: LongWord;
    MessageType: TcnocStackMessageType;
    Flags: TcnocStackFlags;
    ErrorCode: LongWord;
    RoutingInfo: LongWord;
    Reserved: array[0..19] of Byte;
  end;

  TcnocStackMessage = record
    Header: TcnocStackMessageHeader;
    Contents: array [0..100{MaxInt}] of Byte;
    class function CreateMessage(
      AMessageType: TcnocStackMessageType;
      AStackName: string;
      AFlags: TcnocStackFlags;
      AStoryId: LongWord;
      AData: TBytes;
      AnExtAccessKeys: TStringArray = nil;
      AnIntAccessKeys: TStringArray = nil;
      AResponseData: TBytes = nil): PcnocStackMessage; static;
    class procedure DestroyMessage(Message: PcnocStackMessage); static;
    class function AllocateMessage(AContentLength: LongWord): PcnocStackMessage; static;
    function CreateResponseMessage(
      AFlags: TcnocStackFlags;
      AData: TBytes;
      AnExtAccessKeys: TStringArray = nil;
      AnIntAccessKeys: TStringArray = nil;
      AResponseData: TBytes = nil): PcnocStackMessage;
    function GetStackName: AnsiString;
    function GetContents: TBytes;
    function GetContentsAsAnsiString: AnsiString;
    function GetResponseData: TBytes;
    function GetErrorCode: TcnocStackErrorCodes;
    function GetExtAccessKey(Index: Integer): string;
  end;

  TcnocStackMessageTypesQueue = specialize TcnocThreadedQueue<PcnocStackMessage>;

const
  ScnocStackMessageType: array[TcnocStackMessageType] of string = (
    'ToStack',
    'SubscribeToStack',
    'FromStack',
    'ACK',
    'NACK',
    'DirectResponse',
    'ResponseToStack'
  );

implementation

{ TcnocStackMessageHeader }

class function TcnocStackMessage.CreateMessage(AMessageType: TcnocStackMessageType; AStackName: string;
  AFlags: TcnocStackFlags; AStoryId: LongWord; AData: TBytes; AnExtAccessKeys: TStringArray;
  AnIntAccessKeys: TStringArray; AResponseData: TBytes): PcnocStackMessage;
var
  I: Integer;
  AContentLength: LongWord;
  Len: LongWord;
  AResponseDataIndex: LongWord;
  AExtAccessKeyIndex, AIntAccessKeyIndex: LongWord;
  AContentIndex: LongWord;
begin
  AResponseDataIndex := Length(AStackName);
  AIntAccessKeyIndex := AResponseDataIndex + Length(AResponseData);
  AExtAccessKeyIndex := AIntAccessKeyIndex;
  for I := 0 to Length(AnIntAccessKeys) -1 do
    AExtAccessKeyIndex := AExtAccessKeyIndex + Length(AnIntAccessKeys[I]) + SizeOf(LongWord);

  AContentIndex := AExtAccessKeyIndex;
  for I := 0 to Length(AnExtAccessKeys) -1 do
    AContentIndex := AContentIndex + Length(AnExtAccessKeys[I]) + SizeOf(LongWord);

  AContentLength := AContentIndex + Length(AData);

  Result := AllocateMessage(AContentLength);
  Result^.Header.ContentLength := AContentLength;
  Result^.Header.Version := 1;
  Result^.Header.IntAccessKeyCount := Length(AnIntAccessKeys);
  Result^.Header.ExtAccessKeyCount := Length(AnExtAccessKeys);
  Result^.Header.StoryId := AStoryId;
  Result^.Header.Flags := AFlags;
  Result^.Header.MessageType := AMessageType;
  Result^.Header.ResponseDataIndex := AResponseDataIndex;
  Result^.Header.IntAccessKeyIndex := AIntAccessKeyIndex;
  Result^.Header.ExtAccessKeyIndex := AExtAccessKeyIndex;
  Result^.Header.ContentIndex := AContentIndex;
  Move(AStackName[1], Result^.Contents[0], AResponseDataIndex);
  Move(AResponseData[0], Result^.Contents[AResponseDataIndex], Length(AResponseData));
  Move(AData[0], Result^.Contents[AContentIndex], Length(AData));

  AContentIndex := AExtAccessKeyIndex;
  for I := 0 to Length(AnExtAccessKeys) -1 do
    begin
    Len := Length(AnExtAccessKeys[I]);
    Move(Len, Result^.Contents[AContentIndex], SizeOf(Len));
    Inc(AContentIndex, SizeOf(Len));
    Move(AnExtAccessKeys[i][1], Result^.Contents[AContentIndex], Len);
    AContentIndex := AContentIndex + Len;
    end;
end;

class procedure TcnocStackMessage.DestroyMessage(Message: PcnocStackMessage);
begin
  FreeMem(Message);
end;

class function TcnocStackMessage.AllocateMessage(AContentLength: LongWord): PcnocStackMessage;
begin
  Result := Getmem(AContentLength + SizeOf(TcnocStackMessageHeader));
  FillQWord(Result^, SizeOf(TcnocStackMessageHeader) div 8, 0);
end;

function TcnocStackMessage.GetContents: TBytes;
var
  ContentSize: LongWord;
begin
  Result := [];
  ContentSize := Header.ContentLength - Header.ContentIndex;
  SetLength(Result, ContentSize);
  Move(Contents[Header.ContentIndex], Result[0], ContentSize);
end;

function TcnocStackMessage.GetStackName: AnsiString;
begin
  Result := '';
  SetLength(Result, Header.ResponseDataIndex);
  Move(Contents[0], Result[1], Header.ResponseDataIndex);
end;

function TcnocStackMessage.GetResponseData: TBytes;
var
  ContentSize: LongWord;
begin
  Result := [];
  ContentSize := Header.IntAccessKeyIndex - Header.ResponseDataIndex;
  SetLength(Result, ContentSize);
  Move(Contents[Header.ResponseDataIndex], Result[0], ContentSize);
end;

function TcnocStackMessage.GetErrorCode: TcnocStackErrorCodes;
begin
  Result := TcnocStackErrorCodes(Header.ErrorCode);
end;

function TcnocStackMessage.CreateResponseMessage(AFlags: TcnocStackFlags; AData: TBytes;
  AnExtAccessKeys: TStringArray; AnIntAccessKeys: TStringArray; AResponseData: TBytes): PcnocStackMessage;
begin
  Assert(Header.RoutingInfo <> 0, 'No routing-info, inpossible to respond to message');
  Result := CreateMessage(smtResponseToStack, GetStackName, AFlags, Header.StoryId, AData, AnExtAccessKeys, AnIntAccessKeys, AResponseData);
  Result^.Header.RoutingInfo := Header.RoutingInfo;
end;

function TcnocStackMessage.GetContentsAsAnsiString: AnsiString;
var
  ContentSize: LongWord;
begin
  Result := '';
  ContentSize := Header.ContentLength - Header.ContentIndex;
  SetLength(Result, ContentSize);
  Move(Contents[Header.ContentIndex], Result[1], ContentSize);
end;

function TcnocStackMessage.GetExtAccessKey(Index: Integer): string;
var
  ContentSize: LongWord;
  Loc, Len: LongWord;
  I: Integer;
begin
  Result := '';
  if Index < Header.ExtAccessKeyCount then
    begin
    Loc := Header.ExtAccessKeyIndex;
    Len := PLongWord(@Contents[Loc])^;
    for I := 0 to Index -1 do
      begin
      Loc := Loc + SizeOf(Len) + Len;
      Len := PLongWord(@Contents[Loc])^;
      end;
    SetLength(Result, Len);
    Move(Contents[Loc + SizeOf(Loc)], Result[1], Len);
    end;
end;

end.

