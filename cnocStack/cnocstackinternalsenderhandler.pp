unit cnocStackInternalSenderHandler;

{ CnocStack's internal handling of messages

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
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  Generics.Defaults,
  Generics.Collections;

type
  TcnocStackSenderHash = LongWord;
  IStackSender = Interface
    function SendControlMessage(const AItem: PcnocStackMessage): TWaitResult;
  end;
  TStackSenderMap = specialize TObjectHashMap<TcnocStackSenderHash, IStackSender>;


  { TcnocStackInternalSenderHandler }

  TcnocStackInternalSenderHandler = class
  private
    FSenders: TStackSenderMap;
    FSendersLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterSender(ASender: IStackSender): TcnocStackSenderHash;
    procedure UnregisterSender(ASenderHash: TcnocStackSenderHash);
    function SendMessage(const AItem: PcnocStackMessage): TcnocStackErrorCodes;
  end;

implementation

{ TcnocStackInternalSenderHandler }

constructor TcnocStackInternalSenderHandler.Create;
begin
  FSendersLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FSenders := TStackSenderMap.Create();
end;

function TcnocStackInternalSenderHandler.RegisterSender(ASender: IStackSender): TcnocStackSenderHash;
begin
  FSendersLock.Beginwrite;
  try
    repeat
    Result := Random($FFFFFFFF);
    until (result <> 0) and (not FSenders.ContainsKey(Result));
    FSenders.Add(Result, ASender);
  finally
    FSendersLock.Endwrite;
  end;
end;

procedure TcnocStackInternalSenderHandler.UnregisterSender(ASenderHash: TcnocStackSenderHash);
begin
  FSendersLock.Beginwrite;
  try
    FSenders.Remove(ASenderHash);
  finally
    FSendersLock.Endwrite;
  end;
end;

destructor TcnocStackInternalSenderHandler.Destroy;
begin
  FSenders.Free;
  FSendersLock.Free;
  inherited Destroy;
end;

function TcnocStackInternalSenderHandler.SendMessage(const AItem: PcnocStackMessage): TcnocStackErrorCodes;
var
  Sender: IStackSender;
begin
  FSendersLock.Beginread;
  try
    if AItem^.Header.RoutingInfo = 0 then
      Result := ecMissingRoutingInfo
    else if not FSenders.TryGetValue(AItem^.Header.RoutingInfo, Sender) then
      Result := ecSenderNotAvailable
    else
      begin
      AItem^.Header.MessageType := smtDirectResponse;
      case Sender.SendControlMessage(AItem) of
        wrTimeout: Result := ecPushMessageTimeout;
        wrAbandoned: Result := ecPushMessageAbandoned;
        wrError: Result := ecPushMessageError;
      end;
      end;
  finally
    FSendersLock.Endread;
  end;
end;

end.

