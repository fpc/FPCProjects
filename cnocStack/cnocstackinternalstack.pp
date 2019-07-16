unit cnocStackInternalStack;

{ CnocStack's internal handling of stacks

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
  Generics.Defaults,
  Generics.Collections,
  cnocQueue,
  cnocStackMessageTypes;

type
  IStackListener = Interface
    procedure NotifyNewMessage;
  end;
  TStackListenerList = specialize TThreadList<IStackListener>;


  { TcnocStackInternalStack }

  TcnocStackInternalStack = class(TObject)
  private
    FQueue: TcnocStackMessageTypesQueue;
    FListeners: TStackListenerList;
  public
    constructor Create;
    destructor Destroy; override;
    function PushItem(const AItem: PcnocStackMessage): TWaitResult;
    function PopItem(out AItem: PcnocStackMessage): TWaitResult;
    procedure AddListener(AListener: IStackListener);
    procedure RemoveListener(AListener: IStackListener);
  end;
  TcnocStackInternalStackList = specialize TObjectHashMap<string, TcnocStackInternalStack>;

implementation

{ TcnocStackInternalStack }

constructor TcnocStackInternalStack.Create;
begin
  FQueue := TcnocStackMessageTypesQueue.create(10, INFINITE, 10);
  FListeners := TStackListenerList.Create();
end;

destructor TcnocStackInternalStack.Destroy;
begin
  FQueue.Free;
  FListeners.Free;
  inherited Destroy;
end;

function TcnocStackInternalStack.PushItem(const AItem: PcnocStackMessage): TWaitResult;
var
  I: Integer;
begin
  AItem^.Header.MessageType := smtFromStack;
  Result := FQueue.PushItem(AItem);
  if Result = wrSignaled then
    begin
    with FListeners.LockList do
      begin
      try
        for I := 0 to Count -1 do
          begin
          Items[I].NotifyNewMessage;
          end;
      finally
        FListeners.UnlockList;
      end;
      end;
    end;
end;

procedure TcnocStackInternalStack.AddListener(AListener: IStackListener);
begin
  FListeners.Add(AListener);
end;

procedure TcnocStackInternalStack.RemoveListener(AListener: IStackListener);
begin
  FListeners.Remove(AListener);
end;

function TcnocStackInternalStack.PopItem(out AItem: PcnocStackMessage): TWaitResult;
begin
  Result := FQueue.PopItemTimeout(AItem, 0);
end;

end.

