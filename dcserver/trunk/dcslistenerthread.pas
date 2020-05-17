unit dcsListenerThread;

{ Distributed Command Server

  This unit contains a TThread-class which implements a IDCSListener so that
  all events which are sent through the DCServer-distributor can be monitored.

  Copyright (C) 2016 Joost van der Sluis joost@cnoc.nl

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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  dcsHandler,
  DCSInOutputProcessor,
  cnocQueue;

type

  TDCSEventQueue = specialize TcnocThreadedQueue<TDCSEvent>;
  TDCSIdleEvent = procedure of object;
  TDCSEventEvent = procedure (Event: TDCSEvent) of object;

  { TDCSListenerThread }

  TDCSListenerThread = class(TThread, IDCSListener)
  private
    FEventQueue: TDCSEventQueue;
    FOnEvent: TDCSEventEvent;
    FOnIdle: TDCSIdleEvent;
    FTimeout: Integer;
    FListenerId: integer;
    FDistributor: TDCSDistributor;
    FOrigin: string;
    procedure SetDistributor(AValue: TDCSDistributor);
    procedure SetOnEvent(AValue: TDCSEventEvent);
    procedure SetOnIdle(AValue: TDCSIdleEvent);
    procedure SetTimeout(AValue: integer);
  protected
    procedure Execute; override;

    procedure DoOnEvent(Event: TDCSEvent);
    procedure DoOnIdle;
  public
    // Use this constructor to create the thread and set it's properties. It is
    // not possible to adapt any properties while the thread is already running.
    // So alternatively create the thread suspended, set the properties and start
    // the thread manually.
    constructor Create(ADistributor: TDCSDistributor; Timeout: integer; Origin: string; AnOnIdle: TDCSIdleEvent; AnOnEvent: TDCSEventEvent);
    destructor Destroy; override;

    // Can be called from any thread to terminate the listener-thread immediately
    // (no timeout)
    procedure ForceTerminate;

    // IDCSListener
    procedure SendEvent(AnEvent: TDCSEvent);
    procedure InitListener(AListenerId: Integer);
    function GetListenerId: Integer;
    function GetOrigin: string;

    // The DCS-Distributor to be used. Obligatory.
    property Distributor: TDCSDistributor read FDistributor write SetDistributor;

    // Is called within the listener-thread each time an event is received
    property OnEvent: TDCSEventEvent read FOnEvent write SetOnEvent;

    // The timeout for a: any call to OnIdle and b: the check on a requested
    // to terminate the thread (but see ForceTerminate)
    property Timeout: integer read FTimeout write SetTimeout;

    // Is called within the listener-thread when there has not been any event
    // for Timeout milliseconds
    property OnIdle: TDCSIdleEvent read FOnIdle write SetOnIdle;
  end;

implementation

resourcestring
  SSetPropertyWhileRunning = 'It is not possible to set TDCSListenerThread.%s while the thread is already running.';
  SNoDistributor = 'TDCSListenerThread can not run without distributor.';

{ TDCSLogServer }

procedure TDCSListenerThread.SetOnEvent(AValue: TDCSEventEvent);
begin
  if FOnEvent = AValue then Exit;
  if not Suspended then
    raise Exception.CreateFmt(SSetPropertyWhileRunning, ['OnEvent']);
  FOnEvent := AValue;
end;

procedure TDCSListenerThread.SetDistributor(AValue: TDCSDistributor);
begin
  if FDistributor = AValue then Exit;
  if not Suspended then
    raise Exception.CreateFmt(SSetPropertyWhileRunning, ['Distributor']);
  FDistributor := AValue;
end;

procedure TDCSListenerThread.SetOnIdle(AValue: TDCSIdleEvent);
begin
  if FOnIdle = AValue then Exit;
  if not Suspended then
    raise Exception.CreateFmt(SSetPropertyWhileRunning, ['OnIdle']);
  FOnIdle := AValue;
end;

procedure TDCSListenerThread.SetTimeout(AValue: integer);
begin
  if FTimeout = AValue then Exit;
  if not Suspended then
    raise Exception.CreateFmt(SSetPropertyWhileRunning, ['Timeout']);
  FTimeout := AValue;
end;

procedure TDCSListenerThread.Execute;
var
  Event: TDCSEvent;
begin
  // Set some default-values. (In case the create of an ancestor is being used)
  if FOrigin='' then
    FOrigin := ClassName;
  if FTimeout=0 then
    FTimeout := 15*MSecsPerSec;

  if not Assigned(FDistributor) then
    raise Exception.Create(SNoDistributor);

  FListenerId := FDistributor.AddListener(self);
  try
    while not terminated do
      begin
      if (FEventQueue.PopItem(Event) = wrSignaled) then
        begin
        try
          DoOnEvent(Event);
        finally
          Event.Release;
        end;
        end
      else
        DoOnIdle;
      end;
  finally
    FDistributor.RemoveListener(self);
  end;
end;

procedure TDCSListenerThread.DoOnEvent(Event: TDCSEvent);
begin
  if Assigned(FOnEvent) then
    FOnEvent(Event);
end;

procedure TDCSListenerThread.DoOnIdle;
begin
  if Assigned(FOnIdle) then
    FOnIdle;
end;

constructor TDCSListenerThread.Create(ADistributor: TDCSDistributor; Timeout: integer;
  Origin: string; AnOnIdle: TDCSIdleEvent; AnOnEvent: TDCSEventEvent);
var
  PopTimeout: cardinal;
begin
  inherited Create(False);
  FDistributor := ADistributor;
  FTimeout := Timeout;
  FOrigin := Origin;
  FOnIdle := AnOnIdle;
  FOnEvent := AnOnEvent;
  FEventQueue := TDCSEventQueue.create(100, INFINITE, Timeout);
end;

destructor TDCSListenerThread.Destroy;
begin
  FEventQueue.Free;
  inherited Destroy;
end;

procedure TDCSListenerThread.ForceTerminate;
begin
  Terminate;
  FEventQueue.DoShutDown;
end;

procedure TDCSListenerThread.SendEvent(AnEvent: TDCSEvent);
begin
  AnEvent.AddRef;
  FEventQueue.PushItem(AnEvent);
end;

procedure TDCSListenerThread.InitListener(AListenerId: Integer);
begin
  FListenerId := FListenerId;
end;

function TDCSListenerThread.GetListenerId: Integer;
begin
  Result := FListenerId;
end;

function TDCSListenerThread.GetOrigin: string;
begin
  Result := FOrigin;
end;

end.

