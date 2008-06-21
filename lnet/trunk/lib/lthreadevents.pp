unit lThreadEvents;

{$mode objfpc}{$H+}

interface

uses
  {$ifndef windows}cThreads,{$endif}
  Classes, SysUtils,
  lNet, lEvents;
  
type

  { TWorkThread }

  TLWorkThread = class(TThread)
   private
    FEventer: TLEventer;
    FWorking: Boolean;
    FQuit: Boolean;
   public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
                     DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    property Working: Boolean read FWorking;
    property Quit: Boolean read FQuit write FQuit;
    property Eventer: TLEventer read FEventer;
  end;

  { TLThreadedEventer }

  TLThreadedEventer = class(TLEventer)
   protected
    FWorkThread: array of TLWorkThread;
    FThreadCount: Integer;
    FActiveThread: Integer;
    FSocketsPerThread: Integer;
    function CreateWorkThread(aEventer: TLEventer): TLWorkThread;
    
    function GetWorkThread(const i: Integer): TLWorkThread;
    function GetCount: Integer; override;
    function GetTimeout: Integer; override;
    procedure SetTimeout(const aValue: Integer); override;
   public
    constructor Create; override;
    destructor Destroy; override;
    { AddHandle is called from within lNet unit as FEventer.AddHandle
      base on TLConnection's eventer, which means this eventer }
    function AddHandle(aHandle: TLHandle): Boolean; override;
   public
    property WorkThreads[i: Integer]: TLWorkThread read GetWorkThread;
    property ThreadCount: Integer read FThreadCount;
    property SocketsPerThread: Integer read FSocketsPerThread write FSocketsPerThread;
  end;

implementation

{ TLWorkThread }

constructor TLWorkThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FWorking := True; // needed for special case
  
  inherited Create(CreateSuspended, StackSize);
end;

destructor TLWorkThread.Destroy;
begin
  FEventer.Free;

  inherited Destroy;
end;

procedure TLWorkThread.Execute;
begin
  FWorking := True;
  
  while not FQuit do
    FEventer.CallAction;

  FWorking := False;
  FQuit := False; // auto-flip
end;

{ TLThreadedEventer }

function TLThreadedEventer.CreateWorkThread(aEventer: TLEventer): TLWorkThread;
const
  DEF_TIMEOUT = 50; // 50ms is good enough
var
  i: Integer;
begin
  i := Length(FWorkThread);
  Writeln('Creating work thread: ', i);
  SetLength(FWorkThread, i + 1);
  FThreadCount := i + 1;
  FActiveThread := i;

  aEventer.Timeout := DEF_TIMEOUT;

  FWorkThread[i] := TLWorkThread.Create(True);
  FWorkThread[i].FEventer := aEventer;
  Result := FWorkThread[i];
end;

function TLThreadedEventer.GetWorkThread(const i: Integer): TLWorkThread;
begin
  Result := FWorkThread[i];
end;

function TLThreadedEventer.GetCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FThreadCount - 1 do
    Result := Result + FWorkThread[i].Eventer.Count;
end;

function TLThreadedEventer.GetTimeout: Integer;
begin
  Result := 0;
  
  if FThreadCount > 0 then
    Result := FWorkThread[0].Eventer.Timeout;
end;

procedure TLThreadedEventer.SetTimeout(const aValue: Integer);
var
  i: Integer;
begin
  if aValue < 0 then
    raise Exception.Create('TThreadedEventer must have Timeout >= 0');
    
  for i := 0 to FThreadCount - 1 do
    FWorkThread[i].Eventer.Timeout := aValue;
end;

constructor TLThreadedEventer.Create;
begin
  inherited Create;

  CreateWorkThread(BestEventerClass.Create).Resume;
end;

destructor TLThreadedEventer.Destroy;
var
  i: Integer;
begin
  for i := 0 to FThreadCount - 1 do begin
    if FWorkThread[i].Working then
      FWorkThread[i].Quit := True;

    FWorkThread[i].WaitFor;
    FWorkThread[i].Free;
  end;

  inherited Destroy;
end;

function TLThreadedEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  i: Integer;
begin
  if aHandle is TLSocket then
    TLSocket(aHandle).SetState(ssBlocking, True);
    
  if FSocketsPerThread > 0 then
    if FWorkThread[FActiveThread].Eventer.Count >= FSocketsPerThread then begin
      for i := 0 to FThreadCount - 1 do
        if FWorkThread[i].Eventer.Count < FSocketsPerThread then begin
          FActiveThread := i;
          Break;
        end;
      { If there's no thread/eventer with free space, then make new one }
      if FWorkThread[FActiveThread].Eventer.Count >= FSocketsPerThread then
        CreateWorkThread(BestEventerClass.Create).Resume;
    end;
    
  Result := FWorkThread[FActiveThread].Eventer.AddHandle(aHandle);
end;

end.

