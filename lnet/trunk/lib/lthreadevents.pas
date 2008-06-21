unit lThreadEvents;

{$mode objfpc}{$H+}

interface

uses
  {$ifndef windows}cThreads,{$endif}
  Classes,
  lNet, lEvents;
  
type

  { TLThreadedEventer }

  TLThreadedEventer = class(TLEventer)
   protected
    FWorkThread: TThread;
    FWorkEventer: TLEventer;
    procedure CreateWorkThread;
    function GetTimeout: Integer; override;
    procedure SetTimeout(const aValue: Integer); override;
   public
    constructor Create; override;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; override;
    procedure RemoveHandle(aHandle: TLHandle); override;
    procedure UnplugHandle(aHandle: TLHandle); override;
    procedure UnregisterHandle(aHandle: TLHandle); override;
   public
    property WorkThread: TThread read FWorkThread;
    property WorkEventer: TLEventer read FWorkEventer;
  end;

implementation

uses
  syncobjs;
  
var
  CS: TCriticalSection;

type

  { TWorkThread }

  TWorkThread = class(TThread)
   private
    FOwner: TLThreadedEventer;
    FEventer: TLEventer;
    FWorking: Boolean;
    FQuit: Boolean;
   public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
                     DefaultStackSize);
    procedure Execute; override;
    property Working: Boolean read FWorking;
    property Quit: Boolean read FQuit write FQuit;
  end;

{ TWorkThread }

constructor TWorkThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FWorking := True; // needed for special case
  inherited Create(CreateSuspended, StackSize);
end;

procedure TWorkThread.Execute;
begin
  FWorking := True;
  
  while not FQuit do
    FEventer.CallAction;

  FWorking := False;
  FQuit := False; // auto-flip
end;

{ TLThreadedEventer }

procedure TLThreadedEventer.CreateWorkThread;
begin
  FWorkThread := TWorkThread.Create(True);
  TWorkThread(FWorkThread).FEventer := FWorkEventer;
  TWorkThread(FWorkThread).FOwner := Self;
end;

function TLThreadedEventer.GetTimeout: Integer;
begin
  Result := FWorkEventer.Timeout;
end;

procedure TLThreadedEventer.SetTimeout(const aValue: Integer);
begin
  FWorkEventer.Timeout := aValue;
end;

constructor TLThreadedEventer.Create;
begin
  inherited Create;

  FWorkEventer := BestEventerClass.Create;
  FWorkEventer.Timeout := 10; // 10ms is good enough, but power-saving will suck
  
  CreateWorkThread;
  FWorkThread.Resume;
end;

destructor TLThreadedEventer.Destroy;
begin
  CS.Enter;
    if TWorkThread(FWorkThread).Working then
      TWorkThread(FWorkThread).Quit := True;

    FWorkThread.WaitFor;
    FWorkThread.Free;
  CS.Leave;
  
  FWorkEventer.Free;

  inherited Destroy;
end;

function TLThreadedEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  FWorkEventer.AddHandle(aHandle);
end;

procedure TLThreadedEventer.RemoveHandle(aHandle: TLHandle);
begin
  FWorkEventer.RemoveHandle(aHandle);
end;

procedure TLThreadedEventer.UnplugHandle(aHandle: TLHandle);
begin
  FWorkEventer.UnplugHandle(aHandle);
end;

procedure TLThreadedEventer.UnregisterHandle(aHandle: TLHandle);
begin
  FWorkEventer.UnregisterHandle(aHandle);
end;

initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;

end.

