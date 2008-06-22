unit lCriticalSection;

{$mode objfpc}{$H+}

interface

  procedure EnterLNetCS;
  procedure LeaveLNetCS;

implementation

uses
  SyncObjs;

var
  CS: TCriticalSection;
  LNetCSInited: Boolean = False;
  
procedure InitLNetCS;
begin
  if IsMultiThread and (not LNetCSInited) then begin
    CS := TCriticalSection.Create;
    LNetCSInited := True;
  end;
end;

procedure DoneLNetCS;
begin
  if IsMultiThread and LNetCSInited then begin
    CS.Free;
    LNetCSInited := False;
  end;
end;

procedure EnterLNetCS;
begin
  InitLNetCS; // attempt to make a CS, if a thread was created it'll work
  if IsMultiThread and LNetCSInited then
    CS.Enter;
end;

procedure LeaveLNetCS;
begin
  if IsMultiThread and LNetCSInited then
    CS.Leave;
end;

finalization
  DoneLNetCS;

end.

