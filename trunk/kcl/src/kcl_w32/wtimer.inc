{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Win32 specific implementation of TTimer component
// ===================================================================

function TTimer_Proc(Window: HWnd; AMessage, WParam, LParam: Longint): Longint; stdcall; export;
var
  timer: TTimer;
begin
  timer := TTimer(GetProp(Window, PChar('KCL')));
  if timer<>nil then
  if AMessage = WM_TIMER then
    if Assigned(timer.OnTimer) then timer.OnTimer(timer);
  Result := DefWindowProc(Window, AMessage, wParam, lParam);
end;

constructor TTimer.Create(AOwner: TComponent);
var
  wnd: WndClass;
begin
  FEnabled := True;
  FInterval := 1000;

  FillStdWndClass(wnd);
  wnd.lpfnWndProc := WndProc(@TTimer_Proc);
  wnd.lpszClassName := PChar('KCLTIMER');
  if windows.RegisterClass(wnd) <> 0 then
  begin
    Handle := CreateWindow('KCLTIMER', nil, 0, -1, -1, -1, -1, 0, 0, HInstance, nil);
    SetProp(Handle, PChar('KCL'), longint(self));
  end
  else writeln('KCLTIMER Registration failed!');
end;

destructor TTimer.Destroy;
begin
  FEnabled := False;
  Update;
  DestroyWindow(Handle);
end;

procedure TTimer.Update;
begin
  KillTimer(Handle, 1);
  if FEnabled then SetTimer(Handle, 1, FInterval, nil);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:03  sg
  Initial revision

}
