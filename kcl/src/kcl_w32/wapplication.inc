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
//   TApplication Implementation for Win32
// ===================================================================

constructor TApplication.Create;
var
  wnd: WndClass;
begin
 { FOnFormClosedSlot := @OnFormClosed;
  FOnQuitSlot := @Quit;
  FTitle := ATitle;    }
  FForms := TList.Create;
  _fontDC := GetDC(0);
  FillStdWndClass(wnd);
  if Windows.RegisterClass(wnd)=0 then writeln('KCL - Class Registration Failure!')
  else writeln('KCL - Kassandra Component Library');

  _systemFont := TFont.Create;
  _systemFont.Fontname := 'MS Sans Serif';
  _systemFont.Height := 8;
  _systemFont.Allocate;
  //system cursors:
  _syscurDefault := LoadCursor(0, IDC_ARROW);
  _syscurSizeNS := LoadCursor(0, IDC_SIZENS);
  _syscurSizeWE := LoadCursor(0, IDC_SIZEWE);
end;

destructor TApplication.Destroy;
begin
  _systemFont.Free;
  Windows.UnregisterClass('KCLWIDGET', MainInstance);
  DestroyCursor(_syscurSizeNS);
  DestroyCursor(_syscurSizeWE);
  DestroyCursor(_syscurDefault);
  ReleaseDC(0, _fontDC); 
end;

procedure TApplication.AddForm(aform: TGenericForm);
begin
  FForms.Add(aform);
end;

procedure TApplication.RemoveForm(AForm: TGenericForm);
begin
  FForms.Remove(AForm);
  if FForms.Count = 0 then Quit(0);
end;

procedure TApplication.Run;
var
  i: Integer;
  AMessage: Msg;
begin
  for i := 0 to FForms.Count - 1 do
    TForm(FForms.Items[i]).FinishCreation;
  writeln('go! go! go!');
  while GetMessage(@AMessage, 0, 0, 0) do begin
    TranslateMessage(AMessage);
    DispatchMessage(AMessage);
  end;
  Quit(AMessage.wParam);
end;

procedure TApplication.Quit(ExitCode: Integer);
begin
  WriteLn('<<TApplication.Quit>> received');
  Halt(ExitCode);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:04  sg
  Initial revision

}
