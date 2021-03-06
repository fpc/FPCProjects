{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   GTK+ specific implementation of TTimer component
// ===================================================================

function TTimer_TimerProc(timer: TTimer): Integer; cdecl;
begin
  timer.Timer;
  Result := 1;
end;

procedure TTimer.Update;
begin
  if Handle <> 0 then begin
    gtk_timeout_remove(Handle);
    Handle := 0;
  end;

  if FEnabled then
    Handle := gtk_timeout_add(FInterval, TGtkFunction(@TTimer_TimerProc), Self);
end;


{
  $Log$
  Revision 1.3  2000/02/22 14:41:38  sg
  * Fixed TTimer_TimerProc: Now a result value is set so that the timer
    always continues. Before sometimes it stopped after the first tick.

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:45  sg
  Initial import

}
