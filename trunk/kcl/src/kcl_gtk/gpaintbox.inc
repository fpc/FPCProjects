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


// -------------------------------------------------------------------
//   GTK signal handlers
// -------------------------------------------------------------------

procedure TPaintBox_Expose(GtkWidget: PGtkWidget; event: PGdkEventExpose;
  PaintBox: TPaintBox); cdecl;
var
  rect: TRect;
  Canvas: TCanvas;
begin
  if Assigned(GtkWidget) and Assigned(PaintBox.OnPaint) and
    (PaintBox.FCreationState = wcsCreationDone) and
    (PaintBox.BlockDrawingCounter = 0) then begin

    rect.Left := event^.area.x;
    rect.Top := event^.area.y;
    rect.Right := rect.Left + event^.area.width;
    rect.Bottom := rect.Top + event^.area.height;

    Canvas := TGdkExposeCanvas.Create(PaintBox);
    gdk_gc_set_clip_rectangle(Canvas.priv.gc, @event^.area);
    PaintBox.DoPaint(Canvas, rect);
    Canvas.Free;
  end;
end;

function TPaintBox_KeyPressed(GtkWidget: PGtkWidget; Event: PGdkEventKey;
  PaintBox: TPaintBox): Integer; cdecl;
var
  KeyState: Integer;
  Key: Char;
  KeyCode: LongWord;
  ShiftState: TShiftState;
begin
  Result := 1;
  if not Assigned(PaintBox.OnKey) then exit;

  KeyCode := 0;
  Key := #0;

  // WriteLn('KeyVal ', Event^.KeyVal);

  case Event^.KeyVal of
    32..255:
      if (Event^.State and $4c) = 0 then Key := Chr(Event^.KeyVal)
      else begin
        KeyCode := Event^.KeyVal;
	if (KeyCode >= Ord('a')) and (KeyCode <= Ord('z')) then
	  Dec(KeyCode, Ord('a') - Ord('A'));
      end;
    // Cursor block / keypad
    GDK_Insert,    GDK_KP_Insert:    KeyCode := keyInsert;
    GDK_Home,      GDK_KP_Home:      KeyCode := keyHome;
    GDK_Left,      GDK_KP_Left:      KeyCode := keyLeft;
    GDK_Up,        GDK_KP_Up:        KeyCode := keyUp;
    GDK_Right,     GDK_KP_Right:     KeyCode := keyRight;
    GDK_Down,      GDK_KP_Down:      KeyCode := keyDown;
    GDK_Page_Up,   GDK_KP_Page_Up:   KeyCode := keyPageUp;
    GDK_Page_Down, GDK_KP_Page_Down: KeyCode := keyPageDown;
    GDK_End,       GDK_KP_End:       KeyCode := keyEnd;
    // Function keys
    GDK_F1..GDK_F35: KeyCode := keyF1 + (Event^.KeyVal - GDK_F1);
  end;

  if (Key = #0) and (StrLen(Event^.theString) > 0) then begin
    Key := Event^.theString[0];
    // Translate special characters
    case Key of
      #8:   KeyCode := keyBackspace;
      #9:   KeyCode := keyTab;
      #13:  KeyCode := keyReturn;
      #27:  KeyCode := keyEscape;
      #127: KeyCode := keyDelete;
      else if (Event^.State and $4c) <> 0 then Key := #0;
    end;
  end;

  ShiftState := [];
  KeyState := Event^.State;
  if (KeyState and 1) <> 0 then ShiftState := ShiftState + [ssShift];
  if (KeyState and 2) <> 0 then ShiftState := ShiftState + [ssCaps];
  if (KeyState and 4) <> 0 then ShiftState := ShiftState + [ssCtrl];
  if (KeyState and 8) <> 0 then ShiftState := ShiftState + [ssAlt];
  if (KeyState and $10) <> 0 then ShiftState := ShiftState + [ssNum];
  if (KeyState and $40) <> 0 then ShiftState := ShiftState + [ssSuper];
  if (KeyState and $80) <> 0 then ShiftState := ShiftState + [ssScroll];
  if (KeyState and $100) <> 0 then ShiftState := ShiftState + [ssLeft];
  if (KeyState and $200) <> 0 then ShiftState := ShiftState + [ssMiddle];
  if (KeyState and $400) <> 0 then ShiftState := ShiftState + [ssRight];
  if (KeyState and $2000) <> 0 then ShiftState := ShiftState + [ssAltGr];

  if (Key <> #0) or (KeyCode > 0) then
    PaintBox.DoKey(Key, KeyCode, ShiftState);
end;

function GdkToButton(button: Integer): TMouseButton;
begin
  case button of
    1: Result := mbLeft;
    3: Result := mbRight;
    // map (button=2) and all unknown buttons to middle button:
    else Result := mbMiddle;
  end;
end;

function GdkToShiftState(state: Integer): TShiftState;
begin
  {if (state and GDK_SHIFT_MASK) <> 0 then Write('Shift ');
  if (state and GDK_LOCK_MASK) <> 0 then Write('Lock ');
  if (state and GDK_CONTROL_MASK) <> 0 then Write('Control ');
  if (state and GDK_MOD1_MASK) <> 0 then Write('Mod1 ');
  if (state and GDK_MOD2_MASK) <> 0 then Write('Mod2 ');
  if (state and GDK_MOD3_MASK) <> 0 then Write('Mod3 ');
  if (state and GDK_MOD4_MASK) <> 0 then Write('Mod4 ');
  if (state and GDK_MOD5_MASK) <> 0 then Write('Mod5 ');
  if (state and GDK_BUTTON1_MASK) <> 0 then Write('Button1 ');
  if (state and GDK_BUTTON2_MASK) <> 0 then Write('Button2 ');
  if (state and GDK_BUTTON3_MASK) <> 0 then Write('Button3 ');
  if (state and GDK_BUTTON4_MASK) <> 0 then Write('Button4 ');
  if (state and GDK_BUTTON5_MASK) <> 0 then Write('Button5 ');
  WriteLn;}
  Result := [];
  if (state and GDK_SHIFT_MASK)   <> 0 then Result := Result + [ssShift];
  if (state and GDK_LOCK_MASK)    <> 0 then Result := Result + [ssCaps];
  if (state and GDK_CONTROL_MASK) <> 0 then Result := Result + [ssCtrl];
  if (state and GDK_MOD1_MASK)    <> 0 then Result := Result + [ssAlt];
  if (state and GDK_MOD2_MASK)    <> 0 then Result := Result + [ssNum];
  if (state and GDK_MOD4_MASK)    <> 0 then Result := Result + [ssSuper];
  if (state and GDK_MOD5_MASK)    <> 0 then Result := Result + [ssScroll];
  if (state and GDK_BUTTON1_MASK) <> 0 then Result := Result + [ssLeft];
  if (state and GDK_BUTTON2_MASK) <> 0 then Result := Result + [ssMiddle];
  if (state and GDK_BUTTON3_MASK) <> 0 then Result := Result + [ssRight];
end;

function TPaintBox_ButtonPressEvent(GtkWidget: PGtkWidget; event: PGdkEventButton;
  PaintBox: TPaintBox): Integer; cdecl;
begin
  PaintBox.DoMouseButtonDown(GdkToButton(event^.button),
    GdkToShiftState(event^.state), Round(event^.x), Round(event^.y));
  Result := 1;
end;

function TPaintBox_ButtonReleaseEvent(GtkWidget: PGtkWidget; event: PGdkEventButton;
  PaintBox: TPaintBox): Integer; cdecl;
begin
  PaintBox.DoMouseButtonUp(GdkToButton(event^.button),
    GdkToShiftState(event^.state), Round(event^.x), Round(event^.y));
  Result := 1;
end;

function TPaintBox_MotionNotifyEvent(GtkWidget: PGtkWidget; event: PGdkEventMotion;
  PaintBox: TPaintBox): Integer; cdecl;
begin
  PaintBox.DoMouseMove(GdkToShiftState(event^.state),
    Round(event^.x), Round(event^.y));
  Result := 1;
end;


// -------------------------------------------------------------------
//   TCustomPaintBox
// -------------------------------------------------------------------

function TCustomPaintBox.CreateCanvas: TCanvas;
begin
  ASSERT(Assigned(Handle.Widget));
  Result := TGdkCanvas.Create(Handle.Widget^.window);
end;

procedure TCustomPaintBox.OnFinishCreation;
begin
  if not Assigned(Handle.Widget) then
    Handle.Widget := gtk_drawing_area_new;

  gtk_signal_connect_after(PGtkObject(Handle.Widget), 'expose-event',
    GTK_SIGNAL_FUNC(@TPaintBox_Expose), self);
  PGtkObject(Handle.Widget)^.flags := PGtkObject(Handle.Widget)^.flags or
    Ord(GTK_CAN_FOCUS);

  // GTK-connect all needed signals
  gtk_signal_connect_after(PGtkObject(Handle.Widget), 'key-press-event',
    GTK_SIGNAL_FUNC(@TPaintBox_KeyPressed), Self);
  gtk_signal_connect(PGtkObject(Handle.Widget), 'button-press-event',
    GTK_SIGNAL_FUNC(@TPaintBox_ButtonPressEvent), Self);
  gtk_signal_connect(PGtkObject(Handle.Widget), 'button-release-event',
    GTK_SIGNAL_FUNC(@TPaintBox_ButtonReleaseEvent), Self);
  gtk_signal_connect(PGtkObject(Handle.Widget), 'motion-notify-event',
    GTK_SIGNAL_FUNC(@TPaintBox_MotionNotifyEvent), Self);
  gtk_widget_add_events(Handle.Widget, GDK_BUTTON_PRESS_MASK or
    GDK_BUTTON_RELEASE_MASK or GDK_POINTER_MOTION_MASK);

  inherited OnFinishCreation;
end;

procedure TCustomPaintBox.OnSetSize;
begin
  ASSERT(Assigned(Handle.Widget));
  gtk_widget_set_usize(Handle.Widget, FWidth, FHeight);
end;


{
  $Log$
  Revision 1.2  2000/01/05 19:18:37  sg
  * All events are not implemented properly

  Revision 1.1.1.1  1999/12/30 21:33:50  sg
  Initial import

}
