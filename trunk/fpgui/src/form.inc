{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Form implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomForm
// ===================================================================

constructor TCustomForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not Assigned(FStyle) then
    FStyle := Application.DefaultStyle;

  FWnd := Application.Display.CreateWindow;
  Wnd.OnClose := @WndClose;
  Wnd.OnFocusIn := @WndFocusIn;
  Wnd.OnFocusOut := @WndFocusOut;
  Wnd.OnHide := @WndHide;
  Wnd.OnKeyPressed := @WndKeyPressed;
  Wnd.OnKeyReleased := @WndKeyReleased;
  Wnd.OnKeyChar := @WndKeyChar;
  Wnd.OnMouseEnter := @WndMouseEnter;
  Wnd.OnMouseLeave := @WndMouseLeave;
  Wnd.OnMouseMove := @WndMouseMoved;
  Wnd.OnMousePressed := @WndMousePressed;
  Wnd.OnMouseReleased := @WndMouseReleased;
  Wnd.OnMouseWheel := @WndMouseWheel;
  Wnd.OnPaint := @WndPaint;
  Wnd.OnResize := @WndResize;
  Wnd.OnShow := @WndShow;

  if Length(Text) = 0 then
    Wnd.Title := Application.Title;
end;

destructor TCustomForm.Destroy;
begin
  if Assigned(FWnd) then
    FWnd.Free;
  Application.FForms.Remove(Self);
  inherited Destroy;
end;

procedure TCustomForm.Show;
begin
  LAYOUTTRACE('TCustomForm.Show for %s:%s', [Name, ClassName]);
  { Don't call the inherited method, as we process the hide & show events
    delivered by fpGFX for forms. }
  if WidgetState * [wsGetsVisible, wsIsVisible] = [] then
  begin
    Include(WidgetState, wsGetsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
    Update;
    FWnd.Show;
  end;
end;

procedure TCustomForm.Close;
begin
  FWnd.Free;
  FWnd := nil;
end;

procedure TCustomForm.WidgetCoords(AWidget: TWidget; var OutX, OutY: Integer);
begin
  OutX := 0;
  OutY := 0;
  while AWidget <> Self do
  begin
    Inc(OutX, AWidget.Left);
    Inc(OutY, AWidget.Top);
    AWidget := AWidget.Parent;
  end;
end;

procedure TCustomForm.Loaded;
begin
  inherited Loaded;
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TCustomForm.Paint(Canvas: TGfxCanvas);
begin
  Style.DrawWindowBackground(Canvas, Rect(0, 0, Width, Height));
end;


// fpGUI event handlers

function TCustomForm.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event is TResizeEventObj then
    Resized
  else if Event is TDestroyEventObj then
    FWnd := nil;

  Result := inherited ProcessEvent(Event);
end;

procedure TCustomForm.EvRecalcLayout;
var
  OrigW, OrigH: Integer;
begin
  if Assigned(Child) then
  begin
    MinW := Child.MinW + 2 * BorderWidth;
    MinH := Child.MinH + 2 * BorderWidth;
    DefW := Child.DefW + 2 * BorderWidth;
    DefH := Child.DefH + 2 * BorderWidth;
    MaxW := Min(InfiniteSize, Child.MaxW + 2 * BorderWidth);
    MaxH := Min(InfiniteSize, Child.MaxH + 2 * BorderWidth);
  end;

  Wnd.SetMinMaxSize(MinW, MinH, MaxW, MaxH);

  OrigW := Width;
  OrigH := Height;

  if (Width < MinW) or (Height < MinW) or (Width > MaxW) or (Height > MaxH) then
  begin
    if Width < MinW then
      FWidth := MinW;
    if Height < MinH then
      FHeight := MinH;
    if Width > MaxW then
      FWidth := MaxW;
    if Height > MaxH then
      FHeight := MaxH;
    Wnd.SetSize(Width, Height);
  end;

  if not FResizedByUser then
  begin
    FWidth := DefW;
    FHeight := DefH;
    if (Width <> OrigW) or (Height <> OrigH) then
    begin
      LAYOUTTRACE('TCustomForm.EvRecalcLayout for %s:%s: Setting size to %dx%d',
        [Name, ClassName, DefW, DefH]);
      Wnd.SetSize(Width, Height);
      Resized;
    end;
  end;
end;

procedure TCustomForm.EvTextChanged;
begin
  Wnd.Title := Text;
end;


// TCustomForm private methods

procedure TCustomForm.Resized;
begin
  if Assigned(Child) then
    Child.SetBounds(BorderWidth, BorderWidth,
      Width - 2 * BorderWidth, Height - 2 * BorderWidth);
end;

procedure TCustomForm.SetFocusedWidget(AWidget: TWidget);
begin
  if AWidget <> FocusedWidget then
  begin
    if Assigned(FocusedWidget) then
    begin
      Exclude(FFocusedWidget.WidgetState, wsHasFocus);
      FocusedWidget.EvFocusChanged;
    end;
    FFocusedWidget := AWidget;
    Include(FFocusedWidget.WidgetState, wsHasFocus);
    FocusedWidget.EvFocusChanged;
  end;
end;

procedure TCustomForm.SetMouseCaptureWidget(AWidget: TWidget);
begin
  if AWidget <> FMouseCaptureWidget then
  begin
    FMouseCaptureWidget := AWidget;
    if Assigned(AWidget) then
      Wnd.CaptureMouse
    else
      Wnd.ReleaseMouse;
  end;
end;


// GfxWindow message handlers

procedure TCustomForm.WndClose(Sender: TObject);
begin
  SendEvent(TDestroyEventObj.Create(Self));
  FMouseCaptureWidget := nil;
end;

procedure TCustomForm.WndFocusIn(Sender: TObject);
begin
  FIsActive := True;
  if Assigned(FocusedWidget) then
    FocusedWidget.EvFocusChanged;
end;

procedure TCustomForm.WndFocusOut(Sender: TObject);
begin
  FIsActive := False;
  if Assigned(FocusedWidget) then
    FocusedWidget.EvFocusChanged;
end;

procedure TCustomForm.WndHide(Sender: TObject);
begin
  LAYOUTTRACE('TCustomForm.WndHide for %s:%s', [Name, ClassName]);
  if wsIsVisible in WidgetState then
  begin
    Exclude(WidgetState, wsIsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
    Update;
  end;
end;

procedure TCustomForm.WndKeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyPressed(Key, Shift)
  else
    EvKeyPressed(Key, Shift);
end;

procedure TCustomForm.WndKeyReleased(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyReleased(Key, Shift)
  else
    EvKeyReleased(Key, Shift);
end;

procedure TCustomForm.WndKeyChar(Sender: TObject; KeyChar: Char);
begin
  if Assigned(FocusedWidget) then
    FocusedWidget.EvKeyChar(KeyChar)
  else
    EvKeyChar(KeyChar);
end;

procedure TCustomForm.WndMouseEnter(Sender: TObject; Shift: TShiftState; x, y: Integer);
begin
  if wsEnabled in WidgetState then
    SendEvent(TMouseEnterEventObj.Create(Self, Shift, x, y));
end;

procedure TCustomForm.WndMouseLeave(Sender: TObject);
begin
  if wsEnabled in WidgetState then
    SendEvent(TMouseLeaveEventObj.Create(Self));
end;

procedure TCustomForm.WndMouseMoved(Sender: TObject; Shift: TShiftState; x, y: Integer);
{var
  dx, dy: Integer;
  IsInside: Boolean;
begin
  if Assigned(MouseCaptureWidget) then
  begin
    WidgetCoords(MouseCaptureWidget, dx, dy);

    // Emulate MouseEnter/MouseLeave events
    IsInside := (x >= dx) and (y >= dy) and
      (x < dx + MouseCaptureWidget.Width) and (y < dy + MouseCaptureWidget.Height);
    if IsInside and not (wsMouseInside in MouseCaptureWidget.WidgetState) then
      MouseCaptureWidget.EvMouseEnter(Shift, x - dy, y - dy)
    else if (not IsInside) and (wsMouseInside in MouseCaptureWidget.WidgetState) then
      MouseCaptureWidget.EvMouseLeave;

    MouseCaptureWidget.SendEvent(
     TMouseMovedEventObj.Create(Self, Shift, x - dx, y - dy));
  end else}
var
  dx, dy: Integer;
begin
  if wsEnabled in WidgetState then
    if Assigned(MouseCaptureWidget) then
    begin
      WidgetCoords(MouseCaptureWidget, dx, dy);
      MouseCaptureWidget.SendEvent(
        TMouseMoveEventObj.Create(Self, Shift, x - dx, y - dy));
      if not Assigned(MouseCaptureWidget) then
	SendEvent(TMouseMoveEventObj.Create(Self, Shift, x, y));
    end else
      SendEvent(TMouseMoveEventObj.Create(Self, Shift, x, y));
end;

procedure TCustomForm.WndMousePressed(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  dx, dy: Integer;
begin
  if wsEnabled in WidgetState then
    if Assigned(MouseCaptureWidget) then
    begin
      WidgetCoords(MouseCaptureWidget, dx, dy);
      MouseCaptureWidget.SendEvent(
        TMousePressedEventObj.Create(Self, Button, Shift, x - dx, y - dy));
      if not Assigned(MouseCaptureWidget) then
	SendEvent(TMouseMoveEventObj.Create(Self, Shift, x, y));
    end else
      SendEvent(TMousePressedEventObj.Create(Self, Button, Shift, x, y));
end;

procedure TCustomForm.WndMouseReleased(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  dx, dy: Integer;
begin
  if wsEnabled in WidgetState then
    if Assigned(MouseCaptureWidget) then
    begin
      WidgetCoords(MouseCaptureWidget, dx, dy);
      MouseCaptureWidget.SendEvent(
        TMouseReleasedEventObj.Create(Self, Button, Shift, x - dx, y - dy));
      if not Assigned(MouseCaptureWidget) then
	SendEvent(TMouseMoveEventObj.Create(Self, Shift, x, y));
    end else
      SendEvent(TMouseReleasedEventObj.Create(Self, Button, Shift, x, y));
end;

procedure TCustomForm.WndMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta, x, y: Integer);
begin
  if wsEnabled in WidgetState then
    SendEvent(TMouseWheelEventObj.Create(Self, Shift, WheelDelta, x, y));
end;

procedure TCustomForm.WndPaint(Sender: TObject; const ARect: TRect);
begin
  LAYOUTTRACE('TCustomForm.WndPaint for %s:%s (%d/%d-%d/%d)',
    [Name, ClassName, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
  if wsIsVisible in WidgetState then
    SendEvent(TPaintEventObj.Create(Self, Wnd.Canvas));
end;

procedure TCustomForm.WndResize(Sender: TObject);
begin
  LAYOUTTRACE('TCustomForm.WndResize for %s:%s: New size is %dx%d. Visible? %d',
    [Name, ClassName, Wnd.Width, Wnd.Height, Ord(wsIsVisible in WidgetState)]);
  if wsIsVisible in WidgetState then
  begin
    FResizedByUser := (Wnd.Width <> DefW) or (Wnd.Height <> DefH);
    SetBounds(Left, Top, Wnd.Width, Wnd.Height);
  end;
end;

procedure TCustomForm.WndShow(Sender: TObject);
begin
  LAYOUTTRACE('TCustomForm.WndShow for %s:%s', [Name, ClassName]);
  if WidgetState * [wsGetsVisible, wsIsVisible] = [wsGetsVisible] then
  begin
    Exclude(WidgetState, wsGetsVisible);
    Include(WidgetState, wsIsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
  end;
end;


{
  $Log$
  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
