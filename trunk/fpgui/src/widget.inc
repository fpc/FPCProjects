{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Basic widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Basic event objects
// ===================================================================

// -------------------------------------------------------------------
//   TEventObj
// -------------------------------------------------------------------

constructor TEventObj.Create(ASender: TObject);
begin
  Sender := ASender;
  RefCount := 1;
end;

procedure TEventObj.AddRef;
begin
  Inc(RefCount);
end;

procedure TEventObj.Release;
begin
  ASSERT(RefCount > 0);
  Dec(RefCount);
  if RefCount = 0 then
    Self.Free;
end;

function TEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  Inc(RefCount);
  Result := AChild.SendEvent(Self);
end;


// -------------------------------------------------------------------
//   Miscellaneous events
// -------------------------------------------------------------------

function TResizeEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  { This event is not propagated to children automatically, as each container
    widget will have to provide an individual layouting mechanism. }
  Result := False;
end;


function TPreparePaintEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if wsIsVisible in AChild.WidgetState then
    Result := inherited SendToChild(AChild)
  else
    Result := False;
end;


constructor TPaintingEventObj.Create(ASender: TObject; ACanvas: TGfxCanvas);
begin
  inherited Create(ASender);
  FCanvas := ACanvas;
end;

function TPaintEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if wsIsVisible in AChild.WidgetState then
  begin
    Canvas.SaveState;
    if Canvas.IntersectClipRect(Rect(AChild.Left, AChild.Top,
      AChild.Left + AChild.Width, AChild.Top + AChild.Height)) then
    begin
      Canvas.AppendTranslation(AChild.Left, AChild.Top);
      Result := inherited SendToChild(AChild);
    end else
      Result := False;
    Canvas.RestoreState;
  end else
    Result := False;
end;


// -------------------------------------------------------------------
//   Mouse events
// -------------------------------------------------------------------

constructor TMouseEventObj.Create(ASender: TObject; AShift: TShiftState;
  AMouseX, AMouseY: Integer);
begin
  inherited Create(ASender);
  Shift := AShift;
  MouseX := AMouseX;
  MouseY := AMouseY;
end;

function TMouseEventObj.SendToChild(AChild: TWidget): Boolean;
var
  OldX, OldY: Integer;
begin
  if (wsEnabled in AChild.WidgetState) and
    ((AChild = AChild.FindForm.MouseCaptureWidget) or
    ((MouseX >= AChild.Left) and (MouseY >= AChild.Top) and
    (MouseX < AChild.Left + AChild.Width) and
    (MouseY < AChild.Top + AChild.Height))) then
  begin
    // Store the old values, as they might get modified during AChild.SendEvent!
    OldX := MouseX;
    OldY := MouseY;
    MouseX := OldX - AChild.Left;
    MouseY := OldY - AChild.Top;
    Result := inherited SendToChild(AChild);
    MouseX := OldX;
    MouseY := OldY;
  end else
    Result := False;
end;

constructor TMouseButtonEventObj.Create(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; AMouseX, AMouseY: Integer);
begin
  inherited Create(ASender, AShift, AMouseX, AMouseY);
  Button := AButton;
end;

function TMouseMoveEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if (AChild = AChild.FindForm.MouseCaptureWidget) or
    ((MouseX >= AChild.Left) and (MouseY >= AChild.Top) and
    (MouseX < AChild.Left + AChild.Width) and
    (MouseY < AChild.Top + AChild.Height)) then
  begin
    if not (wsMouseInside in AChild.WidgetState) then
      AChild.SendEvent(TMouseEnterEventObj.Create(Self, Shift,
        MouseX - AChild.Left, MouseY - AChild.Top));
  end else
    if wsMouseInside in AChild.WidgetState then
      AChild.SendEvent(TMouseLeaveEventObj.Create(Self));
    
  Result := inherited SendToChild(AChild);
end;

function TMouseLeaveEventObj.SendToChild(AChild: TWidget): Boolean;
begin
  if wsMouseInside in AChild.WidgetState then
    Result := inherited SendToChild(AChild)
  else
    Result := False;
end;

constructor TMouseWheelEventObj.Create(ASender: TObject; AShift: TShiftState;
  AWheelDelta: Single; AMouseX, AMouseY: Integer);
begin
  inherited Create(ASender, AShift, AMouseX, AMouseY);
  WheelDelta := AWheelDelta;
end;


// ===================================================================
//   TWidget
// ===================================================================

constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetState := [wsEnabled];
  FCanExpandWidth := False;
  FCanExpandHeight := False;
  FEnabled := True;
end;

destructor TWidget.Destroy;
begin
  inherited Destroy;
end;

function TWidget.SendEvent(Event: TEventObj): Boolean;
{$IFDEF TraceEvents}
var
  i: Integer;
{$ENDIF}
begin
{$IFDEF TraceEvents}
  for i := 1 to EventNestingLevel do
    Write('  ');
  WriteLn(Event.ClassName, ' event for ', Name, ':', ClassName);
  Inc(EventNestingLevel);
{$ENDIF}

  Result := ProcessEvent(Event);

{$IFDEF TraceEvents}
  Dec(EventNestingLevel);
  for i := 1 to EventNestingLevel do
    Write('  ');
  if Result then
    WriteLn(Event.ClassName, ' event has been blocked.');
{$ENDIF}
  Event.Release;
end;

function TWidget.FindForm: TCustomForm;
var
  Widget: TWidget;
begin
  Widget := Self;
  while Assigned(Widget) and Assigned(Widget.Parent) do
    Widget := Widget.Parent;
  Result := TCustomForm(Widget);
end;

procedure TWidget.SetEmbeddedParent(AParent: TWidget);
begin
  FParent := AParent;
end;

procedure TWidget.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  LAYOUTTRACE('TWidget.SetBounds for %s:%s. Old size: %dx%d, new size: %dx%d',
    [Name, ClassName, Width, Height, AWidth, AHeight]);
  FLeft := ALeft;
  FTop := ATop;
  if (AWidth <> Width) or (AHeight <> Height) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    FClientRect := Rect(0, 0, Width, Height);
    SendEvent(TResizeEventObj.Create(Self));
  end;
end;

function TWidget.ClientToScreen(const APoint: TPoint): TPoint;
begin
  Result.x := APoint.x + Left;
  Result.y := APoint.y + Top;
  if Assigned(Parent) then
    Result := Parent.ClientToScreen(Result);
end;

procedure TWidget.Update;
begin
  LAYOUTTRACE('TWidget.Update for %s:%s', [Name, ClassName]);
  if wsIsUpdating in WidgetState then
    exit;

  Include(WidgetState, wsIsUpdating);
  SendEvent(TUpdateEventObj.Create(Self));
  Exclude(WidgetState, wsIsUpdating);
end;

procedure TWidget.RecalcLayout;
var
  OldW, OldH: Integer;
  x, y: Integer;
  Widget: TWidget;
begin
  if (csLoading in ComponentState) or
    (WidgetState * [wsGetsVisible, wsIsVisible] = []) then
    exit;

  LAYOUTTRACE('TWidget.RecalcLayout for %s:%s', [Name, ClassName]);

  OldW := Width;
  OldH := Height;

  MinW := 0;
  MinH := 0;
  DefW := 0;
  DefH := 0;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;

  EvRecalcLayout;

  if MinW = 0 then MinW := 1;
  if MinH = 0 then MinH := 1;
  if DefW < MinW then DefW := MinW;
  if DefH < MinH then DefH := MinH;

  if (not FCanExpandWidth) or (MaxW < DefW) then
    MaxW := DefW;
  if (not FCanExpandHeight) or (MaxH < DefH) then
    MaxH := DefH;

  if (DefW < OldW) or (DefH < OldH) or ((not (wsOpaque in WidgetStyle)) and
    ((DefW > OldW) or (DefH > OldH))) then
  begin
    x := 0;
    y := 0;
    Widget := Self;
    while Assigned(Widget.Parent) do
    begin
      Inc(x, Widget.Left);
      Inc(y, Widget.Top);
      Widget := Widget.parent;
    end;
    TCustomForm(Widget).Wnd.Invalidate(Rect(x, y,
      x + Max(Width, OldW), y + Max(Height, OldH)));
  end;

  if Assigned(Parent) and not (wsIsUpdating in Parent.WidgetState) then
    Parent.RecalcLayout;
end;

procedure TWidget.Show;
begin
  if WidgetState * [wsGetsVisible, wsIsVisible] = [] then
  begin
    LAYOUTTRACE('TWidget.Show for %s:%s', [Name, ClassName]);
    // !!!: This implementation is currenty very, very wrong
    Include(WidgetState, wsIsVisible);
    SendEvent(TVisibilityChangeEventObj.Create(Self));
    if wsIsUpdating in Parent.WidgetState then
      Parent.SendEvent(TUpdateEventObj.Create(Self))
    else
      Parent.Update;
  end;
end;

procedure TWidget.Hide;
begin
  if wsIsVisible in WidgetState then
  begin
    LAYOUTTRACE('TWidget.Hide for %s:%s', [Name, ClassName]);
    WidgetState := WidgetState - [wsGetsVisible, wsIsVisible];
    SendEvent(TVisibilityChangeEventObj.Create(Self));
    Parent.Update;
  end else
    Exclude(WidgetState, wsGetsVisible);
end;

procedure TWidget.Redraw;
begin
  Redraw(Rect(0, 0, Width, Height));
end;

procedure TWidget.Redraw(const ARect: TRect);
var
  x, y: Integer;
  Widget: TWidget;
begin
  if not (wsIsVisible in WidgetState) then
    exit;

  x := ARect.Left;
  y := ARect.Top;
  Widget := Self;
  while Assigned(Widget.Parent) do
  begin
    Inc(x, Widget.Left);
    Inc(y, Widget.Top);
    Widget := Widget.parent;
  end;
  TCustomForm(Widget).Wnd.Invalidate(Rect(x, y,
    x + ARect.Right - ARect.Left, y + ARect.Bottom - ARect.Top));
end;

procedure TWidget.Scroll(const ARect: TRect; DeltaX, DeltaY: Integer);
var
  r: TRect;
  Widget: TWidget;
begin
  if not (wsIsVisible in WidgetState) then
    exit;

  r := ARect;

  Widget := Self;
  while Assigned(Widget.Parent) do
  begin
    Inc(r.Left, Widget.Left);
    Inc(r.Top, Widget.Top);
    Inc(r.Right, Widget.Left);
    Inc(r.Bottom, Widget.Top);
    Widget := Widget.parent;
  end;

  TCustomForm(Widget).Wnd.Canvas.CopyRect(TCustomForm(Widget).Wnd.Canvas, r,
    r.Left + DeltaX, r.Top + DeltaY);


  // Redraw the areas which has been scrolled in

  if DeltaX <> 0 then
  begin
    r := ARect;
    if DeltaX < 0 then	// Scrolling to the right size
      r.Left := r.Right + DeltaX
    else			// Scrolling to the left size
      r.Right := r.Left + DeltaX;
    Redraw(r);
  end;

  if DeltaY <> 0 then
  begin
    r := ARect;
    if DeltaY < 0 then	// Scrolling downwards
      r.Top := r.Bottom + DeltaY
    else			// Scrolling upwards
      r.Bottom := r.Top + DeltaY;
    Redraw(r);
  end;
end;

procedure TWidget.SetFocus;
begin
  FindForm.FocusedWidget := Self;
end;


// Protected methods

procedure TWidget.Loaded;
begin
  inherited Loaded;
{!!!:  if not (wsEnabled in WidgetState) then
    UpdateEnabledState;}
end;

procedure TWidget.Click;
begin
  Redraw;
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TWidget.Paint(Canvas: TGfxCanvas);
begin
  // Do nothing here.
end;

procedure TWidget.SetParentComponent(AParent: TComponent);
begin
  if AParent.InheritsFrom(TContainerWidget) then
    SetParent(TContainerWidget(AParent));
end;

procedure TWidget.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> Enabled then
  begin
    FEnabled := AEnabled;
    SendEvent(TEnabledChangeEventObj.Create(Self));
  end;
end;

procedure TWidget.EvFocusChanged;
begin
  Redraw;
end;

procedure TWidget.EvKeyPressed(Key: Word; Shift: TShiftState);

  function SetFocusIfPossible(Widget: TWidget): Boolean;
  begin
    Result := (wsClickable in Widget.WidgetStyle) and
      (wsEnabled in Widget.WidgetState);
    if Result then
      Widget.SetFocus;
  end;

  function FocusFirst(Widget: TWidget): Boolean;
  var
    i: Integer;
    Child: TWidget;
  begin
    Result := False;
    if not Widget.InheritsFrom(TContainerWidget) then
      exit;

    for i := 0 to TContainerWidget(Widget).ChildCount - 1 do
    begin
      Child := TContainerWidget(Widget).Children[i];
      if SetFocusIfPossible(Child) or FocusFirst(Child) then
      begin
        Result := True;
	exit;
      end;
    end;
  end;

  function FocusLast(Widget: TWidget): Boolean;
  var
    i: Integer;
    Child: TWidget;
  begin
    Result := False;
    if not Widget.InheritsFrom(TContainerWidget) then
      exit;

    for i := TContainerWidget(Widget).ChildCount - 1 downto 0 do
    begin
      Child := TContainerWidget(Widget).Children[i];
      if SetFocusIfPossible(Child) or FocusLast(Child) then
      begin
        Result := True;
	exit;
      end;
    end;
  end;

var
  mshift: TShiftState;
  i: Integer;
  CurWidget: TWidget;
  CurParent: TContainerWidget;
begin
  // Handle focus movement keys
  mshift := Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  if ((mshift = []) and ((Key = keyLeft) or (Key = keyUp))) or
    ((mshift = [ssShift]) and (Key = keyTab)) then
    if Assigned(Parent) then
    begin
      CurWidget := Self;
      CurParent := TContainerWidget(Parent);
      while Assigned(CurParent) and CurParent.InheritsFrom(TContainerWidget) do
      begin
        for i := CurParent.ChildCount - 1 downto 1 do
	  if CurParent.Children[i] = CurWidget then
	  begin
	    Dec(i);
	    while i >= 0 do
	    begin
	      if SetFocusIfPossible(CurParent.Children[i]) or
	        FocusLast(CurParent.Children[i]) then
		exit;
	      Dec(i);
	    end;
	    break;
	  end;
	CurParent := TContainerWidget(CurParent.Parent);
      end;
    end else
    begin
      if FocusLast(Self) then
        exit
    end
  else if (mshift = []) and
    ((Key = keyRight) or (Key = keyDown) or (Key = keyTab)) then
    if Assigned(Parent) then
    begin
      CurWidget := Self;
      CurParent := TContainerWidget(Parent);
      while Assigned(CurParent) and CurParent.InheritsFrom(TContainerWidget) do
      begin
        for i := 0 to CurParent.ChildCount - 2 do
	  if CurParent.Children[i] = CurWidget then
	  begin
	    Inc(i);
	    while i < CurParent.ChildCount do
	    begin
	      if SetFocusIfPossible(CurParent.Children[i]) or
	        FocusFirst(CurParent.Children[i]) then
		exit;
	      Inc(i);
	    end;
	    break;
	  end;
	CurParent := TContainerWidget(CurParent.Parent);
      end;
    end else
    begin
      if FocusFirst(Self) then
        exit;
    end
  else if (Key = Ord(' ')) and (wsClickable in WidgetStyle) then
  begin
    Click;
    exit;
  end;

  if Assigned(Parent) then
    Parent.EvKeyPressed(Key, Shift);
end;

procedure TWidget.EvKeyReleased(Key: Word; Shift: TShiftState);
begin
  if Assigned(Parent) then
    Parent.EvKeyReleased(Key, Shift);
end;

procedure TWidget.EvKeyChar(KeyChar: Char);
begin
  if Assigned(Parent) then
    Parent.EvKeyChar(KeyChar);
end;

procedure TWidget.EvTextChanged;
begin
  RecalcLayout;
  Redraw;
end;

function TWidget.GetStyle: TStyle;
var
  Widget: TWidget;
begin
  if Assigned(FStyle) then
    Result := FStyle
  else
  begin
    ASSERT(Assigned(Parent));
    // Don't use recursive calls here, they are not necessary
    Widget := Parent;
    while not Assigned(Widget.FStyle) do
    begin
      Widget := Widget.Parent;
      ASSERT(Assigned(Widget));
    end;
    Result := Widget.FStyle;
  end;
end;

function TWidget.ProcessEvent(Event: TEventObj): Boolean;
var
  Canvas: TGfxCanvas;
  Matrix: TGfxMatrix;
  PreparationEvent: TPreparePaintEventObj;
begin
  // Handle events which must be processed before the children get them
  if Event.InheritsFrom(TEnabledChangeEventObj) then
    Result := EvEnabledChange(TEnabledChangeEventObj(Event))
  else if Event.InheritsFrom(TVisibilityChangeEventObj) then
    Result := EvVisibilityChange(TVisibilityChangeEventObj(Event)) or
      DistributeEvent(Event)
  else if Event.InheritsFrom(TPreparePaintEventObj) then
  begin
    Canvas := TPreparePaintEventObj(Event).Canvas;
    if wsOpaque in WidgetStyle then
    begin
      Canvas.ExcludeClipRect(Rect(Left, Top, Left + Width, Top + Height));
      Result := False;
    end else
    begin
      Matrix := Canvas.Matrix;
      Canvas.AppendTranslation(Left, Top);
      Result := DistributeEvent(Event);
      Canvas.Matrix := Matrix;
    end;
  end else if Event.InheritsFrom(TPaintEventObj) then
  begin
    Canvas := TPaintEventObj(Event).Canvas;
    Canvas.SaveState;
    PreparationEvent := TPreparePaintEventObj.Create(Self, Canvas);
    DistributeEvent(PreparationEvent);
    PreparationEvent.Release;
    Paint(Canvas);
    Canvas.RestoreState;
    Result := DistributeEvent(Event);
  end else
  begin
    // First distribute the event to all children, then try to handle them here
    Result := DistributeEvent(Event);
    if not Result then
      if Event.InheritsFrom(TUpdateEventObj) then
        Result := EvUpdate(TUpdateEventObj(Event))
      else if Event.InheritsFrom(TMousePressedEventObj) then
	Result := EvMousePressed(TMousePressedEventObj(Event))
      else if Event.InheritsFrom(TMouseReleasedEventObj) then
	Result := EvMouseReleased(TMouseReleasedEventObj(Event))
      else if Event.InheritsFrom(TMouseEnterEventObj) then
	Result := EvMouseEnter(TMouseEnterEventObj(Event))
      else if Event.InheritsFrom(TMouseLeaveEventObj) then
	Result := EvMouseLeave(TMouseLeaveEventObj(Event))
  end;
end;

function TWidget.DistributeEvent(Event: TEventObj): Boolean;
begin
  // Do nothing here, as TWidget itself doesn't has children
  Result := False;
end;

procedure TWidget.SetParent(AParent: TWidget);
begin
  // !!!: reparenting when changing the form is not possible
  if AParent <> FParent then
  begin
    // Remove the widget from the old parent, if it had a parent...
    if Assigned(FParent) and FParent.InheritsFrom(TContainerWidget) then
      TContainerWidget(FParent).RemoveChild(Self);

    FParent := AParent;

    // ...and add it to the new parent, if existent.
    if Assigned(Parent) and Parent.InheritsFrom(TContainerWidget) then
      TContainerWidget(Parent).InsertChild(Self);
  end;
end;

procedure TWidget.SetCanExpandWidth(allow: Boolean);
begin
  if FCanExpandWidth <> allow then
  begin
    FCanExpandWidth := allow;
    RecalcLayout;
  end;
end;

procedure TWidget.SetCanExpandHeight(allow: Boolean);
begin
  if FCanExpandHeight <> allow then
  begin
    FCanExpandHeight := allow;
    RecalcLayout;
  end;
end;

procedure TWidget.SetText(const AText: String);
begin
  if AText <> Text then
  begin
    FText := AText;
    EvTextChanged;
  end;
end;

// Old event handlers

procedure TWidget.EvRecalcLayout;
begin
  LAYOUTTRACE('TWidget.EvRecalcLayout for %s:%s', [Name, ClassName]);
end;


// Event handling

function TWidget.EvUpdate(Event: TUpdateEventObj): Boolean;
begin
  RecalcLayout;
  Result := False;
end;

function TWidget.EvEnabledChange(Event: TEnabledChangeEventObj): Boolean;
var
  NewState: Boolean;
begin
  if Assigned(Parent) then
    NewState := Enabled and (wsEnabled in Parent.WidgetState)
  else
    NewState := Enabled;

  if NewState and not (wsEnabled in WidgetState) then
  begin
    Include(WidgetState, wsEnabled);
    Redraw;
    Result := DistributeEvent(Event);
  end else if (not NewState) and (wsEnabled in WidgetState) then
  begin
    Exclude(WidgetState, wsEnabled);
    Redraw;
    Result := DistributeEvent(Event);
  end else
    Result := False;
end;

function TWidget.EvVisibilityChange(Event: TVisibilityChangeEventObj): Boolean;
begin
  if (Event.Sender <> Self) and Assigned(Parent) then
  begin
    if wsGetsVisible in Parent.WidgetState then
      Include(WidgetState, wsGetsVisible)
    else
      Exclude(WidgetState, wsGetsVisible);
    if wsIsVisible in Parent.WidgetState then
      Include(WidgetState, wsIsVisible)
    else
      Exclude(WidgetState, wsIsVisible);
    LAYOUTTRACE('TWidget.EvVisibilityChange for %s:%s: New state: GetsVisible=%d, IsVisible=%d',
      [Name, ClassName, Ord(wsGetsVisible in WidgetState), Ord(wsIsVisible in WidgetState)]);
  end;
  Result := False;
end;

function TWidget.EvMousePressed(Event: TMousePressedEventObj): Boolean;
begin
  if (wsClickable in WidgetStyle) and (wsEnabled in WidgetState) and
    (Event.Button = mbLeft) then
  begin
    if wsCaptureMouse in WidgetStyle then
      FindForm.MouseCaptureWidget := Self;
    Include(WidgetState, wsClicked);
    Include(WidgetState, wsHasFocus);
    SetFocus;
    Redraw;
    Result := True;
  end else
    Result := False;
end;

function TWidget.EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
begin
  if (wsClickable in WidgetStyle) and (wsEnabled in WidgetState) and
    (Event.Button = mbLeft) then
  begin
    if wsClicked in WidgetState then
    begin
      Exclude(WidgetState, wsClicked);
      with Event do
        if (MouseX >= 0) and (MouseY >= 0) and
	  (MouseX < Width) and (MouseY < Height) then
          Click
        else
          Redraw;
    end;
    if wsCaptureMouse in WidgetStyle then
      FindForm.MouseCaptureWidget := nil;
    Result := True;
  end else
    Result := False;
end;

function TWidget.EvMouseEnter(Event: TMouseEnterEventObj): Boolean;
begin
  Include(WidgetState, wsMouseInside);
  if (wsClickable in WidgetStyle) and (wsClicked in WidgetState) and
    (wsEnabled in WidgetState) then
    Redraw;
  Result := False;
end;

function TWidget.EvMouseLeave(Event: TMouseLeaveEventObj): Boolean;
begin
  Exclude(WidgetState, wsMouseInside);
  if (wsClickable in WidgetStyle) and (wsClicked in WidgetState) and
    (wsEnabled in WidgetState) then
    Redraw;
  Result := False;
end;


{
  $Log$
  Revision 1.3  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
