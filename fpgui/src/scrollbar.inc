{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Scrollbar widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomScrollBar and helper classes
// ===================================================================

// -------------------------------------------------------------------
//   TScrollBarButton
// -------------------------------------------------------------------

type
  TScrollBarButton = class(TGenericButton)
  protected
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
  public
    Direction: TArrowDirection;
  end;

procedure TScrollBarButton.Paint(Canvas: TGfxCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawScrollBarButton(Canvas, Rect(0, 0, BoundsSize.cx, BoundsSize.cy),
     Direction, (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;

procedure TScrollBarButton.CalcSizes;
begin
  ASSERT(Owner is TCustomScrollBar);
  FMinSize := Style.GetScrollBarBtnSize(TCustomScrollBar(Owner).Orientation);
end;


// -------------------------------------------------------------------
//   TScrollBarSlider
// -------------------------------------------------------------------

type
  TScrollBarSlider = class(TWidget)
  private
    function EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function EvMouseMove(Event: TMouseMoveEventObj): Boolean;
  protected
    ButtonPos, ButtonSize: Integer;
    IsDraggingButton, PrevAreaPressed, NextAreaPressed: Boolean;
    DragStartMousePos, DragStartButtonPos, ButtonMoveSavedPosition: Integer;
    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    procedure CalcSizes; override;
    function CalcPosition: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateBar;
  end;


constructor TScrollBarSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;

procedure TScrollBarSlider.UpdateBar;
var
  Size: Integer;
begin
  ASSERT(Owner is TCustomScrollBar);

  if Visible then
  begin
    with TCustomScrollBar(Owner) do
    begin
      if (Min = Max) or (Max - Min = PageSize - 1) then
	ButtonPos := 0
      else
      begin
	if Orientation = Horizontal then
	  Size := Self.BoundsSize.cx
	else
	  Size := Self.BoundsSize.cy;
	if PageSize = 0 then
	  ButtonPos := (Position - Min) * (Size - ButtonSize) div (Max - Min)
        else
          ButtonPos := (Position - Min) *
	    (Size - ButtonSize) div (Max - Min - PageSize + 1);
      end;
      ButtonSize := GetButtonSize;
    end;
    Redraw;
  end;
end;

procedure TScrollBarSlider.Paint(Canvas: TGfxCanvas);
var
  Size: Integer;
  r: TRect;
  StartPos, EndPos: PInteger;
  Color2: TColor;
begin
  ASSERT(Owner is TCustomScrollBar);
  inherited Paint(Canvas);

  if TCustomScrollBar(Owner).Orientation = Horizontal then
  begin
    r.Top := 0;
    r.Bottom := Height;
    StartPos := @r.Left;
    EndPos := @r.Right;
    Size := Width;
  end else
  begin
    r.Left := 0;
    r.Right := Width;
    StartPos := @r.Top;
    EndPos := @r.Bottom;
    Size := Height;
  end;

  if ButtonPos > 0 then
  begin
    if PrevAreaPressed then
      Color2 := cl3DDkShadow
    else
      Color2 := cl3DLight;
    Canvas.SetColor(GetAvgColor(Style.GetUIColor(clScrollBar),
        Style.GetUIColor(Color2)));
    StartPos^ := 0;
    EndPos^ := ButtonPos;
    Canvas.FillRect(r);
  end;
  if ButtonPos + ButtonSize < Size then
  begin
    if NextAreaPressed then
      Color2 := cl3DDkShadow
    else
      Color2 := cl3DLight;
    Canvas.SetColor(GetAvgColor(Style.GetUIColor(clScrollBar),
        Style.GetUIColor(Color2)));
    StartPos^ := ButtonPos + ButtonSize;
    EndPos^ := Size;
    Canvas.FillRect(r);
  end;

  StartPos^ := ButtonPos;
  EndPos^ := ButtonPos + ButtonSize;
  Style.DrawButtonFace(Canvas, r, [btnIsEmbedded]);
end;

function TScrollBarSlider.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := False;
  if Event.InheritsFrom(TMousePressedEventObj) then
    Result := EvMousePressed(TMousePressedEventObj(Event))
  else if Event.InheritsFrom(TMouseReleasedEventObj) then
    Result := EvMouseReleased(TMouseReleasedEventObj(Event))
  else if Event.InheritsFrom(TMouseMoveEventObj) then
    Result := EvMouseMove(TMouseMoveEventObj(Event));

  if not Result then
    Result := inherited ProcessEvent(Event);

  if Event.InheritsFrom(TVisibilityChangeEventObj) and Visible then
    UpdateBar;
end;

function TScrollBarSlider.EvMousePressed(Event: TMousePressedEventObj): Boolean;
var
  Pos: Integer;
begin
  Result := inherited ProcessEvent(Event);	// For mouse grabbing support

  if Event.Button <> mbLeft then
    exit;

  if TCustomScrollBar(Owner).Orientation = Horizontal then
    Pos := Event.Position.x
  else
    Pos := Event.Position.y;

  if Pos < ButtonPos then
  begin
    PrevAreaPressed := True;
    TCustomScrollBar(Owner).PageUp
  end else if Pos > ButtonPos + ButtonSize then
  begin
    NextAreaPressed := True;
    TCustomScrollBar(Owner).PageDown
  end else
  begin
    IsDraggingButton := True;
    DragStartMousePos := Pos;
    DragStartButtonPos := ButtonPos;
  end;
  Result := True;
end;

function TScrollBarSlider.EvMouseReleased(
  Event: TMouseReleasedEventObj): Boolean;
var
  NewPosition: Integer;
begin
  Result := inherited ProcessEvent(Event);	// For mouse grabbing support

  if Event.Button <> mbLeft then
    exit;

  if IsDraggingButton then
  begin
    IsDraggingButton := False;
    NewPosition := CalcPosition;
    if NewPosition <> TCustomScrollBar(Owner).Position then
    begin
      if Assigned(TCustomScrollBar(Owner).OnScroll) then
        TCustomScrollBar(Owner).OnScroll(Owner, NewPosition);
      TCustomScrollBar(Owner).FPosition := NewPosition;
    end;
    if (NewPosition <> ButtonMoveSavedPosition) and
      Assigned(TCustomScrollBar(Owner).OnChange) then
      TCustomScrollBar(Owner).OnChange(Self);
    UpdateBar;
  end else if PrevAreaPressed then
  begin
    PrevAreaPressed := False;
    Redraw;
  end else if NextAreaPressed then
  begin
    NextAreaPressed := False;
    Redraw;
  end;
  Result := True;
end;

function TScrollBarSlider.EvMouseMove(Event: TMouseMoveEventObj): Boolean;
var
  Pos, Size, VirtualPos: Integer;
begin
  if IsDraggingButton then
  begin
    if wsMouseInside in WidgetState then
      if TCustomScrollBar(Owner).Orientation = Horizontal then
      begin
        Pos := Event.Position.x;
        Size := Width;
      end else
      begin
        Pos := Event.Position.y;
        Size := Height;
      end
    else
    begin
      Pos := DragStartMousePos;
      if TCustomScrollBar(Owner).Orientation = Horizontal then
        Size := Width
      else
        Size := Height;
    end;

    ButtonPos := ClipMinMax(DragStartButtonPos + Pos - DragStartMousePos,
      0, Size - ButtonSize);
    VirtualPos := CalcPosition;

    if VirtualPos <> TCustomScrollBar(Owner).Position then
    begin
      if Assigned(TCustomScrollBar(Owner).OnScroll) then
        TCustomScrollBar(Owner).OnScroll(Owner, VirtualPos);
      TCustomScrollBar(Owner).FPosition := VirtualPos;
    end;

    Redraw;
    Result := True
  end else
    Result := False;
end;

procedure TScrollBarSlider.CalcSizes;
begin
  if TCustomScrollBar(Owner).Orientation = Horizontal then
    FDefSize.cx := Style.GetScrollBarBtnSize(Horizontal).cy * 5
  else
    FDefSize.cy := Style.GetScrollBarBtnSize(Vertical).cx * 5;
end;

function TScrollBarSlider.CalcPosition: Integer;
var
  Size: Integer;
begin
  with TCustomScrollBar(Owner) do
  begin
    if Orientation = Horizontal then
      Size := Self.Width
    else
      Size := Self.Height;

    if Size = ButtonSize then
      Position := 0
    else
    begin
      if PageSize = 0 then
	Result := ButtonPos * (Max - Min + 1)
      else
	Result := ButtonPos * (Max - Min - PageSize + 2);
      Result := Result div (Size - ButtonSize);
      Result := Result + Min;
    end;
    Result := ClipPosition(Result);
  end;
end;


// -------------------------------------------------------------------
//   TCustomScrollBar
// -------------------------------------------------------------------

constructor TCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Include(WidgetStyle, wsOpaque);
  Embedded := False;

  FMax := 100;
  FSmallChange := 1;

  ButtonUp := TScrollBarButton.Create(Self);
  ButtonUp.Name := '#ScrollBarButtonUp';
  TScrollBarButton(ButtonUp).Direction := arrowLeft;
  ButtonUp.Embedded := True;
  ButtonUp.CanExpandWidth := False;
  ButtonUp.CanExpandHeight := False;
  ButtonUp.OnClick := @ButtonUpClick;
  ButtonUp.SetEmbeddedParent(Self);

  Slider := TScrollBarSlider.Create(Self);
  Slider.Name := '#ScrollBarSlider';
  Slider.SetEmbeddedParent(Self);

  ButtonDown := TScrollBarButton.Create(Self);
  ButtonDown.Name := '#ScrollBarButtonDown';
  TScrollBarButton(ButtonDown).Direction := arrowRight;
  ButtonDown.Embedded := True;
  ButtonDown.CanExpandWidth := False;
  ButtonDown.CanExpandHeight := False;
  ButtonDown.OnClick := @ButtonDownClick;
  ButtonDown.SetEmbeddedParent(Self);
end;

procedure TCustomScrollBar.LineUp;
begin
  Position := Position - SmallChange;
end;

procedure TCustomScrollBar.LineDown;
begin
  Position := Position + SmallChange;
end;

procedure TCustomScrollBar.PageUp;
var
  Diff: Integer;
begin
  if LargeChange = 0 then
  begin
    Diff := (Max - Min + 6) div 10;
    if Diff = 0 then
      Inc(Diff);
    Position := Position - Diff;
  end else
    Position := Position - LargeChange;
end;

procedure TCustomScrollBar.PageDown;
var
  Diff: Integer;
begin
  if LargeChange = 0 then
  begin
    Diff := (Max - Min + 6) div 10;
    if Diff = 0 then
      Inc(Diff);
    Position := Position + Diff;
  end else
    Position := Position + LargeChange;
end;

function TCustomScrollBar.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(Slider) or Event.SendToChild(ButtonUp) or
    Event.SendToChild(ButtonDown);
end;

procedure TCustomScrollBar.Paint(Canvas: TGfxCanvas);
begin
  if not Embedded then
    Style.DrawScrollBarBorder(Canvas, Rect(0, 0, Width, Height));
end;

procedure TCustomScrollBar.CalcSizes;
begin
  if Orientation = Horizontal then
  begin
    FMinSize.cx := ButtonUp.DefSize.cx + Slider.MinSize.cx + ButtonDown.DefSize.cx;
    FMinSize.cy := ButtonUp.DefSize.cy;
    FDefSize.cx := ButtonUp.DefSize.cx + Slider.DefSize.cx + ButtonDown.DefSize.cx;
    FDefSize.cy := ButtonUp.DefSize.cy;
    FMaxSize.cx := InfiniteSize;
    FMaxSize.cy := ButtonUp.DefSize.cy;
  end else
  begin
    FMinSize.cx := ButtonUp.DefSize.cx;
    FMinSize.cy := ButtonUp.DefSize.cy + Slider.MinSize.cy + ButtonDown.DefSize.cy;
    FDefSize.cx := ButtonUp.DefSize.cx;
    FDefSize.cy := ButtonUp.DefSize.cy + Slider.DefSize.cy + ButtonDown.DefSize.cy;
    FMaxSize.cx := ButtonUp.DefSize.cx;
    FMaxSize.cy := InfiniteSize;
  end;

  if not Embedded then
    with Style.GetScrollBarBorders(Orientation) do
    begin
      Inc(FMinSize.cx, Left + Right);
      Inc(FMinSize.cy, Top + Bottom);
      FDefSize.cx := fpGUI.Min(DefSize.cx + Left + Right, InfiniteSize);
      FDefSize.cy := fpGUI.Min(DefSize.cy + Top + Bottom, InfiniteSize);
      FMaxSize.cx := fpGUI.Min(MaxSize.cx + Left + Right, InfiniteSize);
      FMaxSize.cy := fpGUI.Min(MaxSize.cy + Top + Bottom, InfiniteSize);
    end;
end;

procedure TCustomScrollBar.Resized;
var
  r: TRect;
begin
  if not Embedded then
    with Style.GetScrollBarBorders(Orientation) do
    begin
      r.Left := Left;
      r.Top := Top;
      r.Right := Width - Right;
      r.Bottom := Height - Bottom;
    end
  else
  begin
    r.Left := 0;
    r.Top := 0;
    r.Right := Width;
    r.Bottom := Height;
  end;

  with r do
    if Orientation = Horizontal then
    begin
      ButtonUp.SetBounds(TopLeft, Size(ButtonUp.DefSize.cx, Bottom - Top));
      ButtonDown.SetBounds(Point(Right - ButtonDown.DefSize.cx, Top),
        Size(ButtonDown.DefSize.cx, Bottom - Top));
      Slider.SetBounds(Point(Left + ButtonUp.DefSize.cx, Top),
        Size(Right - Left - ButtonUp.DefSize.cx - ButtonDown.DefSize.cx,
	  Bottom - Top));
    end else
    begin
      ButtonUp.SetBounds(TopLeft, Size(Right - Left, ButtonDown.DefSize.cy));
      ButtonDown.SetBounds(Point(Left, Bottom - ButtonDown.DefSize.cy),
        Size(Right - Left, ButtonDown.DefSize.cy));
      Slider.SetBounds(Point(Left, Top + ButtonUp.DefSize.cy),
        Size(Right - Left,
	  Bottom - Top - ButtonUp.DefSize.cy - ButtonDown.DefSize.cy));
    end;

  UpdateBar;
end;

procedure TCustomScrollBar.ButtonUpClick(Sender: TObject);
begin
  LineUp;
end;

procedure TCustomScrollBar.ButtonDownClick(Sender: TObject);
begin
  LineDown;
end;

procedure TCustomScrollBar.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> Orientation then
  begin
    FOrientation := AOrientation;
    if Orientation = Horizontal then
    begin
      TScrollBarButton(ButtonUp).Direction := arrowLeft;
      TScrollBarButton(ButtonDown).Direction := arrowRight;
    end else
    begin
      TScrollBarButton(ButtonUp).Direction := arrowUp;
      TScrollBarButton(ButtonDown).Direction := arrowDown;
    end;
  end;
end;

procedure TCustomScrollBar.SetMin(AMin: Integer);
begin
  if AMin <> Min then
  begin
    FMin := AMin;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;

procedure TCustomScrollBar.SetMax(AMax: Integer);
begin
  if AMax <> Max then
  begin
    FMax := AMax;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;

procedure TCustomScrollBar.SetPageSize(APageSize: Integer);
begin
  if APageSize <> PageSize then
  begin
    FPageSize := APageSize;
    Position := Position;	// Do range clipping
    UpdateBar;
  end;
end;

procedure TCustomScrollBar.SetPosition(APosition: Integer);
begin
  APosition := ClipPosition(APosition);

  if (APosition <> Position) and Assigned(OnScroll) then
    OnScroll(Self, APosition);

  if APosition <> Position then
  begin
    FPosition := APosition;
    UpdateBar;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

function TCustomScrollBar.GetButtonSize: Integer;
var
  Size: Integer;
begin
  if PageSize = 0 then
    if Orientation = Horizontal then
      Result := Height
    else
      Result := Width
  else
  begin
    if Orientation = Horizontal then
      Size := Slider.Width
    else
      Size := Slider.Height;
    Result := fpGUI.Max(Style.GetScrollBarBtnMinSize,
      PageSize * Size div fpGUI.Max(1, Max - Min + 1));
    if Result > Size then
      Result := Size;
  end;
end;

function TCustomScrollBar.ClipPosition(APosition: Integer): Integer;
begin
  if APosition > Max - PageSize then
    if PageSize = 0 then
      Result := Max
    else
      Result := Max - PageSize + 1
  else
    Result := APosition;
  if Result < Min then
    Result := Min;
end;

procedure TCustomScrollBar.UpdateBar;
begin
  if Embedded then
    Visible := (Max > Min) and ((PageSize = 0) or (PageSize <= Max - Min));
  TScrollBarSlider(Slider).UpdateBar;
end;


{
  $Log$
  Revision 1.5  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
