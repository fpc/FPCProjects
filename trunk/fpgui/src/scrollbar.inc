{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
    procedure EvRecalcLayout; override;
  public
    Direction: TArrowDirection;
  end;

procedure TScrollBarButton.Paint(Canvas: TGfxCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawScrollBarButton(Canvas, Rect(0, 0, Width, Height), Direction,
    (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;

procedure TScrollBarButton.EvRecalcLayout;
begin
  ASSERT(Owner is TCustomScrollBar);
  with Style.GetScrollBarBtnSize(TCustomScrollBar(Owner).Orientation) do
  begin
    MinW := cx;
    MinH := cy;
  end;
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
    ButtonMovePos: Integer;
    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    procedure EvRecalcLayout; override;
    function CalcPosition: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateBar;
  end;


constructor TScrollBarSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  ButtonMovePos := -1;
end;

procedure TScrollBarSlider.UpdateBar;
var
  Size: Integer;
begin
  ASSERT(Owner is TCustomScrollBar);

  if WidgetState * [wsGetsVisible, wsIsVisible] = [] then
    exit;

  with TCustomScrollBar(Owner) do
  begin
    if (Min = Max) or (Max - Min = PageSize - 1) then
      ButtonPos := 0
    else
    begin
      if Orientation = Horizontal then
        Size := Self.Width
      else
        Size := Self.Height;
      if PageSize = 0 then
        ButtonPos := (Position - Min) *
	  (Size - ButtonSize) div (Max - Min)
      else
        ButtonPos := (Position - Min) *
	  (Size - ButtonSize) div (Max - Min - PageSize + 1);
    end;
    ButtonSize := GetButtonSize;
  end;
  Redraw;
end;

procedure TScrollBarSlider.Paint(Canvas: TGfxCanvas);
var
  Size: Integer;
  r: TRect;
  StartPos, EndPos: PInteger;
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
      Style.SetUIColor(Canvas, cl3DShadow)
    else
      Style.SetUIColor(Canvas, clScrollBar);
    StartPos^ := 0;
    EndPos^ := ButtonPos;
    Canvas.FillRect(r);
  end;
  if ButtonPos + ButtonSize < Size then
  begin
    if NextAreaPressed then
      Style.SetUIColor(Canvas, cl3DShadow)
    else
      Style.SetUIColor(Canvas, clScrollBar);
    StartPos^ := ButtonPos + ButtonSize;
    EndPos^ := Size;
    Canvas.FillRect(r);
  end;

  StartPos^ := ButtonPos;
  EndPos^ := ButtonPos + ButtonSize;
  Style.DrawButtonFace(Canvas, r, []);
end;

function TScrollBarSlider.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := False;
  if Event.InheritsFrom(TVisibilityChangeEventObj) then
  begin
// !!!: Why don't we get an wsIsVisible update?!?
    if wsGetsVisible in WidgetState then
      UpdateBar;
  end else if Event is TMousePressedEventObj then
    Result := EvMousePressed(TMousePressedEventObj(Event))
  else if Event is TMouseReleasedEventObj then
    Result := EvMouseReleased(TMouseReleasedEventObj(Event))
  else if Event is TMouseMoveEventObj then
    Result := EvMouseMove(TMouseMoveEventObj(Event));

  if not Result then
    Result := inherited ProcessEvent(Event);

  if Event is TResizeEventObj then
    UpdateBar;
end;

function TScrollBarSlider.EvMousePressed(Event: TMousePressedEventObj): Boolean;
var
  Pos: Integer;
begin
  inherited ProcessEvent(Event);	// For mouse grabbing support

  if TCustomScrollBar(Owner).Orientation = Horizontal then
    Pos := Event.MouseX
  else
    Pos := Event.MouseY;

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
    ButtonMovePos := Pos;
  end;
  Result := True;
end;

function TScrollBarSlider.EvMouseReleased(
  Event: TMouseReleasedEventObj): Boolean;
begin
  inherited ProcessEvent(Event);	// For mouse grabbing support

  if IsDraggingButton then
  begin
    IsDraggingButton := False;
    TCustomScrollBar(Owner).Position := CalcPosition;
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
    if TCustomScrollBar(Owner).Orientation = Horizontal then
    begin
      Pos := Event.MouseX;
      Size := Width;
    end else
    begin
      Pos := Event.MouseY;
      Size := Height;
    end;

    ButtonPos := ClipMinMax(ButtonPos + Pos - ButtonMovePos,
      0, Size - ButtonSize - 1);
    ButtonMovePos := Pos;
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

procedure TScrollBarSlider.EvRecalcLayout;
begin
  inherited EvRecalcLayout;
  if TCustomScrollBar(Owner).Orientation = Horizontal then
    DefW := Style.GetScrollBarBtnSize(Horizontal).cy * 5
  else
    DefH := Style.GetScrollBarBtnSize(Vertical).cx * 5;
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

    if Result > Max - PageSize then
      if PageSize = 0 then
	Result := Max
      else
	Result := Max - PageSize + 1;
    if Result < Min then
      Result := Min;
  end;
end;


// -------------------------------------------------------------------
//   TCustomScrollBar
// -------------------------------------------------------------------

constructor TCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Include(WidgetStyle, wsOpaque);
  HasBorder := True;

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

function TCustomScrollBar.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event is TResizeEventObj then
  begin
    Resized;
    Result := False;
  end else
    Result := inherited ProcessEvent(Event);
end;

function TCustomScrollBar.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(Slider) or Event.SendToChild(ButtonUp) or
    Event.SendToChild(ButtonDown);
end;

procedure TCustomScrollBar.Paint(Canvas: TGfxCanvas);
begin
  if HasBorder then
    Style.DrawScrollBarBorder(Canvas, Rect(0, 0, Width, Height));
end;

procedure TCustomScrollBar.Resized;
var
  r: TRect;
begin
  if HasBorder then
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
      ButtonUp.SetBounds(Left, Top, ButtonUp.DefW, Bottom - Top);
      ButtonDown.SetBounds(Right - ButtonDown.DefW, Top,
        ButtonDown.DefW, Bottom - Top);
      Slider.SetBounds(Left + ButtonUp.DefW, Top,
        Right - Left - ButtonUp.DefW - ButtonDown.DefW, Bottom - Top);
    end else
    begin
      ButtonUp.SetBounds(Left, Top, Right - Left, ButtonDown.DefH);
      ButtonDown.SetBounds(Left, Bottom - ButtonDown.DefH,
        Right - Left, ButtonDown.DefH);
      Slider.SetBounds(Left, Top + ButtonUp.DefH,
        Right - Left, Bottom - Top - ButtonUp.DefH - ButtonDown.DefH);
    end;
end;

procedure TCustomScrollBar.EvRecalcLayout;
begin
  if Orientation = Horizontal then
  begin
    MinW := ButtonUp.DefW + Slider.MinW + ButtonDown.DefW;
    MinH := ButtonUp.DefH;
    DefW := ButtonUp.DefW + Slider.DefW + ButtonDown.DefW;
    DefH := ButtonUp.DefH;
    MaxW := InfiniteSize;
    MaxH := ButtonUp.DefH;
  end else
  begin
    MinW := ButtonUp.DefW;
    MinH := ButtonUp.DefH + Slider.MinH + ButtonDown.DefH;
    DefW := ButtonUp.DefW;
    DefH := ButtonUp.DefH + Slider.DefH + ButtonDown.DefH;
    MaxW := ButtonUp.DefW;
    MaxH := InfiniteSize;
  end;

  if HasBorder then
    with Style.GetScrollBarBorders(Orientation) do
    begin
      Inc(MinW, Left + Right);
      Inc(MinH, Top + Bottom);
      DefW := fpGUI.Min(DefW + Left + Right, InfiniteSize);
      DefH := fpGUI.Min(DefH + Top + Bottom, InfiniteSize);
      MaxW := fpGUI.Min(MaxW + Left + Right, InfiniteSize);
      MaxH := fpGUI.Min(MaxH + Top + Bottom, InfiniteSize);
    end;
end;

procedure TCustomScrollBar.ButtonUpClick(Sender: TObject);
begin
  LineUp;
end;

procedure TCustomScrollBar.ButtonDownClick(Sender: TObject);
begin
  LineDown;
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

procedure TCustomScrollBar.SetPageSize(APageSize: Integer);
begin
  if APageSize <> PageSize then
  begin
    FPageSize := APageSize;
    Position := Position;	// Do range clipping
    TScrollBarSlider(Slider).UpdateBar;
  end;
end;

procedure TCustomScrollBar.SetPosition(APosition: Integer);
begin
  if (APosition <> Position) and Assigned(OnScroll) then
    OnScroll(Self, APosition);

  if APosition > Max - PageSize then
    if PageSize = 0 then
      APosition := Max
    else
      APosition := Max - PageSize + 1;

  if APosition < Min then
    APosition := Min;

  if APosition <> Position then
  begin
    FPosition := APosition;
    TScrollBarSlider(Slider).UpdateBar;
    if Assigned(OnChange) then
      OnChange(Self);
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
