{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Scrolling support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TScrollingSupport
// ===================================================================

constructor TScrollingSupport.Create(AParent: TWidget);
begin
  Parent := AParent;

  HorzScrollBar := TCustomScrollBar.Create(Parent);
  HorzScrollBar.Name := '#Scrolling_HorzBar';
  HorzScrollBar.HasBorder := False;
  HorzScrollBar.SetEmbeddedParent(Parent);

  VertScrollBar := TCustomScrollBar.Create(Parent);
  VertScrollBar.Name := '#Scrolling_VertBar';
  VertScrollBar.Orientation := Vertical;
  VertScrollBar.HasBorder := False;
  VertScrollBar.SetEmbeddedParent(Parent);
end;

destructor TScrollingSupport.Destroy;
begin
  inherited Destroy;
end;

function TScrollingSupport.ProcessEvent(Event: TEventObj): Boolean;
var
  Canvas: TGfxCanvas;
begin
  if Event.InheritsFrom(TResizeEventObj) then
  begin
    with Parent.Style.GetScrollBoxBorders do
    begin
      HorzScrollBar.SetBounds(Left, Parent.Height - HorzScrollBar.MinH - Bottom,
        Parent.Width - VertScrollBar.MinW - Left - Right, HorzScrollBar.MinH);
      VertScrollBar.SetBounds(Parent.Width - VertScrollBar.MinW - Right, Top,
        VertScrollBar.MinW, Parent.Height - HorzScrollBar.MinH - Top - Bottom);
      ClientRect.Left := Left;
      ClientRect.Top := Top;
      ClientRect.Right := Parent.Width - Right - VertScrollBar.Width;
      ClientRect.Bottom := Parent.Height - Bottom - HorzScrollBar.Height;
    end;
    Result := False;
  end else if Event.InheritsFrom(TPaintEventObj) then
  begin
    Canvas := TPaintEventObj(Event).Canvas;
    Parent.Style.DrawScrollBoxBorder(Canvas,
      Rect(0, 0, Parent.Width, Parent.Height));
    Parent.Style.DrawWindowBackground(Canvas, Rect(VertScrollBar.Left,
      HorzScrollBar.Top, VertScrollBar.Left + VertScrollBar.Width,
      HorzScrollBar.Top + HorzScrollBar.Height));
    Result := False;
  end else if Event.InheritsFrom(TMouseWheelEventObj) then
    Result := EvMouseWheel(TMouseWheelEventObj(Event))
  else
    Result := False;
end;

function TScrollingSupport.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(HorzScrollBar) or
    Event.SendToChild(VertScrollBar);
end;

procedure TScrollingSupport.EvRecalcLayout;
begin
  with Parent, Style.GetScrollBoxBorders do
  begin
    MinW := Left + Right + HorzScrollBar.MinW + VertScrollBar.MinW;
    DefW := Left + Right + HorzScrollBar.DefW + VertScrollBar.DefW;
    MinH := Top + Bottom + HorzScrollBar.MinH + VertScrollBar.MinH;
    DefH := Top + Bottom + HorzScrollBar.DefH + VertScrollBar.DefH;
    MaxW := InfiniteSize;
    MaxH := InfiniteSize;
  end;
end;

function TScrollingSupport.EvMouseWheel(Event: TMouseWheelEventObj): Boolean;
var
  mshift: TShiftState;
begin
  mshift := Event.Shift * [ssShift, ssAlt, ssCtrl, ssMeta,
    ssSuper, ssHyper, ssAltGr];
  if mshift = [] then
    VertScrollBar.Position := VertScrollBar.Position +
      Event.WheelDelta * VertScrollBar.SmallChange
  else if mshift = [ssShift] then
    HorzScrollBar.Position := HorzScrollBar.Position +
      Event.WheelDelta * VertScrollBar.SmallChange;
end;


// ===================================================================
//   TCustomScrollBox
// ===================================================================

constructor TCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsClickable, wsOpaque];
  ScrollingSupport := TScrollingSupport.Create(Self);
end;

destructor TCustomScrollBox.Destroy;
begin
  ScrollingSupport.Free;
  inherited Destroy;
end;


// Protected methods

procedure TCustomScrollBox.Paint(Canvas: TGfxCanvas);
begin
{
  Style.DrawWindowBackground(Canvas, Rect(HorzScrollBar.Left,
    VertScrollBar.Top, HorzScrollBar.Left + HorzScrollBar.Width,
    VertScrollBar.Top + VertScrollBar.Height));}
end;

function TCustomScrollBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.ProcessEvent(Event) or
    inherited ProcessEvent(Event);
end;

function TCustomScrollBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;


procedure TCustomScrollBox.EvRecalcLayout;
begin
  ScrollingSupport.EvRecalcLayout;
end;

{ !!!: Move to TScrollingSupport as soon as this is a real event
procedure TCustomScrollBox.EvKeyPressed(Key: Word; Shift: TShiftState);
var
  mshift: TShiftState;
begin
  mshift := Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  if mshift = [] then
    case Key of
      keyLeft:
        HorzScrollBar.ButtonUpClick(nil);
      keyRight:
        HorzScrollBar.ButtonDownClick(nil);
      keyUp:
        VertScrollBar.ButtonUpClick(nil);
      keyDown:
        VertScrollBar.ButtonDownClick(nil);
      keyPageUp:
        VertScrollBar.PageUp;
      keyPageDown:
        VertScrollBar.PageDown;
      keyHome:
        VertScrollBar.Position := 0;
      keyEnd:
        VertScrollBar.Position := VertScrollBar.Max - VertScrollBar.PageSize;
    end
  else if mshift = [ssShift] then
    case Key of
      keyPageUp:
        HorzScrollBar.PageUp;
      keyPageDown:
        HorzScrollBar.PageDown;
      keyHome:
        HorzScrollBar.Position := 0;
      keyEnd:
        HorzScrollBar.Position := HorzScrollBar.Max - HorzScrollBar.PageSize;
    end
  else
    inherited EvKeyPressed(Key, Shift);
end;}


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
