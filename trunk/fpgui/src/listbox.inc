{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Listbox widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TListBoxStrings
// ===================================================================

type

  TListBoxStrings = class(TStringList)
  protected
    ListBox: TCustomListBox;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AListBox: TCustomListBox);
    function Add(const s: String): Integer; override;
  end;


constructor TListBoxStrings.Create(AListBox: TCustomListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TListBoxStrings.Add(const s: String): Integer;
var
  ItemWidth: Integer;
begin
  Result := inherited Add(s);
  if ListBox.IsParented then
  begin
    ItemWidth := ListBox.FindForm.Wnd.Canvas.TextWidth(s) + 4;
    if ItemWidth > ListBox.FMaxItemWidth then
      ListBox.FMaxItemWidth := ItemWidth;
    ListBox.UpdateScrollBars;
  end;
end;

procedure TListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    ListBox.RecalcWidth;
end;


// ===================================================================
//   TCustomListBox
// ===================================================================

constructor TCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;

  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.HorzScrollBar.OnScroll :=
    @ScrollingSupport.DefHorzScrollHandler;
  ScrollingSupport.VertScrollBar.OnScroll :=
    @ScrollingSupport.DefVertScrollHandler;
  Items := TListBoxStrings.Create(Self);
  FItemIndex := -1;
  UpdateScrollBars;
end;

destructor TCustomListBox.Destroy;
begin
  Items.Free;
  ScrollingSupport.Free;
  inherited Destroy;
end;


// protected methods

procedure TCustomListBox.Paint(Canvas: TGfxCanvas);
var
  i, StartIndex, EndIndex: Integer;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
begin
  inherited Paint(Canvas);

  if not Canvas.IntersectClipRect(ScrollingSupport.ClientRect) then
    exit;

  Style.SetUIColor(Canvas, clWindow);
  Canvas.FillRect(ScrollingSupport.ClientRect);
  Style.SetUIColor(Canvas, clWindowText);

  with ScrollingSupport.VertScrollBar do
  begin
    StartIndex := Position div ItemHeight;
    EndIndex := (Position + PageSize) div ItemHeight;
  end;

  Canvas.AppendTranslation(
    ScrollingSupport.ClientRect.Left - ScrollingSupport.HorzScrollBar.Position,
    ScrollingSupport.ClientRect.Top - ScrollingSupport.VertScrollBar.Position);

  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= Items.Count then
    EndIndex := Items.Count - 1;

  for i := StartIndex to EndIndex do
  begin
    Canvas.SaveState;

    ItemRect.Left := ScrollingSupport.HorzScrollBar.Position;
    ItemRect.Top := i * ItemHeight;
    ItemRect.Right := ScrollingSupport.ClientRect.Right -
      ScrollingSupport.ClientRect.Left +
      ScrollingSupport.HorzScrollBar.Position;
    ItemRect.Bottom := (i + 1) * ItemHeight;

    Canvas.IntersectClipRect(ItemRect);

    ItemFlags := [];
    if (wsHasFocus in WidgetState) and ((i = ItemIndex) or
      ((ItemIndex = -1) and (i = 0))) then
      Include(ItemFlags, ifFocused);
    if i = ItemIndex then
      Include(ItemFlags, ifSelected);

    Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
    Canvas.TextOut(2, i * ItemHeight, Items[i]);
    Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);

    Canvas.RestoreState;
  end;
end;

function TCustomListBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TMousePressedEventObj) then
    Result := ScrollingSupport.ProcessEvent(Event) or
      EvMousePressed(TMousePressedEventObj(Event)) or
      inherited ProcessEvent(Event)
  else
  begin
    Result := ScrollingSupport.ProcessEvent(Event) or
      inherited ProcessEvent(Event);
    if Event.InheritsFrom(TUpdateEventObj) then
    begin
      IsParented := True;
      RecalcWidth;
    end;
    if Event.InheritsFrom(TResizeEventObj) then
    begin
      UpdateScrollBars;
      ScrollingSupport.HorzScrollBar.PageSize :=
	ScrollingSupport.ClientRect.Right -
	ScrollingSupport.ClientRect.Left;
      ScrollingSupport.VertScrollBar.PageSize :=
	ScrollingSupport.ClientRect.Bottom -
	ScrollingSupport.ClientRect.Top;
    end;
  end;
end;

function TCustomListBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TCustomListBox.EvRecalcLayout;
begin
  ScrollingSupport.EvRecalcLayout;
  ItemHeight := FindForm.Wnd.Canvas.FontCellHeight;
  ScrollingSupport.VertScrollBar.SmallChange := ItemHeight;
end;

procedure TCustomListBox.RecalcWidth;
var
  i, ItemWidth: Integer;
begin
  if not IsParented then
    exit;

  FMaxItemWidth := 0;
  for i := 0 to Items.Count - 1 do
  begin
    ItemWidth := FindForm.Wnd.Canvas.TextWidth(Items[i]) + 4;
    if ItemWidth > FMaxItemWidth then
      FMaxItemWidth := ItemWidth;
  end;
  UpdateScrollBars;
end;

procedure TCustomListBox.UpdateScrollBars;
var
  i: Integer;
begin
  ScrollingSupport.HorzScrollBar.Max := FMaxItemWidth;
  ScrollingSupport.VertScrollBar.Max := Items.Count * ItemHeight - 1;
end;

procedure TCustomListBox.RedrawItem(AIndex: Integer);
var
  ItemRect: TRect;
begin
  if AIndex < 0 then
    exit;
  ItemRect := ScrollingSupport.ClientRect;
  Inc(ItemRect.Top, AIndex * ItemHeight -
    ScrollingSupport.VertScrollBar.Position);
  if (ItemRect.Top > ScrollingSupport.ClientRect.Bottom) or
    (ItemRect.Top + ItemHeight <= ScrollingSupport.ClientRect.Top) then
    exit;
  ItemRect.Bottom := Min(ItemRect.Top + ItemHeight,
    ScrollingSupport.ClientRect.Bottom);
  Redraw(ItemRect);
end;


// private methods

function TCustomListBox.EvMousePressed(Event: TMousePressedEventObj): Boolean;
var
  Index: Integer;
begin
  if (Event.Button <> mbLeft) or
    (Event.MouseX < ScrollingSupport.ClientRect.Left) or
    (Event.MouseY < ScrollingSupport.ClientRect.Top) or
    (Event.MouseX >= ScrollingSupport.ClientRect.Right) or
    (Event.MouseY >= ScrollingSupport.ClientRect.Bottom) then
  begin
    Result := False;
    exit;
  end;

  Index := (Event.MouseY - ScrollingSupport.ClientRect.Top +
    ScrollingSupport.VertScrollBar.Position) div ItemHeight;
  if (Index >= 0) and (Index < Items.Count) and (Index <> ItemIndex) then
  begin
    RedrawItem(ItemIndex);
    FItemIndex := Index;
    RedrawItem(ItemIndex);
  end;
  Result := True;
end;


{
  $Log$
  Revision 1.4  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.3  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
