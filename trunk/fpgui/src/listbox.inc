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
  public
    constructor Create(AListBox: TCustomListBox);
    function Add(const S: string): Integer; override;
  end;


constructor TListBoxStrings.Create(AListBox: TCustomListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TListBoxStrings.Add(const S: string): Integer;
begin
  Result := inherited Add(s);
  ListBox.UpdateScrollBars;
end;


// ===================================================================
//   TCustomListBox
// ===================================================================

constructor TCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.VertScrollBar.PageSize := 1;
  ScrollingSupport.VertScrollBar.OnScroll := @VertScrollBarScroll;
  Items := TListBoxStrings.Create(Self);
  UpdateScrollBars;
end;

destructor TCustomListBox.Destroy;
begin
  Items.Free;
  ScrollingSupport.Free;
  inherited Destroy;
end;

procedure TCustomListBox.Paint(Canvas: TGfxCanvas);
var
  i, StartIndex, EndIndex: Integer;
begin
  inherited Paint(Canvas);
  Style.SetUIColor(Canvas, clWindow);
  Canvas.FillRect(ScrollingSupport.ClientRect);
  Style.SetUIColor(Canvas, clWindowText);
  Canvas.IntersectClipRect(ScrollingSupport.ClientRect);

  with ScrollingSupport.VertScrollBar do
  begin
    StartIndex := Position div ItemHeight;
    EndIndex := (Position + PageSize) div ItemHeight;
    Canvas.AppendTranslation(ScrollingSupport.ClientRect.Left,
      ScrollingSupport.ClientRect.Top - Position);
  end;

  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= Items.Count then
    EndIndex := Items.Count - 1;

  for i := StartIndex to EndIndex do
  begin
    Canvas.SaveState;
    Canvas.IntersectClipRect(Rect(0, i * ItemHeight,
      ScrollingSupport.ClientRect.Right, (i + 1) * ItemHeight));
    Canvas.TextOut(0, i * ItemHeight, Items[i]);
    Canvas.RestoreState;
  end;
end;

function TCustomListBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.ProcessEvent(Event) or
    inherited ProcessEvent(Event);
  if Event.InheritsFrom(TResizeEventObj) then
  begin
    UpdateScrollBars;
    ScrollingSupport.VertScrollBar.PageSize :=
      ScrollingSupport.ClientRect.Bottom -
      ScrollingSupport.ClientRect.Top;
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

procedure TCustomListBox.UpdateScrollBars;
begin
  ScrollingSupport.VertScrollBar.Max := Items.Count * ItemHeight;
end;

procedure TCustomListBox.VertScrollBarScroll(Sender: TObject;
  var APosition: Integer);
begin
  Redraw;
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
