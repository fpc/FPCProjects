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
//   TLayout
// -------------------------------------------------------------------

function TLayout.GetWidgetCount: Integer;
begin
  Result := FWidgets.Count;
end;

function TLayout.GetWidgets(AIndex: Integer): TWidget;
begin
  Result := TLayoutItem(FWidgets.Items[AIndex]).Widget;
end;

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLayout.Destroy;
begin
  FWidgets.Free;
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TFixedLayout
// -------------------------------------------------------------------

constructor TFixedLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFixedItem);
end;

procedure TFixedLayout.AddWidget(AWidget: TWidget; ALeft, ATop: LongInt);
var
  item: TFixedItem;
begin
  AWidget.FParent := Self;
  item := TFixedItem(FWidgets.Add);
  item.Left := ALeft;
  item.Top := ATop;
  item.Widget := AWidget;
  if FCreationState >= wcsCreationInProgress then
    PutChild(AWidget, ALeft, ATop);
end;

procedure TFixedLayout.MoveWidget(AWidget: TWidget; ALeft, ATop: LongInt);
var
  i: integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    if item.Widget = AWidget then begin
      item.Left := ALeft;
      item.Top := ATop;
      if FCreationState >= wcsCreationInProgress then
        MoveChild(AWidget, ALeft, ATop);
      exit;
    end;
  end;
  ASSERT(False, 'TFixedLayout.MoveWidget: Widget not found');
end;

procedure TFixedLayout.OnFinishCreation;
var
  i: Integer;
  item: TFixedItem;
begin
  inherited OnFinishCreation;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, item.Left, item.Top);
  end;
end;


// -------------------------------------------------------------------
//   TDockingLayout
// -------------------------------------------------------------------

constructor TDockingLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TDockingItem);
end;

procedure TDockingLayout.OnFinishCreation;
var
  i: Integer;
  item: TDockingItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TDockingItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, 0, 0);
  end;
end;

procedure TDockingLayout.DoRecalcLayout;
var
  i: Integer;
  item: TDockingItem;
  w, cw: TWidget;
begin
  if FWidgets.Count = 0 then begin
    DefW := 200;
    DefH := 200;
    exit;
  end;

  // Find the client widget (widget with DockingMode "dmClient")
  cw := nil;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TDockingItem(FWidgets.Items[i]);
    if item.DockingMode = dmClient then begin
      cw := item.Widget;
      break;
    end;
  end;

  if Assigned(cw) then begin
    MinW := cw.MinW;
    MinH := cw.MinH;
    MaxW := cw.MaxW;
    MaxH := cw.MaxH;
    DefW := cw.DefW;
    DefH := cw.DefH;
  end else begin
    DefW := 200;
    DefH := 200;
    MaxW := InfiniteSize;
    MaxH := InfiniteSize;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TDockingItem(FWidgets.Items[i]);
    w := item.Widget;
    case item.DockingMode of
      dmTop, dmBottom: begin
          if MinW < w.MinW then MinW := w.MinW;
	  Inc(MinH, w.MinH);
	  if MaxW < w.MaxW then MaxW := w.MaxW;
	  if MaxH < InfiniteSize then
	    Inc(MaxH, w.MaxH);
	  if DefW < w.DefW then DefW := w.DefW;
	  Inc(DefH, w.DefH);
        end;
      dmLeft, dmRight: begin
	  Inc(MinW, w.MinW);
          if MinH < w.MinH then MinH := w.MinH;
	  if MaxW < InfiniteSize then
	    Inc(MaxW, w.MaxW);
	  if MaxH < w.MaxH then MaxH := w.MaxH;
	  Inc(DefW, w.DefW);
	  if DefH < w.DefH then DefH := w.DefH;
        end;
    end;
  end;
end;

procedure TDockingLayout.ApplySize;
  function Max(i1, i2: LongInt): LongInt;
  begin
    if i1 >= i2 then Result := i1
    else Result := i2;
  end;
var
  clx, cly, clw, clh: LongInt;   // Client rectangle
  ClientWidget: TWidget;
  i, WidgetW, WidgetH: LongInt;
  item: TDockingItem;
begin
  inherited ApplySize;

  clx := 0;
  cly := 0;
  clw := FWidth;
  clh := FHeight;
  //WriteLn('=> DockingLayout.ApplySize ', FWidth, ' x ', FHeight);
  if (clw = 0) or (clh = 0) then exit;

  // Process all attached widgets
  ClientWidget := nil;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TDockingItem(FWidgets.Items[i]);
    case item.DockingMode of
      dmLeft: begin
	  WidgetW := item.Widget.DefW;
	  WidgetH := clh;
	  item.Left := clx;
	  item.Top := cly;
	  Inc(clx, WidgetW);
	  Dec(clw, WidgetW);
        end;
      dmTop: begin
	  WidgetW := clw;
	  WidgetH := item.Widget.DefH;
	  item.Left := clx;
	  item.Top := cly;
	  Inc(cly, WidgetH);
	  Dec(clh, WidgetH);
        end;
      dmRight: begin
	  WidgetW := item.Widget.DefW;
	  WidgetH := clh;
	  item.Left := clx + clw - WidgetW;
          item.Top := cly;
	  Dec(clw, WidgetW);
        end;
      dmBottom: begin
	  WidgetH := item.Widget.DefH;
	  WidgetW := clw;
	  item.Left := clx;
	  item.Top := cly + clh - WidgetH;
	  Dec(clh, WidgetH);
        end;
      dmClient: begin
          ASSERT(not Assigned(ClientWidget),
	    'DockingLayout owns multiple widgets with docking mode dmClient!');
	  ClientWidget := item.Widget;
        end;
    end;
    if item.DockingMode <> dmClient then begin
      if item.DockingMode <> dmUndocked then
        item.Widget.SetSize(WidgetW, WidgetH);
      MoveChild(item.Widget, item.Left, item.Top);
    end;
  end;
  if Assigned(ClientWidget) then begin
    MoveChild(ClientWidget, clx, cly);
    ClientWidget.SetSize(clw, clh);
  end;
end;

procedure TDockingLayout.AddWidget(AWidget: TWidget; ADockingMode: TDockingMode);
var
  item: TDockingItem;
begin
  AWidget.FParent := Self;
  item := TDockingItem(FWidgets.Add);
  item.Widget := AWidget;
  item.DockingMode := ADockingMode;
  if CreationState = wcsCreationDone then begin
    PutChild(AWidget, 0, 0);
    RecalcLayout;
  end;
end;

procedure TDockingLayout.AddWidget(AWidget: TWidget; ALeft, ATop: LongInt);
var
  item: TDockingItem;
begin
  AWidget.FParent := Self;
  item := TDockingItem(FWidgets.Add);
  item.Widget := AWidget;
  item.DockingMode := dmUndocked;
  item.Left := ALeft;
  item.Top := ATop;
  if CreationState = wcsCreationDone then begin
    PutChild(AWidget, ALeft, ATop);
    RecalcLayout;
  end;
end;


// -------------------------------------------------------------------
//   TGridLayout
// -------------------------------------------------------------------

constructor TGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TGridItem);
  FRows := 2;
  FColumns := 2;
end;

procedure TGridLayout.FSetSameSizeCells(ASameSize: Boolean);
begin
  FSameSizeCells := ASameSize;
end;

procedure TGridLayout.OnFinishCreation;
var
  i: Integer;
  item: TGridItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, 0, 0);
  end;
end;

procedure TGridLayout.DoRecalcLayout;
type
  TIntegerArray = array[0..9999] of Integer;
  PIntegerArray = ^TIntegerArray;
var
  ColWidths, RowHeights: PIntegerArray;
  i, def: Integer;
  item: TGridItem;
begin
  GetMem(ColWidths, FColumns * SizeOf(Integer));
  GetMem(RowHeights, FRows * SizeOf(Integer));
  for i := 0 to FColumns - 1 do ColWidths^[i] := 0;
  for i := 0 to FRows - 1 do RowHeights^[i] := 0;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    ColWidths^[item.x] := Max(ColWidths^[item.x], item.Widget.DefW);
    RowHeights^[item.y] := Max(RowHeights^[item.y], item.Widget.DefH);
  end;

  for i := 0 to FColumns - 1 do
    if ColWidths^[i] > 0 then
      Inc(DefW, ColWidths^[i])
    else
      Inc(DefW, 20);
  for i := 0 to FRows - 1 do
    if RowHeights^[i] > 0 then
      Inc(DefH, RowHeights^[i])
    else
      Inc(DefH, 20);
  FreeMem(RowHeights);
  FreeMem(ColWidths);

  Inc(DefW, (FColumns - 1) * FHorzSpacing);
  Inc(DefH, (FRows - 1) * FVertSpacing);

  MinW := DefW;
  MinH := DefH;
end;

procedure TGridLayout.ApplySize;
type
  TIntegerArray = array[0..9999] of Integer;
  PIntegerArray = ^TIntegerArray;
var
  ColWidths, RowHeights: PIntegerArray;
  i, j, x, y, w, h, def: Integer;
  item: TGridItem;
begin
  inherited ApplySize;

  GetMem(ColWidths, FColumns * SizeOf(Integer));
  GetMem(RowHeights, FRows * SizeOf(Integer));
  for i := 0 to FColumns - 1 do ColWidths^[i] := 0;
  for i := 0 to FRows - 1 do RowHeights^[i] := 0;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    ColWidths^[item.x] := Max(ColWidths^[item.x], item.Widget.DefW);
    RowHeights^[item.y] := Max(RowHeights^[item.y], item.Widget.DefH);
  end;

  for i := 0 to FColumns - 1 do
    if ColWidths^[i] = 0 then ColWidths^[i] := 20;
  for i := 0 to FRows - 1 do
    if RowHeights^[i] = 0 then RowHeights^[i] := 20;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    x := 0; for j := 0 to item.x - 1 do Inc(x, ColWidths^[j]);
    y := 0; for j := 0 to item.y - 1 do Inc(y, RowHeights^[j]);
    MoveChild(item.Widget, x + item.x * FHorzSpacing, y + item.y * FVertSpacing);
    w := 0; for j := 0 to item.Width - 1 do Inc(w, ColWidths^[item.x + j]);
    h := 0; for j := 0 to item.Height - 1 do Inc(h, RowHeights^[item.y + j]);
    Inc(w, (item.Width - 1) * FHorzSpacing);
    Inc(h, (item.Height - 1) * FVertSpacing);
    item.Widget.SetSize(w, h);
  end;

  FreeMem(RowHeights);
  FreeMem(ColWidths);
end;

procedure TGridLayout.AddWidget(AWidget: TWidget; x, y, w, h: LongInt);
var
  item: TGridItem;
begin
  AWidget.FParent := Self;
  item := TGridItem(FWidgets.Add);
  item.Widget := AWidget;
  item.x := x;
  item.y := y;
  item.Width := w;
  item.Height := h;
end;


// -------------------------------------------------------------------
//   TBoxLayout
// -------------------------------------------------------------------

constructor TBoxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TLayoutItem);
  FHorzAlign := horzFixed;
  FVertAlign := vertFixed;
end;

procedure TBoxLayout.OnFinishCreation;
var
  i: Integer;
  item: TLayoutItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, 0, 0);
  end;
end;

procedure TBoxLayout.AddWidget(AWidget: TWidget);
var
  item: TLayoutItem;
begin
  AWidget.FParent := Self;
  item := TLayoutItem(FWidgets.Add);
  item.Widget := AWidget;
  if CreationState = wcsCreationDone then begin
    PutChild(AWidget, 0, 0);
    RecalcLayout;
  end;
end;

procedure TBoxLayout.DoRecalcLayout;
var
  i: Integer;
  item: TLayoutItem;
begin
  i := (FWidgets.Count - 1) * FSpacing;
  if FOrientation = boxHorz then begin
    MinW := i;
    DefW := i;
    MaxH := InfiniteSize
  end else begin
    MinH := i;
    DefH := i;
    MaxW := InfiniteSize;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    if FOrientation = boxHorz then begin
      Inc(MinW, item.Widget.MinW);
      Inc(DefW, item.Widget.DefW);
      MaxW := Min(InfiniteSize, MaxW + item.Widget.MaxW);
      if MinH < item.Widget.MinH then
        MinH := item.Widget.MinH;
      if DefH < item.Widget.DefH then
        DefH := item.Widget.DefH;
      if MaxH > item.Widget.MaxH then
        MaxH := item.Widget.MaxH;
    end else begin
      Inc(MinH, item.Widget.MinH);
      Inc(DefH, item.Widget.DefH);
      MaxH := Min(InfiniteSize, MaxH + item.Widget.MaxH);
      if MinW < item.Widget.MinW then
        MinW := item.Widget.MinW;
      if DefW < item.Widget.DefW then
        DefW := item.Widget.DefW;
      if MaxW > item.Widget.MaxW then
        MaxW := item.Widget.MaxW;
    end;
  end;

  if FHorzAlign = horzFixed then
    MaxW := DefW;
  if FVertAlign = vertFixed then
    MaxH := DefH;
end;

procedure TBoxLayout.ApplySize;
{FWidth and FHeight are the real size of this layout object. But as all
 "ApplySize" calls had a preceding "DoRecalcLayout", we can use the
 calculated values of DefW/DefH, MinW/MinH and MaxW/MaxH here!}
var
  i, x, y, w, h: Integer;
  item: TLayoutItem;
begin
  inherited ApplySize;
  case FHorzAlign of
    horzCenter: x := (FWidth - DefW) div 2;
    horzRight:  x := FWidth - DefW;
    else	x := 0;
  end;
  case FVertAlign of
    vertCenter: y := (FHeight - DefH) div 2;
    vertBottom: y := FHeight - DefH;
    else        y := 0;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    MoveChild(item.Widget, x, y);
    w := item.Widget.DefW;
    h := item.Widget.DefH;
    if FOrientation = boxHorz then begin
      Inc(x, item.Widget.DefW + FSpacing);
      if FVertAlign = vertFill then
        h := Min(FHeight, item.Widget.MaxH);
    end else begin
      Inc(y, item.Widget.DefH + FSpacing);
      if FHorzAlign = horzFill then
        w := Min(FWidth, item.Widget.MaxW);
    end;
    item.Widget.SetSize(w, h);
  end;
end;


{
  $Log$
  Revision 1.4  2000/02/17 22:20:35  sg
  * Adapted TGridLayout and TBoxLayout to new layouting algorithm

  Revision 1.3  2000/02/10 18:28:53  sg
  * Implemented new layouting engine (at this time, for TDockingLayout only)

  Revision 1.2  2000/01/24 00:21:29  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:09  sg
  Initial import

}
