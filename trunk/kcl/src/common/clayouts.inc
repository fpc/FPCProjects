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
//   Common layout widgets implementation
// ===================================================================

resourcestring
  SLayoutWidgetNotFound = 'Layout child widget not found';



procedure AddToSizes(infos: PWidgetArrayInfoArray; count: Integer; TooMuch: Integer);
var
  i, add, FoundElements: Integer;
begin
  while TooMuch > 0 do begin
    add := TooMuch;
    FoundElements := 0;
    for i := 0 to count - 1 do begin
      if not infos^[i].MaxFlag then continue;
      Inc(FoundElements);
      if infos^[i].def + add > infos^[i].max then
        add := infos^[i].max - infos^[i].def;
    end;
    if FoundElements > 0 then begin
      add := add div FoundElements;
      if add <= 0 then add := 1;
    end else
      break;

    for i := 0 to count - 1 do begin
      if not infos^[i].MaxFlag then continue;
      Inc(infos^[i].def, add);
      Dec(TooMuch, add);
      if TooMuch = 0 then exit;
      if infos^[i].def = infos^[i].max then
        infos^[i].MaxFlag := False;
    end;
  end;
end;

procedure SubFromSizes(infos: PWidgetArrayInfoArray; count: Integer; TooMuch: Integer);
var
  i, sub, FoundElements: Integer;
begin
  while TooMuch > 0 do begin
    sub := TooMuch;
    FoundElements := 0;
    for i := 0 to count - 1 do begin
      if not infos^[i].MinFlag then continue;
      Inc(FoundElements);
      if infos^[i].def - sub < infos^[i].min then
        sub := infos^[i].def - infos^[i].min;
    end;
    if FoundElements > 0 then begin
      sub := sub div FoundElements;
      if sub <= 0 then sub := 1;
    end else
      break;

    for i := 0 to count - 1 do begin
      if not infos^[i].MinFlag then continue;
      Dec(infos^[i].def, sub);
      Dec(TooMuch, sub);
      if TooMuch = 0 then exit;
      if infos^[i].def = infos^[i].min then
        infos^[i].MinFlag := False;
    end;
  end;
end;

procedure CorrectSizes(infos: PWidgetArrayInfoArray; count: Integer; SizeDiff: Integer);
var
  TooMuch: Integer;
begin
  TooMuch := SizeDiff;
  if TooMuch > 0 then
    AddToSizes(infos, count, TooMuch)
  else if TooMuch < 0 then
    SubFromSizes(infos, count, -TooMuch);
end;


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


procedure TFixedLayout.AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
var
  item: TFixedItem;
begin
  AWidget.FParent := Self;
  item := TFixedItem(FWidgets.Add);
  item.Left := ALeft;
  item.Top := ATop;
  item.Widget := AWidget;
  if FCreationState >= wcsCreationInProgress then
    AddFixedChild(AWidget);
end;


procedure TFixedLayout.MoveWidget(AWidget: TWidget; ALeft, ATop: Integer);
var
  i: integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    if item.Widget = AWidget then begin
      item.Left := ALeft;
      item.Top := ATop;
      AWidget.SetBounds(ALeft, ATop, AWidget.Width, AWidget.Height);
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutWidgetNotFound);
end;


procedure TFixedLayout.OnFinishCreation;
var
  i: Integer;
  item: TFixedItem;
begin
  inherited OnFinishCreation;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.HandleNeeded;
    AddFixedChild(item.Widget);
    item.Widget.SetBounds(item.Left, item.Top, item.Widget.DefW, item.Widget.DefH);
  end;
end;


procedure TFixedLayout.DoRecalcLayout;
var
  i: Integer;
  item: TFixedItem;
begin
  MinW := 1;
  MinH := 1;
  if FWidgets.Count = 0 then begin
    DefW := 50;
    DefH := 50;
  end else
    for i := 0 to FWidgets.Count - 1 do begin
      item := TFixedItem(FWidgets.Items[i]);
      DefW := Max(DefW, item.Left + item.Widget.DefW);
      DefH := Max(DefH, item.Top + item.Widget.DefH);
    end;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
end;


procedure TFixedLayout.LayoutChildren;
var
  i: Integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.SetBounds(item.Left, item.Top, item.Widget.DefW, item.Widget.DefH);
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
    item.Widget.HandleNeeded;
    AddFixedChild(item.Widget);
  end;
end;


procedure TDockingLayout.DoRecalcLayout;
var
  i: Integer;
  item: TDockingItem;
  w, cw: TWidget;
begin
  if FWidgets.Count = 0 then begin
    MinW := 0;
    MinH := 0;
    DefW := 200;
    DefH := 200;
    MaxW := InfiniteSize;
    MaxH := InfiniteSize;
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
    MinW := 0;
    MinH := 0;
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


procedure TDockingLayout.LayoutChildren;
var
  clx, cly, clw, clh: Integer;   // Client rectangle
  ClientWidget: TWidget;
  i, WidgetW, WidgetH: Integer;
  item: TDockingItem;
begin
  clx := 0;
  cly := 0;
  clw := FWidth;
  clh := FHeight;
  // WriteLn('=> DockingLayout.LayoutChildren ', FWidth, ' x ', FHeight);
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
      dmClient:
	ClientWidget := item.Widget;
    end;
    if item.DockingMode <> dmClient then
      item.Widget.SetBounds(item.Left, item.Top, WidgetW, WidgetH);
  end;
  if Assigned(ClientWidget) then
    ClientWidget.SetBounds(clx, cly, clw, clh);
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
    AddFixedChild(AWidget);
    RecalcLayout;
  end;
end;


procedure TDockingLayout.AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
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
    AddFixedChild(AWidget);
    RecalcLayout;
  end;
end;


// -------------------------------------------------------------------
//   TCustomGridLayout
// -------------------------------------------------------------------

procedure TCustomGridLayout.SetColCount(AColCount: Integer);
begin
  if AColCount <> FColCount then begin
    FColCount := AColCount;
    RecalcLayout;
  end;
end;


procedure TCustomGridLayout.SetRowCount(ARowCount: Integer);
begin
  if ARowCount <> FRowCount then begin
    FRowCount := ARowCount;
    RecalcLayout;
  end;
end;


procedure TCustomGridLayout.SetColSpacing(AColSpacing: Integer);
begin
  if AColSpacing <> FColSpacing then begin
    FColSpacing := AColSpacing;
    RecalcLayout;
  end;
end;


procedure TCustomGridLayout.SetRowSpacing(ARowSpacing: Integer);
begin
  if ARowSpacing <> FRowSpacing then begin
    FRowSpacing := ARowSpacing;
    RecalcLayout;
  end;
end;


procedure TCustomGridLayout.InitSizeInfos(var ColInfos, RowInfos: PWidgetArrayInfoArray);
var
  i: Integer;
  item: TGridItem;
begin
  GetMem(ColInfos, FColCount * SizeOf(TWidgetArrayInfo));
  GetMem(RowInfos, FRowCount * SizeOf(TWidgetArrayInfo));

  for i := 0 to FColCount - 1 do begin
    ColInfos^[i].min := 0;
    ColInfos^[i].def := 0;
    ColInfos^[i].max := InfiniteSize;
    ColInfos^[i].MinFlag := True;
    ColInfos^[i].MaxFlag := True;
  end;

  for i := 0 to FRowCount - 1 do begin
    RowInfos^[i].min := 0;
    RowInfos^[i].def := 0;
    RowInfos^[i].max := InfiniteSize;
    RowInfos^[i].MinFlag := True;
    RowInfos^[i].MaxFlag := True;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    ColInfos^[item.x].min := Max(ColInfos^[item.x].min, item.Widget.MinW);
    ColInfos^[item.x].def := Max(ColInfos^[item.x].def, item.Widget.DefW);
    ColInfos^[item.x].max := Min(ColInfos^[item.x].max, item.Widget.MaxW);
    RowInfos^[item.y].min := Max(RowInfos^[item.y].min, item.Widget.MinH);
    RowInfos^[item.y].def := Max(RowInfos^[item.y].def, item.Widget.DefH);
    RowInfos^[item.y].max := Min(RowInfos^[item.y].max, item.Widget.MaxH);
  end;

  for i := 0 to FColCount - 1 do begin
    if ColInfos^[i].def = 0 then ColInfos^[i].def := 20;
    if ColInfos^[i].min >= ColInfos^[i].def then
      ColInfos^[i].MinFlag := False;
    if ColInfos^[i].max <= ColInfos^[i].def then
      ColInfos^[i].MaxFlag := False;
  end;

  for i := 0 to FRowCount - 1 do begin
    if RowInfos^[i].def = 0 then RowInfos^[i].def := 20;
    if RowInfos^[i].min >= RowInfos^[i].def then
      RowInfos^[i].MinFlag := False;
    if RowInfos^[i].max <= RowInfos^[i].def then
      RowInfos^[i].MaxFlag := False;
  end;
end;


procedure TCustomGridLayout.OnFinishCreation;
var
  i: Integer;
begin
  for i := 0 to FWidgets.Count - 1 do
    TGridItem(FWidgets.Items[i]).Widget.HandleNeeded;

  inherited OnFinishCreation;

  for i := 0 to FWidgets.Count - 1 do
    AddFixedChild(TGridItem(FWidgets.Items[i]).Widget);
end;


procedure TCustomGridLayout.DoRecalcLayout;
var
  ColInfos, RowInfos: PWidgetArrayInfoArray;
  i: Integer;
begin
  MinW := (FColCount - 1) * FColSpacing;
  MinH := (FRowCount - 1) * FRowSpacing;
  DefW := (FColCount - 1) * FColSpacing;
  DefH := (FRowCount - 1) * FRowSpacing;
  MaxW := (FColCount - 1) * FColSpacing;
  MaxH := (FRowCount - 1) * FRowSpacing;

  InitSizeInfos(ColInfos, RowInfos);

  for i := 0 to FColCount - 1 do begin
    Inc(MinW, ColInfos^[i].min);
    Inc(DefW, ColInfos^[i].def);
    MaxW := Min(InfiniteSize, MaxW + ColInfos^[i].max);
  end;

  for i := 0 to FRowCount - 1 do begin
    Inc(MinH, RowInfos^[i].min);
    Inc(DefH, RowInfos^[i].def);
    MaxH := Min(InfiniteSize, MaxH + RowInfos^[i].max);
  end;

  FreeMem(RowInfos);
  FreeMem(ColInfos);
end;


procedure TCustomGridLayout.LayoutChildren;
var
  ColInfos, RowInfos: PWidgetArrayInfoArray;
  i, j, x, y, w, h, def: Integer;
  item: TGridItem;
begin
  InitSizeInfos(ColInfos, RowInfos);

  CorrectSizes(ColInfos, FColCount, FWidth - DefW);
  CorrectSizes(RowInfos, FRowCount, FHeight - DefH);

  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    x := 0; for j := 0 to item.x - 1 do Inc(x, ColInfos^[j].def);
    y := 0; for j := 0 to item.y - 1 do Inc(y, RowInfos^[j].def);
    w := 0; for j := 0 to item.Width - 1 do Inc(w, ColInfos^[item.x + j].def);
    h := 0; for j := 0 to item.Height - 1 do Inc(h, RowInfos^[item.y + j].def);
    Inc(w, (item.Width - 1) * FColSpacing);
    Inc(h, (item.Height - 1) * FRowSpacing);
    item.Widget.SetBounds(x + item.x * FColSpacing, y + item.y * FRowSpacing, w, h);
  end;

  FreeMem(ColInfos);
  FreeMem(RowInfos);
end;


constructor TCustomGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TGridItem);
  FColCount := 2;
  FRowCount := 2;
  FColSpacing := 4;
  FRowSpacing := 4;
end;


procedure TCustomGridLayout.AddWidget(AWidget: TWidget; x, y, w, h: Integer);
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


procedure TCustomGridLayout.MoveWidget(AWidget: TWidget; x, y, w, h: Integer);
var
  i: integer;
  item: TGridItem;
begin
  for i := 0 to FWidgets.Count - 1 do begin
    item := TGridItem(FWidgets.Items[i]);
    if item.Widget = AWidget then begin
      item.x := x;
      item.y := y;
      item.Width := w;
      item.Height := h;
      RecalcLayout;
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutWidgetNotFound);
end;


// -------------------------------------------------------------------
//   TCustomBoxLayout
// -------------------------------------------------------------------

procedure TCustomBoxLayout.SetOrientation(AOrientation: TBoxOrientation);
begin
  if AOrientation <> FOrientation then begin
    FOrientation := AOrientation;
    if HandleAllocated then
      RecalcLayout;
  end;
end;


constructor TCustomBoxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TLayoutItem);
  FHorzAlign := horzFill;
  FVertAlign := vertFill;
  FSpacing := 4;
end;


procedure TCustomBoxLayout.OnFinishCreation;
var
  i: Integer;
  item: TLayoutItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    item.Widget.HandleNeeded;
    AddFixedChild(item.Widget);
  end;
end;


procedure TCustomBoxLayout.AddWidget(AWidget: TWidget);
var
  item: TLayoutItem;
begin
  AWidget.FParent := Self;
  item := TLayoutItem(FWidgets.Add);
  item.Widget := AWidget;
  if CreationState = wcsCreationDone then begin
    AddFixedChild(AWidget);
    RecalcLayout;
  end;
end;


procedure TCustomBoxLayout.DoRecalcLayout;
var
  i: Integer;
  item: TLayoutItem;
begin
  i := (FWidgets.Count - 1) * FSpacing;
  if FOrientation = boxHorz then begin
    MinW := i;
    MinH := 0;
    DefW := 0;
    DefH := MinH;
    MaxW := DefW;
    MaxH := InfiniteSize;
  end else begin
    MinW := 0;
    MinH := i;
    DefW := 0;
    DefH := i;
    MaxW := InfiniteSize;
    MaxH := DefH;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    if FOrientation = boxHorz then begin
      Inc(MinW, item.Widget.MinW);
      Inc(DefW, item.Widget.DefW);
      MaxW := Min(InfiniteSize, MaxW + item.Widget.MaxW);
      MaxH := Min(MaxH, item.Widget.MaxH);
      if MinH < item.Widget.MinH then
        MinH := item.Widget.MinH;
      if DefH < item.Widget.DefH then
        DefH := item.Widget.DefH;
      if MaxH > item.Widget.MaxH then
        MaxH := item.Widget.MaxH;
    end else begin
      Inc(MinH, item.Widget.MinH);
      Inc(DefH, item.Widget.DefH);
      MaxW := Min(MaxW, item.Widget.MaxW);
      MaxH := Min(InfiniteSize, MaxH + item.Widget.MaxH);
      if MinW < item.Widget.MinW then
        MinW := item.Widget.MinW;
      if DefW < item.Widget.DefW then
        DefW := item.Widget.DefW;
      if MaxW > item.Widget.MaxW then
        MaxW := item.Widget.MaxW;
    end;
  end;

  Inc(MinW, 2 * FBorderSpacing);
  Inc(MinH, 2 * FBorderSpacing);
  Inc(DefW, 2 * FBorderSpacing);
  Inc(DefH, 2 * FBorderSpacing);
  MaxW := Min(InfiniteSize, MaxW + FBorderSpacing);
  MaxH := Min(InfiniteSize, MaxH + FBorderSpacing);
end;


procedure TCustomBoxLayout.LayoutChildren;
var
  sizes: PWidgetArrayInfoArray;
  i, x, y, xpos, ypos, w, h, TooMuch, sum: Integer;
  item: TLayoutItem;
begin
  GetMem(sizes, FWidgets.Count * SizeOf(TWidgetArrayInfo));

  for i := 0 to FWidgets.Count - 1 do begin
    sizes^[i].min := 0;
    sizes^[i].def := 0;
    sizes^[i].max := InfiniteSize;
    sizes^[i].MinFlag := True;
    sizes^[i].MaxFlag := True;
  end;

  if FOrientation = boxHorz then
    for i := 0 to FWidgets.Count - 1 do begin
      item := TLayoutItem(FWidgets.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Widget.MinW);
      sizes^[i].def := Max(sizes^[i].def, item.Widget.DefW);
      sizes^[i].max := Min(sizes^[i].max, item.Widget.MaxW);
    end
  else
    for i := 0 to FWidgets.Count - 1 do begin
      item := TLayoutItem(FWidgets.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Widget.MinH);
      sizes^[i].def := Max(sizes^[i].def, item.Widget.DefH);
      sizes^[i].max := Min(sizes^[i].max, item.Widget.MaxH);
    end;

  for i := 0 to FWidgets.Count - 1 do begin
    if sizes^[i].def = 0 then sizes^[i].def := 20;
    if sizes^[i].min >= sizes^[i].def then
      sizes^[i].MinFlag := False;
    if sizes^[i].max <= sizes^[i].def then
      sizes^[i].MaxFlag := False;
  end;

  if FOrientation = boxHorz then begin
    if FHorzAlign = horzFill then
      CorrectSizes(sizes, FWidgets.Count, FWidth - DefW)
  end else
    if FVertAlign = vertFill then
      CorrectSizes(sizes, FWidgets.Count, FHeight - DefH);

  sum := (FWidgets.Count - 1) * FSpacing;
  for i := 0 to FWidgets.Count - 1 do
    Inc(sum, sizes^[i].def);

  if FOrientation = boxHorz then
    case FHorzAlign of
      horzCenter: x := (FWidth - sum) div 2;
      horzRight:  x := FWidth - FBorderSpacing - sum;
      else	  x := FBorderSpacing;
    end
  else
    case FVertAlign of
      vertCenter: y := (FHeight - sum) div 2;
      vertBottom: y := FHeight - FBorderSpacing - sum;
      else        y := FBorderSpacing;
    end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TLayoutItem(FWidgets.Items[i]);
    if FOrientation = boxHorz then begin
      xpos := x;
      w := sizes^[i].def;
      h := Min(FHeight, item.Widget.DefH);
      case FVertAlign of
        vertCenter: ypos := (FHeight - h) div 2;
	vertBottom: ypos := FHeight - FBorderSpacing - h;
	else        ypos := FBorderSpacing;
      end;
      Inc(x, sizes^[i].def + FSpacing);
      if FVertAlign = vertFill then
        h := Min(FHeight, item.Widget.MaxH);
    end else begin
      ypos := y;
      w := Min(FWidth, item.Widget.DefW);
      h := sizes^[i].def;
      case FHorzAlign of
        horzCenter: xpos := (FWidth - w) div 2;
	horzRight : xpos := FWidth - FBorderSpacing - w;
	else        xpos := FBorderSpacing;
      end;
      Inc(y, sizes^[i].def + FSpacing);
      if FHorzAlign = horzFill then
        w := Min(FWidth, item.Widget.MaxW);
    end;
    item.Widget.SetBounds(xpos, ypos, w, h);
  end;

  FreeMem(sizes);
end;


{
  $Log$
  Revision 1.9  2000/03/14 11:48:47  sg
  * More improvements and fixes

  Revision 1.8  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.7  2000/02/22 14:36:25  sg
  * Huge improvements for TGridLayout and TBoxLayout; resizing now works in
    most cases. TODO: Make TGridLayout working correctly for children which
    span several cells.

  Revision 1.6  2000/02/18 22:17:53  sg
  * TBoxLayout now relayouts itself if its orientation has been changed

  Revision 1.5  2000/02/18 18:20:01  sg
  * TFixedLayout works again
  * Fixed layouting of TBoxLayout and TGridLayout

  Revision 1.4  2000/02/17 22:20:35  sg
  * Adapted TGridLayout and TBoxLayout to new layouting algorithm
}
