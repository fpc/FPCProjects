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
//   Common layout controls implementation
// ===================================================================

resourcestring
  SLayoutControlNotFound = 'Layout child control not found';



procedure AddToSizes(infos: PControlArrayInfoArray; count: Integer; TooMuch: Integer);
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

procedure SubFromSizes(infos: PControlArrayInfoArray; count: Integer; TooMuch: Integer);
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

procedure CorrectSizes(infos: PControlArrayInfoArray; count: Integer; SizeDiff: Integer);
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

function TLayout.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;


function TLayout.GetControls(AIndex: Integer): TControl;
begin
  Result := TLayoutItem(FControls.Items[AIndex]).Control;
end;


constructor TLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


destructor TLayout.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TFixedLayout
// -------------------------------------------------------------------

constructor TFixedLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TCollection.Create(TFixedItem);
end;


procedure TFixedLayout.AddControl(AControl: TControl; ALeft, ATop: Integer);
var
  item: TFixedItem;
begin
  AControl.FParent := Self;
  item := TFixedItem(FControls.Add);
  item.Left := ALeft;
  item.Top := ATop;
  item.Control := AControl;
  if FCreationState >= wcsCreationInProgress then
    AddFixedChild(AControl);
end;


procedure TFixedLayout.MoveControl(AControl: TControl; ALeft, ATop: Integer);
var
  i: integer;
  item: TFixedItem;
begin
  for i := 0 to FControls.Count - 1 do begin
    item := TFixedItem(FControls.Items[i]);
    if item.Control = AControl then begin
      item.Left := ALeft;
      item.Top := ATop;
      AControl.SetBounds(ALeft, ATop, AControl.Width, AControl.Height);
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutControlNotFound);
end;


procedure TFixedLayout.OnFinishCreation;
var
  i: Integer;
  item: TFixedItem;
begin
  inherited OnFinishCreation;

  for i := 0 to FControls.Count - 1 do begin
    item := TFixedItem(FControls.Items[i]);
    if item.Control.InheritsFrom(TWinControl) then
      TWinControl(item.Control).HandleNeeded;
    AddFixedChild(item.Control);
    item.Control.SetBounds(item.Left, item.Top, item.Control.DefW, item.Control.DefH);
  end;
end;


procedure TFixedLayout.DoRecalcLayout;
var
  i: Integer;
  item: TFixedItem;
begin
  MinW := 1;
  MinH := 1;
  if FControls.Count = 0 then begin
    DefW := 50;
    DefH := 50;
  end else
    for i := 0 to FControls.Count - 1 do begin
      item := TFixedItem(FControls.Items[i]);
      DefW := Max(DefW, item.Left + item.Control.DefW);
      DefH := Max(DefH, item.Top + item.Control.DefH);
    end;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
end;


procedure TFixedLayout.LayoutChildren;
var
  i: Integer;
  item: TFixedItem;
begin
  for i := 0 to FControls.Count - 1 do begin
    item := TFixedItem(FControls.Items[i]);
    item.Control.SetBounds(item.Left, item.Top, item.Control.DefW, item.Control.DefH);
  end;
end;


// -------------------------------------------------------------------
//   TDockingLayout
// -------------------------------------------------------------------

constructor TDockingLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TCollection.Create(TDockingItem);
end;

procedure TDockingLayout.OnFinishCreation;
var
  i: Integer;
  item: TDockingItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FControls.Count - 1 do
  begin
    item := TDockingItem(FControls.Items[i]);
    if item.Control.InheritsFrom(TWinControl) then
      TWinControl(item.Control).HandleNeeded;
    AddFixedChild(item.Control);
  end;
end;

procedure TDockingLayout.DoRecalcLayout;
var
  i: Integer;
  item: TDockingItem;
  w, cw: TControl;
begin
  if FControls.Count = 0 then begin
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
  for i := 0 to FControls.Count - 1 do begin
    item := TDockingItem(FControls.Items[i]);
    if item.DockingMode = dmClient then begin
      cw := item.Control;
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

  for i := 0 to FControls.Count - 1 do begin
    item := TDockingItem(FControls.Items[i]);
    w := item.Control;
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
  ClientControl: TControl;
  i, ControlW, ControlH: Integer;
  item: TDockingItem;
begin
  clx := 0;
  cly := 0;
  clw := FWidth;
  clh := FHeight;
  // WriteLn('=> DockingLayout.LayoutChildren ', FWidth, ' x ', FHeight);
  if (clw = 0) or (clh = 0) then exit;

  // Process all attached widgets
  ClientControl := nil;
  for i := 0 to FControls.Count - 1 do begin
    item := TDockingItem(FControls.Items[i]);
    case item.DockingMode of
      dmLeft: begin
	  ControlW := item.Control.DefW;
	  ControlH := clh;
	  item.Left := clx;
	  item.Top := cly;
	  Inc(clx, ControlW);
	  Dec(clw, ControlW);
        end;
      dmTop: begin
	  ControlW := clw;
	  ControlH := item.Control.DefH;
	  item.Left := clx;
	  item.Top := cly;
	  Inc(cly, ControlH);
	  Dec(clh, ControlH);
        end;
      dmRight: begin
	  ControlW := item.Control.DefW;
	  ControlH := clh;
	  item.Left := clx + clw - ControlW;
          item.Top := cly;
	  Dec(clw, ControlW);
        end;
      dmBottom: begin
	  ControlH := item.Control.DefH;
	  ControlW := clw;
	  item.Left := clx;
	  item.Top := cly + clh - ControlH;
	  Dec(clh, ControlH);
        end;
      dmClient:
	ClientControl := item.Control;
    end;
    if item.DockingMode <> dmClient then
      item.Control.SetBounds(item.Left, item.Top, ControlW, ControlH);
  end;
  if Assigned(ClientControl) then
    ClientControl.SetBounds(clx, cly, clw, clh);
end;


procedure TDockingLayout.AddControl(AControl: TControl; ADockingMode: TDockingMode);
var
  item: TDockingItem;
begin
  AControl.FParent := Self;
  item := TDockingItem(FControls.Add);
  item.Control := AControl;
  item.DockingMode := ADockingMode;
  if CreationState = wcsCreationDone then begin
    AddFixedChild(AControl);
    RecalcLayout;
  end;
end;


procedure TDockingLayout.AddControl(AControl: TControl; ALeft, ATop: Integer);
var
  item: TDockingItem;
begin
  AControl.FParent := Self;
  item := TDockingItem(FControls.Add);
  item.Control := AControl;
  item.DockingMode := dmUndocked;
  item.Left := ALeft;
  item.Top := ATop;
  if CreationState = wcsCreationDone then begin
    AddFixedChild(AControl);
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


procedure TCustomGridLayout.InitSizeInfos(var ColInfos, RowInfos: PControlArrayInfoArray);
var
  i: Integer;
  item: TGridItem;
begin
  GetMem(ColInfos, FColCount * SizeOf(TControlArrayInfo));
  GetMem(RowInfos, FRowCount * SizeOf(TControlArrayInfo));

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

  for i := 0 to FControls.Count - 1 do begin
    item := TGridItem(FControls.Items[i]);
    ColInfos^[item.x].min := Max(ColInfos^[item.x].min, item.Control.MinW);
    ColInfos^[item.x].def := Max(ColInfos^[item.x].def, item.Control.DefW);
    ColInfos^[item.x].max := Min(ColInfos^[item.x].max, item.Control.MaxW);
    RowInfos^[item.y].min := Max(RowInfos^[item.y].min, item.Control.MinH);
    RowInfos^[item.y].def := Max(RowInfos^[item.y].def, item.Control.DefH);
    RowInfos^[item.y].max := Min(RowInfos^[item.y].max, item.Control.MaxH);
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
  for i := 0 to FControls.Count - 1 do
    if TGridItem(FControls.Items[i]).Control.InheritsFrom(TWinControl) then
      TWinControl(TGridItem(FControls.Items[i]).Control).HandleNeeded;

  inherited OnFinishCreation;

  for i := 0 to FControls.Count - 1 do
    AddFixedChild(TGridItem(FControls.Items[i]).Control);
end;


procedure TCustomGridLayout.DoRecalcLayout;
var
  ColInfos, RowInfos: PControlArrayInfoArray;
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
  ColInfos, RowInfos: PControlArrayInfoArray;
  i, j, x, y, w, h, def: Integer;
  item: TGridItem;
begin
  InitSizeInfos(ColInfos, RowInfos);

  CorrectSizes(ColInfos, FColCount, FWidth - DefW);
  CorrectSizes(RowInfos, FRowCount, FHeight - DefH);

  for i := 0 to FControls.Count - 1 do begin
    item := TGridItem(FControls.Items[i]);
    x := 0; for j := 0 to item.x - 1 do Inc(x, ColInfos^[j].def);
    y := 0; for j := 0 to item.y - 1 do Inc(y, RowInfos^[j].def);
    w := 0; for j := 0 to item.Width - 1 do Inc(w, ColInfos^[item.x + j].def);
    h := 0; for j := 0 to item.Height - 1 do Inc(h, RowInfos^[item.y + j].def);
    Inc(w, (item.Width - 1) * FColSpacing);
    Inc(h, (item.Height - 1) * FRowSpacing);
    item.Control.SetBounds(x + item.x * FColSpacing, y + item.y * FRowSpacing, w, h);
  end;

  FreeMem(ColInfos);
  FreeMem(RowInfos);
end;


constructor TCustomGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TCollection.Create(TGridItem);
  FColCount := 2;
  FRowCount := 2;
  FColSpacing := 4;
  FRowSpacing := 4;
end;


procedure TCustomGridLayout.AddControl(AControl: TControl; x, y, w, h: Integer);
var
  item: TGridItem;
begin
  AControl.FParent := Self;
  item := TGridItem(FControls.Add);
  item.Control := AControl;
  item.x := x;
  item.y := y;
  item.Width := w;
  item.Height := h;
end;


procedure TCustomGridLayout.MoveControl(AControl: TControl; x, y, w, h: Integer);
var
  i: integer;
  item: TGridItem;
begin
  for i := 0 to FControls.Count - 1 do begin
    item := TGridItem(FControls.Items[i]);
    if item.Control = AControl then begin
      item.x := x;
      item.y := y;
      item.Width := w;
      item.Height := h;
      RecalcLayout;
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutControlNotFound);
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
  FControls := TCollection.Create(TLayoutItem);
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
  for i := 0 to FControls.Count - 1 do begin
    item := TLayoutItem(FControls.Items[i]);
    if item.Control.InheritsFrom(TWinControl) then
      TWinControl(item.Control).HandleNeeded;
    AddFixedChild(item.Control);
  end;
end;


procedure TCustomBoxLayout.AddControl(AControl: TControl);
var
  item: TLayoutItem;
begin
  AControl.FParent := Self;
  item := TLayoutItem(FControls.Add);
  item.Control := AControl;
  if CreationState = wcsCreationDone then begin
    AddFixedChild(AControl);
    RecalcLayout;
  end;
end;


procedure TCustomBoxLayout.DoRecalcLayout;
var
  i: Integer;
  item: TLayoutItem;
begin
  i := (FControls.Count - 1) * FSpacing;
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

  for i := 0 to FControls.Count - 1 do begin
    item := TLayoutItem(FControls.Items[i]);
    if FOrientation = boxHorz then begin
      Inc(MinW, item.Control.MinW);
      Inc(DefW, item.Control.DefW);
      MaxW := Min(InfiniteSize, MaxW + item.Control.MaxW);
      MaxH := Min(MaxH, item.Control.MaxH);
      if MinH < item.Control.MinH then
        MinH := item.Control.MinH;
      if DefH < item.Control.DefH then
        DefH := item.Control.DefH;
      if MaxH > item.Control.MaxH then
        MaxH := item.Control.MaxH;
    end else begin
      Inc(MinH, item.Control.MinH);
      Inc(DefH, item.Control.DefH);
      MaxW := Min(MaxW, item.Control.MaxW);
      MaxH := Min(InfiniteSize, MaxH + item.Control.MaxH);
      if MinW < item.Control.MinW then
        MinW := item.Control.MinW;
      if DefW < item.Control.DefW then
        DefW := item.Control.DefW;
      if MaxW > item.Control.MaxW then
        MaxW := item.Control.MaxW;
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
  sizes: PControlArrayInfoArray;
  i, x, y, xpos, ypos, w, h, TooMuch, sum: Integer;
  item: TLayoutItem;
begin
  GetMem(sizes, FControls.Count * SizeOf(TControlArrayInfo));

  for i := 0 to FControls.Count - 1 do begin
    sizes^[i].min := 0;
    sizes^[i].def := 0;
    sizes^[i].max := InfiniteSize;
    sizes^[i].MinFlag := True;
    sizes^[i].MaxFlag := True;
  end;

  if FOrientation = boxHorz then
    for i := 0 to FControls.Count - 1 do begin
      item := TLayoutItem(FControls.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Control.MinW);
      sizes^[i].def := Max(sizes^[i].def, item.Control.DefW);
      sizes^[i].max := Min(sizes^[i].max, item.Control.MaxW);
    end
  else
    for i := 0 to FControls.Count - 1 do begin
      item := TLayoutItem(FControls.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Control.MinH);
      sizes^[i].def := Max(sizes^[i].def, item.Control.DefH);
      sizes^[i].max := Min(sizes^[i].max, item.Control.MaxH);
    end;

  for i := 0 to FControls.Count - 1 do begin
    if sizes^[i].def = 0 then sizes^[i].def := 20;
    if sizes^[i].min >= sizes^[i].def then
      sizes^[i].MinFlag := False;
    if sizes^[i].max <= sizes^[i].def then
      sizes^[i].MaxFlag := False;
  end;

  if FOrientation = boxHorz then begin
    if FHorzAlign = horzFill then
      CorrectSizes(sizes, FControls.Count, FWidth - DefW)
  end else
    if FVertAlign = vertFill then
      CorrectSizes(sizes, FControls.Count, FHeight - DefH);

  sum := (FControls.Count - 1) * FSpacing;
  for i := 0 to FControls.Count - 1 do
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

  for i := 0 to FControls.Count - 1 do begin
    item := TLayoutItem(FControls.Items[i]);
    if FOrientation = boxHorz then begin
      xpos := x;
      w := sizes^[i].def;
      h := Min(FHeight, item.Control.DefH);
      case FVertAlign of
        vertCenter: ypos := (FHeight - h) div 2;
	vertBottom: ypos := FHeight - FBorderSpacing - h;
	else        ypos := FBorderSpacing;
      end;
      Inc(x, sizes^[i].def + FSpacing);
      if FVertAlign = vertFill then
        h := Min(FHeight, item.Control.MaxH);
    end else begin
      ypos := y;
      w := Min(FWidth, item.Control.DefW);
      h := sizes^[i].def;
      case FHorzAlign of
        horzCenter: xpos := (FWidth - w) div 2;
	horzRight : xpos := FWidth - FBorderSpacing - w;
	else        xpos := FBorderSpacing;
      end;
      Inc(y, sizes^[i].def + FSpacing);
      if FHorzAlign = horzFill then
        w := Min(FWidth, item.Control.MaxW);
    end;
    item.Control.SetBounds(xpos, ypos, w, h);
  end;

  FreeMem(sizes);
end;


{
  $Log$
  Revision 1.10  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

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
