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
end;

procedure TGridLayout.FSetSameSizeCells(ASameSize: Boolean);
begin
  FSameSizeCells := ASameSize;
end;

procedure TGridLayout.OnFinishCreation;
begin
  inherited OnFinishCreation;
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
  {if FCreationState >= wcsCreationInProgress then
    gtk_table_attach_defaults(PGtkTable(Handle.Layout), AWidget.Handle.Widget,
      x, x + w, y, y + h);}
end;


// -------------------------------------------------------------------
//   TBoxLayout
// -------------------------------------------------------------------

constructor TBoxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TBoxItem);
end;

procedure TBoxLayout.OnFinishCreation;
var
  i: Integer;
  item: TBoxItem;
begin
  inherited OnFinishCreation;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TBoxItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, 0, 0);
  end;
//  OnRecalcLayout;
end;

procedure TBoxLayout.AddWidget(AWidget: TWidget);
var
  item: TBoxItem;
begin
  AWidget.FParent := Self;
  item := TBoxItem(FWidgets.Add);
  item.Widget := AWidget;
  if CreationState = wcsCreationDone then begin
    PutChild(AWidget, 0, 0);
//    RecalcLayout;
  end;
end;

{procedure TBoxLayout.OnRecalcLayout;
var
  i: Integer;
  size, x, y: LongInt;
  item: TBoxItem;
begin
  size := 0;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TBoxItem(FWidgets.Items[i]);
    if FOrientation = boxHorz then
      Inc(size, item.Widget.Width)
    else
      Inc(size, item.Widget.Height);
  end;

{###  if FOrientation = boxHorz then begin
    if FMinWidth < size then begin
WriteLn('Setze MinWidth auf ', size);
      SetFixedSize(size, FHeight);
{      FMinWidth := size;
      FMinHeight := FHeight;
      OnMinMaxSizeChanged;}
      exit;
    end
  end else
    if FMinHeight < size then begin
      SetMinHeight(size);
      exit;
    end;}

  x := 0;
  y := 0;

  if FOrientation = boxHorz then begin
    x := FWidth;
    case FHorzAlign of
      horzLeft: x := 0;
      horzCenter: x := (x - size) div 2;
      horzRight: x := x - size;
    end;
  end else begin
    y := FHeight;
    case FVertAlign of
      vertTop: y := 0;
      vertCenter: y := (y - size) div 2;
      vertBottom: y := y - size;
    end;
  end;

  for i := 0 to FWidgets.Count - 1 do begin
    item := TBoxItem(FWidgets.Items[i]);

    if FOrientation = boxHorz then
      case FVertAlign of
        vertTop: y := 0;
        vertCenter: y := (FHeight - item.Widget.Height) div 2;
        vertBottom: y := FHeight - item.Widget.Height;
      end
    else
      case FHorzAlign of
        horzLeft: x := 0;
        horzCenter: x := (FWidth - item.Widget.Width) div 2;
        horzRight: x := FWidth - item.Widget.Width;
      end;
WriteLn('Moving ', item.Widget.ClassName, ' ', item.Widget.Name, ' to ', x, ' / ', y);
    MoveChild(item.Widget, x, y);

    if FOrientation = boxHorz then
      Inc(x, item.Widget.Width)
    else
      Inc(y, item.Widget.Height);
  end;
end;}


{
  $Log$
  Revision 1.3  2000/02/10 18:28:53  sg
  * Implemented new layouting engine (at this time, for TDockingLayout only)

  Revision 1.2  2000/01/24 00:21:29  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:09  sg
  Initial import

}
