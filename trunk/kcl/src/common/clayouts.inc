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

procedure TLayout.OnSizeChanged;
begin
  if CreationState >= wcsCreationInProgress then
    RecalcLayout;
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
  ASSERT(false, 'TFixedLayout.MoveWidget: Widget not found');
end;

procedure TFixedLayout.OnFinishCreation;
var
  i: Integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.FinishCreation;
    PutChild(item.Widget, item.Left, item.Top);
  end;
  inherited OnFinishCreation;
end;

procedure TFixedLayout.OnRecalcLayout;
begin
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

procedure TDockingLayout.OnRecalcLayout;
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
  clw := Width;
  clh := Height;

  if (clw = 0) or (clh = 0) then exit;

  // Process all attached widgets
  ClientWidget := nil;
  for i := 0 to FWidgets.Count - 1 do begin
    item := TDockingItem(FWidgets.Items[i]);
    case item.DockingMode of
      dmLeft: begin
	  item.Left := clx;
	  item.Top := cly;
	  WidgetW := item.Widget.Width;
	  WidgetH := clh;
	  item.Widget.CheckSize(WidgetW, WidgetH);
	  Inc(clx, WidgetW);
	  Dec(clw, WidgetW);
        end;
      dmTop: begin
	  item.Left := clx;
	  item.Top := cly;
	  WidgetW := clw;
	  WidgetH := item.Widget.Height;
	  item.Widget.CheckSize(WidgetW, WidgetH);
	  Inc(cly, WidgetH);
	  Dec(clh, WidgetH);
        end;
      dmRight: begin
	  item.Left := clx + clw - item.Widget.Width;
	  item.Top := cly;
	  WidgetW := item.Widget.Width;
	  WidgetH := clh;
	  item.Widget.CheckSize(WidgetW, WidgetH);
	  Dec(clw, WidgetW);
        end;
      dmBottom: begin
	  item.Left := clx;
	  item.Top := cly + clh - item.Widget.Height;
	  WidgetW := clw;
	  WidgetH := item.Widget.Height;
	  item.Widget.CheckSize(WidgetW, WidgetH);
	  Dec(clh, WidgetH);
        end;
      dmClient: begin
          ASSERT(not Assigned(ClientWidget),
	    'DockingLayout owns multiple widgets with "client" docking mode!');
	  ClientWidget := item.Widget;
        end;
      dmUndocked: begin end;    // Do nothing
      else ASSERT(false, 'Invalid docking mode');
    end;
    if item.DockingMode <> dmClient then begin
      if item.DockingMode <> dmUndocked then
        SetChildSize(item.Widget, WidgetW, WidgetH);
      MoveChild(item.Widget, item.Left, item.Top);
    end;
  end;
  if Assigned(ClientWidget) then begin
    ClientWidget.SetSize(clw, clh);
    MoveChild(ClientWidget, clx, cly);
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

procedure TGridLayout.OnRecalcLayout;
begin
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
    RecalcLayout;
  end;
end;

procedure TBoxLayout.OnRecalcLayout;
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

    MoveChild(item.Widget, x, y);

    if FOrientation = boxHorz then
      Inc(x, item.Widget.Width)
    else
      Inc(y, item.Widget.Height);
  end;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:21:29  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:09  sg
  Initial import

}
