{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// ===================================================================
//   TWidget Implementation
// ===================================================================

// -------------------------------------------------------------------
//   GTK signal handlers
// -------------------------------------------------------------------

function TWidget_FocusInEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus;
  Widget: TWidget): Integer; cdecl;
begin
  if Assigned(Widget.OnFocusIn) then
    Widget.OnFocusIn(Widget);
  Result := 1;
end;

function TWidget_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus;
  Widget: TWidget): Integer; cdecl;
begin
  if Assigned(Widget.OnFocusOut) then
    Widget.OnFocusOut(Widget);
  Result := 1;
end;

procedure TWidget_SizeAllocate(widget: PGtkWidget; alloc: PGtkAllocation;
  AWidget: TWidget); cdecl;
begin
//  WriteLn('Size allocated for ', AWidget.ClassName, ': ', alloc^.Width, 'x', alloc^.Height);
  if (alloc^.Width <> AWidget.FWidth) or
    (alloc^.Height <> AWidget.FHeight) then begin
    AWidget.FWidth := alloc^.Width;
    AWidget.FHeight := alloc^.Height;
    AWidget.OnSizeChanged;
  end;
end;


// -------------------------------------------------------------------
//   TWidget property handling
// -------------------------------------------------------------------

procedure TWidget.SetVisible(AVisible: Boolean);
begin
  if AVisible = FVisible then exit;
  FVisible := AVisible;
  if FCreationState >= wcsCreationInProgress then
    if AVisible then
      gtk_widget_show(Handle.Widget)
    else
      gtk_widget_hide(Handle.Widget);
end;

procedure TWidget.SetText(const AText: String);
begin
  FText := AText;
end;

procedure TWidget.SetHint(const AHint: String);
begin
  FHint := AHint;
end;


// -------------------------------------------------------------------
//   TWidget protected methods
// -------------------------------------------------------------------

procedure TWidget.FinishCreation;
begin
  if FCreationState = wcsCreationDone then exit;

  if Assigned(FParent) and FParent.InheritsFrom(TWidget) and
    (TWidget(FParent).FCreationState < wcsCreationInProgress) then exit;

  FCreationState := wcsCreationInProgress;

  OnFinishCreation;
  if not (sizeAuto in FSizeFlags) then
    OnMinMaxSizeChanged;

  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TWidget.OnFinishCreation;
var
  SizeReq: TGtkRequisition;
begin
  ASSERT(Assigned(Handle.Widget));

  if sizeAuto in FSizeFlags then begin
    // Get default size of widget from GTK
    gtk_widget_size_request(Handle.Widget, @SizeReq);
    FWidth := SizeReq.Width;
    FHeight := SizeReq.Height;
  end;

  if sizeFixed in FSizeFlags then
    gtk_widget_set_usize(Handle.Widget, FWidth, FHeight);

  gtk_signal_connect_after(PGtkObject(Handle.Widget), 'size-allocate',
    GTK_SIGNAL_FUNC(@TWidget_SizeAllocate), Self);

  OnSetSize;

  if FShowHint then begin
    // WriteLn(IntToHex(LongInt(Self), 8), ' Setting Tooltip for ', ClassName, ': ', FHint);
    gtk_tooltips_set_tip(gToolTips, Handle.Widget, PChar(FHint), PChar(FHint));
  end;

  if FVisible then
    gtk_widget_show(Handle.Widget);

  FCreationState := wcsCreationDone;

  // GTK-connect all signals
  gtk_signal_connect(PGtkObject(Handle.Widget), 'focus-in-event',
    GTK_SIGNAL_FUNC(@TWidget_FocusInEvent), self);
  gtk_signal_connect(PGtkObject(Handle.Widget), 'focus-out-event',
    GTK_SIGNAL_FUNC(@TWidget_FocusOutEvent), self);
end;

procedure TWidget.OnRecalcLayout; begin end;

procedure TWidget.OnSetSize;
begin
  ASSERT(Assigned(Handle.Widget));

  PGtkObject(Handle.Widget)^.flags := PGtkObject(Handle.Widget)^.flags and not
    Ord(GTK_VISIBLE);

  Handle.Widget^.allocation.Width := FWidth;
  Handle.Widget^.allocation.Height := FHeight;
  gtk_widget_set_usize(Handle.Widget, FWidth, FHeight);

  PGtkObject(Handle.Widget)^.flags := PGtkObject(Handle.Widget)^.flags or
    Ord(GTK_VISIBLE);
end;

procedure TWidget.OnSizeChanged;
begin
{  if (FWidth < FMinWidth) or (FHeight < FMinHeight) or (FWidth > FMaxWidth) or
    (FHeight > FMaxHeight) then begin
    if FWidth < FMinWidth then FWidth := FMinWidth;
    if FWidth > FMaxWidth then FWidth := FMaxWidth;
    if FHeight < FMinHeight then FHeight := FMinHeight;
    if FHeight > FMaxHeight then FHeight := FMaxHeight;
    SetSize(FWidth, FHeight);
  end else begin
}
  ParentRecalcLayout;

  OnMinMaxSizeChanged;    // GTK specific Quick Hack (tm)...
end;

procedure TWidget.OnMinMaxSizeChanged;
var
  geom: TGdkGeometry;
begin
  if Assigned(Handle.Widget) and Assigned(Handle.Widget^.window) then begin
    geom.min_width := FMinWidth;
    geom.min_height := FMinHeight;
    geom.max_width := FMaxWidth;
    geom.max_height := FMaxHeight;
    gdk_window_set_geometry_hints(Handle.Widget^.window, @geom,
      GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE);
  end;
end;


// -------------------------------------------------------------------
//   TWidget public methods
// -------------------------------------------------------------------

constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := True;
  FSizeFlags := [sizeAuto];
  FMinWidth := 0;
  FMinHeight := 0;
  FMaxWidth := InfiniteSize;
  FMaxHeight := InfiniteSize;
end;

destructor TWidget.Destroy;
begin
  if Assigned(Handle.Widget) then
    gtk_widget_destroy(Handle.Widget);
end;

procedure TWidget.SetFocus;
begin
  if Assigned(Handle.Widget) then begin
    gtk_window_set_focus(
      PGtkWindow(gtk_widget_get_toplevel(Handle.Widget)), Handle.Widget);
  end;
end;

function TWidget.HasFocus: Boolean;
begin
  if Assigned(Handle.Widget) then
    Result := GTK_WIDGET_HAS_FOCUS(Handle.Widget)
  else
    Result := False;
end;


procedure TWidget.SetSize(AWidth, AHeight: LongInt);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then exit;
  FWidth := AWidth;
  FHeight := AHeight;
  if CreationState >= wcsCreationInProgress then
    OnSetSize;
end;

procedure TWidget.SetDefaultSize(AWidth, AHeight: LongInt);
begin
  FSizeFlags := FSizeFlags - [sizeAuto];
  SetSize(AWidth, AHeight);
end;

procedure TWidget.SetFixedSize(AWidth, AHeight: LongInt);
begin
  FSizeFlags := [sizeFixed];
  FMinWidth := AWidth;
  FMinHeight := AHeight;
  FMaxWidth := AWidth;
  FMaxHeight := AHeight;
  SetSize(AWidth, AHeight);
end;

procedure TWidget.Redraw;
begin
  if Assigned(Handle.Widget) then
    gtk_widget_queue_draw(Handle.Widget);
end;

procedure TWidget.Redraw(const Rect: TRect);
var
  r: TGdkRectangle;
begin
  if Assigned(Handle.Widget) then begin
    r.x := Rect.Left;
    r.y := Rect.Top;
    r.Width := Rect.Right - Rect.Left + 1;
    r.Height := Rect.Bottom - Rect.Top + 1;
    gtk_widget_draw(Handle.Widget, @r);
  end;
end;

procedure TWidget.Redraw(ax, ay, aw, ah: Integer);
var
  r: TGdkRectangle;
begin
  if Assigned(Handle.Widget) then begin
    r.x := ax;
    r.y := ay;
    r.Width := aw;
    r.Height := ah;
    gtk_widget_draw(Handle.Widget, @r);
  end;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:29  sg
  Initial revision

}
