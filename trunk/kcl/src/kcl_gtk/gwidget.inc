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
//   TWidget Implementation
// ===================================================================

// -------------------------------------------------------------------
//   GTK signal handlers
// -------------------------------------------------------------------

procedure TWidget_Destroy(GtkWidget: PGtkWidget; Widget: TWidget); cdecl;
begin
  ASSERT(Assigned(Widget));
  ASSERT(Widget.FHandle = GtkWidget);
  Widget.FHandle := nil;
end;

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

procedure TWidget_SizeRequest(widget: PGtkWidget; req: PGtkRequisition;
  AWidget: TWidget); cdecl;
begin
  req^.Width := AWidget.MinW;
  req^.Height := AWidget.MinH;
end;

procedure TWidget_SizeAllocate(widget: PGtkWidget; alloc: PGtkAllocation;
  AWidget: TWidget); cdecl;
begin
  //WriteLn('Size allocated for ', AWidget.ClassName, ': ', alloc^.Width, 'x', alloc^.Height);
end;


// -------------------------------------------------------------------
//   TWidget property handling
// -------------------------------------------------------------------

procedure TWidget.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> FEnabled then begin
    FEnabled := AEnabled;
    if Assigned(FHandle) then
      gtk_widget_set_sensitive(FHandle, FEnabled);
  end;
end;

procedure TWidget.SetText(const AText: String);
begin
  FText := AText;
  if Assigned(FHandle) then
    RecalcLayout;
end;

procedure TWidget.SetHint(const AHint: String);
begin
  FHint := AHint;
end;


// -------------------------------------------------------------------
//   TWidget protected methods
// -------------------------------------------------------------------

procedure TWidget.Show;
begin
  ASSERT(Assigned(FHandle));
  gtk_widget_show(FHandle);
end;

procedure TWidget.Hide;
begin
  ASSERT(Assigned(FHandle));
  gtk_widget_hide(FHandle);
end;

procedure TWidget.OnFinishCreation;
begin
  ASSERT(Assigned(FHandle));

  gtk_signal_connect(PGtkObject(FHandle), 'destroy', GTK_SIGNAL_FUNC(@TWidget_Destroy), Self);

  gtk_signal_connect(PGtkObject(FHandle), 'size_request',
    GTK_SIGNAL_FUNC(@TWidget_SizeRequest), Self);

  gtk_signal_connect_after(PGtkObject(FHandle), 'size_allocate',
    GTK_SIGNAL_FUNC(@TWidget_SizeAllocate), Self);

  if FShowHint then
    gtk_tooltips_set_tip(gToolTips, FHandle, PChar(FHint), PChar(FHint));

  if not FEnabled then
    gtk_widget_set_sensitive(FHandle, False);

  // GTK-connect all signals
  gtk_signal_connect(PGtkObject(FHandle), 'focus_in_event',
    GTK_SIGNAL_FUNC(@TWidget_FocusInEvent), self);
  gtk_signal_connect(PGtkObject(FHandle), 'focus_out_event',
    GTK_SIGNAL_FUNC(@TWidget_FocusOutEvent), self);
end;

procedure TWidget.DoRecalcLayout;
var
  SizeReq: TGtkRequisition;
begin
  ASSERT(Assigned(FHandle));

  PGtkWidgetClass(gtk_type_class(GTK_OBJECT_TYPE(PGtkObject(FHandle))))^.
    size_request(FHandle, @SizeReq);

  DefW := SizeReq.Width;
  DefH := SizeReq.Height;
  MinW := DefW;
  MinH := DefH;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
end;

procedure TWidget.ApplySize;
begin
  if Assigned(FHandle) and ((FWidth <> FHandle^.allocation.width)
    or (FHeight <> FHandle^.allocation.height)) then
    gtk_widget_set_usize(FHandle, FWidth, FHeight);
end;


// -------------------------------------------------------------------
//   TWidget public methods
// -------------------------------------------------------------------

constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := True;
  FEnabled := True;
  FCanExpandWidth := True;
  FCanExpandHeight := True;
end;

destructor TWidget.Destroy;
begin
  if Assigned(FHandle) then
    gtk_widget_destroy(FHandle);
  inherited Destroy;
end;

procedure TWidget.SetPosition(x, y: Integer);
begin
  if Assigned(FHandle) then
    gtk_widget_set_uposition(FHandle, x, y);
end;

procedure TWidget.Redraw(AtOnce: Boolean);
var
  r: TGdkRectangle;
begin
  if Assigned(FHandle) then
    if AtOnce then begin
      r.x := 0;
      r.y := 0;
      r.Width := FWidth;
      r.Height := FHeight;
      gtk_widget_draw(FHandle, @r);
    end else
      gtk_widget_queue_draw(FHandle);
end;

procedure TWidget.Redraw(const Rect: TRect; AtOnce: Boolean);
begin
  if Assigned(FHandle) then
    gtk_widget_queue_draw_area(FHandle, Rect.Left, Rect.Top,
      Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

procedure TWidget.Redraw(ax, ay, aw, ah: Integer; AtOnce: Boolean);
var
  r: TGdkRectangle;
begin
  if Assigned(FHandle) then
    if AtOnce then begin
      r.x := ax;
      r.y := ay;
      r.Width := aw;
      r.Height := ah;
      gtk_widget_draw(FHandle, @r);
    end else
      gtk_widget_queue_draw_area(FHandle, ax, ay, aw, ah);
end;

procedure TWidget.SetFocus;
begin
  if Assigned(FHandle) then
    gtk_window_set_focus(
      PGtkWindow(gtk_widget_get_toplevel(FHandle)), FHandle);
end;

function TWidget.HasFocus: Boolean;
begin
  if Assigned(FHandle) then
    Result := GTK_WIDGET_HAS_FOCUS(FHandle)
  else
    Result := False;
end;


{
  $Log$
  Revision 1.7  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.6  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.5  2000/02/18 18:29:56  sg
  * Default size requests are now done directly by calling the GTK widget's
    size_request method instead of calling gtk_widget_size_request.

  Revision 1.4  2000/02/17 22:31:09  sg
  * When the widget text has changed, RecalcLayout is now called

  Revision 1.3  2000/02/10 18:48:50  sg
  * Implemented new size and layout methods

  Revision 1.2  2000/01/24 00:28:42  sg
  * Added "Enabled" property

  Revision 1.1.1.1  1999/12/30 21:33:29  sg
  Initial import

}
