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
//   TWinControl Implementation
// ===================================================================

// -------------------------------------------------------------------
//   GTK signal handlers
// -------------------------------------------------------------------

procedure TWinControl_Destroy(GtkWidget: PGtkWidget; Control: TWinControl); cdecl;
begin
  ASSERT(Assigned(Control));
  ASSERT(Control.FHandle = GtkWidget);
  Control.FHandle := nil;
end;

function TWinControl_FocusInEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus;
  Control: TWinControl): Integer; cdecl;
begin
  if Assigned(Control.OnFocusIn) then
    Control.OnFocusIn(Control);
  Result := 1;
end;

function TWinControl_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus;
  Control: TWinControl): Integer; cdecl;
begin
  if Assigned(Control.OnFocusOut) then
    Control.OnFocusOut(Control);
  Result := 1;
end;

procedure TWinControl_SizeRequest(widget: PGtkWidget; req: PGtkRequisition;
  AControl: TWinControl); cdecl;
begin
  with AControl do begin
    // WriteLn('Size request for ', AWinControl.ClassName);

    MinW := req^.Width;
    MinH := req^.Height;
    DefW := req^.Width;
    DefH := req^.Height;
    MaxW := InfiniteSize;
    MaxH := InfiniteSize;

    DoRecalcLayout;

    if MinW = 0 then MinW := 1;
    if MinH = 0 then MinH := 1;
    if DefW < MinW then DefW := MinW;
    if DefH < MinH then DefH := MinH;

    if (not FCanExpandWidth) or (MaxW < DefW) then
      MaxW := DefW;
    if (not FCanExpandHeight) or (MaxH < DefH) then
      MaxH := DefH;

    req^.Width := DefW;
    req^.Height := DefH;
  end;
end;

procedure TWinControl_SizeAllocate(widget: PGtkWidget; alloc: PGtkAllocation;
  AControl: TWinControl); cdecl;
begin
  // WriteLn('Size allocated for ', AControl.ClassName, ': ', alloc^.Width, 'x', alloc^.Height);
  AControl.FWidth := alloc^.Width;
  AControl.FHeight := alloc^.Height;
  AControl.LayoutChildren;
end;


procedure TWinControl_Clicked(GtkWidget: PGtkWidget; Control: TWinControl); cdecl;
begin
  Control.Click;
end;


// -------------------------------------------------------------------
//   TWinControl property handling
// -------------------------------------------------------------------

function TWinControl.ProcessEvent(Event: TEventObj): Boolean;
begin
  if HandleAllocated then
  begin
    if Event.ClassType = TEnabledChangedEventObj then
    begin
      gtk_widget_set_sensitive(FHandle, FEnabled)
    end
    else if Event.ClassType = TVisibleChangedEventObj then
    begin
      if FVisible then
        gtk_widget_show(Handle)
      else
        gtk_widget_hide(FHandle);
    end;
  end;
  Result := inherited ProcessEvent(Event);
end;


// -------------------------------------------------------------------
//   TWinControl protected methods
// -------------------------------------------------------------------

procedure TWinControl.OnFinishCreation;
var
  i: Integer;
begin
  ASSERT(HandleAllocated);

  gtk_signal_connect(PGtkObject(FHandle), 'destroy', GTK_SIGNAL_FUNC(@TWinControl_Destroy), Self);

  gtk_signal_connect(PGtkObject(FHandle), 'size_request',
    GTK_SIGNAL_FUNC(@TWinControl_SizeRequest), Self);

  gtk_signal_connect(PGtkObject(FHandle), 'size_allocate',
    GTK_SIGNAL_FUNC(@TWinControl_SizeAllocate), Self);

  if FShowHint then
    gtk_tooltips_set_tip(gToolTips, FHandle, PChar(FHint), PChar(FHint));

  if not FEnabled then
    gtk_widget_set_sensitive(FHandle, False);

  // GTK-connect all signals
  gtk_signal_connect(PGtkObject(FHandle), 'focus_in_event',
    GTK_SIGNAL_FUNC(@TWinControl_FocusInEvent), self);
  gtk_signal_connect(PGtkObject(FHandle), 'focus_out_event',
    GTK_SIGNAL_FUNC(@TWinControl_FocusOutEvent), self);

  if Assigned(FParent) then
    gtk_container_add(PGtkContainer(FParent.FHandle), FHandle);


  if FVisible then
    gtk_widget_show(FHandle);
end;


procedure TWinControl.LayoutChildren;
begin
end;


// -------------------------------------------------------------------
//   TWinControl public methods
// -------------------------------------------------------------------

destructor TWinControl.Destroy;
begin
  if HandleAllocated then
    gtk_widget_destroy(FHandle);
  inherited Destroy;
end;

{procedure TWinControl.RecalcLayout;
begin
  if HandleAllocated then
    gtk_widget_queue_resize(FHandle);
end;}

function TWinControl.HandleAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;


procedure TWinControl.InsertControl(AControl: TControl);
begin
end;


procedure TWinControl.RemoveControl(AControl: TControl);
begin
end;


procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  alloc: TGtkAllocation;
begin
  if not HandleAllocated then exit;
  alloc.x := ALeft;
  alloc.y := ATop;

  {We have to adjust the upper left corner for the case that the parent of
   this widget has no underlying GDK window!}
  if Assigned(FParent) and Assigned(FParent.FHandle)
     and GTK_WIDGET_NO_WINDOW(FParent.FHandle) then begin
    Inc(alloc.x, FParent.FHandle^.allocation.x);
    Inc(alloc.y, FParent.FHandle^.allocation.y);
  end;

  alloc.width := Max(1, AWidth);
  alloc.height := Max(1, AHeight);
  FHandle^.allocation := alloc;
  gtk_widget_size_allocate(FHandle, @alloc);
end;

{procedure TWinControl.Redraw(AtOnce: Boolean);
var
  r: TGdkRectangle;
begin
  if HandleAllocated then
    if AtOnce then begin
      r.x := 0;
      r.y := 0;
      r.Width := FWidth;
      r.Height := FHeight;
      gtk_widget_draw(FHandle, @r);
    end else
      gtk_widget_queue_draw(FHandle);
end;}

{procedure TWinControl.Redraw(const Rect: TRect; AtOnce: Boolean);
begin
  if HandleAllocated then
    gtk_widget_queue_draw_area(FHandle, Rect.Left, Rect.Top,
      Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

procedure TWinControl.Redraw(ax, ay, aw, ah: Integer; AtOnce: Boolean);
var
  r: TGdkRectangle;
begin
  if HandleAllocated then
    if AtOnce then begin
      r.x := ax;
      r.y := ay;
      r.Width := aw;
      r.Height := ah;
      gtk_widget_draw(FHandle, @r);
    end else
      gtk_widget_queue_draw_area(FHandle, ax, ay, aw, ah);
end;

procedure TWinControl.SetFocus;
begin
  if HandleAllocated then
    gtk_window_set_focus(
      PGtkWindow(gtk_widget_get_toplevel(FHandle)), FHandle);
end;

function TWinControl.HasFocus: Boolean;
begin
  if HandleAllocated then
    Result := GTK_WIDGET_HAS_FOCUS(FHandle)
  else
    Result := False;
end;}


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
