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
//   TLayout implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   Define internal GTK widget: gtk_freefixed
// -------------------------------------------------------------------

type
  PGtkFreefixed = ^TGtkFreefixed;
  TGtkFreefixed = packed record
    container: TGtkContainer;
    KCLLayout: TLayout;
  end;


procedure gtk_freefixed_map(widget: PGtkWidget); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
begin
  ASSERT(Assigned(widget));
  GTK_WIDGET_SET_FLAGS(widget, GTK_MAPPED);

  layout := PGtkFreefixed(widget)^.KCLLayout;
  if Assigned(layout) then
    for i := 0 to layout.FControls.Count - 1 do
    begin
      Control := TLayoutItem(layout.FControls.Items[i]).Control;
      if Control.InheritsFrom(TWinControl) then
      begin
        Child := TWinControl(Control).FHandle;
        if GTK_WIDGET_VISIBLE(Child) and not GTK_WIDGET_MAPPED(Child) then
          gtk_widget_map(Child);
      end;
    end;
end;

procedure gtk_freefixed_unmap(widget: PGtkWidget); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
begin
  ASSERT(Assigned(widget));
  GTK_WIDGET_UNSET_FLAGS(widget, GTK_MAPPED);

  Layout := PGtkFreefixed(widget)^.KCLLayout;
  if Assigned(Layout) then
    for i := 0 to Layout.FControls.Count - 1 do
    begin
      Control := TLayoutItem(layout.FControls.Items[i]).Control;
      if Control.InheritsFrom(TWinControl) then
      begin
        child := TWinControl(Control).FHandle;
        if GTK_WIDGET_VISIBLE(child) and GTK_WIDGET_MAPPED(child) then
          gtk_widget_unmap(child);
      end;
    end;
end;

procedure gtk_freefixed_size_request(widget: PGtkWidget; requisition: PGtkRequisition); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
  ChildRequisition: TGtkRequisition;
begin
  ASSERT(Assigned(widget) and Assigned(requisition));

  Layout := PGtkFreefixed(widget)^.KCLLayout;
  if Assigned(Layout) then begin
    for i := 0 to Layout.FControls.Count - 1 do begin
      Control := TLayoutItem(layout.FControls.Items[i]).Control;
      if Control.InheritsFrom(TWinControl) then
      begin
        child := TWinControl(Control).FHandle;
        if GTK_WIDGET_VISIBLE(child) then
          gtk_widget_size_request(child, @ChildRequisition);
      end;
    end;
    requisition^.width := layout.DefW;
    requisition^.height := layout.DefH;
  end;
end;

procedure gtk_freefixed_size_allocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
begin
  ASSERT(Assigned(widget) and Assigned(allocation));

  widget^.allocation := allocation^;

  Layout := PGtkFreefixed(widget)^.KCLLayout;
  if Assigned(Layout) then
    for i := 0 to layout.FControls.Count - 1 do
    begin
      Control := TLayoutItem(layout.FControls.Items[i]).Control;
      if Control.InheritsFrom(TWinControl) then
      begin
        Child := TWinControl(Control).FHandle;
        if GTK_WIDGET_VISIBLE(Child) then
          gtk_widget_size_allocate(Child, @Child^.allocation);
      end;
    end;
end;

procedure gtk_freefixed_draw(widget: PGtkWidget; area: PGdkRectangle); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
  ChildArea: TGdkRectangle;
begin
  ASSERT(Assigned(widget));

  if GTK_WIDGET_DRAWABLE(widget) then
  begin
    Layout := PGtkFreefixed(widget)^.KCLLayout;
    if Assigned(Layout) then
      for i := 0 to Layout.FControls.Count - 1 do
      begin
        Control := TLayoutItem(layout.FControls.Items[i]).Control;
	if Control.InheritsFrom(TWinControl) then
	begin
          Child := TWinControl(Control).FHandle;
	  if gtk_widget_intersect(Child, area, @ChildArea) <> 0 then
            gtk_widget_draw(Child, @ChildArea);
        end;
      end;
  end;
end;

function gtk_freefixed_expose(widget: PGtkWidget; event: PGdkEventExpose): Integer; cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
  Child: PGtkWidget;
  ChildArea: TGdkRectangle;
begin
  ASSERT(Assigned(widget));

  if GTK_WIDGET_DRAWABLE(widget) then
  begin
    Layout := PGtkFreefixed(widget)^.KCLLayout;
    if Assigned(Layout) then
      for i := 0 to Layout.FControls.Count - 1 do
      begin
        Control := TLayoutItem(layout.FControls.Items[i]).Control;
	if Control.InheritsFrom(TWinControl) then
	begin
          Child := TWinControl(Control).FHandle;
  	  if gtk_widget_intersect(Child, @event^.area, @ChildArea) <> 0 then
            gtk_widget_event(Child, PGdkEvent(event));
        end;
      end;
  end;
end;

procedure gtk_freefixed_add(container: PGtkContainer; widget: PGtkWidget); cdecl;
begin
  // Adding children is completely handled by TLayout
end;

procedure gtk_freefixed_remove(container: PGtkContainer; widget: PGtkWidget); cdecl;
begin
  ASSERT(Assigned(widget));
  gtk_widget_unparent(widget);
end;

procedure gtk_freefixed_forall(container: PGtkContainer; include_internals: LongBool;
  callback: TGtkCallback; callback_data: Pointer); cdecl;
var
  Layout: TLayout;
  i: Integer;
  Control: TControl;
begin
  ASSERT(Assigned(container));
  Layout := PGtkFreefixed(container)^.KCLLayout;
  if Assigned(Layout) then
    for i := 0 to layout.FControls.Count - 1 do
    begin
      Control := TLayoutItem(layout.FControls.Items[i]).Control;
      if Control.InheritsFrom(TWinControl) and TWinControl(Control).HandleAllocated then
        callback(TWinControl(Control).FHandle, callback_data);
    end;
end;

procedure gtk_freefixed_class_init(cls: PGtkWidgetClass); cdecl;
begin
  cls^.map := @gtk_freefixed_map;
  cls^.unmap := @gtk_freefixed_unmap;
  cls^.size_request := @gtk_freefixed_size_request;
  cls^.size_allocate := @gtk_freefixed_size_allocate;
  cls^.draw := @gtk_freefixed_draw;
  cls^.expose_event := @gtk_freefixed_expose;
  PGtkContainerClass(cls)^.add := @gtk_freefixed_add;
  PGtkContainerClass(cls)^.remove := @gtk_freefixed_remove;
  PGtkContainerClass(cls)^.forall := @gtk_freefixed_forall;
end;

procedure gtk_freefixed_init(freefixed: PGtkFreefixed); cdecl;
begin
  GTK_WIDGET_SET_FLAGS(PGtkWidget(freefixed), GTK_NO_WINDOW);
end;


const

  FreefixedInfo: TGtkTypeInfo = (
    type_name: 'GtkFreefixed';
    object_size: SizeOf(TGtkFreefixed);
    class_size: SizeOf(TGtkContainerClass);
    class_init_func: @gtk_freefixed_class_init;
    object_init_func: TGtkObjectInitFunc(@gtk_freefixed_init);
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil);


var

  FreefixedType: TGtkType;


function gtk_freefixed_get_type: TGtkType;
begin
  if FreefixedType = 0 then
    FreefixedType := gtk_type_unique(gtk_container_get_type, @FreefixedInfo);

  Result := FreefixedType;
end;

function gtk_freefixed_new(AWidget: TLayout): PGtkWidget;
begin
  Result := gtk_type_new(gtk_freefixed_get_type);
  PGtkFreefixed(Result)^.KCLLayout := AWidget;
end;


// -------------------------------------------------------------------
//   TLayout itself
// -------------------------------------------------------------------

procedure TLayout.AddFixedChild(AChild: TControl);
var
  Child: TWinControl;
begin
  if AChild.InheritsFrom(TWinControl) then
  begin
    Child := TWinControl(AChild);
    gtk_widget_set_parent(Child.FHandle, FHandle);
    if GTK_WIDGET_REALIZED(FHandle) then
      gtk_widget_realize(Child.FHandle);
    if GTK_WIDGET_VISIBLE(FHandle) and GTK_WIDGET_VISIBLE(Child.FHandle) and
      GTK_WIDGET_MAPPED(FHandle) then
      gtk_widget_map(Child.FHandle);
  end;
end;

procedure TLayout.OnFinishCreation;
begin
  FHandle := gtk_freefixed_new(Self);
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.8  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.7  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.6  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.5  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.4  2000/02/18 18:23:05  sg
  * Children are only moved if their creation has been finished correctly

  Revision 1.3  2000/02/10 18:44:10  sg
  * Small fixes

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
