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

procedure gtk_freefixed_size_allocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  children: PGList;
  child: PGtkFixedChild;
begin
  widget^.allocation := allocation^;

  if GTK_WIDGET_REALIZED(widget) then
    gdk_window_move_resize(widget^.window, allocation^.x, allocation^.y,
      allocation^.width, allocation^.height);

  children := PGtkFixed(widget)^.children;
  while Assigned(children) do begin
    child := children^.data;
    if GTK_WIDGET_VISIBLE(child^.widget) then
      gtk_widget_size_allocate(child^.widget, @child^.widget^.allocation);
    children := children^.next;
  end;
end;

procedure gtk_freefixed_class_init(cls: PGtkFixedClass); cdecl;
begin
  PGtkWidgetClass(cls)^.size_allocate := @gtk_freefixed_size_allocate;
end;

procedure gtk_freefixed_init(fixed: PGtkFixed); cdecl;
begin
end;

const
  FreefixedInfo: TGtkTypeInfo = (
    type_name: 'GtkFreefixed';
    object_size: SizeOf(TGtkFixed);
    class_size: SizeOf(TGtkFixedClass);
    class_init_func: @gtk_freefixed_class_init;
    object_init_func: TGtkObjectInitFunc(@gtk_freefixed_init);
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil);
var
  FreefixedType: TGtkType;


function gtk_freefixed_get_type: TGtkType;
begin
  if FreefixedType = 0 then begin
    FreefixedType := gtk_type_unique(gtk_fixed_get_type, @FreefixedInfo);
  end;  
  Result := FreefixedType;
end;

function gtk_freefixed_new: PGtkWidget;
begin
  Result := gtk_type_new(gtk_freefixed_get_type);
end;


// -------------------------------------------------------------------
//   TLayout itself
// -------------------------------------------------------------------

procedure TLayout.AddFixedChild(AChild: TWidget);
begin
  gtk_fixed_put(PGtkFixed(FHandle), AChild.FHandle, 0, 0);
end;

procedure TLayout.OnFinishCreation;
begin
  FHandle := gtk_freefixed_new;
  inherited OnFinishCreation;
end;


{
  $Log$
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
