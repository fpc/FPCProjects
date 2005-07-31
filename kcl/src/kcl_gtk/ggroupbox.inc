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
//   TCustomGroupBox implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   Define internal GTK widget: spacingframe
//     This widget is derived from gtk_frame; the only difference is
//     that the spacingframe widget adds an inner border of 4 pixels
//     to each side of the frame.
//     NOTE: The following function is derived from the original GTK
//	     source, which is (C) 1995-1997 by Peter Mattis,
//	     Spencer Kimball and Josh MacDonald
// -------------------------------------------------------------------

procedure spacingframe_size_allocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  child: PGtkWidget;
  ChildAllocation: TGtkAllocation;
  BorderWidth: Integer;
begin
  ASSERT(Assigned(widget) and Assigned(allocation));

  if GTK_WIDGET_MAPPED(widget) and
     ((widget^.allocation.x <> allocation^.x) or (widget^.allocation.width <> allocation^.width) or
      (widget^.allocation.y <> allocation^.y) or (widget^.allocation.height <> allocation^.height))
     and (widget^.allocation.width <> 0) and (widget^.allocation.height <> 0) then
    gtk_widget_queue_clear(widget);

  widget^.allocation := allocation^;

  child := PGtkBin(widget)^.child;
  if Assigned(child) and GTK_WIDGET_VISIBLE(child) then begin
    BorderWidth := PGtkContainer(widget)^.flag0 and bm_TGtkContainer_border_width;
    ChildAllocation.x := BorderWidth + PGtkStyle(widget^.thestyle)^.klass^.xthickness + 4;
    ChildAllocation.width := Max(1, allocation^.width - ChildAllocation.x * 2 - 4);
    ChildAllocation.y := BorderWidth +
      Max(PGtkFrame(widget)^.label_height, PGtkStyle(widget^.thestyle)^.klass^.ythickness) + 4;
    ChildAllocation.height := Max(1, allocation^.width - ChildAllocation.y -
      BorderWidth - PGtkStyle(widget^.thestyle)^.klass^.ythickness - 4);
    Inc(ChildAllocation.x, allocation^.x);
    Inc(ChildAllocation.y, allocation^.y);
    gtk_widget_size_allocate(child, @ChildAllocation);
  end;
end;


procedure spacingframe_class_init(cls: PGtkWidgetClass); cdecl;
begin
  cls^.size_allocate := @spacingframe_size_allocate;
end;


const
  SpacingFrameInfo: TGtkTypeInfo = (
    type_name: 'SpacingFrame';
    object_size: SizeOf(TGtkFrame);
    class_size: SizeOf(TGtkFrameClass);
    class_init_func: @spacingframe_class_init;
    object_init_func: nil;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil);

var
  SpacingFrameType: TGtkType;


function spacingframe_get_type: TGtkType;
begin
  if SpacingFrameType = 0 then
    SpacingFrameType := gtk_type_unique(gtk_frame_get_type, @SpacingFrameInfo);

  Result := SpacingFrameType;
end;


function spacingframe_new(const ALabel: PChar): PGtkWidget;
begin
  Result := gtk_type_new(spacingframe_get_type);
  gtk_frame_set_label(PGtkFrame(Result), ALabel);
end;


// -------------------------------------------------------------------
//   TCustomGroupBox
// -------------------------------------------------------------------

procedure TCustomGroupBox.OnFinishCreation;
begin
  FHandle := spacingframe_new(PChar(FText));

  if Assigned(FContent) and FContent.InheritsFrom(TWinControl) then
    TWinControl(FContent).HandleNeeded;

  inherited OnFinishCreation;
end;

procedure TCustomGroupBox.DoRecalcLayout;
var
  MinWidthWithLabel, BorderWidth, ContentDefW, ContentDefH,
    ContentMinW, ContentMinH, ContentMaxW, ContentMaxH: Integer;
begin
  MinWidthWithLabel := PGtkFrame(FHandle)^.label_width + 8;
  BorderWidth := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width;
  MinW := BorderWidth + PGtkStyle(FHandle^.thestyle)^.klass^.xthickness * 2 + 8;
  MinH := BorderWidth + PGtkFrame(FHandle)^.label_height +
    PGtkStyle(FHandle^.thestyle)^.klass^.ythickness + 8;
  DefW := MinW;
  DefH := MinH;
  MaxW := MinW;
  MaxH := MinH;
  if Assigned(FContent) then begin
    ContentDefW := FContent.DefW;
    ContentDefH := FContent.DefH;
    ContentMinW := FContent.MinW;
    ContentMinH := FContent.MinH;
    ContentMaxW := FContent.MaxW;
    ContentMaxH := FContent.MaxH;
  end else begin
    ContentDefW := 50;
    ContentDefH := 50;
    ContentMinW := 20;
    ContentMinH := 20;
    ContentMaxW := InfiniteSize;
    ContentMaxH := InfiniteSize;
  end;

  if ContentDefW > MinWidthWithLabel then
    Inc(DefW, ContentDefW)
  else
    Inc(DefW, MinWidthWithLabel);

  if ContentMinW > MinWidthWithLabel then
    Inc(MinW, ContentMinW)
  else
    Inc(MinW, MinWidthWithLabel);

  Inc(DefH, ContentDefH);
  Inc(MinH, ContentMinH);

  if ContentMaxW > MinWidthWithLabel then
    MaxW := Min(InfiniteSize, MaxW + ContentMaxW)
  else
    Inc(MaxW, MinWidthWithLabel);

  MaxH := Min(InfiniteSize, MaxH + ContentMaxH);
end;

procedure TCustomGroupBox.LayoutChildren;
begin
end;

function TCustomGroupBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.ClassType = TTextChangedEventObj then
    if HandleAllocated then
      gtk_frame_set_label(PGtkFrame(FHandle), PChar(FText));

  Result := inherited ProcessEvent(Event);
end;


{
  $Log$
  Revision 1.5  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.4  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.3  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.2  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.1  2000/02/17 22:32:24  sg
  * First implementation

}
