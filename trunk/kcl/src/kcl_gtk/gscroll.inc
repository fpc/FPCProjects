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
//   Scroll widgets implementations for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TRange implementations (wrap GTK adjustments)
// -------------------------------------------------------------------

type

  TGtkAdjRange = class(TRange)
  protected
    adj: PGtkAdjustment;
    FPageSize: Integer;
    function  GetMaxValue: Integer; override;
    procedure SetMaxValue(NewMaxValue: Integer); override;
    function  GetCurValue: Integer; override;
    procedure SetCurValue(NewCurValue: Integer); override;
    function  GetPageSize: Integer; override;
    procedure SetPageSize(NewPageSize: Integer); override;
    constructor Create;
  end;

  TScrollBoxRange = class(TGtkAdjRange)
  protected
    ScrollBox: TCustomScrollBox;
    constructor Create(AScrollBox: TCustomScrollBox);
    procedure SetMaxValue(NewMaxValue: Integer); override;
  end;

  TScrollersRange = class(TGtkAdjRange)
  protected
    Scrollers: TCustomScrollers;
    constructor Create(AScrollers: TCustomScrollers);
    procedure SetPageSize(NewPageSize: Integer); override;
    procedure SetMaxValue(NewMaxValue: Integer); override;
  end;


procedure TGtkAdjRange_ValueChanged(adj: PGtkAdjustment; range: TGtkAdjRange); cdecl;
begin
  if Assigned(range.FOnValueChange) then
    range.FOnValueChange(range);
end;

constructor TGtkAdjRange.Create;
begin
  adj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
  gtk_signal_connect(PGtkObject(adj), 'value_changed',
    GTK_SIGNAL_FUNC(@TGtkAdjRange_ValueChanged), self);
end;

function TGtkAdjRange.GetMaxValue: Integer;
begin
  Result := Trunc(adj^.upper);
end;

procedure TGtkAdjRange.SetMaxValue(NewMaxValue: Integer);
begin
  if NewMaxValue = Trunc(adj^.upper) then exit;
  adj^.upper := NewMaxValue;
  adj^.page_size := FPageSize;
  gtk_adjustment_changed(adj);
end;

function TGtkAdjRange.GetCurValue: Integer;
begin
  Result := Trunc(adj^.value);
end;

procedure TGtkAdjRange.SetCurValue(NewCurValue: Integer);
begin
  if NewCurValue < 0 then
    NewCurValue := 0;
  if NewCurValue + PageSize > MaxValue then
    NewCurValue := MaxValue - PageSize;
  gtk_adjustment_set_value(adj, NewCurValue);
end;

function TGtkAdjRange.GetPageSize: Integer;
begin
  Result := Trunc(adj^.page_size);
end;

procedure TGtkAdjRange.SetPageSize(NewPageSize: Integer);
begin
  FPageSize := NewPageSize;
  adj^.page_size := NewPageSize;
  gtk_adjustment_changed(adj);
end;


constructor TScrollBoxRange.Create(AScrollBox: TCustomScrollBox);
begin
  inherited Create;
  ScrollBox := AScrollBox;
end;

procedure TScrollBoxRange.SetMaxValue(NewMaxValue: Integer);
begin
  inherited SetMaxValue(NewMaxValue);
  if ScrollBox.FCreationState = wcsCreationDone then
    if Assigned(ScrollBox.FContent) and ScrollBox.FContent.HandleAllocated then
      gtk_widget_set_usize(ScrollBox.FContent.FHandle,
        ScrollBox.HorzRange.MaxValue, ScrollBox.VertRange.MaxValue);
end;

constructor TScrollersRange.Create(AScrollers: TCustomScrollers);
begin
  inherited Create;
  Scrollers := AScrollers;
end;

procedure TScrollersRange.SetPageSize(NewPageSize: Integer);
begin
  if NewPageSize <> FPageSize then begin
    inherited SetPageSize(NewPageSize);
    if Scrollers.HandleAllocated then
      Scrollers.LayoutChildren;
  end;
end;

procedure TScrollersRange.SetMaxValue(NewMaxValue: Integer);
begin
  if NewMaxValue <> adj^.upper then begin
    inherited SetMaxValue(NewMaxValue);
    if Scrollers.HandleAllocated then
      Scrollers.LayoutChildren;
  end;
end;


// -------------------------------------------------------------------
//   TCustomScrollBox
// -------------------------------------------------------------------

constructor TCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzRange := TScrollBoxRange.Create(Self);
  FVertRange := TScrollBoxRange.Create(Self);
end;

destructor TCustomScrollBox.Destroy;
begin
  FHorzRange.Free;
  FVertRange.Free;
  inherited Destroy;
end;

procedure TCustomScrollBox.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;
//  if HandleAllocated then
//    TScrollBox_SetupContent(Self);
end;

procedure TCustomScrollBox.OnFinishCreation;
begin
  if Assigned(FContent) then
    FContent.HandleNeeded;

  FHandle := gtk_scrolled_window_new(TGtkAdjRange(FHorzRange).adj, TGtkAdjRange(FVertRange).adj);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(FHandle),
    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  priv.viewport := gtk_viewport_new(TGtkAdjRange(FHorzRange).adj, TGtkAdjRange(FVertRange).adj);
  gtk_widget_show(priv.viewport);
  gtk_container_add(PGtkContainer(FHandle), priv.viewport);

  inherited OnFinishCreation;

  if Assigned(FContent) then begin
    gtk_container_add(PGtkContainer(priv.viewport), FContent.FHandle);
    gtk_widget_set_usize(FContent.FHandle, FHorzRange.MaxValue, FVertRange.MaxValue);
  end;
end;

procedure TCustomScrollBox.DoRecalcLayout;
var
  BorderSize: Integer;
begin
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
  if FContent.HandleAllocated then begin
    //BorderSize := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width; ###
    BorderSize := 2;
    MinW := 2 * BorderSize;
    MinH := 2 * BorderSize;
    DefW := FContent.Width + MinW;
    DefH := FContent.Height + MinH;
  end;
end;


// -------------------------------------------------------------------
//   TCustomScrollers
// -------------------------------------------------------------------

constructor TCustomScrollers.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzRange := TScrollersRange.Create(Self);
  FVertRange := TScrollersRange.Create(Self);
end;

destructor TCustomScrollers.Destroy;
begin
  FHorzRange.Free;
  FVertRange.Free;
  inherited Destroy;
end;

procedure TCustomScrollers.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;
end;

procedure TCustomScrollers.OnFinishCreation;
begin
  if Assigned(FContent) then
    FContent.HandleNeeded;

  FHandle := gtk_fixed_new;

  priv.Frame := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(PGtkFrame(priv.Frame), GTK_SHADOW_IN);
  gtk_fixed_put(PGtkFixed(FHandle), priv.Frame, 0, 0);
  gtk_widget_show(priv.Frame);

  if Assigned(FContent) and FContent.HandleAllocated then
    gtk_container_add(PGtkContainer(priv.Frame), FContent.FHandle);

  priv.HorzScrollbar := gtk_hscrollbar_new(TGtkAdjRange(FHorzRange).adj);
  gtk_fixed_put(PGtkFixed(FHandle), priv.HorzScrollbar, 0, 0);

  priv.VertScrollbar := gtk_vscrollbar_new(TGtkAdjRange(FVertRange).adj);
  gtk_fixed_put(PGtkFixed(FHandle), priv.VertScrollbar, 0, 0);

  inherited OnFinishCreation;
end;

procedure TCustomScrollers.DoRecalcLayout;
var
  BorderSize: Integer;
begin
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
  if FContent.HandleAllocated then begin
    //BorderSize := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width; ###
    BorderSize := 2;
    MinW := 2 * BorderSize;
    MinH := 2 * BorderSize;
    DefW := FContent.DefW + MinW;
    DefH := FContent.DefH + MinH;
  end;
end;

procedure TCustomScrollers.LayoutChildren;
var
  HorzBarAlloc, VertBarAlloc, FrameAlloc: TGtkAllocation;
begin
  // Relayout the both scroll bars
  HorzBarAlloc := priv.HorzScrollbar^.allocation;
  VertBarAlloc := priv.VertScrollbar^.allocation;

  if FHorzRange.PageSize >= FHorzRange.MaxValue then
    HorzBarAlloc.Height := 0;

  if FVertRange.PageSize >= FVertRange.MaxValue then
    VertBarAlloc.Width := 0;

  HorzBarAlloc.y := FHeight - HorzBarAlloc.Height;
  if FWidth > VertBarAlloc.Width then
    HorzBarAlloc.Width := FWidth - VertBarAlloc.Width;
  VertBarAlloc.x := FWidth - VertBarAlloc.Width;
  if FHeight > HorzBarAlloc.Height then
    VertBarAlloc.Height := FHeight - HorzBarAlloc.Height;

  if HorzBarAlloc.Height > 0 then begin
    gtk_widget_size_allocate(priv.HorzScrollbar, @HorzBarAlloc);
    gtk_widget_show(priv.HorzScrollbar);
  end else
    gtk_widget_hide(priv.HorzScrollbar);

  if VertBarAlloc.Width > 0 then begin
    gtk_widget_size_allocate(priv.VertScrollbar, @VertBarAlloc);
    gtk_widget_show(priv.VertScrollbar);
  end else
    gtk_widget_hide(priv.VertScrollbar);


  // Relayout the frame
  FrameAlloc.x := 0;
  FrameAlloc.y := 0;
  FrameAlloc.Width := VertBarAlloc.Width + PGtkStyle(priv.Frame^.thestyle)^.klass^.xthickness * 2;
  if FWidth > FrameAlloc.Width then
    FrameAlloc.Width := FWidth - FrameAlloc.Width
  else
    FrameAlloc.Width := 1;

  FrameAlloc.Height := HorzBarAlloc.Height + PGtkStyle(priv.Frame^.thestyle)^.klass^.ythickness * 2;
  if FHeight > FrameAlloc.Height then
    FrameAlloc.Height := FHeight - FrameAlloc.Height
  else
    FrameAlloc.Height := 1;

  gtk_widget_size_allocate(priv.Frame, @FrameAlloc);
end;


// -------------------------------------------------------------------
//   THorzScrollBar and TVertScrollBar
// -------------------------------------------------------------------

constructor THorzScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRange := TGtkAdjRange.Create;
end;

procedure THorzScrollBar.OnFinishCreation;
begin
  FHandle := gtk_hscrollbar_new(nil);
  // ### Scroll bar not yet functional
  inherited OnFinishCreation;
end;


constructor TVertScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRange := TGtkAdjRange.Create;
end;

procedure TVertScrollBar.OnFinishCreation;
begin
  FHandle := gtk_vscrollbar_new(nil);
  // ### Scroll bar not yet functional
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.9  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.8  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.7  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.6  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.5  2000/02/18 18:28:35  sg
  * Started TScrollers

  Revision 1.4  2000/02/10 18:47:00  sg
  * Fixed memory leaks
  * Added layout calculation to TScrollBox

  Revision 1.3  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.2  2000/01/05 19:20:17  sg
  * TScrollBox: Removed the QuickHack with forwarding mouse events to an
    included PaintBox (not necessary anymore!)
  * Scroll bars of TScrollBox are only displayed if necessary (automatic mode)

  Revision 1.1.1.1  1999/12/30 21:33:44  sg
  Initial import

}
