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
//   ScrollWindow widget implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TRange implementations (wrap GTK adjustments)
// -------------------------------------------------------------------

type

  TGtkAdjRange = class(TRange)
  protected
    adj: PGtkAdjustment;
    function  GetMaxValue: Integer; override;
    procedure SetMaxValue(NewMaxValue: Integer); override;
    function  GetCurValue: Integer; override;
    procedure SetCurValue(NewCurValue: Integer); override;
    function  GetPageSize: Integer; override;
    procedure SetPageSize(NewPageSize: Integer); override;
    constructor Create;
  end;

  TGtkScrollboxRange = class(TGtkAdjRange)
    ScrollBox: TScrollBox;
    constructor Create(AScrollBox: TScrollBox);
    procedure SetMaxValue(NewMaxValue: Integer); override;
  end;


constructor TGtkAdjRange.Create;
begin
  adj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
end;

function TGtkAdjRange.GetMaxValue: Integer;
begin
  Result := Trunc(adj^.upper);
end;

procedure TGtkAdjRange.SetMaxValue(NewMaxValue: Integer);
begin
  if NewMaxValue = Trunc(adj^.upper) then exit;
  adj^.upper := NewMaxValue;
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
  adj^.page_size := NewPageSize;
  gtk_adjustment_changed(adj);
end;


constructor TGtkScrollboxRange.Create(AScrollBox: TScrollBox);
begin
  inherited Create;
  ScrollBox := AScrollBox;
end;

procedure TGtkScrollboxRange.SetMaxValue(NewMaxValue: Integer);
begin
  inherited SetMaxValue(NewMaxValue);
  if ScrollBox.FCreationState = wcsCreationDone then
    if Assigned(ScrollBox.FContent) and
      Assigned(ScrollBox.FContent.FHandle) then
      gtk_widget_set_usize(ScrollBox.FContent.FHandle,
        ScrollBox.HorzRange.MaxValue, ScrollBox.VertRange.MaxValue);
end;


// -------------------------------------------------------------------
//   TScrollBox
// -------------------------------------------------------------------

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzRange := TGtkScrollboxRange.Create(Self);
  FVertRange := TGtkScrollboxRange.Create(Self);
end;

procedure TScrollBox_SetupContent(sb: TScrollBox);
begin
  sb.FContent.FinishCreation;
  gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(sb.FHandle),
      sb.FContent.FHandle);
  sb.RecalcLayout;
end;

procedure TScrollBox.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;
  if Assigned(FHandle) then
    TScrollBox_SetupContent(Self);
end;

procedure TScrollBox.OnFinishCreation;
begin
  FHandle := gtk_scrolled_window_new(
    TGtkAdjRange(FHorzRange).adj, TGtkAdjRange(FVertRange).adj);

  gtk_scrolled_window_set_policy(PGtkScrolledWindow(FHandle),
    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  inherited OnFinishCreation;

  if Assigned(FContent) then begin
    TScrollBox_SetupContent(Self);
    ASSERT(Assigned(FContent.FHandle));
    gtk_widget_set_usize(FContent.FHandle,
      FHorzRange.MaxValue, FVertRange.MaxValue);
  end;
end;

procedure TScrollBox.OnRecalcLayout;
begin
  if Assigned(FContent.FHandle) then
    gtk_widget_set_usize(FContent.FHandle,
      FContent.Width, FContent.Height);
end;

procedure TScrollBox.OnSetSize;
begin
  inherited OnSetSize;
end;

procedure TScrollBox.OnSizeChanged;
begin
  inherited OnSizeChanged;
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
