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
//   Notebook widget implementation for GTK+
// ===================================================================

procedure TNotebookPage_SetText(APage: TNotebookPage; const AText: String);
var
  s: String;
  accel: LongWord;
  form: TWidget;
begin
  s := ConvertUnderlines(AText);
  accel := gtk_label_parse_uline(PGtkLabel(APage.priv.LabelWidget), PChar(s));
  if accel <> GDK_VoidSymbol then begin
    form := APage.FNotebook;
    while Assigned(form) and not form.InheritsFrom(TCustomForm) do
      form := form.FParent;
    if Assigned(form) then begin
      gtk_widget_add_accelerator(APage.priv.LabelWidget, 'grab_focus',
        TCustomForm(form).priv.AccelGroup, accel, GDK_MOD1_MASK, GTK_ACCEL_LOCKED);
    end;
  end;
end;

procedure TNotebookPage.SetText(const AText: String);
var
  s: String;
begin
  FText := AText;
  if Assigned(priv.LabelWidget) then
    TNotebookPage_SetText(Self, FText);
end;

procedure TNotebookPage.SetContent(AContent: TWidget);
begin
  FContent := AContent;
end;



function TNotebook.GetCurPage: TNotebookPage;
begin
  if Assigned(FHandle) then
    Result := TNotebookPage(FPages.Items
      [gtk_notebook_get_current_page(PGtkNotebook(FHandle))])
  else
    Result := nil;
end;

function TNotebook.GetPageIndex: Integer;
begin
  if Assigned(FHandle) then
    Result := gtk_notebook_get_current_page(PGtkNotebook(FHandle))
  else
    Result := -1;
end;

procedure TNotebook.SetPageIndex(AIndex: Integer);
begin
  gtk_notebook_set_page(PGtkNotebook(FHandle), AIndex);
end;

procedure TNotebook.OnFinishCreation;
var
  i: Integer;
  pg: TNotebookPage;
begin
  FHandle := gtk_notebook_new;
  gtk_notebook_set_scrollable(PGtkNotebook(FHandle), True);

  for i := 0 to FPages.Count - 1 do begin
    pg := TNotebookPage(FPages.Items[i]);
    pg.priv.BorderWidget := gtk_hbox_new(False, 0);

    pg.Content.FinishCreation;
    gtk_box_pack_start(PGtkBox(pg.priv.BorderWidget), pg.Content.FHandle, True, True, 0);
    gtk_container_set_border_width(PGtkContainer(pg.priv.BorderWidget), pg.BorderWidth);
    gtk_widget_show(pg.priv.BorderWidget);

    pg.priv.LabelWidget := gtk_label_new('');
    TNotebookPage_SetText(pg, pg.FText);

    gtk_notebook_append_page(PGtkNotebook(FHandle), pg.priv.BorderWidget, pg.priv.LabelWidget);
  end;

  inherited OnFinishCreation;
end;

procedure TNotebook.DoRecalcLayout;
var
  i, ythick, border: Integer;
  pg: TNotebookPage;
  lbl: PGtkWidget;
  LabelReq: TGtkRequisition;
begin
  ythick := PGtkStyle(FHandle^.thestyle)^.klass^.ythickness;

  FrameBorders.Left := PGtkStyle(FHandle^.thestyle)^.klass^.xthickness;
  FrameBorders.Right := FrameBorders.Left;
  FrameBorders.Top := 0;
  FrameBorders.Bottom := ythick;

  MinW := 0;
  MinH := 0;
  DefW := 0;
  DefH := 0;

  for i := 0 to FPages.Count - 1 do begin
    pg := TNotebookPage(FPages.Items[i]);
    border := pg.BorderWidth * 2;
    MinW := Max(MinW, pg.Content.MinW + border);
    MinH := Max(MinH, pg.Content.MinH + border);
    MaxW := Min(MaxW, pg.Content.MaxW + border);
    MaxH := Min(MaxH, pg.Content.MaxH + border);
    DefW := Max(DefW, pg.Content.DefW + border);
    DefH := Max(DefH, pg.Content.DefH + border);
    lbl := pg.priv.LabelWidget;
    if Assigned(lbl) then begin
      gtk_widget_size_request(lbl, @LabelReq);
      FrameBorders.Top := Max(FrameBorders.Top, LabelReq.Height + 2 * ythick);
    end;
  end;

  Inc(FrameBorders.Top, 2 * (PGtkNotebook(FHandle)^.tab_vborder + 1) + ythick);

  Inc(MinW, FrameBorders.Left + FrameBorders.Right);
  Inc(MinH, FrameBorders.Top + FrameBorders.Bottom);
  MaxW := Min(InfiniteSize, MaxW + FrameBorders.Left + FrameBorders.Right);
  MaxH := Min(InfiniteSize, MaxH + FrameBorders.Top + FrameBorders.Bottom);
  Inc(DefW, FrameBorders.Left + FrameBorders.Right);
  Inc(DefH, FrameBorders.Top + FrameBorders.Bottom);
end;

procedure TNotebook.LayoutChildren;
begin
end;

function TNotebook.AddPage(AText: String; AWidget: TWidget): Integer;
var
  NewPage: TNotebookPage;
begin
  AWidget.FParent := Self;
  Result := FPages.Count;
  NewPage := TNotebookPage(FPages.Add);
  NewPage.FText := AText;
  NewPage.FContent := AWidget;
  NewPage.FNotebook := Self;

  if Assigned(FHandle) then begin
    AWidget.FinishCreation;
    NewPage.priv.LabelWidget := gtk_label_new('');
    TNotebookPage_SetText(NewPage, NewPage.Text);
    gtk_notebook_append_page(PGtkNotebook(FHandle), AWidget.FHandle, NewPage.priv.LabelWidget);
  end;
  RecalcLayout;
end;

procedure TNotebook.RemovePage(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then exit;
  gtk_notebook_remove_page(PGtkNotebook(FHandle), AIndex);
  FPages.Items[AIndex].Free;
end;


{
  $Log$
  Revision 1.7  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.6  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.5  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.4  2000/02/18 18:28:13  sg
  * Added dummy default sizes (the sizes calculated by GTK 1.2.7 cannot be
    used anymore)

  Revision 1.3  2000/02/10 18:45:49  sg
  * Moved some GTK-independent methods back to cnotebook.inc

  Revision 1.2  2000/01/24 00:25:58  sg
  * Started implementation of shortcut support for notebook pages
    (doesn't work yet!)

  Revision 1.1.1.1  1999/12/30 21:33:24  sg
  Initial import

}
