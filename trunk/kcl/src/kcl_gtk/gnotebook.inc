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

function TNotebook.GetCurPageIndex: Integer;
begin
  if Assigned(FHandle) then
    Result := gtk_notebook_get_current_page(PGtkNotebook(FHandle))
  else
    Result := -1;
end;

procedure TNotebook.SetCurPageIndex(AIndex: Integer);
begin
  gtk_notebook_set_page(PGtkNotebook(FHandle), AIndex);
end;

procedure TNotebook.OnFinishCreation;
var
  i: Integer;
  pg: TNotebookPage;
begin
  FHandle := gtk_notebook_new;

  inherited OnFinishCreation;

  for i := 0 to FPages.Count - 1 do begin
    pg := TNotebookPage(FPages.Items[i]);
    pg.Content.FinishCreation;
    pg.priv.LabelWidget := gtk_label_new('');
    TNotebookPage_SetText(pg, pg.FText);
    gtk_notebook_append_page(PGtkNotebook(FHandle), pg.Content.FHandle, pg.priv.LabelWidget);
  end;
end;

procedure TNotebook.DoRecalcLayout;
begin
  MinW := 50;
  MinH := 20;
  DefW := 100;
  DefH := 100;
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
end;

procedure TNotebook.RemovePage(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then exit;
  gtk_notebook_remove_page(PGtkNotebook(FHandle), AIndex);
  FPages.Items[AIndex].Free;
end;


{
  $Log$
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
