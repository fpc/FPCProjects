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
//   Notebook widget implementation for GTK+
// ===================================================================

procedure TNotebookPage.SetText(const AText: String);
begin
  FText := AText;
  if Assigned(priv.LabelWidget) then
    gtk_label_set_text(priv.LabelWidget, PChar(AText));
end;

procedure TNotebookPage.SetContent(AContent: TWidget);
begin
  FContent := AContent;
end;



constructor TNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TCollection.Create(TNotebookPage);
end;

function TNotebook.GetPage(AIndex: Integer): TNotebookPage;
begin
  Result := TNotebookPage(FPages.Items[AIndex]);
end;

function TNotebook.GetCurPage: TNotebookPage;
begin
  if Assigned(Handle.Widget) then
    Result := TNotebookPage(FPages.Items
      [gtk_notebook_get_current_page(PGtkNotebook(Handle.Widget))])
  else
    Result := nil;
end;

function TNotebook.GetCurPageIndex: Integer;
begin
  if Assigned(Handle.Widget) then
    Result := gtk_notebook_get_current_page(PGtkNotebook(Handle.Widget))
  else
    Result := -1;
end;

procedure TNotebook.SetCurPageIndex(AIndex: Integer);
begin
  gtk_notebook_set_page(PGtkNotebook(Handle.Widget), AIndex);
end;

procedure TNotebook.OnFinishCreation;
var
  i: Integer;
  pg: TNotebookPage;
begin
  Handle.Widget := gtk_notebook_new;

  inherited OnFinishCreation;

  for i := 0 to FPages.Count - 1 do begin
    pg := TNotebookPage(FPages.Items[i]);
    pg.Content.FinishCreation;
    pg.priv.LabelWidget := PGtkLabel(gtk_label_new(PChar(pg.Text)));
    gtk_notebook_append_page(PGtkNotebook(Handle.Widget),
      pg.Content.Handle.Widget, PGtkWidget(pg.priv.LabelWidget));
  end;
end;

procedure TNotebook.OnSizeChanged;
begin
  inherited OnSizeChanged;
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

  if Assigned(Handle.Widget) then begin
    AWidget.FinishCreation;
    NewPage.priv.LabelWidget := PGtkLabel(gtk_label_new(PChar(NewPage.Text)));
    gtk_notebook_append_page(PGtkNotebook(Handle.Widget), AWidget.Handle.Widget,
      PGtkWidget(NewPage.priv.LabelWidget));
  end;
end;

procedure TNotebook.RemovePage(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then exit;
  gtk_notebook_remove_page(PGtkNotebook(Handle.Widget), AIndex);
  FPages.Items[AIndex].Free;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:24  sg
  Initial revision

}
