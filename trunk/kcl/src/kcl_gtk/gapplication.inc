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
//   The main application object
// ===================================================================


destructor TApplication.Destroy;
begin
  if Assigned(gToolTips) then
    gtk_widget_destroy(PGtkWidget(gToolTips));
end;

procedure TApplication.Initialize;
begin
  FForms := TList.Create;
  gtk_init(@argc, @argv);
  gToolTips := gtk_tooltips_new();
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
type
  PForm = ^TGenericForm;
var
  form: PForm;
begin
  form := @Reference;
  form^ := TGenericForm(InstanceClass.Create(nil));
  FForms.Add(form^);
  if Assigned(ReadFormProc) then
    ReadFormProc(form^);
  form^.FinishCreation;
end;

procedure TApplication.AddForm(AForm: TGenericForm);
begin
  FForms.Add(AForm);
  AForm.FinishCreation;
end;

procedure TApplication.Run;
var
  i: Integer;
begin
  for i := 0 to FForms.Count - 1 do
    TForm(FForms.Items[i]).FinishCreation;
  gtk_main;
end;

procedure TApplication.Terminate;
begin
  gtk_main_quit;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:45  sg
  Initial revision

}
