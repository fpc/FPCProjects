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
//   The main application object
// ===================================================================


destructor TApplication.Destroy;
begin
  if Assigned(gToolTips) then
    gtk_widget_destroy(PGtkWidget(gToolTips));
  if Assigned(FForms) then
    FForms.Free;
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
  ASSERT(Assigned(FForms), 'TApplication.CreateForm: Forms = nil; missing call to "Initialize" in application?');
  form := @Reference;
  form^ := TGenericForm(InstanceClass.Create(nil));
  FForms.Add(form^);
  if Assigned(ReadFormProc) then
    ReadFormProc(form^);
  form^.FinishCreation;
end;

procedure TApplication.AddForm(AForm: TGenericForm);
begin
  ASSERT(Assigned(FForms), 'TApplication.AddForm: Forms = nil; missing call to "Initialize" in application?');
  FForms.Add(AForm);
  AForm.FinishCreation;
end;

procedure TApplication.Run;
var
  i: Integer;
begin
  ASSERT(Assigned(FForms), 'TApplication.Run: Forms = nil; missing call to "Initialize" in application?');
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
  Revision 1.3  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.2  1999/12/31 19:19:36  sg
  * Added ASSERTions
  ! Problem regarding memory leaks: Forms added with "CreateForm" instead of
    "AddForm" won't be freed automatically. We should make a decision if we
    support "AddForm" or "CreateForm", supporting both is nonsense.

  Revision 1.1.1.1  1999/12/30 21:33:45  sg
  Initial import

}
