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

// GTK+ idle handler
procedure TApplication_Idle(app: TApplication); cdecl;
begin
  if Assigned(app.FOnIdle) then
    app.FOnIdle(app);
end;

procedure TApplication.SetOnIdle(AOnIdle: TNotifyEvent);
begin
  if Assigned(AOnIdle) and (priv.IdleHandle = 0) then
    priv.IdleHandle := gtk_idle_add(TGtkFunction(@TApplication_Idle), Self)
  else if not Assigned(AOnIdle) and (priv.IdleHandle > 0) then begin
    gtk_idle_remove(priv.IdleHandle);
    priv.IdleHandle := 0;
  end;
  FOnIdle := AOnIdle;
end;

procedure TApplication.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // Terminate if the last form is being destroyed
  if (Operation = opRemove) and (ComponentCount = 1) then
    Terminate;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TApplication.Initialize;
begin
  gtk_init(@argc, @argv);
  gToolTips := gtk_tooltips_new;
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
type
  PForm = ^TCustomForm;
var
  form: PForm;
begin
  form := @Reference;
  form^ := TCustomForm(InstanceClass.Create(Self));
  if Assigned(ReadFormProc) then
    ReadFormProc(form^);
  form^.FinishCreation;
end;

procedure TApplication.AddForm(AForm: TCustomForm);
begin
  AForm.FinishCreation;
end;

procedure TApplication.Run;
var
  i: Integer;
  form: TCustomForm;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWidget) then
      TWidget(Components[i]).FinishCreation;
  IsRunning := True;
  SetOnIdle(FOnIdle);

  gtk_main;
  IsRunning := False;
end;

procedure TApplication.Terminate;
begin
  if IsRunning then
    gtk_main_quit;
end;


{
  $Log$
  Revision 1.5  2000/02/17 22:26:13  sg
  * Implemented support for idle handler

  Revision 1.4  2000/02/10 18:39:27  sg
  * TGenericForm has been changed to TCustomForm
  * Changed the form management to prevent memory leaks and for correct
    handling of closed forms

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
