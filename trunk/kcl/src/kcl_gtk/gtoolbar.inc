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
//   ToolBar widget implementation for GTK+
// ===================================================================

function StripUnderlines(const s: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(s) do begin
    if s[i] = '&' then Inc(i);
    if i <= Length(s) then		// For the case that s ends with "&"
      Result := Result + s[i];
    Inc(i);
  end;
end;


procedure TToolButton_Clicked(GtkButton: PGtkButton;
  button: TToolButton); cdecl;
begin
  if Assigned(button.FCommand) then
    button.FCommand.Execute(button);
end;


destructor TToolBar.Destroy;
var
  i: Integer;
  btn: TToolButton;
begin
  for i := 0 to Items.Count - 1 do
    if TObject(Items.Items[i]).InheritsFrom(TToolButton) then begin
      btn := TToolButton(Items.Items[i]);
      if Assigned(btn.FCommand) then
        btn.FCommand.RemoveEventFilter(TCmdChangeEventObj, @CommandChangeFilter);
    end;
  Items.Free;
  inherited Destroy;
end;

function TToolBar.CommandChangeFilter(AEvent: TEventObj): Boolean;

  function FindButton(cmd: TCommand): TToolButton;
  var
    i: Integer;
    item: TObject;
  begin
    Result := nil;
    for i := 0 to Items.Count - 1 do begin
      item := TObject(Items.Items[i]);
      if (item.InheritsFrom(TToolButton)) and (TToolButton(item).FCommand = cmd) then begin
        Result := TToolButton(item);
	break;
      end;
    end;
  end;

var
  cmd: TCommand;
  btn: TToolButton;
begin
  if AEvent.InheritsFrom(TCmdChangeEventObj) then begin
    ASSERT(AEvent.Sender.InheritsFrom(TCommand));
    ASSERT(TCmdChangeEventObj(AEvent).Sender.InheritsFrom(TCommand));
    cmd := TCommand(TCmdChangeEventObj(AEvent).Sender);
    case TCmdChangeEventObj(AEvent).ChangeType of
      cctEnabled: begin
          btn := FindButton(TCommand(AEvent.Sender));
	  if Assigned(btn) then
	    gtk_widget_set_sensitive(btn.priv.Widget, cmd.FEnabled);
        end;
    end;
  end;
  Result := True;
end;

procedure TToolBar.InternalAddItem(AItem: TKCLComponent);
var
  ToolTip, Shortcut: String;
  pToolTip: PChar;
  widget: TWidget;
  btn: TToolButton;
  cmd: TCommand;
  image: PImageListItem;
  pixmap: PGtkWidget;
  ElType: TGtkToolbarChildType;
begin
  if AItem.InheritsFrom(TToolButton) then begin
    btn := TToolButton(AItem);
    cmd := nil;
    pixmap := nil;
    SetLength(ToolTip, 0);
    if btn.FStyle in [tbsButton, tbsCheck, tbsDropDown] then begin
      cmd := btn.FCommand;
      if Assigned(cmd) then begin
        if Assigned(cmd.FCommandList)
          and Assigned(cmd.FCommandList.FImages) and (cmd.FImageIndex >= 0)
          and (cmd.FImageIndex < cmd.FCommandList.FImages.priv.count) then begin
	  // Create pixmap for the icon on the button
          image := PImageListItem(cmd.FCommandList.FImages.priv.Items[cmd.FImageIndex]);
          pixmap := gtk_pixmap_new(image^.ImageCanvas.pixmap, image^.Mask);
	end;
	ToolTip := StripUnderlines(cmd.FText);
	if Length(cmd.FShortCut) > 0 then
	  ToolTip := Format('%s  (%s)', [ToolTip, ShortcutDisplayName(cmd.FShortCut)]);
      end;
      ElType := GTK_TOOLBAR_CHILD_BUTTON;

      // Set up an event filter for automatic updates of this ToolButton
      if Assigned(btn.FCommand) then
        btn.FCommand.AddEventFilter(TCmdChangeEventObj, @CommandChangeFilter);
    end else begin
      ElType := GTK_TOOLBAR_CHILD_SPACE;
    end;
    if Length(ToolTip) = 0 then
      pToolTip := nil
    else
      pToolTip := PChar(ToolTip);

    btn.priv.Widget := gtk_toolbar_append_element(priv.ToolBar, ElType, nil,
      '', pToolTip, nil, pixmap, nil, nil);

    if Assigned(btn.priv.Widget) then
      gtk_signal_connect(PGtkObject(btn.priv.Widget), 'clicked',
        GTK_SIGNAL_FUNC(@TToolButton_Clicked), AItem);

    if Assigned(cmd) and ((not cmd.FEnabled) or not Assigned(cmd.FOnExecute)) then
      gtk_widget_set_sensitive(btn.priv.Widget, False);

  end else begin

    ASSERT(AItem.InheritsFrom(TWidget));
    widget := TWidget(AItem);

    widget.FinishCreation;

    ToolTip := widget.FHint;
    gtk_toolbar_append_element(priv.ToolBar, GTK_TOOLBAR_CHILD_WIDGET,
      widget.FHandle, nil, nil, nil, nil, nil, nil);
  end;
end;

procedure TToolBar.OnFinishCreation;
var
  i: Integer;
begin
  FHandle := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(PGtkFrame(FHandle), GTK_SHADOW_OUT);

  priv.ToolBar := PGtkToolbar(gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,
    GTK_TOOLBAR_ICONS));
  gtk_container_add(PGtkContainer(FHandle), PGtkWidget(priv.ToolBar));
  gtk_widget_show(PGtkWidget(priv.ToolBar));

  gtk_toolbar_set_button_relief(priv.ToolBar, GTK_RELIEF_NONE);

  for i := 0 to Items.Count - 1 do
    InternalAddItem(TKCLComponent(Items.Items[i]));

  inherited OnFinishCreation;
end;

procedure TToolBar.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  DefW := MinH;
  MinW := MinH * 4;
end;


{
  $Log$
  Revision 1.3  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.2  2000/02/10 18:48:03  sg
  * Fixed memory leaks
  * Added layout calculation to TToolBar

  Revision 1.1  2000/01/24 00:15:23  sg
  * First version

}
