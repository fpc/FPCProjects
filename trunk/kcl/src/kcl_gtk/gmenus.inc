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
//   Implementation of menu related classes for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TMenuItem
// -------------------------------------------------------------------

constructor TMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMenuItem.Destroy;
begin
  if Assigned(FCommand) then
    FCommand.RemoveEventFilter(TCmdChangeEventObj, @CommandChangeFilter);
  inherited Destroy;
end;

procedure TMenuItem_Activate(GtkItem: PGtkMenuItem; item: TMenuItem); cdecl;
begin
  if Assigned(item.FCommand) then
    item.FCommand.Execute(item);
end;

function TMenuItem.CommandChangeFilter(AEvent: TEventObj): Boolean;
var
  cmd: TCommand;
begin
  if AEvent.InheritsFrom(TCmdChangeEventObj) then begin
    ASSERT(AEvent.Sender.InheritsFrom(TCommand));
    ASSERT(TCmdChangeEventObj(AEvent).Sender.InheritsFrom(TCommand));
    cmd := TCommand(TCmdChangeEventObj(AEvent).Sender);
    case TCmdChangeEventObj(AEvent).ChangeType of
      cctEnabled:
	gtk_widget_set_sensitive(FHandle, cmd.FEnabled);
    end;
  end;
  Result := True;
end;

procedure TMenuItem.OnFinishCreation;
var
  s: String;
  i, LeftIndent: Integer;
  form: TWinControl;
  menu: TMenuItem;
  icon: PImageListItem;
  imglist: TImageList;
  accel: LongWord;
  mods: TGdkModifierType;
  w: PGtkWidget;
  sensitive: Boolean;
begin
  case FStyle of
    misDefault, misSeparator:
      priv.Item := gtk_menu_item_new;
    misCheck: begin
        priv.Item := gtk_check_menu_item_new;
	gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(priv.Item), True);
      end;
    else ASSERT(False);
  end;
  FHandle := priv.Item;

  if FStyle <> misSeparator then begin
    if Assigned(FCommand) then
      s := ConvertUnderlines(FCommand.FText)
    else
      SetLength(s, 0);

    priv.Packer := gtk_packer_new;
    gtk_widget_show(priv.Packer);
    gtk_container_add(PGtkContainer(priv.Item), priv.Packer);

    icon := nil;
    if Assigned(FCommand) and Assigned(FCommand.FCommandList) then begin
      imglist := FCommand.FCommandList.FImages;
      if Assigned(imglist) and (FCommand.FImageIndex >= 0)
        and (FCommand.FImageIndex < imglist.priv.Count) then begin
        icon := PImageListItem(imglist.priv.Items[FCommand.FImageIndex]);
        priv.Pixmap := gtk_pixmap_new(icon^.ImageCanvas.Handle, icon^.Mask);
        gtk_widget_show(priv.Pixmap);
        gtk_packer_add(PGtkPacker(priv.Packer), priv.Pixmap, GTK_SIDE_LEFT, GTK_ANCHOR_W,
	  0, 0, 0, 0, 0, 0);
      end;
    end;

    if not Assigned(icon) then
      if FParent.InheritsFrom(TMenuBar) then
        LeftIndent := 0
      else
        LeftIndent := 16 + 31	// ### use maximum icon width in menu instead of 31
    else
      LeftIndent := 16;

    priv.TextLabel := gtk_label_new('');

    accel := gtk_label_parse_uline(PGtkLabel(priv.TextLabel), PChar(s));

    gtk_widget_show(PGtkWidget(priv.TextLabel));
    gtk_packer_add(PGtkPacker(priv.Packer), PGtkWidget(priv.TextLabel),
      GTK_SIDE_LEFT, GTK_ANCHOR_W, 0, 0, LeftIndent, 0, 0, 0);

    if accel <> GDK_VoidSymbol then
      if FParent.InheritsFrom(TMenuBar) then begin
        form := TMenuBar(FParent);
        while Assigned(form) and not form.InheritsFrom(TCustomForm) do
          form := form.FParent;
        if Assigned(form) then
          gtk_widget_add_accelerator(priv.Item, 'activate_item',
            TCustomForm(form).priv.AccelGroup,
            accel, GDK_MOD1_MASK, GTK_ACCEL_LOCKED)
      end else
        if FParent.InheritsFrom(TMenu) then
          gtk_widget_add_accelerator(priv.Item, 'activate_item',
            gtk_menu_ensure_uline_accel_group(PGtkMenu(TMenu(FParent).
            priv.SubMenu)), accel, 0, GTK_ACCEL_LOCKED);


    if Assigned(FCommand) and (Length(FCommand.FShortCut) > 0) then begin
      menu := Self;
      while Assigned(menu.FParent) and menu.FParent.InheritsFrom(TMenuItem) do
        menu := TMenuItem(menu.FParent);
      // 'menu' must be a TMenuBar now; now search our form
      form := TMenuBar(menu.FParent);
      while Assigned(form) and not form.InheritsFrom(TCustomForm) do
        form := form.FParent;
      if Assigned(form) then begin
        priv.AccelLabel := gtk_label_new(PChar(ShortcutDisplayName(FCommand.FShortCut)));
	gtk_widget_show(priv.AccelLabel);
        gtk_packer_add(PGtkPacker(priv.Packer), PGtkWidget(priv.AccelLabel),
          GTK_SIDE_RIGHT, GTK_ANCHOR_E, 0, 0, 8, 0, 0, 0);
      end;
    end;

    if (FStyle = misCheck) and Assigned(FCommand) and FCommand.Checked then
      gtk_check_menu_item_set_active(PGtkCheckMenuItem(priv.Item), True);

    gtk_signal_connect(PGtkObject(priv.Item), 'activate',
      GTK_SIGNAL_FUNC(@TMenuItem_Activate), Self);

    // Set up an event filter for automatic updates of this menu item
    if Assigned(FCommand) then
      FCommand.AddEventFilter(TCmdChangeEventObj, @CommandChangeFilter);

    sensitive := True;
    if Assigned(FCommand) and (not FCommand.FEnabled) then
      sensitive := False
    else if Self.InheritsFrom(TMenu) then begin
      if TMenu(Self).FItems.Count = 0 then
        sensitive := False;
    end else
      if not Assigned(FCommand.FOnExecute) then
        sensitive := False;
  end else		// if FStyle <> misSeparator then begin
    sensitive := False;

  gtk_widget_set_sensitive(FHandle, sensitive);
end;


// -------------------------------------------------------------------
//   TMenu
// -------------------------------------------------------------------

procedure TMenu.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
begin
  inherited OnFinishCreation;
  priv.submenu := gtk_menu_new;
  gtk_menu_item_set_submenu(PGtkMenuItem(priv.item), priv.submenu);

  MenuPriv.TearoffItem := gtk_tearoff_menu_item_new;
  gtk_widget_show(MenuPriv.TearoffItem);
  gtk_menu_append(PGtkMenu(priv.submenu), MenuPriv.TearoffItem);

  for i := 0 to FItems.Count - 1 do begin
    item := TMenuItem(FItems.Items[i]);
    item.OnFinishCreation;
    gtk_menu_append(PGtkMenu(priv.submenu), item.priv.item);
    gtk_widget_show(item.priv.item);
  end;
  gtk_widget_show(priv.submenu);
  gtk_widget_show(priv.item);
end;

procedure TMenu.AddItem(AItem: TMenuItem);
begin
  AItem.FParent := Self;
  FItems.Add(AItem);
  if Assigned(priv.submenu) then begin
    gtk_menu_append(PGtkMenu(priv.submenu), AItem.priv.item);
    gtk_widget_show(AItem.priv.item);
  end;
end;


// -------------------------------------------------------------------
//   TMenuBar
// -------------------------------------------------------------------

procedure TMenuBar.AddItem(AItem: TMenuItem);
begin
  AItem.FParent := Self;
  FItems.Add(AItem);
  if HandleAllocated then begin
    AItem.OnFinishCreation;
    gtk_menu_bar_append(PGtkMenuBar(FHandle), AItem.priv.item);
    RecalcLayout;
  end;
end;

procedure TMenuBar.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
begin
  FHandle := gtk_menu_bar_new;
  inherited OnFinishCreation;

  for i := 0 to FItems.Count - 1 do begin
    item := TMenuItem(FItems.Items[i]);
    item.OnFinishCreation;
    gtk_menu_bar_append(PGtkMenuBar(FHandle), Item.priv.item);
  end;

  RecalcLayout;
end;

procedure TMenuBar.DoRecalcLayout;
begin
  //WriteLn(Format('TMenuBar.DoRecalcLayout: Min=%dx%d, Def=%dx%d', [MinW, MinH, DefW, DefH]));
  if DefH <= 1 then begin
    DefH := 20;
    if DefW <= 1 then DefW := 50;
  end else begin
    MinW := MinH * 4;
    DefW := MinH;
    DefH := MinH;
  end;
end;


{
  $Log$
  Revision 1.8  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.7  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.6  2000/02/10 18:44:54  sg
  * Event filters are removed correctly now
  * Added layout calculation to TMenuBar

  Revision 1.5  2000/01/24 18:37:07  sg
  * Moved the declaration of gtk_check_menu_item_new to the proper location,
    fpc/packages/gtk/gtk/gtkcheckmenuitem.pp

  Revision 1.4  2000/01/24 00:25:25  sg
  * Major reimplementation to support TCommand, including support for
    displaying an icon left to a menu item; corrected shortcut handling

  Revision 1.3  2000/01/07 22:49:47  sg
  * Added full shortcut support for menu items

  Revision 1.2  2000/01/06 23:02:24  sg
  * "&" in menu item texts are now interpreted as underline command, and the
    according keyboard accelerators are created

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
