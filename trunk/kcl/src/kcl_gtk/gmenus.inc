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


constructor TMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TMenuItem.SetText(const AText: String);
begin
  FText := AText;
end;

procedure TMenuItem.SetGray(AGray: Boolean);
begin
  if AGray <> FGray then begin
    FGray := AGray;
    if Assigned(FHandle.Item) then begin
      if FGray then
        gtk_widget_set_state(FHandle.item, GTK_STATE_INSENSITIVE)
      else
        gtk_widget_set_sensitive(FHandle.Item, True);
    end;
  end;
end;

procedure TMenuItem_Activate(GtkItem: PGtkMenuItem; item: TMenuItem); cdecl;
begin
  item.DoClick;
end;

procedure TMenuItem.OnFinishCreation;
var
  s: String;
  i: Integer;
  lbl: PGtkWidget;
  accel: LongWord;
  mods: TGdkModifierType;
  form: TWidget;
  menu: TMenuItem;
begin
  if FText = '-' then
    FHandle.Item := gtk_menu_item_new
  else begin
    // Replace all "&" in FText with "_" (the underline character in GTK)
    s := FText;
    for i := 1 to Length(s) do
      if s[i] = '&' then
        s[i] := '_';

    FHandle.Item := gtk_menu_item_new;
    lbl := gtk_accel_label_new('');		// NIL won't work!
    gtk_misc_set_alignment(PGtkMisc(lbl), 0.0, 0.5);
    gtk_container_add(PGtkContainer(FHandle.Item), lbl);
    gtk_accel_label_set_accel_widget(PGtkAccelLabel(lbl), FHandle.Item);

    accel := gtk_label_parse_uline(PGtkLabel(lbl), PChar(s));
    if accel <> GDK_VoidSymbol then
      if FParent.InheritsFrom(TMenuBar) then begin
        form := TMenuBar(FParent);
	while Assigned(form) and not form.InheritsFrom(TGenericForm) do
	  form := form.FParent;
	if Assigned(form) then
          gtk_widget_add_accelerator(FHandle.Item, 'activate',
	    TGenericForm(form).priv.AccelGroup,
	    accel, GDK_MOD1_MASK, GTK_ACCEL_LOCKED)
      end else if FParent.InheritsFrom(TMenu) then
        gtk_widget_add_accelerator(FHandle.Item, 'activate',
	  gtk_menu_ensure_uline_accel_group(PGtkMenu(TMenu(FParent).
	  FHandle.SubMenu)), accel, 0, GTK_ACCEL_LOCKED);

    if Length(FShortCut) > 0 then begin
      menu := Self;
      while Assigned(menu.FParent) and menu.FParent.InheritsFrom(TMenuItem) do
        menu := TMenuItem(menu.FParent);
      // 'menu' must be a TMenuBar now
      form := TMenuBar(menu.FParent);
      while Assigned(form) and not form.InheritsFrom(TGenericForm) do
	form := form.FParent;
      if Assigned(form) then begin
        gtk_accelerator_parse(PChar(FShortCut), @accel, @mods);
        gtk_widget_add_accelerator(FHandle.Item, 'activate',
	  TGenericForm(form).priv.AccelGroup,
	  accel, mods, GTK_ACCEL_VISIBLE);
      end;
    end;

    gtk_widget_show(lbl);
  end;

  if FGray then
    gtk_widget_set_state(FHandle.Item, GTK_STATE_INSENSITIVE);

  gtk_signal_connect(PGtkObject(FHandle.Item), 'activate',
    GTK_SIGNAL_FUNC(@TMenuItem_Activate), Self);
end;



constructor TMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TCollection.Create(TMenuItemInfo);
end;

procedure TMenu.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
begin
  inherited OnFinishCreation;
  FHandle.submenu := gtk_menu_new;
  gtk_menu_item_set_submenu(PGtkMenuItem(FHandle.item), FHandle.submenu);
  for i := 0 to FItems.Count - 1 do begin
    item := TMenuItemInfo(FItems.Items[i]).Item;
    item.OnFinishCreation;
    gtk_menu_append(PGtkMenu(FHandle.submenu), item.FHandle.item);
    gtk_widget_show(item.FHandle.item);
  end;
  gtk_widget_show(FHandle.submenu);
end;

procedure TMenu.AddItem(AItem: TMenuItem);
var
  NewItem: TMenuItemInfo;
begin
  AItem.FParent := Self;
  NewItem := TMenuItemInfo(FItems.Add);
  NewItem.Item := AItem;
  if Assigned(FHandle.submenu) then begin
    gtk_menu_append(PGtkMenu(FHandle.submenu), AItem.FHandle.item);
    gtk_widget_show(AItem.FHandle.item);
  end;
end;

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TCollection.Create(TMenuItemInfo);
end;

procedure TMenuBar.AddItem(AItem: TMenuItem);
var
  NewItem: TMenuItemInfo;
begin
  AItem.FParent := Self;
  NewItem := TMenuItemInfo(FItems.Add);
  NewItem.Item := AItem;
  if Assigned(Handle.Widget) then begin
    AItem.OnFinishCreation;
    gtk_menu_bar_append(PGtkMenuBar(Handle.Widget), AItem.FHandle.item);
  end;
end;

procedure TMenuBar.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
begin
  Handle.Widget := gtk_menu_bar_new;

  for i := 0 to FItems.Count - 1 do begin
    item := TMenuItemInfo(FItems.Items[i]).Item;
    item.OnFinishCreation;
    gtk_menu_bar_append(PGtkMenuBar(Handle.Widget), Item.FHandle.item);
    gtk_widget_show(item.FHandle.item);
  end;

  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.3  2000/01/07 22:49:47  sg
  * Added full shortcut support for menu items

  Revision 1.2  2000/01/06 23:02:24  sg
  * "&" in menu item texts are now interpreted as underline command, and the
    according keyboard accelerators are created

  Revision 1.1.1.1  1999/12/30 21:33:25  sg
  Initial import

}
