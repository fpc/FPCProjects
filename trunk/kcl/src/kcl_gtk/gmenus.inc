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
begin
  if FText = '-' then
    FHandle.Item := gtk_menu_item_new
  else
    FHandle.Item := gtk_menu_item_new_with_label(PChar(FText));

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
  Revision 1.1  1999/12/30 21:33:25  sg
  Initial revision

}
