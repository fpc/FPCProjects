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
//   TCustomListBox implementation for GTK+
// ===================================================================

// -------------------------------------------------------------------
//   TListBoxStrings
// -------------------------------------------------------------------

type

  TListBoxStrings = class(TStringList)
  protected
    ListBox: TCustomListBox;
  public
    constructor Create(AListBox: TCustomListBox);
    function Add(const S: string): Integer; override;
  end;


constructor TListBoxStrings.Create(AListBox: TCustomListBox);
begin
  inherited Create;
  ListBox := AListBox;
end;

function TListBoxStrings.Add(const S: string): Integer;
var
  item: PGtkWidget;
begin
  Result := inherited Add(s);
  if Assigned(ListBox.priv.list) then begin
    item := gtk_list_item_new_with_label(PChar(s));
    gtk_widget_show(item);
    gtk_container_add(PGtkContainer(ListBox.priv.list), item);
  end;
end;


// -------------------------------------------------------------------
//   TCustomListBox event handlers
// -------------------------------------------------------------------

function TCustomListBox_ButtonPressEvent(GtkWidget: PGtkWidget; event: PGdkEventButton;
  ListBox: TCustomListBox): Integer; cdecl;
begin
  if (event^.thetype = GDK_2BUTTON_PRESS) and Assigned(ListBox.FOnDblClick) then
    ListBox.FOnDblClick(ListBox);
  Result := 1;
end;


// -------------------------------------------------------------------
//   TCustomListBox
// -------------------------------------------------------------------

function TCustomListBox.GetItemIndex: Integer;
var
  list, cur: PGList;
  i: Integer;
begin
  if Assigned(priv.list) then begin
    list := gtk_container_children(PGtkContainer(priv.list));
    cur := list;
    i := 0;
    while Assigned(cur) do begin
      if GTK_WIDGET_HAS_FOCUS(PGtkWidget(cur^.data)) then begin
        Result := i;
	exit;
      end;
      Inc(i);
      cur := g_list_next(cur);
    end;
  end;
  Result := -1;
end;

procedure TCustomListBox.OnFinishCreation;
var
  i: integer;
  item: PGtkWidget;
begin
  FHandle := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(FHandle),
    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  priv.list := gtk_list_new;

  gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(FHandle), priv.list);

  inherited OnFinishCreation;

  for i := 0 to FItems.Count - 1 do begin
    item := gtk_list_item_new_with_label(PChar(FItems[i]));
    gtk_container_add(PGtkContainer(priv.list), item);
  end;
  gtk_widget_show_all(priv.list);

  gtk_signal_connect_after(PGtkObject(priv.list), 'button_press_event',
    GTK_SIGNAL_FUNC(@TCustomListBox_ButtonPressEvent), Self);
end;

procedure TCustomListBox.DoRecalcLayout;
begin
  MinW := 16;
  MinH := 16;
  DefW := 16;
  DefH := 16;
  MaxW := InfiniteSize;
  MaxH := InfiniteSize;
end;

constructor TCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TListBoxStrings.Create(Self);
end;

procedure TCustomListBox.Clear;
var
  list: PGList;
begin
  if Assigned(priv.list) then begin
    list := gtk_container_children(PGtkContainer(priv.list));
    gtk_list_remove_items(PGtkList(priv.list), list);
    g_list_free(list);
  end;
  FItems.Clear;
end;


{
  $Log$
  Revision 1.5  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.4  2000/02/18 18:27:23  sg
  * Changed creation order so that the list items can get a correct size

  Revision 1.3  2000/02/17 22:30:12  sg
  * Major rewrite to get more VCL-like and to get platform-independent
  * TListBox now creates a scrolling window with a GTK listbox inside. GTK's
    default listbox widget does not support scrolling.

  Revision 1.2  2000/01/24 00:30:55  sg
  * Extended copyright to year 2000...
  * Modifications to reflect the changed definition of TWidgetHandle

  Revision 1.1  2000/01/10 09:54:11  peter
    * listbox added

}
