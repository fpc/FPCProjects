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
//   Column list widget implementation for GTK+
// ===================================================================

constructor TListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems:=TStringList.Create;
end;


destructor TListBox.Destroy;
begin
  if FItemGtkWidgetsCnt>0 then
   Freemem(FItemGtkWidgets);
  inherited Destroy;
end;


procedure TListBox.OnFinishCreation;
var
  i : integer;
begin
  Handle.Widget := gtk_list_new;
  for i := 0 to FItems.Count - 1 do
   begin
     ItemGtkWidgets[i] := gtk_list_item_new_with_label(pchar(FItems[i]));
     gtk_container_add(PGtkContainer(Handle.Widget), ItemGtkWidgets[i]);
     gtk_widget_show(ItemGtkWidgets[i]);
   end;
  inherited OnFinishCreation;
end;


function TListBox.GetItemGtkWidget(index: integer): PGtkWidget;
begin
  if index<FItemGtkWidgetsCnt then
   Result:=FItemGtkWidgets^[index]
  else
   Result:=nil;
end;


procedure TListBox.SetItemGtkWidget(index: integer;widget: PGtkWidget);
begin
  if index>=FItemGtkWidgetsCnt then
   begin
     inc(FItemGtkWidgetsCnt,16);
     ReAllocMem(FItemGtkWidgets,sizeof(widget)*FItemGtkWidgetsCnt);
   end;
  FItemGtkWidgets^[index]:=widget;
end;


procedure TListBox.SetItems(AItems: TStrings);
var
  i : integer;
begin
  FItems.Assign(AItems);
  if assigned(Handle.Widget) then
   begin
     for i := 0 to FItems.Count - 1 do
      begin
        ItemGtkWidgets[i] := gtk_list_item_new_with_label(pchar(FItems[i]));
        gtk_container_add(PGtkContainer(Handle.Widget), ItemGtkWidgets[i]);
        gtk_widget_show(ItemGtkWidgets[i]);
      end;
   end;
end;


procedure TListBox.AddItem(AString: string);
var
  i : integer;
begin
  FItems.Add(AString);
  if assigned(Handle.Widget) then
   begin
     i:=FItems.Count-1;
     ItemGtkWidgets[i] := gtk_list_item_new_with_label(pchar(FItems[i]));
     gtk_container_add(PGtkContainer(Handle.Widget), ItemGtkWidgets[i]);
     gtk_widget_show(ItemGtkWidgets[i]);
   end;
end;


procedure TListBox.Clear;
var
  i : integer;
begin
  if assigned(Handle.Widget) then
   begin
     for i := 0 to FItems.Count - 1 do
      gtk_container_remove(PGtkContainer(Handle.Widget), ItemGtkWidgets[i]);
   end;
  FItemGtkWidgetsCnt:=0;
  Freemem(FItemGtkWidgets);
end;

{
  $Log$
  Revision 1.1  2000/01/10 09:54:11  peter
    * listbox added

}
