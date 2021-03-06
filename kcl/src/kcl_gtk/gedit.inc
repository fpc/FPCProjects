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
//   Edit widget implementation for GTK+
// ===================================================================

procedure TCustomEdit.OnFinishCreation;
begin
  if FMultiLine then begin
    FHandle := gtk_text_new(nil, nil);
    gtk_text_set_editable(PGtkText(FHandle), True);
  end else begin
    FHandle := gtk_entry_new;
    if Length(FText) > 0 then
      gtk_entry_set_text(PGtkEntry(FHandle), PChar(FText));
  end;
  inherited OnFinishCreation;
end;

procedure TCustomEdit.DoRecalcLayout;
begin
  if FDefaultWidth > 0 then begin
    MinW := FDefaultWidth;	// ### not correct; only until TToolBar can size a TEdit correctly
    DefW := FDefaultWidth;
  end else begin
    MinW := 20;
    DefW := 40;
  end;
  MaxW := InfiniteSize;
  if not FMultiLine then
    MaxH := DefH;
end;


function TCustomEdit.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.ClassType = TTextChangedEventObj then
    if HandleAllocated then
      gtk_entry_set_text(PGtkEntry(FHandle), PChar(FText));

  Result := inherited ProcessEvent(Event);
end;


function TCustomEdit.GetText: String;
begin
  if HandleAllocated then begin
    FText := StrPas(gtk_entry_get_text(PGtkEntry(FHandle)));
    Result := FText;
  end else
    Result := inherited Gettext;
end;


{
  $Log$
  Revision 1.10  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.9  2000/02/25 13:27:20  michael
  + removed debug statement

  Revision 1.8  2000/02/25 13:26:10  michael
  + Added gettext read for tedit.text

  Revision 1.7  2000/02/25 13:15:58  michael
  + Added gettext read for tedit.text

  Revision 1.6  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.5  2000/02/22 14:40:32  sg
  * More layout fixes; minor improvements

  Revision 1.4  2000/02/18 22:19:49  sg
  * Implemented SetText

  Revision 1.3  2000/02/17 22:27:24  sg
  * Implemented "DoRecalcLayout"

  Revision 1.2  2000/01/24 00:23:14  sg
  * Implemented multi-line support (switchable via property "MultiLine")

  Revision 1.1.1.1  1999/12/30 21:33:33  sg
  Initial import

}
