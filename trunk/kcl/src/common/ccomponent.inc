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
//   Implementation of TKCLComponent
// ===================================================================

destructor TKCLComponent.Destroy;
var
  i: Integer;
  filter: PEventFilter;
begin
  if Assigned(FilterList) then begin
    for i := 0 to FilterList.Count - 1 do begin
      filter := PEventFilter(FilterList.Items[i]);
      Dispose(filter);
    end;
    FilterList.Free;
  end;
  inherited Destroy;
end;

procedure TKCLComponent.AddEventFilter(EventClass: TEventClass;
  FilterFn: TEventFilterFn);
var
  filter: PEventFilter;
begin
  ASSERT(Assigned(FilterFn));

  if not Assigned(FilterList) then
    FilterList := TList.Create;

  New(filter);
  filter^.EventClass := EventClass;
  filter^.FilterFn := FilterFn;
  FilterList.Add(filter);
end;

procedure TKCLComponent.RemoveEventFilter(EventClass: TEventClass;
  FilterFn: TEventFilterFn);
var
  i: Integer;
  filter: PEventFilter;
begin
  ASSERT(Assigned(FilterList));
  for i := 0 to FilterList.Count - 1 do begin
    filter := PEventFilter(FilterList.Items[i]);
    if (filter^.EventClass = EventClass) and (filter^.FilterFn = FilterFn) then begin
      Dispose(filter);
      FilterList.Delete(i);
      break;
    end;
  end;
end;

procedure DumpEvent(Dest: TObject; Event: TEventObj);
type
  PObject = ^TObject;
var
  ClassInfo: PTypeInfo;
  ClassTypeData, TypeData: PTypeData;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, i: Integer;

  c: Char;
  obj: TObject;
  s, s2: String;
begin
  s := Event.ClassName;
  if (Copy(s, 1, 1) = 'T') and (Copy(s, Length(s) - 7, 8) = 'EVENTOBJ') then
    s := Copy(s, 2, Length(s) - 9);
  Write(s);

  ClassInfo := Event.ClassInfo;
  ClassTypeData := GetTypeData(ClassInfo);
  PropCount := ClassTypeData^.PropCount;
  GetMem(PropList, SizeOf(TPropInfo) * PropCount);
  PropCount := GetPropList(ClassInfo, tkAny, PropList);
  for i := 0 to PropCount - 1 do
  begin
    PropInfo := PropList^[i];
    if i = 0 then
      Write(': ')
    else
      Write(', ');
    Write(PropList^[i]^.Name, '=');
    TypeData := GetTypeData(PropInfo^.PropType);
    case PropInfo^.PropType^.Kind of
      tkChar, tkWChar:
        begin
          c := Chr(GetOrdProp(Event, PropInfo));
	  if (c >= ' ') and (c < #255) and (c <> #127) then
            Write('''', c, '''')
	  else
	    Write('#', Ord(c));
        end;
      tkInteger: Write(GetOrdProp(Event, PropInfo));
      tkEnumeration: Write(GetOrdProp(Event, PropInfo));
      tkSet: Write(GetOrdProp(Event, PropInfo));
      tkClass:
        begin
          obj := PObject(Pointer(Event)+Longint(PropInfo^.GetProc))^;
	  //if obj.InheritsFrom(TComponent) and (Length(TComponent(obj).Name) > 0) then
	  //  Write(TComponent(obj).Name)
	  if obj.InheritsFrom(TComponent) then
	  begin
	    s := '';
	    while Assigned(obj) do
	    begin
	      s2 := TPersistent(obj).GetNamePath;
	      if Length(s2) = 0 then s2 := obj.ClassName;
	      if Length(s) > 0 then
	        s := s2 + '.' + s
	      else
	        s := s2;
	      obj := TComponent(obj).Owner;
	    end;
	    Write(s)
	  end else
	    Write(obj.ClassName);
	end;
      else Write('??? ', Ord(PropInfo^.PropType^.Kind));
    end;
  end;
  WriteLn;
  FreeMem(PropList);
end;

function TKCLComponent.CallEventFilters(Event: TEventObj): Boolean;
var
  i: Integer;
  filter: PEventFilter;
begin
  {$IFDEF DebugEvents}
  DumpEvent(Self, Event);
  {$ENDIF}
  Result := True;
  if Assigned(FilterList) then
    for i := 0 to FilterList.Count - 1 do
    begin
      filter := PEventFilter(FilterList.Items[i]);
      if Event.InheritsFrom(filter^.EventClass) and not filter^.FilterFn(Event) then
      begin
        Result := false;
	break;
      end;
    end;
  Event.Release;
end;


{
  $Log$
  Revision 1.5  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.4  2000/02/10 18:27:57  sg
  * Added "RemoveEventFilter"
  * Memory leaks fixed

  Revision 1.3  2000/01/24 00:17:58  sg
  * Implement event handler registration and event distribution

  Revision 1.2  2000/01/05 19:09:58  sg
  * Improved event dumping alot

  Revision 1.1.1.1  1999/12/30 21:33:09  sg
  Initial import

}
