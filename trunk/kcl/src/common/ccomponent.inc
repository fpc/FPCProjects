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

procedure TKCLComponent.AddEventFilter(EventClass: TEventClass;
  FilterFn: TEventFilterFn);
begin
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
  Write(Event.ClassName);

  ClassInfo := Event.ClassInfo;
  ClassTypeData := GetTypeData(ClassInfo);
  PropCount := ClassTypeData^.PropCount;
  GetMem(PropList, SizeOf(TPropInfo) * PropCount);
  PropCount := GetPropList(ClassInfo, tkAny, PropList);
  for i := 0 to PropCount - 1 do begin
    PropInfo := PropList^[i];
    if i = 0 then
      Write(': ')
    else
      Write(', ');
    Write(PropList^[i]^.Name, '=');
    TypeData := GetTypeData(PropInfo^.PropType);
    case PropInfo^.PropType^.Kind of
      tkChar, tkWChar: begin
          c := Chr(GetOrdProp(Event, PropInfo));
	  if (c >= ' ') and (c < #255) and (c <> #127) then
            Write('''', c, '''')
	  else
	    Write('#', Ord(c));
        end;
      tkInteger: Write(GetOrdProp(Event, PropInfo));
      tkEnumeration: Write(GetOrdProp(Event, PropInfo));
      tkSet: Write(GetOrdProp(Event, PropInfo));
      tkClass: begin
          obj := PObject(Pointer(Event)+Longint(PropInfo^.GetProc))^;
	  //if obj.InheritsFrom(TComponent) and (Length(TComponent(obj).Name) > 0) then
	  //  Write(TComponent(obj).Name)
	  if obj.InheritsFrom(TComponent) then begin
	    s := '';
	    while Assigned(obj) do begin
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
end;

function TKCLComponent.CallEventFilters(Event: TEventObj): Boolean;
begin
  {$IFDEF DebugEvents}
  DumpEvent(Self, Event);
  {$ENDIF}
  Result := True;
end;


{
  $Log$
  Revision 1.2  2000/01/05 19:09:58  sg
  * Improved event dumping alot

  Revision 1.1.1.1  1999/12/30 21:33:09  sg
  Initial import

}
