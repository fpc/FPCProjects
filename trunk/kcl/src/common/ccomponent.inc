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
//   Implementation of TKCLComponent
// ===================================================================

procedure TKCLComponent.AddEventFilter(EventClass: TEventClass;
  FilterFn: TEventFilterFn);
begin
end;

procedure DumpEvent(Dest: TObject; Event: TEventObj);
{var
  ClassInfo: PTypeInfo;
  ClassTypeData: PTypeData;
  PropList: PPropList;
  PropCount, i: Integer;}
begin
  WriteLn('Event: ', Event.ClassName, ' for ', Dest.ClassName);

{  ClassInfo := Event.ClassInfo;
WriteLn('ClassInfo: ', Integer(ClassInfo));
  ClassTypeData := GetTypeData(ClassInfo);
WriteLn('ClassTypeData: ', Integer(ClassInfo));
  PropCount := ClassTypeData^.PropCount;
WriteLn('PropCount: ', PropCount);
  GetMem(PropList, SizeOf(TPropInfo) * PropCount);
WriteLn('GetMem OK');
  PropCount := GetPropList(ClassInfo, tkAny, PropList);
WriteLn('GetPropList OK');
  for i := 0 to PropCount - 1 do begin
    WriteLn(PropList^[i]^.Name);
  end;}
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
  Revision 1.1  1999/12/30 21:33:09  sg
  Initial revision

}
