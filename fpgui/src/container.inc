{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Container widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TContainerWidget
// ===================================================================

// Public methods

function TContainerWidget.GetChildAt(APoint: TPoint): TWidget;
var
  i: Integer;
begin
  for i := 0 to ChildCount - 1 do
  begin
    Result := Children[i];
    if PtInRect(Result.BoundsRect, APoint) then
      exit;
  end;
  Result := nil;
end;


// Protected methods

function TContainerWidget.DistributeEvent(Event: TEventObj): Boolean;
var
  i: Integer;
begin
  // Propagate the event to all children
  for i := 0 to ChildCount - 1 do
    if Event.SendToChild(Children[i]) then
    begin
      Result := True;
      exit;
    end;
  // The event hasn't been processed by any child:
  Result := inherited DistributeEvent(Event);
end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
