{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Bin widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TBinWidget
// ===================================================================

function TBinWidget.ContainsChild(AChild: TWidget): Boolean;
begin
  Result := Assigned(AChild) and (FChild = AChild);
end;

procedure TBinWidget.InsertChild(AChild: TWidget);
begin
  if FChild <> AChild then
  begin
    if Assigned(FChild) then
      FChild.Parent := nil;
    FChild := AChild;
    FChild.Parent := Self;
  end;
end;

procedure TBinWidget.RemoveChild(AChild: TWidget);
begin
  if FChild = AChild then
  begin
    FChild := nil;
    AChild.Parent := nil;
  end;
end;

function TBinWidget.GetChildCount: Integer;
begin
  Result := Ord(Assigned(Child));
end;

function TBinWidget.GetChild(Index: Integer): TWidget;
begin
  if (Index = 0) and Assigned(Child) then
    Result := Child
  else
    TList.Error(SListIndexError, Index);
end;

procedure TBinWidget.EvRecalcLayout;
begin
  LAYOUTTRACE('TBinWidget.EvRecalcLayout for %s:%s', [Name, ClassName]);
  if Assigned(Child) then
  begin
    MinW := Child.MinW;
    MinH := Child.MinH;
    DefW := Child.DefW;
    DefH := Child.DefH;
    MaxW := Child.MaxW;
    MaxH := Child.MaxH;
  end;
end;

procedure TBinWidget.SetChild(AChild: TWidget);
begin
  InsertChild(AChild);
end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
