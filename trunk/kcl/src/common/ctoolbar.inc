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
//   Common implementation of TToolButton and TToolBar methods
// ===================================================================

// -------------------------------------------------------------------
//   TToolButton
// -------------------------------------------------------------------

procedure TToolButton.SetParentComponent(AParent: TComponent);
begin
  ASSERT(AParent.InheritsFrom(TToolBar));
  FToolBar := TToolBar(AParent);
end;

procedure TToolButton.SetToolBar(AToolBar: TToolBar);
begin
  FToolBar := AToolBar;
end;

procedure TToolButton.SetCommand(ACommand: TCommand);
begin
  FCommand := ACommand;
end;

function TToolButton.HasParent: Boolean;
begin
  Result := Assigned(FToolBar);
end;

function TToolButton.GetParentComponent: TComponent;
begin
  Result := FToolBar;
end;


// -------------------------------------------------------------------
//   TToolBar
// -------------------------------------------------------------------

procedure TToolBar.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
begin
  WriteLn('### TToolBar.GetChildren not implemented yet');
end;

procedure TToolBar.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  // ### check if a child widget has been removed
end;

constructor TToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items := TList.Create;
end;

destructor TToolBar.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

function TToolBar.AddChildWidget(AWidget: TWidget): Boolean;
begin
  Items.Add(AWidget);
  Result := True;
end;

procedure TToolBar.RemoveChildWidget(AWidget: TWidget);
begin
  WriteLn('### TToolBar.RemoveChildWidget not implemented yet');
end;

function TToolBar.AddButton(AOwner: TComponent): TToolButton;
begin
  Result := TToolButton.Create(AOwner);
  Items.Add(Result);
end;

function TToolBar.CalcItemsWidth: Integer;
begin
  WriteLn('### TToolBar.CalcItemsWidth not implemented yet');
  Result := 0;
end;


{
  $Log$
  Revision 1.1  2000/01/24 00:13:57  sg
  * First version

}
