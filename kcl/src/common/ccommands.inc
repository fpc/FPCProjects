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
//   Common implementation of TCommand and TCommandList
// ===================================================================

// -------------------------------------------------------------------
//   TCommand
// -------------------------------------------------------------------

procedure TCommand.Changed(ChangeType: TCmdChangeType);
begin
  CallEventFilters(TCmdChangeEventObj.Create(Self, ChangeType));
end;

procedure TCommand.SetCommandList(ACommandList: TCommandList);
begin
  FCommandList := ACommandList;
end;

procedure TCommand.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> FEnabled then begin
    FEnabled := AEnabled;
    Changed(cctEnabled);
  end;
end;

procedure TCommand.SetParentComponent(AParent: TComponent);
begin
  ASSERT(AParent.InheritsFrom(TCommandList));
  FCommandList := TCommandList(AParent);
end;

constructor TCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FEnabled := True;
end;

function TCommand.HasParent: Boolean;
begin
  Result := Assigned(FCommandList);
end;

function TCommand.GetParentComponent: TComponent;
begin
  Result := FCommandList;
end;

procedure TCommand.Execute(Sender: TObject);
begin
  if CallEventFilters(TCmdExecuteEventObj.Create(Self)) then
    if Assigned(FOnExecute) then
      FOnExecute(Sender);
end;


// -------------------------------------------------------------------
//   TCommandList
// -------------------------------------------------------------------

function TCommandList.GetCommandCount: Integer;
begin
  Result := List.Count;
end;

function TCommandList.GetCommands(Index: Integer): TCommand;
begin
  Result := TCommand(List.Items[Index]);
end;

procedure TCommandList.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
begin
  WriteLn('### TCommandList.GetChildren not implemented yet');
end;

procedure TCommandList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  // ### check if a command in this list is being removed
end;

procedure TCommandList.SetChildOrder(AComponent: TComponent; AOrder: Integer);
begin
  WriteLn('### TCommandList.SetChildOrder not implemented yet');
end;

constructor TCommandList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  List := TList.Create;
end;

destructor TCommandList.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

function TCommandList.Add(AOwner: TComponent): TCommand;
begin
  Result := TCommand.Create(AOwner);
  Result.FCommandList := Self;
  List.Add(Result);
end;

function TCommandList.Add(AOwner: TComponent; const AComponentName, AText,
  AShortCut: String; AImageIndex: Integer; ANotify: TNotifyEvent): TCommand;
begin
  Result := Add(AOwner);
  Result.Name := AComponentName;
  Result.Text := AText;
  Result.ShortCut := AShortCut;
  Result.ImageIndex := AImageIndex;
  Result.OnExecute := ANotify;
end;


{
  $Log$
  Revision 1.1  2000/01/24 00:13:57  sg
  * First version

}
