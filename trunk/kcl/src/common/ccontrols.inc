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
//   Common implementation of TControl & friends methods
// ===================================================================

function Min(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;


function ClipMinMax(val, min, max: Integer): Integer;
begin
  if val < min then
    Result := min
  else if val > max then
    Result := max
  else
    Result := val;
end;


// -------------------------------------------------------------------
//   TControl
// -------------------------------------------------------------------

procedure TControl.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> FEnabled then
  begin
    FEnabled := AEnabled;
    CallEventFilters(TEnabledChangedEventObj.Create(Self));
  end;
end;


procedure TControl.SetHint(const AHint: String);
begin
  if AHint <> FHint then
  begin
    FHint := AHint;
    CallEventFilters(THintChangedEventObj.Create(Self));
  end;
end;


procedure TControl.SetText(const AText: String);
begin
  if AText <> FText then
  begin
    FText := AText;
    CallEventFilters(TTextChangedEventObj.Create(Self));
  end;
end;


procedure TControl.SetVisible(AVisible: Boolean);
begin
  if AVisible <> FVisible then
  begin
    FVisible := AVisible;
    if FCreationState >= wcsCreationInProgress then
      if AVisible then
        Show
      else
        Hide;
  end;
end;


procedure TControl.SetCanExpandWidth(allow: Boolean);
begin
  if FCanExpandWidth <> allow then
  begin
    FCanExpandWidth := allow;
    RecalcLayout;
  end;
end;


procedure TControl.SetCanExpandHeight(allow: Boolean);
begin
  if FCanExpandHeight <> allow then
  begin
    FCanExpandHeight := allow;
    RecalcLayout;
  end;
end;


procedure TControl.Click;
begin
  CallEventFilters(TClickEventObj.Create(Self));
end;


function TControl.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := True;
  if Event.ClassType = TClickEventObj then
    if Assigned(FOnClick) then
      FOnClick(Self);
end;


procedure TControl.SetParent(AParent: TWinControl);
begin
  if AParent <> FParent then
  begin
    if Assigned(FParent) then
      FParent.RemoveControl(Self);
    FParent := AParent;
    if Assigned(FParent) then
      FParent.InsertControl(Self);
  end;
end;


procedure TControl.DoRecalcLayout;
begin
end;


procedure TControl.BlockDrawing;
begin
  Inc(BlockDrawingCounter);
end;


procedure TControl.UnblockDrawing;
begin
  ASSERT(BlockDrawingCounter > 0, 'Invalid TControl.UnblockDrawing');
  Dec(BlockDrawingCounter);
end;


function TControl.IsRadioGroup: Boolean;
begin
  Result := False;
end;


function TControl.GetText: String;
begin
  Result := FText;
end;


constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AddEventFilter(TEventObj, @ProcessEvent);

  FVisible := True;
  FEnabled := True;
  FCanExpandWidth := True;
  FCanExpandHeight := True;
end;


destructor TControl.Destroy;
begin
  SetParent(nil);
  inherited Destroy;
end;


procedure TControl.Hide;
begin
  if Visible then
  begin
    FVisible := False;
    CallEventFilters(TVisibleChangedEventObj.Create(Self));
  end;
end;


procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;


procedure TControl.Show;
begin
  if not Visible then
  begin
    FVisible := True;
    CallEventFilters(TVisibleChangedEventObj.Create(Self));
  end;
end;


procedure TControl.RecalcLayout;
begin
end;


// -------------------------------------------------------------------
//   TWinControl
// -------------------------------------------------------------------

function TWinControl.GetControlCount: Integer;
begin
  Result := 0;
end;


constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


procedure TWinControl.HandleNeeded;
begin
  if FCreationState >= wcsCreationInProgress then exit;

  if Assigned(FParent) and (FParent.FCreationState < wcsCreationInProgress) then
  begin
    ASSERT(False);
    exit;
  end;

  FCreationState := wcsCreationInProgress;

  // Start the real control creation:
  OnFinishCreation;

  FCreationState := wcsCreationDone;

  if FVisible then
    Show;

  RecalcLayout;
end;


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
