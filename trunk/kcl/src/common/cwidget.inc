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
//   Common implementation of TWidget & friends methods
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
//   TWidget
// -------------------------------------------------------------------

function TWidget.GetWidgetCount: Integer;
begin
  Result := 0;
end;

procedure TWidget.SetSize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  ApplySize;
end;

procedure TWidget.RecalcLayout;
var
  SizeReq: TGtkRequisition;
begin
  if not Assigned(FHandle) then exit;
  MinW := 0;
  MinH := 0;
  MaxW := 0;
  MaxH := 0;
  DefW := 0;
  DefH := 0;

  DoRecalcLayout;

  FCreationState := wcsCreationDone;

  if MinW = 0 then MinW := 1;
  if MinH = 0 then MinH := 1;
  if DefW < MinW then DefW := MinW;
  if DefH < MinH then DefH := MinH;

  if (not FCanExpandWidth) or (MaxW < DefW) then
    MaxW := DefW;
  if (not FCanExpandHeight) or (MaxH < DefH) then
    MaxH := DefH;

  if Assigned(FParent) and (FParent.FCreationState = wcsCreationDone) then
    FParent.RecalcLayout;
end;

procedure TWidget.SetVisible(AVisible: Boolean);
begin
  if AVisible = FVisible then exit;
  FVisible := AVisible;
  if FCreationState >= wcsCreationInProgress then
    if AVisible then
      Show
    else
      Hide;
end;

procedure TWidget.SetCanExpandWidth(allow: Boolean);
begin
  if FCanExpandWidth <> allow then begin
    FCanExpandWidth := allow;
    RecalcLayout;
  end;
end;

procedure TWidget.SetCanExpandHeight(allow: Boolean);
begin
  if FCanExpandHeight <> allow then begin
    FCanExpandHeight := allow;
    RecalcLayout;
  end;
end;

procedure TWidget.FinishCreation;
begin
  if FCreationState = wcsCreationDone then exit;

  if Assigned(FParent) and (FParent.FCreationState < wcsCreationInProgress) then begin
    ASSERT(False);
    exit;
  end;

  FCreationState := wcsCreationInProgress;

  // Start the real widget creation:
  OnFinishCreation;

  if FVisible then
    Show;

  RecalcLayout;

  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TWidget.BlockDrawing;
begin
  Inc(BlockDrawingCounter);
end;

procedure TWidget.UnblockDrawing;
begin
  ASSERT(BlockDrawingCounter > 0, 'Invalid TWidget.UnblockDrawing');
  Dec(BlockDrawingCounter);
end;

function TWidget.AddChildWidget(AWidget: TWidget): Boolean;
begin
  Result := False;
end;

function TWidget.IsRadioGroup: Boolean;
begin
  Result := False;
end;



{
  $Log$
  Revision 1.5  2000/02/22 14:37:13  sg
  * Added stuff for CanExpandWidth and CanExpandHeight properties

  Revision 1.4  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.3  2000/02/17 22:21:07  sg
  * Added methods "GetWidgetCount" and "IsRadioGroup"

  Revision 1.2  2000/02/10 18:31:51  sg
  * Added generic size calculation methods

  Revision 1.1  2000/01/24 00:13:57  sg
  * First version

}
