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
//   Common implementation of methods of form classes
// ===================================================================

// -------------------------------------------------------------------
//   TCustomForm
// -------------------------------------------------------------------

procedure TCustomForm.SetDefaultWidth(AWidth: Integer);
begin
  if FDefaultWidth <> AWidth then begin
    FDefaultWidth := AWidth;
    ApplySize;
  end;
end;

procedure TCustomForm.SetDefaultHeight(AHeight: Integer);
begin
  if FDefaultHeight <> AHeight then begin
    FDefaultHeight := AHeight;
    ApplySize;
  end;
end;

procedure TCustomForm.SetPositionX(x: Integer);
begin
   SetPosition(x, GetPositionY);
end;

procedure TCustomForm.SetPositionY(y: Integer);
begin
  SetPosition(GetPositionX, y);
end;

procedure TCustomForm.DoRecalcLayout;
begin
  if Assigned(FContent) then begin
    MinW := FContent.MinW;
    MinH := FContent.MinH;
    MaxW := FContent.MaxW;
    MaxH := FContent.MaxH;
    DefW := FContent.DefW;
    DefH := FContent.DefH;
  end else begin
    MinW := 1;
    MinH := 1;
    MaxW := InfiniteSize;
    MaxH := InfiniteSize;
    DefW := 100;
    DefH := 100;
  end;

  Inc(MinW, FBorderWidth * 2);
  Inc(MinH, FBorderWidth * 2);
  Inc(MaxW, FBorderWidth * 2);
  Inc(MaxH, FBorderWidth * 2);
  Inc(DefW, FBorderWidth * 2);
  Inc(DefH, FBorderWidth * 2);

  if FWidth = 0 then
    if FDefaultWidth <= 0 then
      FWidth := DefW
    else
      FWidth := ClipMinMax(FDefaultWidth, MinW, MaxW);

  if FHeight = 0 then
    if FDefaultHeight <= 0 then
      FHeight := DefH
    else
      FHeight := ClipMinMax(FDefaultHeight, MinH, MaxH);

  ApplySize;	// Start top-down layouting
  ApplyMinMaxSize;
end;

function TCustomForm.IsRadioGroup: Boolean;
begin
  Result := True;
end;

function TCustomForm.CanClose: Boolean;
begin
  Result := True;
end;

procedure TCustomForm.SetDefaultSize(AWidth, AHeight: Integer);
begin
  if (FDefaultWidth <> AWidth) or (FDefaultHeight <> AHeight) then begin
    FDefaultWidth := AWidth;
    FDefaultHeight := AHeight;
    ApplySize;
  end;
end;


{
  $Log$
  Revision 1.2  2000/02/17 22:20:02  sg
  * DoRecalcLayout now consideres BorderWidth
  * Added IsRadioGroup

  Revision 1.1  2000/02/10 18:51:59  sg
  * First implementation; most methods have been moved from the GTK target
    implementation to this file.

}
