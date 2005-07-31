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
    RecalcLayout;
  end;
end;

procedure TCustomForm.SetDefaultHeight(AHeight: Integer);
begin
  if FDefaultHeight <> AHeight then begin
    FDefaultHeight := AHeight;
    RecalcLayout;
  end;
end;

procedure TCustomForm.SetModalResult(AResult: TModalResult);
begin
  FModalResult := AResult;
  Close;
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
    else begin
      FWidth := ClipMinMax(FDefaultWidth, MinW, MaxW);
      DefW := FWidth;
    end;

  if FHeight = 0 then
    if FDefaultHeight <= 0 then
      FHeight := DefH
    else begin
      FHeight := ClipMinMax(FDefaultHeight, MinH, MaxH);
      DefH := FHeight;
    end;

  if (LastMinW <> MinW) or (LastMinH <> MinH) or (LastMaxW <> MaxW) or (LastMaxH <> MaxH) then begin
    LastMinW := MinW;
    LastMinH := MinH;
    LastMaxW := MaxW;
    LastMaxH := MaxH;
    ApplyMinMaxSize;
  end;
end;

function TCustomForm.IsRadioGroup: Boolean;
begin
  Result := True;
end;

function TCustomForm.CloseQuery: Boolean;
begin
  Result := True;
end;

procedure TCustomForm.SetDefaultSize(AWidth, AHeight: Integer);
begin
  if (FDefaultWidth <> AWidth) or (FDefaultHeight <> AHeight) then begin
    FDefaultWidth := AWidth;
    FDefaultHeight := AHeight;
    RecalcLayout;
  end;
end;


{
  $Log$
  Revision 1.6  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.5  2000/02/25 11:40:28  michael
  + Added Checked reader to TRadioButton
  + Started support for ShowModal

  Revision 1.4  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.3  2000/02/22 14:35:06  sg
  * In DoRecalcLayout: "ApplySize" and "ApplyMinMaxSize" are only called
    after the creation of the form

  Revision 1.2  2000/02/17 22:20:02  sg
  * DoRecalcLayout now consideres BorderWidth
  * Added IsRadioGroup

  Revision 1.1  2000/02/10 18:51:59  sg
  * First implementation; most methods have been moved from the GTK target
    implementation to this file.

}
