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
//   Common implementation of TTimer component
//
//	TTimer.Update is implemented differently for each target
// ===================================================================

procedure TTimer.SetEnabled(AEnabled: Boolean);
begin
  if AEnabled <> FEnabled then begin
    FEnabled := AEnabled;
    Update;
  end;
end;

procedure TTimer.SetInterval(AInterval: Cardinal);
begin
  if AInterval <> FInterval then begin
    FInterval := AInterval;
    if FEnabled then
      Update;
  end;
end;

procedure TTimer.Timer;
begin
  if Assigned(OnTimer) then
    OnTimer(Self);
end;

constructor TTimer.Create(AOwner: TComponent);
begin
  FEnabled := True;
  FInterval := 1000;
end;

destructor TTimer.Destroy;
begin
  FEnabled := False;
  Update;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:09  sg
  Initial revision

}
