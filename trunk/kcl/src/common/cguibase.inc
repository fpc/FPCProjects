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


// -------------------------------------------------------------------
//   Common TWidget method implementations
// -------------------------------------------------------------------

procedure TWidget.SetWidth(AWidth: LongInt);
begin
  SetSize(AWidth, FHeight);
end;

procedure TWidget.SetHeight(AHeight: LongInt);
begin
  SetSize(FWidth, AHeight);
end;

procedure TWidget.SetMinWidth(AWidth: LongInt);
begin
  FMinWidth := AWidth;
  OnMinMaxSizeChanged;
end;

procedure TWidget.SetMinHeight(AHeight: LongInt);
begin
  FMinHeight := AHeight;
  OnMinMaxSizeChanged;
end;

procedure TWidget.SetMaxWidth(AWidth: LongInt);
begin
  FMaxWidth := AWidth;
  OnMinMaxSizeChanged;
end;

procedure TWidget.SetMaxHeight(AHeight: LongInt);
begin
  FMaxHeight := AHeight;
  OnMinMaxSizeChanged;
end;

procedure TWidget.CheckSize(var AWidth, AHeight: LongInt);
begin
  if AWidth < MinWidth then AWidth := MinWidth;
  if AWidth > MaxWidth then AWidth := MaxWidth;
  if AHeight < MinHeight then AHeight := MinHeight;
  if AHeight > MaxHeight then AHeight := MaxHeight;
end;

procedure TWidget.RecalcLayout;
begin
  if (FCreationState <> wcsCreationDone) or (BlockRelayoutCounter > 0) then exit;
  Inc(BlockRelayoutCounter);
  OnRecalcLayout;
  Dec(BlockRelayoutCounter);
end;

procedure TWidget.ParentRecalcLayout;
begin
  if Assigned(FParent) and (FParent.InheritsFrom(TWidget)) then
    TWidget(FParent).RecalcLayout;
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


// -------------------------------------------------------
//   TGenericForm
// -------------------------------------------------------

function TGenericForm.CanClose: Boolean;
begin
  Result := True;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:21:29  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:07  sg
  Initial import

}
