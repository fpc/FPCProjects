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
//   Common implementation of TxxxGrid widgets
// ===================================================================

resourcestring

  SGridIndexOutOfRange = 'Grid index out of range';


// -------------------------------------------------------------------
//   TCustomGrid
// -------------------------------------------------------------------

procedure TCustomGrid.SetColCount(AColCount: Integer);
var
  OldColCount: Integer;
begin
  if AColCount <> FColCount then begin
    OldColCount := FColCount;
    FColCount := AColCount;
    ColWidthsChanged;
    SizeChanged(OldColCount, FRowCount);
  end;
end;


procedure TCustomGrid.SetRowCount(ARowCount: Integer);
var
  OldRowCount: Integer;
begin
  if ARowCount <> FRowCount then begin
    OldRowCount := FRowCount;
    FRowCount := ARowCount;
    RowHeightsChanged;
    SizeChanged(FColCount, OldRowCount);
  end;
end;


procedure TCustomGrid.SetDefaultColWidth(AWidth: Integer);
var
  i: Integer;
begin
  if AWidth <> FDefaultColWidth then begin
    FDefaultColWidth := AWidth;
    for i := 0 to FColCount - 1 do
      FColWidths^[i] := AWidth;
    ColWidthsChanged;
  end;
end;


procedure TCustomGrid.SetDefaultRowHeight(AHeight: Integer);
var
  i: Integer;
begin
  if AHeight <> FDefaultRowHeight then begin
    FDefaultRowHeight := AHeight;
    for i := 0 to FRowCount - 1 do
      FRowHeights^[i] := AHeight;
    RowHeightsChanged;
  end;
end;


function TCustomGrid.GetColWidths(ACol: Integer): Integer;
begin
  if (ACol < 0) or (ACol >= FColCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  Result := FColWidths^[ACol];
end;


procedure TCustomGrid.SetColWidths(ACol, AWidth: Integer);
begin
  if (ACol < 0) or (ACol >= FColCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  FColWidths^[ACol] := AWidth;
end;


function TCustomGrid.GetRowHeights(ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  Result := FRowHeights^[ARow];
end;


procedure TCustomGrid.SetRowHeights(ARow, AHeight: Integer);
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    raise EInvalidGridOperation(SGridIndexOutOfRange);
  FRowHeights^[ARow] := AHeight;
end;


procedure TCustomGrid.PaintBoxPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
var
  x1, y1, x2, y2, x, y, xs, ys, ThisRowHeight: Integer;
  CellRect: TRect;
  state: TGridDrawState;
begin
  // WriteLn(Format('TCustomGrid.PaintBoxPaint(%d/%d - %d/%d)', [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]));
{  x1 := ARect.Left + HorzRange.CurValue;
  y1 := ARect.Top + VertRange.CurValue;
  x2 := ARect.Right + HorzRange.CurValue;
  y2 := ARect.Bottom + VertRange.CurValue;

  ys := 0;
  for y := 0 to RowCount - 1 do begin
    ThisRowHeight := RowHeights[y];
    if ys + ThisRowHeight >= y1 then begin
      xs := 0;
      for x := 0 to ColCount - 1 do begin
        if xs + ColWidths[x] >= x1 then begin
	  CellRect.Left := xs;
	  CellRect.Top := ys;
	  CellRect.Right := xs + ColWidths[x];
	  CellRect.Bottom := ys + ThisRowHeight;
	  state := [];
	  if (x < FixedCols) or (y < FixedRows) then
	    Include(state, gdFixed);
	  DrawCell(x, y, CellRect, state);
	end;
	Inc(xs, ColWidths[x]);
	if xs >= x2 then break;
      end;
    end;
    Inc(ys, ThisRowHeight);
    if ys >= y2 then break;
  end;}
end;


procedure TCustomGrid.ColWidthsChanged;
var
  i, sum: Integer;
begin
{  sum := 0;
  for i := 0 to ColCount - 1 do
    Inc(sum, ColWidths[i]);
  HorzRange.MaxValue := sum;}
end;


procedure TCustomGrid.RowHeightsChanged;
var
  i, sum: Integer;
begin
{  sum := 0;
  for i := 0 to RowCount - 1 do
    Inc(sum, RowHeights[i]);
  VertRange.MaxValue := sum;}
end;


procedure TCustomGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  // This dynamic method is only used for descendants of TCustomGrid
end;


constructor TCustomGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  FColCount := 5;
  FRowCount := 5;
  FFixedCols := 1;
  FFixedRows := 1;
  GetMem(FColWidths, 5 * SizeOf(Integer));
  GetMem(FRowHeights, 5 * SizeOf(Integer));

  for i := 0 to 4 do begin
    FColWidths^[i] := FDefaultColWidth;
    FRowHeights^[i] := FDefaultRowHeight;
  end;

  PaintBox := TPaintBox.Create(Self);
  PaintBox.Name := 'PaintBox';
//!!!:  PaintBox.OnPaint := @PaintBoxPaint;
//!!!:  Content := PaintBox;
end;


destructor TCustomGrid.Destroy;
begin
  FreeMem(FRowHeights);
  FreeMem(FColWidths);
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TDrawGrid
// -------------------------------------------------------------------

procedure TDrawGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
 AState: TGridDrawState);
{var
  canvas: TCanvas;}
begin
{  Canvas := TWidgetCanvas.Create(PaintBox);
  if gdFixed in AState then
    Canvas.DrawButton(Self, ARect, False, False, taCenter, '');
  Canvas.Free;
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState);}
end;


// -------------------------------------------------------------------
//   TStringGrid
// -------------------------------------------------------------------

function TStringGrid.GetCells(ACol, ARow: Integer): String;
begin
  if (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount) then
    Result := CellStrings^[ARow * ColCount + ACol]
  else
    Result := '';
end;


procedure TStringGrid.SetCells(ACol, ARow: Integer; const AValue: String);
begin
  if (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount) then
    CellStrings^[ARow * ColCount + ACol] := AValue;
end;


procedure TStringGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  ReallocMem(CellStrings, ColCount * RowCount * SizeOf(AnsiString));
end;


procedure TStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
 AState: TGridDrawState);
{var
  s: String;
  Canvas: TWidgetCanvas;}
begin
{  // WriteLn('TStringGrid.DrawCell(', ACol, ', ', ARow, ', ', Integer(AState), ');');
  s := Cells[ACol, ARow];
  if Length(s) > 0 then begin
    Canvas := TWidgetCanvas.Create(PaintBox);
    if gdFixed in AState then
      Canvas.DrawButton(Self, ARect, False, False, taCenter, '');
    Canvas.Text(ARect.Left + 2, ARect.Top + 2, s);
    Canvas.Free;
  end;
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState);}
end;


constructor TStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.SizeChanged(0, 0);
end;


destructor TStringGrid.Destroy;
var
  i: Integer;
begin
  for i := 0 to RowCount * ColCount - 1 do
    CellStrings^[i] := '';
  FreeMem(CellStrings);
  inherited Destroy;
end;


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
