{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Grid widgets implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


resourcestring

  SGridIndexOutOfRange = 'Grid index out of range';

// ===================================================================
//   TCustomGrid
// ===================================================================

// public methods

constructor TCustomGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  ScrollingSupport := TScrollingSupport.Create(Self);

  FDefaultColWidth := 64;
  FDefaultRowHeight := 24;
  FColCount := 5;
  FRowCount := 5;
  FFixedCols := 1;
  FFixedRows := 1;
  GetMem(FColWidths, FColCount * SizeOf(Integer));
  GetMem(FRowHeights, FRowCount * SizeOf(Integer));

  for i := 0 to 4 do
  begin
    FColWidths^[i] := FDefaultColWidth;
    FRowHeights^[i] := FDefaultRowHeight;
  end;
  ColWidthsChanged;
  RowHeightsChanged;
end;

destructor TCustomGrid.Destroy;
begin
  FreeMem(FRowHeights);
  FreeMem(FColWidths);
  ScrollingSupport.Free;
  inherited Destroy;
end;


// protected methods

procedure TCustomGrid.Paint(Canvas: TGfxCanvas);
var
  x1, y1, x2, y2, x, y, xs, ys, ThisRowHeight: Integer;
  CellRect: TRect;
  state: TGridDrawState;
begin
{ !!!:  with Canvas.ClipRect do
  begin
    x1 := Left;
    y1 := Top;
    x2 := Right - 1;
    y2 := Bottom - 1;
  end;}
  with ScrollingSupport.ClientRect do
  begin
    x1 := Left;
    y1 := Top;
    x2 := Right - 1;
    y2 := Bottom - 1;
  end;
  WriteLn(Format('TCustomGrid.Paint(%d/%d - %d/%d)', [x1, y1, x2, y2]));

  Canvas.IntersectClipRect(ScrollingSupport.ClientRect);

  // Draw column grid

  Style.SetUIColor(Canvas, cl3DFace);
  xs := ColWidths[0];
  x := 1;
  while True do
  begin
    if xs >= x1 then
      Canvas.DrawLine(xs, 0, xs, GridHeight);
    if x < ColCount then
    begin
      Inc(xs, ColWidths[x]);
      if xs >= x2 then
        break;
      Inc(x);
    end else
      break;
  end;

  ys := RowHeights[0];
  y := 1;
  while True do
  begin
    if ys >= y1 then
      Canvas.DrawLine(0, ys, GridWidth, ys);
    if y < RowCount then
    begin
      Inc(ys, RowHeights[y]);
      if ys >= y2 then
	break;
      Inc(y);
    end else
      break;
  end;


  Style.SetUIColor(Canvas, clWindow);
  ys := 0;
  for y := 0 to RowCount - 1 do
  begin
    ThisRowHeight := RowHeights[y];
    if ys + ThisRowHeight > y1 then
    begin
      xs := 0;
      for x := 0 to ColCount - 1 do
      begin
        if xs + ColWidths[x] > x1 then
	begin
	  CellRect.Left := xs + 1;
	  CellRect.Top := ys + 1;
	  CellRect.Right := xs + ColWidths[x];
	  CellRect.Bottom := ys + ThisRowHeight;
	  Canvas.FillRect(CellRect);
	  state := [];
	  if (x < FixedCols) or (y < FixedRows) then
	    Include(state, gdFixed);
	  Canvas.SaveState;
	  Style.SetUIColor(Canvas, clWindowText);
	  DrawCell(Canvas, x, y, CellRect, state);
	  Canvas.RestoreState;
	end;
	Inc(xs, ColWidths[x]);
	if xs >= x2 then
	  break;
      end;
    end;
    Inc(ys, ThisRowHeight);
    if ys >= y2 then
      break;
  end;
end;

function TCustomGrid.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.ProcessEvent(Event) or
    inherited ProcessEvent(Event);
end;

function TCustomGrid.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := ScrollingSupport.DistributeEvent(Event) or
    inherited DistributeEvent(Event);
end;

procedure TCustomGrid.EvRecalcLayout;
begin
  ScrollingSupport.EvRecalcLayout;
end;

procedure TCustomGrid.ColWidthsChanged;
var
  i: Integer;
begin
  FGridWidth := 0;
  for i := 0 to ColCount - 1 do
    Inc(FGridWidth, ColWidths[i]);
  // HorzRange.MaxValue := FGridWidth;
end;

procedure TCustomGrid.RowHeightsChanged;
var
  i: Integer;
begin
  FGridHeight := 0;
  for i := 0 to RowCount - 1 do
    Inc(FGridHeight, RowHeights[i]);
  // VertRange.MaxValue := FGridHeight;
end;

procedure TCustomGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  // This dynamic method is only used for descendants of TCustomGrid
end;


// private methods

procedure TCustomGrid.SetColCount(AColCount: Integer);
var
  OldColCount, i: Integer;
begin
  if AColCount <> FColCount then
  begin
    OldColCount := FColCount;
    FColCount := AColCount;

    ReallocMem(FColWidths, FColCount * SizeOf(Integer));
    for i := OldColCount to FColCount - 1 do
      FColWidths^[i] := FDefaultColWidth;

    ColWidthsChanged;
    SizeChanged(OldColCount, FRowCount);
  end;
end;

procedure TCustomGrid.SetRowCount(ARowCount: Integer);
var
  OldRowCount, i: Integer;
begin
  if ARowCount <> FRowCount then
  begin
    OldRowCount := FRowCount;
    FRowCount := ARowCount;

    ReallocMem(FRowHeights, FRowCount * SizeOf(Integer));
    for i := OldRowCount to FRowCount - 1 do
      FRowHeights^[i] := FDefaultRowHeight;

    RowHeightsChanged;
    SizeChanged(FColCount, OldRowCount);
  end;
end;

procedure TCustomGrid.SetDefaultColWidth(AWidth: Integer);
var
  i: Integer;
begin
  if AWidth <> FDefaultColWidth then
  begin
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
  if AHeight <> FDefaultRowHeight then
  begin
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


// -------------------------------------------------------------------
//   TDrawGrid
// -------------------------------------------------------------------

procedure TDrawGrid.DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
begin
  if gdFixed in AState then
    Style.DrawButtonFace(ACanvas, ARect, []);
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACanvas, ACol, ARow, ARect, AState);
end;

function TDrawGrid.CellRect(ACol, ARow: Integer): TRect;
var
  i: Integer;
begin
  Result.Left := 0;
  for i := 0 to ACol - 1 do
    Inc(Result.Left, ColWidths[i]);
  Result.Right := Result.Left + ColWidths[ACol];

  Result.Top := 0;
  for i := 0 to ARow - 1 do
    Inc(Result.Top, RowHeights[i]);
  Result.Bottom := Result.Top + RowHeights[ARow];
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

procedure TStringGrid.DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
var
  s: String;
begin
  // WriteLn('TStringGrid.DrawCell(', ACol, ', ', ARow, ', ', Integer(AState), ');');
  if gdFixed in AState then
    Style.DrawButtonFace(ACanvas, ARect, []);
  s := Cells[ACol, ARow];
  if Length(s) > 0 then
  begin
    ACanvas.TextOut(ARect.Left + 2, ARect.Top + 2, s);
  end;
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACanvas, ACol, ARow, ARect, AState);
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
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
