{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
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
  WidgetStyle := WidgetStyle + [wsOpaque];
  FCanExpandWidth := True;
  FCanExpandHeight := True;
  ScrollingSupport := TScrollingSupport.Create(Self);
  ScrollingSupport.HorzScrollBar.OnScroll := @HorzScrollBarScroll;
  ScrollingSupport.VertScrollBar.OnScroll := @VertScrollBarScroll;

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
  x1, y1, x2, y2, x, y, Sum: Integer;
  CellRect: TRect;
  GridDrawState: TGridDrawState;
begin
  if not Canvas.IntersectClipRect(ScrollingSupport.ClientRect) then
    exit;

  with Canvas.GetClipRect do
  begin
    x1 := Left;
    y1 := Top;
    x2 := Right;
    y2 := Bottom;
  end;

  { Initialize these for the case that the cell drawing loop won't get
    executed at all: }
  CellRect.Left := 0;
  CellRect.Right := 0;

  // Draw the cells

  CellRect.Top := ScrollingSupport.ClientRect.Top;
  y := 0;
  while y < RowCount do
  begin
    CellRect.Bottom := CellRect.Top + RowHeights[y];
    if CellRect.Bottom > y1 then
    begin
      CellRect.Left := ScrollingSupport.ClientRect.Left;
      x := 0;
      while x < ColCount do
      begin
        CellRect.Right := CellRect.Left + ColWidths[x];
	if CellRect.Right > x1 then
	begin
	  GridDrawState := [];
	  if (x < FixedCols) or (y < FixedRows) then
	    Include(GridDrawState, gdFixed);

	  Canvas.SaveState;
	  if gdFixed in GridDrawState then
	  begin
	    with CellRect do
	      Style.DrawButtonFace(Canvas,
	        Rect(Left, Top, Right + 1, Bottom + 1), []);
	    Style.SetUIColor(Canvas, clBtnText);
	  end else
	  begin
	    Style.SetUIColor(Canvas, clWindow);
	    Canvas.FillRect(CellRect);
	    Style.SetUIColor(Canvas, clWindowText);
	  end;
	  DrawCell(Canvas, x, y, CellRect, GridDrawState);
	  Canvas.RestoreState;
	end;

	CellRect.Left := CellRect.Right + 1;
	if CellRect.Left >= x2 then
	  break;

        Inc(x);
        if x = FixedRows then
          Inc(x, ScrollingSupport.HorzScrollBar.Position);
      end;
    end;

    CellRect.Top := CellRect.Bottom + 1;
    if CellRect.Top >= y2 then
      break;

    Inc(y);
    if y = FixedRows then
      Inc(y, ScrollingSupport.VertScrollBar.Position);
  end;


  // Draw the grid lines

  Style.SetUIColor(Canvas, cl3DFace);

  y := 0;
  Sum := ScrollingSupport.ClientRect.Top;
  while y < RowCount do
  begin
    Inc(Sum, RowHeights[y]);
    if (y >= FixedRows) and (Sum >= y1) then
      Canvas.DrawLine(Point(ScrollingSupport.ClientRect.Left + FixedWidth, Sum),
        Point(CellRect.Right + 1, Sum));
    Inc(Sum);
    if Sum >= y2 then
      break;

    Inc(y);
    if y = FixedRows then
      Inc(y, ScrollingSupport.VertScrollBar.Position);
  end;

  x := 0;
  Sum := ScrollingSupport.ClientRect.Left;
  while x < ColCount do
  begin
    Inc(Sum, ColWidths[x]);
    if (x >= FixedCols) and (Sum >= x1) then
      Canvas.DrawLine(Point(Sum, ScrollingSupport.ClientRect.Top + FixedHeight),
        Point(Sum, CellRect.Bottom));
    Inc(Sum);
    if Sum >= x2 then
      break;

    Inc(x);
    if x = FixedCols then
      Inc(x, ScrollingSupport.HorzScrollBar.Position);
  end;


  // Clear the empty space right and below the grid itself

  Style.SetUIColor(Canvas, clWindow);
  if CellRect.Right + 1 < x2 then
    Canvas.FillRect(Rect(CellRect.Right + 1, y1, x2, y2));
  if CellRect.Bottom + 1 < y2 then
    Canvas.FillRect(Rect(x1, CellRect.Top, CellRect.Right + 1, y2));
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

procedure TCustomGrid.CalcSizes;
begin
  ScrollingSupport.CalcSizes;
end;

procedure TCustomGrid.Resized;
begin
  ScrollingSupport.Resized;
end;

procedure TCustomGrid.ColWidthsChanged;
var
  i: Integer;
begin
  FGridWidth := 0;
  for i := 0 to ColCount - 1 do
    Inc(FGridWidth, ColWidths[i] + 1);
  if FGridWidth > 0 then
    Dec(FGridWidth);

  FFixedWidth := 0;
  for i := 0 to FixedCols - 1 do
    Inc(FFixedWidth, ColWidths[i] + 1);
  if FFixedWidth > 0 then
    Dec(FFixedWidth);
end;

procedure TCustomGrid.RowHeightsChanged;
var
  i: Integer;
begin
  FGridHeight := 0;
  for i := 0 to RowCount - 1 do
    Inc(FGridHeight, RowHeights[i] + 1);
  if FGridHeight > 0 then
    Dec(FGridHeight);

  FFixedHeight := 0;
  for i := 0 to FixedRows - 1 do
    Inc(FFixedHeight, RowHeights[i] + 1);
  if FFixedHeight > 0 then
    Dec(FFixedHeight);
end;

procedure TCustomGrid.SizeChanged(OldColCount, OldRowCount: Integer);
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

    ScrollingSupport.HorzScrollBar.Max := ColCount -  FixedCols - 1;

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

    ScrollingSupport.VertScrollBar.Max := RowCount - FixedRows - 1;

    RowHeightsChanged;
    SizeChanged(FColCount, OldRowCount);
  end;
end;

procedure TCustomGrid.SetFixedCols(AFixedCols: Integer);
begin
  if AFixedCols <> FixedCols then
  begin
    FFixedCols := AFixedCols;
    ScrollingSupport.HorzScrollBar.Max := ColCount -  FixedCols - 1;
  end;
end;

procedure TCustomGrid.SetFixedRows(AFixedRows: Integer);
begin
  if AFixedRows <> FixedRows then
  begin
    FFixedRows := AFixedRows;
    ScrollingSupport.VertScrollBar.Max := RowCount - FixedRows - 1;
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

procedure TCustomGrid.HorzScrollBarScroll(Sender: TObject;
  var APosition: Integer);
var
  i, Delta: Integer;
  r: TRect;
begin
  Delta := 0;
  if APosition > ScrollingSupport.HorzScrollBar.Position then
    for i := ScrollingSupport.HorzScrollBar.Position to APosition - 1 do
      Dec(Delta, ColWidths[i + FixedCols] + 1)
  else
    for i := APosition to ScrollingSupport.HorzScrollBar.Position - 1 do
      Inc(Delta, ColWidths[i + FixedCols] + 1);

  // Scroll the horizontal fixed cells
  r := ScrollingSupport.ClientRect;
  Inc(r.Left, FixedWidth + 1);
  if r.Left <= r.Right then
  begin
    if Delta < 0 then		// Scrolling to the right side
      Dec(r.Left, Delta)
    else			// Scrolling to the left side
      Dec(r.Right, Delta);
    Scroll(r, Delta, 0);
  end;
end;

procedure TCustomGrid.VertScrollBarScroll(Sender: TObject;
  var APosition: Integer);
var
  i, Delta: Integer;
  r: TRect;
begin
  Delta := 0;
  if APosition > ScrollingSupport.VertScrollBar.Position then
    for i := ScrollingSupport.VertScrollBar.Position to APosition - 1 do
      Dec(Delta, RowHeights[i + FixedRows] + 1)
  else
    for i := APosition to ScrollingSupport.VertScrollBar.Position - 1 do
      Inc(Delta, RowHeights[i + FixedRows] + 1);

  // Scroll the grid body
  r := ScrollingSupport.ClientRect;
  Inc(r.Top, FixedHeight + 1);
  if r.Top <= r.Bottom then
  begin
    if Delta < 0 then		// Scrolling downwards
      Dec(r.Top, Delta)
    else			// Scrolling upwards
      Dec(r.Bottom, Delta);
    Scroll(r, 0, Delta);
  end;
end;


// -------------------------------------------------------------------
//   TDrawGrid
// -------------------------------------------------------------------

procedure TDrawGrid.DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
begin
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
    SetLength(Result, 0);
end;

procedure TStringGrid.SetCells(ACol, ARow: Integer; const AValue: String);
begin
  if (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount) then
    CellStrings^[ARow * ColCount + ACol] := AValue;
end;

procedure TStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
var
  Count: Integer;
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  ReallocMem(CellStrings, ColCount * RowCount * SizeOf(AnsiString));
  Count := ColCount * RowCount - OldColCount * OldRowCount;
  if Count > 0 then
    FillChar(CellStrings^[OldColCount * OldRowCount],
      Count * SizeOf(AnsiString), #0);
end;

procedure TStringGrid.DrawCell(ACanvas: TGfxCanvas; ACol, ARow: Integer;
  ARect: TRect;
 AState: TGridDrawState);
var
  s: String;
begin
  // WriteLn('TStringGrid.DrawCell(', ACol, ', ', ARow, ', ', Integer(AState), ');');
  s := Cells[ACol, ARow];
  if Length(s) > 0 then
    ACanvas.TextOut(ARect.TopLeft + Point(2, 2), s);
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
  Revision 1.6  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.5  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.4  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.3  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
