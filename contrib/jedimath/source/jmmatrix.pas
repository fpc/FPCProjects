{******************************************************************************}
{                                                                              }
{ JmMatrix.pas for Jedi Math Alpha 1.02                                        }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The Original Code is JmMatrix.pas.                                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains object-oriented matrix representation and manipulation.   }
{ This unit contains                                                           }
{                                                                              }
{ Unit owner:  Patrick Van Laake                                               }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

unit JmMatrix;
interface

{$i jedimath.inc}

uses
  SysUtils, Classes, JmTypes;
type
  EJmMatrix = class(Exception);

  TJmMatrixBounds = 1..MaxInt div 8;
  TJmIterBounds   = 0..30;

  TJmFloatArray = array[1..MaxInt div 8] of TJmFloat; // dynamically allocated
  PJmFloatArray = ^TJmFloatArray;

  TJmMatrixBase = class
  private
    FCanConvert: boolean;
  protected
    function GetRows: TJmMatrixBounds; virtual; abstract;
    function GetColumns: TJmMatrixBounds; virtual; abstract;
  public
    constructor Create;
    constructor CreateFromStream(strm: TStream); virtual; abstract;
    destructor Destroy; override;

    procedure LoadFromStream(var strm: TStream); virtual; abstract;
    procedure SaveToStream(var strm: TStream); virtual; abstract;

    property CanConvert: boolean read FCanConvert;
    property Rows: TJmMatrixBounds read GetRows;
    property Columns: TJmMatrixBounds read GetColumns;
  end;

  TJmVector = class; // forward reference

  TJmMatrix = class(TJmMatrixBase)
  private
    FCols: TJmMatrixBounds;
    FData: PJmFloatArray;
    function GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
    procedure SetValueZeroBased(R, C: TJmMatrixBounds;
      const Value: TJmFloat);
  protected
    FRows: TJmMatrixBounds;
    procedure CheckBounds(const r, c : TJmMatrixBounds);
    procedure DataChanged; virtual;

    function  GetRows: TJmMatrixBounds; override;
    function  GetColumns: TJmMatrixBounds; override;
    function  GetValue(r, c: TJmMatrixBounds): TJmFloat;
    procedure SetValue(r, c: TJmMatrixBounds; v: TJmFloat);
    function  GetRow(r: TJmMatrixBounds): PJmFloatArray;

    procedure SwapRows(r1, r2: TJmMatrixBounds);

    function  MultiplyMatrix(b: TJmMatrix): TJmMatrix; virtual;
    function  MultiplyVector(b: TJmVector): TJmVector; virtual;

    property  Data: PJmFloatArray read FData;
    property  Row[R: TJmMatrixBounds]: PJmFloatArray read GetRow;
  public
    constructor Create(r, c: TJmMatrixBounds);
    constructor Copy(m: TJmMatrix);
    constructor CreateFromStream(strm: TStream); override;
    destructor  Destroy; override;

    procedure LoadFromStream(var strm: TStream); override;
    procedure SaveToStream(var strm: TStream); override;

    function  ColumnAsVector(c: TJmMatrixBounds): TJmVector;
    function  Multiply(b: TJmMatrixBase): TJmMatrixBase;
    function  Add(b: TJmMatrix): TJmMatrix;
    procedure MultiplyByScalar(v: TJmFloat);
    function  Transpose: TJmMatrix;

    property  Rows: TJmMatrixBounds read FRows;
    property  Columns: TJmMatrixBounds read FCols;
    property  Cell[R, C: TJmMatrixBounds]: TJmFloat read GetValue write SetValue; default;
    property  CellZeroBased[R, C: TJmMatrixBounds]: TJmFloat read GetValueZeroBased write SetValueZeroBased;
  end;

  TJmMatrixSquare = class(TJmMatrix)
  private
    FLU: TJmMatrixSquare;
  protected
    procedure Decompose;

    procedure DataChanged; override;

    function  InverseMultiplyMatrix(b: TJmMatrix): TJmMatrix;
    function  InverseMultiplyVector(b: TJmVector): TJmVector;
  public
    constructor Create(d: TJmMatrixBounds);
    constructor Identity(d: TJmMatrixBounds);
    destructor  Destroy; override;

    function  Inverse: TJmMatrixSquare;
    procedure GaussJordan(p: TJmMatrix);
    function  Determinant: TJmFloat;
    function  SolveVector(b: TJmVector; iter: TJmIterBounds = 0): TJmVector;
    function  InverseMultiply(b: TJmMatrixBase): TJmMatrixBase;

    property  Dimension: TJmMatrixBounds read FRows;
  end;

  TJmMatrix2x2 = class(TJmMatrixBase)
  private
    FData: array[1..2,1..2] of TJmFloat;
    function GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
    procedure SetValueZeroBased(R, C: TJmMatrixBounds;
      const Value: TJmFloat);
  protected
    function  GetRows: TJmMatrixBounds; override;
    function  GetColumns: TJmMatrixBounds; override;
    function  GetValue(r, c: TJmMatrixBounds): TJmFloat;
    procedure SetValue(r, c: TJmMatrixBounds; v: TJmFloat);

    function  MultiplyMatrix2x2(b: TJmMatrix2x2): TJmMatrix2x2;
    function  MultiplyMatrix(b: TJmMatrix): TJmMatrix;
    function  MultiplyVector(b: TJmVector): TJmVector;
  public
    constructor Create;
    constructor CreateFromStream(strm: TStream); override;
    constructor Identity;
    constructor Copy(m : TJmMatrix2x2);

    procedure LoadFromStream(var strm: TStream); override;
    procedure SaveToStream(var strm: TStream); override;

    function  Multiply(b: TJmMatrixBase): TJmMatrixBase;
    function  Add(b: TJmMatrix2x2): TJmMatrix2x2;
    procedure MultiplyByScalar(v: TJmFloat);
    function  Transpose: TJmMatrix2x2;

    function  Inverse: TJmMatrix2x2;
    function  Determinant: TJmFloat;
    property  Cell[R, C: TJmMatrixBounds]: TJmFloat read GetValue write SetValue; default;
    property  CellZeroBased[R, C: TJmMatrixBounds]: TJmFloat read GetValueZeroBased write SetValueZeroBased;
  end;

  TJmMatrix3x3 = class(TJmMatrixBase)
  private
    FData: array[1..3,1..3] of TJmFloat;
    function GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
    procedure SetValueZeroBased(R, C: TJmMatrixBounds;
      const Value: TJmFloat);
  protected
    function  GetRows: TJmMatrixBounds; override;
    function  GetColumns: TJmMatrixBounds; override;
    function  GetValue(r, c: TJmMatrixBounds): TJmFloat;
    procedure SetValue(r, c: TJmMatrixBounds; v: TJmFloat);

    function  MultiplyMatrix3x3(b: TJmMatrix3x3): TJmMatrix3x3;
    function  MultiplyMatrix(b: TJmMatrix): TJmMatrix;
    function  MultiplyVector(b: TJmVector): TJmVector;
  public
    constructor Create;
    constructor CreateFromStream(strm: TStream); override;
    constructor Identity;
    constructor Copy(m : TJmMatrix3x3);

    procedure LoadFromStream(var strm: TStream); override;
    procedure SaveToStream(var strm: TStream); override;

    function  Multiply(b: TJmMatrixBase): TJmMatrixBase;
    function  Add(b: TJmMatrix3x3): TJmMatrix3x3;
    procedure MultiplyByScalar(v: TJmFloat);
    function  Transpose: TJmMatrix3x3;

    function  Inverse: TJmMatrix3x3;
    function  Determinant: TJmFloat;
    property  Cell[R, C: TJmMatrixBounds]: TJmFloat read GetValue write SetValue; default;
    property  CellZeroBased[R, C: TJmMatrixBounds]: TJmFloat read GetValueZeroBased write SetValueZeroBased;
  end;

  TJmVector = class(TJmMatrixBase)
  private
    FN: TJmMatrixBounds;
    FData: PJmFloatArray;
    function GetValueZeroBased(n: TJmMatrixBounds): TJmFloat;
    procedure SetValueZeroBased(n: TJmMatrixBounds; const Value: TJmFloat);
  protected
    procedure Redim(n: TJmMatrixBounds);
    function GetRows: TJmMatrixBounds; override;
    function GetColumns: TJmMatrixBounds; override;
    function GetValue(n: TJmMatrixBounds): TJmFloat;
    procedure SetValue(n: TJmMatrixBounds; v: TJmFloat);

    function  MultiplyVector(b: TJmVector): TJmMatrix;
    function  Multiply2x2(b: TJmMatrix2x2): TJmVector;
    function  MultiplyAsRow(b: TJmMatrix): TJmVector;
  public
    constructor Create(n: TJmMatrixBounds);
    constructor CreateFromStream(strm: TStream); override;
    constructor Copy(v: TJmVector);
    destructor Destroy; override;

    procedure LoadFromStream(var strm: TStream); override;
    procedure SaveToStream(var strm: TStream); override;

    function  DotProduct(v: TJmVector): TJmFloat;
    function  Multiply(b: TJmMatrixBase): TJmMatrixBase;

    property Rows: TJmMatrixBounds read FN write Redim;
    property Cell[n: TJmMatrixBounds]: TJmFloat read GetValue write SetValue; default;
    property CellZeroBased[n: TJmMatrixBounds]: TJmFloat read GetValueZeroBased write SetValueZeroBased; 
    property Row: PJmFloatArray read FData;
  end;

  function CreateMatrix(const r, c: TJmMatrixBounds; strm: TMemoryStream = nil): TJmMatrixBase;
  function CreateIdentityMatrix(const d: TJmMatrixBounds): TJmMatrixBase;

///////////////////////////////////////////////////////////////////////////
implementation
const
  AlmostZero: TJmFloat = 1.0e-320;

type
  TIntegerArray = array [1..10] of integer;  // dynamically allocated
  PIntegerArray = ^TIntegerArray;

  TJmMatrixDecompose = class(TJmMatrixSquare)
  protected
    FInterchanges: integer;
    FPermutation : PIntegerArray;

    function Determinant: TJmFloat;
    procedure Solve(b: TJmVector);
    procedure Improve(m: TJmMatrixSquare; b, res: TJmVector; iter: TJmIterBounds);
  public
    constructor Create(m: TJmMatrixSquare);
    destructor Destroy; override;

    property Interchanges: integer read FInterchanges;
  end;

///////////////////////////////////////////////////////////////////////////
// TJmMatrixBase
constructor TJmMatrixBase.Create;
begin
  FCanConvert := false;
end;

destructor TJmMatrixBase.Destroy;
begin
  inherited;
end;

////////////////////////////////////////////////////////////////////////////
// TJmMatrix
constructor TJmMatrix.Create(r, c: TJmMatrixBounds);
begin
  inherited Create;
  FCols := c;
  FRows := r;
  FData := AllocMem(r * c * SizeOf(TJmFloat));
end;

constructor TJmMatrix.CreateFromStream(strm: TStream);
begin
  strm.Read(FRows, SizeOf(FRows));
  strm.Read(FCols, SizeOf(FCols));
  GetMem(FData, FRows * FCols * SizeOf(TJmFloat));
  strm.Read(FData^, FRows * FCols * SizeOf(TJmFloat));
end;

constructor TJmMatrix.Copy(m: TJmMatrix);
var dataSize: integer;
begin
  inherited Create;
  FCols := m.FCols;
  FRows := m.FRows;
  dataSize := FRows * FCols * SizeOf(TJmFloat);
  GetMem(FData, dataSize);
  Move(m.Data^, FData^, dataSize);
end;

destructor TJmMatrix.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

// LoadFromStream loads the data for the matrix from the strm parameter. The
// size of the matrix should be implicit in the stream data and no checks are
// performed.
procedure TJmMatrix.LoadFromStream(var strm: TStream);
begin
  strm.Read(FData^, FRows * FCols * SizeOf(TJmFloat));
end;

procedure TJmMatrix.SaveToStream(var strm: TStream);
begin
  strm.Write(FRows, SizeOf(FRows));
  strm.Write(FCols, SizeOf(FCols));
  strm.Write(FData^, FRows * FCols * SizeOf(TJmFloat));
end;

procedure TJmMatrix.DataChanged;
begin
  // Nothing to do here
end;

function TJmMatrix.GetRows: TJmMatrixBounds;
begin
  Result := FRows;
end;

function TJmMatrix.GetColumns: TJmMatrixBounds;
begin
  Result := FCols;
end;

function TJmMatrix.GetValue(r, c: TJmMatrixBounds): TJmFloat;
begin
  CheckBounds(r, c);
  Result := FData^[(r-1)*FCols+c];
end;

procedure TJmMatrix.SetValue(r, c: TJmMatrixBounds; v: TJmFloat);
begin
  CheckBounds(r, c);
  FData^[(r-1)*FCols+c] := v;
  DataChanged;
end;

function TJmMatrix.GetRow(r: TJmMatrixBounds): PJmFloatArray;
begin
  // no bounds checking, protected method
  Result := @FData^[(r-1)*FCols+1];
end;

procedure TJmMatrix.SwapRows(r1, r2: TJmMatrixBounds);
var tmpRow: PJmFloatArray;
    dataSize: integer;
begin
  dataSize := FCols * SizeOf(TJmFloat);
  GetMem(tmpRow, dataSize);
  Move(FData^[(r1-1)*FCols+1], tmpRow^, dataSize);
  Move(FData^[(r2-1)*FCols+1], FData^[(r1-1)*FCols+1], dataSize);
  Move(tmpRow^, FData^[(r2-1)*FCols+1], dataSize);
  FreeMem(tmpRow);
//  DataChanged; // ???: Should this be done. Might mess up LU decomposition
end;

function TJmMatrix.ColumnAsVector(c: TJmMatrixBounds): TJmVector;
var r: integer;
    row: PJmFloatArray;
begin
  Result := TJmVector.Create(FRows);
  row := Result.Row;
  for r := 1 to FRows do
    row^[r] := FData^[(r-1)*FCols+c];
end;

function TJmMatrix.MultiplyMatrix(b: TJmMatrix): TJmMatrix;
var r, c, k, bCols: integer;
    rRow: PJmFloatArray;
    sum: TJmFloat;
begin
  bCols := b.Columns;
  if FRows = bCols then Result := TJmMatrixSquare.Create(FRows)
  else Result := TJmMatrix.Create(FRows, bCols);
  for r := 1 to FRows do begin
    rRow := Result.Row[r];
    for k := 1 to bCols do begin
      sum := 0.0;
      for c := 1 to FCols do
        sum := sum + Cell[r, c] * b.Cell[c, k];
      rRow^[k] := sum;
    end;
  end;
end;

function TJmMatrix.MultiplyVector(b: TJmVector): TJmVector;
var bRow, rRow, selfRow: PJmFloatArray;
    r, c: integer;
    sum: TJmFloat;
begin
  Result := TJmVector.Create(FRows);
  bRow := b.Row;
  rRow := Result.Row;
  selfRow := FData;
  for r := 1 to FRows do begin
    sum := 0.0;
    for c := 1 to FCols do
      sum := sum + selfRow^[c] * bRow^[c];
    rRow^[r] := sum;
    Inc(selfRow, FCols);
  end;
end;

// self * b = result
// Result is returned as either a TJmMatrix, TJmMatrixSquare or TJmVector
function TJmMatrix.Multiply(b: TJmMatrixBase): TJmMatrixBase;
begin
  if b.Rows <> FCols then
    raise EJmMatrix.Create('Incompatible matrix sizes');
  if b is TJmMatrix then
    Result := MultiplyMatrix(b as TJmMatrix)
  else if b is TJmVector then
    Result := MultiplyVector(b as TJmVector)
  else Result := nil;
end;

function TJmMatrix.Add(b: TJmMatrix): TJmMatrix;
var r: integer;
    selfData, bData, rData: PJmFloatArray;
begin
  if (b.Rows <> FRows) or (b.Columns <> FCols) then
    raise EJmMatrix.Create('Cannot add matrices of different sizes');
  if self is TJmMatrixSquare then
    Result := TJmMatrixSquare.Create(FRows)
  else Result := TJmMatrix.Create(FRows, FCols);
  selfData := FData;
  bData := b.Data;
  rData := Result.Data;
  for r := 1 to FRows * FCols do
    rData^[r] := selfData^[r] + bData^[r];
end;

procedure TJmMatrix.MultiplyByScalar(v: TJmFloat);
var r: integer;
begin
  for r := 1 to FRows * FCols do begin
    FData^[r] := FData^[r] * v;
  end;
  DataChanged;
end;

function TJmMatrix.Transpose: TJmMatrix;
var r, c: integer;
begin
  if FCols = FRows then
    Result := TJmMatrixSquare.Create(FCols)
  else Result := TJmMatrix.Create(FCols, FRows);

  for r := 1 to FRows do
    for c := 1 to FCols do
      Result.Cell[c,r] := FData^[(r-1)*FCols+c];

  DataChanged;
end;

////////////////////////////////////////////////////////////////////////////
// TJmVector
constructor TJmVector.Create(n: TJmMatrixBounds);
begin
  inherited Create;
  FN := n;
  FData := AllocMem(n * SizeOf(TJmFloat));
end;

constructor TJmVector.CreateFromStream(strm: TStream);
var
  t: integer;
begin
  strm.Read(FN, SizeOf(FN));
  strm.Read(t, SizeOf(t));
  if t <> 1 then
    raise EJmMatrix.Create('Invalid dimension in stream');
  GetMem(FData, FN * SizeOf(TJmFloat));
  strm.Read(FData^, FN * SizeOf(TJmFloat));
end;

constructor TJmVector.Copy(v: TJmVector);
begin
  Create(v.Rows);
  Move(v.Row^, FData^, FN * SizeOf(TJmFloat));
end;

destructor TJmVector.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

// LoadFromStream loads the data for the matrix from the strm parameter. The
// size of the matrix should be implicit in the stream data and no checks are
// performed.
procedure TJmVector.LoadFromStream(var strm: TStream);
begin
  strm.Read(FData^, FN * SizeOf(TJmFloat));
end;

procedure TJmVector.SaveToStream(var strm: TStream);
var
  d: cardinal;
begin
  d := 1;
  strm.Write(FN, SizeOf(FN));
  strm.Write(d, SizeOf(d));
  strm.Write(FData^, FN * SizeOf(TJmFloat));
end;

function TJmVector.GetRows: TJmMatrixBounds;
begin
  Result := FN;
end;

function TJmVector.GetColumns: TJmMatrixBounds;
begin
  Result := FN;
end;

procedure TJmVector.Redim(n: TJmMatrixBounds);
begin
  ReallocMem(FData, n * SizeOf(TJmFloat));
  FN := n;
end;

function TJmVector.GetValue(n: TJmMatrixBounds): TJmFloat;
begin
  if n > FN then
    raise EJmMatrix.Create('Index out of bounds.');
  Result := FData^[n];
end;

procedure TJmVector.SetValue(n: TJmMatrixBounds; v: TJmFloat);
begin
  if n > FN then
    raise EJmMatrix.Create('Index out of bounds.');
  FData^[n] := v;
end;

function TJmVector.DotProduct(v: TJmVector): TJmFloat;
var n: TJmMatrixBounds;
begin
  if v.FN <> FN then
    raise EJmMatrix.Create('Incompatible vector sizes');
  Result := 0.0;
  for n := 1 to FN do
    Result := Result + FData^[n] * v.FData^[n];
end;

function TJmVector.MultiplyVector(b: TJmVector): TJmMatrix;
var r, c: integer;
    rRow, bRow : PJmFloatArray;
begin
  if b.Columns = FN then
    Result := TJmMatrixSquare.Create(FN)
  else Result := TJmMatrix.Create(FN, b.Columns);
  bRow := b.FData;
  for r := 1 to FN do begin
    rRow := Result.GetRow(r);
    for c := 1 to b.Columns do
      rRow^[c] := FData^[r] * bRow^[c];
  end;
end;

function TJmVector.Multiply2x2(b: TJmMatrix2x2): TJmVector;
begin
  Result := TJmVector.Create(2);
  Result.FData^[1] := FData^[1] * b.FData[1,1] + FData^[2] * b.FData[2,1];
  Result.FData^[2] := FData^[1] * b.FData[1,2] + FData^[2] * b.FData[2,2];
end;

// self is a row vector here. b is a TJmMatrix of dimension [FN, b.Columns]
function TJmVector.MultiplyAsRow(b: TJmMatrix): TJmVector;
var r, c: integer;
    rRow : PJmFloatArray;
    bData : ^TJmFloat;
begin
  Result := TJmVector.Create(b.Columns);
  rRow := Result.FData;
  bData := @b.FData^;     // b data is accessed sequentially
  for r := 1 to FN do
    for c := 1 to b.Columns do begin
      rRow^[c] := rRow^[c] + FData^[r] * bData^;
      Inc(bData);
    end;
end;

// self * b = result
// Returns TJmVector, TJmMatrix, TJmMatrixSquare, or TJmMatrix2x2
function TJmVector.Multiply(b: TJmMatrixBase): TJmMatrixBase;
begin
  if b is TJmVector then
    Result := MultiplyVector(b as TJmVector)
  else if (b is TJmMatrix2x2) and (FN = 2) then
    Result := Multiply2x2(b as TJmMatrix2x2)
  else if b.Rows = FN then
    Result := MultiplyAsRow(b as TJmMatrix)
  else raise EJmMatrix.Create('Incompatible matrix sizes');
end;

/////////////////////////////////////////////////////////////////////////////
// TJmMatrixSquare
constructor TJmMatrixSquare.Create(d: TJmMatrixBounds);
begin
  inherited Create(d, d);
  FCanConvert := true;
  FLU := nil;
end;

constructor TJmMatrixSquare.Identity(d: TJmMatrixBounds);
var c: TJmMatrixBounds;
begin
  inherited Create(d, d);
  FCanConvert := true;
  FLU := nil;
  for c := 1 to d do
    FData^[(c-1)*d+c] := 1.0;
end;

destructor TJmMatrixSquare.Destroy;
begin
  if Assigned(FLU) then
    FLU.Free;
  inherited;
end;

procedure TJmMatrixSquare.DataChanged;
begin
  if Assigned(FLU) then
    FLU.Free;
end;

function TJmMatrixSquare.Inverse: TJmMatrixSquare;
var b: TJmVector;
    c, r: TJmMatrixBounds;
    bRow: PJmFloatArray;
begin
  Result := TJmMatrixSquare.Copy(self);
  b := TJmVector.Create(FRows);
  bRow := b.Row;
  if not Assigned(FLU) then
    Decompose;
  for c := 1 to FCols do begin
    for r := 1 to FRows do
      bRow^[r] := 0.0;
    bRow^[c] := 1.0;
    (FLU as TJmMatrixDecompose).Solve(b);
    for r := 1 to FRows do
      Result.FData^[(r-1)*FCols+c] := bRow^[r];
  end;
  b.Free;
end;

// Gauss-Jordan linear algebraic equation solution with full pivoting
//                        self dot x = p
// Self will be converted to its inverse. p is a matrix with
// an equal number of rows as self and any number of columns (simultaneous
// equation systems). p will be converted to the solution x. If p = nil then
// only self will be inverted.
procedure TJmMatrixSquare.GaussJordan(p: TJmMatrix);
var colNdx, rowNdx, pivot: PIntegerArray;
    rowPtr, pivPtr: PJmFloatArray;
    big, v: TJmFloat;
    i, c, r, col, row, rowSize: integer;
begin
  if Assigned(p) and (p.Rows < FRows) then
    raise EJmMatrix.Create('GaussJordan: Solution vector sub-dimensional');

  rowSize := FRows * SizeOf(integer);
  GetMem(colNdx, rowSize);
  try
    GetMem(rowNdx, rowSize);
    try
      pivot := AllocMem(rowSize);  // Assign + initialize to 0
      try

        //Entering the main loop
        col := 0; row := 0; // avoid compiler warnings
        for i := 1 to FRows do begin
          // Finding the pivot element
          big := 0.0;
          for r := 1 to FRows do begin
            rowPtr := GetRow(r);
            if pivot^[r] <> 1 then
              for c := 1 to FCols do
                if pivot^[c] = 0 then begin
                  v := Abs(rowPtr^[c]);
                  if v > big then begin
                    big := v;
                    row := r;
                    col := c;
                  end;
                end else
                  if pivot^[c] > 1 then
                    raise EJmMatrix.Create('GaussJordan: Singular matrix');
          end;
          Inc(pivot^[col]);

          // Interchange rows
          if row <> col then begin
            SwapRows(row, col);
            if p <> nil then
              p.SwapRows(row, col);
          end;

          // Divide the pivot row by the pivot element
          rowNdx^[i] := row;
          colNdx^[i] := col;
          rowPtr := GetRow(col);
          if big < AlmostZero then
            raise EJmMatrix.Create('GaussJordan: Singular Matrix');
          v := 1 / big;
          rowPtr^[col] := 1.0;
          for c := 1 to FCols do
            rowPtr^[c] := rowPtr^[c] * v;
          if p <> nil then begin
            rowPtr := p.Row[col];
            for c := 1 to p.Columns do
              rowPtr^[c] := rowPtr^[c] * v;
          end;

          // Reduce the rows
          for r := 1 to FRows do
            if r <> col then begin
              rowPtr := GetRow(r);
              pivPtr := GetRow(col);
              v := rowPtr^[col];
              rowPtr^[col] := 0.0;
              for c := 1 to FCols do
                rowPtr^[c] := rowPtr^[c] - pivPtr^[c] * v;
              if p <> nil then begin
                rowPtr := p.Row[r];
                pivPtr := p.Row[col];
                for c := 1 to p.Columns do
                  rowPtr^[c] := rowPtr^[c] - pivPtr^[c] * v;
              end;
            end;
        end;  // Main loop

        // Unscramble the result
        for c := FCols downto 1 do
          if rowNdx^[c] <> colNdx^[c] then
            for r := 1 to FRows do begin
              rowPtr := GetRow(r);
              v := rowPtr^[rowNdx^[c]];
              rowPtr^[rowNdx^[c]] := rowPtr^[colNdx^[c]];
              rowPtr^[colNdx^[c]] := v;
            end;

      finally
        FreeMem(pivot);
      end;
    finally
      FreeMem(rowNdx);
    end;
  finally
    FreeMem(colNdx);
  end;

  DataChanged;
end;

function TJmMatrixSquare.Determinant: TJmFloat;
begin
  if not Assigned(FLU) then Decompose;
  Result := (FLU as TJmMatrixDecompose).Determinant;
end;

function TJmMatrixSquare.SolveVector(b: TJmVector; iter: TJmIterBounds = 0): TJmVector;
begin
  if b.Rows <> FRows then
    raise EJmMatrix.Create('Incompatible matrix sizes');
  Result := TJmVector.Copy(b);
  if not Assigned(FLU) then Decompose;
  (FLU as TJmMatrixDecompose).Solve(Result);
  if iter > 0 then
    (FLU as TJmMatrixDecompose).Improve(self, b, Result, iter);
end;

procedure TJmMatrixSquare.Decompose;
begin
  if Assigned(FLU) then
    FLU.Free;
  FLU := TJmMatrixDecompose.Create(self);
end;

function TJmMatrixSquare.InverseMultiplyMatrix(b: TJmMatrix): TJmMatrix;
var r, c, bCols: integer;
    bRow: PJmFloatArray;
    bCol: TJmVector;
begin
  bCols := b.Columns;
  if bCols = FCols then
    Result := TJmMatrixSquare.Create(FCols)
  else Result := TJmMatrix.Create(FRows, bCols);
  for c := 1 to bCols do begin
    bCol := b.ColumnAsVector(c);
    (FLU as TJmMatrixDecompose).Solve(bCol);
    bRow := bCol.Row;
    for r := 1 to FRows do
      Result.Data^[(c-1)*FCols+r] := bRow^[r];
    bCol.Free;
  end;
end;

function TJmMatrixSquare.InverseMultiplyVector(b: TJmVector): TJmVector;
begin
  Result := TJmVector.Copy(b);
  (FLU as TJmMatrixDecompose).Solve(Result);
end;

function TJmMatrixSquare.InverseMultiply(b: TJmMatrixBase): TJmMatrixBase;
begin
  if b.Rows <> FCols then
    raise EJmMatrix.Create('Incompatible matrix sizes');
  if not Assigned(FLU) then
    Decompose;
  if b is TJmMatrix then
    Result := InverseMultiplyMatrix(b as TJmMatrix)
  else if b is TJmVector then
    Result := InverseMultiplyVector(b as TJmVector)
  else Result := nil;
end;

////////////////////////////////////////////////////////////////////////////
// TJmMatrixDecompose
constructor TJmMatrixDecompose.Create(m: TJmMatrixSquare);
var r, c, ndx, maxRow: TJmMatrixBounds;
    v, big, temp: TJmFloat;
    scale, rowPtr, pivPtr: PJmFloatArray;
begin
  Copy(m);
  GetMem(FPermutation, FRows * SizeOf(integer));
  FInterchanges := 0;
  GetMem(scale, FRows * SizeOf(TJmFloat));

  // Scaling information
  for r := 1 to FRows do begin
    big := 0.0;
    rowPtr := GetRow(r);
    for c := 1 to FCols do begin
      v := Abs(rowPtr^[c]);
      if v > big then big := v;
    end;
    if big < AlmostZero then
      raise EJmMatrix.Create('LU: Singular matrix');
    scale^[r] := 1 / big;
  end;

  // Crout's algorithm
  maxRow := 5000; // avoid compiler warnings
  for c := 1 to FCols do begin
    for r := 1 to c - 1 do begin
      rowPtr := GetRow(r);
      v := rowPtr^[c];
      for ndx := 1 to r - 1 do
        v := v - rowPtr^[ndx] * FData^[(ndx-1)*FCols+c];
      rowPtr^[c] := v;
    end;
    big := 0.0;
    for r := c to FRows do begin
      rowPtr := GetRow(r);
      v := rowPtr^[c];
      for ndx := 1 to c - 1 do
        v := v - rowPtr^[ndx] * FData^[(ndx-1)*FCols+c];
      rowPtr^[c] := v;
      temp := scale^[r] * Abs(v);
      if temp >= big then begin
        big := temp;
        maxRow := r;
      end;
    end;

    if c <> maxRow then begin
      rowPtr := GetRow(maxRow);
      pivPtr := GetRow(c);
      for ndx := 1 to FCols do begin
        v := rowPtr^[ndx];
        rowPtr^[ndx] := pivPtr^[ndx];
        pivPtr^[ndx] := v;
      end;
      Inc(FInterchanges);
      scale^[maxRow] := scale^[c];
    end;

    FPermutation^[c] := maxRow;
    if Abs(FData^[(c-1)*FCols+c]) < AlmostZero then
      raise EJmMatrix.Create('Singular matrix');

    // Divide by the pivot element
    if c < FCols then begin
      v := 1 / FData^[(c-1)*FCols+c];
      for ndx := c + 1 to FRows do
        FData^[(ndx-1)*FCols+c] := FData^[(ndx-1)*FCols+c] * v;
    end;
  end;
  FreeMem(scale);
end;

destructor TJmMatrixDecompose.Destroy;
begin
  FreeMem(FPermutation);
  inherited;
end;

function TJmMatrixDecompose.Determinant: TJmFloat;
var
  ndx : integer;
  selfData: ^TJmFloat;
begin
  if Odd(FInterchanges) then
    Result := -1.0 else Result := 1.0;
  selfData := @FData^;
  for ndx := 1 to FRows do begin
    Result := Result * selfData^;
    Inc(selfData, FCols+1);
  end;
end;

// Backsubstitution of b in equation A dot x = b. Self is the LU decomposition
// of A. b is transformed into x.
procedure TJmMatrixDecompose.Solve(b: TJmVector);
var
  r, ndx, c, p: integer;
  sum: TJmFloat;
  rowPtr, vecPtr: PJmFloatArray;
begin
  ndx := 0;
  vecPtr := b.Row;
  for r := 1 to FRows do begin
    rowPtr := GetRow(r);
    p := FPermutation^[r];
    sum := vecPtr^[p];
    vecPtr^[p] := vecPtr^[r];
    if ndx <> 0 then
      for c := ndx to r - 1 do
        sum := sum - rowPtr^[c] * vecPtr^[c]
    else if Abs(sum) > AlmostZero then
      ndx := r;
    vecPtr^[r] := sum;
  end;
  for r := FRows downto 1 do begin
    rowPtr := GetRow(r);
    sum := vecPtr^[r];
    for c := r + 1 to FCols do
      sum := sum - rowPtr^[c] * vecPtr^[c];
    vecPtr^[r] := sum / rowPtr^[r];
  end;
end;

procedure TJmMatrixDecompose.Improve(m: TJmMatrixSquare; b, res: TJmVector; iter: TJmIterBounds);
var i, r, c: integer;
    err: TJmVector;
    sum: TJmFloat;
    errRow, bRow, rRow, mRow: PJmFloatArray;
begin
  err := TJmVector.Create(FRows);
  bRow := b.Row;
  rRow := res.Row;
  errRow := err.Row;
  for i := 1 to iter do begin
    for r := 1 to FRows do begin
      mRow := m.GetRow(r);
      sum := -bRow^[r];
      for c := 1 to FCols do
        sum := sum + mRow^[c] * rRow^[c];
      errRow^[r] := sum;
    end;
    Solve(err);
    for r := 1 to FRows do
      rRow^[r] := rRow^[r] - errRow^[r];
  end;
  err.Free;
end;

/////////////////////////////////////////////////////////////////////////////
// TJmMatrix2x2
constructor TJmMatrix2x2.Create;
begin
  inherited Create;
  FCanConvert := true;
end;

constructor TJmMatrix2x2.CreateFromStream(strm: TStream);
var
  t: cardinal;
begin
  strm.Read(t, SizeOf(t));
  strm.Read(t, SizeOf(t));
  if t <> 2 then
    raise EJmMatrix.Create('Invalid dimension in stream');
  strm.Read(FData, 4 * SizeOf(TJmFloat));
  FCanConvert := true;
end;

constructor TJmMatrix2x2.Identity;
begin
  inherited Create;
  FCanConvert := true;
  FData[1,1] := 1.0;
  FData[1,2] := 0.0;
  FData[2,1] := 0.0;
  FData[2,2] := 1.0;
end;

constructor TJmMatrix2x2.Copy(m : TJmMatrix2x2);
begin
  inherited Create;
  FCanConvert := true;
  Move(m.FData[1,1], FData[1,1], 4 * SizeOf(TJmFloat));
end;

// LoadFromStream loads the data for the matrix from the strm parameter. The
// size of the matrix should be implicit in the stream data and no checks are
// performed.
procedure TJmMatrix2x2.LoadFromStream(var strm: TStream);
begin
  strm.Read(FData, 4 * SizeOf(TJmFloat));
end;

procedure TJmMatrix2x2.SaveToStream(var strm: TStream);
var
  d: cardinal;
begin
  d := 2;
  strm.Write(d, SizeOf(d));
  strm.Write(d, SizeOf(d));
  strm.Write(FData, 4 * SizeOf(TJmFloat));
end;

function TJmMatrix2x2.MultiplyMatrix2x2(b: TJmMatrix2x2): TJmMatrix2x2;
begin
  Result := TJmMatrix2x2.Create;
  Result.FData[1,1] := FData[1,1] * b.FData[1,1] + FData[1,2] * b.FData[2,1];
  Result.FData[1,2] := FData[1,1] * b.FData[1,2] + FData[1,2] * b.FData[2,2];
  Result.FData[2,1] := FData[2,1] * b.FData[1,1] + FData[2,2] * b.FData[2,1];
  Result.FData[2,2] := FData[2,1] * b.FData[1,2] + FData[2,2] * b.FData[2,2];
end;

function TJmMatrix2x2.MultiplyMatrix(b: TJmMatrix): TJmMatrix;
var c: integer;
    cols: TJmMatrixBounds;
    bRow1, bRow2, rRow1, rRow2: PJmFloatArray;
begin
  cols := b.Columns;
  Result := TJmMatrix.Create(2, cols);
  bRow1 := b.Row[1];
  bRow2 := b.Row[2];
  rRow1 := Result.Row[1];
  rRow2 := Result.Row[2];
  for c := 1 to cols do begin
    rRow1^[c] := FData[1,1] * bRow1^[c] + FData[1,2] * bRow2^[c];
    rRow2^[c] := FData[2,1] * bRow1^[c] + FData[2,2] * bRow2^[c];
  end;
end;

function TJmMatrix2x2.MultiplyVector(b: TJmVector): TJmVector;
begin
  Result := TJmVector.Create(2);
  Result.FData^[1] := FData[1,1] * b.FData^[1] + FData[1,2] * b.FData^[2];
  Result.FData^[2] := FData[2,1] * b.FData^[1] + FData[2,2] * b.FData^[2];
end;

// self * b = result
// Result is returned as either a TJmMatrix, TJmMatrixSquare or TJmVector
function TJmMatrix2x2.Multiply(b: TJmMatrixBase): TJmMatrixBase;
begin
  if b.Rows <> 2 then
    raise EJmMatrix.Create('Incompatible matrix sizes');

  if b is TJmMatrix2x2 then
    Result := MultiplyMatrix2x2(b as TJmMatrix2x2)
  else if b is TJmMatrix then
    Result := MultiplyMatrix(b as TJmMatrix)
  else if b is TJmVector then
    Result := MultiplyVector(b as TJmVector)
  else Result := nil;
end;

function TJmMatrix2x2.Add(b: TJmMatrix2x2): TJmMatrix2x2;
begin
  Result := TJmMatrix2x2.Create;
  Result.FData[1,1] := FData[1,1] + b.FData[1,1];
  Result.FData[1,2] := FData[1,2] + b.FData[1,2];
  Result.FData[2,1] := FData[2,1] + b.FData[2,1];
  Result.FData[2,2] := FData[2,2] + b.FData[2,2];
end;

procedure TJmMatrix2x2.MultiplyByScalar(v: TJmFloat);
begin
  FData[1,1] := FData[1,1] * v;
  FData[1,2] := FData[1,2] * v;
  FData[2,1] := FData[2,1] * v;
  FData[2,2] := FData[2,2] * v;
end;

function TJmMatrix2x2.Transpose: TJmMatrix2x2;
begin
  Result := TJmMatrix2x2.Create;
  Result.FData[1,1] := FData[1,1];
  Result.FData[1,2] := FData[2,1];
  Result.FData[2,1] := FData[1,2];
  Result.FData[2,2] := FData[2,2];
end;

function TJmMatrix2x2.GetRows: TJmMatrixBounds;
begin
  Result := 2;
end;

function TJmMatrix2x2.GetColumns: TJmMatrixBounds;
begin
  Result := 2;
end;

function TJmMatrix2x2.GetValue(r, c: TJmMatrixBounds): TJmFloat;
begin
  if (r > 2) or (c > 2) then
    raise EJmMatrix.Create('Index out of bounds');
  Result := FData[r,c];
end;

procedure TJmMatrix2x2.SetValue(r, c: TJmMatrixBounds; v: TJmFloat);
begin
  if (r > 2) or (c > 2) then
    raise EJmMatrix.Create('Index out of bounds');
  FData[r,c] := v;
end;

function TJmMatrix2x2.Inverse: TJmMatrix2x2;
var d, s: TJmFloat;
begin
  d := Determinant;
  if Abs(d) < AlmostZero then
    raise EJmMatrix.Create('Singular matrix');
  s := 1 / d;
  Result := TJmMatrix2x2.Create;
  Result.FData[1,1] :=  FData[2,2] * s;
  Result.FData[1,2] := -FData[1,2] * s;
  Result.FData[2,1] := -FData[2,1] * s;
  Result.FData[2,2] :=  FData[1,1] * s;
end;

function TJmMatrix2x2.Determinant: TJmFloat;
begin
  Result := FData[1,1] * FData[2,2] - FData[1,2] * FData[2,1];
end;

/////////////////////////////////////////////////////////////////////////////
// TJmMatrix3x3
constructor TJmMatrix3x3.Create;
begin
  inherited Create;
  FCanConvert := true;
end;

constructor TJmMatrix3x3.CreateFromStream(strm: TStream);
var
  t: cardinal;
begin
  strm.Read(t, SizeOf(t));
  strm.Read(t, SizeOf(t));
  if t <> 3 then
    raise EJmMatrix.Create('Invalid dimension in stream');
  strm.Read(FData, 9 * SizeOf(TJmFloat));
  FCanConvert := true;
end;

constructor TJmMatrix3x3.Identity;
begin
  inherited Create;
  FCanConvert := true;
  FData[1,1] := 1.0;
  FData[1,2] := 0.0;
  FData[1,3] := 0.0;
  FData[2,1] := 0.0;
  FData[2,2] := 1.0;
  FData[2,3] := 0.0;
  FData[3,1] := 0.0;
  FData[3,2] := 0.0;
  FData[3,3] := 1.0;
end;

constructor TJmMatrix3x3.Copy(m : TJmMatrix3x3);
begin
  inherited Create;
  FCanConvert := true;
  Move(m.FData[1,1], FData[1,1], 9 * SizeOf(TJmFloat));
end;

// LoadFromStream loads the data for the matrix from the strm parameter. The
// size of the matrix should be implicit in the stream data and no checks are
// performed.
procedure TJmMatrix3x3.LoadFromStream(var strm: TStream);
begin
  strm.Read(FData, 9 * SizeOf(TJmFloat));
end;

procedure TJmMatrix3x3.SaveToStream(var strm: TStream);
var
  d: cardinal;
begin
  d := 3;
  strm.Write(d, SizeOf(d));
  strm.Write(d, SizeOf(d));
  strm.Write(FData, 9 * SizeOf(TJmFloat));
end;

function TJmMatrix3x3.MultiplyMatrix3x3(b: TJmMatrix3x3): TJmMatrix3x3;
begin
  Result := TJmMatrix3x3.Create;
  Result.FData[1,1] := FData[1,1] * b.FData[1,1] + FData[1,2] * b.FData[2,1] + FData[1,3] * b.FData[3,1];
  Result.FData[1,2] := FData[1,1] * b.FData[1,2] + FData[1,2] * b.FData[2,2] + FData[1,3] * b.FData[3,2];
  Result.FData[1,3] := FData[1,1] * b.FData[1,3] + FData[1,2] * b.FData[2,3] + FData[1,3] * b.FData[3,3];
  Result.FData[2,1] := FData[2,1] * b.FData[1,1] + FData[2,2] * b.FData[2,1] + FData[2,3] * b.FData[3,1];
  Result.FData[2,2] := FData[2,1] * b.FData[1,2] + FData[2,2] * b.FData[2,2] + FData[2,3] * b.FData[3,2];
  Result.FData[2,3] := FData[2,1] * b.FData[1,3] + FData[2,2] * b.FData[2,3] + FData[2,3] * b.FData[3,3];
  Result.FData[3,1] := FData[3,1] * b.FData[1,1] + FData[3,2] * b.FData[2,1] + FData[3,3] * b.FData[3,1];
  Result.FData[3,2] := FData[3,1] * b.FData[1,2] + FData[3,2] * b.FData[2,2] + FData[3,3] * b.FData[3,2];
  Result.FData[3,3] := FData[3,1] * b.FData[1,3] + FData[3,2] * b.FData[2,3] + FData[3,3] * b.FData[3,3];
end;

function TJmMatrix3x3.MultiplyMatrix(b: TJmMatrix): TJmMatrix;
var c: integer;
    cols: TJmMatrixBounds;
    bRow1, bRow2, bRow3, rRow1, rRow2, rRow3: PJmFloatArray;
begin
  cols := b.Columns;
  Result := TJmMatrix.Create(3, cols);
  bRow1 := b.Row[1];
  bRow2 := b.Row[2];
  bRow3 := b.Row[3];
  rRow1 := Result.Row[1];
  rRow2 := Result.Row[2];
  rRow3 := Result.Row[3];
  for c := 1 to cols do begin
    rRow1^[c] := FData[1,1] * bRow1^[c] + FData[1,2] * bRow2^[c] + FData[1,3] * bRow3^[c];
    rRow2^[c] := FData[2,1] * bRow1^[c] + FData[2,2] * bRow2^[c] + FData[2,3] * bRow3^[c];
    rRow3^[c] := FData[3,1] * bRow1^[c] + FData[3,2] * bRow2^[c] + FData[3,3] * bRow3^[c];
  end;
end;

function TJmMatrix3x3.MultiplyVector(b: TJmVector): TJmVector;
begin
  Result := TJmVector.Create(3);
  Result.FData^[1] := FData[1,1] * b.FData^[1] + FData[1,2] * b.FData^[2] + FData[1,3] * b.FData^[3];
  Result.FData^[2] := FData[2,1] * b.FData^[1] + FData[2,2] * b.FData^[2] + FData[2,3] * b.FData^[3];
  Result.FData^[3] := FData[3,1] * b.FData^[1] + FData[3,2] * b.FData^[2] + FData[3,3] * b.FData^[3];
end;

// self * b = result
// Result is returned as either a TJmMatrix, TJmMatrixSquare or TJmVector
function TJmMatrix3x3.Multiply(b: TJmMatrixBase): TJmMatrixBase;
begin
  if b.Rows <> 3 then
    raise EJmMatrix.Create('Incompatible matrix sizes');

  if b is TJmMatrix3x3 then
    Result := MultiplyMatrix3x3(b as TJmMatrix3x3)
  else if b is TJmMatrix then
    Result := MultiplyMatrix(b as TJmMatrix)
  else if b is TJmVector then
    Result := MultiplyVector(b as TJmVector)
  else Result := nil;
end;

function TJmMatrix3x3.Add(b: TJmMatrix3x3): TJmMatrix3x3;
begin
  Result := TJmMatrix3x3.Create;
  Result.FData[1,1] := FData[1,1] + b.FData[1,1];
  Result.FData[1,2] := FData[1,2] + b.FData[1,2];
  Result.FData[1,3] := FData[1,3] + b.FData[1,3];
  Result.FData[2,1] := FData[2,1] + b.FData[2,1];
  Result.FData[2,2] := FData[2,2] + b.FData[2,2];
  Result.FData[2,3] := FData[2,3] + b.FData[2,3];
  Result.FData[3,1] := FData[3,1] + b.FData[3,1];
  Result.FData[3,2] := FData[3,2] + b.FData[3,2];
  Result.FData[3,3] := FData[3,3] + b.FData[3,3];
end;

procedure TJmMatrix3x3.MultiplyByScalar(v: TJmFloat);
begin
  FData[1,1] := FData[1,1] * v;
  FData[1,2] := FData[1,2] * v;
  FData[1,3] := FData[1,3] * v;
  FData[2,1] := FData[2,1] * v;
  FData[2,2] := FData[2,2] * v;
  FData[2,3] := FData[2,3] * v;
  FData[3,1] := FData[3,1] * v;
  FData[3,2] := FData[3,2] * v;
  FData[3,3] := FData[3,3] * v;
end;

function TJmMatrix3x3.Transpose: TJmMatrix3x3;
begin
  Result := TJmMatrix3x3.Create;
  Result.FData[1,1] := FData[1,1];
  Result.FData[1,2] := FData[2,1];
  Result.FData[1,3] := FData[3,1];
  Result.FData[2,1] := FData[1,2];
  Result.FData[2,2] := FData[2,2];
  Result.FData[2,3] := FData[3,2];
  Result.FData[3,1] := FData[1,3];
  Result.FData[3,2] := FData[2,3];
  Result.FData[3,3] := FData[3,3];
end;

function TJmMatrix3x3.GetRows: TJmMatrixBounds;
begin
  Result := 3;
end;

function TJmMatrix3x3.GetColumns: TJmMatrixBounds;
begin
  Result := 3;
end;

function TJmMatrix3x3.GetValue(r, c: TJmMatrixBounds): TJmFloat;
begin
  if (r > 3) or (c > 3) then
    raise EJmMatrix.Create('Index out of bounds');
  Result := FData[r,c];
end;

procedure TJmMatrix3x3.SetValue(r, c: TJmMatrixBounds; v: TJmFloat);
begin
  if (r > 3) or (c > 3) then
    raise EJmMatrix.Create('Index out of bounds');
  FData[r,c] := v;
end;

// A-1 := adj(A) / det(A) 
function TJmMatrix3x3.Inverse: TJmMatrix3x3;
var d, s: TJmFloat;
begin
  d := Determinant;
  if Abs(d) < AlmostZero then
    raise EJmMatrix.Create('Singular matrix');
  s := 1 / d;
  Result := TJmMatrix3x3.Create;
  Result.FData[1,1] :=  (FData[2,2] * FData[3,3] - FData[3,2] * FData[2,3]) * s;
  Result.FData[1,2] := -(FData[1,2] * FData[3,3] - FData[1,3] * FData[3,2]) * s;
  Result.FData[1,3] :=  (FData[1,2] * FData[2,3] - FData[1,3] * FData[2,2]) * s;
  Result.FData[2,1] := -(FData[2,1] * FData[3,3] - FData[2,3] * FData[3,1]) * s;
  Result.FData[2,2] :=  (FData[1,1] * FData[3,3] - FData[1,3] * FData[3,1]) * s;
  Result.FData[2,3] := -(FData[1,1] * FData[2,3] - FData[1,3] * FData[2,1]) * s;
  Result.FData[3,1] :=  (FData[2,1] * FData[3,2] - FData[2,2] * FData[3,1]) * s;
  Result.FData[3,2] := -(FData[1,1] * FData[3,2] - FData[1,2] * FData[3,1]) * s;
  Result.FData[3,3] :=  (FData[1,1] * FData[2,2] - FData[2,1] * FData[1,2]) * s;
end;

// Co-factor expansion of row 1
function TJmMatrix3x3.Determinant: TJmFloat;
begin
  Result := FData[1,1] * (FData[2,2] * FData[3,3] - FData[3,2] * FData[2,3]) -
            FData[1,2] * (FData[2,1] * FData[3,3] - FData[3,1] * FData[2,3]) +
            FData[1,3] * (FData[2,1] * FData[3,2] - FData[3,1] * FData[2,2]);
end;

////////////////////////////////////////////////////////////////////////////////
// Public functions that create any kind of matrix
function CreateMatrix(const r, c: TJmMatrixBounds; strm: TMemoryStream = nil): TJmMatrixBase;
begin
  if r = c then
    if r = 2 then
      if strm = nil then
        Result := TJmMatrix2x2.Create
      else Result := TJmMatrix2x2.CreateFromStream(strm)
    else if r = 3 then
      if strm = nil then
        Result := TJmMatrix3x3.Create
      else Result := TJmMatrix3x3.CreateFromStream(strm)
    else
      if strm = nil then
        Result := TJmMatrixSquare.Create(r)
      else Result := TJmMatrixSquare.CreateFromStream(strm)
  else
    if c = 1 then
      if strm = nil then
        Result := TJmVector.Create(r)
      else Result := TJmVector.CreateFromStream(strm)
    else
      if strm = nil then
        Result := TJmMatrix.Create(r, c)
      else Result := TJmMatrix.CreateFromStream(strm)
end;

function CreateIdentityMatrix(const d: TJmMatrixBounds): TJmMatrixBase;
begin
  if d = 2 then
    Result := TJmMatrix2x2.Identity
  else if d = 3 then
    Result := TJmMatrix3x3.Identity
  else
    Result := TJmMatrixSquare.Identity(d)
end;

procedure TJmMatrix.CheckBounds(const r, c: TJmMatrixBounds);
begin
  if r > FRows then raise EJmMatrix.Create(Format('Row %d out of bounds', [r]));
  if c > FCols then raise EJmMatrix.Create(Format('Column %d out of bounds', [c]));
end;

function TJmMatrix.GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
begin
  result := GetValue(R + 1, C + 1);
end;

procedure TJmMatrix.SetValueZeroBased(R, C: TJmMatrixBounds;
  const Value: TJmFloat);
begin
  SetValue(R + 1, C + 1, Value);
end;

function TJmMatrix2x2.GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
begin
  result := GetValue(R + 1, C + 1);
end;

procedure TJmMatrix2x2.SetValueZeroBased(R, C: TJmMatrixBounds;
  const Value: TJmFloat);
begin
  SetValue(R + 1, C + 1, Value);
end;

function TJmMatrix3x3.GetValueZeroBased(R, C: TJmMatrixBounds): TJmFloat;
begin
  result := GetValue(R + 1, C + 1);
end;

procedure TJmMatrix3x3.SetValueZeroBased(R, C: TJmMatrixBounds;
  const Value: TJmFloat);
begin
  SetValue(R + 1, C + 1, Value);
end;

function TJmVector.GetValueZeroBased(n: TJmMatrixBounds): TJmFloat;
begin
  result := GetValue(n + 1);
end;

procedure TJmVector.SetValueZeroBased(n: TJmMatrixBounds;
  const Value: TJmFloat);
begin
  SetValue( n + 1, Value )
end;

end.


