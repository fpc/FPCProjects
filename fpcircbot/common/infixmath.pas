unit InfixMath;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  DEF_INFLATE = 10;

type
  TCharInt = record
    case Byte of
      0: (c: Char);
      1: (i: Integer);
  end;

  TCharStack = class
   protected
    FData: array of TCharInt;
    FPos: Integer;
    FLength: Integer;
    function GetEmpty: Boolean;
    procedure Inflate;
   public
    constructor Create;
    function Top: TCharInt;
    function Pop: TCharInt;
    procedure Push(const s: TCharInt);
    procedure Clear;
    procedure WriteChars;
    procedure WriteInts;
    property Empty: Boolean read GetEmpty;
  end;

  operator := (c: Char): TCharInt;
  operator := (i: Integer): TCharInt;
  operator := (ci: TCharInt): Char;
  operator := (ci: TCharInt): Integer;

  function ParseStringForMath(const s: string): Integer;

implementation

uses
  Math;

var
  aZero: TCharInt; 
  OperatorSet, OperandSet: set of Char;

operator := (c: Char): TCharInt;
begin
  Result.c:=c;
end;

operator := (i: Integer): TCharInt;
begin
  Result.i:=i;
end;

operator := (ci: TCharInt): Char;
begin
  Result:=ci.c;
end;

operator := (ci: TCharInt): Integer;
begin
  Result:=ci.i;
end;

constructor TCharStack.Create;
begin
  FData:=nil;
  FPos:=-1;
  FLength:=0;
  Inflate;
end;

function TCharStack.GetEmpty: Boolean;
begin
  Result:=FPos < 0;
end;

procedure TCharStack.Inflate;
begin
  FLength:=FLength + DEF_INFLATE;
  SetLength(FData, FLength);
end;

function TCharStack.Top: TCharInt;
begin
  Result:=#0;
  if FPos >= 0 then
    Result:=FData[FPos];
end;

function TCharStack.Pop: TCharInt;
begin
  Result:=#0;
  if FPos >= 0 then begin
    Result:=FData[FPos];
    Dec(FPos);
  end;
end;

procedure TCharStack.Push(const s: TCharInt);
begin
  Inc(FPos);
  if FPos >= FLength then Inflate;
  FData[FPos]:=s;
end;

procedure TCharStack.Clear;
begin
  FPos:=-1;
end;

procedure TCharStack.WriteChars;
var
  i: Integer;
begin
  if FPos >= 0 then
    for i:=0 to FPos do
      System.Write(FData[i].c, ' ');
  Writeln;
end;

procedure TCharStack.WriteInts;
var
  i: Integer;
begin
  if FPos >= 0 then
    for i:=0 to FPos do
      System.Write(FData[i].i, ' ');
  Writeln;
end;

// parser

function ParseStringForMath(const s: string): Integer;
var
  Operands, Operators: TCharStack;

  function GetStrength(const OP: Char): Integer;
  begin
    Result:=0;
    case OP of
      '+': Result:=1;
      '-': Result:=1;
      '/': Result:=2;
      '*': Result:=2;
      '%': Result:=2;
      '^': Result:=3;
    end;
  end;

  function ComputeMath(const OP: Char; const N1, N2: Integer): Integer;
  begin
    Result:=N1;
    case OP of
      '+': Result:=Result + N2;
      '-': Result:=Result - N2;
      '/': Result:=Result div N2;
      '*': Result:=Result * N2;
      '%': Result:=Result mod N2;
      '^': Result:=Result ** N2;
    end;
  end;

  function ComputeLast: Integer;
  var
    N1: Integer;
  begin
    if not Operands.Empty then begin
      Result:=Operands.Pop;
      while not Operands.Empty do begin
        N1:=Operands.Pop;
        Result:=ComputeMath(Operators.Pop, Result, N1);
      end;
    end else raise Exception.Create('Cannot calculate');
  end;

  function GetOtherP(const s: string; const i: Integer): Integer;
  var
    Count: Integer;
  begin
    Result:=i;
    Count:=1;
    while Result >= 1 do begin
      if s[Result] = ')' then Inc(Count);
      if s[Result] = '(' then Dec(Count);
      if Count = 0 then begin
        Result:=Result + 1;
        Exit;
      end;
      Dec(Result);
    end;
  end;
  
  function ValidP(const s: string): Boolean;
  var
    i, Count: Integer;
  begin
    Count:=0;
    Result:=False;
    if Length(s) > 0 then
      for i:=1 to Length(s) do begin
        if Count < 0 then Exit(False);
        if s[i] = '(' then Inc(Count);
        if s[i] = ')' then Dec(Count);
      end;
    if Count = 0 then Result:=True;
  end;
  
var
  i, n, Last: Integer;
  LastPushed: Char;
  Num: string;
begin
  if not ValidP(s) then begin
    raise Exception.Create('Wrong parentheses');
    Exit(0);
  end;
  LastPushed:=#0;
  Operands:=TCharStack.Create;
  Operators:=TCharStack.Create;
  Last:=0;
  Num:='';
  i:=Length(s);
  if Length(s) > 0 then
    while i >= 1 do begin
      if s[i] in OperandSet then begin
        Num:=s[i] + Num;
        LastPushed:=#0;
      end else if s[i] in OperatorSet then begin
        if GetStrength(s[i]) < Last then begin
          if Length(Num) > 0 then begin
            Operands.Push(StrToInt(Num));
            Num:='';
          end;
          if LastPushed = '-' then
            Operands.Push(aZero);
          Operands.Push(ComputeLast);
        end;
        Last:=GetStrength(s[i]);
        LastPushed:=s[i];
        Operators.Push(s[i]);
        if Length(Num) > 0 then begin
          Operands.Push(StrToInt(Num));
          Num:='';
        end;
      end else begin
        if Length(Num) > 0 then begin
          Operands.Push(StrToInt(Num));
          Num:='';
          LastPushed:=#0;
        end;
        
        if (s[i] = ')') and (i > 1) then begin
          n:=GetOtherP(s, i - 1);
          Operands.Push(ParseStringForMath(Copy(s, n, i - n)));
          Dec(i, i - n);
        end;
      end;
      Dec(i);
    end;
  if Length(Num) > 0 then
    Operands.Push(StrToInt(Num));
  if LastPushed = '-' then
    Operands.Push(aZero);
  Result:=ComputeLast;
  Operands.Free;
  Operators.Free;
end;

initialization
  OperatorSet:=['+', '-', '/', '%', '*', '^'];
  OperandSet:=['0'..'9'];
  aZero.i:=0;

end.
