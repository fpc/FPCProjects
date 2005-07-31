{******************************************************************************}
{                                                                              }
{ JmMathBasics.pas for Jedi Math Alpha 1.02                                    }
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
{ The Original Code is jclMaths.pas.                                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit was initially based upon the jclMath unit.                         }
{ This unit contains                                                           }
{                                                                              }
{ Unit owner:    Chris Eyre                                                    }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}


unit JmMathBasics;
interface


{$IFDEF VER130}
   {$DEFINE MSWINDOWS} // For Delphi 5
{$ENDIF}

{$IFDEF VER140}
   {$DEFINE MSWINDOWS} // For Delphi 6
{$ENDIF}
{$i JediMath.inc}
uses

{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
{$IFDEF VER150} // This will need some refinement
  Types,
{$ENDIF}
  JmTypes;

resourcestring
  RsMathDomainError    = 'Domain check failure in JmMath';
  RsEmptyArray         = 'Empty array is not allowed as input parameter';
  RsNonPositiveArray   = 'Input array contains non-positive or zero values';
  RsUnexpectedDataType = 'Unexpected data type';
  RsUnexpectedValue    = 'Unexpected data value';
  RsRangeError         = 'Cannot merge range';
  RsInvalidRational    = 'Invalid rational number';
  RsDivByZero          = 'Division by zero';
  RsRationalDivByZero  = 'Rational division by zero';
  RsNoNaN              = 'NaN expected';
  RsNaNTagError        = 'NaN Tag value %d out of range';
  RsNaNSignal          = 'NaN signaling %d';



{ Mathematical constants }

const
  kCbrt2: TJmFloat   = 1.2599210498948731647672106072782;  // CubeRoot(2)
  kCbrt3: TJmFloat   = 1.4422495703074083823216383107801;  // CubeRoot(3)
  kCbrt10: TJmFloat  = 2.1544346900318837217592935665194;  // CubeRoot(10)
  kCbrt100: TJmFloat = 4.6415888336127788924100763509194;  // CubeRoot(100)
  kCbrtPi: TJmFloat  = 1.4645918875615232630201425272638;  // CubeRoot(PI)
  kPi: TJmFloat      = 3.1415926535897932384626433832795;  // PI
  kPiOn2: TJmFloat   = 1.5707963267948966192313216916398;  // PI / 2
  kPiOn3: TJmFloat   = 1.0471975511965977461542144610932;  // PI / 3
  kPiOn4: TJmFloat   = 0.78539816339744830961566084581988; // PI / 4
  kSqrt2: TJmFloat   = 1.4142135623730950488016887242097;  // Sqrt(2)
  kSqrt3: TJmFloat   = 1.7320508075688772935274463415059;  // Sqrt(3)
  kSqrt5: TJmFloat   = 2.2360679774997896964091736687313;  // Sqrt(5)
  kSqrt10: TJmFloat  = 3.1622776601683793319988935444327;  // Sqrt(10)
  kSqrtPi: TJmFloat  = 1.7724538509055160272981674833411;  // Sqrt(PI)
  kSqrt2Pi: TJmFloat = 2.506628274631000502415765284811;   // Sqrt(2 * PI)
  kTwoPi: TJmFloat   = 6.283185307179586476925286766559;   // 2 * PI
  kThreePi: TJmFloat = 9.4247779607693797153879301498385;  // 3 * PI
  kLn2: TJmFloat     = 0.69314718055994530941723212145818; // Ln(2)
  kLn10: TJmFloat    = 2.3025850929940456840179914546844;  // Ln(10)
  kLnPi: TJmFloat    = 1.1447298858494001741434273513531;  // Ln(PI)
  kLog2: TJmFloat    = 0.30102999566398119521373889472449; // Log10(2)
  kLog3: TJmFloat    = 0.47712125471966243729502790325512; // Log10(3)
  kLogPi: TJmFloat   = 0.4971498726941338543512682882909;  // Log10(PI)
  kLogE: TJmFloat    = 0.43429448190325182765112891891661; // Log10(E)
  E: TJmFloat       = 2.7182818284590452353602874713527;  // Natural constant
  khLn2Pi: TJmFloat  = 0.91893853320467274178032973640562; // Ln(2*PI)/2
  kinv2Pi: TJmFloat  = 0.159154943091895;                  // 0.5 / Pi
  kTwoToPower63: TJmFloat = 9223372036854775808.0;         // 2^63

const
  MaxAngle: TJmFloat = 9223372036854775808.0; // 2^63 Rad

type
  TDynByteArray     = array of Byte;
  TDynShortintArray = array of Shortint;
  TDynSmallintArray = array of Smallint;
  TDynWordArray     = array of Word;
  TDynIntegerArray  = array of Integer;
  TDynLongintArray  = array of Longint;
  TDynCardinalArray = array of Cardinal;
  TDynInt64Array    = array of Int64;
  TDynExtendedArray = array of Extended;
  TDynDoubleArray   = array of Double;
  TDynSingleArray   = array of Single;
  TDynFloatArray    = array of TJmFloat;
  TDynPointerArray  = array of Pointer;
  TDynStringArray   = array of string;

const

  {$IFDEF jmExtendedPrecision}
  MaxTanH: TJmFloat = 5678.2617031470719747459655389854; // Ln(2^16384)/2
  MaxFactorial   = 1754;
  MaxFloatingPoint: TJmFloat = 1.189731495357231765085759326628E+4932; // 2^16384
  MinFloatingPoint: TJmFloat = 3.3621031431120935062626778173218E-4932; // 2^(-16382)
  {$ENDIF jmExtendedPrecision}
  {$IFDEF jmDoublePrecision}
  MaxTanH : TJmFloat = 354.89135644669199842162284618659; // Ln(2^1024)/2
  MaxFactorial   = 170;
  MaxFloatingPoint : TJmFloat = 1.797693134862315907729305190789E+308; // 2^1024
  MinFloatingPoint : TJmFloat = 2.2250738585072013830902327173324E-308; // 2^(-1022)
  {$ENDIF jmDoublePrecision}
  {$IFDEF jmSinglePrecision}
  MaxTanH: TJmFloat = 44.361419555836499802702855773323; // Ln(2^128)/2
  MaxFactorial   = 33;
  MaxFloatingPoint: TJmFloat = 3.4028236692093846346337460743177E+38; // 2^128
  MinFloatingPoint: TJmFloat = 1.1754943508222875079687365372222E-38; // 2^(-126)
  {$ENDIF jmSinglePrecision}

var
  PrecisionTolerance: TJmFloat = 0.0000001;
  EpsSingle: Single;
  EpsDouble: Double;
  EpsExtended: Extended;
  Epsilon: TJmFloat;
  ThreeEpsSingle: Single;
  ThreeEpsDouble: Double;
  ThreeEpsExtended: Extended;
  ThreeEpsilon: TJmFloat;

type
  TPrimalityTestMethod = (ptTrialDivision, ptRabinMiller);
{ Logarithmic }

function LogBase10(X: TJmFloat): TJmFloat;
{Alternative version to replace maths unit}
function log10(X: TJmFloat): TJmFloat;

function LogBase2(X: TJmFloat): TJmFloat;
function LogBaseN(Base, X: TJmFloat): TJmFloat;
{Log of given base}
function logn(Base, X: TJmFloat): TJmFloat;
{Natural log, base E}
function ln( X: TJmFloat): TJmFloat;
{Natural log of N plus one}
function lnxp1( X: TJmFloat): TJmFloat;
function log2( X: TJmFloat): TJmFloat;

{ Transcendental }

function ArcCos(X: TJmFloat): TJmFloat;
function ArcCot(X: TJmFloat): TJmFloat;
function ArcCsc(X: TJmFloat): TJmFloat;
function ArcSec(X: TJmFloat): TJmFloat;
function ArcSin(X: TJmFloat): TJmFloat;
function ArcTan(X: TJmFloat): TJmFloat;
function ArcTan2(Y, X: TJmFloat): TJmFloat;
function Cos(X: TJmFloat): TJmFloat;
function Cot(X: TJmFloat): TJmFloat;
function cotan(X: TJmFloat): TJmFloat;

function Coversine(X: TJmFloat): TJmFloat;
function Csc(X: TJmFloat): TJmFloat;
function Exsecans(X: TJmFloat): TJmFloat;
function Haversine(X: TJmFloat): TJmFloat;
function Sec(X: TJmFloat): TJmFloat;
function Sin(X: TJmFloat): TJmFloat;
procedure SinCos(X: TJmFloat; var Sin, Cos: TJmFloat);
function Tan(X: TJmFloat): TJmFloat;
function Versine(X: TJmFloat): TJmFloat;


{ Hyperbolic }

function ArcCosH(X: TJmFloat): TJmFloat;
function ArcCotH(X: TJmFloat): TJmFloat;
function ArcCscH(X: TJmFloat): TJmFloat;
function ArcSecH(X: TJmFloat): TJmFloat;
function ArcSinH(X: TJmFloat): TJmFloat;
function ArcTanH(X: TJmFloat): TJmFloat;
function CosH(X: TJmFloat): TJmFloat;
function CotH(X: TJmFloat): TJmFloat;
function CscH(X: TJmFloat): TJmFloat;
function SecH(X: TJmFloat): TJmFloat;
function SinH(X: TJmFloat): TJmFloat;
function TanH(X: TJmFloat): TJmFloat;

{ Coordinate conversion }

function DegMinSecToFloat(const Degs, Mins, Secs: TJmFloat): TJmFloat; // obsolete (see JclUnitConv)
procedure FloatToDegMinSec(const X: TJmFloat; var Degs, Mins, Secs: TJmFloat); // obsolete (see JclUnitConv)

{ Exponential }

function Exp(const x: TJmFloat): TJmFloat;
function Power(const Base, Exponent: TJmFloat): TJmFloat;
function PowerInt(const X: TJmFloat; N: Integer): TJmFloat;
function TenToY(const Y: TJmFloat): TJmFloat;
function TruncPower(const Base, Exponent: TJmFloat): TJmFloat;
function TwoToY(const Y: TJmFloat): TJmFloat;


{ Floating point support routines }

function IsFloatZero(const X: TJmFloat): Boolean;
function FloatsEqual(const X, Y: TJmFloat): Boolean;
function MaxFloat(const X, Y: TJmFloat): TJmFloat;
function MinFloat(const X, Y: TJmFloat): TJmFloat;
function ModFloat(const X, Y: TJmFloat): TJmFloat;
function RemainderFloat(const X, Y: TJmFloat): TJmFloat;
function SetPrecisionTolerance(NewTolerance: TJmFloat): TJmFloat;
procedure SwapFloats(var X, Y: TJmFloat);
procedure CalcMachineEpsSingle;
procedure CalcMachineEpsDouble;
procedure CalcMachineEpsExtended;
procedure CalcMachineEps;
procedure SetPrecisionToleranceToEpsilon;

{ Miscellaneous }

function Ackermann(const A, B: Integer): Integer;
function Ceiling(const X: TJmFloat): Integer;
function Factorial(const N: Integer): TJmFloat;
function Fibonacci(const N: Integer): Integer;
function Floor(const X: TJmFloat): Integer;
function GCD(const X, Y: Cardinal): Cardinal;
function ISqrt(const I: Smallint): Smallint;
function LCM(const X, Y: Cardinal): Cardinal;
function NormalizeAngle(const Angle: TJmFloat): TJmFloat;
function Pythagoras(const X, Y: TJmFloat): TJmFloat;
function Hypot(const O, A : TJmFloat) : TJmFloat;
function Sgn(const X: TJmFloat): Integer;
function Signe(const X, Y: TJmFloat): TJmFloat;

{ Prime numbers }

function IsRelativePrime(const X, Y: Cardinal): Boolean;
function IsPrimeTD(N: Cardinal): Boolean;
function IsPrimeRM(N: Cardinal): Boolean;
function IsPrimeFactor(const F, N: Cardinal): Boolean;
function PrimeFactors(N: Cardinal): TDynCardinalArray;

var
  IsPrime: function(N: Cardinal): Boolean = IsPrimeTD;

procedure SetPrimalityTest(const Method: TPrimalityTestMethod);

{ Floating point value classification }

type
  TFloatingPointClass =
   (
    fpZero,     // zero
    fpNormal,   // normal finite <> 0
    fpDenormal, // denormalized finite
    fpInfinite, // infinite
    fpNaN,      // not a number
    fpInvalid   // unsupported floating point format
   );

function FloatingPointClass(const Value: Single): TFloatingPointClass; overload;
function FloatingPointClass(const Value: Double): TFloatingPointClass; overload;
function FloatingPointClass(const Value: Extended): TFloatingPointClass; overload;

{ NaN and INF support }

type
  TNaNTag = -$3FFFFF..$3FFFFE;

const
  Infinity    = 1/0;       // tricky
  NaN         = 0/0;       // tricky
  NegInfinity = -Infinity;


function IsInfinite(const Value: Single): Boolean; overload;
function IsInfinite(const Value: Double): Boolean; overload;
function IsInfinite(const Value: Extended): Boolean; overload;

function IsNaN(const Value: Single): Boolean; overload;
function IsNaN(const Value: Double): Boolean; overload;
function IsNaN(const Value: Extended): Boolean; overload;

function IsSpecialValue(const X: TJmFloat): Boolean;

procedure MakeQuietNaN(var X: Single; Tag: TNaNTag = 0); overload;
procedure MakeQuietNaN(var X: Double; Tag: TNaNTag = 0); overload;
procedure MakeQuietNaN(var X: Extended; Tag: TNaNTag = 0); overload;

procedure MakeSignalingNaN(var X: Single; Tag: TNaNTag = 0); overload;
procedure MakeSignalingNaN(var X: Double; Tag: TNaNTag = 0); overload;
procedure MakeSignalingNaN(var X: Extended; Tag: TNaNTag = 0); overload;

{ Mine*Buffer fills "Buffer" with consecutive tagged signaling NaNs.

  This allows for real number arrays which enforce initialization: any attempt
  to load an uninitialized array element into the FPU will raise an exception
  either of class EInvalidOp (Windows 9x/ME) or EJmNaNSignal (Windows NT).

  Under Windows NT it is thus possible to derive the violating array index from
  the EJmNaNSignal object's Tag property. }

procedure MineSingleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag = 0);
procedure MineDoubleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag = 0);

function MinedSingleArray(Length: Integer): TDynSingleArray;
function MinedDoubleArray(Length: Integer): TDynDoubleArray;

function GetNaNTag(const NaN: Single): TNaNTag; overload;
function GetNaNTag(const NaN: Double): TNaNTag; overload;
function GetNaNTag(const NaN: Extended): TNaNTag; overload;

{ Set support }

type
  TJmASet = class (TObject)
  protected
    function GetBit(const Idx: Integer): Boolean; virtual; abstract;
    procedure SetBit(const Idx: Integer; const Value: Boolean); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Invert; virtual; abstract;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; virtual; abstract;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); virtual; abstract;
  end;

type
  TJmFlatSet = class (TJmASet)
  private
    FBits: TBits;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetBit(const Idx: Integer): Boolean; override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
  end;

type
  TPointerArray = array [0..MaxLongint div 256] of Pointer;
  PPointerArray = ^TPointerArray;
  TDelphiSet = set of Byte; // 256 elements
  PDelphiSet = ^TDelphiSet;

const
  EmptyDelphiSet: TDelphiSet = [];
  CompleteDelphiSet: TDelphiSet = [0..255];

type
  TJmSparseFlatSet = class (TJmASet)
  private
    FSetList: PPointerArray;
    FSetListEntries: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    function GetBit(const Idx: Integer): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
  end;

{ Rational numbers }

type
  TJmRational = class (TObject)
  private
    FT: Integer;
    FN: Integer;
    function GetAsString: string;
    procedure SetAsString(const S: string);
    function GetAsFloat: TJmFloat;
    procedure SetAsFloat(const R: TJmFloat);
  protected
    procedure Simplify;
  public
    constructor Create; overload;
    constructor Create(const R: TJmFloat); overload;
    constructor Create(const Numerator: Integer; const Denominator: Integer = 1); overload;

    property Numerator: Integer read FT;
    property Denominator: Integer read FN;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: TJmFloat read GetAsFloat write SetAsFloat;

    procedure Assign(const R: TJmRational); overload;
    procedure Assign(const R: TJmFloat); overload;
    procedure Assign(const Numerator: Integer; const Denominator: Integer = 1); overload;

    procedure AssignZero;
    procedure AssignOne;
    function Duplicate: TJmRational;

    function IsEqual(const R: TJmRational): Boolean; reintroduce; overload;
    function IsEqual(const Numerator: Integer; const Denominator: Integer = 1) : Boolean; reintroduce; overload;
    function IsEqual(const R: TJmFloat): Boolean; reintroduce; overload;

    function IsZero: Boolean;
    function IsOne: Boolean;

    procedure Add(const R: TJmRational); overload;
    procedure Add(const V: TJmFloat); overload;
    procedure Add(const V: Integer); overload;

    procedure Subtract(const R: TJmRational); overload;
    procedure Subtract(const V: TJmFloat); overload;
    procedure Subtract(const V: Integer); overload;

    procedure Negate;
    procedure Abs;
    function Sgn: Integer;

    procedure Multiply(const R: TJmRational); overload;
    procedure Multiply(const V: TJmFloat); overload;
    procedure Multiply(const V: Integer); overload;

    procedure Reciprocal;

    procedure Divide(const R: TJmRational); overload;
    procedure Divide(const V: TJmFloat); overload;
    procedure Divide(const V: Integer); overload;

    procedure Sqrt;
    procedure Sqr;

    procedure Power(const R: TJmRational); overload;
    procedure Power(const V: Integer); overload;
    procedure Power(const V: TJmFloat); overload;
  end;


type
  EJmMathError = class (Exception)
  public
    constructor CreateResRec(ResStringRec: PResStringRec);
    constructor CreateResRecFmt(ResStringRec: PResStringRec; const Args: array of const);
  end;


  EJmNaNSignal = class (EJmMathError)
  private
    FTag: TNaNTag;
  public
    constructor Create(ATag: TNaNTag);
    property Tag: TNaNTag read FTag;
  end;

procedure DomainCheck(Err: Boolean);

{ CRC }

function Crc16_P(X: PByteArray; N: Integer; Crc: Word = 0): Word;
function Crc16(const X: array of Byte; N: Integer; Crc: Word = 0): Word;
function Crc16_A(const X: array of Byte; Crc: Word = 0): Word;

function CheckCrc16_P(X: PByteArray; N: Integer; Crc: Word): Integer;
function CheckCrc16(var X: array of Byte; N: Integer; Crc: Word): Integer;
function CheckCrc16_A(var X: array of Byte; Crc: Word): Integer;

function Crc32_P(X: PByteArray; N: Integer; Crc: Cardinal = 0): Cardinal;
function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal = 0): Cardinal;
function Crc32_A(const X: array of Byte; Crc: Cardinal = 0): Cardinal;

function CheckCrc32_P(X: PByteArray; N: Integer; Crc: Cardinal): Integer;
function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
function CheckCrc32_A(var X: array of Byte; Crc: Cardinal): Integer;

{$IFDEF CRCINIT}
procedure InitCrc32 (Polynom, Start: Cardinal);
procedure InitCrc16 (Polynom, Start: Word);
{$ENDIF}

implementation
uses
  Jm8087;
//  Jcl8087, JclResources, JclUnitConv;

//==================================================================================================
// Internal helper routines
//==================================================================================================

// to be independent from JclLogic

function Min(const X, Y: Integer): Integer;
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

//--------------------------------------------------------------------------------------------------

// to be independent from JCLLogic

procedure SwapOrd(var X, Y: Integer);
var
  Temp: Integer;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

//--------------------------------------------------------------------------------------------------

function DoubleToHex(const D: Double): string;
var
  Overlay: array [1..2] of LongInt absolute D;
begin
  // Look at element 2 before element 1 because of "Little Endian" order.
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;

//--------------------------------------------------------------------------------------------------

function HexToDouble(const Hex: string): Double;
var
  D: Double;
  Overlay: array [1..2] of LongInt absolute D;
begin
  if Length(Hex) <> 16 then
    raise EJmMathError.CreateResRec(@RsUnexpectedValue);
  Overlay[1] := StrToInt('$' + Copy(Hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(Hex, 1, 8));
  Result := D;
end;

//--------------------------------------------------------------------------------------------------

const
  _180: Integer = 180;
  _200: Integer = 200;

// Converts degrees to radians. Expects degrees in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 180

procedure FDegToRad; assembler;
asm
        FLDPI
        FIDIV   [_180]
        FMUL
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

// Converts radians to degrees. Expects radians in ST(0), leaves degrees in ST(0)
// ST(0) := ST(0) * (180 / PI);

procedure FRadToDeg; assembler;
asm
        FLD1
        FLDPI
        FDIV
        FLD     [_180]
        FMUL
        FMUL
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

// Converts grads to radians. Expects grads in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 200

procedure FGradToRad; assembler;
asm
        FLDPI
        FIDIV   [_200]
        FMUL
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

// Converts radians to grads. Expects radians in ST(0), leaves grads in ST(0)
// ST(0) := ST(0) * (200 / PI);

procedure FRadToGrad; assembler;
asm
        FLD1
        FLDPI
        FDIV
        FLD     [_200]
        FMUL
        FMUL
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

procedure DomainCheck(Err: Boolean);
begin
  if Err then
    raise EJmMathError.CreateResRec(@RsMathDomainError);
end;

//==================================================================================================
// Logarithmic
//==================================================================================================

function LogBase10(X: TJmFloat): TJmFloat;

  function FLogBase10(X: TJmFloat): TJmFloat; assembler;
  asm
          FLDLG2
          FLD     X
          FYL2X
          FWAIT
  end;

begin
  DomainCheck(X <= 0.0);
  Result := FLogBase10(X);
end;

function log10(X: TJmFloat): TJmFloat;
begin
  result := LogBase10(X);
end;

//--------------------------------------------------------------------------------------------------

function LogBase2(X: TJmFloat): TJmFloat;

  function FLogBase2(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD1
          FLD     X
          FYL2X
          FWAIT
  end;

begin
  DomainCheck(X <= 0.0);
  Result := FLogBase2(X);
end;

//--------------------------------------------------------------------------------------------------

function LogBaseN(Base, X: TJmFloat): TJmFloat;

  function FLogBaseN(Base, X: TJmFloat): TJmFloat; assembler;
  asm
          FLD1
          FLD     X
          FYL2X
          FLD1
          FLD     Base
          FYL2X
          FDIV
          FWAIT
  end;

begin
  DomainCheck((X <= 0.0) or (Base <= 0.0) or (Base = 1.0));
  Result := FLogBaseN(Base, X);
end;

function logn(Base, X: TJmFloat): TJmFloat;
begin
  result := LogBaseN( Base, X );
end;

function log2( X: TJmFloat): TJmFloat;
begin
  result := logn(2, X);
end;

function ln( X: TJmFloat): TJmFloat;
begin
  result := logn(E, X);
end;

function lnxp1( X: TJmFloat): TJmFloat;
begin
  result := ln(X + 1);
end;

//===================================================================================================
// Transcendental
//===================================================================================================

function ArcCos(X: TJmFloat): TJmFloat;

  function FArcCos(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBRP  ST(1), ST
          FSQRT
          FXCH
          FPATAN
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > 1.0);
  Result := FArcCos(X);
end;

//--------------------------------------------------------------------------------------------------

function ArcCot(X: TJmFloat): TJmFloat;
begin
  DomainCheck(X = 0);
  Result := ArcTan(1 / X);
end;

//--------------------------------------------------------------------------------------------------

function ArcCsc(X: TJmFloat): TJmFloat;
begin
  Result := arcsec(x / sqrt(x * x -1));
end;

//--------------------------------------------------------------------------------------------------

function ArcSec(X: TJmFloat): TJmFloat;

  function FArcTan(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FLD1
          FPATAN
          FWAIT
  end;

begin
  Result := FArcTan(sqrt(x*x - 1));
end;

//--------------------------------------------------------------------------------------------------
       
function ArcSin(X: TJmFloat): TJmFloat;

  function FArcSin(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBRP  ST(1), ST
          FSQRT
          FPATAN
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > 1.0);
  Result := FArcSin(X);
end;

//--------------------------------------------------------------------------------------------------

function ArcTan(X: TJmFloat): TJmFloat; assembler;
{$IFDEF PUREPASCAL}
begin
  Result := ArcTan2(X, 1);
end;
{$ELSE}
asm
        FLD     X
        FLD1
        FPATAN
        FWAIT
end;
{$ENDIF DEF PUREPASCAL}
{ EJmMathError }

constructor EJmMathError.CreateResRec(ResStringRec: PResStringRec);
begin
  {$IFDEF FPC}
  inherited Create(ResStringRec^);
  {$ELSE FPC}
  inherited Create(LoadResString(ResStringRec));
  {$ENDIF FPC}
end;

constructor EJmMathError.CreateResRecFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  {$IFDEF FPC}
  inherited CreateFmt(ResStringRec^, Args);
  {$ELSE FPC}
  inherited CreateFmt(LoadResString(ResStringRec), Args);
  {$ENDIF FPC}
end;


//--------------------------------------------------------------------------------------------------

function ArcTan2(Y, X: TJmFloat): TJmFloat; assembler;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

function Cos(X: TJmFloat): TJmFloat;

  function FCos(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FCOS
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > MaxAngle);
  Result := FCos(X);
end;

//--------------------------------------------------------------------------------------------------

function Cot(X: TJmFloat): TJmFloat;

  function FCot(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FPTAN
          FDIVRP
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > MaxAngle);
  // TODO Cot = 1 / Tan -> Tan(X) <> 0.0
  Result := FCot(X);
end;

function cotan(X: TJmFloat): TJmFloat;
begin
  result := Cot(X);
end;

//--------------------------------------------------------------------------------------------------

function Coversine(X: TJmFloat): TJmFloat;
begin
  Result := 1 - sin(x);
end;

//--------------------------------------------------------------------------------------------------

function Csc(X: TJmFloat): TJmFloat;
var
  Y: TJmFloat;
begin
  DomainCheck(Abs(X) > MaxAngle);

  Y := Sin(X);
  DomainCheck(Y = 0.0);
  Result := 1.0 / Y;
end;

//--------------------------------------------------------------------------------------------------

function Exsecans(X: TJmFloat): TJmFloat;
begin
  Result := sec(x) - 1;
end;

//--------------------------------------------------------------------------------------------------

function Haversine(X: TJmFloat): TJmFloat;
begin
  Result := 0.5 * (1 - cos(x)) ;
end;

//--------------------------------------------------------------------------------------------------

function Sec(X: TJmFloat): TJmFloat;

  function FSec(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FCOS
          FLD1
          FDIVRP
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > MaxAngle);
  // TODO Sec = 1 / Cos -> Cos(X) <> 0!
  Result := FSec(X);
end;

//--------------------------------------------------------------------------------------------------

function Sin(X: TJmFloat): TJmFloat;

  function FSin(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FSIN
          FWAIT
  end;

begin
  {$IFNDEF MATH_EXT_SPECIALVALUES}
  DomainCheck(Abs(X) > MaxAngle);
  {$ENDIF}

  Result := FSin(X);
end;

//--------------------------------------------------------------------------------------------------

procedure SinCos(X: TJmFloat; var Sin, Cos: TJmFloat);

  procedure FSinCos(X: TJmFloat; var Sin, Cos: TJmFloat); assembler;
  asm
          FLD     X
          FSINCOS
          FSTP    TByte PTR [EDX]
          FSTP    TByte PTR [EAX]
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > MaxAngle);
  FSinCos(X, Sin, Cos);
end;

//--------------------------------------------------------------------------------------------------

function Tan(X: TJmFloat): TJmFloat;

  function FTan(X: TJmFloat): TJmFloat; assembler;
  asm
          FLD     X
          FPTAN
          FSTP    ST(0)
          FWAIT
  end;

begin
  DomainCheck(Abs(X) > MaxAngle);
  Result := FTan(X);
end;

//--------------------------------------------------------------------------------------------------

function Versine(X: TJmFloat): TJmFloat;
begin
  Result := 1 - cos(x);
end;

//===================================================================================================
// Hyperbolic
//===================================================================================================

function ArcCosH(X: TJmFloat): TJmFloat;

  function FArcCosH(X: TJmFloat): TJmFloat; assembler;
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBP   ST(1), ST
          FSQRT
          FADDP   ST(1), ST
          FYL2X
  end;

begin
  DomainCheck(X < 1.0);
  Result := FArcCosH(X);
end;

//--------------------------------------------------------------------------------------------------

function ArcCotH(X: TJmFloat): TJmFloat;
begin
  DomainCheck(Abs(X) = 1.0);
  Result := 0.5 * Ln((X + 1.0) / (X - 1.0));
end;

//--------------------------------------------------------------------------------------------------

function ArcCscH(X: TJmFloat): TJmFloat;
begin
  DomainCheck(X = 0);
  Result := Ln((Sgn(X) * Sqrt(Sqr(X) + 1.0) + 1.0) / X);
end;

//--------------------------------------------------------------------------------------------------

function ArcSecH(X: TJmFloat): TJmFloat;
begin
  DomainCheck(Abs(X) > 1.0);
  Result := Ln((Sqrt(1.0 - Sqr(X)) + 1.0) / X);
end;

//--------------------------------------------------------------------------------------------------

function ArcSinH(X: TJmFloat): TJmFloat; assembler;
asm
        FLDLN2
        FLD     X
        FLD     ST(0)
        FMUL    ST(0), ST
        FLD1
        FADDP   ST(1), ST
        FSQRT
        FADDP   ST(1), ST
        FYL2X
end;

//--------------------------------------------------------------------------------------------------

function ArcTanH(X: TJmFloat): TJmFloat;

  function FArcTanH(X: TJmFloat): TJmFloat; assembler;
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FLD1
          FADDP   ST(1), ST
          FXCH
          FLD1
          FSUBRP  ST(1), ST
          FDIVP   ST(1), ST
          FSQRT
          FYL2X
          FWAIT
  end;

begin
  DomainCheck(Abs(X) >= 1.0);
  Result := FArcTanH(X);
end;

//--------------------------------------------------------------------------------------------------

function CosH(X: TJmFloat): TJmFloat;
begin
  Result := 0.5 * (Exp(X) + Exp(-X));
end;

//--------------------------------------------------------------------------------------------------

function CotH(X: TJmFloat): TJmFloat; assembler;
const
  RoundDown: Word = $177F;
  OneHalf: TJmFloat = 0.5;
var
  ControlWW: Word;
asm
        FLD     X  // TODO Legal values for X?
        FLDL2E
        FMULP   ST(1), ST
        FSTCW   ControlWW
        FLDCW   RoundDown
        FLD     ST(0)
        FRNDINT
        FLDCW   ControlWW
        FXCH
        FSUB    ST, ST(1)
        F2XM1
        FLD1
        FADDP   ST(1), ST
        FSCALE
        FST     ST(1)
        FLD1
        FDIVRP  ST(1), ST
        FADDP   ST(1), ST
        FLD     OneHalf
        FMULP   ST(1), ST
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

function CscH(X: TJmFloat): TJmFloat;
begin
  Result := Exp(X) - Exp(-X);
  DomainCheck(Result = 0.0);
  Result := 2.0 / Result;
end;

//--------------------------------------------------------------------------------------------------

function SecH(X: TJmFloat): TJmFloat;
begin
  Result := Exp(X) + Exp(-X);
  DomainCheck(Result = 0.0);
  Result := 2.0 / Result;
end;

//--------------------------------------------------------------------------------------------------

function SinH(X: TJmFloat): TJmFloat; assembler;
const
  RoundDown: Word = $177F;
  OneHalf: TJmFloat = 0.5;
var
  ControlWW: Word;
asm
        FLD     X // TODO Legal values for X?
        FLDL2E
        FMULP   ST(1), ST
        FSTCW   ControlWW
        FLDCW   RoundDown
        FLD     ST(0)
        FRNDINT
        FLDCW   ControlWW
        FXCH
        FSUB    ST, ST(1)
        F2XM1
        FLD1
        FADDP   ST(1), ST
        FSCALE
        FST     ST(1)
        FLD1
        FDIVRP  ST(1), ST
        FSUBP   ST(1), ST
        FLD     OneHalf
        FMULP   ST(1), ST
        FWAIT
end;

//--------------------------------------------------------------------------------------------------

function TanH(X: TJmFloat): TJmFloat;
begin
  if X > MaxTanH then
    Result := 1.0
  else
  begin
    if X < -MaxTanH then
      Result := -1.0
    else
    begin
      Result := Exp(X);
      Result := Result * Result;
      Result := (Result - 1.0) / (Result + 1.0);
    end;
  end;
end;

//===================================================================================================
// Coordinate conversion
//===================================================================================================

function DegMinSecToFloat(const Degs, Mins, Secs: TJmFloat): TJmFloat; // obsolete
begin
  Result := Degs + (Mins / 60.0) + (Secs / 3600.0);
end;

//--------------------------------------------------------------------------------------------------

procedure FloatToDegMinSec(const X: TJmFloat; var Degs, Mins, Secs: TJmFloat); // obsolete
var
  Y: TJmFloat;
begin
  Degs := System.Int(X);
  Y := Frac(X) * 60;
  Mins := System.Int(Y);
  Secs := Frac(Y) * 60;
end;

//===================================================================================================
// Exponential
//===================================================================================================

function Exp(const x: TJmFloat): TJmFloat;
begin
{$IFDEF MATH_EXT_EXTREMEVALUES}
  if IsSpecialValue(X) then
  begin
    if IsNaN(X) or (X = Infinity) then
      Result := X
    else
      Result := 0;
    Exit;
  end;
{$ENDIF}

  Result := System.Exp(x);
end;

//--------------------------------------------------------------------------------------------------

function Power(const Base, Exponent: TJmFloat) : TJmFloat;
var
  IsAnInteger, IsOdd: Boolean;

begin
  if (Exponent = 0.0) or (Base = 1.0) then
    Result := 1
  else
    if (Base = 0.0) then
    begin
      if (Exponent > 0.0) then
        Result := 0.0
      else
        {$IFDEF MATH_EXT_EXTREMEVALUES}
        Result := Infinity; 
        {$ELSE}
        raise EJmMathError.Create('Power function: Result is infinite');
        {$ENDIF}
  end
  else
  if (Base > 0.0) then
    Result := Exp(Exponent * ln(Base))
  else
  begin
    IsAnInteger := (Frac(Exponent) = 0.0);
    if IsAnInteger then
    begin
      Result := exp(Exponent * ln(abs(Base)));
      IsOdd := abs(round(ModFloat(Exponent, 2))) = 1;
      if IsOdd then
        Result := -result;
    end
    else
      raise EJmMathError.Create('Power function: Result is complex');
  end;
end;

//--------------------------------------------------------------------------------------------------

function PowerInt(const X: TJmFloat; N: Integer): TJmFloat;
var
  M: Integer;
  T: TJmFloat;
  Xc: TJmFloat;
begin
  if X = 0.0 then
  begin
    if N = 0 then
      Result := 1.0
    else
    if N > 0 then
      Result := 0.0
    else
      Result := MaxFloatingPoint;
    Exit;
  end;

  if N = 0 then
  begin
    Result := 1.0;
    Exit;
  end;

  // Legendre's algorithm for minimizing the number of multiplications

  T := 1.0;
  M := Abs(N);
  Xc := X;
  repeat
    if Odd(M) then
    begin
      Dec(M);
      T := T * Xc;
    end
    else
    begin
      M := M div 2;
      Xc := Sqr(Xc);
    end;
  until M = 0;

  if N > 0 then
    Result := T
  else
    Result := 1.0 / T;
end;

//--------------------------------------------------------------------------------------------------

function TenToY(const Y: TJmFloat): TJmFloat;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := Exp(Y * kLn10);
end;

//--------------------------------------------------------------------------------------------------

function TruncPower(const Base, Exponent: TJmFloat): TJmFloat;
begin
  if Base > 0 then
    Result := Power(Base, Exponent)
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function TwoToY(const Y: TJmFloat): TJmFloat;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := Exp(Y * kLn2);
end;

//===================================================================================================
// Floating point support routines
//===================================================================================================

function IsFloatZero(const X: TJmFloat): Boolean;
begin
  Result := Abs(X) < PrecisionTolerance;
end;

//--------------------------------------------------------------------------------------------------

function FloatsEqual(const X, Y: TJmFloat): Boolean;
begin
  try
    if Y = 0 then
      // catch exact equality
      Result := (X = Y) or (Abs(1 - Y/X ) <= PrecisionTolerance)
    else
      // catch exact equality
      Result := (X = Y) or (Abs(1 - X/Y ) <= PrecisionTolerance);
  except
    Result := False;  // catch real rare overflow e.g.  1.0e3000/1.0e-3000
  end
end;

//--------------------------------------------------------------------------------------------------

function MaxFloat(const X, Y: TJmFloat): TJmFloat;
begin
  if X < Y then
    Result := Y
  else
    Result := X;
end;

//--------------------------------------------------------------------------------------------------

function MinFloat(const X, Y: TJmFloat): TJmFloat;
begin
  if X > Y then
    Result := Y
  else
    Result := X;
end;

//--------------------------------------------------------------------------------------------------

function ModFloat(const X, Y: TJmFloat): TJmFloat;
var
  Z: TJmFloat;
begin
  Result := X / Y;
  Z := System.Int(Result);
  if Frac(Result) < 0.0 then
    Z := Z - 1.0;
  Result := X - Y * Z;
end;

//--------------------------------------------------------------------------------------------------

function RemainderFloat(const X, Y: TJmFloat): TJmFloat;
begin
  Result := X - System.Int(X / Y) * Y;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapFloats(var X, Y: TJmFloat);
var
  T: TJmFloat;
begin
  T := X;
  X := Y;
  Y := T;
end;

//--------------------------------------------------------------------------------------------------

procedure CalcMachineEpsSingle;
var
  One: Single;
  T: Single;
begin
  One := 1.0;
  EpsSingle := One;
  repeat
    EpsSingle := 0.5 * EpsSingle;
    T := One + EpsSingle;
  until One = T;
  EpsSingle := 2.0 * EpsSingle;
  ThreeEpsSingle := 3.0 * EpsSingle;
end;

//--------------------------------------------------------------------------------------------------

procedure CalcMachineEpsDouble;
var
  One: Double;
  T: Double;
begin
  One := 1.0;
  EpsDouble := One;
  repeat
    EpsDouble := 0.5 * EpsDouble;
    T := One + EpsDouble;
  until One = T;
  EpsDouble := 2.0 * EpsDouble;
  ThreeEpsDouble := 3.0 * EpsDouble;
end;

//--------------------------------------------------------------------------------------------------

procedure CalcMachineEpsExtended;
var
  One: Extended;
  T: Extended;
begin
  One := 1.0;
  EpsExtended := One;
  repeat
    EpsExtended := 0.5 * EpsExtended;
    T := One + EpsExtended;
  until One = T;
  EpsExtended := 2.0 * EpsExtended;
  ThreeEpsExtended := 3.0 * EpsExtended;
end;

//--------------------------------------------------------------------------------------------------

procedure CalcMachineEps;
begin
  {$IFDEF MATH_EXTENDED_PRECISION}
  CalcMachineEpsExtended;
  Epsilon := EpsExtended;
  ThreeEpsilon := ThreeEpsExtended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  CalcMachineEpsDouble;
  Epsilon := EpsDouble;
  ThreeEpsilon := ThreeEpsDouble;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  CalcMachineEpsSingle;
  Epsilon := EpsSingle;
  ThreeEpsilon := ThreeEpsSingle;
  {$ENDIF MATH_SINGLE_PRECISION}
end;

//--------------------------------------------------------------------------------------------------

procedure SetPrecisionToleranceToEpsilon;
begin
  CalcMachineEps;
  PrecisionTolerance := Epsilon;
end;

//--------------------------------------------------------------------------------------------------

function SetPrecisionTolerance(NewTolerance: TJmFloat): TJmFloat;
begin
  Result := PrecisionTolerance;
  PrecisionTolerance := NewTolerance;
end;

//===================================================================================================
// Miscellaneous
//===================================================================================================

function Ceiling(const X: TJmFloat): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

//--------------------------------------------------------------------------------------------------

const
  PreCompFactsCount = 33; // all factorials that fit in a Single

  {$IFDEF jmSinglePrecision}
  PreCompFacts: array [0..PreCompFactsCount] of TJmFloat =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178289152.0,
    1307674279936.0,
    20922788478976.0,
    355687414628352.0,
    6.4023735304192E15,
    1.21645096004223E17,
    2.43290202316367E18,
    5.10909408371697E19,
    1.12400072480601E21,
    2.58520174445945E22,
    6.20448454699065E23,
    1.55112110792462E25,
    4.03291499589617E26,
    1.08888704151327E28,
    3.04888371623715E29,
    8.8417630793192E30,
    2.65252889961724E32,
    8.22283968552752E33,
    2.63130869936881E35,
    8.68331850984666E36
   );
  {$ENDIF jmSinglePrecision}
  {$IFDEF jmDoublePrecision}
  PreCompFacts: array [0..PreCompFactsCount] of TJmFloat =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6.402373705728E15,
    1.21645100408832E17,
    2.43290200817664E18,
    5.10909421717094E19,
    1.12400072777761E21,
    2.5852016738885E22,
    6.20448401733239E23,
    1.5511210043331E25,
    4.03291461126606E26,
    1.08888694504184E28,
    3.04888344611714E29,
    8.8417619937397E30,
    2.65252859812191E32,
    8.22283865417792E33,
    2.63130836933694E35,
    8.68331761881189E36
   );
  {$ENDIF jmDoublePrecision}
  {$IFDEF jmExtendedPrecision}
  PreCompFacts: array [0..PreCompFactsCount] of TJmFloat =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6.402373705728E15,
    1.21645100408832E17,
    2.43290200817664E18,
    5.10909421717094E19,
    1.12400072777761E21,
    2.5852016738885E22,
    6.20448401733239E23,
    1.5511210043331E25,
    4.03291461126606E26,
    1.08888694504184E28,
    3.04888344611714E29,
    8.8417619937397E30,
    2.65252859812191E32,
    8.22283865417792E33,
    2.63130836933694E35,
    8.68331761881189E36
   );
  {$ENDIF jmExtendedPrecision}

//--------------------------------------------------------------------------------------------------

function Factorial(const N: Integer): TJmFloat;
var
  I: Integer;
begin
  if (N < 0) or (N > MaxFactorial) then
    Result := 0.0
  else
  begin
    if N <= PreCompFactsCount then
      Result := PreCompFacts[N]
    else
    begin // TODO Change following by: Gamma(N + 1)
      Result := PreCompFacts[PreCompFactsCount];
      for I := PreCompFactsCount + 1 to N do
        Result := Result * I;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function Floor(const X: TJmFloat): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    dec(Result);
end;

//--------------------------------------------------------------------------------------------------

function GCD(const X, Y: Cardinal): Cardinal; assembler;
{ Euclid's algorithm }
asm
        JMP     @01      // We start with EAX <- X, EDX <- Y, and check to see if Y=0
@00:
        MOV     ECX, EDX // ECX <- EDX prepare for division
        XOR     EDX, EDX // clear EDX for Division
        DIV     ECX      // EAX <- EDX:EAX div ECX, EDX <- EDX:EAX mod ECX
        MOV     EAX, ECX // EAX <- ECX, and repeat if EDX <> 0
@01:
        AND     EDX, EDX // test to see if EDX is zero, without changing EDX
        JNZ     @00      // when EDX is zero EAX has the Result
end;

//--------------------------------------------------------------------------------------------------

function ISqrt(const I: Smallint): Smallint; assembler;
asm
        PUSH    EBX

        MOV     CX, AX  // load argument
        MOV     AX, -1  // init Result
        CWD             // init odd numbers to -1
        XOR     BX, BX  // init perfect squares to 0
@LOOP:
        INC     AX      // increment Result
        INC     DX      // compute
        INC     DX      // next odd number
        ADD     BX, DX  // next perfect square
        CMP     BX, CX  // perfect square > argument ?
        JBE     @LOOP   // until square greater than argument

        POP     EBX
end;

//--------------------------------------------------------------------------------------------------

function LCM(const X, Y: Cardinal): Cardinal;
var
  e: Cardinal;

begin
  e := GCD(X, Y);
  if e > 0 then
    Result := (X div e) * Y
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function NormalizeAngle(const Angle: TJmFloat): TJmFloat;
begin
  Result := Angle;
  {$IFDEF MATH_ANGLE_DEGREES}
  Result := DegToRad(Result);
  {$ENDIF MATH_ANGLE_DEGREES}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := GradToRad(Result);
  {$ENDIF MATH_ANGLE_GRADS}

  Result := Frac(Result * kInv2Pi);
  if Result < -0.5 then
    Result := Result + 1.0
  else
  if Result >= 0.5 then
    Result := Result - 1.0;

  Result := Result * kTwoPi;

  {$IFDEF MATH_ANGLE_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF MATH_ANGLE_DEGREES}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF MATH_ANGLE_GRADS}
end;

//--------------------------------------------------------------------------------------------------

function Pythagoras(const X, Y: TJmFloat): TJmFloat;
var
  AbsX, AbsY: TJmFloat;
begin
  AbsX := Abs(X);
  AbsY := Abs(Y);

  if AbsX > AbsY then
    Result := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
  else
  if AbsY = 0.0 then
    Result := 0.0
  else
    Result := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
end;

function Hypot(const O, A : TJmFloat) : TJmFloat;
begin
  result := Sqrt( Sqr(O) + Sqr(A));
end;

//--------------------------------------------------------------------------------------------------

function Sgn(const X: TJmFloat): Integer;
begin
  if X > 0.0 then
    Result := 1
  else
  if X < 0.0 then
    Result := -1
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function Signe(const X, Y: TJmFloat): TJmFloat;
begin
  if X > 0.0 then
  begin
    if Y > 0.0 then
      Result := X
    else
      Result := -X;
  end
  else
  begin
    if Y < 0.0 then
      Result := X
    else
      Result := -X;
  end;
end;

//--------------------------------------------------------------------------------------------------

function Ackermann(const A, B: Integer): Integer;
begin
  if a = 0 then
  begin
    Result := b + 1;
    Exit;
  end;

  if b = 0 then
    Result := Ackermann(a-1, 1)
  else
    Result := Ackermann(a-1, Ackermann(a, b-1));
end;

//--------------------------------------------------------------------------------------------------

function Fibonacci(const N: Integer): Integer;
var
  i: Integer;
  p1, p2: Integer;

begin
  Assert(N >= 0);
  Result := 0;
  p1 := 1;
  p2 := 1;

  if (n = 1) or (n = 2) then
  begin
    Result := 1;
    Exit;
  end;

  for i := 3 to N do
  begin
    Result := p1 + p2;
    p1 := p2;
    p2 := Result;
  end;
end;

//===================================================================================================
// Set support
//===================================================================================================

constructor TJmFlatSet.Create;
begin
  inherited Create;
  FBits := TBits.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJmFlatSet.Destroy;
begin
  FBits.Free;
  FBits := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmFlatSet.Clear;
begin
  FBits.Size := 0;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmFlatSet.Invert;
var
  I: Integer;
begin
  for I := 0 to FBits.Size - 1 do
    FBits[I] := not FBits[I];
end;

//--------------------------------------------------------------------------------------------------

procedure TJmFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I: Integer;
begin
  for I := High downto Low do
    FBits[I] := Value;
end;

//--------------------------------------------------------------------------------------------------

function TJmFlatSet.GetBit(const Idx: Integer): Boolean;
begin
  Result := FBits[Idx];
end;

//--------------------------------------------------------------------------------------------------

function TJmFlatSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I: Integer;
begin
  if not Value and (High >= FBits.Size) then
  begin
    Result := False;
    Exit;
  end;
  for I := Low to Min(High, FBits.Size - 1) do
    if FBits[I] <> Value then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
begin
  FBits[Idx] := Value;
end;

//--------------------------------------------------------------------------------------------------

destructor TJmSparseFlatSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmSparseFlatSet.Clear;
var
  F: Integer;
begin
  if FSetList <> nil then
  begin
    for F := 0 to FSetListEntries - 1 do
      if FSetList^[F] <> nil then
        Dispose(PDelphiSet(FSetList^[F]));
    FreeMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FSetList := nil;
    FSetListEntries := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmSparseFlatSet.Invert;
var
  F: Integer;
begin
  for F := 0 to FSetListEntries - 1 do
    if FSetList^[F] <> nil then
      PDelphiSet(FSetList^[F])^ := CompleteDelphiSet - PDelphiSet(FSetList^[F])^;
end;

//--------------------------------------------------------------------------------------------------

function TJmSparseFlatSet.GetBit(const Idx: Integer): Boolean;
var
  SetIdx: Integer;
begin
  SetIdx := Idx shr 8;
  Result := (SetIdx < FSetListEntries) and (FSetList^[SetIdx] <> nil) and
    (Byte(Idx and $FF) in PDelphiSet(FSetList^[SetIdx])^);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmSparseFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
var
  I, SetIdx: Integer;
  S: PDelphiSet;
begin
  SetIdx := Idx shr 8;
  if SetIdx >= FSetListEntries then
    if Value then
    begin
      I := FSetListEntries;
      FSetListEntries := SetIdx + 1;
      ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
      FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
    end
    else
      Exit;
  S := FSetList^[SetIdx];
  if S = nil then
    if Value then
    begin
      New(S);
      S^ := [];
      FSetList^[SetIdx] := S;
    end
    else
      Exit;
  Include(S^, Byte(Idx and $FF));
end;

//--------------------------------------------------------------------------------------------------

procedure TJmSparseFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I, LowSet, HighSet: Integer;

  procedure SetValue(const S: TDelphiSet; const SetIdx: Integer);
  var
    D: PDelphiSet;
  begin
    D := FSetList^[SetIdx];
    if D = nil then
    begin
      if Value then
      begin
        New(D);
        D^ := S;
        FSetList^[SetIdx] := D;
      end;
    end
    else
    if Value then
      D^ := D^ + S
    else
      D^ := D^ - S;
  end;

begin
  LowSet := Low shr 8;
  HighSet := High shr 8;
  if HighSet >= FSetListEntries then
  begin
    I := FSetListEntries;
    FSetListEntries := HighSet + 1;
    ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
  end;
  if LowSet = HighSet then
    SetValue([Byte(Low and $FF)..Byte(High and $FF)], LowSet)
  else
  begin
    SetValue([Byte(Low and $FF)..$FF], LowSet);
    SetValue([0..Byte(High and $FF)], HighSet);
    for I := LowSet + 1 to HighSet - 1 do
      SetValue(CompleteDelphiSet, I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJmSparseFlatSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I: Integer;
begin
  if not Value and (High >= FSetListEntries) then
  begin
    Result := False;
    Exit;
  end;
  for I := Low to Min(High, FSetListEntries) do
    if GetBit(I) <> Value then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//===================================================================================================
// Prime numbers
//===================================================================================================

const
  PrimeCacheLimit = 65537; // 4K lookup table. Note: Sqr(65537) > MaxLongint

var
  PrimeSet: TJmFlatSet = nil;

procedure InitPrimeSet;
var
  I, J, MaxI, MaxJ : Integer;
begin
  PrimeSet := TJmFlatSet.Create;
  PrimeSet.SetRange(1, PrimeCacheLimit div 2, True);
  PrimeSet.SetBit(0, False);               // 1 is no prime
  MaxI := System.Trunc(System.Sqrt(PrimeCacheLimit));
  I := 3;
  repeat
    if PrimeSet.GetBit(I div 2) then
    begin
      MaxJ := PrimeCacheLimit div I;
      J := 3;
      repeat
          PrimeSet.SetBit((I*J) div 2, False);
          inc (J,2);
      until J > MaxJ;
    end;
    inc (I, 2);
  until I > MaxI;
end;

//--------------------------------------------------------------------------------------------------

function IsPrimeTD(N: Cardinal): Boolean;
{ Trial Division Algorithm }
var
  I, MAX: Cardinal;
  R: Extended;
begin
  if N = 2 then
  begin
    Result := True;
    exit;
  end;
  if (N and 1) = 0 then   //Zero or even
  begin
    Result := False;
    exit;
  end;
  if PrimeSet = nil then // initialize look-up table
     InitPrimeSet;
  if N <= PrimeCacheLimit then // do look-up
  begin
     Result := PrimeSet.GetBit(N div 2)
  end
  else
  begin // calculate
    R := N;
    MAX := Round(Sqrt (R));
    if MAX > PrimeCacheLimit then
    begin
      raise EJmMathError.CreateResRec(@RsUnexpectedValue);
      Exit;
    end;
    I := 1;
    repeat
      inc (I,2);
      if PrimeSet.GetBit(I div 2) then
      begin
        if N mod I = 0 then
           begin
           Result := False;
           Exit;
         end;
      end;
    until I >= MAX;
    Result := True;
  end;
end;

//--------------------------------------------------------------------------------------------------

{ Rabin-Miller Strong Primality Test }

function IsPrimeRM(N: Cardinal): Boolean;
asm
       TEST  EAX,1            // Odd(N) ??
       JNZ   @@1
       CMP   EAX,2            // N == 2 ??
       SETE  AL
       RET
@@1:   CMP   EAX,73
       JBE   @@C
       PUSH  ESI
       PUSH  EDI
       PUSH  EBX
       PUSH  EBP
       PUSH  EAX              // save N as Param for @@5
       LEA   EBP,[EAX - 1]    // M == N -1, Exponent
       MOV   ECX,32           // calc remaining Bits of M and shift M'
       MOV   ESI,EBP
@@2:   DEC   ECX
       SHL   ESI,1
       JNC   @@2
       PUSH  ECX              // save Bits as Param for @@5
       PUSH  ESI              // save M' as Param for @@5
       CMP   EAX,08A8D7Fh     // N >= 9080191 ??
       JAE   @@3
// now if (N < 9080191) and SPP(31, N) and SPP(73, N) then N is prime
       MOV   EAX,31
       CALL  @@5
       JC    @@4
       MOV   EAX,73
       PUSH  OFFSET @@4
       JMP   @@5
// now if (N < 4759123141) and SPP(2, N) and SPP(7, N) and SPP(61, N) then N is prime
@@3:   MOV   EAX,2
       CALL  @@5
       JC    @@4
       MOV   EAX,7
       CALL  @@5
       JC    @@4
       MOV   EAX,61
       CALL  @@5
@@4:   SETNC AL
       ADD   ESP,4 * 3
       POP   EBP
       POP   EBX
       POP   EDI
       POP   ESI
       RET
// do a Strong Pseudo Prime Test
@@5:   MOV   EBX,[ESP + 12]   // N on stack
       MOV   ECX,[ESP +  8]   // remaining Bits
       MOV   ESI,[ESP +  4]   // M'
       MOV   EDI,EAX          // T = b, temp. Base
@@6:   DEC   ECX
       MUL   EAX
       DIV   EBX
       MOV   EAX,EDX
       SHL   ESI,1
       JNC   @@7
       MUL   EDI
       DIV   EBX
       AND   ESI,ESI
       MOV   EAX,EDX
@@7:   JNZ   @@6
       CMP   EAX,1            // b^((N -1)(2^s)) mod N ==  1 mod N ??
       JE    @@A
@@8:   CMP   EAX,EBP          // b^((N -1)(2^s)) mod N == -1 mod N ??
       JE    @@A
       DEC   ECX              // second part to 2^s
       JNG   @@9
       MUL   EAX
       DIV   EBX
       CMP   EDX,1
       MOV   EAX,EDX
       JNE   @@8
@@9:   STC
@@A:   RET
@@B:   DB    3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73
@@C:   MOV   EDX,OFFSET @@B
       MOV   ECX,19
@@D:   CMP   AL,[EDX + ECX]
       JE    @@E
       DEC   ECX
       JNL   @@D
@@E:   SETE  AL
end;

//--------------------------------------------------------------------------------------------------

function PrimeFactors(N: Cardinal): TDynCardinalArray;
var
  I, L, Max: Cardinal;
  R: Extended;
begin
  SetLength(Result, 0);
  if N <= 1 then
    Exit
  else
  begin
    if PrimeSet = nil then
      InitPrimeSet;
    L := 0;
    R := N;
    R := Sqrt(R);
    Max := Round (R);        // only one factor can be > sqrt (N)
    if N mod 2 = 0 then      // test even at first
    begin                    // 2 is a prime factor
      Inc(L);
      SetLength(Result, L);
      Result[L - 1] := 2;
      repeat
        N := N div 2;
        if N = 1 then        // no more factors
          Exit;
      until N mod 2 <> 0;
    end;
    I := 3;                  // test all odd factors
    repeat
      if (N mod I = 0) and IsPrime (I)  then
      begin                  // I is a prime factor
        Inc(L);
        SetLength(Result, L);
        Result[L - 1] := I;
        repeat
          N := N div I;
          if N = 1 then      // no more factors
            Exit;
        until N mod I <> 0;
      end;
      inc (I, 2);
    until I > Max;
    Inc(L);                  // final factor (> sqrt(N))
    SetLength(Result, L);
    Result[L - 1] := N;
  end;
end;

//--------------------------------------------------------------------------------------------------

function IsPrimeFactor(const F, N: Cardinal): Boolean;
begin
  Result := (N mod F = 0) and IsPrime(F);
end;

//--------------------------------------------------------------------------------------------------

function IsRelativePrime(const X, Y: Cardinal): Boolean;
begin
  Result := GCD(X, Y) = 1;
end;

//--------------------------------------------------------------------------------------------------

procedure SetPrimalityTest(const Method: TPrimalityTestMethod);
begin
  case Method of
    ptTrialDivision: IsPrime := IsPrimeTD;
    ptRabinMiller: IsPrime := IsPrimeRM;
  end;
end;

//===================================================================================================
// Floating point value classification
//===================================================================================================

const
  fpEmpty = TFloatingPointClass(Ord(High(TFloatingPointClass))+1);

  FPClasses: array [0..6] of TFloatingPointClass =
   (
    fpInvalid,
    fpNaN,
    fpNormal,
    fpInfinite,
    fpZero,
    fpEmpty,    // should not happen
    fpDenormal
   );

function _FPClass: TFloatingPointClass;
// In: ST(0) Value to examine
asm
        FXAM
        XOR     EDX, EDX
        FNSTSW  AX
        FFREE   ST(0)
        FINCSTP
        BT      EAX, 14 // C3
        RCL     EDX, 1
        BT      EAX, 10 // C2
        RCL     EDX, 1
        BT      EAX, 8  // C0
        RCL     EDX, 1
        MOVZX   EAX, TFloatingPointClass(FPClasses[EDX])
end;

//--------------------------------------------------------------------------------------------------

function FloatingPointClass(const Value: Single): TFloatingPointClass; overload;
asm
        FLD     Value
        CALL    _FPClass
end;

//--------------------------------------------------------------------------------------------------

function FloatingPointClass(const Value: Double): TFloatingPointClass; overload;
asm
        FLD     Value
        CALL    _FPClass
end;

//--------------------------------------------------------------------------------------------------

function FloatingPointClass(const Value: Extended): TFloatingPointClass; overload;
asm
        FLD     Value
        CALL    _FPClass
end;


//===================================================================================================
// NaN and Infinity support
//===================================================================================================

function IsInfinite(const Value: Single): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;

//--------------------------------------------------------------------------------------------------

function IsInfinite(const Value: Double): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;

//--------------------------------------------------------------------------------------------------

function IsInfinite(const Value: Extended): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;

//--------------------------------------------------------------------------------------------------

const
  sSignBit = 31;
  dSignBit = 63;
  xSignBit = 79;

type
  TSingleBits = set of 0..sSignBit;
  TDoubleBits = set of 0..dSignBit;
  TExtendedBits = set of 0..xSignBit;

  sFractionBits = 0..22; // Single type fraction bits
  dFractionBits = 0..51; // Double type fraction bits
  xFractionBits = 0..62; // Extended type fraction bits

  sExponentBits = 23..sSignBit-1;
  dExponentBits = 52..dSignBit-1;
  xExponentBits = 64..xSignBit-1;

  QWord = Int64;

  PExtendedRec = ^TExtendedRec;
  TExtendedRec = packed record
    Significand: QWord;
    Exponent: Word;
  end;

const
  ZeroTag = $3FFFFF;
  InvalidTag = TNaNTag($80000000);
  NaNTagMask = $3FFFFF;

  sNaNQuietFlag = High(sFractionBits);
  dNaNQuietFlag = High(dFractionBits);
  xNaNQuietFlag = High(xFractionBits);

  dNaNTagShift = High(dFractionBits) - High(sFractionBits);
  xNaNTagShift = High(xFractionBits) - High(sFractionBits);

  sNaNBits = $7F800000;
  dNaNBits = $7FF0000000000000;

  sQuietNaNBits = sNaNBits or (1 shl sNaNQuietFlag);
  dQuietNaNBits = dNaNBits or (Int64(1) shl dNaNQuietFlag);

//--------------------------------------------------------------------------------------------------

function IsNaN(const Value: Single): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;

//--------------------------------------------------------------------------------------------------

function IsNaN(const Value: Double): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;

//--------------------------------------------------------------------------------------------------

function IsNaN(const Value: Extended): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckNaN(const Value: Single); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJmMathError.CreateResRec(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckNaN(const Value: Double); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJmMathError.CreateResRec(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure CheckNaN(const Value: Extended); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJmMathError.CreateResRec(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetNaNTag(const NaN: Single): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := PLongint(@NaN)^ and NaNTagMask;
  if sSignBit in TSingleBits(NaN) then
    Result := -Temp
  else
    if Temp = ZeroTag then
      Result := 0
    else
      Result := Temp;
end;

//--------------------------------------------------------------------------------------------------

function testbit (const a;ind:byte):boolean;

var i,j:integer;

begin
  i:=ind mod 8; j:=ind div 8;
  result:=(pbytearray(a)^[j] and (1 shl i))<>0;
end;

procedure setbit(var a;ind:byte);
var i,j:integer;

begin
  i:=ind mod 8; j:=ind div 8;
  pbytearray(a)^[j]:=pbytearray(a)^[j] or (1 shl i);
end;

procedure clearbit(var a;ind:byte);
var i,j:integer;

begin
  i:=ind mod 8; j:=ind div 8;
  pbytearray(a)^[j]:=pbytearray(a)^[j] and not (1 shl i);
end;

function GetNaNTag(const NaN: Double): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := (PInt64(@NaN)^ shr dNaNTagShift) and NaNTagMask;
//  if dSignBit in TDoubleBits(NaN) then
  if TestBit(NaN,dSignBit) Then
    Result := -Temp
  else
    if Temp = ZeroTag then
      Result := 0
    else
      Result := Temp;
end;

//--------------------------------------------------------------------------------------------------

function GetNaNTag(const NaN: Extended): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := (PExtendedRec(@NaN)^.Significand shr xNaNTagShift) and NaNTagMask;
//   if xSignBit in TExtendedBits(NaN) then
  if TestBit(NaN,xSignBit) Then
    Result := -Temp
  else
    if Temp = ZeroTag then
      Result := 0
    else
      Result := Temp; 
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  TRealType = (rtUndef, rtSingle, rtDouble, rtExtended);

  { ExceptionInformation record for FPU exceptions under WinNT,
    where documented? }
  PFPUExceptionInfo = ^TFPUExceptionInfo;
  TFPUExceptionInfo = packed record
    Unknown: array [0..7] of Longint;
    ControlWord: Word;
    Dummy1: Word;
    StatusWord: Word;
    Dummy2: Word;
    TagWord: Word;
    Dummy3: Word;
    InstructionPtr: Pointer;
    UnknownW: Word;
    OpCode: Word;  // Note: 5 most significant bits of first opcode byte
                   // (always 11011b) not stored in FPU opcode register
    OperandPtr: Pointer;
    UnknownL: Longint;
  end;

  TExceptObjProc = function (P: PExceptionRecord): Exception;

var
  PrevExceptObjProc: TExceptObjProc;
  ExceptObjProcInitialized: Boolean = False;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  Tag: TNaNTag;
  FPUExceptInfo: PFPUExceptionInfo;
  OPtr: Pointer;
  OType: TRealType;

  function GetOperandType(OpCode: Word): TRealType;
  var
    nnn: 0..7;
  begin
    Result := rtUndef;
    nnn := (Lo(OpCode) shr 3) and 7;   // nnn field of ModR/M byte
    if Lo(OpCode) <= $BF then
    case Hi(OpCode) of   // 3 least significant bits of first opcode byte
      0:
        Result := rtSingle;
      1:
        if nnn < 4 then
          Result := rtSingle;
      // Extended signaling NaNs don't cause exceptions on FLD/FST(P) ?!
      3:
        if nnn = 5 then
          Result := rtExtended;
      4:
        Result := rtDouble;
      5:
        if nnn = 0 then
          Result := rtDouble;
    end;
  end;

begin
  Tag := InvalidTag; // shut up compiler warning
  OType := rtUndef;
  if P^.ExceptionCode = STATUS_FLOAT_INVALID_OPERATION then
  begin
    FPUExceptInfo := @P^.ExceptionInformation;
    OPtr := FPUExceptInfo^.OperandPtr;
    OType := GetOperandType(FPUExceptInfo^.OpCode);
    case OType of
      rtSingle:
        Tag := GetNaNTag(PSingle(OPtr)^);
      rtDouble:
        Tag := GetNaNTag(PDouble(OPtr)^);
      rtExtended:
        Tag := GetNaNTag(PExtended(OPtr)^);
    end;
  end;

  if OType = rtUndef then
    Result := PrevExceptObjProc(P)
  else
    Result := EJmNaNSignal.Create(Tag);
end;

{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

procedure InitExceptObjProc;

  function IsInitialized: Boolean;
  asm
     MOV   AL, True
     LOCK XCHG  AL, ExceptObjProcInitialized
  end;

begin
 if not IsInitialized then
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      PrevExceptObjProc := Pointer(InterlockedExchange(Integer(ExceptObjProc), Integer(@GetExceptionObject)));
      end;

{$ENDIF}

//--------------------------------------------------------------------------------------------------

constructor EJmNaNSignal.Create(ATag: TNaNTag);
begin
  FTag := ATag;
  CreateResRecFmt(@RsNaNSignal, [ATag]);
end;

//--------------------------------------------------------------------------------------------------

procedure CheckTag(Tag: TNaNTag);
begin
  if (Tag < Low(TNaNTag)) or (Tag > High(TNaNTag)) then
    raise EJmMathError.CreateResRecFmt(@RsNaNTagError, [Tag]);
end;

//--------------------------------------------------------------------------------------------------

procedure MakeQuietNaN(var X: Single; Tag: TNaNTag);
var
  Bits: DWord;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag or sQuietNaNBits
  else
    Bits := Abs(Tag) or sQuietNaNBits;
  if Tag < 0 then
    Include(TSingleBits(Bits), sSignBit);
  PDWord(@X)^ := Bits;
end;

//--------------------------------------------------------------------------------------------------

procedure MakeQuietNaN(var X: Double; Tag: TNaNTag);
var
  Bits: Int64;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag
  else
    Bits := Abs(Tag);
  PInt64(@X)^ := (Bits shl dNaNTagShift) or dQuietNaNBits;
  if Tag < 0 then
    SetBit(X,dSignBit);  
//    Include(TDoubleBits(X), dSignBit);
end;

//--------------------------------------------------------------------------------------------------

procedure MakeQuietNaN(var X: Extended; Tag: TNaNTag);
const
  QuietNaNSignificand = $C000000000000000;
  QuietNaNExponent = $7FFF;
var
  Bits: Int64;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag
  else
    Bits := Abs(Tag);
  TExtendedRec(X).Significand := (Bits shl xNaNTagShift) or QuietNaNSignificand;
  TExtendedRec(X).Exponent := QuietNaNExponent;
  if Tag < 0 then
     Setbit(X,xSignBit);
//    Include(TExtendedBits(X), xSignBit);
end;

//--------------------------------------------------------------------------------------------------

procedure MakeSignalingNaN(var X: Single; Tag: TNaNTag);
begin
  {$IFDEF MSWINDOWS}
  InitExceptObjProc;
  {$ENDIF}
  MakeQuietNaN(X, Tag);
  ClearBit(x,sNaNQuietFlag);
//  Exclude(TSingleBits(X), sNaNQuietFlag);
end;

//--------------------------------------------------------------------------------------------------

procedure MakeSignalingNaN(var X: Double; Tag: TNaNTag);
begin
  {$IFDEF MSWINDOWS}
  InitExceptObjProc;
  {$ENDIF}
  MakeQuietNaN(X, Tag);
  ClearBit(x,dNaNQuietFlag);
//  Exclude(TDoubleBits(X), dNaNQuietFlag);
end;

//--------------------------------------------------------------------------------------------------

procedure MakeSignalingNaN(var X: Extended; Tag: TNaNTag);
begin
  {$IFDEF MSWINDOWS}
  //InitExceptObjProc;
  {$ENDIF}
  MakeQuietNaN(X, Tag);
  ClearBit(x,xNaNQuietFlag);
//  Exclude(TExtendedBits(X), xNaNQuietFlag);
end;

//--------------------------------------------------------------------------------------------------

procedure MineSingleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag);
var
  Tag, StopTag: TNaNTag;
  P: PLongint;
begin
  {$IFDEF MSWINDOWS}
  InitExceptObjProc;
  {$ENDIF}
  StopTag := StartTag + Count - 1;
  CheckTag(StartTag);
  CheckTag(StopTag);
  P := @Buffer;
  for Tag := StartTag to StopTag do
  begin
    if Tag > 0 then
      P^ := sNaNBits or Tag
    else
    if Tag < 0 then
      P^ := sNaNBits or Longint($80000000) or -Tag
    else
      P^ := sNaNBits or ZeroTag;
    Inc(P);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure MineDoubleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag);
var
  Tag, StopTag: TNaNTag;
  P: PInt64;
begin
  {$IFDEF MSWINDOWS}
  InitExceptObjProc;
  {$ENDIF}
  StopTag := StartTag + Count - 1;
  CheckTag(StartTag);
  CheckTag(StopTag);
  P := @Buffer;
  for Tag := StartTag to StopTag do
  begin
    if Tag > 0 then
      P^ := dNaNBits or (Int64(Tag) shl dNaNTagShift)
    else
    if Tag < 0 then
      P^ := dNaNBits or $8000000000000000 or (Int64(-Tag) shl dNaNTagShift)
    else
      P^ := dNaNBits or (Int64(ZeroTag) shl dNaNTagShift);
    Inc(P);
  end;
end;

//--------------------------------------------------------------------------------------------------

function MinedSingleArray(Length: Integer): TDynSingleArray;
begin
  SetLength(Result, Length);
  MineSingleBuffer(Result[0], Length, 0);
end;

//--------------------------------------------------------------------------------------------------

function MinedDoubleArray(Length: Integer): TDynDoubleArray;
begin
  SetLength(Result, Length);
  MineDoubleBuffer(Result[0], Length, 0);
end;

//--------------------------------------------------------------------------------------------------

function IsSpecialValue(const X: TJmFloat): Boolean;
begin
  Result := IsNan(x) or IsInfinite(x);
end;


//===================================================================================================
// Rational Numbers
//===================================================================================================

constructor TJmRational.Create(const Numerator: Integer; const Denominator: Integer);
begin
  inherited Create;
  Assign(Numerator, Denominator);
end;

//--------------------------------------------------------------------------------------------------

constructor TJmRational.Create;
begin
  inherited Create;
  AssignZero;
end;

//--------------------------------------------------------------------------------------------------

constructor TJmRational.Create(const R: TJmFloat); 
begin
  inherited Create;
  Assign(R);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Simplify;
var
  I: Integer;
begin
  if FN < 0 then
  begin
    FT := -FT;
    FN := -FN;
  end;

  if (FT = 1) or (FN = 1) or (FT = 0) then
    Exit;

  I := GCD(System.Abs(FT), FN);
  FT := FT div I;
  FN := FN div I;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Assign(const Numerator: Integer; const Denominator: Integer);
begin
  if Denominator = 0 then
    raise EJmMathError.CreateResRec(@RsInvalidRational);
  FT := Numerator;
  FN := Denominator;
  if FN <> 1 then
    Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Assign(const R: TJmRational);
begin
  FT := R.FT;
  FN := R.FN;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Assign(const R: TJmFloat);
var
  T: TJmRational;
  Z: Integer;

  function CalcFrac(const R: TJmFloat; const Level: Integer): TJmRational;
  var
    I: TJmFloat;
    Z: Integer;
  begin
    if IsFloatZero(R) or (Level = 12) then // 0 (if Level = 12 we get an approximation)
      Result := TJmRational.Create
    else
    if FloatsEqual(R, 1.0) then // 1
    begin
      Result := TJmRational.Create;
      Result.AssignOne;
    end
    else
    if IsFloatZero(Frac(R * 1E8)) then // terminating decimal (<8)
      Result := TJmRational.Create(Trunc(R * 1E8), 100000000)
    else
    begin // recursive process
      I := 1.0 / R;
      Result := CalcFrac(Frac(I), Level + 1);
      Z := Trunc(I);
      Result.Add(Z);
      Result.Reciprocal;
    end;
  end;

begin
  T := CalcFrac(Frac(R), 1);
  try
    Z := Trunc(R);
    T.Add(Z);
    Assign(T);
  finally
    T.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.AssignOne;
begin
  FT := 1;
  FN := 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.AssignZero;
begin
  FT := 0;
  FN := 1;
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.IsEqual(const Numerator: Integer; const Denominator: Integer): Boolean;
var
  R: TJmRational;
begin
  R := TJmRational.Create(Numerator, Denominator);
  Result := IsEqual(R);
  R.Free;
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.IsEqual(const R: TJmRational): Boolean;
begin
  Result := (FT = R.FT) and (FN = R.FN);
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.IsEqual(const R: TJmFloat): Boolean;
begin
  Result := FloatsEqual(R, GetAsFloat);
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.IsOne: Boolean;
begin
  Result := (FT = 1) and (FN = 1);
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.IsZero: Boolean;
begin
  Result := FT = 0;
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.Duplicate: TJmRational;
begin
  Result := TJmRational.Create(FT, FN);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.SetAsFloat(const R: TJmFloat);
begin
  Assign(R);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.SetAsString(const S: string);
var
  F: Integer;
begin
  F := Pos('/', S);
  if F = 0 then
    Assign(StrToFloat(S))
  else
    Assign(StrToInt(Trim(Copy(S,1,F - 1))), StrToInt(Trim(Copy(S, F + 1,Length(s)))));
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.GetAsFloat: TJmFloat;
begin
  Result := FT / FN;
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.GetAsString: string;
begin
  Result := IntToStr(FT) + '/' + IntToStr(FN);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Add(const R: TJmRational);
begin
  FT := FT * R.FN + R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Add(const V: Integer);
begin
  Inc(FT, FN * V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Add(const V: TJmFloat);
begin
  Assign(GetAsFloat + V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Subtract(const V: TJmFloat);
begin
  Assign(GetAsFloat - V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Subtract(const R: TJmRational);
begin
  FT := FT * R.FN - R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Subtract(const V: Integer);
begin
  Dec(FT, FN * V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Negate;
begin
  FT := -FT;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Abs;
begin
  FT := System.Abs(FT);
  FN := System.Abs(FN);
end;

//--------------------------------------------------------------------------------------------------

function TJmRational.Sgn: Integer;
begin
  if FT = 0 then
    Result := 0
  else
  begin
    if JmMathBasics.Sgn(FT) = JmMathBasics.Sgn(FN) then
      Result := 1
    else
      Result := -1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Divide(const V: Integer);
begin
  if V = 0 then
    raise EJmMathError.CreateResRec(@RsDivByZero);

  FN := FN * V;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Divide(const R: TJmRational);
begin
  if R.FT = 0 then
    raise EJmMathError.CreateResRec(@RsRationalDivByZero);

  FT := FT * R.FN;
  FN := FN * R.FT;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Divide(const V: TJmFloat);
begin
  Assign(GetAsFloat / V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Reciprocal;
begin
  if FT = 0 then
    raise EJmMathError.CreateResRec(@RsRationalDivByZero);
  SwapOrd(FT, FN);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Multiply(const R: TJmRational);
begin
  FT := FT * R.FT;
  FN := FN * R.FN;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Multiply(const V: Integer);
begin
  FT := FT * V;
  Simplify;
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Multiply(const V: TJmFloat);
begin
  Assign(GetAsFloat * V);
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Power(const R: TJmRational);
begin
  Assign(JmMathBasics.Power(GetAsFloat, R.GetAsFloat));
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Power(const V: Integer);
var
  T, N: Extended;
begin
  T := FT;
  N := FN;
  FT := Round(JmMathBasics.Power(T, V));
  FN := Round(JmMathBasics.Power(N, V));
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Power(const V: TJmFloat);
begin
  Assign(JmMathBasics.Power(FT, V) / JmMathBasics.Power(FN, V));
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Sqrt;
begin
  Assign(System.Sqrt(FT / FN));
end;

//--------------------------------------------------------------------------------------------------

procedure TJmRational.Sqr;
begin
  FT := System.Sqr(FT);
  FN := System.Sqr(FN);
end;



//--------------------------------------------------------------------------------------------------
// CRC
//--------------------------------------------------------------------------------------------------

{$IFDEF CRCINIT}
{$J+ to have the Polynomial changable}
{$ENDIF}

// CRC 16

const
//  CRC16Polynom = $1021;

  Crc16Table: array [0..255] of Word = (
    $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0
   );




  Crc16Bits = 16;
  Crc16Start : Cardinal = $FFFF;
  Crc16Bytes = 2;
  Crc16HighBit = $8000;
  NotCrc16HighBit = $7FFF;

//--------------------------------------------------------------------------------------------------

function Crc16Corr(Crc: Word; N: Integer): Integer;
var
  I: Integer;
//  CrcX : Cardinal;
begin
  // calculate Syndrome
//  CrcX := CrC;
  for I := 1 to Crc16Bytes do
//  a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
      Crc := Crc16Table[Crc shr (CRC16Bits-8)] xor Word(Crc shl 8);
  I := -1;
  repeat
    Inc(I);
    if (Crc and 1) <> 0 then
      Crc := ((Crc xor Crc16Table[1]) shr 1) or Crc16HighBit
//      Crc16Table[1] = Crc16Polynom
    else
      Crc := (Crc shr 1) and NotCrc16HighBit;
  until (Crc = Crc16HighBit) or (I = (N + Crc16Bytes) * 8);
  if Crc <> Crc16HighBit then
    Result := -1000 // not correctable
  else
      // I = No. of single faulty bit
      // (high bit first,
      // starting from lowest with CRC bits)
    Result := I - Crc16Bits;
      // Result <  0 faulty CRC-bit
      // Result >= 0 No. of faulty data bit
end;

//--------------------------------------------------------------------------------------------------

function Crc16_P(X: PByteArray; N: Integer; Crc: Word = 0): Word;
var
  I: Integer;
begin
  Result := Crc16Start;
  for I := 0 to N - 1 do // The CRC Bytes are located at the end of the information
  begin
//  a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc16Table[Result shr (CRC16Bits-8)] xor Word((Result shl 8)) xor X[I];
  end;
  for i := 0 to Crc16Bytes - 1 do
  begin
//  a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc16Table[Result shr (CRC16Bits-8)] xor Word((Result shl 8)) xor (Crc shr (CRC16Bits-8));
    Crc := Word(Crc shl 8);
  end;
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc16_P(X: PByteArray; N: Integer; Crc: Word): Integer;
// checks and corrects a single bit in up to 2^15-16 Bit -> 2^12-2 = 4094 Byte
var
  I, J: Integer;
  C: Byte;
begin
  Crc := Crc16_P(X, N, Crc);
  if Crc = 0 then
    Result := 0 // No CRC-error
  else
  begin
    J := Crc16Corr(Crc, N);
    if J < -(Crc16Bytes * 8 + 1) then
      Result := -1 // non-correctable error (more than one wrong bit)
    else
    begin
      if J < 0 then
        Result := 1 // one faulty Bit in CRC itself
      else
      begin // Bit j is faulty
        I := J and 7; // I <= 7 (faulty Bit in Byte)
        C := 1 shl I; // C <= 128
        I := J shr 3; // I: Index of faulty Byte
        X[N - 1 - I] := X[N - 1 - I] xor C; // correct faulty bit
        Result := 1; // Correctable error
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function Crc16(const X: array of Byte; N: Integer; Crc: Word = 0): Word;
begin
   Result := Crc16_P(@X, N, Crc);
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc16(var X: array of Byte; N: Integer; Crc: Word): Integer;
begin
  Result := CheckCRC16_P(@X, N, CRC);
end;

//--------------------------------------------------------------------------------------------------

function Crc16_A(const X: array of Byte; Crc: Word = 0): Word;
begin
   Result := Crc16_P(@X, Length(X), Crc);
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc16_A(var X: array of Byte; Crc: Word): Integer;
begin
  Result := CheckCrc16_P(@X, Length(X), Crc);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF CRCINIT}
// The CRC Table can be generated like this:
// const crc16start0 = 0;  !!

function crc16_bitwise(x: PByteArray; n: integer; crc: Word; Polynom: Word) : Word;
var
  i, j: integer;
  sr, srhighbit: Word;
  b: Byte;

const
  crc16start0 = 0;   //Generating the table

begin
   sr := crc16start0;
   srhighbit := 0;
   for i := 0 to n-1+crc16bytes do
   begin
      if i>=n then
      begin
         b := crc shr (crc16bits-8);
         crc := crc shl 8;
        end
        else
        begin
         b := x[i]
      end;
      for j := 1 to 8 do
      begin
         if (srhighbit <> 0) then
         begin
           sr := sr xor polynom;
         end;
         srhighbit := sr and crc16highbit ;
         sr := (Word (sr shl 1)) or ((b shr 7) and 1);
         b := byte (b shl 1);
      end
   end;
   if (srhighbit <> 0) then
   begin
      sr := sr xor polynom;
   end;
   crc16_bitwise := sr;
end;

//--------------------------------------------------------------------------------------------------

procedure InitCrc16 (Polynom, Start: Word);
var
  x: array [0..0] of byte;
  i: integer;
begin
   for i := 0 to 255 do
   begin
     x[0] := i;
     crc16table [i] := crc16_bitwise (@x, 1, 0, Polynom); { nur mit crcstart=0 !!!!}
   end;
   Crc16Start := Start;
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

// CRC 32

const
//  CRC32Polynom = $04C11DB7;

  Crc32Table: array [0..255] of Cardinal = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61, $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039, $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1, $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,
    $34867077, $30476DC0, $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE, $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6, $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637, $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF, $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7, $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8, $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0, $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668, $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
    );

  Crc32Start : Cardinal = $FFFFFFFF;
  Crc32Bits = 32;
  Crc32Bytes = 4;
  Crc32HighBit = $80000000;
  NotCrc32HighBit = $7FFFFFFF;

//--------------------------------------------------------------------------------------------------

function Crc32Corr(Crc: Cardinal; N: Integer): Integer;
var
  I: Integer;
begin
  // calculate Syndrome
  for I := 1 to Crc32Bytes do
    Crc := Crc32Table[Crc shr (CRC32Bits-8)] xor (Crc shl 8);
  I := -1;
  repeat
    Inc(I);
    if (Crc and 1) <> 0 then
      Crc := ((Crc xor Crc32Table[1]) shr 1) or Crc32HighBit
//      Crc32Table[1] = Crc32Polynom
    else
      Crc := (Crc shr 1) and NotCrc32HighBit;
  until (Crc = Crc32HighBit) or (I = (N + Crc32Bytes) * 8);
  if Crc <> Crc32HighBit then
    Result := -1000 // not correctable
  else
    // I = No. of single faulty bit
    // (high bit first,
    // starting from lowest with CRC bits)
    Result := I - Crc32Bits;
    // Result <  0 faulty CRC-bit
    // Result >= 0 No. of faulty data bit
end;

//--------------------------------------------------------------------------------------------------

function Crc32_P(X: PByteArray; N: Integer; Crc: Cardinal = 0): Cardinal;
var
  I: Integer;
begin
  Result := Crc32Start;
  for I := 0 to N - 1 do // The CRC Bytes are located at the end of the information
  begin
    // a 32 bit value shr 24 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc32Table[Result shr (CRC32Bits-8)] xor (Result shl 8) xor X[I];
  end;
  for i := 0 to Crc32Bytes - 1 do
  begin
    // a 32 bit value shr 24 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc32Table[Result shr (CRC32Bits-8)] xor (Result shl 8) xor (Crc shr (CRC32Bits-8));
    Crc := Crc shl 8;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc32_P(X: PByteArray; N: Integer; Crc: Cardinal): Integer;
// checks and corrects a single bit in up to 2^31-32 Bit -> 2^28-4 = 268435452 Byte
var
  I, J: Integer;
  C: Byte;
begin
  Crc := Crc32_P(X, N, Crc);
  if Crc = 0 then
    Result := 0 // No CRC-error
  else
  begin
    J := Crc32Corr(Crc, N);
    if J < -(Crc32Bytes * 8 + 1) then
      Result := -1 // non-correctable error (more than one wrong bit)
    else
    begin
      if J < 0 then
        Result := 1 // one faulty Bit in CRC itself
      else
      begin // Bit j is faulty
        I := J and 7; // I <= 7 (faulty Bit in Byte)
        C := 1 shl I; // C <= 128
        I := J shr 3; // I: Index of faulty Byte
        X[N - 1 - I] := X[N - 1 - I] xor C; // correct faulty bit
        Result := 1; // Correctable error
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal = 0): Cardinal;
begin
   Result := Crc32_P(@X, N, Crc);
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
begin
  Result := CheckCRC32_P(@X, N, CRC);
end;

//--------------------------------------------------------------------------------------------------

function Crc32_A(const X: array of Byte; Crc: Cardinal = 0): Cardinal;
begin
   Result := Crc32_P(@X, Length(X), Crc);
end;

//--------------------------------------------------------------------------------------------------

function CheckCrc32_A(var X: array of Byte; Crc: Cardinal): Integer;
begin
  Result := CheckCrc32_P(@X, Length(X), Crc);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF CRCINIT}
// The CRC Table can be generated like this:
// const crc32start0 = 0;  !!

function crc32_bitwise (x: PByteArray; n: integer; crc: Cardinal; Polynom: Cardinal) : Cardinal;
var
 i, j: Integer;
 sr, srhighbit: Cardinal;
 b: Byte;

const
  crc32start0 = 0;   //Generating the table

begin
  sr := crc32start0;
  srhighbit := 0;
  for i := 0 to n-1+crc32bytes do begin
    if i > =n then
    begin
      b := crc shr (crc32bits-8);
      crc := crc shl 8;
    end
    else
    begin
       b := x[i]
    end;
    for j := 1 to 8 do
    begin
       if (srhighbit <> 0) then
       begin
         sr := sr xor polynom;
       end;
       srhighbit := sr and crc32highbit ;
       sr := (sr shl 1) or ((b shr 7) and 1);
       b := byte (b shl 1);
    end
  end;

  if (srhighbit <> 0) then
  begin
    sr := sr xor polynom;
  end;
  crc32_bitwise := sr;
end;

//--------------------------------------------------------------------------------------------------

procedure InitCrc32 (Polynom, Start: Cardinal);
var
  x: array [0..0] of byte;
  i: integer;

begin
   for i := 0 to 255 do
   begin
     x[0] := i;
     crc32table [i] := crc32_bitwise (@x, 1, 0, Polynom);
   end;
   Crc32Start := Start;
end;
{$ENDIF}


end.


