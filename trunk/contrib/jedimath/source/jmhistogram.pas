{******************************************************************************}
{                                                                              }
{ JmHistogram.pas for Jedi Math Alpha 1.02                                     }
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
{ The Original Code is JmHistogram.pas.                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains histogram analysis code and a visual component.           }
{                                                                              }
{ Unit owner:    Patrick Van Laake                                             }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

unit JmHistogram;
interface

{$i jedimath.inc}

{$ifdef FPC}
{$define NoForms}
{$endif}

{$I JmHistogram.inc}

uses
  SysUtils, Classes 

 {$ifndef NoForms}
  {$ifdef MSWindows}
    ,Controls, Qraphics 
  {$endif}
  {$ifdef Linux}			// ugh ugh Shouldn't these be
					// something with CLX in it?
    ,QControls, QGraphics 
 {$endif}
 {$endif};

type
  TJmCountArray = array of cardinal;

  // Abstract ancestor of all histogram classes. Descendants implement processing
  // of data of a specific type.
  TJmHisto = class
  private
    FHisto    : TJmCountArray;
    FCount,                           // total number of elements in [FMin..FMax]
    FUnder,                           // number of elements < FMin
    FOver     : cardinal;             // number of elements > FMax
    FMin,
    FMax      : double;
    FBins     : integer;
  protected
    function    GetCount(bin: integer): cardinal;
  public
    constructor Create(min, max: double; bins: integer = 512);
    destructor  Destroy; override;

    function    AddData(const data: pointer; num: integer): cardinal; virtual; abstract;
    procedure   Reset;

    property    Min: double read FMin;
    property    Max: double read FMax;
    property    Bins: integer read FBins;
    property    Count[bin: integer]: cardinal read GetCount;
    property    Total: cardinal read FCount;
    property    Histogram: TJmCountArray read FHisto;
  end;

  TJmHistoByte = class(TJmHisto)
  public
    constructor Create(min: byte = 0; max: byte = 255; bins: integer = 256);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoShort = class(TJmHisto)
  public
    constructor Create(min: shortint = -128; max: shortint = 127; bins: integer = 256);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoWord = class(TJmHisto)
  public
    constructor Create(min: word = 0; max: word = 65535; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoSmall = class(TJmHisto)
  public
    constructor Create(min: smallint = -32768; max: smallint = 32767; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoCardinal = class(TJmHisto)
  public
    constructor Create(min, max: cardinal; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoInteger = class(TJmHisto)
  public
    constructor Create(min, max: integer; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoInt64 = class(TJmHisto)
  public
    constructor Create(min, max: Int64; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoSingle = class(TJmHisto)
  public
    constructor Create(min, max: single; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

  TJmHistoDouble = class(TJmHisto)
  public
    constructor Create(min, max: double; bins: integer = 512);
    function    AddData(const data: pointer; num: integer): cardinal; override;
  end;

{$ifndef NoForms}
  // The histogram component
  TJmHistogram = class(TCustomControl)
  private
    FHisto      : TJmHisto;
    FMax        : cardinal;    // max number of elements
    FUserMax    : cardinal;    // User set maximum
    FDataArea   : TRect;       // area where data is drawn, rest is for axes and labels
    FBinLabels  : boolean;
    FValueLabels: boolean;
    FValueLabelsInside: boolean;// if true, value label is written inside the data area
    FAxisColor,
    FDataColor  : TColor;
    FMajorBinTicks,            // number of major ticks, labeled if F*Labels
    FMinorBinTicks,            // number of minor ticks between major ticks
    FMajorValueTicks,
    FMinorValueTicks : integer;
  protected
    procedure   Paint; override;
    procedure   Resize; override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure   SetMax(max: cardinal);
    procedure   SetTick(Index: integer; Value: integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure   AssignHisto(h: TJmHisto);
  published
    // Properties
    property    Align;
    property    Color;
    property    Font;
    property    ShowHint;
    property    MaxValue: cardinal read FUserMax write SetMax;
    property    LabelBins: boolean read FBinLabels write FBinLabels default false;
    property    LabelValues: boolean read FValueLabels write FValueLabels default false;
    property    AxisColor: TColor read FAxisColor write FAxisColor default clRed;
    property    DataColor: TColor read FDataColor write FDataColor default clBlue;
    property    MajorBinTicks: integer index 1 read FMajorBinTicks write SetTick default 3;
    property    MinorBinTicks: integer index 2 read FMinorBinTicks write SetTick default 0;
    property    MajorValueTicks: integer index 3 read FMajorValueTicks write SetTick default 3;
    property    MinorValueTicks: integer index 4 read FMinorValueTicks write SetTick default 0;
    property    ValueLabelsInside: boolean read FValueLabelsInside write FValueLabelsInside default false;

    // Events
    property    OnClick;
    property    OnDblClick;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnEnter;
    property    OnExit;
  end;
{$endif}

  procedure Register;

implementation
uses Math;

////////////////////////////////////////////////////////////////////////////////
// TJmHistogram
constructor TJmHisto.Create(min, max: double; bins: integer = 512);
var i: integer;
begin
  inherited Create;
  Assert((min < max) and (bins > 0) and (bins <= 4096));
  FMin := min;
  FMax := max;

  // Initialize the bins
  FBins := bins;
  SetLength(FHisto, bins);
  for i := 0 to bins - 1 do FHisto[i] := 0;
end;

destructor TJmHisto.Destroy;
begin
  Finalize(FHisto);
  inherited;
end;

function TJmHisto.GetCount(bin: integer): cardinal;
begin
  Assert((bin >= 0) and (bin < Length(FHisto)));
  Result := FHisto[bin];
end;

procedure TJmHisto.Reset;
var i: integer;
begin
  if Length(FHisto) > 0 then
    for i := 0 to High(FHisto) do FHisto[i] := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Fast 8 bit full histogram
procedure _Histogram8BitFull(const data: pointer; num: integer; histo: pointer);
{var i: integer;
begin
  for i := 1 to num do begin
    Inc(histo[data^]);
    Inc(data);
  end;   }
asm
  push   ebx                      // save ebx on the stack
  xor    ebx,ebx                  // ebx := 0

@loop:
  mov    bl,[eax]                 // move byte from data into bl
  inc    dword ptr [ecx+ebx*4]    // Inc(histo[bl])
  inc    eax                      // move to the next byte in data
  dec    edx                      // Dec(num)
  jnz    @loop                    // if num > 0 then loop

  pop    ebx                      // restore ebx from the stack
end;

constructor TJmHistoByte.Create(min: byte = 0; max: byte = 255; bins: integer = 256);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoByte.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    bp: ^byte;
    min, max: byte;
    scale: double;
begin
  if FBins = 256 then begin
    _Histogram8BitFull(data, num, @FHisto[0]);
    Inc(FCount, num);
  end else begin
    bp := data;
    min := Round(FMin);
    max := Round(FMax);
    scale := FBins / (max - min + 1);
    for i := 1 to num do begin
      if bp^ < min then Inc(FUnder)
      else if bp^ > max then Inc(FOver)
      else begin
        Inc(FHisto[Floor((bp^ - min) * scale)]);
        Inc(FCount);
      end;
      Inc(bp);
    end;
  end;
  Result := FCount;
end;

constructor TJmHistoShort.Create(min: shortint = -128; max: shortint = 127; bins: integer = 256);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoShort.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    shp: ^shortint;
    min, max: shortint;
    scale: double;
begin
  if FBins = 256 then begin
    _Histogram8BitFull(data, num, @FHisto[0]);
    Inc(FCount, num);
  end else begin
    shp := data;
    min := Round(FMin);
    max := Round(FMax);
    scale := FBins / (max - min + 1);
    for i := 1 to num do begin
      if shp^ < min then Inc(FUnder)
      else if shp^ > max then Inc(FOver)
      else begin
        Inc(FHisto[Floor((shp^ - min) * scale)]);
        Inc(FCount);
      end;
      Inc(shp);
    end;
  end;
  Result := FCount;
end;

constructor TJmHistoWord.Create(min: word = 0; max: word = 65535; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoWord.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    wp: ^word;
    min, max: word;
    scale: double;
begin
  wp := data;
  min := Round(FMin);
  max := Round(FMax);
  scale := FBins / (max - min + 1);
  for i := 1 to num do begin
    if wp^ < min then Inc(FUnder)
    else if wp^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((wp^ - min) * scale)]);
      Inc(FCount);
    end;
    Inc(wp);
  end;
  Result := FCount;
end;

constructor TJmHistoSmall.Create(min: smallint = -32768; max: smallint = 32767; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoSmall.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    smp: ^smallint;
    min, max: smallint;
    scale: double;
begin
  smp := data;
  min := Round(FMin);
  max := Round(FMax);
  scale := FBins / (max - min + 1);
  for i := 1 to num do begin
    if smp^ < min then Inc(FUnder)
    else if smp^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((smp^ - min) * scale)]);
      Inc(FCount);
    end;
    Inc(smp);
  end;
  Result := FCount;
end;

constructor TJmHistoCardinal.Create(min, max: cardinal; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoCardinal.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    cp: ^cardinal;
    min, max: cardinal;
    scale: double;
begin
  cp := data;
  min := Round(FMin);
  max := Round(FMax);
  scale := FBins / (max - min + 1);
  for i := 1 to num do begin
    if cp^ < min then Inc(FUnder)
    else if cp^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((cp^ - min) * scale)]);
      Inc(FCount);
    end;
    Inc(cp);
  end;
  Result := FCount;
end;

constructor TJmHistoInteger.Create(min, max: integer; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoInteger.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    ip: ^integer;
    min, max: integer;
    scale: double;
begin
  ip := data;
  min := Round(FMin);
  max := Round(FMax);
  scale := FBins / (max - min + 1);
  for i := 1 to num do begin
    if ip^ < min then Inc(FUnder)
    else if ip^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((ip^ - min) * scale)]);
      Inc(FCount);
    end;
    Inc(ip);
  end;
  Result := FCount;
end;

constructor TJmHistoInt64.Create(min, max: Int64; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoInt64.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    ip: ^Int64;
    min, max: Int64;
    scale: double;
begin
  ip := data;
  min := Round(FMin);
  max := Round(FMax);
  scale := FBins / (max - min + 1);
  for i := 1 to num do begin
    if ip^ < min then Inc(FUnder)
    else if ip^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((ip^ - min) * scale)]);
      Inc(FCount);
    end;
    Inc(ip);
  end;
  Result := FCount;
end;

constructor TJmHistoSingle.Create(min, max: single; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoSingle.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    sp: ^single;
    scale: double;
begin
  sp := data;
  scale := FBins / (FMax - FMin);
  for i := 1 to num do begin
    if sp^ < FMin then Inc(FUnder)
    else if sp^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((sp^ - FMin) * scale)]);
      Inc(FCount);
    end;
    Inc(sp);
  end;
  Result := FCount;
end;

constructor TJmHistoDouble.Create(min, max: double; bins: integer = 512);
begin
  inherited Create(min, max, bins);
end;

function TJmHistoDouble.AddData(const data: pointer; num: integer): cardinal;
var i: integer;
    dp: ^double;
    scale: double;
begin
  dp := data;
  scale := FBins / (FMax - FMin);
  for i := 1 to num do begin
    if dp^ < FMin then Inc(FUnder)
    else if dp^ > max then Inc(FOver)
    else begin
      Inc(FHisto[Floor((dp^ - FMin) * scale)]);
      Inc(FCount);
    end;
    Inc(dp);
  end;
  Result := FCount;
end;

////////////////////////////////////////////////////////////////////////////////
// TJmHistogram
{$ifndef NoForms}
constructor TJmHistogram.Create(AOwner: TComponent);
begin
  inherited;
  FBinLabels := false;
  FValueLabels := false;
  FValueLabelsInside := false;
  FAxisColor := clRed;
  FDataColor := clBlue;
  FMajorBinTicks := 3;
  FMinorBinTicks := 0;
  FMajorValueTicks := 3;
  FMinorValueTicks := 0;
  FHisto := nil;
end;

procedure TJmHistogram.AssignHisto(h: TJmHisto);
var i: integer;
begin
  FHisto := h;
  if FUserMax = 0 then begin
    FMax := FHisto.Count[0];
    for i := 1 to FHisto.Bins - 1 do
      if FMax < FHisto.Count[i] then FMax := FHisto.Count[i];
  end else FMax := FUserMax;
  Resize;
  Repaint;
end;

procedure TJmHistogram.Paint;
var i, j, bins, line, len, len2: integer;
    hscale, vscale: double;
    str: string;
begin
  inherited;
  // Erase the background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  Canvas.Pen.Color := FAxisColor;

  // Need some data and space to work with, otherwise exit
  if (FHisto = nil) or (ClientWidth < 10) or (ClientHeight < 10) then begin
    Canvas.MoveTo(2, ClientHeight - 5);
    Canvas.LineTo(ClientWidth - 2, ClientHeight - 5);
    Canvas.MoveTo(4, 2);
    Canvas.LineTo(4, ClientHeight - 2);
    Exit;
  end;

  // Data scaling
  bins := FHisto.Bins;
  hscale := (FDataArea.Right - FDataArea.Left + 1) / bins;
  vscale := (FDataArea.Bottom - FDataArea.Top + 1) / FMax;

  // Draw the axes and the ticks
  Canvas.MoveTo(FDataArea.Left - 1, FDataArea.Top);
  Canvas.LineTo(FDataArea.Left - 1, FDataArea.Bottom + 5);
  if (FMajorValueTicks > 0) and (FMajorValueTicks < (FDataArea.Bottom - FDataArea.Top)) then begin
    len := Round((FDataArea.Bottom - FDataArea.Top + 1) / FMajorValueTicks);
    for i := 0 to FMajorValueTicks - 1 do begin
      Canvas.MoveTo(FDataArea.Left - 5, FDataArea.Top + len * i);
      Canvas.LineTo(FDataArea.Left - 1, FDataArea.Top + len * i);
      if FMinorValueTicks > 0 then begin
        len2 := Round(len / (FMinorValueTicks + 1));
        for j := 1 to FMinorValueTicks do begin
          Canvas.MoveTo(FDataArea.Left - 3, FDataArea.Top + len * i + len2 * j);
          Canvas.LineTo(FDataArea.Left - 1, FDataArea.Top + len * i + len2 * j);
        end;
      end;
    end;
  end;
  // Bin axis
  Canvas.MoveTo(FDataArea.Left - 5, FDataArea.Bottom + 1);
  Canvas.LineTo(FDataArea.Right, FDataArea.Bottom + 1);
  if (FMajorBinTicks > 0) and (FMajorBinTicks < FHisto.Bins) and
     (FMajorBinTicks < (FDataArea.Right - FDataArea.Left)) then begin
    len := Round((FDataArea.Right - FDataArea.Left + 1) / FMajorBinTicks);
    for i := 0 to FMajorBinTicks - 1 do begin
      Canvas.MoveTo(FDataArea.Right - len * i, FDataArea.Bottom + 1);
      Canvas.LineTo(FDataArea.Right - len * i, FDataArea.Bottom + 5);
      if FMinorBinTicks > 0 then begin
        len2 := Round(len / (FMinorBinTicks + 1));
        for j := 1 to FMinorBinTicks do begin
          Canvas.MoveTo(FDataArea.Right - len * i - len2 * j, FDataArea.Bottom + 1);
          Canvas.LineTo(FDataArea.Right - len * i - len2 * j, FDataArea.Bottom + 3);
        end;
      end;
    end;
  end;

  // Draw the data
  Canvas.Pen.Color := FDataColor;
  for i := 0 to bins - 1 do begin
    line := FDataArea.Left + Trunc(i * hscale);
    if FHisto.Count[i] >= FMax then len := FDataArea.Bottom - FDataArea.Top + 1
    else len := Round(FHisto.Count[i] * vscale);
    Canvas.MoveTo(line, FDataArea.Bottom);
    Canvas.LineTo(line, FDataArea.Bottom - len);
  end;

  // Label the axes and the ticks
  Canvas.Font := Font;
  j := Abs(Font.Height) div 2;
  if FValueLabels and (FMajorValueTicks > 0) then begin
    len2 := Round((FDataArea.Bottom - FDataArea.Top + 1) / FMajorValueTicks);
    for i := 1 to FMajorValueTicks do begin
      str := IntToStr(Round(FMax * i / FMajorValueTicks));
      if FValueLabelsInside then len := FDataArea.Left + 1
      else len := FDataArea.Left - Canvas.TextWidth(str) - 6;
      Canvas.TextOut(len, FDataArea.Bottom - len2 * i - j, str);
    end;
  end;
  if FBinLabels and (FMajorBinTicks > 0) then begin
    len := Canvas.TextWidth(FloatToStrF(FHisto.Min, ffFixed, 12, 1));
    Canvas.TextOut(FDataArea.Left - len div 2, FDataArea.Bottom + 7, FloatToStrF(FHisto.Min, ffFixed, 12, 1));
    len2 := Round((FDataArea.Right - FDataArea.Left + 1) / FMajorBinTicks);
    for i := 1 to FMajorBinTicks do begin
      str := FloatToStrF(FHisto.Min + (FHisto.Max - FHisto.Min) * i / FMajorBinTicks, ffFixed, 12, 1);
      len := FDataArea.Left + len2 * i - Canvas.TextWidth(str) div 2;
      Canvas.TextOut(len, FDataArea.Bottom + 7, str);
    end;
  end;
end;

procedure TJmHistogram.Resize;
var h, len: integer;
begin
  inherited;
  if (ClientWidth < 10) or (ClientHeight < 10) then
    FDataArea := Rect(0, 0, 0, 0)
  else begin
    // Top & Bottom
    h := Abs(Font.Height);
    if FValueLabels then begin
      FDataArea.Top := 3 + h div 2;
      if FBinLabels then FDataArea.Bottom := ClientHeight - 9 - h
      else FDataArea.Bottom := ClientHeight - Max(9, h div 2);
    end else begin
      FDataArea.Top := 3;
      if FBinLabels then FDataArea.Bottom := ClientHeight - 9 - h
      else FDataArea.Bottom := ClientHeight - 9;
    end;

    // Left & Right
    Canvas.Font := Font;
    len := Canvas.TextWidth(FloatToStrF(FMax, ffFixed, 12, 1));
    if FBinLabels then begin
      if Assigned(FHisto) then begin
        if FValueLabels and not FValueLabelsInside then
          FDataArea.Left := 6 + Max(len, Canvas.TextWidth(FloatToStrF(FHisto.Min, ffFixed, 12, 1)) div 2)
        else FDataArea.Left := 6 + Canvas.TextWidth(FloatToStrF(FHisto.Min, ffFixed, 12, 1)) div 2;
        FDataArea.Right := ClientWidth - 6 - len div 2;
      end else begin
        FDataArea.Left := 6 + Canvas.TextWidth('0') div 2;
        FDataArea.Right := ClientWidth - 6 - Canvas.TextWidth('4096') div 2;
      end;
    end else begin
      FDataArea.Left := 6;
      FDataArea.Right := ClientWidth - 6;
    end;
  end;
end;

procedure TJmHistogram.MouseMove(Shift: TShiftState; X, Y: Integer);
var ndx: integer;
begin
  inherited;
  if Assigned(FHisto) and (X >= FDataArea.Left) and (X <= FDataArea.Right) and
     (Y >= FDataArea.Top) and (Y <= FDataArea.Bottom) then begin
    ndx := Min(Round((X - FDataArea.Left) * FHisto.Bins / (FDataArea.Right - FDataArea.Left + 1)), FHisto.Bins - 1);
    Hint := Format('Bin: %d'#13#10'Value: %d', [ndx, FHisto.Count[ndx]]);
  end else Hint := '';
end;

procedure TJmHistogram.SetMax(max: cardinal);
begin
  FUserMax := max;
  FMax := max;
  Resize;
  Repaint;
end;

procedure TJmHistogram.SetTick(Index: integer; Value: integer);
begin
  case Index of
    1: FMajorBinTicks := Max(0, Value);
    2: FMinorBinTicks := Min(Max(0, Value), 9);
    3: FMajorValueTicks := Max(0, Value);
    4: FMinorValueTicks := Min(Max(0, Value), 9);
  end;
end;
{$endif}
procedure Register;
begin
 {$ifndef NoForms}
  RegisterComponents('Samples', [TJmHistogram]);
 {$endif}
end;

end.


