{
    $Id$

    KCLSHEdit - SHEdit Widget Implementation for KCL
    Copyright (C) 1999 - 2000  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}
{$H+}

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

unit KCLSHEdit;
interface
uses
  SysUtils, Classes,
  KCL,
  doc_text, SHEdit;

const
  colInvalid = $ff000000;
  colDefault = $ffffffff;

type

  TSHFontStyle = (fsNormal, fsBold, fsItalics, fsBoldItalics);

  TSHStyle = record
    Name: String[32];
    Color, Background: LongWord;
    FontStyle: TSHFontStyle;
  end;

  TSHStyleArray = array[1..255] of TSHStyle;  // Notice the 1!
  PSHStyleArray = ^TSHStyleArray;

  TKCLSHWidget = class(TScrollers)
  protected
    WidgetIface: ISHWidget;
    SHStyles: PSHStyleArray;
    SHStyleCount: Integer;              // # of currently registered styles
    shWhitespace: Integer;

    Timer: TTimer;
    PaintBox: TPaintBox;
    HorzScroll, VertScroll: Integer;	// Current scrolling offsets
    FLeftIndent: Integer;		// Left indent (border) in pixel (!)
    FDefMaxTextWidth: Integer;          // Position of vertical bar, in chars
    FMinHorzVisible, FMinVertVisible, FDefHorzVisible, FDefVertVisible: Integer;
    CharW, CharH: Integer;
    Fonts: array[TSHFontStyle] of TFont; // Fonts for content drawing
    Canvas: TCanvas;
    FDoc: TTextDoc;
    FEditor: TSHTextEdit;

    FFontName: String;
    FFontSize: Integer;
    CursorVisible: Boolean;
    MouseSelStartX, MouseSelStartY: Integer;

    procedure LineInserted(Sender: TSHTextEdit; Line: Integer);
    procedure LineRemoved(Sender: TSHTextEdit; Line: Integer);
    procedure ScrollerChange(Sender: TObject);
    procedure TimerTick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; ACanvas: TCanvas; const rect: TRect);
    function  KeyPressed(Sender: TObject; key: Char; KeyCode: LongWord;
      ShiftState: TShiftState): Boolean;
    procedure PaintboxMouseButtonDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MouseX, MouseY: Integer);
    procedure PaintboxMouseMove(Sender: TObject; Shift: TShiftState;
      MouseX, MouseY: Integer);
    procedure PaintboxFocusIn(Sender: TObject);
    procedure PaintboxFocusOut(Sender: TObject);

    procedure DoRecalcLayout; override;
    procedure ApplySize; override;
    procedure SetMinHorzVisible(AChars: Integer);
    procedure SetMinVertVisible(AChars: Integer);
    procedure SetDefHorzVisible(AChars: Integer);
    procedure SetDefVertVisible(AChars: Integer);

    // The "real" ISHWidget Implemenation:

    procedure InvalidateRect(x, y, w, h: Integer);

    // Drawing
    procedure ClearRect(x, y, w, h: Integer);
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar);

    // Cursor
    procedure ShowCursor(x, y: Integer);
    procedure HideCursor(x, y: Integer);

    // Scrolling support
    function  GetHorzPos: Integer;
    procedure SetHorzPos(x: Integer);
    function  GetVertPos: Integer;
    procedure SetVertPos(y: Integer);
    function  GetPageWidth: Integer;
    function  GetPageHeight: Integer;
    function  GetLineWidth: Integer;
    procedure SetLineWidth(w: Integer);
    function  GetLineCount: Integer;
    procedure SetLineCount(count: Integer);

    // Clipboard support
    function  GetClipboard: String;
    procedure SetClipboard(AContent: String);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupEditor(ADoc: TTextDoc; AEditClass: TSHTextEditClass);

    function  AddSHStyle(const AName: String; AColor, ABackground: LongWord;
      AStyle: TSHFontStyle): Integer;

    property Document: TTextDoc read FDoc;
    property Editor: TSHTextEdit read FEditor;
    property LeftIndent: Integer read FLeftIndent write FLeftIndent;
    DrawVBar: Boolean;
    property DefMaxTextWidth: Integer read FDefMaxTextWidth write FDefMaxTextWidth;

    property MinHorzVisible: Integer read FMinHorzVisible write SetMinHorzVisible default 1;
    property MinVertVisible: Integer read FMinVertVisible write SetMinVertVisible default 1;
    property DefHorzVisible: Integer read FDefHorzVisible write SetDefHorzVisible default 5;
    property DefVertVisible: Integer read FDefVertVisible write SetDefVertVisible default 3;
  end;


// ===================================================================
// ===================================================================

implementation


type

  {This is just a 'forwarding' class, which delegates all calls to a
   TKCLSHWidget object. It can be removed as soon as the compiler supports
   interface classes.}

  TKCLWidgetIf = class(ISHWidget)
    Widget: TKCLSHWidget;
    procedure InvalidateRect(x, y, w, h: Integer); override;
    procedure ClearRect(x, y, w, h: Integer); override;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); override;
    procedure ShowCursor(x, y: Integer); override;
    procedure HideCursor(x, y: Integer); override;
    function  GetHorzPos: Integer; override;
    procedure SetHorzPos(x: Integer); override;
    function  GetVertPos: Integer; override;
    procedure SetVertPos(y: Integer); override;
    function  GetPageWidth: Integer; override;
    function  GetPageHeight: Integer; override;
    function  GetLineWidth: Integer; override;
    procedure SetLineWidth(w: Integer); override;
    function  GetLineCount: Integer; override;
    procedure SetLineCount(count: Integer); override;
    function  GetClipboard: String; override;
    procedure SetClipboard(Content: String); override;
  end;

procedure TKCLWidgetIf.InvalidateRect(x, y, w, h: Integer);	   begin Widget.InvalidateRect(x, y, w, h) end;
procedure TKCLWidgetIf.ClearRect(x, y, w, h: Integer);	   	   begin Widget.ClearRect(x, y, w, h) end;
procedure TKCLWidgetIf.DrawTextLine(x1, x2, y: Integer; s: PChar); begin Widget.DrawTextLine(x1, x2, y, s) end;
procedure TKCLWidgetIf.ShowCursor(x, y: Integer);		   begin Widget.ShowCursor(x, y) end;
procedure TKCLWidgetIf.HideCursor(x, y: Integer);		   begin Widget.HideCursor(x, y)  end;
function  TKCLWidgetIf.GetHorzPos: Integer;			   begin Result := Widget.GetHorzPos end;
procedure TKCLWidgetIf.SetHorzPos(x: Integer);			   begin Widget.SetHorzPos(x) end;
function  TKCLWidgetIf.GetVertPos: Integer;			   begin Result := Widget.GetVertPos end;
procedure TKCLWidgetIf.SetVertPos(y: Integer);			   begin Widget.SetVertPos(y) end;
function  TKCLWidgetIf.GetPageWidth: Integer;			   begin Result := Widget.GetPageWidth end;
function  TKCLWidgetIf.GetPageHeight: Integer;			   begin Result := Widget.GetPageHeight end;
function  TKCLWidgetIf.GetLineWidth: Integer;			   begin Result := Widget.GetLineWidth end;
procedure TKCLWidgetIf.SetLineWidth(w: Integer);		   begin Widget.SetLineWidth(w) end;
function  TKCLWidgetIf.GetLineCount: Integer;			   begin Result := Widget.GetLineCount end;
procedure TKCLWidgetIf.SetLineCount(count: Integer);		   begin Widget.SetLineCount(count) end;
function  TKCLWidgetIf.GetClipboard: String;			   begin Result := Widget.GetClipboard end;
procedure TKCLWidgetIf.SetClipboard(Content: String);		   begin Widget.SetClipboard(Content) end;



constructor TKCLSHWidget.Create(AOwner: TComponent);
var
  r: TKCLWidgetIf;
  i: Integer;
  f: TFont;
begin
  inherited Create(AOwner);

  // Set up the interface adapter
  r := TKCLWidgetIf.Create;
  r.Widget := Self;
  WidgetIface := r;

  // Create Fonts
  FFontName := 'courier';
  FFontSize := 14;
  for i := 0 to 3 do begin
    f := TFont.Create;
    f.FontName := FFontName;
    f.Height := FFontSize;
    f.Bold := (i and 1) <> 0;
    f.Italics := (i and 2) <> 0;
    Fonts[TSHFontStyle(i)] := f;
  end;
  CharW := Fonts[fsBold].GetTextWidth(' ');
  CharH := FFontSize + 3;   // *** find better way to determine max. cell height

  FLeftIndent := CharW div 2;

  FMinHorzVisible := 1;
  FMinVertVisible := 1;
  FDefHorzVisible := 5;
  FDefVertVisible := 3;

  PaintBox := TPaintBox.Create(Self);
  PaintBox.Name := 'PaintBox';
  Content := PaintBox;
end;

destructor TKCLSHWidget.Destroy;
var
  i: Integer;
begin
  Timer.Free;
  FEditor.Free;

  for i := 0 to 3 do
    Fonts[TSHFontStyle(i)].Free;
  WidgetIface.Free;

  FreeMem(SHStyles);

  inherited Destroy;
end;

procedure TKCLSHWidget.SetupEditor(ADoc: TTextDoc; AEditClass: TSHTextEditClass);
var
  action: TKeyboardActionDescr;
begin
  FDoc := ADoc;
  FEditor := AEditClass.Create(ADoc, WidgetIface);
  shWhitespace       := AddSHStyle('Whitespace', colBlack, colWhite, fsNormal);
  FEditor.shDefault  := AddSHStyle('Default',    colBlack, colWhite, fsNormal);
  FEditor.shSelected := AddSHStyle('Selected',   colWhite, colBlack, fsNormal);

  // Set up default keys
  FEditor.AddKeyDef(@FEditor.CursorUp, selClear, 'Cursor up', keyUp, []);
  FEditor.AddKeyDef(@FEditor.CursorDown, selClear, 'Cursor down', keyDown, []);
  FEditor.AddKeyDef(@FEditor.CursorLeft, selClear, 'Cursor left', keyLeft, []);
  FEditor.AddKeyDef(@FEditor.CursorRight, selClear, 'Cursor right', keyRight, []);
  FEditor.AddKeyDef(@FEditor.CursorHome, selClear, 'Cursor to start of line', keyHome, []);
  FEditor.AddKeyDef(@FEditor.CursorEnd, selClear, 'Cursor to end of line', keyEnd, []);
  FEditor.AddKeyDef(@FEditor.CursorPageUp, selClear, 'Cursor PageUp', keyPageUp, []);
  FEditor.AddKeyDef(@FEditor.CursorPageDown, selClear, 'Cursor PageDown', keyPageDown, []);

  FEditor.AddKeyDef(@FEditor.CursorUp, selExtend, 'Selection up', keyUp, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorDown, selExtend, 'Selection down', keyDown, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorLeft, selExtend, 'Selection left', keyLeft, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorRight, selExtend, 'Selection right', keyRight, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorHome, selExtend, 'Selection Home', keyHome, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorEnd, selExtend, 'Selection Home', keyEnd, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorPageUp, selExtend, 'Selection PageUp', keyPageUp, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorPageDown, selExtend, 'Selection PageDown', keyPageDown, [ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorDocBegin, selExtend, 'Selection Document Start', keyPageUp, [ssCtrl,ssShift]);
  FEditor.AddKeyDef(@FEditor.CursorDocEnd, selExtend, 'Selection Document End', keyPageDown, [ssCtrl,ssShift]);

  FEditor.AddKeyDef(@FEditor.ToggleOverwriteMode, selClear, 'Toggle overwrite mode', keyInsert, []);
  FEditor.AddKeyDef(@FEditor.EditDelLeft, selClear, 'Delete char left of cursor', keyBackspace, []);
  FEditor.AddKeyDef(@FEditor.EditDelRight, selClear, 'Delete char right of cursor', keyDelete, []);
  FEditor.AddKeyDef(@FEditor.EditDelLine, selClear, 'Delete current line', Ord('Y'), [ssCtrl]);
  FEditor.AddKeyDef(@FEditor.EditUndo, selClear, 'Undo last action', keyBackspace, [ssAlt]);
  FEditor.AddKeyDef(@FEditor.EditRedo, selClear, 'Redo last undone action', keyBackspace, [ssShift, ssAlt]);

  action := FEditor.AddKeyboardAction(@FEditor.ClipboardCut, selClear, 'Move text selection to clipboard');
  FEditor.AddKeyboardAssignment(keyDelete, [ssShift], action);
  FEditor.AddKeyboardAssignment(Ord('X'), [ssCtrl], action);

  action := FEditor.AddKeyboardAction(@FEditor.ClipboardCopy, selNothing, 'Copy text selection to clipboard');
  FEditor.AddKeyboardAssignment(keyInsert, [ssCtrl], action);
  FEditor.AddKeyboardAssignment(Ord('C'), [ssCtrl], action);

  action := FEditor.AddKeyboardAction(@FEditor.ClipboardPaste, selClear, 'Insert content of clipboard');
  FEditor.AddKeyboardAssignment(keyInsert, [ssShift], action);
  FEditor.AddKeyboardAssignment(Ord('V'), [ssCtrl], action);

  // Now the paintbox can be set up correctly:
  PaintBox.OnPaint := @PaintBoxPaint;
  PaintBox.OnKey := @KeyPressed;
  PaintBox.OnMouseButtonDown := @PaintboxMouseButtonDown;
  PaintBox.OnMouseMove := @PaintboxMouseMove;
  PaintBox.OnFocusIn := @PaintboxFocusIn;
  PaintBox.OnFocusOut := @PaintboxFocusOut;

  HorzRange.OnValueChange := @ScrollerChange;
  VertRange.OnValueChange := @ScrollerChange;

  FEditor.OnLineInsert := @LineInserted;
  FEditor.OnLineRemove := @LineRemoved;

  Timer := TTimer.Create(Self);
  Timer.Name := 'Timer';
  Timer.Interval := 55;
//  Timer.OnTimer := @TimerTick;
  Timer.Enabled := False;
end;


procedure TKCLSHWidget.LineInserted(Sender: TSHTextEdit; Line: Integer);
var
  lcanvas: TCanvas;
begin
  if not Assigned(PaintBox.Handle) then exit;
  lcanvas := PaintBox.CreateCanvas;
  lcanvas.CopyRect(0, (Line + 1) * CharH - VertScroll, PaintBox.Width, PaintBox.Height,
    0, Line * CharH - VertScroll);
  lcanvas.Free;
end;

procedure TKCLSHWidget.LineRemoved(Sender: TSHTextEdit; Line: Integer);
var
  lcanvas: TCanvas;
  sy, dy, h: Integer;
begin
  if (not Assigned(PaintBox.Handle)) or (Line * CharH > VertScroll + PaintBox.Height) then
    exit;

  sy := (Line + 1) * CharH - VertScroll;
  dy := Line * CharH - VertScroll;
  h := PaintBox.Height;
  if sy + h > PaintBox.Height then
    h := PaintBox.Height - sy;
  if dy + h > PaintBox.Height then
    h := PaintBox.Height - dy;

  lcanvas := PaintBox.CreateCanvas;
  lcanvas.CopyRect(0, dy, PaintBox.Width, h, 0, sy);
  lcanvas.Free;
  PaintBox.Redraw(0, PaintBox.Height - CharH, PaintBox.Width, CharH, True);
end;

procedure TKCLSHWidget.ScrollerChange(Sender: TObject);
var
  OldHorz, OldVert: Integer;
begin
  OldHorz := HorzScroll;
  OldVert := VertScroll;
  HorzScroll := HorzRange.CurValue;
  VertScroll := VertRange.CurValue;
  PaintBox.ScrollBy(HorzScroll - OldHorz, VertScroll - OldVert, False);
end;

procedure TKCLSHWidget.TimerTick(Sender: TObject);
var
  dx, dy: Integer;
begin
  if HorzScroll <> HorzRange.CurValue then begin
    dx := (HorzRange.CurValue - HorzScroll) * 3 div 4;
    Inc(HorzScroll, dx);
  end else
    dx := 0;
  if VertScroll <> VertRange.CurValue then begin
    dy := (VertRange.CurValue - VertScroll) * 3 div 4;
    Inc(VertScroll, dy);
  end else
    dy := 0;

  if (dx <> 0) or (dy <> 0) then
    PaintBox.ScrollBy(dx, dy, False);
end;

procedure TKCLSHWidget.PaintBoxPaint(Sender: TObject; ACanvas: TCanvas;
  const rect: TRect);
var
  OldCanvas: TCanvas;
  px, x, y: Integer;
  r: TRect;
begin
  ASSERT(Assigned(ACanvas));
  OldCanvas := Canvas;	// necessary for nested paint events!
  Canvas := ACanvas;

  if DrawVBar then begin
    // Draw vertical lines
    px := FLeftIndent - 4 - HorzScroll;
    if rect.Left <= px then begin
      Canvas.Color := colBlack;
      Canvas.Line(px, rect.Top, px, rect.Bottom);
      Canvas.Color := colGray;
      Canvas.Line(px - 1, rect.Top, px - 1, rect.Bottom);
      Canvas.Line(px - 3, rect.Top, px - 3, rect.Bottom);
      Canvas.Color := colWhite;
      Canvas.Line(px - 2, rect.Top, px - 2, rect.Bottom);
    end;

    // Erase left bar
    r := rect;
    r.Left := 0;
    r.Right := FLeftIndent - 7 - HorzScroll;
    if r.Right >= r.Left then begin
      Canvas.Color := colLightGray;
      Canvas.FillRect(r);
    end;
    r.Left := FLeftIndent - 3 - HorzScroll;
    r.Right := FLeftIndent - HorzScroll;
    Canvas.Color := SHStyles^[shWhitespace].Background;
    Canvas.FillRect(r);
  end else begin
    Canvas.Color := SHStyles^[shWhitespace].Background;
    Canvas.FillRect(0, rect.Top, FLeftIndent - HorzScroll, rect.Bottom - rect.Top + 1);
  end;

  // Draw text
  r := rect;
  Inc(r.Left, HorzScroll);
  Inc(r.Top, VertScroll);
  Inc(r.Right, HorzScroll);
  Inc(r.Bottom, VertScroll);
  x := (r.Left - FLeftIndent) div CharW;
  y := r.Top div CharH;
  FEditor.DrawContent(x, y, (r.Right - FLeftIndent + CharW + 1) div CharW - x,
    (r.Bottom + CharH - 1) div CharH - y);

  if FDefMaxTextWidth > 0 then begin
    px := FLeftIndent + FDefMaxTextWidth * CharW - HorzScroll;
    Canvas.Color := colGray;
    Canvas.Line(px, rect.Top, px, rect.Bottom);
  end;

  Canvas := OldCanvas;
end;

function TKCLSHWidget.KeyPressed(Sender: TObject; key: Char; KeyCode: LongWord;
  ShiftState: TShiftState): Boolean;
begin
  // WriteLn('Key pressed: Key=#', Ord(key), ', KeyCode=', KeyCode);
  if key <> #0 then
    Result := not FEditor.KeyPressed(Ord(key), ShiftState)
  else
    Result := not FEditor.KeyPressed(KeyCode, ShiftState);
end;

procedure TKCLSHWidget.PaintboxMouseButtonDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; mx, my: Integer);
begin
  if (Button <> mbLeft) or (mx < FLeftIndent) or (not Assigned(FDoc)) then
    exit;
  MouseSelStartX := (mx - FLeftIndent + CharW div 2 + HorzScroll) div CharW;
  MouseSelStartY :=  (my + VertScroll) div CharH;
  if MouseSelStartY >= FDoc.LineCount then
    MouseSelStartY := FDoc.LineCount - 1;
  FEditor.StartSelectionChange;
  FEditor.CursorX := MouseSelStartX;
  FEditor.CursorY := MouseSelStartY;
  FEditor.Selection.Clear;
  FEditor.EndSelectionChange;
  PaintBox.SetFocus;
end;

procedure TKCLSHWidget.PaintboxMouseMove(Sender: TObject; Shift: TShiftState;
  mx, my: Integer);
begin
  if (mx < FLeftIndent) or (not Assigned(FDoc)) then exit;
  if ssLeft in Shift then begin
    mx := (mx - FLeftIndent + CharW div 2 + HorzScroll) div CharW;
    my := (my + VertScroll) div CharH;
    if my >= FDoc.LineCount then
      my := FDoc.LineCount - 1;
    FEditor.StartSelectionChange;
    FEditor.Selection.StartX := MouseSelStartX;
    FEditor.Selection.StartY := MouseSelStartY;
    FEditor.Selection.EndX := mx;
    FEditor.Selection.EndY := my;
    FEditor.CursorX := mx;
    FEditor.CursorY := my;
    FEditor.EndSelectionChange;
  end;
end;

procedure TKCLSHWidget.PaintboxFocusIn(Sender: TObject);
begin
  FEditor.FocusIn;
end;

procedure TKCLSHWidget.PaintboxFocusOut(Sender: TObject);
begin
  FEditor.FocusOut;
end;


function TKCLSHWidget.AddSHStyle(const AName: String; AColor, ABackground: LongWord;
  AStyle: TSHFontStyle): Integer;
begin
  ReAllocMem(SHStyles, SizeOf(TSHStyle) * (SHStyleCount + 1));
  Inc(SHStyleCount);
  SHStyles^[SHStyleCount].Name       := AName;
  SHStyles^[SHStyleCount].Color      := AColor;
  SHStyles^[SHStyleCount].Background := ABackground;
  SHStyles^[SHStyleCount].FontStyle  := AStyle;
  Result := SHStyleCount;
end;


procedure TKCLSHWidget.InvalidateRect(x, y, w, h: Integer);
begin
//WriteLn('  Invalidate ', x, '/', y, ', ', w, 'x', h);
  PaintBox.Redraw(FLeftIndent + x * CharW - HorzScroll, y * CharH - VertScroll,
    w * CharW, h * CharH, True);
end;

procedure TKCLSHWidget.ClearRect(x, y, w, h: Integer);
var
  DestroyCanvas: Boolean;
begin
//WriteLn('ClearRect: ', x, '/', y, ', ', w, 'x', h);

  if Assigned(Canvas) then
    DestroyCanvas := False
  else begin
    DestroyCanvas := True;
    Canvas := PaintBox.CreateCanvas;
  end;

  Canvas.Color := SHStyles^[shWhitespace].Background;
  Canvas.FillRect(FLeftIndent + x * CharW - HorzScroll,
    y * CharH - VertScroll, w * CharW, h * CharH);

  if DestroyCanvas then begin
    Canvas.Free;
    Canvas := nil;
  end;
end;

procedure TKCLSHWidget.DrawTextLine(x1, x2, y: Integer; s: PChar);
var
  CurColor: LongWord;
  CurX1, CurX2: Integer;

  procedure DoErase;
  begin
    Canvas.Color := CurColor;
    if CurX2 >= x1 then
      if (CurX1 < FDefMaxTextWidth) and (CurX2 >= FDefMaxTextWidth) then begin
        // Prevent flickering of the vertical text limiter line :-)
        Canvas.FillRect(CurX1 * CharW + FLeftIndent - HorzScroll, y * CharH - VertScroll,
	  (FDefMaxTextWidth - CurX1) * CharW, CharH);
        Canvas.FillRect(FDefMaxTextWidth * CharW + FLeftIndent - HorzScroll + 1, y * CharH - VertScroll,
	  (CurX2 - CurX1 + 1) * CharW + 1, CharH);
      end else
        Canvas.FillRect(CurX1 * CharW + FLeftIndent - HorzScroll, y * CharH - VertScroll,
	  (CurX2 - CurX1 + 1) * CharW, CharH);
    CurX1 := CurX2 + 1;
  end;

var
  RequestedColor: Integer;
  NewColor: LongWord;
  hs: PChar;
begin
//  WriteLn('DrawTextLine: x1=', x1, ', x2=', x2, ', y=', y);
  ASSERT(Assigned(Canvas));

  // Erase the (potentially multi-coloured) background
  hs := s;
  CurColor := SHStyles^[shWhitespace].Background;

  CurX1 := x1;
  CurX2 := -1;
  while (hs[0] <> #0) and (CurX2 <= x2) do begin
    case hs[0] of
      LF_Escape: begin
          NewColor := SHStyles^[Ord(hs[1])].Background;
          if NewColor = colDefault then
            NewColor := SHStyles^[shWhitespace].Background;
          if NewColor <> CurColor then begin
	      DoErase;
            CurColor := NewColor;
          end;
          Inc(hs, 2);
        end;
      #9: begin
          repeat
            Inc(CurX2);
          until (CurX2 and 7) = 0;
          Inc(hs);
        end;
      else begin
        Inc(hs);
        Inc(CurX2);
      end;
    end;
  end;
  CurX2 := x2;
  DoErase;


  // Draw text line

  RequestedColor := shWhitespace;
  CurX1 := 0;
  while s[0] <> #0 do
    case s[0] of
      LF_Escape: begin
          RequestedColor := Ord(s[1]);
          Inc(s, 2);
        end;
      #9: begin
          repeat
            Inc(CurX1);
          until (CurX1 and 7) = 0;
          Inc(s);
        end;
      ' ': begin
          Inc(s);
          Inc(CurX1);
        end;
      else begin
        if (CurX1 >= x1) and (CurX1 <= x2) then begin
          Canvas.Color := SHStyles^[RequestedColor].Color;
	  Canvas.Font := Fonts[SHStyles^[RequestedColor].FontStyle];
	  Canvas.Text(CurX1 * CharW + FLeftIndent - HorzScroll, y * CharH - VertScroll, s[0]);
        end;
        Inc(s);
        Inc(CurX1);
      end;
    end;
end;

procedure TKCLSHWidget.ShowCursor(x, y: Integer);
var
  px, py: Integer;
  lcanvas: TCanvas;
begin
  px := FLeftIndent + x * CharW - HorzScroll;
  py := y * CharH - VertScroll;
  lcanvas := PaintBox.CreateCanvas;
  lcanvas.Color := colBlack;
  lcanvas.Line(px, py, px, py + CharH - 1);
  Inc(px);
  lcanvas.Line(px, py, px, py + CharH - 1);
  lcanvas.Free;
end;

procedure TKCLSHWidget.HideCursor(x, y: Integer);
begin
  PaintBox.Redraw(FLeftIndent + x * CharW - HorzScroll, y * CharH - VertScroll, 2, CharH, True);
end;

function TKCLSHWidget.GetHorzPos: Integer;
begin
  // Do NOT use LeftIndent here!
  Result := HorzScroll div CharW;
end;

procedure TKCLSHWidget.SetHorzPos(x: Integer);
begin
  // Do NOT use LeftIndent here!
  HorzRange.CurValue := x * CharW;
end;

function TKCLSHWidget.GetVertPos: Integer;
begin
  Result := (VertScroll + CharH - 1) div CharH;
end;

procedure TKCLSHWidget.SetVertPos(y: Integer);
begin
  VertRange.CurValue := y * CharH;
end;

function TKCLSHWidget.GetPageWidth: Integer;
begin
  Result := (PaintBox.Width - FLeftIndent + CharW - 1) div CharW;
end;

function TKCLSHWidget.GetPageHeight: Integer;
begin
  Result := PaintBox.Height div CharH;
end;

function TKCLSHWidget.GetLineWidth: Integer;
begin
  Result := (HorzRange.MaxValue - FLeftIndent) div CharW;
end;

procedure TKCLSHWidget.SetLineWidth(w: Integer);
begin
  HorzRange.MaxValue := FLeftIndent + w * CharW;
end;

function TKCLSHWidget.GetLineCount: Integer;
begin
  Result := VertRange.MaxValue div CharH;
end;

procedure TKCLSHWidget.SetLineCount(count: Integer);
begin
  VertRange.MaxValue := count * CharH;
end;

function TKCLSHWidget.GetClipboard: String;
begin
  Clipboard.Open;
  if not Clipboard.HasFormat('text/plain') then begin
    Clipboard.Close;
    Result := '';
  end else begin
    Result := Clipboard.AsText;
    Clipboard.Close;
  end;
end;

procedure TKCLSHWidget.SetClipboard(AContent: String);
begin
  Clipboard.Open;
  Clipboard.Clear;
  Clipboard.AsText := AContent;
  Clipboard.Close;
end;


procedure TKCLSHWidget.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  Inc(MinW, FMinHorzVisible * CharW + FLeftIndent);
  Inc(MinH, FMinVertVisible * CharH);
  DefW := FDefHorzVisible * CharW + FLeftIndent;
  DefH := FDefVertVisible * CharH;
end;

procedure TKCLSHWidget.ApplySize;
begin
  inherited ApplySize;
  HorzRange.PageSize := PaintBox.Width;
  VertRange.PageSize := PaintBox.Height;
end;

procedure TKCLSHWidget.SetMinHorzVisible(AChars: Integer);
begin
  FMinHorzVisible := AChars;
  RecalcLayout;
end;

procedure TKCLSHWidget.SetMinVertVisible(AChars: Integer);
begin
  FMinVertVisible := AChars;
  RecalcLayout;
end;

procedure TKCLSHWidget.SetDefHorzVisible(AChars: Integer);
begin
  FDefHorzVisible := AChars;
  RecalcLayout;
end;

procedure TKCLSHWidget.SetDefVertVisible(AChars: Integer);
begin
  FDefVertVisible := AChars;
  RecalcLayout;
end;


end.


{
  $Log$
  Revision 1.7  2000/02/22 14:43:46  sg
  * Lot of fixes, works now perfectly flicker-free

  Revision 1.6  2000/02/19 19:10:00  sg
  * KCLSHEdit is now flicker-free :-)  (the base class of TKCLSHEdit changed
    from TScrollBox to TScrollers)

  Revision 1.5  2000/02/10 18:50:34  sg
  * Added layout calculations
  * This widget now supports a minimum and default visible size

  Revision 1.4  2000/01/31 19:32:26  sg
  * Adapted to new SHEdit interface (mainly for selection redrawing fixes)
  * Fixed horizontal scrolling

  Revision 1.3  2000/01/24 00:32:16  sg
  * "KeyPressed" now returns a flag which indicates wether the key press
    has been processed or not

  Revision 1.2  2000/01/05 19:23:48  sg
  * Added mouse selection support
  * Added clipboard support
  * Paint events now may be nested (this caused some crashs before)

  Revision 1.1.1.1  1999/12/30 21:34:09  sg
  Initial import

}
