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
  colInvalid     = $ff000000;
  colDefault     = $ffffffff;

type

  TSHFontStyle = (fsNormal, fsBold, fsItalics, fsBoldItalics);

  TSHStyle = record
    Name: String[32];
    Color, Background: LongWord;
    FontStyle: TSHFontStyle;
  end;

  TSHStyleArray = array[1..255] of TSHStyle;  // Notice the 1!
  PSHStyleArray = ^TSHStyleArray;

  TKCLSHWidget = class(TScrollBox)
  protected
    WidgetIface: ISHWidget;
    SHStyles: PSHStyleArray;
    SHStyleCount: Integer;              // # of currently registered styles
    shWhitespace: Integer;

    PaintBox: TPaintBox;
    FLeftIndent: Integer;		// Left indent (border) in pixel (!)
    FDefMaxTextWidth: Integer;          // Position of vertical bar, in chars
    CharW, CharH: Integer;
    Fonts: array[TSHFontStyle] of TFont; // Fonts for content drawing
    Canvas: TCanvas;
    FDoc: TTextDoc;
    FEditor: TSHTextEdit;

    FFontName: String;
    FFontSize: Integer;
    CursorVisible: Boolean;
    MouseSelStartX, MouseSelStartY: Integer;

    procedure OnPaintBoxPaint(Sender: TObject; ACanvas: TCanvas;
      const rect: TRect);
    procedure KeyPressed(Sender: TObject; key: Char; KeyCode: LongWord;
      ShiftState: TShiftState);
    procedure PaintboxMouseButtonDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; MouseX, MouseY: Integer);
    procedure PaintboxMouseMove(Sender: TObject; Shift: TShiftState;
      MouseX, MouseY: Integer);
    procedure PaintboxFocusIn(Sender: TObject);
    procedure PaintboxFocusOut(Sender: TObject);


    // The "real" ISHWidget Implemenation:

    procedure InvalidateRect(x1, y1, x2, y2: Integer);
    procedure InvalidateLines(y1, y2: Integer);

    // Drawing
    procedure ClearRect(x1, y1, x2, y2: Integer);
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
    procedure SetupEditor(ADoc: TTextDoc; AEditClass: TSHTextEditClass);

    function  AddSHStyle(AName: String; AColor, ABackground: LongWord;
      AStyle: TSHFontStyle): Integer;
    property Document: TTextDoc read FDoc;
    property Editor: TSHTextEdit read FEditor;
    property LeftIndent: Integer read FLeftIndent write FLeftIndent;
    DrawVBar: Boolean;
    property DefMaxTextWidth: Integer read FDefMaxTextWidth write FDefMaxTextWidth;
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
    procedure InvalidateRect(x1, y1, x2, y2: Integer); override;
    procedure InvalidateLines(y1, y2: Integer); override;
    procedure ClearRect(x1, y1, x2, y2: Integer); override;
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

procedure TKCLWidgetIf.InvalidateRect(x1, y1, x2, y2: Integer);	   begin Widget.InvalidateRect(x1, y1, x2, y2) end;
procedure TKCLWidgetIf.InvalidateLines(y1, y2: Integer);	   begin Widget.InvalidateLines(y1, y2) end;
procedure TKCLWidgetIf.ClearRect(x1, y1, x2, y2: Integer);	   begin Widget.ClearRect(x1, y1, x2, y2) end;
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

  PaintBox := TPaintBox.Create(Self);
  PaintBox.Name := 'PaintBox';
  Content := PaintBox;
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
  PaintBox.OnPaint := @OnPaintBoxPaint;
  PaintBox.OnKey := @KeyPressed;
  PaintBox.OnMouseButtonDown := @PaintboxMouseButtonDown;
  PaintBox.OnMouseMove := @PaintboxMouseMove;
  PaintBox.OnFocusIn := @PaintboxFocusIn;
  PaintBox.OnFocusOut := @PaintboxFocusOut;
end;

procedure TKCLSHWidget.OnPaintBoxPaint(Sender: TObject; ACanvas: TCanvas;
  const rect: TRect);
var
  OldCanvas: TCanvas;
  px: Integer;
  r: TRect;
begin
  ASSERT(Assigned(ACanvas));
  OldCanvas := Canvas;	// necessary for nested paint events!
  Canvas := ACanvas;

  if DrawVBar then begin
    // Draw vertical lines
    px := FLeftIndent - 4;
    if rect.Left < px then begin
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
    r.Right := FLeftIndent - 7;
    Canvas.Color := colLightGray;
    Canvas.FillRect(r);
    r.Left := FLeftIndent - 3;
    r.Right := FLeftIndent;
    Canvas.Color := SHStyles^[shWhitespace].Background;
    Canvas.FillRect(r);
  end else begin
    Canvas.Color := SHStyles^[shWhitespace].Background;
    Canvas.FillRect(0, rect.Top, FLeftIndent, rect.Bottom - rect.Top + 1);
  end;

  // Draw text
  FEditor.DrawContent((rect.Left - FLeftIndent) div CharW, rect.Top div CharH,
    (rect.Right - FLeftIndent - 1) div CharW, (rect.Bottom - 1) div CharH);

  if FDefMaxTextWidth > 0 then begin
    px := FLeftIndent + FDefMaxTextWidth * CharW;
    Canvas.Color := colGray;
    Canvas.Line(px, rect.Top, px, rect.Bottom);
  end;

  Canvas := OldCanvas;
end;

procedure TKCLSHWidget.KeyPressed(Sender: TObject; key: Char; KeyCode: LongWord;
  ShiftState: TShiftState);
begin
  // WriteLn('Key pressed: Key=#', Ord(key), ', KeyCode=', KeyCode);
  if key <> #0 then
    FEditor.KeyPressed(Ord(key), ShiftState)
  else
    FEditor.KeyPressed(KeyCode, ShiftState);
end;

procedure TKCLSHWidget.PaintboxMouseButtonDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; mx, my: Integer);
begin
  if (Button <> mbLeft) or (mx < FLeftIndent) or (not Assigned(FDoc)) then
    exit;
  MouseSelStartX := (mx - FLeftIndent + CharW div 2) div CharW;
  MouseSelStartY :=  my div CharH;
  if MouseSelStartY >= FDoc.LineCount then exit;
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
    mx := (mx - FLeftIndent + CharW div 2) div CharW;
    my := my div CharH;
    if my >= FDoc.LineCount then exit;
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


function TKCLSHWidget.AddSHStyle(AName: String; AColor, ABackground: LongWord;
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


procedure TKCLSHWidget.InvalidateRect(x1, y1, x2, y2: Integer);
var
  r: TRect;
begin
  r.Left := FLeftIndent + x1 * CharW;
  r.Top := y1 * CharH;
  r.Right := FLeftIndent + (x2 + 1) * CharW;
  r.Bottom := (y2 + 1) * CharH;
  PaintBox.Redraw(r);
end;

procedure TKCLSHWidget.InvalidateLines(y1, y2: Integer);
begin
  PaintBox.Redraw(FLeftIndent, y1 * CharH, FWidth, (y2 - y1 + 1) * CharH);
end;

procedure TKCLSHWidget.ClearRect(x1, y1, x2, y2: Integer);
begin
  ASSERT(Assigned(Canvas));
  Canvas.Color := SHStyles^[shWhitespace].Background;
  Canvas.FillRect(x1 * CharW + FLeftIndent, y1 * CharH,
    (x2 - x1 + 1) * CharW, (y2 - y1 + 1) * CharH);
end;

procedure TKCLSHWidget.DrawTextLine(x1, x2, y: Integer; s: PChar);
var
  CurColor: LongWord;
  CurX1, CurX2: Integer;

  procedure DoErase;
  begin
    Canvas.Color := CurColor;
    if CurX1 < x1 then
      CurX1 := x1;
    if CurX2 >= x1 then
      Canvas.FillRect(CurX1 * CharW + FLeftIndent, y * CharH,
        (CurX2 - CurX1 + 1) * CharW + FLeftIndent, CharH);
    CurX1 := CurX2 + 1;
  end;

var
  RequestedColor: Integer;
  NewColor: LongWord;
  hs: PChar;
begin
//  WriteLn('DrawTextLine: x1=', x1, ', x2=', x2, ', y=', y);
  if Canvas = nil then WriteLn('Canvas = nil !!!');
  ASSERT(Assigned(Canvas));

  // Erase the (potentially multi-coloured) background
  hs := s;
  CurColor := SHStyles^[shWhitespace].Background;

  CurX1 := 0;
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
	  Canvas.Text(CurX1 * CharW + FLeftIndent, y * CharH, s[0]);
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
  px := FLeftIndent + x * CharW;
  py := y * CharH;
  lcanvas := PaintBox.CreateCanvas;
  lcanvas.Color := colBlack;
  lcanvas.Line(px, py, px, py + CharH - 1);
  Inc(px);
  lcanvas.Line(px, py, px, py + CharH - 1);
  lcanvas.Free;
end;

procedure TKCLSHWidget.HideCursor(x, y: Integer);
begin
  PaintBox.Redraw(FLeftIndent + x * CharW, y * CharH, 2, CharH);
end;

function TKCLSHWidget.GetHorzPos: Integer;
begin
  Result := (HorzRange.CurValue - FLeftIndent) div CharW;
end;

procedure TKCLSHWidget.SetHorzPos(x: Integer);
begin
  HorzRange.CurValue := FLeftIndent + x * CharW;
end;

function TKCLSHWidget.GetVertPos: Integer;
begin
  Result := VertRange.CurValue div CharH;
end;

procedure TKCLSHWidget.SetVertPos(y: Integer);
begin
  VertRange.CurValue := y * CharH;
end;

function TKCLSHWidget.GetPageWidth: Integer;
begin
  Result := HorzRange.PageSize;
end;

function TKCLSHWidget.GetPageHeight: Integer;
begin
  Result := VertRange.PageSize div CharH;
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


end.


{
  $Log$
  Revision 1.2  2000/01/05 19:23:48  sg
  * Added mouse selection support
  * Added clipboard support
  * Paint events now may be nested (this caused some crashs before)

  Revision 1.1.1.1  1999/12/30 21:34:09  sg
  Initial import

}
