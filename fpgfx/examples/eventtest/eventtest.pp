{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Event Test example

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program EventTest;

uses SysUtils, Classes, GFXBase, GFXImpl;

const
  ButtonNames: array[TMouseButton] of PChar =
    ('Left', 'Right', 'Middle');

type
  TMainWindow = class
    procedure FocusIn(Sender: TObject);
    procedure FocusOut(Sender: TObject);
    procedure KeyPressed(Sender: TObject; AKey: Word; AShiftState: TShiftState);
    procedure KeyReleased(Sender: TObject; AKey: Word; AShiftState: TShiftState);
    procedure KeyChar(Sender: TObject; AKeyChar: Char);
    procedure MouseEnter(Sender: TObject; AShift: TShiftState;
      const AMousePos: TPoint);
    procedure MouseLeave(Sender: TObject);
    procedure MousePressed(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseReleased(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseMove(Sender: TObject; AShift: TShiftState;
      const AMousePos: TPoint);
    procedure MouseWheel(Sender: TObject; AShift: TShiftState;
      AWheelDelta: Single; const AMousePos: TPoint);
    procedure Paint(Sender: TObject; const ARect: TRect);
    procedure Move(Sender: TObject);
    procedure Resize(Sender: TObject);
  private
    FWindow: TGfxWindow;
    function ShiftStateToStr(Shift: TShiftState): String;
    function MouseState(AShift: TShiftState; const AMousePos: TPoint): String;
  public
    constructor Create(ADisplay: TDefDisplay);
  end;

constructor TMainWindow.Create(ADisplay: TDefDisplay);
begin
  inherited Create;
  FWindow := ADisplay.DefaultScreen.CreateWindow(nil, wtWindow);
  FWindow.SetClientSize(Size(500, 100));
  FWindow.Title := 'fpGFX Event Test example';
  FWindow.OnFocusIn := @FocusIn;
  FWindow.OnFocusOut := @FocusOut;
  FWindow.OnKeyPressed := @KeyPressed;
  FWindow.OnKeyReleased := @KeyReleased;
  FWindow.OnKeyChar := @KeyChar;
  FWindow.OnMouseEnter := @MouseEnter;
  FWindow.OnMouseLeave := @MouseLeave;
  FWindow.OnMousePressed := @MousePressed;
  FWindow.OnMouseReleased := @MouseReleased;
  FWindow.OnMouseMove := @MouseMove;
  FWindow.OnMouseWheel := @MouseWheel;
  FWindow.OnPaint := @Paint;
  FWindow.OnMove := @Move;
  FWindow.OnResize := @Resize;
  FWindow.Show;
end;

function TMainWindow.ShiftStateToStr(Shift: TShiftState): String;
begin
  SetLength(Result, 0);
  if ssShift in Shift then
    Result := 'Shift ';
  if ssAlt in Shift then
    Result := Result + 'Alt ';
  if ssCtrl in Shift then
    Result := Result + 'Ctrl ';
  if ssMeta in Shift then
    Result := Result + 'Meta ';
  if ssSuper in Shift then
    Result := Result + 'Super ';
  if ssHyper in Shift then
    Result := Result + 'Hyper ';
  if ssAltGr in Shift then
    Result := Result + 'AltGr ';
  if ssCaps in Shift then
    Result := Result + 'Caps ';
  if ssNum in Shift then
    Result := Result + 'Num ';
  if ssScroll in Shift then
    Result := Result + 'Scroll ';
  if ssLeft in Shift then
    Result := Result + 'Left ';
  if ssRight in Shift then
    Result := Result + 'Right ';
  if ssMiddle in Shift then
    Result := Result + 'Middle ';
  if ssDouble in Shift then
    Result := Result + 'Double ';
  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 1);
end;

function TMainWindow.MouseState(AShift: TShiftState;
  const AMousePos: TPoint): String;
var
  ShiftStateStr: String;
begin
  ShiftStateStr := ShiftStateToStr(AShift);
  Result := '[X=' + IntToStr(AMousePos.x) + ' Y=' + IntToStr(AMousePos.y);
  if Length(ShiftStateStr) > 0 then
    Result := Result + ' ' + ShiftStateStr;
  Result := Result + ']';
end;

procedure TMainWindow.FocusIn(Sender: TObject);
begin
  WriteLn('Got focus');
end;

procedure TMainWindow.FocusOut(Sender: TObject);
begin
  WriteLn('Lost focus');
end;

procedure TMainWindow.KeyPressed(Sender: TObject; AKey: Word;
  AShiftState: TShiftState);
begin
  WriteLn('[', ShiftStateToStr(AShiftState), '] Key pressed: ',
    KeycodeToText(AKey, []));
end;

procedure TMainWindow.KeyReleased(Sender: TObject; AKey: Word;
  AShiftState: TShiftState);
begin
  WriteLn('[', ShiftStateToStr(AShiftState), '] Key released: ',
    KeycodeToText(AKey, []));
end;

procedure TMainWindow.KeyChar(Sender: TObject; AKeyChar: Char);
begin
  Write('Character generated: ');
  if AKeyChar >= ' ' then
    WriteLn('''', AKeyChar, '''')
  else
    WriteLn('#', Ord(AKeyChar));
end;

procedure TMainWindow.MouseEnter(Sender: TObject; AShift: TShiftState;
  const AMousePos: TPoint);
begin
  WriteLn(MouseState(AShift, AMousePos), 'Mouse entered window');
end;

procedure TMainWindow.MouseLeave(Sender: TObject);
begin
  WriteLn('Mouse left window');
end;

procedure TMainWindow.MousePressed(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  WriteLn(MouseState(AShift, AMousePos),
    'Mouse button pressed: ', ButtonNames[AButton]);
end;

procedure TMainWindow.MouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  WriteLn(MouseState(AShift, AMousePos),
    'Mouse button released: ', ButtonNames[AButton]);
end;

procedure TMainWindow.MouseMove(Sender: TObject; AShift: TShiftState;
  const AMousePos: TPoint);
begin
  WriteLn(MouseState(AShift, AMousePos), 'Mouse moved');
end;

procedure TMainWindow.MouseWheel(Sender: TObject; AShift: TShiftState;
  AWheelDelta: Single; const AMousePos: TPoint);
begin
  WriteLn(MouseState(AShift, AMousePos), 'Mouse wheel rotated by ',
    AWheelDelta:0:2, ' ticks');
end;

procedure TMainWindow.Paint(Sender: TObject; const ARect: TRect);
begin
  with FWindow.Canvas do
  begin
    SetColor(colWhite);
    FillRect(ARect);
    SetColor(colBlack);
    TextOut(Point(0, 0), 'Event test');
    TextOut(Point(0, FontCellHeight),
      'Do something interactive (move mouse, press keys...)');
    TextOut(Point(0, FontCellHeight * 2),
      'and watch the output on the console.');
  end;
end;

procedure TMainWindow.Move(Sender: TObject);
begin
  WriteLn('Window has been moved to ', FWindow.Left, '/', FWindow.Top);
end;

procedure TMainWindow.Resize(Sender: TObject);
begin
  WriteLn('Window has been resized. New width: ',
    FWindow.Width, ' x ', FWindow.Height,
    '; new client width: ', FWindow.ClientWidth, ' x ', FWindow.ClientHeight);
end;

var
  Display: TDefDisplay;
  MainWindow: TMainWindow;
begin
  Display := TDefDisplay.Create;
  MainWindow := TMainWindow.Create(Display);
  Display.Run;
  MainWindow.Free;
  Display.Free;
end.


{
  $Log$
  Revision 1.8  2001/02/14 23:07:47  sg
  * Switched to use TSize and TPoint whereever possible

  Revision 1.7  2001/02/09 20:48:02  sg
  * Adapted to fpGFX interface improvements

  Revision 1.6  2001/01/18 15:00:14  sg
  * Added TGfxWindowType and implemented support for it

}
