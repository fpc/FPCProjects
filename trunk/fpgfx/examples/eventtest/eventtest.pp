// $Id$

{ EventTest for fpGFX, written by Sebastian Guenther (sg@freepascal.org) }

program EventTest;

uses SysUtils, Classes, GFXBase, GFXImpl;

const
  ButtonNames: array[TMouseButton] of PChar =
    ('Left', 'Right', 'Middle');

type
  TMainWindow = class
    procedure FocusIn(Sender: TObject);
    procedure FocusOut(Sender: TObject);
    procedure KeyPressed(Sender: TObject; Key: Word; ShiftState: TShiftState);
    procedure KeyReleased(Sender: TObject; Key: Word; ShiftState: TShiftState);
    procedure KeyChar(Sender: TObject; AKeyChar: Char);
    procedure MouseEnter(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure MouseLeave(Sender: TObject);
    procedure MousePressed(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure MouseReleased(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Single; x, y: Integer);
    procedure Paint(Sender: TObject; const Rect: TRect);
    procedure Move(Sender: TObject);
    procedure Resize(Sender: TObject);
  private
    FWindow: TGfxWindow;
    function ShiftStateToStr(Shift: TShiftState): String;
    function MouseState(Shift: TShiftState; x, y: Integer): String;
  public
    constructor Create(ADisplay: TDefDisplay);
  end;

constructor TMainWindow.Create(ADisplay: TDefDisplay);
begin
  inherited Create;
  FWindow := ADisplay.DefaultScreen.CreateWindow(True);
  FWindow.SetClientSize(500, 100);
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

function TMainWindow.MouseState(Shift: TShiftState; x, y: Integer): String;
var
  ShiftStateStr: String;
begin
  ShiftStateStr := ShiftStateToStr(Shift);
  Result := '[X=' + IntToStr(x) + ' Y=' + IntToStr(y);
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

procedure TMainWindow.KeyPressed(Sender: TObject; Key: Word;
  ShiftState: TShiftState);
begin
  WriteLn('[', ShiftStateToStr(ShiftState), '] Key pressed: ',
    KeycodeToText(Key, []));
end;

procedure TMainWindow.KeyReleased(Sender: TObject; Key: Word;
  ShiftState: TShiftState);
begin
  WriteLn('[', ShiftStateToStr(ShiftState), '] Key released: ',
    KeycodeToText(Key, []));
end;

procedure TMainWindow.KeyChar(Sender: TObject; AKeyChar: Char);
begin
  Write('Character generated: ');
  if AKeyChar >= ' ' then
    WriteLn('''', AKeyChar, '''')
  else
    WriteLn('#', Ord(AKeyChar));
end;

procedure TMainWindow.MouseEnter(Sender: TObject; Shift: TShiftState;
  x, y: Integer);
begin
  WriteLn(MouseState(Shift, x, y), 'Mouse entered window');
end;

procedure TMainWindow.MouseLeave(Sender: TObject);
begin
  WriteLn('Mouse left window');
end;

procedure TMainWindow.MousePressed(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
begin
  WriteLn(MouseState(Shift, x, y),
    'Mouse button pressed: ', ButtonNames[Button]);
end;

procedure TMainWindow.MouseReleased(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
begin
  WriteLn(MouseState(Shift, x, y),
    'Mouse button released: ', ButtonNames[Button]);
end;

procedure TMainWindow.MouseMove(Sender: TObject; Shift: TShiftState;
  x, y: Integer);
begin
  WriteLn(MouseState(Shift, x, y), 'Mouse moved');
end;

procedure TMainWindow.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Single; x, y: Integer);
begin
  WriteLn(MouseState(Shift, x, y), 'Mouse wheel rotated by ', WheelDelta:0:2,
    ' ticks');
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
begin
  with FWindow.Canvas do
  begin
    SetColor(MapColor(colWhite));
    FillRect(Rect);
    SetColor(MapColor(colBlack));
    TextOut(0, 0, 'Event test');
    TextOut(0, FontCellHeight,
      'Do something interactive (move mouse, press keys...)');
    TextOut(0, FontCellHeight * 2, 'and watch the output on the console.');
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
  $Log:
}
