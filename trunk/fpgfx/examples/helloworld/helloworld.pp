program HelloWorld;

uses Classes, GFXBase, GFXImpl;

const
  HelloWorldString: String = 'Hello, world!';

type
  TMainWindow = class
    procedure Paint(Sender: TObject; const Rect: TRect);
  private
    Display: TDefDisplay;
    Window: TGfxWindow;
    Context: TGfxContext;
    TextSize: TSize;
    Font: TGfxFont;
  public
    constructor Create(ADisplay: TDefDisplay);
    destructor Destroy; override;
  end;

constructor TMainWindow.Create(ADisplay: TDefDisplay);
begin
  inherited Create;
  Display := ADisplay;
  Font := Display.CreateFont('-*-*-*-r-normal--36-*-*-*-*-*-iso8859-1');
  Window := ADisplay.CreateWindow;
  Window.Title := 'fpGFX Hello World example';
  Window.OnPaint := @Paint;
  Window.Show;
  Context := Window.Drawable.CreateContext;
  Context.SetFont(Font);
  TextSize := Context.TextExtent(HelloWorldString);
  Window.SetSize((TextSize.cx * 3) div 2, TextSize.cy * 2);
  Window.SetMinMaxSize(TextSize.cx, TextSize.cy, 0, 0);
end;

destructor TMainWindow.Destroy;
begin
  Context.Free;
  Font.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
const
  Black: TGfxColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: 0);
  White: TGfxColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: 0);
var
  Color: TGfxColor;
  r: TRect;
  i: Integer;
begin
  Color.Red := 0;
  Color.Green := 0;
  Color.Alpha := 0;
  r.Left := Rect.Left;
  r.Right := Rect.Right;
  for i := Rect.Top to Rect.Bottom - 1 do
  begin
    Color.Blue := $ffff - (i * $ffff) div Window.Height;
    Context.SetColor(Context.MapColor(Color));
    r.Top := i;
    r.Bottom := i + 1;
    Context.FillRect(r);
  end;

  Context.SetColor(Context.MapColor(Black));
  Context.TextOut(
    (Window.Width - TextSize.cx) div 2 + 1,
    (Window.Height - TextSize.cy) div 2 + 1,
    HelloWorldString);

  Context.SetColor(Context.MapColor(White));
  Context.TextOut(
    (Window.Width - TextSize.cx) div 2 - 1,
    (Window.Height - TextSize.cy) div 2 - 1,
    HelloWorldString);
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
