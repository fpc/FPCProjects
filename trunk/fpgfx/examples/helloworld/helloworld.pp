{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    'Hello world' example

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


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
  Window.Canvas.SetFont(Font);
  TextSize := Window.Canvas.TextExtent(HelloWorldString);
  Window.SetSize((TextSize.cx * 3) div 2, TextSize.cy * 2);
  Window.SetMinMaxSize(TextSize.cx, TextSize.cy, 0, 0);
  Window.Show;
end;

destructor TMainWindow.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
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
    Window.Canvas.SetColor(Window.Canvas.MapColor(Color));
    r.Top := i;
    r.Bottom := i + 1;
    Window.Canvas.FillRect(r);
  end;

  Window.Canvas.SetColor(Window.Canvas.MapColor(colBlack));
  Window.Canvas.TextOut((Window.Width - TextSize.cx) div 2 + 1,
    (Window.Height - TextSize.cy) div 2 + 1, HelloWorldString);

  Window.Canvas.SetColor(Window.Canvas.MapColor(colWhite));
  Window.Canvas.TextOut((Window.Width - TextSize.cx) div 2 - 1,
    (Window.Height - TextSize.cy) div 2 - 1, HelloWorldString);
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
