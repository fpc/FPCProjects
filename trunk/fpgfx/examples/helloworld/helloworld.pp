{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 - 2001 by
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
  Font := Display.CreateFont('-*-' + Display.GetDefaultFontName(fcSerif) +
    '-*-r-normal--36-*-*-*-*-*-iso8859-1');
  Window := ADisplay.DefaultScreen.CreateWindow(nil, wtWindow);
  Window.Title := 'fpGFX Hello World example';
  Window.OnPaint := @Paint;
  Window.Canvas.SetFont(Font);
  TextSize.cx := Window.Canvas.TextWidth(HelloWorldString);
  TextSize.cy := Window.Canvas.FontCellHeight;
  Window.SetClientSize((TextSize.cx * 3) div 2, TextSize.cy * 2);
  Window.SetMinMaxClientSize(TextSize.cx, TextSize.cy, 0, 0);
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
    Color.Blue := $ffff - (i * $ffff) div Window.ClientHeight;
    Window.Canvas.SetColor(Color);
    r.Top := i;
    r.Bottom := i + 1;
    Window.Canvas.FillRect(r);
  end;

  Window.Canvas.SetColor(colBlack);
  Window.Canvas.SetFont(Font);
  Window.Canvas.TextOut((Window.ClientWidth - TextSize.cx) div 2 + 1,
    (Window.ClientHeight - TextSize.cy) div 2 + 1, HelloWorldString);

  Window.Canvas.SetColor(colWhite);
  Window.Canvas.TextOut((Window.ClientWidth - TextSize.cx) div 2 - 1,
    (Window.ClientHeight - TextSize.cy) div 2 - 1, HelloWorldString);
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
  Revision 1.7  2001/02/09 20:48:02  sg
  * Adapted to fpGFX interface improvements

}
