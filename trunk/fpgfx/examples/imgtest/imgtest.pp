{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Image Test example

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program ImgTest;

uses Classes, GFXBase, GFXImpl;

type
  TMainWindow = class
    procedure Paint(Sender: TObject; const Rect: TRect);
  private
    Display: TDefDisplay;
    Window: TGfxWindow;
    Image: TGfxImage;
  public
    constructor Create(ADisplay: TDefDisplay);
    destructor Destroy; override;
  end;

constructor TMainWindow.Create(ADisplay: TDefDisplay);
type
  PLongWord = ^LongWord;
var
  Data: Pointer;
  Stride: LongWord;
  i, j: Integer;
begin
  inherited Create;
  Display := ADisplay;
  Window := ADisplay.CreateWindow;
  Window.Title := 'fpGFX Bitmap Test';
  Window.OnPaint := @Paint;
  Window.SetSize(256, 256);
  Window.Show;

  Image := Display.CreateImage(256, 256, PixelFormatRGB32);

  Image.Lock(Data, Stride);
  for j := 0 to 255 do
    for i := 0 to 255 do
      PLongWord(Data)[j * 256 + i] := (i shl 16) or (j shl 8);
  Image.Unlock;
end;

destructor TMainWindow.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
var
  r: TRect;
begin
  Window.Drawable.SetColor(Window.Drawable.MapColor(colBlue));
  r.Left := 0;
  r.Top := 0;
  r.Right := Window.Width;
  r.Bottom := Window.Height;
  Window.Drawable.FillRect(r);
  Image.Draw(Window.Drawable, 0, 0);
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
