{
    $Id$

    fpImg  -  Free Pascal Imaging Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Example: Display BMP file

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program Disp_BMP;

uses Classes, GFXBase, GFXImpl, fpImg, BMPReader;

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
begin
  inherited Create;
  Display := ADisplay;
  Image := CreateImageFromFile(Display, TBMPReader, ParamStr(1));
  Window := ADisplay.CreateWindow;
  Window.Title := 'fpImg Bitmap Test';
  Window.OnPaint := @Paint;
  Window.SetSize(Image.Width, Image.Height);
  Window.Show;
end;

destructor TMainWindow.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
begin
  Window.Canvas.DrawImage(Image, 0, 0);
end;

var
  Display: TDefDisplay;
  MainWindow: TMainWindow;
begin
  if ParamCount <> 1 then
  begin
    WriteLn(StdErr, 'Please give the name of a BMP file as argument');
    Halt(2);
  end;

  Display := TDefDisplay.Create;
  MainWindow := TMainWindow.Create(Display);
  Display.Run;
  MainWindow.Free;
  Display.Free;
end.


{
  $Id$
}
