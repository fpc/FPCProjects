{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Segment functions

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function imagelib_roberts_gradient(ASource: TFPMemoryImage): TFPMemoryImage;
var
  w: integer;
  h: integer;

  function calc_col_gradient(ColorXY, ColorX, ColorY: word): word;
  var
    gradX: word;
    gradY: word;
  begin
    gradX := Abs(ColorXY - ColorX);
    gradY := Abs(ColorXY - ColorY);

    if gradX > gradY then
      Result := gradX
    else
      Result := gradY;
  end;

  function calc_gradient(XY, X, Y: TFPColor): TFPColor;
  begin
    //red
    Result.red := calc_col_gradient(XY.red, X.red, Y.red);

    //green
    Result.green := calc_col_gradient(XY.green, X.green, Y.green);

    //blue
    Result.blue := calc_col_gradient(XY.blue, X.blue, Y.blue);
  end;

begin
  try
    Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
    Result.UsePalette := False;

    for w := 0 to ASource.Width-2 do
      for h := 0 to ASource.Height-2 do
        Result.Colors[w,h] := calc_gradient(ASource.Colors[w, h], ASource.Colors[w+1, h], ASource.Colors[w, h+1]);
  except
    raise ImageLibException.Create('Cannot determine Roberts gradient');
  end;
end;

procedure convolute_image(ASource: TFPMemoryImage; var AImage: TFPMemoryImage; Kx, Ky: TConvKernel; Kxw, Kxh, Kyw, Kyh: integer);
var
  w: integer;
  h: integer;
  gradX: word;
  gradY: word;
  GradColX: TFPColor;
  GradColY: TFPColor;

  function single_pixel_convolution(x, y: integer; K: TConvKernel; Kw, Kh: integer): TFPColor;
  var
    w: integer;
    h: integer;
    gradX: word;
    gradY: word;
  begin
    Result := FPColor(0,0,0);
    for w := 0 to Kw-1 do
      for h := 0 to Kh-1 do
      begin
	Result.red := Result.red + (ASource.Colors[x+w, y+h].red * K[w, h]);
	Result.green := Result.green + (ASource.Colors[x+w, y+h].green * K[w, h]);
	Result.blue := Result.blue + (ASource.Colors[x+w, y+h].blue * K[w, h]);
      end;
  end;

  function gradient_magnitude(Ax, Ay: word): word;
  var
    x, y: word;
  begin
    //Result := round(sqrt(Ax*Ax + Ay*Ay));

    if Ax > Ay then
      Result := Ax
    else
      Result := Ay;
  end;

begin
  for w := Kxw to AImage.Width - 1 - Kxw do
    for h := Kyh to AImage.Width - 1 - Kyh do
    begin
      GradColX := single_pixel_convolution(w, h, Kx, Kxw, Kxh);
      GradColY := single_pixel_convolution(w, h, Kx, Kxw, Kxh);


      AImage.Colors[w, h]:=FPColor(gradient_magnitude(GradColX.red, GradColY.red),
                                   gradient_magnitude(GradColX.green, GradColY.green),
                                   gradient_magnitude(GradColX.blue, GradColY.blue));
    end;
end;

function imagelib_roberts_gradient2(ASource: TFPMemoryImage): TFPMemoryImage;
var
  ROBERTS_HX: TConvKernel;
  ROBERTS_HY: TConvKernel;
begin
{
   Hx = +1  0
         0 -1

   Hy =  0 +1
        -1  0
}

  SetLength(ROBERTS_HX, 2, 2);
  ROBERTS_HX[0,0] := 1;
  ROBERTS_HX[1,0] := 0;
  ROBERTS_HX[0,0] := 0;
  ROBERTS_HX[1,0] := -1;

  SetLength(ROBERTS_HY, 2, 2);
  ROBERTS_HY[0,0] := 0;
  ROBERTS_HY[1,0] := 1;
  ROBERTS_HY[0,0] := -1;
  ROBERTS_HY[1,0] := 0;

  try
    Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
    Result.UsePalette := False;

    convolute_image(ASource, Result, ROBERTS_HX, ROBERTS_HY, 2, 1, 1, 2);
  except
    raise ImageLibException.Create('Cannot determine Roberts gradient');
  end;
end;
