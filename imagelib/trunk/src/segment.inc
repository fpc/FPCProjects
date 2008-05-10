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

