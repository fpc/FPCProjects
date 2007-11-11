function imagelib_roberts_gradient(ASource: TFPCustomImage): TFPCustomImage;

var
  w: integer;
  h: integer;

  function calc_gradient(x, y: integer): TFPColor;
  var
    GradientX: integer;
    GradientY: integer;
  begin
    //red
    GradientX := ASource.Colors[x, y].red - ASource.Colors[x+1,y].red;
    GradientY := ASource.Colors[x, y].red - ASource.Colors[x, y+1].red;

    GradientX := Abs(GradientX);
    GradientY := Abs(GradientY);

    if GradientX > GradientY then
      Result.red := GradientX
    else
      Result.red := GradientY;

    //green
    GradientX := ASource.Colors[x, y].green - ASource.Colors[x+1,y].green;
    GradientY := ASource.Colors[x, y].green - ASource.Colors[x, y+1].green;

    GradientX := Abs(GradientX);
    GradientY := Abs(GradientY);

    if GradientX > GradientY then
      Result.green := GradientX
    else
      Result.green := GradientY;

    //blue
    GradientX := ASource.Colors[x, y].blue - ASource.Colors[x+1,y].blue;
    GradientY := ASource.Colors[x, y].blue - ASource.Colors[x, y+1].blue;

    GradientX := Abs(GradientX);
    GradientY := Abs(GradientY);

    if GradientX > GradientY then
      Result.blue := GradientX
    else
      Result.blue := GradientY;
  end;

begin
  try
    Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
    Result.Palette.Copy(ASource.Palette);

    for w := 0 to ASource.Width-2 do
      for h := 0 to ASource.Height-2 do
      begin
        Result.Colors[w,h] := calc_gradient(w, h);
      end;
  except
    raise ImageLibException.Create('Cannot determine Roberts gradient');
  end;
end;

