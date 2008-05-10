function imagelib_roberts_gradient(ASource: TFPMemoryImage): TFPMemoryImage;

var
  w: integer;
  h: integer;

  function calc_gradient(x, y: integer): TFPColor;
  var
    gradX: word;
    gradY: word;
  begin
    //red
    gradX := Abs(ASource.Colors[x, y].red - ASource.Colors[x+1,y].red);
    gradY := Abs(ASource.Colors[x, y].red - ASource.Colors[x, y+1].red);

    if gradX > gradY then
      Result.red := gradX
    else
      Result.red := gradY;

    //green
    gradX := Abs(ASource.Colors[x, y].green - ASource.Colors[x+1,y].green);
    gradY := Abs(ASource.Colors[x, y].green - ASource.Colors[x, y+1].green);

    if gradX > gradY then
      Result.green := gradX
    else
      Result.green := gradY;

    //blue
    gradX := Abs(ASource.Colors[x, y].blue - ASource.Colors[x+1,y].blue);
    gradY := Abs(ASource.Colors[x, y].blue - ASource.Colors[x, y+1].blue);

    if gradX > gradY then
      Result.blue := gradX
    else
      Result.blue := gradY;
  end;

begin
  try
    Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
    Result.UsePalette := False;

    for w := 0 to ASource.Width-2 do
      for h := 0 to ASource.Height-2 do
        Result.Colors[w,h] := calc_gradient(w, h);
  except
    raise ImageLibException.Create('Cannot determine Roberts gradient');
  end;
end;

