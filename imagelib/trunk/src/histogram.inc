function imagelib_equalize_histogram(ASource: TFPMemoryImage): TFPMemoryImage;
var
  MinCol, MaxCol: TFPColor;
  Color: TFPColor;
  w: integer;
  h: integer;
  red: word;
  green: word;
  blue: word;
  
  redC: single;
  greenC: single;
  blueC: single;
begin
  Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
  Result.UsePalette := False;
  
  MinCol := colWhite;
  MaxCol := colBlack;

  //retrieve min / max values of each channel
  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      Color := ASource.Colors[w, h];
      if Color.red < MinCol.red then
        MinCol.red := Color.red;
      if Color.red > MaxCol.red then
        MaxCol.red := Color.red;

      if Color.green < MinCol.green then
        MinCol.green := Color.green;
      if Color.green > MaxCol.green then
        MaxCol.green := Color.green;

      if Color.blue < MinCol.blue then
        MinCol.blue := Color.blue;
      if Color.blue > MaxCol.blue then
        MaxCol.blue := Color.blue;
    end;

  //calculate correction factor per channel
  redC := High(Word)/(MaxCol.red - MinCol.red);
  greenC := High(Word)/(MaxCol.green - MinCol.green);
  blueC := High(Word)/(MaxCol.blue - MinCol.blue);

  //calculate new value for each pixel
  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      Color := ASource.Colors[w, h];
      
      red := round(redC * (Color.red - MinCol.red));
      if red > $ffff then
        red := $ffff;

      green := round(greenC * (Color.green - MinCol.green));
      if green > $ffff then
        green := $ffff;

      blue := round(blueC * (Color.blue - MinCol.blue));
      if blue > $ffff then
        blue := $ffff;

      Result.Colors[w,h] := FPColor(red, green, blue);
    end;
end;
