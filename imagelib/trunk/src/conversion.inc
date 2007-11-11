type
  TColorMatrix = record
                   red: double;
                   green: double;
                   blue: double;
                 end;

function imagelib_convert_to_monochrome(ASource: TFPCustomImage; R, G, B: TColorMatrix): TFPCustomImage;
var
  w: integer;
  h: integer;
  Gray: TFPColor;
  GrayIndex: integer;
begin
  try
    Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);

    for w := 0 to ASource.Width-1 do
      for h := 0 to ASource.Height-1 do
      begin
        Gray.red := Round(ASource.Colors[w,h].red * R.red +
                          ASource.Colors[w,h].green * R.green +
                          ASource.Colors[w,h].blue * R.blue);
        Gray.green := Round(ASource.Colors[w,h].red * G.red +
                            ASource.Colors[w,h].green * G.green +
                            ASource.Colors[w,h].blue * G.blue);
        Gray.blue := Round(ASource.Colors[w,h].red * B.red +
                           ASource.Colors[w,h].green * B.green +
                           ASource.Colors[w,h].blue * B.blue);

        GrayIndex := Result.Palette.IndexOf(Gray);
        Result.Pixels[w,h] := GrayIndex;
      end;
  except
    raise ImageLibException.Create('Cannot convert to grayscale');
  end;

end;

function imagelib_rgb_to_grayscale(ASource: TFPCustomImage): TFPCustomImage;
var
  R: TColorMatrix;
  G: TColorMatrix;
  B: TColorMatrix;
begin
  R.red := 0.3;
  R.green:= 0.59;
  R.blue:= 0.11;
  G := R;
  B := R;
  
  Result := imagelib_convert_to_monochrome(ASource, R, G, B);
end;

function imagelib_rgb_to_sepia(ASource: TFPCustomImage): TFPCustomImage;
var
  R: TColorMatrix;
  G: TColorMatrix;
  B: TColorMatrix;
begin
  R.red := 0.393;
  R.green:= 0.769;
  R.blue:= 0.189;

  G.red := 0.349;
  G.green:= 0.686;
  G.blue:= 0.168;

  B.red := 0.272;
  B.green:= 0.534;
  B.blue:= 0.131;

  Result := imagelib_convert_to_monochrome(ASource, R, G, B);
end;

function imagelib_rgb_to_luma(ASource: TFPCustomImage): TFPCustomImage;
var
  R: TColorMatrix;
  G: TColorMatrix;
  B: TColorMatrix;
begin
  R.red := 0.2126;
  R.green:= 0.7152;
  R.blue:= 0.0722;

  G.red := 0.2126;
  G.green:= 0.7152;
  G.blue:= 0.0722;

  B.red := 0.2126;
  B.green:= 0.7152;
  B.blue:= 0.0722;

  Result := imagelib_convert_to_monochrome(ASource, R, G, B);
end;
