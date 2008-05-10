function imagelib_add_edge(ASource: TFPMemoryImage; ASize: integer; const AColor: TFPColor): TFPMemoryImage;
var
  w: integer;
  h: integer;
  PaletteIndex: integer;
begin
  try
    Result := TFPMemoryImage.Create(ASource.Width + 2*ASize, ASource.Height + 2*ASize);
    Result.Palette.Copy(ASource.Palette);
    PaletteIndex := Result.Palette.IndexOf(AColor);

    for w := 0 to Result.Width-1 do
      for h := 0 to Result.Height-1 do
        if (w <= ASize) or (h <= ASize) or (w >= ASource.Width + ASize) or (h >= ASource.Height + ASize) then
          Result.Pixels[w, h] := PaletteIndex
        else
          Result.Pixels[w, h] := ASource.Pixels[w - ASize, h - ASize];
  except
    raise ImageLibException.Create('Cannot add edge to image');
  end;
end;
