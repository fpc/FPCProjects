function imagelib_find_feature(ASource: TFPMemoryImage; Radius: smallint; FeatureCount: integer): TFeatureList;
var
  Count : integer;
  w : integer;
  h: integer;
  Tmp: TFeature;
begin
  //create a list with features
  w := radius;
  Count := 0;
  while w < ASource.width - 1 - radius do
  begin
    h := radius;
    while h < ASource.height - 1 - radius do
    begin
      Inc(Count);
      SetLength(Result, Count);

      with Result[Count - 1] do
      begin
        index := Abs(CompareColors(ASource.Colors[w,h], ASource.Colors[w-radius,h]));
        index := index + Abs(CompareColors(ASource.Colors[w,h], ASource.Colors[w+radius,h]));
        index := index + Abs(CompareColors(ASource.Colors[w,h], ASource.Colors[w,h-radius]));
        index := index + Abs(CompareColors(ASource.Colors[w,h], ASource.Colors[w,h+radius]));

        x := w;
        y := h;
      end;

      h := h + radius*2;
    end;
    w := w + radius*2;
  end;

  //sort the featurelist
  for w := 0 to Count - 1 do
    for h := w to Count - 1 do
    begin
      if Result[w].index < Result[h].index then
      begin
        tmp := Result[h];
        Result[h] := Result[w];
        Result[w] := tmp;
      end;
    end;

  //limit the size of the featurelist
  if FeatureCount < Count then
    SetLength(Result, FeatureCount);
end;
