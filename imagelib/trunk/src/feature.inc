{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Feature detection functions

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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

function imagelib_find_feature_FAST(ASource: TFPMemoryImage; Radius: smallint; Threshold: integer): TFeatureList;
var
  Count : integer;
  w : integer;
  h: integer;
  NucleusPixel: word;
  testnum: integer;
  index: integer;
  tempcol: word;

  procedure TestPixel(APixelValue: word; var PixelCount: integer);
  begin
    if Abs(NucleusPixel - APixelValue) > Threshold then
    begin
      inc(PixelCount);
    end
    else
    begin
      index := 0;
    end;
  end;

begin
{
   this is a Bressenham circle of radius 3, needs to be generalized for radius r

   ....XXX....
   ...X...X...
   ..X.....X..
   ..X..p..X..
   ..X.....X..
   ...X...X...
   ....XXX....
}

  //create a list with features
  testnum := 9;
  
  w := radius+2; //remove the +2
  Count := 0;
  while w < ASource.width - 1 - radius -2 do  //remove the -2
  begin
    h := radius+2;  //remove the +2
    while h < ASource.height - 1 - radius -2 do  //remove the -2
    begin

        NucleusPixel := ASource.Colors[w,h].red;
        tempcol := ASource.Colors[w+3,h].red;
        
        index := 0;
        while true do
        begin
          TestPixel(ASource.Colors[w+3,h].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+3,h-1].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+2,h-2].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+1,h-3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w,h-3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-1,h-3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-2,h-2].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-3,h-1].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-3,h].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-3,h+1].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-2,h+2].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w-1,h+3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w,h+3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+1,h+3].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+2,h+2].red, index);
          if index >= testnum then break;
          TestPixel(ASource.Colors[w+3,h+1].red, index);

          break;
        end;
        
      if index >=testnum then
      begin
        Inc(Count);
        SetLength(Result, Count);

        with Result[Count - 1] do
        begin

          x := w;
          y := h;
        end;
      end;
      
      h := h + radius*2;
    end;
    w := w + radius*2;
  end;
end;
