{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Colormap conversions

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  GCM_SEPIA_R : TGrayConvMatrix = (red:0.3930; green:0.7690; blue:0.1890);
  GCM_SEPIA_G : TGrayConvMatrix = (red:0.3490; green:0.6860; blue:0.1680);
  GCM_SEPIA_B : TGrayConvMatrix = (red:0.2720; green:0.5340; blue:0.1310);
  GCM_LUMA    : TGrayConvMatrix = (red:0.2126; green:0.7152; blue:0.0722);

function imagelib_rgb_to_bw(ASource: TFPMemoryImage): TFPMemoryImage;
const threshold = High(Word) div 2;
var
  gray: TFPColor;
  w: integer;
  h: integer;
begin
  GrayConvMatrix := GCM_JPEG;

  Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
  Result.UsePalette := False;

  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      gray := ASource.Colors[w, h];
      if (gray.red > threshold) or (gray.green > threshold) or (gray.blue > threshold) then
        Result.Colors[w, h] := fpcolor(High(Word), High(Word), High(Word))
      else
        Result.Colors[w, h] := fpcolor(0, 0, 0);
    end;
end;

function imagelib_rgb_to_grayscale(ASource: TFPMemoryImage): TFPMemoryImage;
var
  gray: word;
  w: integer;
  h: integer;
begin
  GrayConvMatrix := GCM_JPEG;
  
  Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
  Result.UsePalette := False;
  
  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      gray := CalculateGray(ASource.Colors[w, h]);
      Result.Colors[w, h] := fpcolor(gray, gray, gray);
    end;
end;

function imagelib_rgb_to_sepia(ASource: TFPMemoryImage): TFPMemoryImage;
var
  sepiaR: word;
  sepiaG: word;
  sepiaB: word;
  w: integer;
  h: integer;
begin
  Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
  Result.UsePalette := False;

  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      GrayConvMatrix := GCM_SEPIA_R;
      sepiaR := CalculateGray(ASource.Colors[w, h]);
      GrayConvMatrix := GCM_SEPIA_G;
      sepiaG := CalculateGray(ASource.Colors[w, h]);
      GrayConvMatrix := GCM_SEPIA_B;
      sepiaB := CalculateGray(ASource.Colors[w, h]);

      Result.Colors[w, h] := fpcolor(sepiaR, sepiaG, sepiaB);
    end;
end;

function imagelib_rgb_to_luma(ASource: TFPMemoryImage): TFPMemoryImage;
var
  gray: word;
  w: integer;
  h: integer;
begin
  GrayConvMatrix := GCM_LUMA;

  Result := TFPMemoryImage.Create(ASource.Width, ASource.Height);
  Result.UsePalette := False;

  for w := 0 to ASource.Width-1 do
    for h := 0 to ASource.Height-1 do
    begin
      gray := CalculateGray(ASource.Colors[w, h]);
      Result.Colors[w, h] := fpcolor(gray, gray, gray);
    end;
end;
