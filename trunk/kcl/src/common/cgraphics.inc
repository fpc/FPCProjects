{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Common implementation for graphics support
// ===================================================================

// -------------------------------------------------------------------
//   TGraphicsObject
// -------------------------------------------------------------------

procedure TGraphicsObject.Changed;
begin
  Modified := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


destructor TGraphicsObject.Destroy;
begin
  inherited Destroy;
end;


procedure TGraphicsObject.AddNotifier(ANotifier: TGraphicsObjectNotify; UserData: TObject);
begin
end;


procedure TGraphicsObject.RemoveNotifier(ANotifier: TGraphicsObjectNotify);
begin
end;


// -------------------------------------------------------------------
//   TFont
// -------------------------------------------------------------------

// procedure TFont.SetCharset(ACharset: TFontCharset);


procedure TFont.SetColor(AColor: TColor);
begin
  if AColor <> FColor then begin
    FColor := AColor;
    Changed;
  end;
end;


procedure TFont.SetHeight(AHeight: Integer);
begin
  if AHeight <> FHeight then begin
    FHeight := AHeight;
    Changed;
  end;
end;


procedure TFont.SetName(const AName: TFontName);
begin
  if AName <> FName then begin
    FName := AName;
    Changed;
  end;
end;


procedure TFont.SetPitch(APitch: TFontPitch);
begin
  if APitch <> FPitch then begin
    FPitch := APitch;
    Changed;
  end;
end;


procedure TFont.SetSize(ASize: Integer);
begin
  if ASize <> FSize then begin
    FSize := ASize;
    Changed;
  end;
end;


procedure TFont.SetStyle(AStyle: TFontStyles);
begin
  if AStyle <> FStyle then begin
    FStyle := AStyle;
    Changed;
  end;
end;


procedure TFont.Assign(Source: TPersistent);
var
  f: TFont;
begin
  if Source.InheritsFrom(TFont) then begin
    f := TFont(Source);
    if {(FCharset <> f.Charset) or} (FColor <> f.Color) or
       (FHeight <> f.Height) or (FName <> f.Name) or (FPitch <> f.Pitch) or
       (FSize <> f.Size) or (FStyle <> f.Style) then begin
      // FCharset := f.Charset;
      FColor := f.Color;
      FHeight := f.Height;
      FName := f.Name;
      FPitch := f.Pitch;
      FSize := f.Size;
      FStyle := f.Style;
      Changed;
    end;
  end else
    inherited Assign(Source);
end;


// -------------------------------------------------------------------
//   TCanvas
// -------------------------------------------------------------------

procedure TCanvas.FillRect(ax, ay, aw, ah: Integer);
var
  rect: TRect;
begin
  rect.Left := ax;
  rect.Top := ay;
  rect.Right := ax + aw;
  rect.Bottom := ay + ah;
  FillRect(rect);
end;


procedure TCanvas.DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap);
begin
  DrawBitmap(ADestX, ADestY, ABitmap, 0, 0, ABitmap.FWidth, ABitmap.FHeight);
end;


procedure TCanvas.DrawBitmap(ADestX, ADestY: Integer; ABitmap: TBitmap;
  ASourceW, ASourceH: Integer);
begin
  DrawBitmap(ADestX, ADestY, ABitmap, 0, 0, ASourceW, ASourceH);
end;


// -------------------------------------------------------------------
//   TBitmap
// -------------------------------------------------------------------

type

  TBitmapFileHeader = packed record
    bfType1, bfType2: Char;	// 'BM' (the original declaration uses a Word!)
    bfSize: LongWord;
    bfReserved1, bfReserved2: Word;
    bfOffBits: LongWord;	// Offset to bitmap data from beginning of file
  end;

  TBitmapInfoHeader = packed record
    biSize: LongWord;		// Size of this structure
    biWidth: LongInt;
    biHeight: LongInt;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: LongWord;
    biSizeImage: LongWord;
    biXPelsPerMeter: LongInt;
    biYPelsPerMeter: LongInt;
    biClrUsed: LongWord;
    biClrImportant: LongWord;
  end;


destructor TBitmap.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TBitmap.LoadFromFile(const AFilename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TBitmap.LoadFromStream(AStream: TStream);
var
  FileHeader: TBitmapFileHeader;
  InfoHeader: TBitmapInfoHeader;
  x, y: Integer;
  p: PChar;
  c: Char;
begin
  AStream.Read(@FileHeader, SizeOf(FileHeader));
  ASSERT((FileHeader.bfType1 = 'B') and (FileHeader.bfType2 = 'M'));
  AStream.Read(@InfoHeader, SizeOf(InfoHeader));
  ASSERT(InfoHeader.biSize >= SizeOf(TBitmapInfoHeader));
  FWidth := InfoHeader.biWidth;
  FHeight := InfoHeader.biHeight;
  FBitsPerPixel := InfoHeader.biBitCount;
  FStride := (((FBitsPerPixel + 7) shr 3) * FWidth) and not 3;

  //  WriteLn(Format('Bitmap: %dx%d, %d bpp, stride=%d', [FWidth, FHeight, FBitsPerPixel, FStride]));

  GetMem(FData, FStride * FHeight);
  FillChar(FData^, FStride * FHeight, 0);

  // Load the BMP data and create a color map for GdkRGB, if necessary
  AStream.Position := FileHeader.bfOffBits;
  for y := 0 to FHeight - 1 do begin
    p := FData + (FHeight - y - 1) * FStride;
    AStream.Read(p^, FStride);
    case FBitsPerPixel of
      24: begin
          // Swap the red and blue elements of all pixels
	  for x := 0 to FWidth - 1 do begin
	    c := p[0];
	    p[0] := p[2];
	    p[2] := c;
	    Inc(p, 3);
	  end;
        end;
      32: begin
          // Swap the red and blue elements of all pixels
	  for x := 0 to FWidth - 1 do begin
	    c := p[0];
	    p[0] := p[2];
	    p[2] := c;
	    Inc(p, 4);
	  end;
        end;
    end;
  end;
end;

function TBitmap.GetRGB(x, y: Integer): TColor;
var
  p: Pointer;
begin
  case FBitsPerPixel of
    24: begin
        Result := 0;
	p := FData;
	Inc(p, y * FStride);
	Inc(p, x * 3);
	Move(p^, @Result, 3);
      end;
    else begin
      WriteLn('### TBitmap.GetRGB not supported for bpp=', FBitsPerPixel);
      Result := 0;
    end;
  end;
end;


{
  $Log$
  Revision 1.2  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.1  2000/01/24 00:13:57  sg
  * First version

}
