{
    $Id$

    fpImg  -  Free Pascal Imaging Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Image reader for BMP files

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit BMPReader;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses Classes, GFXBase, ImageIO;

type
  DWORD = LongWord;
  LONG = LongInt;

  TBitmapFileHeader = packed record
    bfType: WORD;
    bfSize: DWORD;
    bfReserved1: WORD;
    bfReserved2: WORD;
    bfOffBits: DWORD;
  end;

  TBitmapInfoHeader = packed record
    biSize: DWORD;
    biWidth: LONG;
    biHeight: LONG;
    biPlanes: WORD;
    biBitCount: WORD;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;


  TBMPReader = class(TImageReader)
  protected
    FFileHeader: TBitmapFileHeader;
    FInfoHeader: TBitmapInfoHeader;
    FFileStride: LongWord;

    HeaderBytesRead: Integer;
    ScanlinesLeft: Integer;
    ThisSegmentHeight: Integer;
    ScanlinesLeftInSegment: Integer;
    ScanlineBytesDone: LongWord;
    CurScanline: Pointer;

    procedure DoProcessHeaderData(AStream: TStream); override;
    function DoGetImageSegmentStartY(ASegmentHeight: Integer): Integer;
      override;
    procedure InitImageReading; override;
    procedure InitSegmentReading;
    procedure DoProcessImageData(AStream: TStream); override;
  public
    property FileHeader: TBitmapFileHeader read FFileHeader;
    property InfoHeader: TBitmapInfoHeader read FInfoHeader;
    property FileStride: LongWord read FFileStride;
  end;


implementation


procedure TBMPReader.DoProcessHeaderData(AStream: TStream);
var
  HaveRead: Integer;
  IsFirstRead: Boolean;
begin
  if HeaderBytesRead < SizeOf(FileHeader) then
  begin
    HaveRead := AStream.Read(PChar(@FileHeader)[HeaderBytesRead],
      SizeOf(FileHeader) - HeaderBytesRead);
    if HaveRead = 0 then
      raise EImgOutOfData.Create;
    Inc(HeaderBytesRead, HaveRead);
    IsFirstRead := False;
  end else
    IsFirstRead := True;

  if HeaderBytesRead < SizeOf(FileHeader) + SizeOf(InfoHeader) then
  begin
    HaveRead := AStream.Read(
      PChar(@InfoHeader)[HeaderBytesRead - SizeOf(FileHeader)],
      SizeOf(FileHeader) + SizeOf(InfoHeader) - HeaderBytesRead);
    if HaveRead = 0 then
      if IsFirstRead then
        raise EImgOutOfData.Create
      else
        exit;
    Inc(HeaderBytesRead, HaveRead);
  end;

  if HeaderBytesRead = SizeOf(FileHeader) + SizeOf(InfoHeader) then
  begin
    FWidth := InfoHeader.biWidth;
    FHeight := InfoHeader.biHeight;

    // Set up the pixel format
    case InfoHeader.biBitCount of
      24:
        begin
          FFileStride := (Width * 3 + 3) and not 3;
  	  FPixelFormat := PixelFormatBGR24;
	  // !!!: Doesn't handle formats with other masks yet
        end;
      // !!!: Add support for more color depths
      else
        raise EImgUnsupportedPixelFormat.Create;
    end;

    HeaderFinished;
  end;
end;

function TBMPReader.DoGetImageSegmentStartY(ASegmentHeight: Integer): Integer;
begin
  Result := ScanlinesLeft - ASegmentHeight;
  if Result < 0 then
    Result := 0;
end;

procedure TBMPReader.InitImageReading;
begin
  ScanlinesLeft := Height;
  InitSegmentReading;
end;

procedure TBMPReader.InitSegmentReading;
begin
  ThisSegmentHeight := ScanlinesLeft;
  if ThisSegmentHeight > SegmentHeight then
    ThisSegmentHeight := SegmentHeight;
  ScanlinesLeftInSegment := ThisSegmentHeight;
  ScanlineBytesDone := 0;
  CurScanline := SegmentData + (ThisSegmentHeight - 1) * SegmentStride;
end;

procedure TBMPReader.DoProcessImageData(AStream: TStream);

  procedure ScanlineDone;
  begin
    Dec(ScanlinesLeftInSegment);
    Dec(ScanlinesLeft);

    if ScanlinesLeftInSegment = 0 then
    begin
      SegmentFinished(ScanlinesLeft, ThisSegmentHeight);
      if ScanlinesLeft = 0 then
        ImageFinished
      else
        InitSegmentReading;
    end else
      Dec(CurScanline, SegmentStride);
  end;

var
  ReadMayFail: Boolean;
  ToRead, HaveRead: Integer;
begin
  if ScanlineBytesDone > 0 then
  begin
    ToRead := SegmentStride;
    if ToRead > FileStride then
      ToRead := FileStride;
    Dec(ToRead, ScanlineBytesDone);
    HaveRead := AStream.Read(PChar(CurScanline)[ScanlineBytesDone], ToRead);
    if HaveRead = 0 then
      raise EImgOutOfData.Create;
    if HaveRead = ToRead then
    begin
      ScanlineBytesDone := 0;
      ScanlineDone
    end else
    begin
      Inc(ScanlineBytesDone, HaveRead);
      exit;
    end;
    ReadMayFail := True;
  end else
    ReadMayFail := False;

  while ScanlinesLeft > 0 do
  begin
    ToRead := SegmentStride;
    if ToRead > FileStride then
      ToRead := FileStride;

    HaveRead := AStream.Read(CurScanline^, ToRead);

    if HaveRead = 0 then
      if ReadMayFail then
        exit
      else
        raise EImgOutOfData.Create;

    if HaveRead < ToRead then
    begin
      ScanlineBytesDone := HaveRead;
      break;
    end;

    // Handle the ordinary case: a full scanline has been read
    if ToRead < FileStride then
      AStream.Position := AStream.Position + FileStride - ToRead;
    ReadMayFail := True;
    ScanlineDone;
  end;
end;

end.


{
  $Log$
  Revision 1.2  2000/10/28 20:17:52  sg
  * Improved segment handling
  * Interface cleaning up

  Revision 1.1  2000/10/27 23:40:00  sg
  * First version

}
