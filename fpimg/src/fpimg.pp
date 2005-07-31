{
    $Id$

    fpImg  -  Free Pascal Imaging Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    fpImg interface declarations;
    this is the main glue code between different fpImg submodules
    such as ImageIO and fpGFX.

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit fpImg;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses Classes, GFXBase, ImageIO;


function CreateImageFromFile(AScreen: TGfxScreen; AReader: TImageReaderClass;
  const AFilename: String): TGfxImage;

function CreateImageFromStream(AScreen: TGfxScreen; AReader: TImageReaderClass;
  AStream: TStream): TGfxImage;



implementation


function CreateImageFromFile(AScreen: TGfxScreen; AReader: TImageReaderClass;
  const AFilename: String): TGfxImage;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    Result := CreateImageFromStream(AScreen, AReader, Stream);
  finally
    Stream.Free;
  end;
end;


function CreateImageFromStream(AScreen: TGfxScreen; AReader: TImageReaderClass;
  AStream: TStream): TGfxImage;
var
  Reader: TImageReader;
  Data: Pointer;
  Stride: LongWord;
  Palette: TGfxPalette;
begin
  Reader := AReader.Create;
  try
    Reader.ProcessHeaderData(AStream);
    Result := AScreen.Display.CreateImage(Reader.Width, Reader.Height,
      Reader.PixelFormat);
    if Reader.PaletteSize > 0 then
    begin
      Palette := AScreen.CreatePalette(Reader.PaletteSize,
        Reader.Palette);
      try
        Result.Palette := Palette;
      finally
        Palette.Release;
      end;
    end;
    try
      Result.Lock(Data, Stride);
      try
        Reader.SetImageSegmentBuffer(Data, Stride, Reader.Height);
	Reader.ProcessImageData(AStream);
      finally
        Result.Unlock;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Reader.Free;
  end;
end;

end.


{
  $Log$
  Revision 1.3  2001/01/11 23:21:53  sg
  *** empty log message ***

  Revision 1.2  2000/10/29 12:15:23  sg
  * Now uses TGfxDevice instead of TGfxDisplay

  Revision 1.1  2000/10/27 23:40:00  sg
  * First version

}
