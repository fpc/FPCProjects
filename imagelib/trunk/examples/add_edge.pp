{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Example file of add edge to image function

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program add_edge;

{$mode objfpc}{$H+}

uses 
  imagelib, FPImage, FPWritePNG, FPReadPNG;

var
  ci, image : TFPCustomImage;
  writer : TFPCustomImageWriter;
  reader : TFPCustomImageReader;
  Color: TFPColor;
  
begin
  ci := TFPMemoryImage.Create(100,100);
  Writer := TFPWriterPNG.Create;
  reader := TFPReaderPNG.Create;

  Color.blue := High(Word);
  Color.red := High(Word);
  Color.green := High(Word);

  with TFPWriterPNG(Writer) do
    begin
    indexed := false;
    wordsized := false;
    UseAlpha := false;
    GrayScale := false;
    end;
  try
    ci.LoadFromFile('lenna_color.png', reader);

    writeln('Adding edge to image');
    image := imagelib_add_edge(ci, 3, Color);

    writeln ('Saving to inspect !');
    image.SaveToFile('lenna_with_edge.png', writer);
  finally
    image.Free;
    writer.Free;
    ci.free;
    reader.Free;
  end;
end.
