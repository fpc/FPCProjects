program grayscale;

{$mode objfpc}{$H+}

uses 
  SysUtils, imagelib, FPImage, FPWritePNG, FPReadPNG;

var
  ci, image : TFPCustomImage;
  writer : TFPCustomImageWriter;
  reader : TFPCustomImageReader;
begin
  ci := TFPMemoryImage.Create(100,100);
  Writer := TFPWriterPNG.Create;
  reader := TFPReaderPNG.Create;
  with TFPWriterPNG(Writer) do
    begin
    indexed := false;
    wordsized := false;
    UseAlpha := false;
    GrayScale := false;
    end;
  try
    ci.LoadFromFile ('lenna_color.png', reader);

    //writeln('Converting to grayscale');
    //image := imagelib_rgb_to_grayscale(ci);

    //writeln ('Saving to inspect !');
    //image.SaveToFile ('lenna_grayscale.png', writer);

    //writeln('Converting to sepia');
    //image := imagelib_rgb_to_sepia(ci);

    //writeln ('Saving to inspect !');
    //image.SaveToFile ('lenna_sepia.png', writer);

    writeln('Converting to luma');
    image := imagelib_rgb_to_luma(ci);

    writeln ('Saving to inspect !');
    image.SaveToFile ('lenna_luma.png', writer);
  finally
    image.Free;
    writer.Free;
    ci.free;
    reader.Free;
  end;
end.
