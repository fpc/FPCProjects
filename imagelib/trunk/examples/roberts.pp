program roberts;

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
    GrayScale := true;
    end;
  try
    ci.LoadFromFile ('lenna_grayscale.png', reader);

    writeln('Calculating Roberts gradient');
    image := imagelib_roberts_gradient(ci);

    writeln ('Saving to inspect !');
    image.SaveToFile ('lenna_roberts.png', writer);
  finally
    image.Free;
    writer.Free;
    ci.free;
    reader.Free;
  end;
end.
