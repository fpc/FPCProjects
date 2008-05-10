program grayscale;

{$mode objfpc}{$H+}

uses 
  SysUtils, imagelib, FPImage, FPWriteJPEG, FPReadJPEG;

var
  ci, image : TFPMemoryImage;
  writer : TFPCustomImageWriter;
  reader : TFPCustomImageReader;
  start: TDateTime;
begin
  ci := TFPMemoryImage.Create(0,0);
  ci.UsePalette := False;
  
  reader := TFPReaderJPEG.Create;
  writer := TFPWriterJPEG.Create;

  try
    writeln('Loading JPEG file from disk');
    ci.LoadFromFile ('lenna_color.jpg', reader);

    start := Now;
    write('Converting to grayscale...');
    image := imagelib_rgb_to_grayscale(ci);
    image.SaveToFile ('lenna_grayscale.jpg', writer);
    image.Free;
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

    start := Now;
    write('Converting to sepia.......');
    image := imagelib_rgb_to_sepia(ci);
    image.SaveToFile ('lenna_sepia.jpg', writer);
    image.Free;
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

    start := Now;
    write('Converting to luma........');
    image := imagelib_rgb_to_luma(ci);
    image.SaveToFile ('lenna_luma.jpg', writer);
    image.Free;
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  finally
    writer.Free;
    ci.free;
    reader.Free;
  end;
  writeln('done.');
  readln;
end.
