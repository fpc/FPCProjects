program roberts;

{$mode objfpc}{$H+}

uses 
  SysUtils, imagelib, FPImage, FPWriteJPEG, FPReadJPEG;

var
  ci, image : TFPMemoryImage;
  grayImage : TFPMemoryImage;
  writer : TFPCustomImageWriter;
  reader : TFPCustomImageReader;
  start: TDateTime;
begin
  ci := TFPMemoryImage.Create(0,0);
  ci.UsePalette := False;
  
  writer := TFPWriterJPEG.Create;
  reader := TFPReaderJPEG.Create;
  
  try
    writeln('Loading JPEG file from disk');
    ci.LoadFromFile ('lenna_color.jpg', reader);

    start := Now;
    write('Converting to grayscale...');
    grayImage := imagelib_rgb_to_grayscale(ci);
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

    start := Now;
    write('Calculating Roberts gradient...');
    image := imagelib_roberts_gradient(grayImage);
    image.SaveToFile ('lenna_roberts.jpg', writer);
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  finally
    image.Free;
    writer.Free;
    ci.free;
    grayImage.Free;
    reader.Free;
  end;
  
  writeln('done.');
  readln;
end.
