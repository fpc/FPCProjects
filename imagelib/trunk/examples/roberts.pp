{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Example file of roberts operator

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program roberts;

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
  
  writer := TFPWriterJPEG.Create;
  reader := TFPReaderJPEG.Create;
  
  try
    writeln('Loading JPEG file from disk');
    ci.LoadFromFile ('lenna_color.jpg', reader);

    //start := Now;
    //write('Converting to grayscale........ ');
    //image := imagelib_rgb_to_grayscale(ci);
    //image.SaveToFile ('lenna_grayscale.jpg', writer);
    //writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

    //start := Now;
    //write('Equalizing histogram........... ');
    //image := imagelib_equalize_histogram(image);
    //image.SaveToFile ('lenna_roberts.jpg', writer);
    //writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

    start := Now;
    write('Calculating Roberts gradient... ');
    image := imagelib_roberts_gradient(ci);
    image.SaveToFile ('lenna_roberts.jpg', writer);
    writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  finally
    image.Free;
    writer.Free;
    ci.free;
    reader.Free;
  end;
  
  writeln('done.');
  readln;
end.
