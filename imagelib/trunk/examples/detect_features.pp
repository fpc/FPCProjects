{
    This file is part of the Free Pascal imagelib.
    Copyright (c) 2008 by Darius Blaszyk

    Example file of feature detection

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program detect_features;

uses
  fpimage,fpcanvas,fpreadjpeg, fpwritejpeg,fpimgcanv, sysutils, imagelib,
  classes;

var
  image : TFPMemoryImage;
  w : integer;
  h: integer;
  start: TDateTime;
  allstart: TDateTime;
  radius: smallint;
  FeatureList: TFeatureList;
  Tmp: TFeature;
  Count: integer;
  canvas : TFPcustomCAnvas;
begin
  //load image
  image:=TFPMemoryImage.Create(0,0);
  image.UsePalette:=False;

  allstart := Now;
  
  start := Now;
  write('Loading image.................. ');
  image.LoadFromFile('lenna_color.jpg', TFPReaderJPEG.Create);
  writeln('done in ', round((Now-start)*24*3600*1000), ' msec');
  start := Now;
  write('Equalizing histogram........... ');
  image := imagelib_equalize_histogram(image);
  //image.SaveToFile ('lenna_grayscale_equalized.jpg', TFPWriterJPEG.Create);
  writeln('done in ', round((Now-start)*24*3600*1000), ' msec');
  start := Now;
  write('Converting to grayscale........ ');
  image := imagelib_rgb_to_grayscale(image);
  writeln('done in ', round((Now-start)*24*3600*1000), ' msec');
  //start := Now;
  //write('Calculating Roberts gradient... ');
  //image := imagelib_roberts_gradient(image);
  //writeln('done in ', round((Now-start)*24*3600*1000), ' msec');
  //start := Now;
  //write('Converting to bw............... ');
  //image := imagelib_rgb_to_bw(image);
  //image.SaveToFile ('lenna_equalized_BW.jpg', TFPWriterJPEG.Create);
  //writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  //create a list with features
  start := Now;
  write('Extracting features............ ');
  featurelist := imagelib_find_feature(image, 3, 200);

  //draw the features in the original image
  canvas := TFPImageCanvas.Create (image);
  canvas.Pen.FPColor := colGreen;

  for w := 0 to High(featurelist) do
  begin
    canvas.Line(featurelist[w].x - 3, featurelist[w].y, featurelist[w].x + 3, featurelist[w].y);
    canvas.Line(featurelist[w].x, featurelist[w].y - 3, featurelist[w].x, featurelist[w].y + 3);
  end;

  writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  //save image
  start := Now;
  write('Saving image................... ');
  image.SaveToFile('lenna_features.jpg',TFPWriterJPEG.Create);
  writeln('done in ', round((Now-start)*24*3600*1000), ' msec');

  writeln('done in ', round((Now-allstart)*24*3600*1000), ' msec');

  //free objects
  Image.Free;

  readln;
end.
