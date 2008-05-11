unit imagelib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FPImage, FPWritePNG, FPReadPNG, FPCanvas;

type
  ImageLibException = class(exception);

  TFeature = record
    index: integer;
    x: integer;
    y: integer;
  end;

  TFeatureList = array of TFeature;

//segment.inc
function imagelib_roberts_gradient(ASource: TFPMemoryImage): TFPMemoryImage;

//utils.inc
function imagelib_add_edge(ASource: TFPMemoryImage; ASize: integer; const AColor: TFPColor): TFPMemoryImage;

//conversion.inc
function imagelib_rgb_to_bw(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_grayscale(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_sepia(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_luma(ASource: TFPMemoryImage): TFPMemoryImage;

//histogram.inc
function imagelib_equalize_histogram(ASource: TFPMemoryImage): TFPMemoryImage;

//feature.inc
function imagelib_find_feature(ASource: TFPMemoryImage; Radius: smallint; FeatureCount: integer): TFeatureList;

implementation

{$i segment.inc}
{$i conversion.inc}
{$i histogram.inc}
{$i utils.inc}
{$i feature.inc}

begin
end.
