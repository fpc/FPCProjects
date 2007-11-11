unit imagelib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FPImage, FPWritePNG, FPReadPNG;

type
  ImageLibException = class(exception);

//segment.inc
function imagelib_roberts_gradient(ASource: TFPCustomImage): TFPCustomImage;

//utils.inc
function imagelib_add_edge(ASource: TFPCustomImage; ASize: integer; const AColor: TFPColor): TFPCustomImage;

//conversion
function imagelib_rgb_to_grayscale(ASource: TFPCustomImage): TFPCustomImage;
function imagelib_rgb_to_sepia(ASource: TFPCustomImage): TFPCustomImage;
function imagelib_rgb_to_luma(ASource: TFPCustomImage): TFPCustomImage;

implementation

{$i segment.inc}
{$i conversion.inc}
{$i utils.inc}

begin
end.
