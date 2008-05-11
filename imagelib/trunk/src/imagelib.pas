unit imagelib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FPImage, FPWritePNG, FPReadPNG;

type
  ImageLibException = class(exception);

//segment.inc
function imagelib_roberts_gradient(ASource: TFPMemoryImage): TFPMemoryImage;

//utils.inc
function imagelib_add_edge(ASource: TFPMemoryImage; ASize: integer; const AColor: TFPColor): TFPMemoryImage;

//conversion
function imagelib_rgb_to_bw(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_grayscale(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_sepia(ASource: TFPMemoryImage): TFPMemoryImage;
function imagelib_rgb_to_luma(ASource: TFPMemoryImage): TFPMemoryImage;

implementation

{$i segment.inc}
{$i conversion.inc}
{$i utils.inc}

begin
end.
