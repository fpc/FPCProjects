{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Image conversion helpers

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFXImage;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses
  Classes,
  GfxBase;		// fpGFX units


procedure ConvertImage(
  ASourceRect: TRect; ASourceFormat: TGfxPixelFormat;
  ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestFormat: TGfxPixelFormat;
  ADestData: Pointer; ADestStride: LongWord);


implementation

type
  PLongWord = ^LongWord;

  TConvertParams = record
    RedShiftR, RedShiftL,
      GreenShiftR, GreenShiftL,
      BlueShiftR, BlueShiftL: Byte;
    RedMask, GreenMask, BlueMask: LongWord;
    RedMult, GreenMult, BlueMult: Word;
  end;

  TConvertToInternalProc = procedure(Params: TConvertParams; Data: Pointer;
    StartX, EndX: Integer; Dest: Pointer);

  TConvertFromInternalProc = procedure(Params: TConvertParams; Data: Pointer;
    Dest: Pointer; Width: Integer);


procedure ConvertRGB24ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  PixelIn: LongWord;
begin
  while StartX < EndX do
  begin
    PixelIn := 0;
    Move(Data^, PixelIn, 3);
    PLongWord(Dest)^ :=
      (((PixelIn shr Params.RedShiftR) and $ff) shl Params.RedShiftL) or
      (((PixelIn shr Params.GreenShiftR) and $ff) shl Params.GreenShiftL) or
      (((PixelIn shr Params.BlueShiftR) and $ff) shl Params.BlueShiftL);
    Inc(StartX);
    Inc(Data, 3);
    Inc(Dest, 4);
  end;
end;

procedure ConvertRGB32ToInternal(Params: TConvertParams; Data: Pointer;
  StartX, EndX: Integer; Dest: Pointer);
var
  PixelIn: LongWord;
begin
  while StartX < EndX do
  begin
    PixelIn := PLongWord(Data)^;
    PLongWord(Dest)^ :=
      (((PixelIn shr Params.RedShiftR) and $ff) shl Params.RedShiftL) or
      (((PixelIn shr Params.GreenShiftR) and $ff) shl Params.GreenShiftL) or
      (((PixelIn shr Params.BlueShiftR) and $ff) shl Params.BlueShiftL);
    Inc(StartX);
    Inc(Data, 4);
    Inc(Dest, 4);
  end;
end;

{ !!!: 15/16 bit to 24/32 bit extension:
    PByte(Dest)[0] := HiByte(Word(((PixelIn and Params.RedMask) shr Params.RedShiftR) * Params.RedMult));
    PByte(Dest)[1] := HiByte(Word(((PixelIn and Params.GreenMask) shr Params.GreenShiftR) * Params.GreenMult));
    PByte(Dest)[2] := HiByte(Word(((PixelIn and Params.BlueMask) shr Params.BlueShiftR) * Params.BlueMult));
}

procedure ConvertInternalToRGB16(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PWord(Dest)^ :=
      (((PixelIn and $0000ff) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $ff0000) shr Params.BlueShiftR) shl Params.BlueShiftL);

    Inc(Data, 4);
    Inc(Dest, 2);
    Dec(Width);
  until Width = 0;
end;

procedure ConvertInternalToRGB24(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn, PixelOut: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PixelOut :=
      (((PixelIn and $ff0000) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $0000ff) shr Params.BlueShiftR) shl Params.BlueShiftL);
    PWord(Dest)^ := Word(PixelOut);
    PByte(Dest)[2] := PixelOut shr 16;

    Inc(Data, 4);
    Inc(Dest, 4);
    Dec(Width);
  until Width = 0;
end;

procedure ConvertInternalToRGB32(Params: TConvertParams; Data: Pointer;
  Dest: Pointer; Width: Integer);
var
  PixelIn: LongWord;
begin
  repeat
    PixelIn := PLongWord(Data)^;
    PLongWord(Dest)^ :=
      (((PixelIn and $ff0000) shr Params.RedShiftR) shl Params.RedShiftL) or
      (((PixelIn and $00ff00) shr Params.GreenShiftR) shl Params.GreenShiftL) or
      (((PixelIn and $0000ff) shr Params.BlueShiftR) shl Params.BlueShiftL);

    Inc(Data, 4);
    Inc(Dest, 4);
    Dec(Width);
  until Width = 0;
end;


function GetBitShiftAndCount(Mask: LongWord; var Shift: Byte): Integer;
begin
  Shift := 0;
  while (Mask and 1) = 0 do
  begin
    Mask := Mask shr 1;
    Inc(Shift);
  end;
  Result := 0;
  while Mask > 0 do
  begin
    Mask := Mask shr 1;
    Inc(Result);
  end;
end;

procedure SetupShifts(PixelFormat: TGfxPixelFormat; var Params: TConvertParams);
begin
  Params.RedShiftR := 8 -
    GetBitShiftAndCount(PixelFormat.RedMask, Params.RedShiftL);
  Params.GreenShiftR := 16 -
    GetBitShiftAndCount(PixelFormat.GreenMask, Params.GreenShiftL);
  Params.BlueShiftR := 24 -
    GetBitShiftAndCount(PixelFormat.BlueMask, Params.BlueShiftL);
end;

procedure ConvertImage(
  ASourceRect: TRect; ASourceFormat: TGfxPixelFormat;
  ASourceData: Pointer; ASourceStride: LongWord;
  ADestX, ADestY: Integer; ADestFormat: TGfxPixelFormat;
  ADestData: Pointer; ADestStride: LongWord);
var
  ParamsTo, ParamsFrom: TConvertParams;
  ConvertToInternal: TConvertToInternalProc;
  ConvertFromInternal: TConvertFromInternalProc;
  Scanline: Pointer;
  w, y: Integer;
begin
  case ASourceFormat.FormatType of
    ftRGB24:
      begin
        ConvertToInternal := @ConvertRGB24ToInternal;
	ParamsTo.RedShiftR := 8 -
	  GetBitShiftAndCount(ASourceFormat.RedMask, ParamsTo.RedShiftL);
	ParamsTo.GreenShiftR := 16 -
	  GetBitShiftAndCount(ASourceFormat.GreenMask, ParamsTo.GreenShiftL);
	ParamsTo.BlueShiftR := 24 -
	  GetBitShiftAndCount(ASourceFormat.BlueMask, ParamsTo.BlueShiftL);
      end;
    ftRGB32:
      begin
        ConvertToInternal := @ConvertRGB32ToInternal;
	ParamsTo.RedShiftR := 8 -
	  GetBitShiftAndCount(ASourceFormat.RedMask, ParamsTo.RedShiftL);
	ParamsTo.GreenShiftR := 16 -
	  GetBitShiftAndCount(ASourceFormat.GreenMask, ParamsTo.GreenShiftL);
	ParamsTo.BlueShiftR := 24 -
	  GetBitShiftAndCount(ASourceFormat.BlueMask, ParamsTo.BlueShiftL);
      end;
    else
      raise EGfxError.Create(SUnsupportedPixelFormat);
  end;

  case ADestFormat.FormatType of
    ftRGB16:
      begin
        ConvertFromInternal := @ConvertInternalToRGB16;
	SetupShifts(ADestFormat, ParamsFrom);
      end;
    ftRGB24:
      begin
        ConvertFromInternal := @ConvertInternalToRGB24;
	SetupShifts(ADestFormat, ParamsFrom);
      end;
    ftRGB32:
      begin
        ConvertFromInternal := @ConvertInternalToRGB32;
	SetupShifts(ADestFormat, ParamsFrom);
      end;
    else
      raise EGfxError.Create(SUnsupportedPixelFormat);
  end;

  w := ASourceRect.Right - ASourceRect.Left;
  GetMem(Scanline, w * SizeOf(TGfxPixel));
  for y := ASourceRect.Top to ASourceRect.Bottom - 1 do
  begin
    ConvertToInternal(ParamsTo, ASourceData,
      ASourceRect.Left, ASourceRect.Right, Scanline);
    Inc(ASourceData, ASourceStride);
    ConvertFromInternal(ParamsFrom, Scanline, ADestData, w);
    Inc(ADestData, ADestStride);
  end;
  FreeMem(Scanline);
end;

end.


{
  $Log$
  Revision 1.1  2000/10/28 20:28:27  sg
  * First version

}
