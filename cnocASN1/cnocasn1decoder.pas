{ ASN.1 Ber/Der-decoder

  Copyright (C) 2017 Joost van der Sluis (CNOC) joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit cnocASN1Decoder;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  cnocASN1,
  cnocASN1TagFactory;

type

  { TcnocASN1BERDecoder }

  TcnocASN1BERDecoder = class
  private
    FTagFactory: TcnocASN1TagFactory;
  protected
    function ReadHighTagNumber(AStream: TStream): Int64;
    function ReadLongFormLength(FirstLengthOctet: Byte; AStream: TStream): QWord;
  public
    function LoadElementFromStream(AStream: TStream; AFormat: TcnocASN1Format): TObject;
    procedure LoadFromStream(AStream: TStream; AFormat: TcnocASN1Format; ALength: Integer; AnOutputList: TObjectList);
    function Read128BaseInteger(AStream: TStream; out ASize: Integer): Int64;
    property TagFactory: TcnocASN1TagFactory read FTagFactory write FTagFactory;
  end;

  IcnocASN1DecodableClass = Interface ['{D8327562-E5FD-4CD2-A31A-BB46C3CFB284}']
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format);
  end;

implementation

{ TcnocASN1BERDecoder }

function TcnocASN1BERDecoder.ReadHighTagNumber(AStream: TStream): Int64;
var
  i: Integer;
begin
  Result := Read128BaseInteger(AStream, i);
end;

function TcnocASN1BERDecoder.ReadLongFormLength(FirstLengthOctet: Byte; AStream: TStream): QWord;
var
  l,b: Byte;
  i: Byte;
begin
  result := 0;
  l := FirstLengthOctet and %01111111;
  if l = 0 then
    Result := -1
  else
    begin
    for i := 0 to l -1 do
      begin
      b := AStream.ReadByte;
      Result := (result shl 8) + b;
      end;
    end;
end;

function TcnocASN1BERDecoder.LoadElementFromStream(AStream: TStream; AFormat: TcnocASN1Format): TObject;
var
  FirstIdentifierOctet: Byte;
  AClass: TcnocASN1Class;
  TagNr: Int64;
  FirstLengthOctet: Byte;
  ALength: Int64;
  Encoding: TcnocASN1Encoding;
  DecodableClass: IcnocASN1DecodableClass;
begin
  Result := nil;
  if not Assigned(FTagFactory) then
    raise Exception.Create('Can not load ASN1-elements without a tag-factory.');

  FirstIdentifierOctet := AStream.ReadByte;

  case (FirstIdentifierOctet and %11000000) of
    %00000000 : AClass := cacUniversal;
    %01000000 : AClass := cacApplication;
    %10000000 : AClass := cacContextSpecific;
    %11000000 : AClass := cacPrivate;
  end;

  if ((FirstIdentifierOctet and %00100000) = 0) then
    Encoding := caePrimitive
  else
    Encoding := caeConstructed;

  if (FirstIdentifierOctet and %00011111) = %00011111 then
    TagNr := ReadHighTagNumber(AStream)
  else
    TagNr := FirstIdentifierOctet and %00011111;

  FirstLengthOctet := AStream.ReadByte;

  if (FirstIdentifierOctet = 0) and (FirstLengthOctet = 0) then
    // Assume that we encountered the end of an indefinite-length structure.
    Exit;

  if (FirstLengthOctet and %10000000) = 0 then
    ALength := FirstLengthOctet and %01111111
  else
    ALength := ReadLongFormLength(FirstLengthOctet, AStream);

  Result := FTagFactory.ObtainObjectForASN1Tag(AClass, TagNr);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Class with TagNr %d (%s) is not registered.', [TagNr, ScnocASN1ClassString[AClass]]);

  if Supports(Result, IcnocASN1DecodableClass, DecodableClass) then
    begin
    DecodableClass.ParseASN1Content(AClass, TagNr, Encoding, ALength, Self, AStream, AFormat);
    end
  else
    raise Exception.CreateFmt('The %s class can not be used to decode ASN-content. It should implement the IcnocASN1DecodableClass interface',[Result.ClassName]);
end;

procedure TcnocASN1BERDecoder.LoadFromStream(AStream: TStream; AFormat: TcnocASN1Format;
  ALength: Integer; AnOutputList: TObjectList);
var
  Element: TObject;
  StartPos: Int64;
begin
  StartPos := AStream.Position;
  Element := LoadElementFromStream(AStream, AFormat);
  while Assigned(Element) do
    begin
    AnOutputList.Add(Element);
    if (ALength < 1) or (AStream.Position < (StartPos + ALength)) then
      Element := LoadElementFromStream(AStream, AFormat)
    else
      Element := nil;
    end;
end;

function TcnocASN1BERDecoder.Read128BaseInteger(AStream: TStream; out ASize: Integer): Int64;
var
  b: byte;
  value: QWord;
begin
  value := 0;
  ASize := 0;

  repeat
  b := AStream.ReadByte;
  inc(ASize);
  value := (value shl 7) + (b and %01111111);
  until ( b and (%10000000) ) = 0;

  Result := value;
end;

end.

