{ ASN.1 Ber/Der-encoder

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

unit cnocASN1Encoder;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  math,
  cnocASN1;

type

  { TcnocASN1BEREncoder }

  TcnocASN1BEREncoder = class
  private
  protected
    procedure WriteHighTagNumber(AStream: TStream; TagNr: Int64);
    procedure WriteLongFormLength(AStream: TStream; ALength: QWord);
  public
    //function LoadElementFromStream(AStream: TStream; AFormat: TcnocASN1Format): TObject;
    procedure SaveToStream(AStream: TStream; AFormat: TcnocASN1Format; ALength: Integer; AnObjectList: TObjectList);
    //function Read128BaseInteger(AStream: TStream; out ASize: Integer): Int64;
  end;

  IcnocASN1EncodableClass = Interface['{C9C07D0D-B55E-4FEF-BD67-31CBEAC5AB87}']
    function GetASN1HeaderInfo(
      out AClass: TcnocASN1Class; out ATag: Integer; out AnEncoding: TcnocASN1Encoding; out ALength: QWord;
      AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean;
    procedure StreamASN1Content(
      AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
  end;


implementation

{ TcnocASN1BEREncoder }

procedure TcnocASN1BEREncoder.WriteHighTagNumber(AStream: TStream; TagNr: Int64);
begin
  //
end;

procedure TcnocASN1BEREncoder.WriteLongFormLength(AStream: TStream; ALength: QWord);
var
  LengthLE: QWord;
  i: integer;
  B: PByte;
  Started: Boolean;
begin
  LengthLE := NtoBE(ALength);
  Started := False;
  B := @LengthLE;
  for i := 0 to SizeOf(ALength) -1 do
    begin
    if not Started and (B^ <> 0) then
      begin
      Started := True;
      AStream.WriteByte((SizeOf(QWord)-i) or %10000000);
      end;
    if Started then
      AStream.WriteByte(B^);
    inc(B);
    end;
end;

procedure TcnocASN1BEREncoder.SaveToStream(AStream: TStream; AFormat: TcnocASN1Format;
  ALength: Integer; AnObjectList: TObjectList);
var
  i, TagNr: Integer;
  Length: QWord;
  AnObject: TObject;
  EncodableClass: IcnocASN1EncodableClass;
  AClass: TcnocASN1Class;
  Encoding: TcnocASN1Encoding;
  FirstIdentifierOctet: Byte;
begin
  for i := 0 to AnObjectList.Count -1 do
    begin
    AnObject := AnObjectList.Items[i];
    if supports(AnObject, IcnocASN1EncodableClass, EncodableClass) then
      begin
      if EncodableClass.GetASN1HeaderInfo(AClass, TagNr, Encoding, Length, Self, AFormat) then
        begin
        case AClass of
          cacUniversal      : FirstIdentifierOctet := %00000000;
          cacApplication    : FirstIdentifierOctet := %01000000;
          cacContextSpecific: FirstIdentifierOctet := %10000000;
          cacPrivate        : FirstIdentifierOctet := %11000000;
        else
          raise Exception.Create('Unknown ASN.1-class, unable to encode object.');
        end;

        if Encoding = caeConstructed then
          FirstIdentifierOctet := FirstIdentifierOctet or %00100000;

        if TagNr < 31 then
          begin
          FirstIdentifierOctet := FirstIdentifierOctet or TagNr;
          AStream.WriteByte(FirstIdentifierOctet);
          end
        else
          begin
          FirstIdentifierOctet := FirstIdentifierOctet or %00011111;
          AStream.WriteByte(FirstIdentifierOctet);
          WriteHighTagNumber(AStream, TagNr);
          end;

        if (length=-1) then
          begin
          Assert(Encoding = caeConstructed);
          AStream.WriteByte(%10000000);
          end
        else if Length<128 then
          AStream.WriteByte(Length)
        else
          WriteLongFormLength(AStream, Length);

        EncodableClass.StreamASN1Content(AStream, Self, AFormat);
        end;
      end
    else
      raise Exception.CreateFmt('The %s class can not be used to encode as ASN-content. It should implement the IcnocASN1EncodableClass interface', [AnObject.ClassName]);
    end;
  if ALength = -1 then
    begin
    AStream.WriteByte(0);
    AStream.WriteByte(0);
    end;
end;

end.

