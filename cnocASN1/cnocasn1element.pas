{ Classes which can be encoded/decoded into the Ber/Der-format

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

unit cnocASN1Element;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  strutils,
  math,
  FmtBCD,
  cnocASN1,
  cnocASN1TagFactory,
  cnocASN1Decoder,
  cnocASN1Encoder;

type
  { TcnocASN1CustomElement }

  TcnocASN1CustomElement = class(IcnocASN1DecodableClass, IcnocASN1EncodableClass)
  protected
    FEncoding: TcnocASN1Encoding;
    FContentLength: Integer;
    function GetActualClass: TcnocASN1Class; virtual;
    function GetActualTagNr: Integer; virtual;
  public
    class function GetTagNr: Int64; virtual; abstract;
    class function GetClass: TcnocASN1Class; virtual; abstract;
    class function GetClassName: string; virtual;

    // IcnocASN1DecodableClass
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); virtual;

    // IcnocASN1EncodableClass
    function GetASN1HeaderInfo(
      out AClass: TcnocASN1Class; out ATag: Integer; out AnEncoding: TcnocASN1Encoding; out ALength: QWord;
      AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean; virtual;
    procedure StreamASN1Content(
      AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format); virtual;

    procedure Assign(ASource: TcnocASN1CustomElement); virtual;

    property ActualTagNr: Integer read GetActualTagNr;
    property ActualClass: TcnocASN1Class read GetActualClass;
    property Encoding: TcnocASN1Encoding read FEncoding;
    property ContentLength: Integer read FContentLength;
  end;
  TcnocASN1CustomElementClass = class of TcnocASN1CustomElement;
  TcnocASN1ElementList = TObjectList;

  { TcnocASN1GenericElement }

  TcnocASN1GenericElement = class(TcnocASN1CustomElement)
  private
    FContent: TBytes;
    FClass: TcnocASN1Class;
    FTagNr: Integer;
    FItems: TcnocASN1ElementList;
    function GetItems: TcnocASN1ElementList;
    procedure SetItems(AValue: TcnocASN1ElementList);
  protected
    function GetActualTagNr: Integer; override;
    function GetActualClass: TcnocASN1Class; override;
  public
    class function GetTagNr: Int64; override;
    class function GetClass: TcnocASN1Class; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property Content: TBytes read FContent write FContent;
    property Items: TcnocASN1ElementList read GetItems write SetItems;
    property TagNr: Integer read FTagNr write FTagNr;
    property ASN1Class: TcnocASN1Class read FClass write FClass;
    property Encoding: TcnocASN1Encoding read FEncoding write FEncoding;
    property ContentLength: Integer read FContentLength write FContentLength;
  end;

  { TcnocASN1UniversalElement }

  TcnocASN1UniversalElement = class(TcnocASN1CustomElement)
  public
    class function GetClass: TcnocASN1Class; override;
    class procedure RegisterUniversalASNElements(AFactory: TcnocASN1TagFactory);
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
  end;

  { TcnocASN1IntegerElement }


  TcnocASN1IntegerElement = class(TcnocASN1UniversalElement)
  protected
    type TcnocASN1IntegerElementStoreFormat = (sfInt, sfBCD, sfBytes);
  var
    FValue: Int64;
    FBCDValue: tBCD;
    // Keeps the value as signed hexadecimal representation (two complements)
    FBinaryValue: TBytes;
    FUsedFormat: TcnocASN1IntegerElementStoreFormat;
    function CalculateAmountOfOctets: Integer;
    function BCDToBytes(AValue: tBCD): TBytes;

    function GetAsBinarySigned: TBytes;
    procedure SetAsBinarySigned(AValue: TBytes);

    function GetAsBinaryUnsigned: TBytes;
    procedure SetAsBinaryUnsigned(AValue: TBytes);

    function GetBCDValue: tBCD;
    procedure SetBCDValue(AValue: tBCD);

    function GetValue: Int64;
    procedure SetValue(AValue: Int64);
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding;
      ALength: Integer; ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property AsInt64: Int64 read GetValue write SetValue;
    property AsBCD: tBCD read GetBCDValue write SetBCDValue;
    property AsBinarySigned: TBytes read GetAsBinarySigned write SetAsBinarySigned;
    property AsBinaryUnsigned: TBytes read GetAsBinaryUnsigned write SetAsBinaryUnsigned;
  end;

  { TcnocASN1SetElement }

  TcnocASN1SetElement = class(TcnocASN1UniversalElement)
  private
    FFixedLength: Boolean;
    FItems: TcnocASN1ElementList;
    function GetItems: TcnocASN1ElementList;
    procedure SetItems(AValue: TcnocASN1ElementList);
    function CalculateLength(AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Int64;
  public
    destructor Destroy; override;
    class function GetClassName: string; override;
    class function GetTagNr: Int64; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;
    property Items: TcnocASN1ElementList read GetItems write SetItems;
    property FixedLength: Boolean read FFixedLength write FFixedLength;
  end;

   { TcnocASN1SequenceElement }

   TcnocASN1SequenceElement = class(TcnocASN1SetElement)
   public
     class function GetClassName: string; override;
     class function GetTagNr: Int64; override;
   end;


  { TcnocASN1ObjectIdentifierElement }

  TcnocASN1ObjectIdentifierElement = class(TcnocASN1UniversalElement)
  private
    FValues: TBoundArray;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property Values: TBoundArray read FValues write FValues;
  end;

  { TcnocASN1NullElement }

  TcnocASN1NullElement = class(TcnocASN1UniversalElement)
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;

    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;
  end;

  { TcnocASN1UniversalStringElement }

  TcnocASN1UniversalStringElement = class(TcnocASN1UniversalElement)
  private
    FValue: UCS4String;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property AsUTF32String: UCS4String read FValue write FValue;
  end;

  { TcnocASN1UTF8StringElement }

  TcnocASN1UTF8StringElement = class(TcnocASN1UniversalElement)
  private
    FValue: UTF8String;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property AsUTF8String: UTF8String read FValue write FValue;
  end;

  { TcnocASN1UTCTimeElement }

  TcnocASN1UTCTimeElement = class(TcnocASN1UniversalElement)
  private
    FValue: UTF8String;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    //property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsUTF8String: UTF8String read FValue write FValue;
  end;

  { TcnocASN1BITStringElement }

  TcnocASN1BITStringElement = class(TcnocASN1UniversalElement)
  private
    FValue: UTF8String;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;

    procedure Assign(ASource: TcnocASN1CustomElement); override;

    property AsUTF8String: UTF8String read FValue write FValue;
  end;


implementation

{ TcnocASN1BITStringElement }

class function TcnocASN1BITStringElement.GetTagNr: Int64;
begin
  Result := 3;
end;

class function TcnocASN1BITStringElement.GetClassName: string;
begin
  Result := 'BIT STRING';
end;

procedure TcnocASN1BITStringElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
var
  Padding: Byte;
  B: Byte;
  i: Integer;
begin
  inherited;
  Padding := AContentStream.ReadByte;
  FValue := '';
  for I := 0 to ALength -2 do
    begin
    B := AContentStream.ReadByte;
    FValue := FValue + intToBin(B, 8);
    end;
  FValue := copy(FValue, 1, length(FValue) - Padding);
end;

procedure TcnocASN1BITStringElement.Assign(ASource: TcnocASN1CustomElement);
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1BITStringElement then
    FValue := TcnocASN1BITStringElement(ASource).FValue
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1UTCTimeElement }

class function TcnocASN1UTCTimeElement.GetTagNr: Int64;
begin
  Result := 23;
end;

class function TcnocASN1UTCTimeElement.GetClassName: string;
begin
  Result := 'UTCTIME';
end;

procedure TcnocASN1UTCTimeElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
var
  YearStr,
  MonStr,
  DayStr,
  HourStr,
  MinStr,
  SecStr: string;
  OffStr,
  HourOffStr,
  MinOffStr: string;
  RawStr: UTF8String;
  C: AnsiChar;
  OffIndex: Integer;
begin
  inherited;
  if AnEncoding=caePrimitive then
    begin
    SetLength(RawStr, ALength);
    AContentStream.Read(RawStr[1], ALength);
    YearStr := Copy(RawStr, 1, 2);
    MonStr := Copy(RawStr, 3, 2);
    DayStr := Copy(RawStr, 5, 2);
    HourStr := Copy(RawStr, 7, 2);
    MinStr := Copy(RawStr, 9, 2);
    C := RawStr[11];
    if C in ['Z', '-', '+'] then
      begin
      OffIndex := 11;
      SecStr := '';
      end
    else
      begin
       OffIndex := 13;
       SecStr := Copy(RawStr, 11, 2);
      end;
    OffStr := Copy(RawStr, OffIndex, 1);
    if (OffStr='+') or (OffStr='-') then
      begin
      HourOffStr := Copy(RawStr, OffIndex+1, 2);
      MinOffStr := Copy(RawStr, OffIndex+3, 2);
      end
    else
      begin
      HourOffStr := '';
      MinOffStr := '';
      end;
    end
  else
    begin
    raise Exception.Create('Constructed encoding of UTCTime is not supported.');
    end;
  FValue := Format('%s-%s-%s %s:%s', [YearStr, MonStr, DayStr, HourStr, MinStr]);
  if SecStr<>'' then
    FValue := FValue + ':' + SecStr;

  if OffStr = 'Z' then
    FValue := FValue + OffStr
  else
    FValue := FValue + OffStr + HourOffStr + '''' + MinOffStr;
end;

procedure TcnocASN1UTCTimeElement.Assign(ASource: TcnocASN1CustomElement);
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1UTCTimeElement then
    FValue := TcnocASN1UTCTimeElement(ASource).FValue
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1UTF8StringElement }

class function TcnocASN1UTF8StringElement.GetTagNr: Int64;
begin
  Result := 12;
end;

class function TcnocASN1UTF8StringElement.GetClassName: string;
begin
  Result := 'UTF8STRING';
end;

procedure TcnocASN1UTF8StringElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
begin
  inherited;
  Assert(ALength>-1);
  SetLength(FValue, ALength);
  AContentStream.Read(FValue[1], ALength);
end;

procedure TcnocASN1UTF8StringElement.Assign(ASource: TcnocASN1CustomElement);
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1UTF8StringElement then
    FValue := TcnocASN1UTF8StringElement(ASource).FValue
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1UniversalStringElement }

class function TcnocASN1UniversalStringElement.GetTagNr: Int64;
begin
  Result := 28;
end;

class function TcnocASN1UniversalStringElement.GetClassName: string;
begin
  Result := 'UNIVERSALSTRING';
end;

procedure TcnocASN1UniversalStringElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
begin
  inherited;
  Assert(ALength>-1);
  SetLength(FValue, ALength);
  AContentStream.Read(FValue[0], ALength);
end;

procedure TcnocASN1UniversalStringElement.Assign(ASource: TcnocASN1CustomElement);
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1UniversalStringElement then
    FValue := TcnocASN1UniversalStringElement(ASource).FValue
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1NullElement }

class function TcnocASN1NullElement.GetTagNr: Int64;
begin
  Result := 5;
end;

class function TcnocASN1NullElement.GetClassName: string;
begin
  Result := 'NULL';
end;

function TcnocASN1NullElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  Result := inherited GetASN1HeaderInfo(AClass, ATag, AnEncoding, ALength, AnEncoder, AFormat);
  AnEncoding := caePrimitive;
  ALength := 0;
end;

procedure TcnocASN1NullElement.StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  // Null means nothing....
end;

{ TcnocASN1SequenceElement }

class function TcnocASN1SequenceElement.GetClassName: string;
begin
  Result := 'SEQUENCE';
end;

class function TcnocASN1SequenceElement.GetTagNr: Int64;
begin
  Result := 16;
end;

{ TcnocASN1ObjectIdentifierElement }

class function TcnocASN1ObjectIdentifierElement.GetTagNr: Int64;
begin
  Result := 6;
end;

class function TcnocASN1ObjectIdentifierElement.GetClassName: string;
begin
  Result := 'OBJECT IDENTIFIER';
end;

procedure TcnocASN1ObjectIdentifierElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
var
  B: Byte;
  i,l: Integer;
begin
  inherited;
  Assert(ALength>0);
  SetLength(FValues, 2);
  B := AContentStream.ReadByte;
  FValues[0] := B div 40;
  FValues[1] := B mod 40;
  i := 1;
  while i < ALength do
    begin
    setlength(FValues, Length(FValues) +1);
    FValues[High(FValues)] := ADecoder.Read128BaseInteger(AContentStream, l);
    i := i + l;
    end;
end;

function TcnocASN1ObjectIdentifierElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out
  ATag: Integer; out AnEncoding: TcnocASN1Encoding; out ALength: QWord;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean;
var
  i: Integer;
begin
  Result := inherited GetASN1HeaderInfo(AClass, ATag, AnEncoding, ALength, AnEncoder, AFormat);
  AnEncoding := caePrimitive;
  ALength := 1;
  for i := 2 to High(FValues) do
    ALength := ALength + AnEncoder.GetOctetLengthFor128BaseInteger(FValues[i]);
end;

procedure TcnocASN1ObjectIdentifierElement.StreamASN1Content(AContentStream: TStream;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
var
  i: Integer;
begin
  if length(FValues) < 2 then
    raise Exception.Create('An object identifier must have at least two elements.');
  AContentStream.WriteByte((40 * FValues[0]) + FValues[1]);
  for i := 2 to High(FValues) do
    AnEncoder.Write128BaseInteger(AContentStream, FValues[i]);
end;

procedure TcnocASN1ObjectIdentifierElement.Assign(ASource: TcnocASN1CustomElement);
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1ObjectIdentifierElement then
    FValues := TcnocASN1ObjectIdentifierElement(ASource).FValues
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1SetElement }

procedure TcnocASN1SetElement.SetItems(AValue: TcnocASN1ElementList);
begin
  Items.Assign(AValue);
end;

function TcnocASN1SetElement.CalculateLength(AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Int64;
var
  TmpLength: QWord;
  i: Integer;
begin
  if FFixedLength then
    begin
    Result := 0;
    for i := 0 to FItems.Count -1 do
      begin
      TmpLength := AnEncoder.GetElementLength(FItems[i] as IcnocASN1EncodableClass, AFormat, True);
      if TmpLength = High(TmpLength) then
        begin
        Result := -1;
        Break;
        end
      else
        Result := Result + TmpLength;
      end;
    end
  else
    Result := -1;
end;

function TcnocASN1SetElement.GetItems: TcnocASN1ElementList;
begin
  if not Assigned(FItems) then
    FItems := TcnocASN1ElementList.Create(True);
  Result := FItems;
end;

destructor TcnocASN1SetElement.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

class function TcnocASN1SetElement.GetClassName: string;
begin
  Result := 'SET';
end;

class function TcnocASN1SetElement.GetTagNr: Int64;
begin
  Result :=17;
end;

procedure TcnocASN1SetElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
begin
  inherited;
  if ALength > -1 then
    FFixedLength := True;
  ADecoder.LoadFromStream(AContentStream, AFormat, ALength, Items);
end;

function TcnocASN1SetElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  Result := inherited GetASN1HeaderInfo(AClass, ATag, AnEncoding, ALength, AnEncoder, AFormat);
  AnEncoding := caeConstructed;
  ALength := CalculateLength(AnEncoder, AFormat);
end;

procedure TcnocASN1SetElement.StreamASN1Content(AContentStream: TStream;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  AnEncoder.SaveToStream(AContentStream, AFormat, CalculateLength(AnEncoder, AFormat), FItems);
end;

{ TcnocASN1UniversalElement }

class function TcnocASN1UniversalElement.GetClass: TcnocASN1Class;
begin
  Result := cacUniversal;
end;

class procedure TcnocASN1UniversalElement.RegisterUniversalASNElements(AFactory: TcnocASN1TagFactory);

  procedure RegisterElement(ElementClass: TcnocASN1CustomElementClass);
  begin
    AFactory.RegisterASN1Type(ElementClass.GetClass, ElementClass.GetTagNr, ElementClass);
  end;

begin
  RegisterElement(TcnocASN1IntegerElement);
  RegisterElement(TcnocASN1SetElement);
  RegisterElement(TcnocASN1SequenceElement);
  RegisterElement(TcnocASN1ObjectIdentifierElement);
  RegisterElement(TcnocASN1NullElement);
  RegisterElement(TcnocASN1UniversalStringElement);
  RegisterElement(TcnocASN1UTF8StringElement);
  RegisterElement(TcnocASN1UTCTimeElement);
  RegisterElement(TcnocASN1BITStringElement);
end;

function TcnocASN1UniversalElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer;
  out AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  Result := True;
  AClass := GetClass;
  ATag := GetTagNr;
end;

{ TcnocASN1CustomElement }

function TcnocASN1CustomElement.GetActualClass: TcnocASN1Class;
begin
  Result := GetClass;
end;

function TcnocASN1CustomElement.GetActualTagNr: Integer;
begin
  Result := GetTagNr;
end;

class function TcnocASN1CustomElement.GetClassName: string;
begin
  Result := 'unknown';
end;

procedure TcnocASN1CustomElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
begin
  FEncoding := AnEncoding;
  FContentLength := ALength;
end;

function TcnocASN1CustomElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean;
begin
  Raise Exception.Create('Streaming ' + ClassName + ' elements is not implemented');
  Result := True;
end;

procedure TcnocASN1CustomElement.StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  Raise Exception.Create('Streaming ' + ClassName + ' elements is not implemented');
end;

procedure TcnocASN1CustomElement.Assign(ASource: TcnocASN1CustomElement);
begin
  FEncoding := ASource.FEncoding;
  FContentLength := ASource.ContentLength;
end;

{ TcnocASN1GenericElement }

function TcnocASN1GenericElement.GetItems: TcnocASN1ElementList;
begin
  if not Assigned(FItems) then
    FItems := TcnocASN1ElementList.Create(True);
  Result := FItems;
end;

procedure TcnocASN1GenericElement.SetItems(AValue: TcnocASN1ElementList);
begin
  Items.Assign(AValue);
end;

function TcnocASN1GenericElement.GetActualTagNr: Integer;
begin
  Result := FTagNr;
end;

function TcnocASN1GenericElement.GetActualClass: TcnocASN1Class;
begin
  Result := FClass;
end;

class function TcnocASN1GenericElement.GetTagNr: Int64;
begin
  Result := -1;
end;

class function TcnocASN1GenericElement.GetClass: TcnocASN1Class;
begin
  Result := cacUnknown;
end;

procedure TcnocASN1GenericElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
begin
  Inherited;
  FTagNr := ATag;
  FClass := AClass;
  if AnEncoding = caeConstructed then
    begin
    ADecoder.LoadFromStream(AContentStream, AFormat, ALength, Items);
    end
  else
    begin
    Assert(ALength > 0);
    SetLength(FContent, ALength);
    AContentStream.ReadBuffer(FContent[0], ALength);
    end;
end;

function TcnocASN1GenericElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean;
begin
  Result := True;
  AClass := ActualClass;
  ATag := ActualTagNr;
  AnEncoding := Encoding;
  ALength := ContentLength
end;

procedure TcnocASN1GenericElement.StreamASN1Content(AContentStream: TStream;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  if Length(FContent) > 0 then
    AContentStream.WriteBuffer(FContent[0], Length(FContent));
  if Items.Count > 0 then
    AnEncoder.SaveToStream(AContentStream, AFormat, ContentLength, Items)
end;

procedure TcnocASN1GenericElement.Assign(ASource: TcnocASN1CustomElement);
var
  GenSource: TcnocASN1GenericElement;
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1GenericElement then
    begin
    GenSource := TcnocASN1GenericElement(ASource);
    FContent := GenSource.FContent;
    FClass := GenSource.FClass;
    FTagNr := GenSource.FTagNr;
    //FItems.Assign(GenSource.FItems);
    end
  else
    raise Exception.Create('Unsupported type for assignment');
end;

{ TcnocASN1IntegerElement }

function TcnocASN1IntegerElement.CalculateAmountOfOctets: Integer;
var
  v: Int64;
begin
  case FUsedFormat of
    sfInt:
      begin
      if FValue < 0 then
        v := not(FValue)
      else
        v := FValue;

      if v=0 then
        Result := 1
      else
        begin
        v := Floor(Log2(v)) +2;
        Result := ((v -1) div 8) +1;
        end;
      end;
    sfBCD:
      Result := Length(BCDToBytes(FBCDValue));
    sfBytes:
      Result := Length(FBinaryValue);
  end;
end;

function TcnocASN1IntegerElement.BCDToBytes(AValue: tBCD): TBytes;
var
  DivVal: tBCD;
  ModVal: Byte;
  i: Integer;
  Buf: TBytes;
  IsNegative: Boolean;
begin
  IsNegative := IsBCDNegative(AValue);
  if IsNegative then
    begin
    BCDNegate(AValue);
    AValue := AValue -1;
    end;
  SetLength(Buf, BCDPrecision(AValue));

  i := 0;
  repeat
  DivVal := AValue / 256;
  NormalizeBCD(DivVal, DivVal, BCDPrecision(DivVal), 0);
  ModVal := AValue - (DivVal * 256);

  AValue := DivVal;

  if IsNegative then
    ModVal := not(ModVal);

  Buf[High(Buf)-i] := ModVal;
  inc(i);
  until DivVal = 0;

  SetLength(Result, i);
  move(Buf[Length(Buf)-i], Result[0], i);
end;

function TcnocASN1IntegerElement.GetAsBinarySigned: TBytes;
var
  l: Integer;
  ValueLE: Int64;
begin
  case FUsedFormat of
    sfBCD: Result := BCDToBytes(FBCDValue);
    sfInt:
      begin
      l := CalculateAmountOfOctets;
      ValueLE := NtoBE(FValue);
      SetLength(Result, l);
      move(ValueLE, Result[0], l);
      end;
    sfBytes: Result := FBinaryValue;
  end;
end;

function TcnocASN1IntegerElement.GetBCDValue: tBCD;

  function PowerBCD(Base, Exponent: Integer): tBCD;
  var
    i: Integer;
  begin
    result := 1;
    for i := 0 to Exponent -1 do
      begin
      result := Result * Base;
      end;
  end;

var
  i: Integer;
begin
  case FUsedFormat of
    sfBCD: Result := FBCDValue;
    sfInt: Result := IntegerToBCD(FValue);
    sfBytes:
      begin
      Result := 0;
      for i := 0 to high(FBinaryValue) do
        Result := Result + (tBCD(FBinaryValue[high(FBinaryValue) - i]) * (PowerBCD(256, i)));
      end;
  end;
end;

procedure TcnocASN1IntegerElement.SetAsBinarySigned(AValue: TBytes);
begin
  FBinaryValue := AValue;
  FUsedFormat := sfBytes;
end;

function TcnocASN1IntegerElement.GetAsBinaryUnsigned: TBytes;
begin
  Result := GetAsBinarySigned;
  if Length(Result) > 0 then
    begin
    if (Result[0] and %10000000) = %10000000 then
      raise Exception.Create('Overflow, can not represent negative value as unsigned.');
    end;
end;

procedure TcnocASN1IntegerElement.SetAsBinaryUnsigned(AValue: TBytes);
var
  l: Integer;
begin
  FBinaryValue := AValue;
  FUsedFormat := sfBytes;

  if Length(FBinaryValue) > 0 then
    begin
    if (FBinaryValue[0] and %10000000) = %10000000 then
      begin
      // Add leading zero, or else the number will be regarded as negative.
      l := length(FBinaryValue);
      SetLength(FBinaryValue, l+1);
      Move(FBinaryValue[0], FBinaryValue[1], l);
      FBinaryValue[0] := 0;
      end
    end
end;

function TcnocASN1IntegerElement.GetValue: Int64;
begin
  case FUsedFormat of
    sfBCD: Result := BCDToInteger(FBCDValue);
    sfInt: Result := FValue;
    sfBytes: raise Exception.Create('Value nog accessible as int64');
  end;
end;

procedure TcnocASN1IntegerElement.SetBCDValue(AValue: tBCD);
begin
  FBCDValue := AValue;
  FUsedFormat := sfBCD;
end;

procedure TcnocASN1IntegerElement.SetValue(AValue: Int64);
begin
  FValue := AValue;
  FUsedFormat := sfInt;
end;

class function TcnocASN1IntegerElement.GetTagNr: Int64;
begin
  Result := 2;
end;

class function TcnocASN1IntegerElement.GetClassName: string;
begin
  Result := 'INTEGER';
end;

procedure TcnocASN1IntegerElement.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
var
  i: Integer;
  B: Byte;
  IsNegative: Boolean;
begin
  inherited;
  if AnEncoding<>caePrimitive then
    raise Exception.Create('Only primitive integer-types are supported');
  Assert(ALength>0);

  if ALength <= SizeOf(FValue) then
    begin
    FValue := 0;
    for i := 0 to ALength -1 do
      begin
      B := AContentStream.ReadByte;
      if i = 0 then
        IsNegative := (B and %10000000) = %10000000;
      if IsNegative then
        b := not(B);
      FValue := FValue shl 8 + B;
      end;
    if IsNegative then
      FValue := -FValue -1;
    FUsedFormat := sfInt;
    end
  else if ALength <= 24 then // Arbitrary, but 200 bit RSA-keys do not work with 26
    begin
    FBCDValue := 0;
    for i := 0 to ALength -1 do
      begin
      B := AContentStream.ReadByte;
      if i = 0 then
        IsNegative := (B and %10000000) = %10000000;
      if IsNegative then
        b := not(B);
      FBCDValue := (FBCDValue * 256) + B;
      end;
    if IsNegative then
      FBCDValue := -FBCDValue -1;
    FUsedFormat := sfBCD;
    end
  else
    begin
    SetLength(FBinaryValue, ALength);
    AContentStream.ReadBuffer(FBinaryValue[0], ALength);
    FUsedFormat := sfBytes;
    end;
end;

function TcnocASN1IntegerElement.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  Result := inherited GetASN1HeaderInfo(AClass, ATag, AnEncoding, ALength, AnEncoder, AFormat);
  AnEncoding := caePrimitive;
  ALength := CalculateAmountOfOctets;
end;

procedure TcnocASN1IntegerElement.StreamASN1Content(AContentStream: TStream;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
var
  ValueLE: QWord;
  l: Integer;
  i: Integer;
  B: PByte;
  Buf: TBytes;
begin
  case FUsedFormat of
    sfInt:
      begin
      l := CalculateAmountOfOctets;
      ValueLE := NtoBE(FValue);
      B := @ValueLE;
      Inc(B, SizeOf(ValueLE) -l);
      for i := 0 to l -1 do
        begin
        AContentStream.WriteByte(B^);
        inc(B);
        end;
      end;
    sfBCD:
      begin
      Buf := BCDToBytes(FBCDValue);
      AContentStream.WriteBuffer(Buf[0], Length(Buf));
      end;
    sfBytes:
      begin
      AContentStream.WriteBuffer(FBinaryValue[0], Length(FBinaryValue));
      end;
  end;
end;

procedure TcnocASN1IntegerElement.Assign(ASource: TcnocASN1CustomElement);
var
  IntSource: TcnocASN1IntegerElement;
begin
  inherited Assign(ASource);
  if ASource is TcnocASN1IntegerElement then
    begin
    IntSource := TcnocASN1IntegerElement(ASource);
    FUsedFormat := IntSource.FUsedFormat;
    case FUsedFormat of
      sfInt:
        begin
        FValue := IntSource.FValue;
        end;
      sfBCD:
        begin
        FBCDValue := IntSource.FBCDValue;
        end;
      sfBytes:
        begin
        FBinaryValue := IntSource.FBinaryValue;
        end;
    end
    end
  else
    raise Exception.Create('Unsupported type for assignment');
end;

end.

