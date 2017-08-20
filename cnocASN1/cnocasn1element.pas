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
  cnocASN1,
  cnocASN1TagFactory,
  cnocASN1Decoder,
  cnocASN1Encoder;

type
  { TcnocASN1CustomElement }

  TcnocASN1CustomElement = class(IcnocASN1DecodableClass, IcnocASN1EncodableClass)
  private
    FContentLength: Integer;
  protected
    FEncoding: TcnocASN1Encoding;
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
    property Content: TBytes read FContent write FContent;
    property Items: TcnocASN1ElementList read GetItems write SetItems;
  end;

  { TcnocASN1UniversalElement }

  TcnocASN1UniversalElement = class(TcnocASN1CustomElement)
  public
    class function GetClass: TcnocASN1Class; override;
    class procedure RegisterUniversalASNElements(AFactory: TcnocASN1TagFactory);
  end;

  { TcnocASN1IntegerElement }

  TcnocASN1IntegerElement = class(TcnocASN1UniversalElement)
  private
    FValue: Int64;
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
    procedure ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding;
      ALength: Integer; ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    property AsInt64: Int64 read FValue write FValue;
  end;

  { TcnocASN1SetElement }

  TcnocASN1SetElement = class(TcnocASN1UniversalElement)
  private
    FItems: TcnocASN1ElementList;
    function GetItems: TcnocASN1ElementList;
    procedure SetItems(AValue: TcnocASN1ElementList);
  public
    destructor Destroy; override;
    class function GetClassName: string; override;
    class function GetTagNr: Int64; override;
    procedure ParseASN1Content(
      AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding; ALength: Integer;
      ADecoder: TcnocASN1BERDecoder; AContentStream: TStream; AFormat: TcnocASN1Format); override;
    property Items: TcnocASN1ElementList read GetItems write SetItems;
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
    property Values: TBoundArray read FValues write FValues;
  end;

  { TcnocASN1NullElement }

  TcnocASN1NullElement = class(TcnocASN1UniversalElement)
  public
    class function GetTagNr: Int64; override;
    class function GetClassName: string; override;
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
  FValue := Format('%s-%s-%s %s:%s', [YearStr, MonStr, HourStr, HourStr, MinStr]);
  if SecStr<>'' then
    FValue := FValue + ':' + SecStr;

  if OffStr = 'Z' then
    FValue := FValue + OffStr
  else
    FValue := FValue + OffStr + HourOffStr + '''' + MinOffStr;
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

{ TcnocASN1NullElement }

class function TcnocASN1NullElement.GetTagNr: Int64;
begin
  Result := 5;
end;

class function TcnocASN1NullElement.GetClassName: string;
begin
  Result := 'NULL';
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

{ TcnocASN1SetElement }

procedure TcnocASN1SetElement.SetItems(AValue: TcnocASN1ElementList);
begin
  Items.Assign(AValue);
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
var
  Element: TObject;
begin
  inherited;
  assert(ALength = -1, 'Definite length sequences are not supported.');
  ADecoder.LoadFromStream(AContentStream, AFormat, ALength, Items);
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
end;

procedure TcnocASN1CustomElement.StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  Raise Exception.Create('Streaming ' + ClassName + ' elements is not implemented');
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
  if Encoding = caeConstructed then
    AnEncoder.SaveToStream(AContentStream, AFormat, ContentLength, Items)
  else
    AContentStream.WriteBuffer(FContent[0], Length(FContent));
end;

{ TcnocASN1IntegerElement }

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
begin
  inherited;
  if AnEncoding<>caePrimitive then
    raise Exception.Create('Only primitive integer-types are supported');
  Assert(ALength>0);

  FValue := 0;
  for i := 0 to ALength -1 do
    begin
    B := AContentStream.ReadByte;
    FValue := FValue shl 8 + B;
    end;
end;

end.

