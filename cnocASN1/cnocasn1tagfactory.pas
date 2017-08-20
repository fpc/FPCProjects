{ ASN.1 Factory to provide the ASN.1 Ber/Der-decoder with classes

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

unit cnocASN1TagFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  cnocASN1;

type
  { TcnocASN1TagFactory }

  TcnocASN1TagClass = class
    TagClass: TClass;
    TagNr: Integer;
    ASN1Class: TcnocASN1Class;
  end;
  TcnocASN1TagClassList = specialize TFPGObjectList<TcnocASN1TagClass>;

  TcnocASN1TagFactory = class
  private
    FTagList: TcnocASN1TagClassList;
    function FindClassForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterASN1Type(ASN1Class: TcnocASN1Class; TagNr: Integer; TagClass: TClass);
    function GetClassForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): TClass;
    function ObtainObjectForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): TObject;
  end;

implementation

{ TcnocASN1TagFactory }

function TcnocASN1TagFactory.FindClassForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  For i := 0 to FTagList.Count -1 do
    begin
    if (FTagList[i].TagNr = ATag) and (FTagList[i].ASN1Class = AClass) then
      begin
      result := i;
      break;
      end;
    end;
end;

constructor TcnocASN1TagFactory.Create;
begin
  FTagList := TcnocASN1TagClassList.Create(True);
end;

destructor TcnocASN1TagFactory.Destroy;
begin
  FTagList.Free;
  inherited Destroy;
end;

procedure TcnocASN1TagFactory.RegisterASN1Type(ASN1Class: TcnocASN1Class; TagNr: Integer; TagClass: TClass);
var
  i: Integer;
  ANS1TagClass: TcnocASN1TagClass;
begin
  ANS1TagClass := TcnocASN1TagClass.Create;
  try
    ANS1TagClass.TagNr := TagNr;
    ANS1TagClass.ASN1Class := ASN1Class;
    ANS1TagClass.TagClass := TagClass;

    i := FindClassForASN1Tag(ASN1Class, TagNr);
    if i > -1 then
      FTagList[i] := ANS1TagClass
    else
      FTagList.Add(ANS1TagClass);
  except
    ANS1TagClass.Free;
    Raise;
  end;
end;

function TcnocASN1TagFactory.GetClassForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): TClass;
var
  i: Integer;
begin
  i := FindClassForASN1Tag(AClass, ATag);

  if (ATag<>-1) and (i=-1) then
    i := FindClassForASN1Tag(cacUnknown, -1);

  if i > -1 then
    Result := FTagList.Items[i].TagClass
  else
    Result := nil;
end;

function TcnocASN1TagFactory.ObtainObjectForASN1Tag(AClass: TcnocASN1Class; ATag: Integer): TObject;
var
  ElementClass: TClass;
begin
  ElementClass := GetClassForASN1Tag(AClass, ATag);
  if Assigned(ElementClass) then
    begin
    Result := ElementClass.newinstance;
    end
  else
    Result := nil;
end;

end.

