{ ASN.1 Parser/Encoder example

  Copyright (C) 2017 Joost van der Sluis (CNOC) joost@cnoc.nl

}

program asn1parser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  strutils,
  cnocASN1,
  cnocASN1Decoder,
  cnocASN1encoder,
  cnocASN1Element,
  cnocASN1TagFactory;

type

  { TASN1ParserExample }

  TASN1ParserExample = class(TCustomApplication)
  protected
    FASN1TagFactory: TcnocASN1TagFactory;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function ParseASN1File(AFilename: string): TcnocASN1ElementList;
    procedure DumpASN1ElementList(AnElementList: TcnocASN1ElementList);
    procedure EncodeASN1ElementList(ElementList: TcnocASN1ElementList; AFilename: string);
    procedure OutputElement(Prefix: string; AnElement: TcnocASN1CustomElement);
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TASN1ParserExample }

procedure TASN1ParserExample.DoRun;
var
  ErrorMsg: String;
  ElementList: TcnocASN1ElementList;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hi:o:u', ['help', 'inputfile:', 'outputfile:', 'universalclasses']);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') or not HasOption('i', 'inputfile') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  ElementList := ParseASN1File(GetOptionValue('i', 'inputfile'));
  try
    DumpASN1ElementList(ElementList);
    if HasOption('o', 'outputfile') then
      EncodeASN1ElementList(ElementList, GetOptionValue('o', 'outputfile'));
  finally
    ElementList.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TASN1ParserExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FASN1TagFactory := TcnocASN1TagFactory.Create;

  if HasOption('u', 'universalclasses') then
    TcnocASN1UniversalElement.RegisterUniversalASNElements(FASN1TagFactory);

  FASN1TagFactory.RegisterASN1Type(TcnocASN1GenericElement.GetClass, TcnocASN1GenericElement.GetTagNr, TcnocASN1GenericElement);
end;

function TASN1ParserExample.ParseASN1File(AFilename: string): TcnocASN1ElementList;
var
  FileStream: TFileStream;
  Decoder: TcnocASN1BERDecoder;
  ElementList: TcnocASN1ElementList;
begin
  FileStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    Decoder := TcnocASN1BERDecoder.Create;
    try
      Decoder.TagFactory := FASN1TagFactory;
      ElementList := TcnocASN1ElementList.Create(True);
      try
        Decoder.LoadFromStream(FileStream, cafDER, FileStream.Size, ElementList);
        Result := ElementList;
      except
        ElementList.Free;
      end;
    finally
      Decoder.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TASN1ParserExample.DumpASN1ElementList(AnElementList: TcnocASN1ElementList);
var
  i: Integer;
begin
  for i := 0 to AnElementList.Count -1 do
    OutputElement('', AnElementList[i] as TcnocASN1CustomElement);
end;

procedure TASN1ParserExample.EncodeASN1ElementList(ElementList: TcnocASN1ElementList; AFilename: string);
var
  FS: TFileStream;
  ASN1Encoder: TcnocASN1BEREncoder;
begin
  FS := TFileStream.Create(AFilename, fmCreate);
  try
    ASN1Encoder := TcnocASN1BEREncoder.Create;
    try
      ASN1Encoder.SaveToStream(FS, cafDER, 0, ElementList);
    finally
      ASN1Encoder.Free;
    end;
  finally
    FS.Free;
  end;
end;

procedure TASN1ParserExample.OutputElement(Prefix: string; AnElement: TcnocASN1CustomElement);
var
  Sequence: TcnocASN1SetElement;
  GenericElement: TcnocASN1GenericElement;
  i: Integer;
  StrVal: string;
begin
  if AnElement is TcnocASN1IntegerElement then
    StrVal := IntToStr(TcnocASN1IntegerElement(AnElement).AsInt64)
  else if AnElement is TcnocASN1ObjectIdentifierElement then
    begin
    for i := 0 to length(TcnocASN1ObjectIdentifierElement(AnElement).Values)  -1 do
      StrVal := StrVal + IntToStr(TcnocASN1ObjectIdentifierElement(AnElement).Values[i]) + '.';
    StrVal := TrimRightSet(StrVal, ['.']);
    end
  else if AnElement is TcnocASN1UTF8StringElement then
    StrVal := TcnocASN1UTF8StringElement(AnElement).AsUTF8String
  else if AnElement is TcnocASN1UTCTimeElement then
    StrVal := TcnocASN1UTCTimeElement(AnElement).AsUTF8String
  else if AnElement is TcnocASN1BITStringElement then
    StrVal := TcnocASN1BITStringElement(AnElement).AsUTF8String
  else
    StrVal := '';

  writeln(Prefix + Format('%s (%s, %d). Length: %d %s. %s',[AnElement.GetClassName, ScnocASN1ClassString[AnElement.ActualClass], AnElement.ActualTagNr, AnElement.ContentLength, ScnocASN1EncodingString[AnElement.Encoding], StrVal]));
  if (AnElement is TcnocASN1SetElement) then
    begin
    Sequence := TcnocASN1SetElement(AnElement);
    for i := 0 to Sequence.Items.Count -1 do
      OutputElement(Prefix + '  ', Sequence.Items[i] as TcnocASN1CustomElement);
    end;
  if (AnElement is TcnocASN1GenericElement) then
    begin
    GenericElement := TcnocASN1GenericElement(AnElement);
    for i := 0 to GenericElement.Items.Count -1 do
      OutputElement(Prefix + '  ', GenericElement.Items[i] as TcnocASN1CustomElement);
    end;
end;

destructor TASN1ParserExample.Destroy;
begin
  FASN1TagFactory.Free;
  inherited Destroy;
end;

procedure TASN1ParserExample.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' <options> ');
  writeln('');
  writeln('Possible options:');
  writeln('  -h                    Write this help and exit');
  writeln('  --inputfile=<value>   Reads the contents of a BER-encoded stream');
  writeln('    -i <value>');
  writeln('  --outputfile=<value>  Save the parsed contents to a BER-encoded stream again' );
  writeln('    -o <value>');
  writeln('  --universalclasses    Add support for some some basic universal ASN.1');
  writeln('    -u                  classes while parsing the stream');
end;

var
  Application: TASN1ParserExample;
begin
  Application := TASN1ParserExample.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.

