{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profile log reader class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPPReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead;

type
  TTrace = record
    position: string;
    elapsed: longint;    //msec since first frame was created
    func: string;        //function - procedure name that made the call
    source: string;      //sourcefile where procedure is located
    line: integer;       //line number in sourcefile
    heapused: integer;   //bytes in use on the heap
  end;

  TTraceList = array of TTrace;

  { TFPPReader }

  TFPPReader = class(TObject)
  private
    FCount: integer;
    FList: TTraceList;

    function GetList(Index: integer): TTrace;
    procedure SetList(Index: integer; const AValue: TTrace);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure AddData(position, time, func, source, line, heapused: string);
    property Count: integer read FCount;
    property List[Index: integer]: TTrace read GetList write SetList; default;
  end;

implementation

{ TFPPReader }

function TFPPReader.GetList(Index: integer): TTrace;
begin
  Result := FList[Index];
end;

procedure TFPPReader.SetList(Index: integer; const AValue: TTrace);
begin
  FList[Index] := AValue;
end;

constructor TFPPReader.Create(const AFileName: string);
var
  XMLDoc: TXMLDocument;
  Node: TDomNode;
  NodeList: TDomNodeList;
  i: integer;
  Attributes: TDOMNamedNodeMap;
begin
  if not FileExists(AFileName) then
  begin
    writeln('error: cannot find ', AFileName);
    halt;
  end;

  FCount := 0;
  SetLength(FList, FCount);

  ReadXMLFile(XMLDoc, AFileName);
  try
    Node := XMLDoc.FindNode('profilelog');
    Node := Node.FindNode('tracelog');

    if Assigned(Node) then
    begin
      NodeList := Node.GetChildNodes;

      for i := 0 to NodeList.Count - 1 do
      begin
        Attributes := NodeList[i].Attributes;
        AddData(Attributes.GetNamedItem('pos').NodeValue,
                Attributes.GetNamedItem('time').NodeValue,
                Attributes.GetNamedItem('func').NodeValue,
                Attributes.GetNamedItem('source').NodeValue,
                Attributes.GetNamedItem('line').NodeValue,
                Attributes.GetNamedItem('heapused').NodeValue);
      end;
    end;
  finally
    XMLDoc.Free;
  end;
end;

destructor TFPPReader.Destroy;
begin
  SetLength(FList, 0);
  inherited Destroy;
end;

procedure TFPPReader.AddData(position, time, func, source, line, heapused: string);
begin
  Inc(FCount);
  SetLength(FList, FCount);

  FList[Pred(FCount)].position := Trim(position);
  FList[Pred(FCount)].elapsed := StrToInt(time);
  FList[Pred(FCount)].func := func;
  FList[Pred(FCount)].source := source;
  FList[Pred(FCount)].line := StrToInt(line);
  FList[Pred(FCount)].heapused := StrToInt(heapused);
end;

end.

