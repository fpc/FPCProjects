{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profile log writer class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPPWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead;
  
const
  _FPPROFLOG_ = 'fpprof.xml';
  
type

  { TFPPWriter }

  TFPPWriter = class(TObject)
  private
    XMLDoc: TXMLDocument;
    Node: TDomNode;
  public
    constructor Create; overload;
    constructor Create(Reload: boolean); overload;

    destructor Destroy; override;
    
    procedure AddTrace(position, time, func, source, line, heapused: string);
    procedure AddIgnoredFile(const AFileName: string);
    procedure CreateIgnored;
    procedure CreateTraceLog;
    procedure Save;
  end;

implementation

{ TFPPWriter }

constructor TFPPWriter.Create(Reload: boolean);
begin
  if Reload and FileExists(_FPPROFLOG_) then
    ReadXMLFile(XMLDoc, _FPPROFLOG_);

  if not Reload or not Assigned(XMLDoc) then
  begin
    XMLDoc := TXMLDocument.Create;
    XMLDoc.AppendChild(XMLDoc.CreateElement('profilelog'));
  end;
  Node := XMLDoc.FindNode('profilelog');
end;

constructor TFPPWriter.Create;
begin
  Create(False);
end;

destructor TFPPWriter.Destroy;
begin
  Save;
  XMLDoc.Free;

  inherited Destroy;
end;

procedure TFPPWriter.Save;
begin
  WriteXMLFile(XMLDoc, _FPPROFLOG_);
end;

procedure TFPPWriter.CreateTraceLog;
begin
  Node := XMLDoc.DocumentElement;

  //create the tracelog element
  Node.AppendChild(XMLDoc.CreateElement('tracelog'));
  Node := Node.FindNode('tracelog');
end;

procedure TFPPWriter.CreateIgnored;
begin
  Node := XMLDoc.DocumentElement;

  //create the ignored element
  Node.AppendChild(XMLDoc.CreateElement('ignored'));
  Node := Node.FindNode('ignored');
end;

procedure TFPPWriter.AddTrace(position, time, func, source, line, heapused: string);
var
  Element: TDomElement;
begin
  Element := XMLDoc.CreateElement('trace');
  with Element do
  begin
    AttribStrings['pos'] := position;
    AttribStrings['time'] := time;
    AttribStrings['func'] := func;
    AttribStrings['source'] := source;
    AttribStrings['line'] := line;
    AttribStrings['heapused'] := heapused;
  end;
  Node.AppendChild(Element);
end;

procedure TFPPWriter.AddIgnoredFile(const AFileName: string);
var
  Element: TDomElement;
begin
  Element := XMLDoc.CreateElement('file');
  Element.AttribStrings['name'] := AFileName;
  Node.AppendChild(Element);
end;

end.

