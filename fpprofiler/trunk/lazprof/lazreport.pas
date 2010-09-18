{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Lazarus profiler report class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FPPReport, FPCallGraph, TASeries, Process,
  FileUtil;

type
  { TLazReport }

  TLazReport = class(TCustomFPPReport)
  private
    FMemSerie: TAreaSeries;
    FListView: TListView;
    FPNGFileName: string;
    procedure SetMemSerie(const AValue: TAreaSeries);
    procedure SetListView(const AValue: TListView);

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteTable; override;
    procedure CallGraph(ACallGraph: TFPCallGraph); override;
    property ListView: TListView read FListView write SetListView;
    property MemSerie: TAreaSeries read FMemSerie write SetMemSerie;
    property PNGFileName: string read FPNGFileName;
  end;

implementation

{ TLazReport }

procedure TLazReport.SetListView(const AValue: TListView);
begin
  if FListView = AValue then
    exit;
  FListView := AValue;
end;

procedure TLazReport.SetMemSerie(const AValue: TAreaSeries);
begin
  if FMemSerie = AValue then
    exit;
  FMemSerie := AValue;
end;

constructor TLazReport.Create;
begin

end;

destructor TLazReport.Destroy;
begin
  inherited Destroy;
end;

procedure TLazReport.WriteTable;
var
  r: integer;
  c: integer;
  ListItem: TListItem;
begin
  if not Assigned(ListView) then
    exit;

  //print data
  for r := 1 to FRowCount - 1 do
  begin
    ListItem := ListView.Items.Add;
    for c := 0 to FColumnCount - 1 do
    begin
      if c = 0 then
        ListItem.Caption := Cells[r, c]
      else
      begin
        ListItem.SubItems.Add(Cells[r, c]);

        if c = 6 then
          MemSerie.AddXY(r, StrToInt(Cells[r, c]));
      end;
    end;
  end;
end;

procedure TLazReport.CallGraph(ACallGraph: TFPCallGraph);
var
  i: integer;
  j: integer;
  t: Text;
  AProcess: TProcess;
  DOTFileName: string;
begin
  DOTFileName := IncludeTrailingPathDelimiter(GetTempDir) + 'temp.dot';

  Assign(t, DOTFileName);
  Rewrite(t);

  writeln(t, '# example compilation of dot script');
  writeln(t, '# dot -Tpng test.dot -o test.png');

  writeln(t, 'digraph G {');

  //definition of nodes
  for i := 0 to ACallGraph.Caller.Count - 1 do
    writeln(t, '  ', ACallGraph.Caller[i], ';');

  writeln(t);

  //definition of connections
  for i := 0 to ACallGraph.Caller.Count - 1 do
    for j := 0 to ACallGraph.Caller.Count - 1 do
      if ACallGraph.Called[i, j] <> 0 then
        writeln(t, '  ', ACallGraph.Caller[i], ' -> ', ACallGraph.Caller[j],
          ' [label="', ACallGraph.Called[i, j], ' calls"];');

  writeln(t, '}');

  Close(t);

  //generate the png file
  FPNGFileName := IncludeTrailingPathDelimiter(GetTempDir) + 'temp.png';
  try
    AProcess := TProcess.Create(nil);
    AProcess.CommandLine := 'dot -Tpng ' + DOTFileName + ' -o ' + FPNGFileName;
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.ShowWindow := swoHide;
    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;

end.

