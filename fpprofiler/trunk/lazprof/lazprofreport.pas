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

unit LazProfReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FPPReport, FPCallGraph, TASeries, Process,
  FileUtil;

type
  { TLazProfReport }

  TLazProfReport = class(TCustomFPPReport)
  private
    FAvgTimeSpent: double;
    FMemSerie: TAreaSeries;
    FListView: TListView;
    FPasses: integer;
    FPNGFileName: string;
    FTimeSpent: integer;
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

    procedure CalcStats(AUnitName, FuncName: string);
    property Passes: integer read FPasses;
    property TimeSpent: integer read FTimeSpent;
    property AvgTimeSpent: double read FAvgTimeSpent;
  end;

implementation

uses
  LazProfSettings;

{ TLazProfReport }

procedure TLazProfReport.SetListView(const AValue: TListView);
begin
  if FListView = AValue then
    exit;
  FListView := AValue;
end;

procedure TLazProfReport.SetMemSerie(const AValue: TAreaSeries);
begin
  if FMemSerie = AValue then
    exit;
  FMemSerie := AValue;
end;

procedure TLazProfReport.CalcStats(AUnitName, FuncName: string);
var
  r: integer;
  f: boolean;
  start: integer;
begin
  FPasses := 0;
  FTimeSpent := 0;
  FAvgTimeSpent := -1;

  //loop through the data and search for the correct unit/function
  f := false;
  for r := 1 to FRowCount - 1 do
  begin
    if (Cells[r, 1] = 'entry') and
       (UpperCase(Cells[r, 3]) = UpperCase(FuncName)) and
       (UpperCase(Cells[r, 4]) = UpperCase(AUnitName)) then
    begin
      Inc(FPasses);
      start := StrToInt(Cells[r, 2]);
    end;

    if (Cells[r, 1] = 'exit') and
       (UpperCase(Cells[r, 3]) = UpperCase(FuncName)) and
       (UpperCase(Cells[r, 4]) = UpperCase(AUnitName)) then
    begin
      FTimeSpent := FTimeSpent + StrToInt(Cells[r, 2]) - start;
    end;
  end;

  if FPasses <> 0 then
    FAvgTimeSpent := FTimeSpent / FPasses;
end;

constructor TLazProfReport.Create;
begin

end;

destructor TLazProfReport.Destroy;
begin
  inherited Destroy;
end;

procedure TLazProfReport.WriteTable;
var
  r: integer;
  c: integer;
  ListItem: TListItem;
begin
  if not Assigned(ListView) then
    exit;

  //clear any previous data
  ListView.Clear;
  MemSerie.Clear;

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

procedure TLazProfReport.CallGraph(ACallGraph: TFPCallGraph);
var
  i: integer;
  j: integer;
  t: Text;
  AProcess: TProcess;
  DOTFileName: string;
begin
  //prevent error messages if the dot executable is not found
  if not FileExists(XMLConfig.GetValue('LazProfOptions/GraphViz/Path', '')) then
    exit;

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
    AProcess.CommandLine := XMLConfig.GetValue('LazProfOptions/GraphViz/Path', '') + ' -Tpng ' + DOTFileName + ' -o ' + FPNGFileName;
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.ShowWindow := swoHide;
    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;

end.
