{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Lazarus profiler stats

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FPPStats, FPPReader, FPPReport, LazReport,
  TASeries, FPCallGraph;

type
  { TLazProfStats }

  TLazProfStats = class(TCustomProfStats)
  private
    FMemSerie: TAreaSeries;
    FListView: TListView;
    procedure SetMemSerie(const AValue: TAreaSeries);
    procedure SetListView(const AValue: TListView);

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
    property ListView: TListView read FListView write SetListView;
    property MemSerie: TAreaSeries read FMemSerie write SetMemSerie;
  end;


implementation

{ TLazProfStats }

procedure TLazProfStats.SetListView(const AValue: TListView);
begin
  if FListView = AValue then
    exit;
  FListView := AValue;
  TLazReport(FPPReport).ListView := ListView;
end;

procedure TLazProfStats.SetMemSerie(const AValue: TAreaSeries);
begin
  if FMemSerie = AValue then
    exit;
  FMemSerie := AValue;
  TLazReport(FPPReport).MemSerie := MemSerie;
end;

constructor TLazProfStats.Create(AReader: TFPPReader; const AValue: TFPPReportType);
begin
  FReader := AReader;
  FPPReport := TLazReport.Create;
end;

destructor TLazProfStats.Destroy;
begin
  inherited Destroy;
end;

procedure TLazProfStats.Run;
var
  i: integer;
  FPCallGraph: TFPCallGraph;
  Caller: TStrings;
begin
  inherited Run;

  FPPReport.Clear;
  FPPReport.Cells[0, 0] := '#';
  FPPReport.Cells[0, 1] := 'Position';
  FPPReport.Cells[0, 2] := 'Elapsed msec';
  FPPReport.Cells[0, 3] := 'Function';
  FPPReport.Cells[0, 4] := 'Source';
  FPPReport.Cells[0, 5] := 'Line';

  for i := 0 to Pred(FReader.Count) do
  begin
    FPPReport.Cells[i + 1, 0] := IntToStr(i + 1);
    FPPReport.Cells[i + 1, 1] := FReader[i].position;

    FPPReport.Cells[i + 1, 2] := IntToStr(FReader[i].elapsed);

    FPPReport.Cells[i + 1, 3] := FReader[i].func;
    FPPReport.Cells[i + 1, 4] := FReader[i].Source;
    FPPReport.Cells[i + 1, 5] := IntToStr(FReader[i].line);
    FPPReport.Cells[i + 1, 6] := IntToStr(FReader[i].heapused);
  end;

  FPPReport.WriteTable;


  FPCallGraph := TFPCallGraph.Create;
  try
    Caller := TStringList.Create;
    try
      //writeln('Total Call Count = ', FReader.Count);
      //first entry is mother of all calls so put it on the stack
      Caller.Add(FReader[0].func);

      for i := 0 to FReader.Count - 1 do
      begin
        if FReader[i].position = 'entry' then
        begin
          //writeln('  peeking: ',Caller[0]);
          FPCallGraph.AddCall(Caller[0], FReader[i].func);
          //writeln('  pushing: ',FReader[i].func);
          Caller.Insert(0, FReader[i].func);
        end
        else
        begin
          //writeln('  popping: ',Caller[0]);
          Caller.Delete(0);
        end;
      end;
      //writeln('********   FINISHED   *********');
    finally
      Caller.Free;
    end;

    FPPReport.CallGraph(FPCallGraph);
  finally
    FPCallGraph.Free;
  end;
end;

end.
