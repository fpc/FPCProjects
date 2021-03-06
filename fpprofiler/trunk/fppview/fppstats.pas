{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiler statistics class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPPStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPReader, FPPReport, FPCallGraph;

type

  { TCustomProfStats }

  TCustomProfStats = class(TObject)
    FReport: TFPPReportType;
    FReader: TFPPReader;
    procedure SetReportType(const AValue: TFPPReportType);
  private

  protected
    FPPReport: TCustomFPPReport;

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); virtual;
    destructor Destroy; override;
    
    property ReportType: TFPPReportType read FReport write SetReportType;
    property Report: TCustomFPPReport read FPPReport;
    procedure Run; virtual;
  end;

  { TFlatProfStats }

  TFlatProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

  { TCallGraphStats }

  TCallGraphStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

implementation

{ TCustomProfStats }

procedure TCustomProfStats.SetReportType(const AValue: TFPPReportType);
begin
  if (FReport=AValue) and Assigned(FPPReport) then exit;
  FReport:=AValue;
  case FReport of
    rtPlain: FPPReport := TPlainReport.Create;
    rtGraphViz: FPPReport := TGraphVizReport.Create;
  else
    FPPReport := TPlainReport.Create;
  end;
end;

constructor TCustomProfStats.Create(AReader: TFPPReader;
  const AValue: TFPPReportType);
begin
  FReader := AReader;
  ReportType := AValue;
end;

destructor TCustomProfStats.Destroy;
begin
  FPPReport.Free;
  inherited Destroy;
end;

procedure TCustomProfStats.Run;
begin
  if not Assigned(FPPReport) then
    raise Exception.Create('No report object created.');
end;

{ TFlatProfStats }

constructor TFlatProfStats.Create(AReader: TFPPReader; const AValue: TFPPReportType);
begin
  inherited Create(AReader, AValue);
end;

destructor TFlatProfStats.Destroy;
begin
  inherited Destroy;
end;

procedure TFlatProfStats.Run;
var
  i: integer;
begin
  inherited Run;
    
  FPPReport.Clear;
  FPPReport.Cells[0,0] := '#';
  FPPReport.Cells[0,1] := 'Position';
  FPPReport.Cells[0,2] := 'Elapsed msec';
  FPPReport.Cells[0,3] := 'Function';
  FPPReport.Cells[0,4] := 'Source';
  FPPReport.Cells[0,5] := 'Line';
  FPPReport.Cells[0,6] := 'Heap used';

  for i := 0 to Pred(FReader.Count) do
  begin
    FPPReport.Cells[i + 1, 0] :=  IntToStr(i + 1);
    FPPReport.Cells[i + 1, 1] :=  FReader[i].position;
    FPPReport.Cells[i + 1, 2] :=  IntToStr(FReader[i].elapsed);
    FPPReport.Cells[i + 1, 3] :=  FReader[i].func;
    FPPReport.Cells[i + 1, 4] :=  FReader[i].source;
    FPPReport.Cells[i + 1, 5] :=  IntToStr(FReader[i].line);
    FPPReport.Cells[i + 1, 6] :=  IntToStr(FReader[i].heapused);
  end;

  FPPReport.WriteTable;
end;

{ TCallGraphStats }

constructor TCallGraphStats.Create(AReader: TFPPReader; const AValue: TFPPReportType);
begin
  inherited Create(AReader, AValue);
end;

destructor TCallGraphStats.Destroy;
begin
  inherited Destroy;
end;

procedure TCallGraphStats.Run;
var
  FPCallGraph: TFPCallGraph;
  i: integer;
  Caller: TStrings;
begin
  inherited Run;

  FPCallGraph := TFPCallGraph.Create;
  try
    Caller := TStringList.Create;
    try
      //writeln('Total Call Count = ', FReader.Count);
      //first entry is mother of all calls so put it on the stack
      Caller.Add(FReader[0].func);

      for i := 0 to FReader.Count-1 do
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

