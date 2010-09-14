unit LazStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FPPStats, FPPReader, FPPReport, LazReport,
  TASeries;

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
  if FMemSerie=AValue then exit;
  FMemSerie:=AValue;
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

    if i <> 0 then
      FPPReport.Cells[i + 1, 2] := IntToStr(FReader[i].elapsed - FReader[i - 1].elapsed)
    else
      FPPReport.Cells[i + 1, 2] := IntToStr(FReader[i].elapsed);

    FPPReport.Cells[i + 1, 3] := FReader[i].func;
    FPPReport.Cells[i + 1, 4] := FReader[i].Source;
    FPPReport.Cells[i + 1, 5] := IntToStr(FReader[i].line);
    FPPReport.Cells[i + 1, 6] := IntToStr(FReader[i].heapused);
  end;

  FPPReport.WriteTable;
end;

end.

