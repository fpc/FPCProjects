unit LazProfView;

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, Controls, Graphics, Dialogs, EditBtn, ComCtrls,
  MenuIntf, FileUtil, TAGraph, TASeries, FPPStats, FPPReader, LazStats,
  FPPReport;

type

  { TLazProfileViewer }

  TLazProfileViewer = class(TForm)
    MemoryChart: TChart;
    MemoryChartSeries: TAreaSeries;
    ImageList: TImageList;
    ListView1: TListView;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    StatusBar1: TStatusBar;
    FlatReportTabSheet: TTabSheet;
    CallGraphTabSheet: TTabSheet;
    MemoryTabSheet: TTabSheet;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    { private declarations }
    FPPReader: TFPPReader;
    ProfStats: TCustomProfStats;
    ReportType: TFPPReportType;
  public
    { public declarations }
  end;

var
  LazProfileViewer: TLazProfileViewer = nil;

procedure Register;

implementation

  {$R *.lfm}

procedure ShowLazProfileViewer;
begin
  if not Assigned(LazProfileViewer) then
    LazProfileViewer := TLazProfileViewer.Create(Application);
  LazProfileViewer.Show;
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  ShowLazProfileViewer;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'mnuLazProfileViewer',
    'Profile viewer', nil, @IDEMenuClicked);
end;

{ TLazProfileViewer }

procedure TLazProfileViewer.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := FlatReportTabSheet;
end;

procedure TLazProfileViewer.ToolButton1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if FileExistsUTF8(OpenDialog.FileName) then
    begin
      try
        FPPReader := TFPPReader.Create(OpenDialog.FileName);
        try
          ProfStats := TLazProfStats.Create(FPPReader, rtPlain); //report type is not used

          //flat profile
          TLazProfStats(ProfStats).ListView := ListView1;
          TLazProfStats(ProfStats).MemSerie := MemoryChartSeries;
          ProfStats.Run;

          //call graph

          //mem usage
        finally
          ProfStats.Free;
        end;
      finally
        FPPReader.Free;
      end;
    end;
  end;
end;

end.

