unit LazProfView;

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, Controls, Graphics, Dialogs, EditBtn, ComCtrls, MenuIntf,
  FileUtil, ExtCtrls, TAGraph, TASeries, FPPStats, FPPReader, LazStats,
  FPPReport, Classes, LazProfSettings, LazReport, LazIDEIntf, SysUtils;

type

  { TLazProfileViewer }

  TLazProfileViewer = class(TForm)
    Image1: TImage;
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
    OpenLogButton: TToolButton;
    SettingsButton: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure OpenLogButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
  private
    { private declarations }
    FPPReader: TFPPReader;
    ProfStats: TCustomProfStats;
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

procedure TLazProfileViewer.ListView1Click(Sender: TObject);
var
  FileName: string;
  Row: integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    FileName := ListView1.Selected.SubItems[3];
    Row := StrToInt(ListView1.Selected.SubItems[4]);
    LazarusIDE.DoOpenFileAndJumpToPos(FileName, Point(1, Row), -1,
      -1, -1, [ofOnlyIfExists]);
  end;
end;

procedure TLazProfileViewer.OpenLogButtonClick(Sender: TObject);
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

          //call graph

          //mem usage
          TLazProfStats(ProfStats).MemSerie := MemoryChartSeries;

          ProfStats.Run;
          Image1.Picture.LoadFromFile(TLazReport(ProfStats.Report).PNGFileName);
        finally
          ProfStats.Free;
        end;
      finally
        FPPReader.Free;
      end;
    end;
  end;
end;

procedure TLazProfileViewer.SettingsButtonClick(Sender: TObject);
begin
  try
    SettingsForm := TSettingsForm.Create(nil);
    SettingsForm.ShowModal;
  finally
    SettingsForm.Free;
  end;
end;

end.

