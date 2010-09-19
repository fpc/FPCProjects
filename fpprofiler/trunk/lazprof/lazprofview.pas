{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Lazarus profile viewer

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfView;

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, Controls, Graphics, Dialogs, EditBtn, ComCtrls, MenuIntf,
  FileUtil, ExtCtrls, TAGraph, TASeries, FPPStats, FPPReader, LazStats,
  FPPReport, Classes, LazProfSettings, LazReport, LazIDEIntf, SysUtils,
  ProjectIntf, Process, IDEExternToolIntf;

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

procedure IDEMenuClicked(Sender: TObject);
begin
  if not Assigned(LazProfileViewer) then
    LazProfileViewer := TLazProfileViewer.Create(Application);
  LazProfileViewer.Show;
end;

procedure IDERunWithProfilingClicked(Sender: TObject);
var
  AProcess: TProcess;
begin

  LazarusIDE.DoBuildProject(crCompile, []);
  if LazarusIDE.ActiveProject.ExecutableType=petProgram then
  begin
    //execute application
    try
      AProcess := TProcess.Create(nil);
      AProcess.CommandLine := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename) + LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename;
      AProcess.Options := AProcess.Options + [poWaitOnExit];
      AProcess.ShowWindow := swoShowNormal;
      AProcess.Execute;
    finally
      AProcess.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'mnuLazProfileViewer', 'Profile viewer', nil, @IDEMenuClicked);
  //RegisterIDEMenuCommand(itmRunBuilding, 'mnuLazProfileBuild', 'Build with profiling', nil, @IDEBuildWithProfilingClicked);
  RegisterIDEMenuCommand(itmRunnning, 'mnuLazProfileRun', 'Run with profiling enabled', nil, @IDERunWithProfilingClicked);
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
    {$note in order to get this working you need the full path, if we compile from the IDE we will be able to search the -Fu paths}
    FileName := ListView1.Selected.SubItems[3];
    Row := StrToInt(ListView1.Selected.SubItems[4]);
    LazarusIDE.DoOpenFileAndJumpToPos(FileName, Point(1, Row), -1, -1, -1, [ofOnlyIfExists]);
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

