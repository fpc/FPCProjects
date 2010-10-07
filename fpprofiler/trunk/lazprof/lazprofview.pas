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
  FileUtil, ExtCtrls, Menus, TAGraph, TASeries, FPPStats, FPPReader,
  LazStats, FPPReport, Classes, LazProfSettings, LazReport, LazIDEIntf,
  SysUtils, ProjectIntf, Process;

type
  TFile = record
    Name: string;
    path: string;
  end;

  { TLazProfileViewer }

  TLazProfileViewer = class(TForm)
    MemImage: TImage;
    CallGraphImage: TImage;
    MemoryChart: TChart;
    MemoryChartSeries: TAreaSeries;
    ImageList: TImageList;
    ListView1: TListView;
    mnuZoomToFit: TMenuItem;
    mnuZoom100: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    FlatReportTabSheet: TTabSheet;
    CallGraphTabSheet: TTabSheet;
    MemoryTabSheet: TTabSheet;
    ToolBar: TToolBar;
    OpenLogButton: TToolButton;
    SettingsButton: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ZoomMenuClick(Sender: TObject);
    procedure OpenLogButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
  private
    { private declarations }
    FPPReader: TFPPReader;
    ProfStats: TCustomProfStats;
    FileList: array of TFile;
    FileListCount: integer;
    procedure ZoomImage(Level: longint);
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
  if LazarusIDE.ActiveProject.ExecutableType = petProgram then
  begin
    //execute application
    try
      AProcess := TProcess.Create(nil);
      AProcess.CommandLine :=
        ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename) +
        LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename;
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
  MemImage := TImage.Create(nil);

  FileListCount := 0;
  SetLength(FileList, 0);
end;

procedure TLazProfileViewer.FormDestroy(Sender: TObject);
begin
  MemImage.Free;
end;

procedure TLazProfileViewer.ListView1Click(Sender: TObject);

  function FileListIndexOf(AFileName: string): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := 0 to FileListCount - 1 do
      if FileList[i].Name = AFileName then
        Result := i;
  end;

  procedure FileListAdd(AFilePath: string; AFileName: string);
  begin
    Inc(FileListCount);
    SetLength(FileList, FileListCount);
    FileList[FileListCount - 1].name := AFileName;
    FileList[FileListCount - 1].path := AFilePath;
  end;

var
  FileName: string;
  Row: integer;
  index: integer;
  OpenUnitDialog: TOpenDialog;
begin
  if Assigned(ListView1.Selected) then
  begin
    {$note this could be even improved, when compiling from the IDE to search the -Fu paths}

    //get the selected filename
    FileName := ListView1.Selected.SubItems[3];

    //check if the filepath is in the filelist
    index := FileListIndexOf(FileName);
    if index <> -1 then
      FileName := FileList[index].path;

    //if file does not exist then ask the user to point out one
    if not FileExists(FileName) then
    begin
      //ask to locate file manually
      if MessageDlg('File not found',
        Format('The file "%s"%swas not found.%sDo you want to locate it yourself ?%s',
        [FileName, #13, #13, #13]), mtConfirmation, [mbYes, mbNo], 0) <>
        mrYes then
        Exit;

      try
        OpenUnitDialog := TOpenDialog.Create(nil);
        OpenUnitDialog.Title := 'Open file ' + FileName;
        OpenUnitDialog.Options := OpenUnitDialog.Options + [ofFileMustExist];
        OpenUnitDialog.FileName := FileName;
        if not OpenUnitDialog.Execute then
          exit;

        //add the new file to the cache list
        FileListAdd(CleanAndExpandFilename(OpenUnitDialog.FileName), FileName);
        FileName := CleanAndExpandFilename(OpenUnitDialog.FileName);
      finally
        OpenUnitDialog.Free;
      end;
    end;

    //get the line number
    Row := StrToInt(ListView1.Selected.SubItems[4]);

    LazarusIDE.DoOpenFileAndJumpToPos(FileName, Point(1, Row), -1,
      -1, -1, [ofOnlyIfExists]);
  end;
end;

procedure TLazProfileViewer.ZoomMenuClick(Sender: TObject);
begin
  ZoomImage(TMenuItem(Sender).Tag);
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
          ProfStats := TLazProfStats.Create(FPPReader, rtPlain);
          //report type is not used

          //flat profile
          TLazProfStats(ProfStats).ListView := ListView1;

          //call graph

          //mem usage
          TLazProfStats(ProfStats).MemSerie := MemoryChartSeries;

          ProfStats.Run;
          MemImage.Picture.LoadFromFile(TLazReport(ProfStats.Report).PNGFileName);
          ZoomImage(100);
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

procedure TLazProfileViewer.ZoomImage(Level: longint);
begin
  CallGraphImage.Align := alNone;
  CallGraphImage.Stretch := False;

  case Level of
    //zoom to fit
    -1:
    begin
      CallGraphImage.Picture := MemImage.Picture;
      //CallGraphImage.Align := alClient;
      CallGraphImage.Stretch := True;
      CallGraphImage.Width := 200;
      CallGraphImage.Height := 200;
    end;
    //100% zoom
    100: CallGraphImage.Picture := MemImage.Picture;
    else
      CallGraphImage.Picture := MemImage.Picture;
  end;
end;

end.

