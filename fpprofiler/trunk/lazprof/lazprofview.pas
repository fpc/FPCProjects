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
  LazStats, FPPReport, Classes, LazProfSettings, LazProfReport, LazIDEIntf,
  SysUtils, ProjectIntf, Process, SrcEditorIntf, LCLProc,
  CodeTree, CodeToolManager, CodeCache, CodeAtom, PascalParserTool,
  LazProfSelectFiles;

type
  TFile = record
    Name: string;
    path: string;
  end;

  { TLazProfileViewer }

  TLazProfileViewer = class(TForm)
    CallGraphImage: TImage;
    CodeBrowseListView: TListView;
    MemImage: TImage;
    MemoryChart: TChart;
    MemoryChartSeries: TAreaSeries;
    ImageList: TImageList;
    ListView1: TListView;
    mnuZoomToFit: TMenuItem;
    mnuZoom100: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    CallGraphPanel: TPanel;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    StatusBar: TStatusBar;
    FlatReportTabSheet: TTabSheet;
    CallGraphTabSheet: TTabSheet;
    MemoryTabSheet: TTabSheet;
    BrowseCodeTabSheet: TTabSheet;
    ToolBar: TToolBar;
    OpenLogButton: TToolButton;
    SettingsButton: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

    CachedUnit: string;
    CachedPos: TPoint;
    CodeBuf: TCodeBuffer;
    CodeTool: TCodeTool;

    procedure ZoomImage(Level: longint);
    procedure ShowCodeInfo(Sender: TObject);

    //code tool helper functions
    function ParseCode(ACodeBuf: TCodeBuffer; out ACodeTool: TCodeTool): boolean;
    function CaretToSourcePosition(ACodeTool: TCodeTool; ACodeBuf: TCodeBuffer; Line, Column: integer; out CleanPos: integer): boolean;
  public
    { public declarations }
  end;

var
  LazProfileViewer: TLazProfileViewer = nil;

procedure Register;

implementation

{$R *.lfm}

uses
  LazProfResourceStings;

procedure IDEMenuClicked(Sender: TObject);
begin
  if not Assigned(LazProfileViewer) then
    LazProfileViewer := TLazProfileViewer.Create(Application);
  LazProfileViewer.Show;
end;

procedure ExecuteApplication(CommandLine: string);
var
  AProcess: TProcess;
begin
  //execute application
  try
    AProcess := TProcess.Create(nil);
    AProcess.CommandLine := CommandLine;
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.ShowWindow := swoShowNormal;
    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;

procedure IDERunWithProfilingClicked(Sender: TObject);
var
  IncludePath: string;
  UnitPath: string;
  CommandLine: string;
  BuildResult: TModalResult;
  OldUnitFiles: string;

  function GetSearchPaths(ADelimitedPaths, APrefix: string): string;
  var
    Paths: TStrings;
    i: integer;
  begin
    try
      Paths := TStringList.Create;
      Paths.Delimiter := ';';
      Paths.StrictDelimiter := True;
      Paths.DelimitedText := ADelimitedPaths;

      //create path string
      Result := '';
      for i := 0 to Paths.Count - 1 do
        if Paths[i] <> '' then
          Result := Result + ' ' + APrefix + '"' + Paths[i] + '"';
    finally
      Paths.Free;
    end;
  end;

begin
  //save project
  LazarusIDE.SaveSourceEditorChangesToCodeCache(-1); // -1: commit all source editors
  LazarusIDE.DoSaveProject([sfProjectSaving]);

  //insert profiling code
  IncludePath := GetSearchPaths(CodeToolBoss.GetIncludePathForDirectory(''), '-Fi');
  UnitPath := GetSearchPaths(CodeToolBoss.GetUnitPathForDirectory(''), '-Fu');

  CommandLine := AppendPathDelim(XMLConfig.GetValue('LazProfOptions/FPP/Path', '')) +
                                 'fppinsert' +
                                 IncludePath +
                                 UnitPath;

  DebugLn('IDERunWithProfilingClicked: ' + CommandLine);
  ExecuteApplication(CommandLine);

  //build project
  OldUnitFiles := LazarusIDE.ActiveProject.LazCompilerOptions.OtherUnitFiles;
  LazarusIDE.ActiveProject.LazCompilerOptions.OtherUnitFiles :=
    LazarusIDE.ActiveProject.LazCompilerOptions.OtherUnitFiles + ';' + XMLConfig.GetValue('LazProfOptions/FPProfUnit/Path', '');
  BuildResult := LazarusIDE.DoBuildProject(crCompile, []);
  LazarusIDE.ActiveProject.LazCompilerOptions.OtherUnitFiles := OldUnitFiles;

  //remove profiling code
  CommandLine := AppendPathDelim(XMLConfig.GetValue('LazProfOptions/FPP/Path', '')) +
                                 'fppremove' +
                                 IncludePath +
                                 UnitPath;
  ExecuteApplication(CommandLine);

  if (BuildResult = mrOk) and (LazarusIDE.ActiveProject.ExecutableType = petProgram) then
    ExecuteApplication(ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename) +
      LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename);
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'mnuLazProfileViewer', rsProfileViewer, nil, @IDEMenuClicked, nil, 'menu_hourglass');
  //RegisterIDEMenuCommand(itmRunBuilding, 'mnuLazProfileBuild', rsBuildWithProfiling, nil, @IDEBuildWithProfilingClicked);
  RegisterIDEMenuCommand(itmRunnning, 'mnuLazProfileRun', rsRunWithProfilingEnabled, nil, @IDERunWithProfilingClicked);
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

procedure TLazProfileViewer.FormHide(Sender: TObject);
begin
  SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorStatus, @ShowCodeInfo);
end;

procedure TLazProfileViewer.FormShow(Sender: TObject);
begin
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorStatus, @ShowCodeInfo);
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
    FileList[FileListCount - 1].Name := AFileName;
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
      if MessageDlg(rsFileNotFound,
        Format(rsTheFileSSwasNotFoundSDoYouWantToLocateItYourselfS,
        [FileName, #13, #13, #13]), mtConfirmation, [mbYes, mbNo], 0) <>
        mrYes then
        Exit;

      try
        OpenUnitDialog := TOpenDialog.Create(nil);
        OpenUnitDialog.Title := Format(rsOpenFile, [FileName]);
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

    LazarusIDE.DoOpenFileAndJumpToPos(FileName, Point(1, Row), -1, -1, -1, [ofOnlyIfExists]);
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

          //load the call graph
          if FileExists(TLazProfReport(ProfStats.Report).PNGFileName) then
          begin
            CallGraphPanel.Caption := '';
            MemImage.Picture.LoadFromFile(TLazProfReport(ProfStats.Report).PNGFileName);
          end
          else
            CallGraphPanel.Caption := rsErrorCouldNotCreateCallGraph;

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

procedure TLazProfileViewer.ShowCodeInfo(Sender: TObject);
var
  li: TListItem;
  AUnit: string;
  AProc: string;
  Pos: TPoint;
  CleanPos: integer;
  ProcNode: TCodeTreeNode;
begin
  AUnit := SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor.FileName;
  Pos := SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor.CursorTextXY;

  //nothing changed
  if (AUnit = CachedUnit) and (Pos.x = CachedPos.x) and (Pos.y = CachedPos.y) then
    exit;

  if AUnit <> CachedUnit then
  begin
    DebugLn('TLazProfileViewer.ShowCodeInfo - selected new unit: ' + AUnit);

    //load the unit file
    CodeBuf := CodeToolBoss.LoadFile(AUnit, False, False);

    if CodeBuf = nil then
      exit;

    // parse the code
    if not ParseCode(CodeBuf, CodeTool) then
      exit;
  end;

  DebugLn(Format('TLazProfileViewer.ShowCodeInfo - caret position = (%d:%d)', [pos.x, pos.y]));

  //find the source position
  if not CaretToSourcePosition(CodeTool, CodeBuf, Pos.y, Pos.x, CleanPos) then
    exit;

  DebugLn(Format('TLazProfileViewer.ShowCodeInfo - CleanPos %d', [CleanPos]));

  // find procedure node
  ProcNode := CodeTool.FindDeepestNodeAtPos(CleanPos, False);
  if ProcNode <> nil then
    ProcNode := ProcNode.GetNodeOfType(ctnProcedure);

  //nothing found so exit
  if ProcNode = nil then
    exit;

  AProc := CodeTool.ExtractProcHead(ProcNode, [phpWithoutSemicolon, phpWithoutParamList, phpWithoutBrackets]);

  DebugLn('TLazProfileViewer.ShowCodeInfo - Method name : ' + AProc);

  //display the unit and procedure in the statusbar
  StatusBar.SimpleText := AUnit + ' - ' + AProc;

  CodeBrowseListView.Clear;

  if not Assigned(ProfStats) then
    exit;

  if not Assigned(ProfStats.Report) then
    exit;

  TLazProfReport(ProfStats.Report).CalcStats(AUnit, AProc);

  li := CodeBrowseListView.Items.Add;
  li.Caption := rsNumberOfPasses;
  li.SubItems.Add(IntToStr(TLazProfReport(ProfStats.Report).Passes));

  li := CodeBrowseListView.Items.Add;
  li.Caption := rsTotalTimeSpent;
  li.SubItems.Add(FloatToStr(TLazProfReport(ProfStats.Report).TimeSpent) + 'ms');

  li := CodeBrowseListView.Items.Add;
  li.Caption := rsAverageTimeSpent;
  if TLazProfReport(ProfStats.Report).AvgTimeSpent <> -1 then
    li.SubItems.Add(FloatToStr(TLazProfReport(ProfStats.Report).AvgTimeSpent) + 'ms')
  else
    li.SubItems.Add(rsNA);

  //cache the unit and procedure names
  CachedUnit := AUnit;
  CachedPos := Pos;
end;

function TLazProfileViewer.ParseCode(ACodeBuf: TCodeBuffer; out ACodeTool: TCodeTool): boolean;
  // commits any editor changes to the codetools, parses the unit
  // and if there is a syntax error, tells the IDE jump to it
begin
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
  if not CodeToolBoss.Explore(ACodeBuf, ACodeTool, False) then
  begin
    LazarusIDE.DoJumpToCodeToolBossError;
    Result := False;
  end
  else
  begin
    Result := True;
  end;
end;

function TLazProfileViewer.CaretToSourcePosition(ACodeTool: TCodeTool; ACodeBuf: TCodeBuffer; Line, Column: integer; out CleanPos: integer): boolean;
  // find the source position in the parsed
  // The parsed source is the source combined of all include files
  // stripped of irrelevant IFDEFs and parsed macros.
var
  CursorPos: TCodeXYPosition;
begin
  CursorPos.X := Column;
  CursorPos.Y := Line;
  CursorPos.Code := ACodeBuf;
  if ACodeTool.CaretToCleanPos(CursorPos, CleanPos) <> 0 then
    Result := False
  else
    Result := True;
end;

initialization
  {$I lazprof_images.lrs}

end.

