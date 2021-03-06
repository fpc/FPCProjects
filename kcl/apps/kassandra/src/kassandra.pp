{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999 - 2000  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}
{$H+}

program Kassandra;
uses
  DOS, SysUtils, Classes,			// system units
  XMLCfg, GetText, Process, AsyncIO,		// FPC helper units / FCL
  SHEdit, doc_text, sh_pas, sh_xml,		// SHEdit & friends
  KCL, KCLSHEdit,				// KCL units
  ViewMan, Vw_SHText;				// Kassandra units

resourcestring
  menuFile = '&File';
  menuNewFile = '&New file';
  menuOpenFile = '&Open file...';
  menuSaveFile = '&Save file';
  menuSaveFileAs = 'Save file &as...';
  menuCloseFile = '&Close file';
  menuOpenWorkspace = 'Open &workspace...';
  menuSaveWorkspace = 'Save workspace';
  menuSaveWorkspaceAs = 'Save workspace as...';
  menuOpenRecentFile = 'Open recent file';
  menuOpenRecentWorkspace = 'Open recent workspace';
  menuExit = 'E&xit';
  menuEdit = '&Edit';
  menuEditCut = '&Cut';
  menuEditCopy = 'C&opy';
  menuEditPaste = '&Paste';
  menuCompiler = '&Compiler';
  menuCompilerCompile = '&Compile';
  menuView = '&View';
  menuViewOutput = '&Output window';
  menuOptions = '&Options';
  menuGeneralOptions = '&General options...';
  menuHelp = '&Help';

  dlgGlobalOptions = 'Global Options';

  strReady = 'Ready';

const

  RECENTFILECOUNT = 8;

type

  TMainForm = class(TForm)
    Layout: TDockingLayout;
    Views: TViewManager;
    StatusBar: TStatusBar;
    OutputSplitter: TSplitter;
    CompilerOutputWnd: TKCLSHWidget;
    ImageList: TImageList;

    // Commands
    Commands: TCommandList;
    FileMenuCmd, FileNewCmd, FileOpenCmd, FileSaveCmd, FileSaveAsCmd,
      FileCloseCmd, FileWorkspaceOpenCmd, FileWorkspaceSaveCmd,
      FileWorkspaceSaveAsCmd, FileRecentFilesMenuCmd,
      FileRecentWorkspacesMenuCmd, FileExitCmd,
      EditMenuCmd, EditCutCmd, EditCopyCmd, EditPasteCmd,
      ViewMenuCmd, ViewOutputCmd, ViewOptionsCmd,
      CompilerMenuCmd, CompilerCompileCmd,
      HelpMenuCmd, HelpAboutCmd: TCommand;

    // Menu bar
    MenuBar: TMenuBar;
    RecentFilesMenu, RecentWorkspacesMenu: TMenu;

    // Tool bar
    Toolbar: TToolBar;
    SearchEdit: TEdit;

  protected
    Config: TXMLConfig;
    NonameCounter, ExternalToolsRunning: Integer;
    RecentFiles: array[0..RECENTFILECOUNT-1] of String;
    RecentFileItems: array[0..RECENTFILECOUNT-1] of TMenuItem;
    AsyncIOManager: TAsyncIOManager;
    CompilerProcess: TProcess;
    CompilerOutput: TAsyncStreamLineReader;

    procedure CreateMenuBar;
    procedure ViewsChanged;
    procedure IdleHandler(Sender: TObject);
    procedure AsyncTimeout(Sender: TObject);
    procedure CompilerOutputLineAvailable(const line: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SaveConfig;
    procedure StartExternalToolUsage;
    procedure StopExternalToolUsage;

    procedure OpenFileByName(AFileName: String);

    procedure OnFileNewCmd(Sender: TObject);
    procedure OnFileOpenCmd(Sender: TObject);
    procedure OnFileSaveCmd(Sender: TObject);
    procedure OnFileSaveAsCmd(Sender: TObject);
    procedure OnFileCloseCmd(Sender: TObject);
    procedure OnFileExitCmd(Sender: TObject);
    procedure OnViewToggleOutputWindowCmd(Sender: TObject);
    procedure OnViewOptionsCmd(Sender: TObject);
    procedure CompilerCompileExecute(Sender: TObject);
  end;


constructor TMainForm.Create(AOwner: TComponent);

  function AddToolButton(command: TCommand): TToolButton;
  begin
    Result := ToolBar.AddButton(Self);
    Result.Command := command;
  end;

var
  i: Integer;
  sep: TToolButton;
  bmp: TBitmap;
begin
  inherited Create(AOwner);

  // Read configuration file
  Config := TXMLConfig.Create(GetEnv('HOME') + '/.kassandra');  // *** not platform independent (sg)
  for i := 1 to RECENTFILECOUNT do
    RecentFiles[i - 1] :=
      Config.GetValue('MainWindow/RecentFiles/File'+ IntToStr(i), '');

  // Init main window
  SetPosition(
    Config.GetValue('MainWindow/PosAndSize/X', -1),
    Config.GetValue('MainWindow/PosAndSize/Y', -1));
  SetDefaultSize(
    Config.GetValue('MainWindow/PosAndSize/Width', 600),
    Config.GetValue('MainWindow/PosAndSize/Height', 400));

  Text := Application.Title + '  (Version: ' + {$I %date%} + ' ' + {$I %time%} +
    ' ' + {$I %user%} + ')';

  // Create basic layout object
  Layout := TDockingLayout.Create(Self);
  Layout.Name := 'Layout';
  Content := Layout;

  // Create notebook with file views
  Views := TViewManager.Create(Self);
  Views.Name := 'Views';

  // Create compiler output window
  CompilerOutputWnd := TKCLSHWidget.Create(Self);
  CompilerOutputWnd.Name := 'CompilerOutputWnd';
  CompilerOutputWnd.SetupEditor(TTextDoc.Create, TSHTextEdit);
  CompilerOutputWnd.Document.AddLine('');

  // Create splitter which contains views and compiler output
  OutputSplitter := TSplitter.Create(Self);
  OutputSplitter.Name := 'OutputSplitter';
  OutputSplitter.ResizePolicy := srFixedPane2;
  OutputSplitter.Pane1 := Views;
  OutputSplitter.Pane2 := CompilerOutputWnd;
  Layout.AddWidget(OutputSplitter, dmClient);

  // Create image list used for the command list
  ImageList := TImageList.Create(Self);
  ImageList.Name := 'ImageList';
  bmp := TBitmap.Create;
  bmp.LoadFromFile('toolbar.bmp');
  ImageList.AddMasked(bmp, colMagenta);
  bmp.Free;

  // Create command list
  Commands := TCommandList.Create(Self);
  Commands.Images := ImageList;
  FileMenuCmd := Commands.Add(Self, 'FileMenuCmd', menuFile, '', -1, nil);
  FileNewCmd := Commands.Add(Self, 'FileNewCmd', menuNewFile, '<control>N', 0, @OnFileNewCmd);
  FileOpenCmd := Commands.Add(Self, 'FileOpenCmd', menuOpenFile, '<control>O', 1, @OnFileOpenCmd);
  FileSaveCmd := Commands.Add(Self, 'FileSaveCmd', menuSaveFile, '<control>S', 2, @OnFileSaveCmd);
  FileSaveAsCmd := Commands.Add(Self, 'FileSaveAsCmd', menuSaveFileAs, '<control><shift>S', -1, @OnFileSaveAsCmd);
  FileCloseCmd := Commands.Add(Self, 'FileCloseCmd', menuCloseFile, '<control>F4', -1, @OnFileCloseCmd);
  FileWorkspaceOpenCmd := Commands.Add(Self, 'FileWorkspaceOpenCmd', menuOpenWorkspace, '', -1, nil);
  FileWorkspaceSaveCmd := Commands.Add(Self, 'FileWorkspaceSaveCmd', menuSaveWorkspace, '', -1, nil);
  FileWorkspaceSaveAsCmd := Commands.Add(Self, 'FileWorkspaceSaveAsCmd', menuSaveWorkspaceAs, '', -1, nil);
  FileRecentFilesMenuCmd := Commands.Add(Self, 'FileRecentFilesMenuCmd', menuOpenRecentFile, '', -1, nil);
  FileRecentWorkspacesMenuCmd := Commands.Add(Self, 'FileRecentWorkspacesMenuCmd', menuOpenRecentWorkspace, '', -1, nil);
  FileExitCmd := Commands.Add(Self, 'FileExitCmd', menuExit, '<alt>X', -1, @OnFileExitCmd);
  EditMenuCmd := Commands.Add(Self, 'EditMenuCmd', menuEdit, '', -1, nil);
  EditCutCmd := Commands.Add(Self, 'EditCutCmd', menuEditCut, '<control>X', 3, nil);
  EditCopyCmd := Commands.Add(Self, 'EditCopyCmd', menuEditCopy, '<control>C', 4, nil);
  EditPasteCmd := Commands.Add(Self, 'EditPasteCmd', menuEditPaste, '<control>V', 5, nil);
  ViewMenuCmd := Commands.Add(Self, 'ViewMenuCmd', menuView, '', -1, nil);
  ViewOutputCmd := Commands.Add(Self, 'ViewOutputCmd', menuViewOutput, '', -1, nil{@OnViewToggleOutputWindowCmd}); ViewOutputCmd.Checked := True;
  ViewOptionsCmd := Commands.Add(Self, 'ViewOptionsCmd', menuOptions, '', -1, @OnViewOptionsCmd);
  CompilerMenuCmd := Commands.Add(Self, 'CompilerMenuCmd', menuCompiler, '', -1, nil);
  CompilerCompileCmd := Commands.Add(Self, 'CompilerCompileCmd', menuCompilerCompile, '', -1, @CompilerCompileExecute);
  HelpMenuCmd := Commands.Add(Self, 'HelpMenuCmd', menuHelp, '', -1, nil);
  HelpAboutCmd := Commands.Add(Self, 'HelpAboutCmd', '<HelpAbout>', 'F1', 7, nil);

  // Create menu bar
  CreateMenuBar;

  // Create tool bar
  ToolBar := TToolBar.Create(Self);
  ToolBar.Name := 'ToolBar';
  Layout.AddWidget(ToolBar, dmTop);

  AddToolButton(FileNewCmd);
  AddToolButton(FileOpenCmd);
  AddToolButton(FileSaveCmd);
  sep := ToolBar.AddButton(Self); sep.Style := tbsSeparator;
  AddToolButton(EditCutCmd);
  AddToolButton(EditCopyCmd);
  AddToolButton(EditPasteCmd);
  sep := ToolBar.AddButton(Self); sep.Style := tbsSeparator;
  SearchEdit := TEdit.Create(Self);
  SearchEdit.Name := 'SearchEdit';
  SearchEdit.Text := 'QuickSearch...';
  SearchEdit.DefaultWidth := 100;
  ToolBar.AddChildWidget(SearchEdit);
  sep := ToolBar.AddButton(Self); sep.Style := tbsSeparator;
  AddToolButton(HelpAboutCmd);

  // Create status bar
  StatusBar := TStatusBar.Create(Self);
  StatusBar.Name := 'StatusBar';
  StatusBar.Text := strReady;
  Layout.AddWidget(StatusBar, dmBottom);

  ViewsChanged;
end;

destructor TMainForm.Destroy;
begin
  CompilerProcess.Free;
  CompilerOutput.Free;
  AsyncIOManager.Free;
  SaveConfig;
  Config.Free;
  inherited Destroy;
end;

procedure TMainForm.CreateMenuBar;

  function AddItem(menu: TMenu; command: TCommand): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Command := command;
    menu.AddItem(Result);
  end;

var
  menu: TMenu;
  item: TMenuItem;
begin
  MenuBar := TMenuBar.Create(Self);
  MenuBar.Name := 'MenuBar';
  Layout.AddWidget(MenuBar, dmTop);

  // Add file menu
  menu := TMenu.Create(Self);
  menu.Command := FileMenuCmd;
  MenuBar.AddItem(menu);
  AddItem(menu, FileNewCmd);
  AddItem(menu, FileOpenCmd);
  AddItem(menu, FileSaveCmd);
  AddItem(menu, FileSaveAsCmd);
  AddItem(menu, FileCloseCmd);
  item := TMenuItem.Create(Self); item.Style := misSeparator; menu.AddItem(item);
  AddItem(menu, FileWorkspaceOpenCmd);
  AddItem(menu, FileWorkspaceSaveCmd);
  AddItem(menu, FileWorkspaceSaveAsCmd);
  item := TMenuItem.Create(Self); item.Style := misSeparator; menu.AddItem(item);
  RecentFilesMenu := TMenu.Create(Self);
  RecentFilesMenu.Name := 'RecentFilesMenu';
  RecentFilesMenu.Command := FileRecentFilesMenuCmd;
  menu.AddItem(RecentFilesMenu);
  RecentWorkspacesMenu := TMenu.Create(Self);
  RecentWorkspacesMenu.Name := 'RecentWorkspacesMenu';
  RecentWorkspacesMenu.Command := FileRecentWorkspacesMenuCmd;
  menu.AddItem(RecentWorkspacesMenu);
  item := TMenuItem.Create(Self); item.Style := misSeparator; menu.AddItem(item);
  AddItem(menu, FileExitCmd);

  menu := TMenu.Create(Self);
  menu.Command := EditMenuCmd;
  MenuBar.AddItem(menu);
  AddItem(menu, EditCutCmd);
  AddItem(menu, EditCopyCmd);
  AddItem(menu, EditPasteCmd);

  menu := TMenu.Create(Self);
  menu.Command := ViewMenuCmd;
  MenuBar.AddItem(menu);
  item := AddItem(menu, ViewOutputCmd); item.Style := misCheck;
  AddItem(menu, ViewOptionsCmd);

  menu := TMenu.Create(Self);
  menu.Command := CompilerMenuCmd;
  MenuBar.AddItem(menu);
  AddItem(menu, CompilerCompileCmd);

  menu := TMenu.Create(Self);
  menu.Command := HelpMenuCmd;
  MenuBar.AddItem(menu);
end;

procedure TMainForm.ViewsChanged;
var
  HasViews: Boolean;
begin
  HasViews := Views.Count > 0;
  FileSaveCmd.Enabled := HasViews;
  FileSaveAsCmd.Enabled := HasViews;
  FileCloseCmd.Enabled := HasViews;
  EditCutCmd.Enabled := HasViews;
  EditCopyCmd.Enabled := HasViews;
  EditPasteCmd.Enabled := HasViews;
  SearchEdit.Enabled := HasViews;
end;

procedure TMainForm.SaveConfig;
begin
  Config.SetValue('MainWindow/PosAndSize/X', PositionX);
  Config.SetValue('MainWindow/PosAndSize/Y', PositionY);
  Config.SetValue('MainWindow/PosAndSize/Width', Width);
  Config.SetValue('MainWindow/PosAndSize/Height', Height);
  Config.Flush;
end;

procedure TMainForm.StartExternalToolUsage;
begin
  if ExternalToolsRunning = 0 then begin
    AsyncIOManager := TAsyncIOManager.Create;
    AsyncIOManager.SetTimeoutHandler(@AsyncTimeout, nil);
    AsyncIOManager.Timeout := 10;
    Application.OnIdle := @IdleHandler;
  end;
  Inc(ExternalToolsRunning);
end;

procedure TMainForm.StopExternalToolUsage;
begin
  Dec(ExternalToolsRunning);
  if ExternalToolsRunning = 0 then begin
    Application.OnIdle := nil;
    AsyncIOManager.Free;
    AsyncIOManager := nil;
  end;
end;

procedure TMainForm.IdleHandler(Sender: TObject);
begin
  AsyncIOManager.Run;
end;

procedure TMainForm.AsyncTimeout(Sender: TObject);
begin
  AsyncIOManager.BreakRun;
end;

procedure TMainForm.OpenFileByName(AFileName: String);
var
  doc: TTextDoc;
  view: TGenericView;
  ext: String;
begin
  doc := TTextDoc.Create;
  doc.LoadFromFile(AFileName);
  // Get file extension and determine which editor to use
  ext := ExtractFileExt(AFileName);
  ext := Copy(ext, 2, Length(ext));
  if (ext <> '') and (Pos(UpCase(ext) + ';',
    Config.GetValue('SyntaxHighlighter/Pascal/ext', 'PP;PAS;INC') + ';') > 0) then
    view := TSHPasView.Create(Views, doc)
  else if (ext <> '') and (Pos(UpCase(ext) + ';',
    Config.GetValue('SyntaxHighlighter/XML/ext', 'XML;DTD;HTML;HTM') + ';') > 0) then
    view := TSHXMLView.Create(Views, doc)
  else
    view := TSHTextView.Create(Views, doc);

  view.FileName := AFileName;
  Views.PageIndex := Views.AddView(view);
  ViewsChanged;
end;

procedure TMainForm.OnFileNewCmd(Sender: TObject);
var
  doc: TTextDoc;
  view: TSHTextView;
begin
  doc := TTextDoc.Create;
  doc.AddLine('');
  view := TSHPasView.Create(Views, doc);
  view.FileName := 'noname' + IntToStr(NonameCounter) + '.pp';
  view.HasDefaultName := True;
  Inc(NonameCounter);
  Views.PageIndex := Views.AddView(view);
  ViewsChanged;
end;

procedure TMainForm.OnFileOpenCmd(Sender: TObject);
var
  FileDlg: TFileDialog;
begin
  FileDlg := TFileDialog.Create(nil);
  if FileDlg.Run then
    OpenFileByName(FileDlg.Filename);
  FileDlg.Free;
end;

procedure TMainForm.OnFileSaveCmd(Sender: TObject);
var
  View: TGenericView;
begin
  if Views.PageIndex < 0 then exit;
  View := Views.GetView(Views.PageIndex);
  if View.HasDefaultName then
    OnFileSaveAsCmd(Sender)
  else
    View.Save;
end;

procedure TMainForm.OnFileSaveAsCmd(Sender: TObject);
var
  View: TGenericView;
  FileDlg: TFileDialog;
begin
  if Views.PageIndex < 0 then exit;
  View := Views.GetView(Views.PageIndex);

  FileDlg := TFileDialog.Create(nil);
  FileDlg.FileName := View.FileName;
  if FileDlg.Run then begin
    View.Filename := FileDlg.FileName;
    View.Save;
    View.HasDefaultName := False;
  end;
end;

procedure TMainForm.OnFileCloseCmd(Sender: TObject);
var
  index: Integer;
begin
  index := Views.PageIndex;
  if index < 0 then exit;
  Views.CloseView(index);
  ViewsChanged;
end;

procedure TMainForm.OnFileExitCmd(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.OnViewToggleOutputWindowCmd(Sender: TObject);
begin
  if Assigned(OutputSplitter.Pane2) then
    OutputSplitter.Pane2 := nil
  else
    OutputSplitter.Pane2 := CompilerOutputWnd;
end;

procedure TMainForm.OnViewOptionsCmd(Sender: TObject);
var
  dlg: TStdBtnDialog;
  l: TLabel;
begin
  dlg := TStdBtnDialog.Create(Self);
  dlg.Buttons := [btnOK, btnCancel];
  dlg.Text := dlgGlobalOptions;

  l := TLabel.Create(dlg);
  l.Text := 'This will be the configuration dialog...';

  dlg.Content := l;
  dlg.Run;
  dlg.Free;
end;

procedure TMainForm.CompilerCompileExecute(Sender: TObject);
begin
  with CompilerOutputWnd do begin
    Document.Clear;
    Document.AddLine('');
    Editor.CursorX := 0;
    Editor.CursorY := 1;
  end;
  StartExternalToolUsage;
  CompilerProcess := TProcess.Create('/usr/bin/make clean all', [poUsePipes, poStderrToOutPut]);
  CompilerProcess.Execute;
  CompilerOutput := TAsyncStreamLineReader.Create(AsyncIOManager, CompilerProcess.Output);
  CompilerOutput.OnLine := @CompilerOutputLineAvailable;
end;

procedure TMainForm.CompilerOutputLineAvailable(const line: String);
var
  TrackCursor: Boolean;
  LineCount: Integer;
begin
  LineCount := CompilerOutputWnd.Document.LineCount;
  if (CompilerOutputWnd.Editor.CursorX = 0) and ((CompilerOutputWnd.Editor.CursorY = LineCount) or
     (LineCount = 0)) then
    TrackCursor := True;

  CompilerOutputWnd.Document.InsertLine(LineCount - 1, line);

  if TrackCursor then begin
    CompilerOutputWnd.Editor.CursorX := 0;
    CompilerOutputWnd.Editor.CursorY := LineCount;
  end;
end;


var
  MainForm: TMainForm;
begin
  gettext.TranslateResourceStrings('intl/kassandra.%s.mo');

  Application.Initialize;
  Application.Title := 'Kassandra IDE';

  MainForm := TMainForm.Create(Application);
  MainForm.Name := 'MainForm';
  Application.AddForm(MainForm);
  Application.Run;
end.


{
  $Log$
  Revision 1.9  2000/02/22 14:33:02  sg
  * Unit name change: Async_IO -> AsyncIO (in FCL)

  Revision 1.8  2000/02/20 11:01:27  sg
  * The syntax highlighter for files created with "File - New" is now
    initialized correctly

  Revision 1.7  2000/02/19 19:08:07  sg
  * Adapted to changes in KCL

  Revision 1.6  2000/02/17 22:42:09  sg
  * Re-enabled the dummy configuration dialog
  * Re-enabled gettext (memory leaks have been fixed)
  * Added an idle handler & the possibility to do a "make clean all" in
    the current directory, the output will be displayed in the window
    docked at the bottom of the main window...

  Revision 1.5  2000/02/10 18:23:44  sg
  * The position of the main window is now stored
  * Gettext disabled due to some memory leak problems in the RTL
  * Lots of small corrections and adaptions to changed KCL interface

  Revision 1.4  2000/01/24 00:33:10  sg
  * All possible menu commands are now TCommand's; the menu bar creation has
    been adapted accordingly
  * Added a nice toolbar ;)

  Revision 1.3  2000/01/06 23:05:07  sg
  * Menu items now use "&" to mark the accelerator key

  Revision 1.2  2000/01/05 19:28:14  sg
  * Lots of bug fixes and adaptions to changes in other units
  * almost all widgets now have a correct component name and owner

  Revision 1.1.1.1  1999/12/30 21:32:52  sg
  Initial import

}
