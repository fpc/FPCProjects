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
  DOS, SysUtils,				// system units
  XMLCfg, GetText,				// FPC helper units
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
  menuExit = 'E&xit';
  menuView = '&View';
  menuViewOutput = '&Output window';
  menuOptions = '&Options';
  menuGeneralOptions = '&General options...';

  dlgGlobalOptions = 'Global Options';

  strReady = 'Ready';
  strCompilerOutput = 'Compiler output will go here';

const

  RECENTFILECOUNT = 8;

type

  TMainForm = class(TForm)
  protected
    Layout: TDockingLayout;
    Views: TViewManager;
    StatusBar: TStatusBar;
    OutputSplitter: TSplitter;
    CompilerOutput: TKCLSHWidget;
    Config: TXMLConfig;
    NonameCounter: Integer;
    RecentFiles: array[0..RECENTFILECOUNT-1] of String;

    // Menu bar
    MenuBar: TMenuBar;
    RecentFileMenu: TMenu;
    SaveFileItem, SaveFileAsItem, CloseFileItem,
      OpenWorkspaceItem, SaveWorkspaceItem, SaveWorkspaceAsItem: TMenuItem;
    RecentFileItems: array[0..RECENTFILECOUNT-1] of TMenuItem;

    procedure CreateMenuBar;
    procedure UpdateRecentFiles;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure SaveConfig;
    procedure OpenFileByName(AFileName: String);

    procedure FileMenu(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FileClose(Sender: TObject);
    procedure FileQuit(Sender: TObject);
    procedure ViewToggleOutputWindow(Sender: TObject);
    procedure Options(Sender: TObject);
  end;


constructor TMainForm.Create;
var
  i: Integer;
begin
  inherited Create(nil);

  // Read configuration file
  Config := TXMLConfig.Create(GetEnv('HOME') + '/.kassandra');  // *** not platform independent (sg)
  for i := 1 to RECENTFILECOUNT do
    RecentFiles[i - 1] :=
      Config.GetValue('MainWindow/RecentFiles/File'+ IntToStr(i), '');

  // Init main window and create widgets

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
  CompilerOutput := TKCLSHWidget.Create(Self);
  CompilerOutput.Name := 'CompilerOutput';
  CompilerOutput.SetupEditor(TTextDoc.Create, TSHTextEdit);
  CompilerOutput.Document.AddLine(strCompilerOutput);

  // Create splitter which contains views and compiler output
  OutputSplitter := TSplitter.Create(Self);
  OutputSplitter.Name := 'OutputSplitter';
  OutputSplitter.ResizePolicy := srFixedPane2;
  OutputSplitter.Pane1 := Views;
  OutputSplitter.Pane2 := CompilerOutput;
  Layout.AddWidget(OutputSplitter, dmClient);

  // Create menu bar
  CreateMenuBar;

  // Create status bar
  StatusBar := TStatusBar.Create(Self);
  StatusBar.Name := 'StatusBar';
  StatusBar.Text := strReady;
  Layout.AddWidget(StatusBar, dmBottom);
end;

destructor TMainForm.Destroy;
begin
  SaveConfig;
  inherited Destroy;
end;

procedure TMainForm.CreateMenuBar;
var
  menu: TMenu;
  item: TMenuItem;
begin
  MenuBar := TMenuBar.Create(Self);
  MenuBar.Name := 'MenuBar';
  Layout.AddWidget(MenuBar, dmTop);

  // Add file menu
  menu := TMenu.Create(Self);
  menu.Name := 'FileMenu';	// ###
  menu.Text := menuFile;
  menu.OnClick := @FileMenu;
  MenuBar.AddItem(menu);
  item := TMenuItem.Create(Self);
    item.Name := 'NewFileItem';	// ###
    item.Text := menuNewFile;
    item.OnClick := @FileNew;
    menu.AddItem(item);
  item := TMenuItem.Create(Self);
    item.Name := 'OpenFileItem';	// ###
    item.Text := menuOpenFile;
    item.OnClick := @FileOpen;
    menu.AddItem(item);
  SaveFileItem := TMenuItem.Create(Self);
    SaveFileItem.Name := 'SaveFileItem';
    SaveFileItem.Text := menuSaveFile;
    SaveFileItem.OnClick := @FileSave;
    menu.AddItem(SaveFileItem);
  SaveFileAsItem := TMenuItem.Create(Self);
    SaveFileAsItem.Name := 'SaveFileAsItem';
    SaveFileAsItem.Text := menuSaveFileAs;
    SaveFileAsItem.OnClick := @FileSaveAs;
    menu.AddItem(SaveFileAsItem);
  CloseFileItem := TMenuItem.Create(Self);
    CloseFileItem.Name := 'CloseFileItem';
    CloseFileItem.Text := menuCloseFile;
    CloseFileItem.OnClick := @FileClose;
    menu.AddItem(CloseFileItem);
  item := TMenuItem.Create(Self);
    item.Name := 'FileSepItem1';	// ###
    item.Text := '-';
    menu.AddItem(item);
  OpenWorkspaceItem := TMenuItem.Create(Self);
    OpenWorkspaceItem.Name := 'OpenWorkspaceItem';
    OpenWorkspaceItem.Text := menuOpenWorkspace;
    OpenWorkspaceItem.Gray := True;
    menu.AddItem(OpenWorkspaceItem);
  SaveWorkspaceItem := TMenuItem.Create(Self);
    SaveWorkspaceItem.Name := 'SaveWorkspaceItem';
    SaveWorkspaceItem.Text := menuSaveWorkspace;
    SaveWorkspaceItem.Gray := True;
    menu.AddItem(SaveWorkspaceItem);
  SaveWorkspaceAsItem := TMenuItem.Create(Self);
    SaveWorkspaceAsItem.Name := 'SaveWorkspaceAsItem';
    SaveWorkspaceAsItem.Text := menuSaveWorkspaceAs;
    SaveWorkspaceAsItem.Gray := True;
    menu.AddItem(SaveWorkspaceAsItem);
  item := TMenuItem.Create(Self);
    item.Name := 'FileSepItem2';	// ###
    item.Text := '-';
    menu.AddItem(item);
  RecentFileMenu := TMenu.Create(Self);
    RecentFileMenu.Name := 'RecentFileMenu';
    RecentFileMenu.Text := menuOpenRecentFile;
    RecentFileMenu.Gray := True;
    menu.AddItem(RecentFileMenu);
  item := TMenuItem.Create(Self);
    item.Name := 'FileSepItem3';	// ###
    item.Text := '-';
    menu.AddItem(item);
  item := TMenuItem.Create(Self);
    item.Name := 'ExitItem';	// ###
    item.Text := menuExit;
    item.OnClick := @FileQuit;
    menu.AddItem(item);

  menu := TMenu.Create(Self);
  menu.Name := 'ViewMenu';	// ###
  menu.Text := menuView;
  MenuBar.AddItem(menu);
  item := TMenuItem.Create(Self);
    item.Name := 'ViewOutputItem';	// ###
    item.Text := menuViewOutput;
    item.OnClick := @ViewToggleOutputWindow;
    menu.AddItem(item);

{ Deactivated until all dialog and widget classes are working!
  menu := TMenu.Create(Self);
  menu.Text := menuOptions;
  MenuBar.AddItem(menu);
  item := TMenuItem.Create(Self);
    item.Text := menuGeneralOptions;
    item.OnClick := @Options;
    menu.AddItem(item);
}

  UpdateRecentFiles;
end;

procedure TMainForm.UpdateRecentFiles;
begin
end;

procedure TMainForm.SaveConfig;
begin
  Config.SetValue('MainWindow/PosAndSize/Width', Width);
  Config.SetValue('MainWindow/PosAndSize/Height', Height);
  Config.Flush;
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
  Views.CurPageIndex := Views.AddView(view);
end;

procedure TMainForm.FileMenu(Sender: TObject);
var
  NoViews: Boolean;
begin
  NoViews := Views.Count = 0;
  SaveFileItem.Gray := NoViews;
  SaveFileAsItem.Gray := NoViews;
  CloseFileItem.Gray := NoViews;
end;

procedure TMainForm.FileNew(Sender: TObject);
var
  view: TSHTextView;
begin
  view := TSHTextView.Create(Views, TTextDoc.Create, TSHPasEdit);
  view.FileName := 'noname' + IntToStr(NonameCounter) + '.pp';
  view.HasDefaultName := True;
  Inc(NonameCounter);
  Views.CurPageIndex := Views.AddView(view);
end;

procedure TMainForm.FileOpen(Sender: TObject);
var
  FileDlg: TFileDialog;
begin
  FileDlg := TFileDialog.Create(nil);
  if FileDlg.Run then
    OpenFileByName(FileDlg.Filename);
  FileDlg.Free;
end;

procedure TMainForm.FileSave(Sender: TObject);
var
  View: TGenericView;
begin
  if Views.CurPageIndex < 0 then exit;
  View := Views.GetView(Views.CurPageIndex);
  if View.HasDefaultName then
    FileSaveAs(Sender)
  else
    View.Save;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
var
  View: TGenericView;
  FileDlg: TFileDialog;
begin
  if Views.CurPageIndex < 0 then exit;
  View := Views.GetView(Views.CurPageIndex);

  FileDlg := TFileDialog.Create(nil);
  FileDlg.FileName := View.FileName;
  if FileDlg.Run then begin
    View.Filename := FileDlg.FileName;
    View.Save;
    View.HasDefaultName := False;
  end;
end;

procedure TMainForm.FileClose(Sender: TObject);
var
  index: Integer;
begin
  index := Views.CurPageIndex;
  if index < 0 then exit;
  Views.CloseView(index);
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.ViewToggleOutputWindow(Sender: TObject);
begin
  if Assigned(OutputSplitter.Pane2) then
    OutputSplitter.Pane2 := nil
  else
    OutputSplitter.Pane2 := CompilerOutput;
end;

procedure TMainForm.Options(Sender: TObject);
{###var
  dlg: TStdBtnDialog;
  dl: TDockingLayout;
  l: TLabel;
  buttons: TGridLayout;
  OkButton: TButton;
begin
  dlg := TStdBtnDialog.Create(nil);
  // dlg.Buttons := [btnOK, btnCancel];
  dlg.Text := dlgGlobalOptions;
  dl := TDockingLayout.Create(dlg);
  dl.AddWidget(TKCLSHEditConfig.Create(nil), dmClient);

  l := TLabel.Create(dlg);
  l.Text := 'The editor config is only temporarily here';
  dl.AddWidget(l, dmTop);

  OkButton := TButton.Create;
  OkButton.Text := 'OK';

  buttons := TGridLayout.Create;
  buttons.Rows := 1;
  buttons.Columns := 3;
  buttons.AddWidget(OkButton, 0, 0, 1, 1);

  dl.AddWidget(buttons, dmBottom);

  dlg.Content := dl;
  dlg.Run;
end;}
begin
end;


var
  MainForm: TMainForm;
begin
  gettext.TranslateResourceStrings('intl/kassandra.%s.mo');

  Application.Initialize;
  Application.Title := 'Kassandra IDE';

  MainForm := TMainForm.Create;
  MainForm.Name := 'MainForm';
  Application.AddForm(MainForm);
  Application.Run;
  MainForm.Free;
end.


{
  $Log$
  Revision 1.3  2000/01/06 23:05:07  sg
  * Menu items now use "&" to mark the accelerator key

  Revision 1.2  2000/01/05 19:28:14  sg
  * Lots of bug fixes and adaptions to changes in other units
  * almost all widgets now have a correct component name and owner

  Revision 1.1.1.1  1999/12/30 21:32:52  sg
  Initial import

}
