{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}
{$H+}

program Kassandra;
uses dos, sysutils, xmlcfg, gettext, kcl,
  view_notebook, vw_generic, vw_shtext,
  sh_pas, sh_xml, shedit, kclshedit, doc_text;

resourcestring
  menuFile = 'File';
  menuNewFile = 'New file';
  menuOpenFile = 'Open file...';
  menuSaveFile = 'Save file';
  menuSaveFileAs = 'Save file as...';
  menuCloseFile = 'Close file';
  menuOpenRecentFile = 'Open recent file';
  menuExit = 'Exit';
  menuView = 'View';
  menuViewOutput = 'Output window';
  menuOptions = 'Options';
  menuGeneralOptions = 'General options...';

  dlgGlobalOptions = 'Global Options';

  strReady = 'Ready';
  strCompilerOutput = 'Compiler output will go here';

const

  RECENTFILECOUNT = 8;

type

  TMainForm = class(TForm)
  protected
    Layout: TDockingLayout;
    Views: TViewNotebook;
    StatusBar: TStatusBar;
    OutputSplitter: TSplitter;
    CompilerOutput: TKCLSHWidget;
    Config: TXMLConfig;
    NonameCounter: Integer;
    RecentFiles: array[0..RECENTFILECOUNT-1] of String;

    // Menu bar
    SaveFileItem, SaveFileAsItem, CloseFileItem: TMenuItem;
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
  Content := Layout;

  // Create notebook with file views
  Views := TViewNotebook.Create(Self);

  // Create compiler output window
  CompilerOutput := TKCLSHWidget.Create(Self);
  CompilerOutput.SetupEditor(TTextDoc.Create, TSHTextEdit);
  CompilerOutput.Document.AddLine(strCompilerOutput);

  // Create splitter which contains views and compiler output
  OutputSplitter := TSplitter.Create(Self);
  OutputSplitter.ResizePolicy := srFixedPane2;
  OutputSplitter.Pane1 := Views;
  OutputSplitter.Pane2 := CompilerOutput;
  Layout.AddWidget(OutputSplitter, dmClient);

  // Create menu bar
  CreateMenuBar;

  // Create status bar
  StatusBar := TStatusBar.Create(Self);
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
  bar: TMenuBar;
  menu, submenu: TMenu;
  item: TMenuItem;
begin
  bar := TMenuBar.Create(Self);
  Layout.AddWidget(bar, dmTop);

  // Add file menu
  menu := TMenu.Create(Self);
  menu.Text := menuFile;
  menu.OnClick := @FileMenu;
  bar.AddItem(menu);
  item := TMenuItem.Create(Self);
    item.Text := menuNewFile;
    item.OnClick := @FileNew;
    menu.AddItem(item);
  item := TMenuItem.Create(Self);
    item.Text := menuOpenFile;
    item.OnClick := @FileOpen;
    menu.AddItem(item);
  SaveFileItem := TMenuItem.Create(Self);
    SaveFileItem.Text := menuSaveFile;
    SaveFileItem.OnClick := @FileSave;
    menu.AddItem(SaveFileItem);
  SaveFileAsItem := TMenuItem.Create(Self);
    SaveFileAsItem.Text := menuSaveFileAs;
    SaveFileAsItem.OnClick := @FileSaveAs;
    menu.AddItem(SaveFileAsItem);
  CloseFileItem := TMenuItem.Create(Self);
    CloseFileItem.Text := menuCloseFile;
    CloseFileItem.OnClick := @FileClose;
    menu.AddItem(CloseFileItem);
  item := TMenuItem.Create(Self);
    item.Text := '-';
    menu.AddItem(item);
  //  TMenuItem.Create(menu).Text := 'Open workspace...';
  //  TMenuItem.Create(menu).Text := 'Save workspace';
  //  TMenuItem.Create(menu).Text := 'Save workspace as...';
  //  TMenuItem.Create(menu).Text := '-';
  submenu := TMenu.Create(Self);
    menu.AddItem(submenu);
    submenu.Text := menuOpenRecentFile;
    submenu.Gray := True;
  item := TMenuItem.Create(Self);
    item.Text := '-';
    menu.AddItem(item);
  item := TMenuItem.Create(Self);
    item.Text := menuExit;
    item.OnClick := @FileQuit;
    menu.AddItem(item);

  menu := TMenu.Create(Self);
  menu.Text := menuView;
  bar.AddItem(menu);
  item := TMenuItem.Create(Self);
    item.Text := menuViewOutput;
    item.OnClick := @ViewToggleOutputWindow;
    menu.AddItem(item);

{ Deactivated until all dialog and widget classes are working!
  menu := TMenu.Create(Self);
  menu.Text := menuOptions;
  bar.AddItem(menu);
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
    view := TSHPasView.Create(doc)
  else if (ext <> '') and (Pos(UpCase(ext) + ';',
    Config.GetValue('SyntaxHighlighter/XML/ext', 'XML;DTD;HTML;HTM') + ';') > 0) then
    view := TSHXMLView.Create(doc)
  else
    view := TSHTextView.Create(doc);

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
  WriteLn('FileNew');
  view := TSHTextView.Create(TTextDoc.Create, TSHPasEdit);
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
  if not FileDlg.Run then exit;

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
  Application.AddForm(MainForm);
  Application.Run;
  MainForm.Free;
end.


{
  $Log$
  Revision 1.1  1999/12/30 21:32:52  sg
  Initial revision

}
