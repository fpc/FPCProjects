{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Lazarus profile settings form

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LazProfSettings;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, ButtonPanel, StdCtrls, XMLCfg, FileUtil, SysUtils, Classes;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    FPPPathButton: TButton;
    FPProfUnitPathButton: TButton;
    FPPPathEdit: TEdit;
    FPProfunitPathEdit: TEdit;
    FPProfUnitPathLabel: TLabel;
    GraphVizPathButton: TButton;
    ButtonPanel1: TButtonPanel;
    GraphVizPathEdit: TEdit;
    GraphVizPathLabel: TLabel;
    FPPPathLabel: TLabel;
    OpenDialog: TOpenDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPPPathButtonClick(Sender: TObject);
    procedure FPProfUnitPathButtonClick(Sender: TObject);
    procedure GraphVizPathButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure SaveSettings;
  public
    { public declarations }
  end;

var
  SettingsForm: TSettingsForm;
  XMLConfig: TXMLConfig;

implementation

{$R *.lfm}

uses
  LazProfResourceStings;

const
  Path = 'LazProfOptions/';

{ TSettingsForm }

procedure TSettingsForm.GraphVizPathButtonClick(Sender: TObject);
begin
  OpenDialog.Title := rsFindThePathToTheGraphVizExecutable;

  if OpenDialog.Execute then
    GraphVizPathEdit.Text := OpenDialog.FileName;
end;

procedure TSettingsForm.OKButtonClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TSettingsForm.SaveSettings;
begin
  //save all settings
  XMLConfig.SetDeleteValue(Path + 'GraphViz/Path', GraphVizPathEdit.Text, '');
  XMLConfig.SetDeleteValue(Path + 'FPP/Path', FPPPathEdit.Text, '');
  XMLConfig.SetDeleteValue(Path + 'FPProfUnit/Path', FPProfUnitPathEdit.Text, '');

  XMLConfig.Flush;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  GraphVizPathLabel.Caption := Format(rsGraphVizPathEGDot, [GetExeExt]);
  FPPPathLabel.Caption := Format(rsFPPPathEGFpp, [GetExeExt]);

  //load all settings
  GraphVizPathEdit.Text := XMLConfig.GetValue(Path + 'GraphViz/Path', '');
  FPPPathEdit.Text := XMLConfig.GetValue(Path + 'FPP/Path', '');
  FPProfUnitPathEdit.Text := XMLConfig.GetValue(Path + 'FPProfUnit/Path', '');
end;

procedure TSettingsForm.FPPPathButtonClick(Sender: TObject);
begin
  SelectDirectoryDialog.Title := rsFindThePathToTheFPPExecutable;

  if SelectDirectoryDialog.Execute then
    FPPPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TSettingsForm.FPProfUnitPathButtonClick(Sender: TObject);
begin
  SelectDirectoryDialog.Title := rsFindThePathToTheFPProfUnits;

  if SelectDirectoryDialog.Execute then
    FPProfUnitPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TSettingsForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

initialization
  XMLConfig := TXMLConfig.Create(nil);
  XMLConfig.Filename := GetAppConfigDir(False) + 'lazprof.xml';

finalization
  XMLConfig.Flush;
  XMLConfig.Free;

end.

