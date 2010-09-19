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
    GraphVizPathButton: TButton;
    ButtonPanel1: TButtonPanel;
    GraphVizPathEdit: TEdit;
    GraphVizPathLabel: TLabel;
    OpenDialog: TOpenDialog;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

const
  Path = 'LazProfOptions/';

{ TSettingsForm }

procedure TSettingsForm.GraphVizPathButtonClick(Sender: TObject);
begin
  OpenDialog.Title := 'Find the path to the GraphViz executable';

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

  XMLConfig.Flush;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  GraphVizPathLabel.Caption := 'GraphViz path (e.g. dot' + GetExeExt + ')';

  //load all settings
  GraphVizPathEdit.Text := XMLConfig.GetValue(Path + 'GraphViz/Path', '');
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

