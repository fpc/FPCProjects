{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}
{$H+}

program dlgtest;
uses SysUtils, Classes, KCL;

type
  TTestDialog = class(TStandardDialog)
    BaseLayout: TGridLayout;
    CheckBox: TCheckBox;
    Button: TButton;
    RadioGroup1, RadioGroup2: TGroupBox;
    RadioLayout1, RadioLayout2: TBoxLayout;
    Radio1a, Radio1b, Radio2a, Radio2b: TRadioButton;
    ListBox: TListBox;
    Edit: TEdit;
    ComboBox: TComboBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;


constructor TTestDialog.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  BorderWidth := 4;

  BaseLayout := TGridLayout.Create(Self);
    BaseLayout.RowCount := 4;
    BaseLayout.ColCount := 2;

    CheckBox := TCheckBox.Create(Self);
      CheckBox.Name := 'CheckBox';
      CheckBox.Text := 'A checkbox';
    BaseLayout.AddControl(CheckBox, 0, 0, 1, 1);

    Button := TButton.Create(Self);
      Button.Name := 'Button';
      Button.Text := 'A button';
    BaseLayout.AddControl(Button, 1, 0, 1, 1);

    RadioGroup1 := TGroupBox.Create(Self);
      RadioGroup1.Name := 'RadioGroup1';
      RadioGroup1.Text := 'Radio buttons 1';
      RadioLayout1 := TBoxLayout.Create(Self);
        RadioLayout1.Name := 'RadioLayout1';
        RadioLayout1.Orientation := boxVert;
        Radio1a := TRadioButton.Create(Self);
          Radio1a.Name := 'Radio1a';
          Radio1a.Text := 'Radio button 1 a';
        RadioLayout1.AddControl(Radio1a);
        Radio1b := TRadioButton.Create(Self);
          Radio1b.Name := 'Radio1b';
          Radio1b.Text := 'Radio button 1 b';
        RadioLayout1.AddControl(Radio1b);
    RadioGroup1.Content := RadioLayout1;
    BaseLayout.AddControl(RadioGroup1, 0, 1, 1, 1);

    RadioGroup2 := TGroupBox.Create(Self);
      RadioGroup2.Name := 'RadioGroup2';
      RadioGroup2.Text := 'Radio buttons 2';
      RadioLayout2 := TBoxLayout.Create(Self);
        RadioLayout2.Name := 'RadioLayout2';
        RadioLayout2.Orientation := boxVert;
        Radio2a := TRadioButton.Create(Self);
          Radio2a.Name := 'Radio2a';
          Radio2a.Text := 'Radio button 2 a';
        RadioLayout2.AddControl(Radio2a);
        Radio2b := TRadioButton.Create(Self);
          Radio2b.Name := 'Radio2b';
          Radio2b.Text := 'Radio button 2 b';
        RadioLayout2.AddControl(Radio2b);
      RadioGroup2.Content := RadioLayout2;
    BaseLayout.AddControl(RadioGroup2, 1, 1, 1, 1);

    ListBox := TListBox.Create(Self);
      ListBox.Name := 'ListBox';
      for i := 1 to 20 do
        ListBox.Items.Add(Format('List item %d', [i]));
    BaseLayout.AddControl(ListBox, 0, 2, 1, 2);

    Edit := TEdit.Create(Self);
      Edit.Name := 'Edit';
      Edit.Text := 'Edit field';
    BaseLayout.AddControl(Edit, 1, 2, 1, 1);

    ComboBox := TComboBox.Create(Self);
      ComboBox.Name := 'ComboBox';
      ComboBox.Text := 'ComboBox!';
      for i := 1 to 20 do
        ComboBox.Items.Add(Format('Combo item %d', [i]));
    BaseLayout.AddControl(ComboBox, 1, 3, 1, 1);

  Content := BaseLayout;
end;


var
  dlg: TTestDialog;

begin
  Application.Initialize;
  Application.Title := 'KCL Dialog Test';

  dlg := TTestDialog.Create(nil);
  WriteLn('Dialog result code: ', dlg.ShowModal);
  dlg.Free;
end.


{
  $Log$
  Revision 1.3  2000/03/19 00:32:57  sg
  * Adapted to changes in KCL interface

  Revision 1.2  2000/02/22 14:42:36  sg
  * Adapted radio box layout widgets to changes in KCL interface

  Revision 1.1  2000/02/17 22:38:27  sg
  * First public version

}
