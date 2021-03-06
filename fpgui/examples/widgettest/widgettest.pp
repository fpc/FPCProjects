program WidgetTest;

uses SysUtils, Classes, fpGUI;

type
  TMainForm = class(TForm)
    Box: TBoxLayout;
    Title: TLabel;
    ExitBtn: TButton;
    Separator: TSeparator;

    procedure CheckBoxBtnClick(Sender: TObject);
    procedure RadioButtonBtnClick(Sender: TObject);
    procedure GroupBoxBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure ScrollBarBtnClick(Sender: TObject);
    procedure ScrollBoxBtnClick(Sender: TObject);
    procedure ListBoxBtnClick(Sender: TObject);
    procedure ComboBoxBtnClick(Sender: TObject);
    procedure GridBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
  end;

  TTestForm = class(TForm)
  end;

  TCheckBoxForm = class(TTestForm)
    Box: TBoxLayout;
    GrayCheckBox, CheckBox1, CheckBox2: TCheckBox;

    procedure GrayCheckBoxClick(Sender: TObject);
  end;

  TRadioButtonForm = class(TTestForm)
    Box, HorzBox, ButtonBox1, ButtonBox2: TBoxLayout;
    GrayCheckBox: TCheckBox;
    Radio1a, Radio1b, Radio2a, Radio2b: TRadioButton;

    procedure GrayCheckBoxClick(Sender: TObject);
  end;

  TGroupBoxForm = class(TTestForm)
    HorzBox, VertBox1, VertBox2: TBoxLayout;
    GroupBox1, GroupBox2: TGroupBox;
    GrayCheckBox: TCheckBox;
    Button: TButton;
    Radio1, Radio2, Radio3, Radio4, Radio5: TRadioButton;

    procedure GrayCheckBoxClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
  end;

  TEditForm = class(TTestForm)
    Grid: TGridLayout;
    VertBox, HorzBox1, HorzBox2: TBoxLayout;
    Label1, Label2, PasswordDisplay: TLabel;
    Edit1, Edit2: TEdit;
    GrayCheckBox1, GrayCheckBox2: TCheckBox;
    Separator: TSeparator;

    procedure GrayCheckBox1Click(Sender: TObject);
    procedure GrayCheckBox2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  end;

  TScrollBarForm = class(TTestForm)
    VertLayout: TBoxLayout;
    GrayCheckBox: TCheckBox;
    HorzBox: TBoxLayout;
    HorzGrid, VertGrid: TGridLayout;
    VertBar: TSeparator;
    VertLabel, Label1, Label2, Label3, Label4, Label5: TLabel;
    PosLabel1, PosLabel2, PosLabel3, PosLabel4, PosLabel5: TLabel;
    VertScrollBar, ScrollBar1, ScrollBar2, ScrollBar3,
      ScrollBar4, ScrollBar5: TScrollBar;

    procedure GrayCheckBoxClick(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
    procedure ScrollBar4Change(Sender: TObject);
    procedure ScrollBar5Change(Sender: TObject);
  end;

  TScrollBoxForm = class(TTestForm)
    ScrollBox: TScrollBox;
  end;

  TListBoxForm = class(TTestForm)
    ListBox: TListBox;
  end;

  TComboBoxForm = class(TTestForm)
    VertLayout: TBoxLayout;
    GrayCheckBox: TCheckBox;
    BetaLabel: TLabel;
    ComboBox1, ComboBox2: TComboBox;

    procedure GrayCheckBoxClick(Sender: TObject);
  end;

  TGridForm = class(TTestForm)
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

procedure TMainForm.CheckBoxBtnClick(Sender: TObject);
var
  Form: TCheckBoxForm;
begin
  Application.CreateForm(TCheckBoxForm, Form);
end;

procedure TMainForm.RadioButtonBtnClick(Sender: TObject);
var
  Form: TRadioButtonForm;
begin
  Application.CreateForm(TRadioButtonForm, Form);
end;

procedure TMainForm.GroupBoxBtnClick(Sender: TObject);
var
  Form: TGroupBoxForm;
begin
  Application.CreateForm(TGroupBoxForm, Form);
end;

procedure TMainForm.EditBtnClick(Sender: TObject);
var
  Form: TEditForm;
begin
  Application.CreateForm(TEditForm, Form);
end;

procedure TMainForm.ScrollBarBtnClick(Sender: TObject);
var
  Form: TScrollBarForm;
begin
  Application.CreateForm(TScrollBarForm, Form);
end;

procedure TMainForm.ScrollBoxBtnClick(Sender: TObject);
var
  Form: TScrollBoxForm;
begin
  Application.CreateForm(TScrollBoxForm, Form);
end;

procedure TMainForm.ListBoxBtnClick(Sender: TObject);
var
  Form: TListBoxForm;
begin
  Application.CreateForm(TListBoxForm, Form);
end;

procedure TMainForm.ComboBoxBtnClick(Sender: TObject);
var
  Form: TComboBoxForm;
begin
  Application.CreateForm(TComboBoxForm, Form);
end;

procedure TMainForm.GridBtnClick(Sender: TObject);
var
  Form: TGridForm;
begin
  Application.CreateForm(TGridForm, Form);
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;


// -------------------------------------------------------------------
//   TCheckBoxForm
// -------------------------------------------------------------------

procedure TCheckBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  CheckBox1.Enabled := not GrayCheckBox.Checked;
  CheckBox2.Enabled := not GrayCheckBox.Checked;
end;


// -------------------------------------------------------------------
//   TRadioButtonForm
// -------------------------------------------------------------------

procedure TRadioButtonForm.GrayCheckBoxClick(Sender: TObject);
begin
  HorzBox.Enabled := not GrayCheckBox.Checked;
end;


// -------------------------------------------------------------------
//   TGroupBoxForm
// -------------------------------------------------------------------

procedure TGroupBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  GroupBox2.Enabled := not GrayCheckBox.Checked;
end;

procedure TGroupBoxForm.ButtonClick(Sender: TObject);
begin
  Radio1.Checked := True;
  Button.Enabled := False;
end;

procedure TGroupBoxForm.RadioButtonClick(Sender: TObject);
begin
  Button.Enabled := not Radio1.Checked;
end;


// -------------------------------------------------------------------
//   TEditForm
// -------------------------------------------------------------------

procedure TEditForm.GrayCheckBox1Click(Sender: TObject);
begin
  Edit1.Enabled := not GrayCheckBox1.Checked;
end;

procedure TEditForm.GrayCheckBox2Click(Sender: TObject);
begin
  Edit2.Enabled := not GrayCheckBox2.Checked;
end;

procedure TEditForm.Edit2Change(Sender: TObject);
begin
  PasswordDisplay.Text := '(= ' + Edit2.Text + ')';
end;


// -------------------------------------------------------------------
//   TScrollBarForm
// -------------------------------------------------------------------

procedure TScrollBarForm.GrayCheckBoxClick(Sender: TObject);
begin
  HorzBox.Enabled := not GrayCheckBox.Checked;
end;

procedure TScrollBarForm.ScrollBar1Change(Sender: TObject);
begin
  PosLabel1.Text := IntToStr(ScrollBar1.Position);
end;

procedure TScrollBarForm.ScrollBar2Change(Sender: TObject);
begin
  PosLabel2.Text := IntToStr(ScrollBar2.Position);
end;

procedure TScrollBarForm.ScrollBar3Change(Sender: TObject);
begin
  PosLabel3.Text := IntToStr(ScrollBar3.Position);
end;

procedure TScrollBarForm.ScrollBar4Change(Sender: TObject);
begin
  PosLabel4.Text := IntToStr(ScrollBar4.Position);
end;

procedure TScrollBarForm.ScrollBar5Change(Sender: TObject);
begin
  PosLabel5.Text := IntToStr(ScrollBar5.Position);
end;


// -------------------------------------------------------------------
//   TComboBoxForm
// -------------------------------------------------------------------

procedure TComboBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  ComboBox1.Enabled := not GrayCheckBox.Checked;
  ComboBox2.Enabled := not GrayCheckBox.Checked;
end;


// -------------------------------------------------------------------
//   TGridForm
// -------------------------------------------------------------------

procedure TGridForm.FormCreate(Sender: TObject);
var
  x, y: Integer;
begin
  for y := 0 to StringGrid.RowCount - 1 do
    for x := 0 to StringGrid.ColCount - 1 do
      StringGrid.Cells[x, y] := Format('%d, %d', [x, y]);
end;


var
  MainForm: TMainForm;
begin
WriteLn('Version: ' + {$I %date%} + ' ' + {$I %time%});
  Application.Title := 'Widget Test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
