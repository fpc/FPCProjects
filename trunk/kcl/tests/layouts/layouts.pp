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

program Layouts;
uses Classes, KCL;

type
  TMainForm = class(TForm)
    Box: TBoxLayout;
    Caption: TLabel;
    SimpleBtn, FixedBtn, BoxBtn, GridBtn, DockingBtn, ExitBtn: TButton;
    Separator: TSeparator;

    procedure SimpleBtnClicked(Sender: TObject);
    procedure FixedBtnClicked(Sender: TObject);
    procedure ExitBtnClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TSimpleForm = class(TForm)
    Button: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TFixedForm = class(TForm)
    Layout: TFixedLayout;
    Button1, Button2: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderWidth := 8;

  Box := TBoxLayout.Create(Self);
    Box.Name := 'Box';
    Box.Orientation := boxVert;
    Box.HorzAlign := horzFill;
    Box.Spacing := 8;
    Caption := TLabel.Create(Self);
      Caption.Name := 'Caption';
      Caption.Text := 'Choose a test form:';
    Box.AddWidget(Caption);
    SimpleBtn := TButton.Create(Self);
      SimpleBtn.Name := 'SimpleBtn';
      SimpleBtn.Text := 'Simple layout';
      SimpleBtn.OnClick := @SimpleBtnClicked;
    Box.AddWidget(SimpleBtn);
    FixedBtn := TButton.Create(Self);
      FixedBtn.Name := 'FixedBtn';
      FixedBtn.Text := 'Fixed layout';
      FixedBtn.OnClick := @FixedBtnClicked;
    Box.AddWidget(FixedBtn);
    BoxBtn := TButton.Create(Self);
      BoxBtn.Name := 'BoxBtn';
      BoxBtn.Text := 'Box layout';
      BoxBtn.Enabled := False;
    Box.AddWidget(BoxBtn);
    GridBtn := TButton.Create(Self);
      GridBtn.Name := 'GridBtn';
      GridBtn.Text := 'Grid layout';
      GridBtn.Enabled := False;
    Box.AddWidget(GridBtn);
    DockingBtn := TButton.Create(Self);
      DockingBtn.Name := 'DockingBtn';
      DockingBtn.Text := 'Docking layout';
      DockingBtn.Enabled := False;
    Box.AddWidget(DockingBtn);
    Separator := TSeparator.Create(Self);
      Separator.Name := 'Separator';
    Box.AddWidget(Separator);
    ExitBtn := TButton.Create(Self);
      ExitBtn.Name := 'ExitBtn';
      ExitBtn.Text := 'Exit';
      ExitBtn.OnClick := @ExitBtnClicked;
    Box.AddWidget(ExitBtn);
  Content := Box;
end;

procedure TMainForm.SimpleBtnClicked(Sender: TObject);
var
  form: TSimpleForm;
begin
  Application.CreateForm(TSimpleForm, form);
end;

procedure TMainForm.FixedBtnClicked(Sender: TObject);
var
  form: TFixedForm;
begin
  Application.CreateForm(TFixedForm, form);
end;

procedure TMainForm.ExitBtnClicked(Sender: TObject);
begin
  Close;
end;


// -------------------------------------------------------------------
//   TSimpleForm
// -------------------------------------------------------------------

constructor TSimpleForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Simple Layout';
  BorderWidth := 8;

  Button := TButton.Create(Self);
    Button.Text := 'A button...';
  Content := Button;
end;


// -------------------------------------------------------------------
//   TFixedForm
// -------------------------------------------------------------------

constructor TFixedForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Fixed Layout';
  BorderWidth := 8;

  Layout := TFixedLayout.Create(Self);
    Layout.Name := 'Layout';
    Button1 := TButton.Create(Self);
      Button1.Name := 'Button1';
      Button1.Text := 'A button';
    Layout.AddWidget(Button1, 20, 20);
    Button2 := TButton.Create(Self);
      Button2.Name := 'Button2';
      Button2.Text := 'Another button';
    Layout.AddWidget(Button2, 50, 100);
  Content := Layout;
end;


// -------------------------------------------------------------------
//   Application setup
// -------------------------------------------------------------------

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.Title := 'KCL Layout Test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log$
  Revision 1.1  2000/02/18 18:31:10  sg
  * First version

}
