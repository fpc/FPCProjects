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
    procedure DockingBtnClicked(Sender: TObject);
    procedure GridBtnClicked(Sender: TObject);
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

  TDockingForm = Class(TForm)
    Layout : TDockingLayout;
    Button1,Button2,Button3,Button4,Button5 : TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGridForm = Class(TForm)
    Layout : TGridLayout;
    Button1,Button2,Button3,Button4,Button5 : TButton;
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
      GridBtn.Enabled := True;
      GridBtn.OnClick := @GridBtnCLicked;
    Box.AddWidget(GridBtn);
    DockingBtn := TButton.Create(Self);
      DockingBtn.Name := 'DockingBtn';
      DockingBtn.Text := 'Docking layout';
      DockingBtn.Enabled := True;
      DockingBtn.OnClick := @DockingBtnCLicked;
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

procedure TMainForm.DockingBtnClicked(Sender: TObject);
var
  form: TDockingForm;
begin
  Application.CreateForm(TDockingForm, form);
end;

procedure TMainForm.GridBtnClicked(Sender: TObject);
var
  form: TGridForm;
begin
  Application.CreateForm(TGridForm, form);
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
//   TDockingForm
// -------------------------------------------------------------------

constructor TDockingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Docking Layout';
  BorderWidth := 8;

  Layout := TDockingLayout.Create(Self);
    Layout.Name := 'Layout';
    Button1 := TButton.Create(Self);
      Button1.Name := 'BTop';
      Button1.Text := 'Top Alignment';
    Layout.AddWidget(Button1, dmTop);
    Button2 := TButton.Create(Self);
      Button2.Name := 'BBottom';
      Button2.Text := 'Bottom Alignment';
    Layout.AddWidget(Button2, dmBottom);
    Button3 := TButton.Create(Self);
      Button3.Name := 'BLeft';
      Button3.Text := 'Left Alignment';
    Layout.AddWidget(Button3, dmLeft);
    Button4 := TButton.Create(Self);
      Button4.Name := 'BRight';
      Button4.Text := 'Right Alignment';
    Layout.AddWidget(Button4, dmRight);
    Button5 := TButton.Create(Self);
      Button5.Name := 'BCLient';
      Button5.Text := 'Client Alignment';
    Layout.AddWidget(Button5, dmClient);
  Content := Layout;
end;

// -------------------------------------------------------------------
//   TGridForm
// -------------------------------------------------------------------

constructor TGridForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Grid Layout';
  BorderWidth := 8;

  Layout := TGridLayout.Create(Self);
    Layout.Name := 'Layout';
    Layout.Rows := 3;
    Layout.Columns := 3;
    Layout.HorzSpacing := 4;
    Layout.VertSpacing := 4;

    Button1 := TButton.Create(Self);
      Button1.Name := 'TopLeft';
      Button1.Text := 'Top Left';
    Layout.AddWidget(Button1, 0, 0, 1, 1);

    Button2 := TButton.Create(Self);
      Button2.Name := 'TopRight';
      Button2.Text := 'Top Right';
    Layout.AddWidget(Button2, 2,0,1,1);
    Button3 := TButton.Create(Self);
      Button3.Name := 'CenterCenter';
      Button3.Text := 'Center Center';
    Layout.AddWidget(Button3, 1,1,1,1);
    Button4 := TButton.Create(Self);
      Button4.Name := 'BottomLeft';
      Button4.Text := 'Bottom Left';
    Layout.AddWidget(Button4,0,2,1,1);
    Button5 := TButton.Create(Self);
      Button5.Name := 'BottomRight';
      Button5.Text := 'Bottom Right';
    Layout.AddWidget(Button5, 2,2,1,1);
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
  Revision 1.3  2000/02/18 20:02:57  michael
  + Added grid layout

  Revision 1.2  2000/02/18 19:53:09  michael
  + Added docking layout

  Revision 1.1  2000/02/18 18:31:10  sg
  * First version

}
