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

program sizetest;
uses Classes, KCL;

type
  TMainForm = class(TForm)
    button: TButton;
    procedure ButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;



constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  SetDefaultSize(640, 400);
  button := TButton.Create(Self);
  button.Name := 'button';
  button.Text := 'Click me';
  button.OnClick := @ButtonClick;
  Content := button;
end;

procedure TMainForm.ButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;


var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.Title := 'KCL resizing test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log$
  Revision 1.2  2000/03/19 00:32:57  sg
  * Adapted to changes in KCL interface

  Revision 1.1  2000/02/17 22:38:27  sg
  * First public version

}
