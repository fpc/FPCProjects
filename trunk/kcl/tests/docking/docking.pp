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

program docking;
uses Classes, KCL;

type

  TMainForm = class(TForm)
    DockingLayout: TDockingLayout;
    StatusBar: TStatusBar;
    top, left: TButton;
    btn: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockingLayout := TDockingLayout.Create(Self);
  Content := DockingLayout;

  StatusBar := TStatusBar.Create(Self);
  StatusBar.Text := 'Bottom';
  DockingLayout.AddWidget(StatusBar, dmBottom);

  top := TButton.Create(Self);
  top.Text := 'Top';
  DockingLayout.AddWidget(top, dmTop);

{  left := TButton.Create(Self);
  left.Text := 'Left';
  DockingLayout.AddWidget(left, dmLeft);}

  btn := TButton.Create(Self);
  btn.Text := 'Client';
  DockingLayout.AddWidget(btn, dmClient);
end;

var
  MainForm: TMainForm;

begin

  Application.Initialize;
  Application.Title := 'KCL docking test';

  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log$
  Revision 1.1  2000/02/17 22:38:27  sg
  * First public version

}
