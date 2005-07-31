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

program basicapp;
uses Classes, KCL;

type
  TMainForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaultSize(640, 400);
end;


var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.Title := 'KCL application skeleton';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log$
  Revision 1.1  2000/02/17 22:38:27  sg
  * First public version

}
