{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Dialogs declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomStandardDialog = class(TCustomForm)
  private
    procedure StdBtnClicked(Sender: TObject);
  protected
    FButtons: TMsgDlgButtons;
    FDlgChild: TWidget;
    MainLayout, BtnLayout: TBoxLayout;
    procedure SetButtons(AButtons: TMsgDlgButtons);
    property Buttons: TMsgDlgButtons read FButtons write SetButtons default [mbOk, mbCancel];
    property Child: TWidget read FDlgChild write FDlgChild;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TStandardDialog = class(TCustomStandardDialog)
  published
    property Buttons;
    property Child;
    property Text;
  end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
