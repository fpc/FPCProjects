{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
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
    MainLayout, BtnLayout: TBoxLayout;
    Separator: TSeparator;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
    procedure CalcSizes; override;
    procedure Resized; override;
    procedure SetButtons(AButtons: TMsgDlgButtons);
    property Buttons: TMsgDlgButtons read FButtons write SetButtons default [mbOk, mbCancel];
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TStandardDialog = class(TCustomStandardDialog)
  published
    property Text;
    property OnCreate;
    property Buttons;
  end;


{
  $Log$
  Revision 1.3  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
