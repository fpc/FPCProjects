{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Combobox widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomComboBox = class(TWidget)
  private
    procedure ButtonClick(Sender: TObject);
    procedure DropDownDeactivate(Sender: TObject);
    procedure DropDownDestroy(Sender: TObject);
  protected
    FButton: TGenericButton;
    FDropDown: TCustomForm;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
    procedure Resized; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
    property CanExpandWidth default True;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TComboBox = class(TCustomComboBox)
  published
    property CanExpandWidth;
    property Enabled;
    property Text;
  end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
