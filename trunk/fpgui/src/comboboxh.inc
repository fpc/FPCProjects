{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
    function EvResize(Event: TResizeEventObj): Boolean;
    procedure ButtonClick(Sender: TObject);
    procedure ListDeactivate(Sender: TObject);
  protected
    FButton: TGenericButton;
    FDropDown: TCustomForm;
    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
    procedure EvRecalcLayout; override;
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
  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
