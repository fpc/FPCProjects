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
  protected
    FButton: TGenericButton;
    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
    procedure EvRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TComboBox = class(TCustomComboBox)
  published
    property Enabled;
    property Text;
  end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
