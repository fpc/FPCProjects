{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Listbox widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomListBox = class(TWidget)
  private
    FItems: TStrings;
    FItemIndex: Integer;
    function EvMousePressed(Event: TMousePressedEventObj): Boolean;
  protected
    ScrollingSupport: TScrollingSupport;
    FMaxItemWidth: Integer;
    ItemHeight: Integer;
    IsParented: Boolean;

    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
//    procedure EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure EvRecalcLayout; override;

    procedure RecalcWidth;
    procedure UpdateScrollBars;
    procedure RedrawItem(AIndex: Integer);

    property CanExpandWidth default True;
    property CanExpandHeight default True;
    property ItemIndex: Integer read FItemIndex default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read FItems write FItems;
  end;

  TListBox = class(TCustomListBox)
  published
    property Enabled;
    property Items;
  end;


{
  $Log$
  Revision 1.3  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
