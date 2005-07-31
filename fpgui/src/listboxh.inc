{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
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
    FHotTrack: Boolean;
    FItems: TStrings;
    FItemIndex: Integer;
    function EvMousePressed(Event: TMousePressedEventObj): Boolean;
    function EvMouseReleased(Event: TMouseReleasedEventObj): Boolean;
    function EvMouseMoved(Event: TMouseMoveEventObj): Boolean;
    function ProcessMouseEvent(Event: TMouseEventObj): Boolean;
  protected
    ScrollingSupport: TScrollingSupport;
    FMaxItemWidth: Integer;
    ItemHeight: Integer;

    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
//    procedure EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure CalcSizes; override;
    procedure Resized; override;

    procedure RecalcWidth;
    procedure UpdateScrollBars;
    procedure RedrawItem(AIndex: Integer);

    property CanExpandWidth default True;
    property CanExpandHeight default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property ItemIndex: Integer read FItemIndex default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStrings read FItems write FItems;
  end;

  TListBox = class(TCustomListBox)
  published
    // TWidget properties
    property OnClick;
    property Enabled;
    // TCustomListBox properties
    property HotTrack;
    property Items;
  end;


{
  $Log$
  Revision 1.5  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
