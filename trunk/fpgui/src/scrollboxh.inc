{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Scrolling support declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TScrollingSupport = class
  private
    Parent: TWidget;
    FBorders: TRect;
    FClientRect: TRect;
    FHorzScrollBar, FVertScrollBar: TScrollBar;
    FOnClientRectChange: TNotifyEvent;
    function EvMouseWheel(Event: TMouseWheelEventObj): Boolean;
  public
    constructor Create(AParent: TWidget);
    destructor Destroy; override;
    function ProcessEvent(Event: TEventObj): Boolean;
    function DistributeEvent(Event: TEventObj): Boolean;
    function SendToChild(AChild: TWidget; Event: TEventObj): Boolean;
    procedure EvRecalcLayout;
    function CalcClientSize(AHorzBarVisible, AVertBarVisible: Boolean): TSize;

    procedure DefHorzScrollHandler(Sender: TObject; var APosition: Integer);
    procedure DefVertScrollHandler(Sender: TObject; var APosition: Integer);

    property Borders: TRect read FBorders;
    property ClientRect: TRect read FClientRect;
    property HorzScrollBar: TScrollBar read FHorzScrollBar;
    property VertScrollBar: TScrollBar read FVertScrollBar;
    property OnClientRectChange: TNotifyEvent read FOnClientRectChange write FOnClientRectChange;
  end;


  TCustomScrollBox = class(TWidget)
  protected
    ScrollingSupport: TScrollingSupport;
    procedure Paint(Canvas: TGfxCanvas); override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    function DistributeEvent(Event: TEventObj): Boolean; override;
//    procedure EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure EvRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TScrollBox = class(TCustomScrollBox)
  end;


{
  $Log$
  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/17 21:36:55  sg
  * Scrolling improvements

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
