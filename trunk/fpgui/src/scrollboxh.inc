{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
    FClientRect: TRect;
    Parent: TWidget;
    HorzScrollBar, VertScrollBar: TCustomScrollBar;
    function EvMouseWheel(Event: TMouseWheelEventObj): Boolean;
  public
    constructor Create(AParent: TWidget);
    destructor Destroy; override;
    function ProcessEvent(Event: TEventObj): Boolean;
    function DistributeEvent(Event: TEventObj): Boolean;
    procedure EvRecalcLayout;
    property ClientRect: TRect read FClientRect;
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
  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
