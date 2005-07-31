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


// ===================================================================
//   Menu Support (including Menu Bars and Menu Items)
// ===================================================================

type

  TMenuItemStyle = (misDefault, misSeparator, misCheck);

  TMenuItem = class(TKCLComponent)
  protected
    FHandle: TMenuItemHandle;
    FStyle: TMenuItemStyle;
    FCommand: TCommand;
    FParent: TObject;
    priv: TMenuItem_private;
    function  CommandChangeFilter(AEvent: TEventObj): Boolean;
    procedure OnFinishCreation; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: TMenuItemHandle read FHandle;
  published
    property Style: TMenuItemStyle read FStyle write FStyle;
    property Command: TCommand read FCommand write FCommand;
  end;

  TMenu = class(TMenuItem)
  protected
    FItems: TList;
    FText: String;
    MenuPriv: TMenu_private;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(AItem: TMenuItem);
  end;

  TMenuBar = class(TWinControl)
  protected
    FItems: TList;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(AItem: TMenuItem);
  end;


{
  $Log$
  Revision 1.6  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.5  2000/02/10 18:37:08  sg
  * Added destructor to TMenuItem to remove a memory leak
  * Added DoRecalcLayout to TMenuBar

  Revision 1.4  2000/01/24 00:06:18  sg
  * Restructuring, menu items are now bound to a TCommand

  Revision 1.3  2000/01/07 22:48:49  sg
  * Added property "ShortCut" to TMenuItem

  Revision 1.2  2000/01/06 22:58:52  sg
  * TMenuItem now stores its parent (which can be TMenuBar or TMenu)

  Revision 1.1.1.1  1999/12/30 21:33:19  sg
  Initial import

}
