{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
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

  TMenuItem = class(TKCLComponent)
  protected
    FText: String;
    FGray: Boolean;
    FHandle: TMenuItemHandle;
    FOnClick: TNotifyEvent;
    FParent: TObject;
    procedure OnFinishCreation; virtual;
    procedure SetGray(AGray: Boolean);
    procedure SetText(const AText: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoClick;
  published
    property  Text: String read FText write SetText;
    property  Gray: Boolean read FGray write SetGray;
    property  OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TMenuItemInfo = class(TCollectionItem)
  protected
    FItem: TMenuItem;
  published
    property Item: TMenuItem read FItem write FItem;
  end;

  TMenu = class(TMenuItem)
  protected
    FItems: TCollection;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(AItem: TMenuItem);
  published
    property Items: TCollection read FItems;
  end;

  TMenuBar = class(TWidget)
  protected
    FItems: TCollection;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(AItem: TMenuItem);
  published
    property Items: TCollection read FItems;
  end;


{
  $Log$
  Revision 1.2  2000/01/06 22:58:52  sg
  * TMenuItem now stores its parent (which can be TMenuBar or TMenu)

  Revision 1.1.1.1  1999/12/30 21:33:19  sg
  Initial import

}
