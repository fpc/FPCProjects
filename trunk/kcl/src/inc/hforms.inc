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
//   Base Classes for Forms
// ===================================================================

type

// -------------------------------------------------------------------
//   TCustomForm
// -------------------------------------------------------------------

  TCustomForm = class(TWidget)
  protected
    priv: TGenericForm_private;
    FContent: TWidget;
    FDefaultWidth, FDefaultHeight, FPositionX, FPositionY: Integer;
    FOnClose: TNotifyEvent;

    procedure SetContent(AContent: TWidget);
    procedure SetDefaultWidth(AWidth: Integer);
    procedure SetDefaultHeight(AHeight: Integer);
    function  GetPositionX: Integer;
    function  GetPositionY: Integer;
    procedure SetPositionX(x: Integer);
    procedure SetPositionY(y: Integer);
    procedure ApplyMinMaxSize;

    procedure OnFinishCreation; override;
    procedure ApplySize; override;
    procedure DoRecalcLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    function  CanClose: Boolean; virtual;
    procedure Show;
    procedure Close;
    procedure SetDefaultSize(AWidth, AHeight: Integer);
    procedure SetPosition(x, y: Integer);

    property Content: TWidget read FContent write SetContent;
    property DefaultWidth: Integer read FDefaultWidth write SetDefaultWidth default -1;
    property DefaultHeight: Integer read FDefaultHeight write SetDefaultHeight default -1;
    property PositionX: Integer read GetPositionX write SetPositionX default -1;
    property PositionY: Integer read GetPositionY write SetPositionY default -1;
    property OnClose: TNotifyEvent read FOnClose;
  end;


// -------------------------------------------------------------------
//   TForm
// -------------------------------------------------------------------

  TForm = class(TCustomForm)
  published
    property Text;
    property Content;
    property DefaultWidth;
    property DefaultHeight;
    property PositionX;
    property PositionY;
    property OnClose;
  end;


{
  $Log$
  Revision 1.4  2000/02/10 18:35:18  sg
  * Renamed TGenericForm to TCustomForm, to make this more VCL-like
  * Added new properties for accessing the position and the default size
    of a form

  Revision 1.3  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.2  2000/01/06 22:58:10  sg
  * Added "priv" field to TGenericForm

  Revision 1.1.1.1  1999/12/30 21:33:19  sg
  Initial import

}
