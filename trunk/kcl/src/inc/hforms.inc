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

  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TCloseEvent = procedure(Sender: TObject; var AAction: TCloseAction) of object;

  TFormState = set of (fsCreating, fsVisible, fsShowing, fsModal);
  TModalResult = type Integer;

  TCustomForm = class(TWinControl)
  private
    LastMinW, LastMinH, LastMaxW, LastMaxH: Integer;
    FContent: TControl;
    FModalResult: TModalResult;
    FBorderWidth, FDefaultWidth, FDefaultHeight, FPositionX, FPositionY: Integer;
    FOnClose: TCloseEvent;
    FOnCreate: TNotifyEvent;
    procedure SetContent(AContent: TControl);
    procedure SetDefaultHeight(AHeight: Integer);
    procedure SetDefaultWidth(AWidth: Integer);
    procedure SetModalResult(AResult: TModalResult);
    function  GetPositionX: Integer;
    function  GetPositionY: Integer;
    procedure SetPositionX(x: Integer);
    procedure SetPositionY(y: Integer);
  protected
    priv: TGenericForm_private;
    FFormState: TFormState;
    procedure ApplyMinMaxSize;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    procedure LayoutChildren; override;
    property  BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property  Content: TControl read FContent write SetContent;
    property  DefaultHeight: Integer read FDefaultHeight write SetDefaultHeight default -1;
    property  DefaultWidth: Integer read FDefaultWidth write SetDefaultWidth default -1;
    property  OnClose: TCloseEvent read FOnClose write FOnClose;
    property  OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close;
    function  CloseQuery: Boolean; virtual;
    function  IsRadioGroup: Boolean; override;
    procedure SetDefaultSize(AWidth, AHeight: Integer);
    procedure SetPosition(x, y: Integer);
    procedure Show;
    function  ShowModal: Integer; virtual;
    property  FormState: TFormState;
    property  ModalResult: TModalResult read FModalResult write SetModalResult;
    property  PositionX: Integer read GetPositionX write SetPositionX default -1;
    property  PositionY: Integer read GetPositionY write SetPositionY default -1;
  end;


// -------------------------------------------------------------------
//   TForm
// -------------------------------------------------------------------

  TForm = class(TCustomForm)
  published
    property BorderWidth;
    property Content;
    property DefaultWidth;
    property DefaultHeight;
    property PositionX;
    property PositionY;
    property Text;
    property OnClose;
  end;


{
  $Log$
  Revision 1.8  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.7  2000/02/25 11:40:28  michael
  + Added Checked reader to TRadioButton
  + Started support for ShowModal

  Revision 1.6  2000/02/24 13:39:32  sg
  * More layout fixes...

  Revision 1.5  2000/02/17 22:23:01  sg
  * Changed the visibility of some TCustomForm properties to get more VCL-like
  * Added "IsRadioGroup" (needed for TRadioButton under GTK)

  Revision 1.4  2000/02/10 18:35:18  sg
  * Renamed TGenericForm to TCustomForm, to make this more VCL-like
  * Added new properties for accessing the position and the default size
    of a form
}
