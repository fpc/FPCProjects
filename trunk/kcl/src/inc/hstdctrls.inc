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
//   Standard Controls:
//	T(Custom)GroupBox
//	T(Custom)Label
//	T(Custom)Edit
//	T(Custom)Memo
//	T(Custom)ComboBox
//	TButtonControl, TButton
//	T(Custom)CheckBox
//	TRadioButton
//	T(Custom)ListBox
//	TScrollBar
//	TStaticText
// ===================================================================

type

// -------------------------------------------------------------------
//   Group Box
// -------------------------------------------------------------------

  TCustomGroupBox = class(TCustomControl)
  private
    FContent: TControl;
  protected
    function  GetControlCount: Integer; override;
    function  GetControls(AIndex: Integer): TControl; override;
    procedure DoRecalcLayout; override;
    procedure LayoutChildren; override;
    procedure OnFinishCreation; override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    property  Content: TControl read FContent write FContent;
  public
    function  IsRadioGroup: Boolean; override;
  end;

  TGroupBox = class(TCustomGroupBox)
  published
    property Text;
    property CanExpandWidth;
    property CanExpandHeight;
    property Content;
  end;


// -------------------------------------------------------------------
//   Label
// -------------------------------------------------------------------

  TCustomLabel = class(TGraphicControl)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(AAlignment: TAlignment);
  protected
//    procedure DoRecalcLayout; override;
//    function ProcessEvent(Event: TEventObj): Boolean; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
  end;

  TLabel = class(TCustomLabel)
  published
    property Text;
    property CanExpandWidth;
    property CanExpandHeight;
    property Alignment;
  end;


// -------------------------------------------------------------------
//   Edit
// -------------------------------------------------------------------

  TCustomEdit = class(TWinControl)
  protected
    FDefaultWidth: Integer;
    FMultiLine: Boolean;
    procedure DoRecalcLayout; override;
    function GetText: String; override;
    procedure OnFinishCreation; override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    property DefaultWidth: Integer read FDefaultWidth write FDefaultWidth;
    property MultiLine: Boolean read FMultiLine write FMultiLine default False;
  end;

  TEdit = class(TCustomEdit)
    property CanExpandWidth;
    property CanExpandHeight;
    property DefaultWidth;
    property MultiLine;
    property Text;
  end;


// -------------------------------------------------------------------
//   Memo
// -------------------------------------------------------------------

// Nothing here yet...


// -------------------------------------------------------------------
//   Combo Box
// -------------------------------------------------------------------

  TComboBoxStyle = (csDropDown, csDropDownList);

  TCustomComboBox = class(TWinControl)
  private
    FItems: TStrings;
    FStyle: TComboBoxStyle;
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer);
  protected
    procedure DoRecalcLayout; override;
    procedure OnFinishCreation; override;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    property Style: TComboBoxStyle read FStyle write FStyle default csDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read FItems write FItems;
  end;

  TComboBox = class(TCustomComboBox)
  published
    property CanExpandWidth;
    property Style;
    property Items;
  end;


// -------------------------------------------------------------------
//   Buttons
// -------------------------------------------------------------------

  TButtonControl = class(TWinControl)
  protected
    function ProcessEvent(Event: TEventObj): Boolean; override;
  end;

  TButton = class(TButtonControl)
  private
    FDefault: Boolean;
  protected
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property Default: Boolean read FDefault write FDefault default False;
    property Text;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCustomCheckBox = class(TButtonControl)
  private
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    FState: TCheckBoxState;
    function  GetState: TCheckBoxState;
    procedure SetState(AState: TCheckBoxState);
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read GetState write SetState default cbUnchecked;
  end;

  TCheckBox = class(TCustomCheckBox)
  published
    property CanExpandWidth;
    property Checked;
    property State;
    property Text;
  end;

  TRadioButton = class(TButtonControl)
  private
    FChecked: Boolean;
  protected
    function  GetChecked: Boolean;
    procedure SetChecked(AChecked: Boolean);
    procedure DoRecalcLayout; override;
    procedure OnFinishCreation; override;
  published
    property CanExpandWidth;
    property Checked: Boolean read GetChecked write SetChecked;
    property Text;
  end;


// -------------------------------------------------------------------
//   List Box
// -------------------------------------------------------------------

  TCustomListBox = class(TWinControl)
  private
    FItems: TStrings;
  protected
    priv: TCustomListBox_private;
    function GetItemIndex: Integer;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
    property OnDrawItem: TDrawItemEvent;
    property OnMeasureItem: TMeasureItemEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Items: TStrings read FItems write FItems;
    property ItemIndex: Integer read GetItemIndex;
  end;

  TListBox = class(TCustomListBox)
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property Items;
    property ItemIndex;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


// -------------------------------------------------------------------
//   Scroll Bar
// -------------------------------------------------------------------

  // !!!: Find a better solution for all this range stuff
  TRange = class(TPersistent)
  protected
    FOnValueChange: TNotifyEvent;
    function  GetMaxValue: Integer; virtual; abstract;
    procedure SetMaxValue(AMaxValue: Integer); virtual; abstract;
    function  GetCurValue: Integer; virtual; abstract;
    procedure SetCurValue(ACurValue: Integer); virtual; abstract;
    function  GetPageSize: Integer; virtual; abstract;
    procedure SetPageSize(APageSize: Integer); virtual; abstract;
  published
  //property MinValue: Integer;
    property MaxValue: Integer read GetMaxValue write SetMaxValue default 200;
    property CurValue: Integer read GetCurValue write SetCurValue;
    property PageSize: Integer read GetPageSize write SetPageSize default 100;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;


{  TScrollBar = class(TWinControl)
  private
    FRange: TRange;
  protected
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Range: TRange read FRange;
  end;}


// -------------------------------------------------------------------
//   Static Text
// -------------------------------------------------------------------

  TCustomStaticText = class(TWinControl)
  end;

  TStaticText = class(TCustomStaticText)
  end;


{
  $Log$
  Revision 1.1  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

}
