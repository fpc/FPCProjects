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
//   Command lists and commands
// ===================================================================

type

  TCommandList = class;


// -------------------------------------------------------------------
//   TCommand
// -------------------------------------------------------------------

  TCommand = class(TKCLComponent)
  protected
    FCommandList: TCommandList;
    FText, FHint, FDescription, FShortCut: String;
    FImageIndex: Integer;
    FEnabled, FChecked: Boolean;
    FOnExecute: TNotifyEvent;

    procedure Changed(ChangeType: TCmdChangeType);
    procedure SetCommandList(ACommandList: TCommandList);
    procedure SetEnabled(AEnabled: Boolean);

    procedure SetParentComponent(AParent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(Sender: TObject);

    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
  //### published  compiler bug workaround !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    property CommandList: TCommandList read FCommandList write SetCommandList;
    property Text: String read FText write FText;
    property Hint: String read FHint write FHint;
    property Description: String read FDescription write FDescription;
    property ShortCut: String read FShortCut write FShortCut;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Checked: Boolean read FChecked write FChecked;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;


// -------------------------------------------------------------------
//   TCommandList
// -------------------------------------------------------------------

  TCommandList = class(TKCLComponent)
  protected
    List: TList;
    FImages: TImageList;
    function GetCommandCount: Integer;
    function GetCommands(Index: Integer): TCommand;

    // Streaming support:
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure SetChildOrder(AComponent: TComponent; AOrder: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(AOwner: TComponent): TCommand;
    function Add(AOwner: TComponent; const AComponentName, AText,
      AShortCut: String; AImageIndex: Integer; ANotify: TNotifyEvent): TCommand;

    property CommandCount: Integer read GetCommandCount;
    property Commands[Index: Integer]: TCommand read GetCommands; default;
  published
    property Images: TImageList read FImages write FImages;
  end;


{
  $Log$
  Revision 1.1  2000/01/24 00:02:21  sg
  * First version

}
