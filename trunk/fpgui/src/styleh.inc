{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Style declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// Styles

  TItemFlags = set of (ifFocused, ifSelected, ifDeactivated);

  TArrowDirection = (arrowUp, arrowDown, arrowLeft, arrowRight);

  TStyle = class
  protected
    UIColorCache: array[0..$18] of TGfxColor;
  public
    constructor Create(ADisplay: TGfxDisplay); virtual;
    // Colors
    procedure UpdateUIColorCache;
    function GetGUIColor(Color: TColor): TGfxColor; virtual; abstract;

    function GetUIColor(AColor: TColor): TGfxColor;
    procedure SetUIColor(Canvas: TGfxCanvas; Color: TColor);

    // General
    procedure DrawText(Canvas: TGfxCanvas; x, y: Integer; const AText: String;
      State: TWidgetState); virtual; abstract;

    procedure DrawItemBefore(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TItemFlags); virtual; abstract;
    procedure DrawItemAfter(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TItemFlags); virtual; abstract;

    // Windows
    procedure DrawWindowBackground(Canvas: TGfxCanvas;
      const ARect: TRect); virtual; abstract;
    // Buttons
    procedure DrawButtonFace(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TButtonFlags); virtual; abstract;
    function GetButtonBorders: TRect; virtual; abstract;
    // Separators
    function GetSeparatorSize: Integer; virtual; abstract;
    procedure DrawSeparator(Canvas: TGfxCanvas; const ARect: TRect;
      AOrientation: TOrientation); virtual; abstract;
    // Group boxes
    procedure DrawGroupBox(Canvas: TGfxCanvas; const ARect: TRect;
      const ALabel: String; WidgetState: TWidgetState); virtual; abstract;
    function GetGroupBoxBorders(Canvas: TGfxCanvas; const ALabel: String;
      var LabelWidth: Integer): TRect; virtual; abstract;
    // Edit widgets
    procedure DrawEditBox(Canvas: TGfxCanvas;
      const ARect: TRect); virtual; abstract;
    function GetEditBoxBorders: TRect; virtual; abstract;
    // Check boxes
    procedure DrawCheckBox(Canvas: TGfxCanvas; const ARect, LabelRect: TRect;
      Flags: TCheckboxFlags); virtual; abstract;
    procedure GetCheckBoxLayout(LabelW, LabelH: Integer;
      var TotalW, TotalH, LabelX, LabelY: Integer); virtual; abstract;
    // Radio buttons
    procedure DrawRadioButton(Canvas: TGfxCanvas;
      const ARect, LabelRect: TRect; Flags: TCheckboxFlags); virtual; abstract;
    procedure GetRadioButtonLayout(LabelW, LabelH: Integer;
      var TotalW, TotalH, LabelX, LabelY: Integer); virtual; abstract;
    // Combo boxes
    function GetComboBoxArrowSize: TSize; virtual; abstract;
    procedure DrawComboBoxArrow(Canvas: TGfxCanvas; const ARect: TRect;
      IsPressed, IsEnabled: Boolean); virtual; abstract;
    // Scroll bars
    function GetScrollBarBorders(Orientation: TOrientation): TRect; virtual; abstract;
    function GetScrollBarBtnSize(Orientation: TOrientation): TSize; virtual; abstract;
    function GetScrollBarBtnMinSize: Integer; virtual; abstract;
    procedure DrawScrollBarBorder(Canvas: TGfxCanvas;
      const ARect: TRect); virtual; abstract;
    procedure DrawScrollBarButton(Canvas: TGfxCanvas; const ARect: TRect;
      Direction: TArrowDirection;
      IsPressed, IsEnabled: Boolean); virtual; abstract;
    // Scroll boxes
    function GetScrollBoxBorders: TRect; virtual; abstract;
    procedure DrawScrollBoxBorder(Canvas: TGfxCanvas;
      const ARect: TRect); virtual; abstract;
  end;

  TDefaultStyle = class(TStyle)
  protected
    ImageCanvas, MaskCanvas: TGfxCanvas;
//    ArrowBitmaps: array[TArrowDirection] of TGfxImage;
  public
    constructor Create(ADisplay: TGfxDisplay); override;
    destructor Destroy; override;

    procedure Draw3DFrame(Canvas: TGfxCanvas; const ARect: TRect;
      Color1, Color2, Color3, Color4: TColor);
    procedure DrawFocusRect(Canvas: TGfxCanvas; const ARect: TRect);
    procedure DrawSunkenOuterBorder(Canvas: TGfxCanvas; const ARect: TRect);

    function GetGUIColor(Color: TColor): TGfxColor; override;
    procedure DrawText(Canvas: TGfxCanvas; x, y: Integer; const AText: String;
      State: TWidgetState); override;
    procedure DrawItemBefore(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TItemFlags); override;
    procedure DrawItemAfter(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TItemFlags); override;
    procedure DrawWindowBackground(Canvas: TGfxCanvas;
      const ARect: TRect); override;
    procedure DrawButtonFace(Canvas: TGfxCanvas; const ARect: TRect;
      Flags: TButtonFlags); override;
    function GetButtonBorders: TRect; override;
    function GetSeparatorSize: Integer; override;
    procedure DrawSeparator(Canvas: TGfxCanvas; const ARect: TRect;
      AOrientation: TOrientation); override;
    procedure DrawGroupBox(Canvas: TGfxCanvas; const ARect: TRect;
      const ALabel: String; WidgetState: TWidgetState); override;
    function GetGroupBoxBorders(Canvas: TGfxCanvas; const ALabel: String;
      var LabelWidth: Integer): TRect; override;
    procedure DrawEditBox(Canvas: TGfxCanvas; const ARect: TRect); override;
    function GetEditBoxBorders: TRect; override;
    procedure DrawCheckBox(Canvas: TGfxCanvas; const ARect, LabelRect: TRect;
      Flags: TCheckboxFlags); override;
    procedure GetCheckBoxLayout(LabelW, LabelH: Integer;
      var TotalW, TotalH, LabelX, LabelY: Integer); override;
    procedure DrawRadioButton(Canvas: TGfxCanvas;
      const ARect, LabelRect: TRect; Flags: TCheckboxFlags); override;
    procedure GetRadioButtonLayout(LabelW, LabelH: Integer;
      var TotalW, TotalH, LabelX, LabelY: Integer); override;
    function GetComboBoxArrowSize: TSize; override;
    procedure DrawComboBoxArrow(Canvas: TGfxCanvas; const ARect: TRect;
      IsPressed, IsEnabled: Boolean); override;
    function GetScrollBarBorders(Orientation: TOrientation): TRect; override;
    function GetScrollBarBtnSize(Orientation: TOrientation): TSize; override;
    function GetScrollBarBtnMinSize: Integer; override;
    procedure DrawScrollBarBorder(Canvas: TGfxCanvas;
      const ARect: TRect); override;
    procedure DrawScrollBarButton(Canvas: TGfxCanvas; const ARect: TRect;
      Direction: TArrowDirection; IsPressed, IsEnabled: Boolean); override;
    function GetScrollBoxBorders: TRect; override;
    procedure DrawScrollBoxBorder(Canvas: TGfxCanvas;
      const ARect: TRect); override;
  end;


{
  $Log$
  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
