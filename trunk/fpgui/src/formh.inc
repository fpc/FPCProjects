{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Form declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomForm = class(TBinWidget)
  private
    FFocusedWidget: TWidget;
    FMouseCaptureWidget: TWidget;
    FLastSetCursor: TGfxCursor;
    FWnd: TGfxWindow;
    FWindowType: TGfxWindowType;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    // Property access
    procedure SetFocusedWidget(AWidget: TWidget);
    procedure SetMouseCaptureWidget(AWidget: TWidget);
    // fpGFX event handling
    procedure WndClose(Sender: TObject);
    procedure WndFocusIn(Sender: TObject);
    procedure WndFocusOut(Sender: TObject);
    procedure WndHide(Sender: TObject);
    procedure WndKeyPressed(Sender: TObject; AKey: Word; AShift: TShiftState);
    procedure WndKeyReleased(Sender: TObject; AKey: Word; AShift: TShiftState);
    procedure WndKeyChar(Sender: TObject; AKeyChar: Char);
    procedure WndMouseEnter(Sender: TObject; AShift: TShiftState;
       const AMousePos: TPoint);
    procedure WndMouseLeave(Sender: TObject);
    procedure WndMouseMoved(Sender: TObject; AShift: TShiftState;
      const AMousePos: TPoint);
    procedure WndMousePressed(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure WndMouseReleased(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure WndMouseWheel(Sender: TObject; AShift: TShiftState;
      AWheelDelta: Single; const AMousePos: TPoint);
    procedure WndPaint(Sender: TObject; const ARect: TRect);
    procedure WndMove(Sender: TObject);
    procedure WndResize(Sender: TObject);
    procedure WndShow(Sender: TObject);
  protected
    FBorderWidth: Integer;
    FIsActive, FResizedByUser, FPositionSpecified: Boolean;
    procedure Loaded; override;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure Resized; override;
    function WidgetCoords(AWidget: TWidget): TPoint;
    function ProcessEvent(Event: TEventObj): Boolean; override;
    procedure CalcSizes; override;
    procedure EvTextChanged; override;

    procedure CreateWnd;

    property CanExpandWidth default True;
    property CanExpandHeight default True;
    property Cursor default crArrow;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property WindowType: TGfxWindowType read FWindowType write FWindowType default wtWindow;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show; override;
    procedure Close;
    procedure SetPosition(APosition: TPoint);
    property FocusedWidget: TWidget read FFocusedWidget write SetFocusedWidget;
    property IsActive: Boolean read FIsActive;
    property MouseCaptureWidget: TWidget read FMouseCaptureWidget write SetMouseCaptureWidget;
    property Wnd: TGfxWindow read FWnd;
  end;

  TForm = class(TCustomForm)
  published
    property Enabled;
    property BorderWidth;
    property WindowType;
    property Text;
    property OnCreate;
  end;


{
  $Log$
  Revision 1.5  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/18 15:37:27  sg
  * Added support for the different TGfxWindow window types

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
