{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
    FWnd: TGfxWindow;
    FFrameType: Boolean;
    FOnCreate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    // Event handling
    procedure Resized;
    // Property access
    procedure SetFocusedWidget(AWidget: TWidget);
    procedure SetMouseCaptureWidget(AWidget: TWidget);
    // fpGFX event handling
    procedure WndClose(Sender: TObject);
    procedure WndFocusIn(Sender: TObject);
    procedure WndFocusOut(Sender: TObject);
    procedure WndHide(Sender: TObject);
    procedure WndKeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
    procedure WndKeyReleased(Sender: TObject; Key: Word; Shift: TShiftState);
    procedure WndKeyChar(Sender: TObject; KeyChar: Char);
    procedure WndMouseEnter(Sender: TObject; Shift: TShiftState; x, y: Integer);
    procedure WndMouseLeave(Sender: TObject);
    procedure WndMouseMoved(Sender: TObject; Shift: TShiftState; x, y: Integer);
    procedure WndMousePressed(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure WndMouseReleased(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure WndMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Single; x, y: Integer);
    procedure WndPaint(Sender: TObject; const ARect: TRect);
    procedure WndMove(Sender: TObject);
    procedure WndResize(Sender: TObject);
    procedure WndShow(Sender: TObject);
  protected
    FBorderWidth: Integer;
    FIsActive, FResizedByUser, FPositionSpecified: Boolean;
    procedure Loaded; override;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure WidgetCoords(AWidget: TWidget; var OutX, OutY: Integer);
    function ProcessEvent(Event: TEventObj): Boolean; override;
    procedure EvRecalcLayout; override;
    procedure EvTextChanged; override;

    procedure CreateWnd;
    procedure ApplyNewLayout;

    property CanExpandWidth default True;
    property CanExpandHeight default True;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property FrameType: Boolean read FFrameType write FFrameType;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
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
    property FrameType;
    property Text;
    property OnCreate;
  end;


{
  $Log$
  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
