{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Scrollbar widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

  // !!!: Add argument which indicates the type of scrolling
  TScrollEvent = procedure(Sender: TObject; var APosition: Integer) of object;

  TCustomScrollBar = class(TWidget)
  private
    FOrientation: TOrientation;
    FMin, FMax, FPageSize, FPosition: Integer;
    FSmallChange, FLargeChange: Integer;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;
    ButtonUp, ButtonDown: TGenericButton;
    Slider: TWidget;
    Embedded: Boolean;		// for internal embedded usage!

    // Event handling
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    // Property access
    procedure SetOrientation(AOrientation: TOrientation);
    procedure SetMin(AMin: Integer);
    procedure SetMax(AMax: Integer);
    procedure SetPageSize(APageSize: Integer);
    procedure SetPosition(APosition: Integer);
    // Helpers
    function GetButtonSize: Integer;
    function ClipPosition(APosition: Integer): Integer;
    procedure UpdateBar;
  protected
    function DistributeEvent(Event: TEventObj): Boolean; override;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
    procedure Resized; override;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition default 0;
    property SmallChange: Integer read FSmallChange write FSmallChange default 1;
    property LargeChange: Integer read FLargeChange write FLargeChange default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LineUp;
    procedure LineDown;
    procedure PageUp;
    procedure PageDown;
  end;

  TScrollBar = class(TCustomScrollBar)
  published
    property Enabled;
    property Orientation;
    property Min;
    property Max;
    property PageSize;
    property Position;
    property SmallChange;
    property LargeChange;
    property OnChange;
    property OnScroll;
  end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
