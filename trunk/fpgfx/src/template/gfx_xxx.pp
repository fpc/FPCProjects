{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    Template for new target implementations

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFX_xxx;

interface

uses
  SysUtils, Classes,	// FPC units
                  	// xxx (insert target dependent units here)
  GfxBase;		// fpGUI units

type

  ExxxError = class(Exception);

  TxxxDrawable = class;
  TxxxDisplay = class;

  TxxxFont = class(TGfxFont)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  PxxxContextState = ^TxxxContextState;
  TxxxContextState = record
    Prev: PxxxContextState;
    Matrix: TGfxMatrix;
    // xxx Region data etc.
  end;

  TxxxContext = class(TGfxContext)
  private
    FStateStackpointer: PxxxContextState;
    function GetDrawable: TxxxDrawable;
  public
    constructor Create(ADrawable: TxxxDrawable);
    destructor Destroy; override;

    function CreateMemoryDrawable(AWidth, AHeight: Integer;
      const APixelFormat: TGfxPixelFormat;
      const AData: TGfxPixelData): TGfxDrawable; override;
    procedure SaveState; override;
    procedure RestoreState; override;
    function ExcludeClipRect(const ARect: TRect): Boolean; override;
    function IntersectClipRect(const ARect: TRect): Boolean; override;
    function GetClipRect: TRect; override;
    function MapColor(const AColor: TGfxColor): TGfxPixel; override;
    procedure SetColor(AColor: TGfxPixel); override;
    procedure SetFont(AFont: TGfxFont); override;
    procedure SetLineStyle(ALineStyle: TGfxLineStyle); override;

    procedure DrawArc(const Rect: TRect; StartAngle, EndAngle: Single); override;
    procedure DrawCircle(const Rect: TRect); override;
    procedure DrawLine(x1, y1, x2, y2: Integer); override;
    procedure FillRect(const Rect: TRect); override;
    function FontCellHeight: Integer; override;
    function TextWidth(const AText: String): Integer; override;
    procedure TextOut(x, y: Integer; const AText: String); override;

    procedure CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
      DestX, DestY: Integer); override;

    property Drawable: TxxxDrawable read GetDrawable;
  end;


  TxxxDrawable = class(TGfxDrawable)
  private
    FDisplay: TxxxDisplay;
    procedure Resized(NewWidth, NewHeight: Integer);
  public
    constructor Create(ADisplay: TxxxDisplay);	// xxx And other arguments optionally
    function CreateContext: TGfxContext; override;
    property Display: TxxxDisplay read FDisplay;
  end;


  TxxxWindow = class;

  TxxxDisplay = class(TGfxDisplay)
  public
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateWindow: TGfxWindow; override;
    procedure Run; override;
  end;


  TxxxWindow = class(TGfxWindow)
  protected
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
  private
    constructor Create(ADisplay: TxxxDisplay);
  public
    destructor Destroy; override;

    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
    procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;
  end;


// ===================================================================
// ===================================================================

implementation


// -------------------------------------------------------------------
//   TxxxFont
// -------------------------------------------------------------------

constructor TxxxFont.Create;
begin
  inherited Create;
  // !!!: Implement this
end;

destructor TxxxFont.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TxxxContext
// -------------------------------------------------------------------

constructor TxxxContext.Create(ADrawable: TxxxDrawable);
begin
  inherited Create(ADrawable);
  // !!!: Create handle, init context (line attributes, font etc.)
end;

destructor TxxxContext.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;

function TxxxContext.CreateMemoryDrawable(AWidth, AHeight: Integer;
  const APixelFormat: TGfxPixelFormat;
  const AData: TGfxPixelData): TGfxDrawable;
begin
  // !!!: Implement this
  raise EGfxError.Create(SUnsupportedPixelFormat);
end;

procedure TxxxContext.SaveState;
var
  SavedState: PxxxContextState;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  // !!!: Save additional state informations
  FStateStackpointer := SavedState;
end;

procedure TxxxContext.RestoreState;
var
  SavedState: PxxxContextState;
begin
  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;
  // !!!: Restore additional state informations
  Dispose(SavedState);
end;

function TxxxContext.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TxxxContext.IntersectClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TxxxContext.GetClipRect: TRect;
begin
  // !!!: Implement this
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TxxxContext.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
  // !!!: Implement this
  Result := 0;
end;

procedure TxxxContext.SetColor(AColor: TGfxPixel);
begin
  // !!!: Implement this
end;

procedure TxxxContext.SetFont(AFont: TGfxFont);
begin
  // !!!: Implement this
end;

procedure TxxxContext.SetLineStyle(ALineStyle: TGfxLineStyle);
begin
  // !!!: Implement this
end;

procedure TxxxContext.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  // !!!: Implement this
end;

procedure TxxxContext.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  // !!!: Implement this
end;

procedure TxxxContext.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  // !!!: Implement this
end;

procedure TxxxContext.FillRect(const Rect: TRect);
var
  r: TRect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  // !!!: Implement this
end;

function TxxxContext.FontCellHeight: Integer;
begin
  // !!!: Implement this
  Result := 16;
end;

function TxxxContext.TextWidth(const AText: String): Integer;
begin
  // !!!: Implement this
  Result := 16 * Length(AText);
end;

procedure TxxxContext.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  // !!!: Implement this
end;

procedure TxxxContext.CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
  DestX, DestY: Integer);
begin
  Transform(DestX, DestY, DestX, DestY);
  // !!!: Implement this
end;

function TxxxContext.GetDrawable: TxxxDrawable;
begin
  Result := TxxxDrawable(FDrawable);
end;


// -------------------------------------------------------------------
//   TxxxDrawable
// -------------------------------------------------------------------

constructor TxxxDrawable.Create(ADisplay: TxxxDisplay);
begin
  inherited Create(ADisplay);
  FDisplay := ADisplay;
  // !!!: Implement this
end;

function TxxxDrawable.CreateContext: TGfxContext;
begin
  Result := TxxxContext.Create(Self);
end;

procedure TxxxDrawable.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


// -------------------------------------------------------------------
//   TxxxDisplay
// -------------------------------------------------------------------

destructor TxxxDisplay.Destroy;
begin
  // !!!: Implement this
  inherited Destroy;
end;

function TxxxDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TxxxFont.Create;
end;

function TxxxDisplay.CreateWindow: TGfxWindow;
begin
  Result := TxxxWindow.Create(Self);
  // !!!: Implement this
end;

procedure TxxxDisplay.Run;
begin
  // !!!: Implement this
end;


// -------------------------------------------------------------------
//   TxxxWindow
// -------------------------------------------------------------------

function TxxxWindow.GetTitle: String;
begin
  // !!!: Implement this
  Result := inherited;
end;

procedure TxxxWindow.SetTitle(const ATitle: String);
begin
  // !!!: Implement this
end;

constructor TxxxWindow.Create(ADisplay: TxxxDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  // !!!: Implement this
  FDrawable := TxxxDrawable.Create(ADisplay); // !!!: Create a suitable drawable
end;

destructor TxxxWindow.Destroy;
begin
  if Assigned(OnClose) then
    OnClose(Self);

  Drawable.Free;

  // !!!: Clean up

  inherited Destroy;
end;

procedure TxxxWindow.SetSize(AWidth, AHeight: Integer);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.Show;
begin
  // !!!: Implement this
end;

procedure TxxxWindow.Invalidate(const ARect: TRect);
begin
  // !!!: Implement this
end;

procedure TxxxWindow.CaptureMouse;
begin
  // !!!: Implement this
end;

procedure TxxxWindow.ReleaseMouse;
begin
  // !!!: Implement this
end;


end.


{
  $Log$
  Revision 1.1  2000/08/04 21:05:53  sg
  * First version in CVS

}
