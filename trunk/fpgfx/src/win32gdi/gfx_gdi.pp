{
    $Id$

    fpGFX  -  Free Pascal Graphics Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Win32 GDI target implementation

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit GFX_GDI;

interface

uses
  Windows,
  SysUtils, Classes,	// FPC units
  GfxBase;		// fpGUI units

type

  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
	wParam: WPARAM;
	lParam: LPARAM;
	Result: LRESULT);
      1: (
	wParamLo, wParamHi: Word;
	lParamLo, lParamHi: Word;
	ResultLo, ResultHi: Word);
  end;


  EGDIError = class(EGfxError);

  TGDIDrawable = class;
  TGDIDisplay = class;

  TGDIFont = class(TGfxFont)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  PGDIDrawableState = ^TGDIDrawableState;
  TGDIDrawableState = record
    Prev: PGDIDrawableState;
    Matrix: TGfxMatrix;
    Color: TGfxPixel;
  end;

  TGDIDrawable = class(TGfxDrawable)
  private
    FHandle: HDC;
    FColor, FBrushColor, FPenColor, FFontColor: TGfxPixel;
    FBrush, FOldBrush: HBRUSH;
    FPen, FOldPen: HPEN;
    FFontMetrics: TTextMetric;
    FStateStackpointer: PGDIDrawableState;
    procedure Resized(NewWidth, NewHeight: Integer);
  protected
    procedure NeedBrush;
    procedure NeedPen;
    procedure NeedFont(NeedFontColor: Boolean);
  public
    constructor Create(AHandle: HDC);
    destructor Destroy; override;

    function CreateMemoryDrawable(AWidth, AHeight: Integer;
      const APixelFormat: TGfxPixelFormat;
      AStride: LongWord; AData: Pointer): TGfxDrawable; override;

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
    function TextExtent(const AText: String): TSize; override;
    procedure TextOut(x, y: Integer; const AText: String); override;

    procedure CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
      ADestX, ADestY: Integer); override;

    property Handle: HDC read FHandle;
  end;

  TGDIWindowDrawable = class(TGDIDrawable)
  private
    FWnd: HWND;
  public
    constructor Create(AWnd: HWND);
    destructor Destroy; override;
  end;


  TGDIWindow = class;

  TGDIDisplay = class(TGfxDisplay)
  private
    FWindows: TList;
  public
    destructor Destroy; override;
    function CreateFont(const Descriptor: String): TGfxFont; override;
    function CreateWindow: TGfxWindow; override;
    procedure Run; override;
  end;


  TGDIWindow = class(TGfxWindow)
  private
    FHandle: HWND;
    FMinWidth, FMinHeight, FMaxWidth, FMaxHeight: Integer;
    // Messages:
    procedure WMCreate(var Msg: TMessage); message WM_CREATE;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure WMGetMinMaxInfo(var Msg: TMessage); message WM_GETMINMAXINFO;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    // Input messages:
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
  protected
    WindowClass: TWndClass; {!!!: static;}
    function GetTitle: String; override;
    procedure SetTitle(const ATitle: String); override;
  private
    constructor Create(ADisplay: TGDIDisplay);
  public
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;

    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure SetMinMaxSize(AMinWidth, AMinHeight,
      AMaxWidth, AMaxHeight: Integer); override;
    procedure Show; override;
    procedure Invalidate(const ARect: TRect); override;
    procedure CaptureMouse; override;
    procedure ReleaseMouse; override;

    property Handle: HWND read FHandle;
  end;


// ===================================================================
// ===================================================================

implementation

// -------------------------------------------------------------------
//   TGDIFont
// -------------------------------------------------------------------

constructor TGDIFont.Create;
begin
  inherited Create;
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIFont.Create');
end;

destructor TGDIFont.Destroy;
begin
  WriteLn('Not implemented yet: TGDIFont.Destroy');
  // !!!: Implement this
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TGDIDrawable
// -------------------------------------------------------------------

constructor TGDIDrawable.Create(AHandle: HDC);
begin
  inherited Create;
  FHandle := AHandle;
  ASSERT(FHandle <> 0);
  Windows.SelectObject(Handle, Windows.GetStockObject(DEFAULT_GUI_FONT));
  Windows.GetTextMetrics(Handle, @FFontMetrics);
  SetBkMode(Handle, TRANSPARENT);
end;

destructor TGDIDrawable.Destroy;
begin
  if FBrush <> 0 then
  begin
    SelectObject(Handle, FOldBrush);
    DeleteObject(FBrush);
  end;
  if FPen <> 0 then
  begin
    SelectObject(Handle, FOldPen);
    DeleteObject(FPen);
  end;
  inherited Destroy;
end;

function TGDIDrawable.CreateMemoryDrawable(AWidth, AHeight: Integer;
  const APixelFormat: TGfxPixelFormat;
  AStride: LongWord; AData: Pointer): TGfxDrawable;
begin
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIDrawable.CreateMemoryDrawable');
  raise EGfxError.Create(SUnsupportedPixelFormat);
  Result := nil;
end;

procedure TGDIDrawable.SaveState;
var
  SavedState: PGDIDrawableState;
begin
  New(SavedState);
  SavedState^.Prev := FStateStackpointer;
  SavedState^.Matrix := Matrix;
  SavedState^.Color := FColor;
  // !!!: Save additional state informations
  FStateStackpointer := SavedState;
end;

procedure TGDIDrawable.RestoreState;
var
  SavedState: PGDIDrawableState;
begin
  SavedState := FStateStackpointer;
  FStateStackpointer := SavedState^.Prev;
  Matrix := SavedState^.Matrix;
  FColor := SavedState^.Color;
  // !!!: Restore additional state informations
  Dispose(SavedState);
end;

function TGDIDrawable.ExcludeClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
  WriteLn('Not implemented yet: TGDIDrawable.ExcludeClipRect');
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TGDIDrawable.IntersectClipRect(const ARect: TRect): Boolean;
var
  x1, y1, x2, y2: Integer;
begin
  Transform(ARect.Left, ARect.Top, x1, y1);
  Transform(ARect.Right, ARect.Bottom, x2, y2);

  if (x2 > x1) and (y2 > y1) then
  begin
  WriteLn('Not implemented yet: TGDIDrawable.IntersectClipRect');
    // !!!: Implement this
    Result := True; // !!!: Return False if region is empty
  end else
    Result := False;
end;

function TGDIDrawable.GetClipRect: TRect;
begin
  WriteLn('Not implemented yet: TGDIDrawable.GetClipRect:');
  // !!!: Implement this
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TGDIDrawable.MapColor(const AColor: TGfxColor): TGfxPixel;
begin
  Result := Windows.GetNearestColor(Handle, RGB(AColor.Red div 257,
    AColor.Green div 257, AColor.Blue div 257));
end;

procedure TGDIDrawable.SetColor(AColor: TGfxPixel);
begin
  FColor := AColor;
end;

procedure TGDIDrawable.SetFont(AFont: TGfxFont);
begin
  WriteLn('Not implemented yet: TGDIDrawable.SetFont');
  // !!!: Implement this
end;

procedure TGDIDrawable.SetLineStyle(ALineStyle: TGfxLineStyle);
begin
  WriteLn('Not implemented yet: TGDIDrawable.SetLineStyle');
  // !!!: Implement this
end;

procedure TGDIDrawable.DrawArc(const Rect: TRect; StartAngle, EndAngle: Single);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  WriteLn('Not implemented yet: TGDIDrawable.DrawArc');
  // !!!: Implement this
end;

procedure TGDIDrawable.DrawCircle(const Rect: TRect);
var
  x1, y1, x2, y2: Integer;
begin
  Transform(Rect.Left, Rect.Top, x1, y1);
  Transform(Rect.Right, Rect.Bottom, x2, y2);
  WriteLn('Not implemented yet: TGDIDrawable.DrawCircle');
  // !!!: Implement this
end;

procedure TGDIDrawable.DrawLine(x1, y1, x2, y2: Integer);
begin
  Transform(x1, y1, x1, y1);
  Transform(x2, y2, x2, y2);
  NeedPen;
  Windows.MoveToEx(Handle, x1, y1, nil);
  Windows.LineTo(Handle, x2, y2);
end;

procedure TGDIDrawable.FillRect(const Rect: TRect);
var
  r: Windows.Rect;
begin
  Transform(Rect.Left, Rect.Top, r.Left, r.Top);
  Transform(Rect.Right, Rect.Bottom, r.Right, r.Bottom);
  NeedBrush;
  Windows.FillRect(Handle, r, FBrush);
end;

function TGDIDrawable.FontCellHeight: Integer;
begin
  Result := FFontMetrics.tmHeight;
end;

function TGDIDrawable.TextExtent(const AText: String): TSize;
begin
  Windows.GetTextExtentPoint32(Handle, PChar(AText), Length(AText), @Result);
end;

procedure TGDIDrawable.TextOut(x, y: Integer; const AText: String);
begin
  Transform(x, y, x, y);
  NeedFont(True);
  Windows.TextOut(Handle, x, y, PChar(AText), Length(AText));
end;

procedure TGDIDrawable.CopyRect(ASource: TGfxDrawable; const ASourceRect: TRect;
  ADestX, ADestY: Integer);
begin
  Transform(DestX, DestY, DestX, DestY);
  WriteLn('Not implemented yet: TGDIDrawable.CopyRect');
  // !!!: Implement this
end;

procedure TGDIDrawable.NeedBrush;
begin
  if (FBrush = 0) or (FBrushColor <> FColor) then
  begin
    if FBrush <> 0 then
    begin
      SelectObject(Handle, FOldBrush);
      DeleteObject(FBrush);
    end;
    FBrushColor := FColor;
    FBrush := CreateSolidBrush(FBrushColor);
    FOldBrush := SelectObject(Handle, FBrush);
  end;
end;

procedure TGDIDrawable.NeedPen;
begin
  if (FPen = 0) or (FPenColor <> FColor) then
  begin
    if FPen <> 0 then
    begin
      SelectObject(Handle, FOldPen);
      DeleteObject(FPen);
    end;
    FPenColor := FColor;
    FPen := CreatePen(PS_SOLID, 0, FPenColor);
    FOldPen := SelectObject(Handle, FPen);
  end;
end;

procedure TGDIDrawable.NeedFont(NeedFontColor: Boolean);
begin
  if NeedFontColor and (FFontColor <> FColor) then
  begin
    FFontColor := FColor;
    SetTextColor(Handle, FFontColor);
  end;
end;

procedure TGDIDrawable.Resized(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;


// -------------------------------------------------------------------
//   TGDIWindowDrawable
// -------------------------------------------------------------------

constructor TGDIWindowDrawable.Create(AWnd: HWND);
begin
  FWnd := AWnd;
  inherited Create(GetDC(FWnd));
end;

destructor TGDIWindowDrawable.Destroy;
begin
  inherited Destroy;
  if Handle <> 0 then
    ReleaseDC(FWnd, Handle);
end;


// -------------------------------------------------------------------
//   TGDIDisplay
// -------------------------------------------------------------------

destructor TGDIDisplay.Destroy;
var
  i: Integer;
begin
  if Assigned(FWindows) then
  begin
    for i := 0 to FWindows.Count - 1 do
      TGDIWindow(FWindows[i]).Free;
    FWindows.Free;
  end;
  inherited Destroy;
end;

function TGDIDisplay.CreateFont(const Descriptor: String): TGfxFont;
begin
  Result := TGDIFont.Create;
end;

function TGDIDisplay.CreateWindow: TGfxWindow;
begin
  Result := TGDIWindow.Create(Self);
  if not Assigned(FWindows) then
    FWindows := TList.Create;
  FWindows.Add(Result);
end;

procedure TGDIDisplay.Run;
var
  Msg: TMsg;
begin
  if not Assigned(FWindows) then
    exit;

  while GetMessage(@Msg, 0, 0, 0) do
  begin
    TranslateMessage(@msg);
    DispatchMessage(@msg);
  end;
end;


// -------------------------------------------------------------------
//   TGDIWindow
// -------------------------------------------------------------------

function fpGFXWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  Window: TGDIWindow;
  Msg: TMessage;
begin
  if uMsg = WM_CREATE then
  begin
    Window := TGDIWindow(PCreateStruct(lParam)^.lpCreateParams);
    Window.FHandle := hwnd;
    SetWindowLong(hwnd, GWL_USERDATA, LongWord(Window));
  end else
    Window := TGDIWindow(GetWindowLong(hwnd, GWL_USERDATA));

  if Assigned(Window) then
  begin
    Msg.msg := uMsg;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := 0;
    Window.Dispatch(Msg);
    Result := Msg.Result;
  end else
    Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
end;

function TGDIWindow.GetTitle: String;
var
  l: Integer;
begin
  l := GetWindowTextLength(Handle);
  SetLength(Result, l);
  GetWindowText(Handle, @Result[1], l);
end;

procedure TGDIWindow.SetTitle(const ATitle: String);
begin
  SetWindowText(Handle, PChar(ATitle));
end;

constructor TGDIWindow.Create(ADisplay: TGDIDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;

  // Initialize a window class, if necessary
  if not Assigned(WindowClass.lpfnWndProc) then
  begin
    with WindowClass do
    begin
      style := CS_DBLCLKS or CS_HREDRAW or
 CS_VREDRAW;
      lpfnWndProc := WndProc(@fpGFXWindowProc);
      hInstance := MainInstance;
      hIcon := LoadIcon(0, IDI_APPLICATION);
      hCursor := LoadCursor(0, IDC_ARROW);
      hbrBackground := 0;
      lpszClassName := 'fpGFX';
    end;
    Windows.RegisterClass(@WindowClass);
  end;

  FHandle := CreateWindowEx(
    WS_EX_APPWINDOW,			// extended window style
    'fpGFX',				// registered class name
    'fpGFX Window',			// window name
    WS_OVERLAPPEDWINDOW,		// window style
    CW_USEDEFAULT,			// horizontal position of window
    CW_USEDEFAULT,			// vertical position of window
    CW_USEDEFAULT,			// window width
    CW_USEDEFAULT,			// window height
    0,					// handle to parent or owner window
    0,					// menu handle or child identifier
    MainInstance,			// handle to application instance
    Self);				// window-creation data

  FDrawable := TGDIWindowDrawable.Create(Handle);
end;

destructor TGDIWindow.Destroy;
begin
  inherited Destroy;

  if Assigned(OnClose) then
    OnClose(Self);

  if Handle <> 0 then
    DestroyWindow(Handle);
  Drawable.Free;
  TGDIDisplay(Display).FWindows.Remove(Self);

  inherited Destroy;
end;

procedure TGDIWindow.DefaultHandler(var Message);
begin
  TMessage(Message).Result := DefWindowProc(Handle, TMessage(Message).Msg,
    TMessage(Message).wParam, TMessage(Message).lParam);
end;

procedure TGDIWindow.SetSize(AWidth, AHeight: Integer);
begin
  SetWindowPos(Handle, 0, 0, 0, AWidth, AHeight, SWP_NOMOVE or SWP_NOZORDER);
end;

procedure TGDIWindow.SetMinMaxSize(AMinWidth, AMinHeight,
  AMaxWidth, AMaxHeight: Integer);
begin
  FMinWidth := AMinWidth;
  FMinHeight := AMinHeight;
  FMaxWidth := AMaxWidth;
  FMaxHeight := AMaxHeight;
end;

procedure TGDIWindow.Show;
begin
  ShowWindow(Handle, SW_SHOWNORMAL);
  UpdateWindow(Handle);
end;

procedure TGDIWindow.Invalidate(const ARect: TRect);
begin
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIWindow.Invalidate');
end;

procedure TGDIWindow.CaptureMouse;
begin
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIWindow.CaptureMouse');
end;

procedure TGDIWindow.ReleaseMouse;
begin
  // !!!: Implement this
  WriteLn('Not implemented yet: TGDIWindow.ReleaseMouse');
end;


// Message handlers

procedure TGDIWindow.WMCreate(var Msg: TMessage);
begin
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

procedure TGDIWindow.WMDestroy(var Msg: TMessage);
begin
  // Are we the last window for our owning display?
  if TGDIDisplay(Display).FWindows.Count = 1 then
    PostQuitMessage(0);

  FHandle := 0;
  Self.Free;
end;

procedure TGDIWindow.WMGetMinMaxInfo(var Msg: TMessage);
begin
  with PMinMaxInfo(Msg.lParam)^ do
  begin
    if FMinWidth > 0 then
      ptMinTrackSize.x := FMinWidth;
    if FMinHeight > 0 then
      ptMinTrackSize.y := FMinHeight;
    if FMaxWidth > 0 then
      ptMaxTrackSize.x := FMaxWidth;
    if FMaxHeight > 0 then
      ptMaxTrackSize.y := FMaxHeight;
  end;
end;

procedure TGDIWindow.WMPaint(var Msg: TMessage);
var
  PaintStruct: TPaintStruct;
  r: TRect;
begin
  BeginPaint(Handle, @PaintStruct);
  if Assigned(OnPaint) then
  begin
    with PaintStruct.rcPaint do
    begin
      r.Left := Left;
      r.Top := Top;
      r.Right := Right;
      r.Bottom := Bottom;
    end;
    OnPaint(Self, r);
  end;
  EndPaint(Handle, @PaintStruct);
end;

procedure TGDIWindow.WMSize(var Msg: TMessage);
var
  r: Windows.Rect;
begin
  if (FWidth <> Msg.lParamLo) or (FHeight <> Msg.lParamHi) then
  begin
    GetClientRect(Handle, r);
{    FWidth := Msg.lParamLo;
    FHeight := Msg.lParamHi;}
    FWidth := r.Right;
    FHeight := r.Bottom;
    WriteLn('Neue Größe: ', FWidth, ' x ', FHeight);
    TGDIDrawable(Drawable).Resized(FWidth, FHeight);
    if Assigned(OnResize) then
      OnResize(Self);
  end;
end;

procedure TGDIWindow.WMLButtonDown(var Msg: TMessage);
begin
  if Assigned(OnMousePressed) then
    OnMousePressed(Self, mbLeft, [], Msg.lParamLo, Msg.lParamHi);
end;

procedure TGDIWindow.WMLButtonUp(var Msg: TMessage);
begin
  if Assigned(OnMouseReleased) then
    OnMouseReleased(Self, mbLeft, [], Msg.lParamLo, Msg.lParamHi);
end;


end.


{
  $Log$
  Revision 1.2  2000/12/23 23:07:24  sg
  *** empty log message ***

  Revision 1.1  2000/10/28 20:30:50  sg
  * First version (NOT compilable at the moment, as the sources haven't been
    adapted to recent interfaces changes yet!)

}
