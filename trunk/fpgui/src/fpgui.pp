{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    fpGUI master file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit fpGUI;

{ $DEFINE LAYOUTTRACES}
{ $DEFINE TRACEEVENTS}

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses
  SysUtils, Classes, DOM,
  GFXBase, GFXImpl;

type
  TColor = type LongWord;

const
  {COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_ENDCOLORS = COLOR_GRADIENTINACTIVECAPTION;
  COLOR_3DFACE = COLOR_BTNFACE;
  COLOR_3DSHADOW = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT;}

  clScrollBar = TColor($80000000);
  clMenu = TColor($80000004);
  clWindow = TColor($80000005);
  clMenuText = TColor($80000007);
  clWindowText = TColor($80000008);
  clAppWorkSpace = TColor($8000000c);
  clHighlight = TColor($8000000d);
  clHighlightText = TColor($8000000e);
  cl3DFace = TColor($8000000f);
  cl3DShadow = TColor($80000010);
  clGrayText = TColor($80000011);
  clBtnText = TColor($80000012);
  cl3DHighlight = TColor($80000014);
  cl3DDkShadow = TColor($80000015);
  cl3DLight = TColor($80000016);
  clInfoText = TColor($80000017);
  clInfoBk = TColor($80000018);


resourcestring
  mbText_Yes = 'Yes';
  mbText_No = 'No';
  mbText_Ok = 'Ok';
  mbText_Cancel = 'Cancel';
  mbText_Apply = 'Apply';
  mbText_Abort = 'Abort';
  mbText_Retry = 'Retry';
  mbText_Ignore = 'Ignore';
  mbText_All = 'All';
  mbText_NoToAll = 'No to all';
  mbText_YesToAll = 'Yes to all';
  mbText_Help = 'Help';


type

// Types

  TWidget = class;
  TEventObj = class;
  TCustomForm = class;

  TWidgetState = set of (
    wsEnabled,		// Is the widget currently enabled, or is it deactivated
    wsGetsVisible,	// Widget will be visible soon
    wsIsVisible,	// Widget and parents are really visible now
    wsIsUpdating,	// The widget is currently updating itself
    wsHasFocus,		// Widget has the input focus
    wsMouseInside,	// Mouse cursor is inside this widget
    wsClicked);		// User currently clicks on this widget


  TOrientation = (Horizontal, Vertical);


  // The following flags are used for styles

  TButtonFlags = set of (btnIsDefault, btnIsPressed,
    btnIsSelected, btnHasFocus);

  TCheckboxFlags = set of (cbIsPressed, cbHasFocus, cbIsEnabled, cbIsChecked);


  // Other stuff

  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbApply, mbAbort, mbRetry,
    mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;


{$INCLUDE styleh.inc}
{$INCLUDE widgeth.inc}
{$INCLUDE containerh.inc}
{$INCLUDE binh.inc}
{$INCLUDE layoutsh.inc}
{$INCLUDE labelh.inc}
{$INCLUDE edith.inc}
{$INCLUDE buttonsh.inc}
{$INCLUDE scrollbarh.inc}
{$INCLUDE scrollboxh.inc}
{$INCLUDE checkboxh.inc}
{$INCLUDE radiobuttonh.inc}
{$INCLUDE separatorh.inc}
{$INCLUDE groupboxh.inc}
{$INCLUDE listboxh.inc}
{$INCLUDE comboboxh.inc}
{$INCLUDE gridh.inc}
{$INCLUDE formh.inc}
{$INCLUDE dialogsh.inc}
{$INCLUDE applicationh.inc}


var
  Application: TApplication;


function Min(a, b: Integer): Integer;
function Max(a, b: Integer): Integer;
function ClipMinMax(val, min, max: Integer): Integer;


// ===================================================================
// ===================================================================

implementation

uses XMLRead {!!!:, XMLStreaming};

const
  InfiniteSize = 16383;

  // !!!: Only here until FCL has been switched to Resourcestrings
  SListIndexError = 'List index exceeds bounds (%d)';


{$IFDEF TraceEvents}
var
  EventNestingLevel: Integer;
{$ENDIF}


function Min(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function ClipMinMax(val, min, max: Integer): Integer;
begin
  if val < min then
    Result := min
  else if val > max then
  begin
    Result := max;
    if Result < min then
      Result := min;
  end else
    Result := val;
end;


{$IFDEF LAYOUTTRACES}
procedure LAYOUTTRACE(const Position: String; const args: array of const);
{$IFDEF TraceEvents}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF TraceEvents}
  for i := 1 to EventNestingLevel do
    Write('  ');
  {$ENDIF}
  WriteLn(Format(Position, args));
end;
{$ELSE}
procedure LAYOUTTRACE(const Position: String; const args: array of const);
begin
end;
{$ENDIF}


{$INCLUDE style.inc}
{$INCLUDE widget.inc}
{$INCLUDE container.inc}
{$INCLUDE bin.inc}
{$INCLUDE layouts.inc}
{$INCLUDE label.inc}
{$INCLUDE edit.inc}
{$INCLUDE buttons.inc}
{$INCLUDE scrollbar.inc}
{$INCLUDE scrollbox.inc}
{$INCLUDE checkbox.inc}
{$INCLUDE radiobutton.inc}
{$INCLUDE separator.inc}
{$INCLUDE groupbox.inc}
{$INCLUDE listbox.inc}
{$INCLUDE combobox.inc}
{$INCLUDE grid.inc}
{$INCLUDE form.inc}
{$INCLUDE dialogs.inc}
{$INCLUDE application.inc}


const
  Orientations: array[TOrientation] of TIdentMapEntry = (
    (Value: Ord(Horizontal); Name: 'Horizontal'),
    (Value: Ord(Vertical); Name: 'Vertical'));

function IdentToOrientation(const Ident: String; var Orientation: LongInt): Boolean;
begin
  Result := IdentToInt(Ident, Orientation, Orientations);
end;

function OrientationToIdent(Orientation: LongInt; var Ident: String): Boolean;
begin
  Result := IntToIdent(Orientation, Ident, Orientations);
end;


initialization
  RegisterIntegerConsts(TypeInfo(TOrientation),
    @IdentToOrientation, @OrientationToIdent);

  Application := TApplication.Create;

finalization
  Application.Free;
end.


{
  $Log$
  Revision 1.2  2000/12/24 13:18:12  sg
  * Some small fixes (improved speed, and better scrollbar handling where
    appropiate)

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
