(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *   Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
 *     Initially adapted for use with Lazarus and FPC - 12-6-2003
 *
 * ***** END LICENSE BLOCK ***** *)

 (* Changes

 Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
   Initially adapted for use with Lazarus and FPC - 12-6-2003
   All changes can be found by searching for: ////TL
   Unresolved issues/problems/needed checks can be found by searching for the
   above change string and 1 or more ! suffixes. The more ! the worse the issue.
   17-6-2003 status: compiled ok but has serious FPC asm parameter passing issues
   ////TL! This unit has FPC asm issues
 *)

{*********************************************************}
{* TurboPower SysTools for Kylix: StStr.pas 1.01         *}
{*********************************************************}
{* SysTools: String Handling Routines                    *}
{*********************************************************}

{$I stdefine.inc}

unit ststr;

interface

uses
  Classes,
  SysUtils,
  StConst,
  StBase;

type
  LStrRec = record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;

const
  StrLOffset = SizeOf(LStrRec);


type
  WStrRec = packed record
    Length    : Longint;
  end;

////TL: No WideString support in FPC... added in the meantime
WideString = AnsiString; ////////TL
WideChar = char; ////////TL

const
  StrWOffset = SizeOf(WStrRec);



  {-------- Numeric conversion -----------}

{-Return the hex string for a byte.}
function HexBL(B : Byte) : AnsiString;
function HexBS(B : Byte) : ShortString;
function HexBW(B : Byte) : WideString;
function HexBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;

{-Return the hex string for a word.}
function HexWL(W : Word) : AnsiString;
function HexWS(W : Word) : ShortString;
function HexWW(W : Word) : WideString;
function HexWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;

{-Return the hex string for a long integer.}
function HexLL(L : LongInt) : AnsiString;
function HexLS(L : LongInt) : ShortString;
function HexLW(L : LongInt) : WideString;
function HexLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;

{-Return the hex string for a pointer.}
function HexPtrL(P : Pointer) : AnsiString;
function HexPtrS(P : Pointer) : ShortString;
function HexPtrW(P : Pointer) : WideString;
function HexPtrZ(Dest : PAnsiChar; P : Pointer) : PAnsiChar;

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}
function BinaryBS(B : Byte) : ShortString;
  {-Return a binary string for a byte.}
function BinaryBW(B : Byte) : WideString;
  {-Return a binary string for a byte.}
function BinaryBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return a binary string for a byte.}

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}
function BinaryWS(W : Word) : ShortString;
  {-Return the binary string for a word.}
function BinaryWW(W : Word) : WideString;
  {-Return the binary string for a word.}
function BinaryWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return the binary string for a word.}

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}
function BinaryLS(L : LongInt) : ShortString;
  {-Return the binary string for a long integer.}
function BinaryLW(L : LongInt) : WideString;
  {-Return the binary string for a long integer.}
function BinaryLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Return the binary string for a long integer.}


function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}
function OctalBS(B : Byte) : ShortString;
  {-Return an octal string for a byte.}
function OctalBW(B : Byte) : WideString;
  {-Return an octal string for a byte.}
function OctalBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return an octal string for a byte.}


function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}
function OctalWS(W : Word) : ShortString;
  {-Return an octal string for a word.}
function OctalWW(W : Word) : WideString;
  {-Return an octal string for a word.}
function OctalWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return an octal string for a word.}


function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}
function OctalLS(L : LongInt) : ShortString;
  {-Return an octal string for a long integer.}
function OctalLW(L : LongInt) : WideString;
  {-Return an octal string for a long integer.}
function OctalLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Return an octal string for a long integer.}

function Str2Int16L(const S : AnsiString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}
function Str2Int16S(const S : ShortString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}
function Str2Int16W(const S : WideString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}
function Str2Int16Z(S : PAnsiChar; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordL(const S : AnsiString; var I : Word) : Boolean;
  {-Convert a string to a word.}
function Str2WordZ(S : PAnsiChar; var I : Word) : Boolean;
  {-Convert a string to a word.}
function Str2WordW(const S : WideString; var I : Word) : Boolean;
  {-Convert a string to a word.}
function Str2WordS(const S : ShortString; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongL(const S : AnsiString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}
function Str2LongZ(S : PAnsiChar; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}
function Str2LongW(const S : WideString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}
function Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

function Str2RealL(const S : AnsiString; var R : Double) : Boolean;
function Str2RealZ(S : PAnsiChar; var R : Double) : Boolean;
  {-Convert a string to a real.}
function Str2RealW(const S : WideString; var R : Double) : Boolean;
  {-Convert a string to a real.}
function Str2RealS(const S : ShortString; var R : Double) : Boolean;
  {-Convert a string to a real.}
  {-Convert a string to a real.}

function Str2ExtL(const S : AnsiString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
function Str2ExtZ(S : PAnsiChar; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
function Str2ExtW(const S : WideString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
function Str2ExtS(const S : ShortString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrL(L : LongInt) : AnsiString;
  {-Convert an integer type to a string.}
function Long2StrZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Convert an integer type to a string.}
function Long2StrW(L : LongInt) : WideString;
  {-Convert an integer type to a string.}
function Long2StrS(L : LongInt) : ShortString;
  {-Convert an integer type to a string.}

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert a real to a string.}
function Real2StrZ(Dest : PAnsiChar; R : Double; Width : Byte;
                  Places : ShortInt) : PAnsiChar;
function Real2StrW(R : Double; Width : Byte; Places : ShortInt) : WideString;
  {-Convert a real to a string.}
function Real2StrS(R : Double; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert a real to a string.}

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert an extended to a string.}
function Ext2StrZ(Dest : PAnsiChar; R : Extended; Width : Byte;
                 Places : ShortInt) : PAnsiChar;
function Ext2StrW(R : Extended; Width : Byte; Places : ShortInt) : WideString;
  {-Convert an extended to a string.}
function Ext2StrS(R : Extended; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert an extended to a string.}

function ValPrepL(const S : AnsiString) : AnsiString;
  {-Prepares a string for calling Val.}
function ValPrepZ(S : PAnsiChar) : PAnsiChar;
  {-Prepares a string for calling Val.}
function ValPrepW(const S : WideString) : WideString;
  {-Prepares a string for calling Val.}
function ValPrepS(const S : ShortString) : ShortString;
  {-Prepares a string for calling Val.}


  {-------- General purpose string manipulation --------}

function PadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with a specified character.
    This primitive version modifies the source string directly.}

function PadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with spaces. This primitive version modifies the
    source string directly.}

function LeftPadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with a specified character. This primitive
    version modifies the source string directly.}

function LeftPadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with spaces. This primitive version modifies the
    source string directly.}

function TrimLeadPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed. This primitive version
    modifies the source string directly.}

function TrimTrailPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed. This primitive version
    modifies the source string directly.}

function TrimPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed. This
    primitive version modifies the source string directly.}

function TrimSpacesPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed. This primitive
    version modifies the source string directly.}

function CenterChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with a specified character. This
    primitive version modifies the source string directly.}

function CenterPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with spaces. This primitive version
    modifies the source string directly.}

function ScramblePrimZ(S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption. This
    primitive version modifies the source string directly.}

function CharStrL(C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Return a string filled with the specified character.}
function CharStrZ(Dest : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string filled with the specified character.}
function CharStrW(C : WideChar; Len : Cardinal) : WideString;
  {-Return a string filled with the specified character.}
function CharStrS(C : AnsiChar; Len : Cardinal) : ShortString;
  {-Return a string filled with the specified character.}

function PadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with a specified character.}
function PadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with a specified character.}
function PadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the right with a specified character.}
function PadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the right with a specified character.}

function PadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with spaces.}
function PadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with spaces.}
function PadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the right with spaces.}
function PadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the right with spaces.}

function LeftPadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with a specified character.}
function LeftPadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with a specified character.}
function LeftPadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left with a specified character.}
function LeftPadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left with a specified character.}

function LeftPadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with spaces.}
function LeftPadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with spaces.}
function LeftPadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left with spaces.}
function LeftPadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left with spaces.}

function TrimLeadL(const S : AnsiString) : AnsiString; overload;
  {-Return a string with leading white space removed.}
function TrimLeadZ(Dest, S : PAnsiChar) : PAnsiChar; overload;
  {-Return a string with leading white space removed.}
function TrimLeadW(const S : WideString) : WideString; overload;
  {-Return a string with leading white space removed.}
function TrimLeadS(const S : ShortString) : ShortString; overload;
  {-Return a string with leading white space removed.}

function TrimTrailL(const S : AnsiString) : AnsiString;
  {-Return a string with trailing white space removed.}
function TrimTrailZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed.}
function TrimTrailW(const S : WideString) : WideString;
  {-Return a string with trailing white space removed.}
function TrimTrailS(const S : ShortString) : ShortString;
  {-Return a string with trailing white space removed.}

function TrimL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing white space removed.}
function TrimZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed.}
function TrimW(const S : WideString) : WideString;
  {-Return a string with leading and trailing white space removed.}
function TrimS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing spaces removed.}
function TrimSpacesZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed.}
function TrimSpacesW(const S : WideString) : WideString;
  {-Return a string with leading and trailing spaces removed.}
function TrimSpacesS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing spaces removed.}

function CenterChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with a specified character.}
function CenterChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with a specified character.}
function CenterChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with a specified character.}
function CenterChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with a specified character.}

function CenterL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with spaces.}
function CenterZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with spaces.}
function CenterW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with spaces.}
function CenterS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with spaces.}

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Convert blanks in a string to tabs.}
function EntabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  {-Convert blanks in a string to tabs.}
function EntabW(const S : WideString; TabSize : Byte) : WideString;
  {-Convert blanks in a string to tabs.}
function EntabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Convert blanks in a string to tabs.}

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Expand tabs in a string to blanks.}
function DetabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  {-Expand tabs in a string to blanks.}
function DetabW(const S : WideString; TabSize : Byte) : WideString;
  {-Expand tabs in a string to blanks.}
function DetabS(const S : ShortString; TabSize : Byte) : ShortString;
  {-Expand tabs in a string to blanks.}

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
function ScrambleZ(Dest, S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
function ScrambleW(const S, Key : WideString) : WideString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
function ScrambleS(const S, Key : ShortString) : ShortString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteL(const S, FromStr, ToStr : AnsiString) : AnsiString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
function SubstituteZ(Dest, Src, FromStr, ToStr : PAnsiChar) : PAnsiChar;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
function SubstituteW(const S, FromStr, ToStr : WideString) : WideString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
function SubstituteS(const S, FromStr, ToStr : ShortString) : ShortString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterL(const S, Filters : AnsiString) : AnsiString;
  {-Remove characters from a string. The characters to remove are specified in ChSet.}
function FilterZ(Dest, Src, Filters : PAnsiChar) : PAnsiChar;
  {-Remove characters from a string. The characters to remove are specified in ChSet.}
function FilterW(const S, Filters : WideString) : WideString;
  {-Remove characters from a string. The characters to remove are specified in ChSet.}
function FilterS(const S, Filters1 : ShortString) : ShortString;
  {-Remove characters from a string. The characters to remove are specified in ChSet.}


  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : AnsiString; C : AnsiChar) : Boolean;
  {-Determine whether a given character exists in a string. }
function CharExistsZ(S : PAnsiChar; C : AnsiChar) : Boolean;
  {-Determine whether the given character exists in a string. }
function CharExistsW(const S : WideString; C : WideChar) : Boolean;
  {-Determine whether a given character exists in a string. }
function CharExistsS(const S : ShortString; C : AnsiChar) : Boolean;
  {-Determines whether a given character exists in a string. }

function CharCountL(const S : AnsiString; C : AnsiChar) : Cardinal;
  {-Count the number of a given character in a string. }
function CharCountZ(S : PAnsiChar; C : AnsiChar) : Cardinal;
  {-Count the number of a given character in a string. }
function CharCountW(const S : WideString; C : WideChar) : Cardinal;
  {-Count the number of a given character in a string. }
function CharCountS(const S : ShortString; C : AnsiChar) : Byte;
  {-Count the number of a given character in a string. }

function WordCountL(const S, WordDelims : AnsiString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
function WordCountZ(S : PAnsiChar; WordDelims : PAnsiChar) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
function WordCountW(const S, WordDelims : WideString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
function WordCountS(const S, WordDelims : ShortString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                      var Pos : Cardinal) : Boolean;
function WordPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                      var Pos : Cardinal) : Boolean;
function WordPositionW(N : Cardinal; const S, WordDelims : WideString;
                      var Pos : Cardinal) : Boolean;
function WordPositionS(N : Cardinal; const S, WordDelims : ShortString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordL(N : Cardinal; const S, WordDelims : AnsiString) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string.}
function ExtractWordZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                     WordDelims : PAnsiChar) : PAnsiChar;
function ExtractWordW(N : Cardinal; const S, WordDelims : WideString) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string.}
function ExtractWordS(N : Cardinal; const S, WordDelims : ShortString) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountL(const S, WordDelims : AnsiString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
function AsciiCountZ(S : PAnsiChar; WordDelims : PAnsiChar; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
function AsciiCountW(const S, WordDelims : WideString; Quote : WideChar) : Cardinal;
  {-Return the number of words in a string.}
function AsciiCountS(const S, WordDelims : ShortString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
function AsciiPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
function AsciiPositionW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar; var Pos : Cardinal) : Boolean;
function AsciiPositionS(N : Cardinal; const S, WordDelims : ShortString;
                        Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar) : AnsiString;
function ExtractAsciiZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                      WordDelims : PAnsiChar; Quote : AnsiChar) : PAnsiChar;
function ExtractAsciiW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar) : WideString;
function ExtractAsciiS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapL(const InSt : AnsiString; var OutSt, Overlap : AnsiString;
                   Margin : Cardinal; PadToMargin : Boolean);
procedure WordWrapZ(Dest : PAnsiChar; InSt, Overlap : PAnsiChar;
                   Margin : Cardinal; PadToMargin : Boolean);
procedure WordWrapW(const InSt : WideString; var OutSt, Overlap : WideString;
                   Margin : Cardinal; PadToMargin : Boolean);
procedure WordWrapS(const InSt : ShortString; var OutSt, Overlap : ShortString;
                    Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : AnsiString) : Integer;
  {-Compare two strings.}
function CompStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Compare two strings.}
function CompStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings.}
function CompStringS(const S1, S2 : ShortString) : Integer;
  {-Compare two strings.}

function CompUCStringL(const S1, S2 : AnsiString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}
function CompUCStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Compare two strings. This compare is not case sensitive.}
function CompUCStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}
function CompUCStringS(const S1, S2 : ShortString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string.}
//function SoundexL(const S : AnsiString) : AnsiString;
//  {-Return 4 character soundex of an input string}
function SoundexZ(Dest : PAnsiChar; S : PAnsiChar) : PAnsiChar;
  {-Return 4 character soundex of an input string}
function SoundexS(const S : ShortString) : ShortString;
  {-Return 4 character soundex of an input string.}

//function MakeLetterSetL(const S : AnsiString) : Longint;
//  {-Return a bit-mapped long storing the individual letters contained in S.}
function MakeLetterSetL(const S : AnsiString) : Longint;
  {-Return a bit-mapped long storing the individual letters contained in S.}
function MakeLetterSetZ(S : PAnsiChar) : Longint;
  {-Return a bit-mapped long storing the individual letters contained in S.}
function MakeLetterSetS(const S : ShortString) : Longint;
  {-Return a bit-mapped long storing the individual letters contained in S.}

//procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable);
//  {-Build a Boyer-Moore link table}
procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable);
  {-Build a Boyer-Moore link table}
procedure BMMakeTableZ(MatchString : PAnsiChar; var BT : BTable);
  {-Build a Boyer-Moore link table}
procedure BMMakeTableS(const MatchString : ShortString; var BT : BTable);
  {-Build a Boyer-Moore link table}

function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
function BMSearchS(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : ShortString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
function BMSearchZ(var Buffer; BufLength : Cardinal; var BT : BTable;
                  MatchString : PAnsiChar ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}

function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : ShortString ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

function BMSearchUCZ(var Buffer; BufLength : Cardinal; var BT : BTable;
                    MatchString : PAnsiChar ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Return a file name with a default extension attached.}
function DefaultExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a file name with a default extension attached.}
function DefaultExtensionW(const Name, Ext : WideString) : WideString;
  {-Return a file name with a default extension attached.}
function DefaultExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Return a file name with a default extension attached.}

function ForceExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Force the specified extension onto the file name.}
function ForceExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Force the specified extension onto the file name.}
function ForceExtensionW(const Name, Ext : WideString) : WideString;
  {-Force the specified extension onto the file name.}
function ForceExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Force the specified extension onto the file name.}

function JustFilenameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename and extension of a pathname.}
function JustFilenameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the filename and extension of a pathname.}
function JustFilenameW(const PathName : WideString) : WideString;
  {-Return just the filename and extension of a pathname.}
function JustFilenameS(const PathName : ShortString) : ShortString;
  {-Return just the filename and extension of a pathname.}

function JustNameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
function JustNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
function JustNameW(const PathName : WideString) : WideString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
function JustNameS(const PathName : ShortString) : ShortString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionL(const Name : AnsiString) : AnsiString;
  {-Return just the extension of a pathname.}
function JustExtensionZ(Dest : PAnsiChar; Name : PAnsiChar) : PAnsiChar;
  {-Return just the extension of a pathname.}
function JustExtensionW(const Name : WideString) : WideString;
  {-Return just the extension of a pathname.}
function JustExtensionS(const Name : ShortString) : ShortString;
  {-Return just the extension of a pathname.}

function JustPathnameL(const PathName : AnsiString) : AnsiString;
  {-Return just the drive and directory portion of a pathname.}
function JustPathnameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the drive and directory portion of a pathname.}
function JustPathnameW(const PathName : WideString) : WideString;
  {-Return just the drive and directory portion of a pathname.}
function JustPathnameS(const PathName : ShortString) : ShortString;
  {-Return just the drive and directory portion of a pathname.}

function AddSlashL(const DirName : AnsiString) : AnsiString;
  {-Add a default slash to a directory name.}
function AddSlashZ(Dest : PAnsiChar; DirName : PAnsiChar) : PAnsiChar;
  {-Add a default slash to a directory name.}
function AddSlashW(const DirName : WideString) : WideString;
  {-Add a default slash to a directory name.}
function AddSlashS(const DirName : ShortString) : ShortString;
  {-Add a default slash to a directory name.}

function HasExtensionL(const Name : AnsiString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
function HasExtensionZ(Name : PAnsiChar; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
function HasExtensionW(const Name : WideString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
function HasExtensionS(const Name : ShortString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}
function StrChInsertPrimZ(Dest : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a character into a string at a specified position. This primitive
    version modifies the source string directly.}

function StrStInsertPrimZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a string into another string at a specified position. This
    primitive version modifies the source string directly.}

function StrChDeletePrimZ(P : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Delete the character at a specified position in a string. This primitive
    version modifies the source string directly.}

function StrStDeletePrimZ(P : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
  {-Delete characters at a specified position in a string. This primitive
    version modifies the source string directly.}

function CommaizeL(L : LongInt) : AnsiString;
  {-Convert a long integer to a string with commas.}
function CommaizeZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Convert a long integer to a string with commas.}
function CommaizeW(L : LongInt) : WideString;
  {-Convert a long integer to a string with commas.}
function CommaizeS(L : LongInt) : ShortString;
  {-Convert a long integer to a string with commas.}

function CommaizeChL(L : Longint; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions.}
function CommaizeChZ(Dest : PAnsiChar; L : Longint; Ch : AnsiChar) : PAnsiChar;
  {-Convert a long integer to a string with Ch in comma positions.}
function CommaizeChW(L : Longint; Ch : WideChar) : WideString;
  {-Convert a long integer to a string with Ch in comma positions.}
function CommaizeChS(L : Longint; Ch : AnsiChar) : ShortString;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormL(const Mask : AnsiString ; R : TstFloat ; const LtCurr,
                    RtCurr : AnsiString ; Sep, DecPt : AnsiChar) : AnsiString;
function FloatFormZ(Dest, Mask : PAnsiChar ; R : TstFloat ; LtCurr,
                    RtCurr : PAnsiChar ; Sep, DecPt : AnsiChar) : PAnsiChar;
function FloatFormW(const Mask : WideString ; R : TstFloat ; const LtCurr,
                    RtCurr : WideString ; Sep, DecPt : WideChar) : WideString;
function FloatFormS(const Mask : ShortString ; R : TstFloat ; const LtCurr,
                    RtCurr : ShortString ; Sep, DecPt : AnsiChar) : ShortString;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormL(const Mask : AnsiString ; L : LongInt ; const LtCurr,
                      RtCurr : AnsiString ; Sep : AnsiChar) : AnsiString;
function LongIntFormZ(Dest, Mask : PAnsiChar ; L : LongInt ; LtCurr,
                     RtCurr : PAnsiChar ; Sep : AnsiChar) : PAnsiChar;
function LongIntFormW(const Mask : WideString ; L : LongInt ; const LtCurr,
                      RtCurr : WideString ; Sep : WideChar) : WideString;
function LongIntFormS(const Mask : ShortString ; L : LongInt ; const LtCurr,
                     RtCurr : ShortString ; Sep : AnsiChar) : ShortString;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
function StrChPosZ(P : PAnsiChar; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
function StrChPosW(const P : WideString; C : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
function StrChPosS(const P : ShortString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}

function StrStPosL(const P, S : AnsiString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
function StrStPosZ(P, S : PAnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
function StrStPosW(const P, S : WideString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
function StrStPosS(const P, S : ShortString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Copy characters at a specified position in a string.}
function StrStCopyZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
  {-Copy characters at a specified position in a string.}
function StrStCopyW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Copy characters at a specified position in a string.}
function StrStCopyS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Copy characters at a specified position in a string.}

function StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
  {-Insert a character into a string at a specified position.}
function StrChInsertZ(Dest, S : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a character into a string at a specified position.}
function StrChInsertW(const S : WideString; C : WideChar; Pos : Cardinal) : WideString;
  {-Insert a character into a string at a specified position.}
function StrChInsertS(const S : ShortString; C : AnsiChar; Pos : Cardinal) : ShortString;
  {-Insert a character into a string at a specified position.}

function StrStInsertL(const S1, S2 : AnsiString; Pos : Cardinal) : AnsiString;
  {-Insert a string into another string at a specified position.}
function StrStInsertZ(Dest, S1, S2 : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a string into another string at a specified position.}
function StrStInsertW(const S1, S2 : WideString; Pos : Cardinal) : WideString;
  {-Insert a string into another string at a specified position.}
function StrStInsertS(const S1, S2 : ShortString; Pos : Cardinal) : ShortString;
  {-Insert a string into another string at a specified position.}

function StrChDeleteL(const S : AnsiString; Pos : Cardinal) : AnsiString;
  {-Delete the character at a specified position in a string.}
function StrChDeleteZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Delete the character at a specified position in a string.}
function StrChDeleteW(const S : WideString; Pos : Cardinal) : WideString;
  {-Delete the character at a specified position in a string.}
function StrChDeleteS(const S : ShortString; Pos : Cardinal) : ShortString;
  {-Delete the character at a specified position in a string.}

function StrStDeleteL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Delete characters at a specified position in a string.}
function StrStDeleteZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
  {-Delete characters at a specified position in a string.}
function StrStDeleteW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Delete characters at a specified position in a string.}
function StrStDeleteS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Delete characters at a specified position in a string.}


{--------------------------  New Functions -----------------------------------}

function ContainsOnlyL(const S, Chars : AnsiString;
                       var BadPos : Cardinal) : Boolean;
function ContainsOnlyZ(const S, Chars : PAnsiChar;
                       var BadPos : Cardinal) : Boolean;
function ContainsOnlyW(const S, Chars : WideString;
                       var BadPos : Cardinal) : Boolean;
function ContainsOnlyS(const S, Chars : ShortString;
                       var BadPos : Cardinal) : Boolean;

function ContainsOtherThanL(const S, Chars : AnsiString;
                            var BadPos : Cardinal) : Boolean;
function ContainsOtherThanZ(const S, Chars : PAnsiChar;
                            var BadPos : Cardinal) : Boolean;
function ContainsOtherThanW(const S, Chars : WideString;
                            var BadPos : Cardinal) : Boolean;
function ContainsOtherThanS(const S, Chars : ShortString;
                            var BadPos : Cardinal) : Boolean;

function CopyLeftL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Return the left Len characters of a string}
function CopyLeftZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return the left Len characters of a string}
function CopyLeftW(const S : WideString; Len : Cardinal) : WideString;
  {-Return the left Len characters of a string}
function CopyLeftS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Return the left Len characters of a string}

function CopyMidL(const S : AnsiString; First, Len : Cardinal) : AnsiString;
  {-Return the mid part of a string}
function CopyMidZ(Dest, S : PAnsiChar; First, Len : Cardinal) : PAnsiChar;
  {-Return the mid part of a string}
function CopyMidW(const S : WideString; First, Len : Cardinal) : WideString;
  {-Return the mid part of a string}
function CopyMidS(const S : ShortString; First, Len : Cardinal) : ShortString;
  {-Return the mid part of a string}

function CopyRightL(const S : AnsiString; First : Cardinal) : AnsiString;
  {-Return the right Len characters of a string}
function CopyRightZ(Dest, S : PAnsiChar; First : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}
function CopyRightW(const S : WideString; First : Cardinal) : WideString;
  {-Return the right Len characters of a string}
function CopyRightS(const S : ShortString; First : Cardinal) : ShortString;
  {-Return the right Len characters of a string}

function CopyRightAbsL(const S : AnsiString; NumChars : Cardinal) : AnsiString;
  {-Return NumChar characters starting from end}
function CopyRightAbsZ(Dest, S : PAnsiChar; NumChars : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}
function CopyRightAbsW(const S : WideString; NumChars : Cardinal) : WideString;
  {-Return NumChar characters starting from end}
function CopyRightAbsS(const S : ShortString; NumChars : Cardinal) : ShortString;
  {-Return NumChar characters starting from end}

function CopyFromNthWordL(const S, WordDelims : AnsiString;
                         AWord : AnsiString; N : Cardinal;
                         var SubString : AnsiString) : Boolean;
function CopyFromNthWordS(const S, WordDelims : ShortString;
                          AWord : ShortString; N : Cardinal;
                          var SubString : ShortString) : Boolean;
function CopyFromNthWordW(const S, WordDelims : WideString;
                         AWord : WideString; N : Cardinal;
                         var SubString : WideString) : Boolean;
function CopyFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                          N : Cardinal) : Boolean;


function CopyFromToWordL(const S, WordDelims, Word1, Word2 : AnsiString;
                         N1, N2 : Cardinal;
                         var SubString : AnsiString) : Boolean;
function CopyFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                         N1, N2 : Cardinal) : Boolean;
function CopyFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                         N1, N2 : Cardinal;
                         var SubString : WideString) : Boolean;
function CopyFromToWordS(const S, WordDelims, Word1, Word2 : ShortString;
                         N1, N2 : Cardinal;
                         var SubString : ShortString) : Boolean;

function CopyWithinL(const S, Delimiter : AnsiString;
                    Strip : Boolean) : AnsiString;
function CopyWithinZ(Dest, S, Delimiter : PAnsiChar;
                     Strip : Boolean) : PAnsiChar;
function CopyWithinW(const S, Delimiter : WideString;
                     Strip : Boolean) : WideString;
function CopyWithinS(const S, Delimiter : ShortString;
                     Strip : Boolean) : ShortString;

function DeleteFromNthWordL(const S, WordDelims : AnsiString;
                            AWord : AnsiString; N : Cardinal;
                            var SubString : AnsiString) : Boolean;
function DeleteFromNthWordS(const S, WordDelims : ShortString;
                            AWord : ShortString; N : Cardinal;
                            var SubString : ShortString) : Boolean;
function DeleteFromNthWordW(const S, WordDelims : WideString;
                            AWord : WideString; N : Cardinal;
                            var SubString : WideString) : Boolean;
function DeleteFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                            N : Cardinal) : Boolean;


function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : AnsiString;
                           N1, N2 : Cardinal;
                           var SubString : AnsiString) : Boolean;
function DeleteFromToWordS(const S, WordDelims, Word1, Word2 : ShortString;
                           N1, N2 : Cardinal;
                           var SubString : ShortString) : Boolean;
function DeleteFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                           N1, N2 : Cardinal;
                           var SubString : WideString) : Boolean;
function DeleteFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                           N1, N2 : Cardinal) : Boolean;

function DeleteWithinL(const S, Delimiter : AnsiString) : AnsiString;
function DeleteWithinZ(Dest, S, Delimiter : PAnsiChar) : PAnsiChar;
function DeleteWithinW(const S, Delimiter : WideString) : WideString;
function DeleteWithinS(const S, Delimiter : ShortString) : ShortString;

function ExtractTokensL(const S, Delims: AnsiString;
                        QuoteChar  : AnsiChar;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;
function ExtractTokensS(const S, Delims: ShortString;
                        QuoteChar  : Char;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;
function ExtractTokensW(const S, Delims: WideString;
                        QuoteChar  : WideChar;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;
function ExtractTokensZ(S, Delims   : PAnsiChar;
                        QuoteChar   : AnsiChar;
                        AllowNulls  : Boolean;
                        Tokens      : TStrings) : Cardinal;

function IsChAlphaL(C : AnsiChar) : Boolean;
 {-Returns true if Ch is an alpha}
function IsChAlphaZ(C : AnsiChar) : Boolean;
 {-Returns true if Ch is an alpha}
function IsChAlphaW(C : WideChar) : Boolean;
 {-Returns true if Ch is an alpha}
function IsChAlphaS(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}

function IsChNumericL(C : AnsiChar; Numbers : AnsiString) : Boolean;
 {-Returns true if Ch in numeric set}
function IsChNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
 {-Returns true if Ch in numeric set}
function IsChNumericW(C : WideChar; Numbers : WideString) : Boolean;
 {-Returns true if Ch in numeric set}
function IsChNumericS(C : Char; Numbers : ShortString) : Boolean;
 {-Returns true if Ch in numeric set}

function IsChAlphaNumericL(C : AnsiChar; Numbers : AnsiString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
function IsChAlphaNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
function IsChAlphaNumericW(C : WideChar; Numbers : WideString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
function IsChAlphaNumericS(C : Char; Numbers : ShortString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}

function IsStrAlphaL(const S : AnsiString) : Boolean;
  {-Returns true if all characters in string are an alpha}
function IsStrAlphaZ(S : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are an alpha}
function IsStrAlphaW(const S : WideString) : Boolean;
  {-Returns true if all characters in string are an alpha}
function IsStrAlphaS(const S : Shortstring) : Boolean;
  {-Returns true if all characters in string are an alpha}

function IsStrNumericL(const S, Numbers : AnsiString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
function IsStrNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are in numeric set}
function IsStrNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
function IsStrNumericS(const S, Numbers : ShortString) : Boolean;
  {-Returns true if all characters in string are in numeric set}

function IsStrAlphaNumericL(const S, Numbers : AnsiString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
function IsStrAlphaNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
function IsStrAlphaNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
function IsStrAlphaNumericS(const S, Numbers : ShortString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}

function KeepCharsL(const S, Chars : AnsiString) : AnsiString;
function KeepCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
function KeepCharsW(const S, Chars : WideString) : WideString;
function KeepCharsS(const S, Chars : ShortString) : ShortString;
  {-returns a string containing only those characters in a given set}

function LastWordL(const S, WordDelims, AWord : AnsiString;
                   var Position : Cardinal) : Boolean;
function LastWordS(const S, WordDelims, AWord : ShortString;
                   var Position : Cardinal) : Boolean;
function LastWordW(const S, WordDelims, AWord : WideString;
                   var Position : Cardinal) : Boolean;
function LastWordZ(S, WordDelims, AWord : PAnsiChar;
                   var Position : Cardinal) : Boolean;
{-returns the position in a string of the last instance of a given word}

function LastWordAbsL(const S, WordDelims : AnsiString;
                        var Position : Cardinal) : Boolean;
function LastWordAbsS(const S, WordDelims : ShortString;
                      var Position : Cardinal) : Boolean;
function LastWordAbsW(const S, WordDelims : WideString;
                        var Position : Cardinal) : Boolean;
function LastWordAbsZ(S, WordDelims : PAnsiChar;
                      var Position : Cardinal) : Boolean;
  {-returns the position in a string of the last word}

function LastStringL(const S, AString : AnsiString;
                     var Position : Cardinal) : Boolean;
function LastStringZ(S, AString : PAnsiChar;
                     var Position : Cardinal) : Boolean;
function LastStringW(const S, AString : WideString;
                     var Position : Cardinal) : Boolean;
function LastStringS(const S, AString : ShortString;
                     var Position : Cardinal) : Boolean;
  {-returns the position in a string of the last instance of a given string}

function LeftTrimCharsL(const S, Chars : AnsiString) : AnsiString;
function LeftTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
function LeftTrimCharsW(const S, Chars : WideString) : WideString;
function LeftTrimCharsS(const S, Chars : ShortString) : ShortString;
  {-strips given characters from the beginning of a string}


function ReplaceWordL(const S, WordDelims, OldWord, NewWord : AnsiString;
                      N : Cardinal;
                      var Replacements : Cardinal) : AnsiString;
function ReplaceWordZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                      N : Cardinal;
                      var Replacements : Cardinal) : PAnsiChar;
function ReplaceWordW(const S, WordDelims, OldWord, NewWord : WideString;
                      N : Cardinal;
                      var Replacements : Cardinal) : WideString;
function ReplaceWordS(const S, WordDelims, OldWord, NewWord : ShortString;
                      N : Cardinal;
                      var Replacements : Cardinal) : ShortString;
  {-replaces a given word with one or more instances of a string}

function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : AnsiString;
                         var Replacements : Cardinal) : AnsiString;
function ReplaceWordAllZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                         var Replacements : Cardinal) : PAnsiChar;
function ReplaceWordAllW(const S, WordDelims, OldWord, NewWord : WideString;
                         var Replacements : Cardinal) : WideString;
function ReplaceWordAllS(const S, WordDelims, OldWord, NewWord : ShortString;
                         var Replacements : Cardinal) : ShortString;

function ReplaceStringL(const S, OldString, NewString : AnsiString;
                        N : Cardinal;
                        var Replacements : Cardinal) : AnsiString;
function ReplaceStringS(const S, OldString, NewString : ShortString;
                        N : Cardinal;
                        var Replacements : Cardinal) : ShortString;
  {-replaces a substring with up to Replacements instances of a string}
function ReplaceStringW(const S, OldString, NewString : WideString;
                        N : Cardinal;
                        var Replacements : Cardinal) : WideString;
function ReplaceStringZ(Dest, S, OldString, NewString : PAnsiChar;
                        N : Cardinal;
                        var Replacements : Cardinal) : PAnsiChar;

function ReplaceStringAllL(const S, OldString, NewString : AnsiString;
                           var Replacements : Cardinal) : AnsiString;
function ReplaceStringAllZ(Dest, S, OldString, NewString : PAnsiChar;
                           var Replacements : Cardinal) : PAnsiChar;
function ReplaceStringAllW(const S, OldString, NewString : WideString;
                           var Replacements : Cardinal) : WideString;
function ReplaceStringAllS(const S, OldString, NewString : ShortString;
                        var Replacements : Cardinal) : ShortString;
  {-replaces all instances of a substring with one or more instances of a string}


function RepeatStringL(const RepeatString : AnsiString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : AnsiString;
function RepeatStringZ(Dest, RepeatString : PAnsiChar;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : PAnsiChar;
function RepeatStringW(const RepeatString : WideString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : WideString;
function RepeatStringS(const RepeatString : ShortString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : ShortString;
  {-creates a string of up to Repetition instances of a string}

function RightTrimCharsL(const S, Chars : AnsiString) : AnsiString;
function RightTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
function RightTrimCharsW(const S, Chars : WideString) : WideString;
function RightTrimCharsS(const S, Chars : ShortString) : ShortString;
  {-removes those characters at the end of a string contained in a set of characters}

function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}
function StrWithinS(const S, SearchStr : ShortString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}
function StrWithinW(const S, SearchStr : WideString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}
function StrWithinZ(S, SearchStr : PAnsiChar;
                    Start        : Cardinal;
                    var Position : Cardinal) : Boolean;

function TrimCharsL(const S, Chars : AnsiString) : AnsiString;
function TrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
function TrimCharsW(const S, Chars : WideString) : WideString;
function TrimCharsS(const S, Chars : ShortString) : ShortString;
  {-removes trailing and leading characters defined by a string from a string}

function WordPosL(const S, WordDelims, AWord : AnsiString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
function WordPosZ(S, WordDelims, AWord : PAnsiChar;
                  N : Cardinal; var Position : Cardinal) : Boolean;
function WordPosW(const S, WordDelims, AWord : WideString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
function WordPosS(const S, WordDelims, AWord : ShortString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a word within a string}

implementation
uses
  stutils;


  {-------- Numeric conversion -----------}

function HexBS(B : Byte) : ShortString;
  {-Return the hex string for a byte.}
begin
  Result[0] := #2;
  Result[1] := StHexDigits[B shr 4];
  Result[2] := StHexDigits[B and $F];
end;

function HexWS(W : Word) : ShortString;
  {-Return the hex string for a word.}
begin
  Result[0] := #4;
  Result[1] := StHexDigits[hi(W) shr 4];
  Result[2] := StHexDigits[hi(W) and $F];
  Result[3] := StHexDigits[lo(W) shr 4];
  Result[4] := StHexDigits[lo(W) and $F];
end;

function HexLS(L : LongInt) : ShortString;
  {-Return the hex string for a long integer.}
begin
  Result := HexWS(HiWord(L)) + HexWS(LoWord(L));
end;

function HexPtrS(P : Pointer) : ShortString;
  {-Return the hex string for a pointer.}
type
  OS = record O, S : word; end;
begin
  Result := ':' + HexLS(LongInt(P));
end;

function BinaryBS(B : Byte) : ShortString;
  {-Return a binary string for a byte.}
var
  I, N : Cardinal;
begin
  N := 1;
  Result[0] := #8;
  for I := 7 downto 0 do begin
    Result[N] := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWS(W : Word) : ShortString;
  {-Return the binary string for a word.}
var
  I, N : Cardinal;
begin
  N := 1;
  Result[0] := #16;
  for I := 15 downto 0 do begin
    Result[N] := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLS(L : LongInt) : ShortString;
  {-Return the binary string for a long integer.}
var
  I : Longint;
  N : Byte;
begin
  N := 1;
  Result[0] := #32;
  for I := 31 downto 0 do begin
    Result[N] := StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBS(B : Byte) : ShortString;
  {-Return an octal string for a byte.}
var
  I : Cardinal;
begin
  Result[0] := #3;
  for I := 0 to 2 do begin
    Result[3-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWS(W : Word) : ShortString;
  {-Return an octal string for a word.}
var
  I : Cardinal;
begin
  Result[0] := #6;
  for I := 0 to 5 do begin
    Result[6-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLS(L : LongInt) : ShortString;
  {-Return an octal string for a long integer.}
var
  I : Cardinal;
begin
  Result[0] := #12;
  for I := 0 to 11 do begin
    Result[12-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
end;

function Str2Int16S(const S : ShortString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

var
  ec : Integer;
begin
  ValSmallint(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2WordS(const S : ShortString; var I : Word) : Boolean;
  {-Convert a string to a word.}

var
  ec : Integer;
begin
  ValWord(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

var
  ec : Integer;
begin
  ValLongint(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2RealS(const S : ShortString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : ShortString;
  SLen : Byte absolute St;
begin
  St := S;
  {trim trailing blanks}
  while St[SLen] = ' ' do
    Dec(SLen);
  Val(ValPrepS(St), R, Code);
  if Code <> 0 then begin
    R := Code;
    Result := False;
  end else
    Result := True;
end;

function Str2ExtS(const S : ShortString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P : ShortString;
  PLen : Byte absolute P;
begin
  P := S;
  {trim trailing blanks}
  while P[PLen] = ' ' do
    Dec(PLen);
  Val(ValPrepS(P), R, Code);
  if Code <> 0 then begin
    R := Code;
    Result := False;
  end else
    Result := True;
end;

function Long2StrS(L : LongInt) : ShortString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrS(R : Double; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrS(R : Extended; Width : Byte; Places : ShortInt) : ShortString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepS(const S : ShortString) : ShortString;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesS(S);
  if Result <> '' then begin
    if StrChPosS(Result, DecimalSeparator, P) then begin
      Result[P] := '.';
      if P = Byte(Result[0]) then
        Result[0] := AnsiChar(Pred(P));
    end;
  end else begin
    Result := '0';
  end;
end;

  {-------- General purpose string manipulation --------}

function CharStrS(C : AnsiChar; Len : Cardinal) : ShortString;
  {-Return a string filled with the specified character.}
begin
  if Len = 0 then
    Result[0] := #0
  else begin
    Result[0] := Chr(Len);
    FillChar(Result[1], Len, C);
  end;
end;

function PadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the right with a specified character.}
var
  SLen : Byte absolute S;
begin
  if Length(S) >= Len then
    Result := S
  else begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    Move(S[1], Result[1], SLen);
    if SLen < 255 then
      FillChar(Result[Succ(SLen)], Len-SLen, C);
  end;
end;

function PadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChS(S, ' ', Len);
end;

function LeftPadChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    Move(S[1], Result[Succ(Word(Len))-Length(S)], Length(S));
    FillChar(Result[1], Len-Length(S), C);
  end;
end;

function LeftPadS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChS(S, ' ', Len);
end;

function TrimLeadS(const S : ShortString) : ShortString;
  {-Return a string with leading white space removed}
var
  I : Cardinal;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  Move(S[I], Result[1], Length(S)-I+1);
  Result[0] := Char(Length(S)-I+1);
end;

function TrimTrailS(const S : ShortString) : ShortString;
  {-Return a string with trailing white space removed.}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    Dec(Result[0]);
end;

function TrimS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing white space removed.}
var
  I    : Cardinal;
  SLen : ^Byte;
begin
  Result := S;
  while (SLen^ > 0) and (Result[SLen^] <= ' ') do
    Dec(SLen^);
  I := 1;
  while (I<=SLen^) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function TrimSpacesS(const S : ShortString) : ShortString;
  {-Return a string with leading and trailing spaces removed.}
var
  I    : Word;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    Dec(Result[0]);
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    Delete(Result, 1, I);
end;

function CenterChS(const S : ShortString; C : AnsiChar; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= Len then
    Result := S
  else if Length(S) < 255 then begin
    if Len > 255 then Len := 255;
    Result[0] := Chr(Len);
    FillChar(Result[1], Len, C);
    Move(S[1], Result[Succ((Len-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChS(S, ' ', Len);
end;

function EntabS(const S : ShortString; TabSize : Byte) : ShortString; assembler;
  {-Convert blanks in a string to tabs.}
asm
  push   ebx                 { Save registers }
  push   edi
  push   esi

  mov    esi, eax            { ESI => input string }
  mov    edi, ecx            { EDI => output string }
  xor    ebx, ebx            { Initial SpaceCount = 0 }
  xor    ecx, ecx            { Default input length = 0 }
  and    edx, 0FFh           { Default output length = 0 in DH, TabSize in DL }

  mov    cl, [esi]           { Get input length }
  inc    esi
  or     edx, edx            { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx            { Return zero length string if TabSize = 0 }

@@DefLength:
  mov    [edi], cl           { Store default output length }
  inc    edi
  or     ecx, ecx
  jz     @@Done              { Done if empty input string }
  inc    ch                  { Current input position=1 }

@@Next:
  or     ebx, ebx            { Compare SpaceCount to 0 }
  jz     @@NoTab             { If SpaceCount=0 then no tab insert here }
  xor    eax, eax
  mov    al, ch              { Ipos to AL }
  div    dl                  { Ipos DIV TabSize }
  cmp    ah, 1               { Ipos MOD TabSize = 1 ? }
  jnz    @@NoTab             { If not, no tab insert here }
  sub    edi, ebx            { Remove unused characters from output string }
  sub    dh, bl              { Reduce Olen by SpaceCount }
  inc    dh                  { Add one to output length }
  xor    ebx, ebx            { Reset SpaceCount }
  mov    byte ptr [edi], 09h { Store a tab }
  inc    edi

@@NoTab:
  mov    al, [esi]           { Get next input character }
  inc    esi
  cmp    cl, ch              { End of string? }
  jz     @@Store             { Yes, store character anyway }
  inc    bl                  { Increment SpaceCount }
  cmp    al, 32              { Is character a space? }
  jz     @@Store             { Yes, store it for now }
  xor    ebx, ebx            { Reset SpaceCount }
  cmp    al, 39              { Is it a quote? }
  jz     @@Quotes            { Yep, enter quote loop }
  cmp    al, 34              { Is it a doublequote? }
  jnz    @@Store             { Nope, store it }

@@Quotes:
  mov    ah, al              { Save quote start }

@@NextQ:
  mov    [edi], al           { Store quoted character }
  inc    edi
  inc    dh                  { Increment output length }
  mov    al, [esi]           { Get next character }
  inc    esi
  inc    ch                  { Increment Ipos }
  cmp    ch, cl              { At end of line? }
  jae    @@Store             { If so, exit quote loop }
  cmp    al, ah              { Matching end quote? }
  jnz    @@NextQ             { Nope, stay in quote loop }
  cmp    al, 39              { Single quote? }
  jz     @@Store             { Exit quote loop }
  cmp    byte ptr [esi-2],'\' { Previous character an escape? }
  jz     @@NextQ             { Stay in if so }

@@Store:
  mov    [edi], al           { Store last character }
  inc    edi
  inc    dh                  { Increment output length }
  inc    ch                  { Increment input position }
  jz     @@StoreLen          { Exit if past 255 }
  cmp    ch, cl              { Compare Ipos to Ilen }
  jbe    @@Next              { Repeat while characters left }

@@StoreLen:
  xor    eax, eax
  mov    al, dh
  sub    edi, eax
  dec    edi
  mov    [edi], dh           { Store final length }

@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;


function DetabS(const S : ShortString; TabSize : Byte) : ShortString; assembler;
  {-Expand tabs in a string to blanks.}
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, ecx           { EDI => output string }
  mov    esi, eax           { ESI => input string }
  xor    ecx, ecx           { Default input length = 0 }
  and    edx, 0FFh          { Default output length = 0 in DH, DL is Tabsize }
  xor    eax, eax
  mov    cl, [esi]          { Get input length }
  inc    esi
  or     edx, edx           { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx           { Return zero length string if TabSize = 0 }

@@DefLength:
  mov    [edi], cl          { Store default output length }
  inc    edi
  or     ecx, ecx
  jz     @@Done             { Done if empty input string }
  mov    ah, 09h            { Store tab in AH }
  mov    bl, 255            { Maximum length of output }

@@Next:
  mov    al, [esi]          { Next input character }
  inc    esi
  cmp    al, ah             { Is it a tab? }
  jz     @@Tab              { Yes, compute next tab stop }
  mov    [edi], al          { No, store to output }
  inc    edi
  inc    dh                 { Increment output length }
  cmp    dh, bl             { 255 characters max }
  jz     @@StoreLen
  dec    cl
  jnz    @@Next             { Next character while Olen <= 255 }
  jmp    @@StoreLen         { Loop termination }

@@Tab:
  mov    bh, cl             { Save input counter }
  mov    al, dh             { Current output length in AL }
  and    eax, 0FFh          { Clear top byte }
  div    dl                 { OLen DIV TabSize in AL }
  inc    al                 { Round up to next tab position }
  mul    dl                 { Next tab position in AX }
  or     ah, ah             { AX > 255? }
  jnz    @@StoreLen         { Can't store it }
  sub    al, dh             { Count of blanks to insert }
  add    dh, al             { New output length in DH }
  mov    cl, al             { Loop counter for blanks }
  mov    ax, 0920h          { Tab in AH, Blank in AL }
  rep    stosb              { Store blanks }
  mov    cl, bh             { Restore input position }
  dec    cl
  jnz    @@Next             { Back for next input }

@@StoreLen:
  xor    eax, eax
  mov    al, dh
  sub    edi, eax
  dec    edi
  mov    [edi], dh           { Store final length }

@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;

function ScrambleS(const S, Key : ShortString) : ShortString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  J, LKey, LStr : Byte;
  I : Cardinal;
begin
  Result := S;
  LKey := Length(Key);
  LStr := Length(S);
  if LKey = 0 then Exit;
  if LStr = 0 then Exit;
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := Char(Byte(S[I]) xor Byte(Key[J]));
    inc(I);
    dec(J);
  end;
end;

function SubstituteS(const S, FromStr, ToStr : ShortString) : ShortString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  P : Cardinal;
  I : Byte;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      if StrChPosS(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterS(const S, Filters1 : ShortString) : ShortString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  for I := 1 to Length(S) do
    if not CharExistsS(Filters1, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  Result[0] := Char(Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsS(const S : ShortString; C : AnsiChar) : Boolean;  assembler;
  {-Determine whether a given character exists in a string. }
asm
  xor   ecx, ecx
  mov   ch, [eax]
  inc   eax
  or    ch, ch
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   cl
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   cl
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   cl
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   cl
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ch, 4
  jna   @@Done

@@5:
  cmp   ch, 4
  jae   @@Loop

  cmp   ch, 3
  je    @@1

  cmp   ch, 2
  je    @@2

  cmp   ch, 1
  je    @@3

@@Done:
  xor   eax, eax
  mov   al, cl
end;

function CharCountS(const S : ShortString; C : AnsiChar) : Byte; assembler;
  {-Count the number of a given character in a string. }
asm
  xor   ecx, ecx
  mov   ch, [eax]
  inc   eax
  or    ch, ch
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   cl

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   cl

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   cl

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   cl

@@4:
  add   eax, 4
  sub   ch, 4
  jna   @@Done

@@5:
  cmp   ch, 4
  jae   @@Loop

  cmp   ch, 3
  je    @@1

  cmp   ch, 2
  je    @@2

  cmp   ch, 1
  je    @@3

@@Done:
  mov   al, cl
end;

function WordCountS(const S, WordDelims : ShortString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I     : Integer;
  SLen  : Byte;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionS(N : Cardinal; const S, WordDelims : ShortString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  I     : Cardinal;
  Count : Byte;
  SLen  : Byte absolute S;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsS(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordS(N : Cardinal; const S, WordDelims : ShortString) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  I    : Cardinal;
  Len  : Byte;
  SLen : Byte absolute S;
begin
  Len := 0;
  if WordPositionS(N, S, WordDelims, I) then
    {find the end of the current word}
    while (I <= SLen) and not CharExistsS(WordDelims, S[I]) do begin
      {add the I'th character to result}
      Inc(Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  Result[0] := Char(Len);
end;

function AsciiCountS(const S, WordDelims : ShortString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Cardinal;
  InQuote : Boolean;
  SLen    : Byte absolute S;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and (S[i] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);
    {find the end of the current word}
    while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  I       : Cardinal;
  Count   : Byte;
  InQuote : Boolean;
  SLen    : Byte absolute S;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (I <= SLen) and (S[I] <> Quote) and CharExistsS(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> N then
      while (I <= SLen) and (InQuote or not CharExistsS(WordDelims, S[I])) do begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractAsciiS(N : Cardinal; const S, WordDelims : ShortString;
                       Quote : AnsiChar) : ShortString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  I       : Cardinal;
  Len     : Byte;
  SLen    : Byte absolute S;
  InQuote : Boolean;
begin
  Len := 0;
  InQuote := False;
  if AsciiPositionS(N, S, WordDelims, Quote, I) then
    {find the end of the current word}
    while (I <= SLen) and ((InQuote) or not CharExistsS(WordDelims, S[I])) do begin
      {add the I'th character to result}
      Inc(Len);
      if S[I] = Quote then
        InQuote := not(InQuote);
      Result [Len] := S[I];
      Inc(I);
    end;
  Result [0] := Char(Len);
end;

procedure WordWrapS(const InSt : ShortString; var OutSt, Overlap : ShortString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  EOS, BOS : Cardinal;
  InStLen  : Byte;
  OutStLen : Byte absolute OutSt;
  OvrLen   : Byte absolute Overlap;
begin
  InStLen := Length(InSt);
  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  OutStLen := EOS;
  Move(InSt[1], OutSt[1], OutStLen);

  {find the start of the next word in the line}
  BOS := EOS+1;
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    OvrLen := 0
  else begin
    {copy from the start of the next word to the end of the line}
    OvrLen := Succ(InStLen-BOS);
    Move(InSt[BOS], Overlap[1], OvrLen);
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (OutStLen < Margin) then begin
    FillChar(OutSt[OutStLen+1], Margin-OutStLen, ' ');
    OutStLen := Margin;
  end;
end;

  {--------------- String comparison and searching -----------------}
function CompStringS(const S1, S2 : ShortString) : Integer; assembler;
  {-Compare two strings.}
asm
  push   edi
  mov    edi, edx           { EDI points to S2 }
  push   esi
  mov    esi, eax           { ESI points to S1 }

  xor    ecx, ecx

  mov    dl, [edi]          { DL = Length(S2) }
  inc    edi                { EDI points to S2[1] }
  mov    cl, [esi]
  inc    esi                { CL = Length(S1) - ESI points to S1[1] }

  or     eax, -1            { EAX holds temporary result }

  cmp    cl, dl             { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    eax                { S1 longer than S2 }
  mov    cl, dl             { Length(S2) in CL }

@@EqLen:
  inc    eax                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

  repe   cmpsb              { Compare until no match or ECX = 0 }
  je     @@Done             { If Equal, result ready based on length }

  mov    eax, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     eax, -1            { Else S1 Less, Return -1 }

@@Done:
  pop    esi
  pop    edi
end;

function CompUCStringS(const S1, S2 : ShortString) : Integer;  assembler;
  {-Compare two strings. This compare is not case sensitive.}
asm
  push   ebx
  push   edi                { Save registers }
  push   esi

  mov    edi, edx           { EDI points to S2 }
  mov    esi, eax           { ESI points to S1 }

  xor    eax, eax           { EAX holds chars from S1 }
  xor    ecx, ecx           { ECX holds count of chars to compare }
  xor    edx, edx           { DH holds temp result, DL chars from S2 }
  or     ebx, -1

  mov    al, [edi]          { AH = Length(S2) }
  inc    edi                { EDI points to S2[1] }
  mov    cl, [esi]          { CL = Length(S1) - SI points to S1[1] }
  inc    esi

  cmp    cl, al             { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    ebx                { S1 longer than S2 }
  mov    cl, al             { Shorter length in CL }

@@EqLen:
  inc    ebx                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if lesser string is empty }

@@Start:
  mov    al, [esi]          { S1[?] into AL }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  mov    dl, [edi]          { S2[?] into DL }
  inc    edi                { Point EDI to next char in S2 }
  mov    dh, al
  mov    al, dl
  mov    dl, dh

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  cmp    dl, al             { Compare until no match }
  jnz    @@Output
  dec    ecx
  jnz    @@Start

  je     @@Done             { If Equal, result ready based on length }

@@Output:
  mov    ebx, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     ebx, -1            { Else S1 Less, Return -1 }

@@Done:
  mov    eax, ebx           { Result into AX }
  pop    esi                { Restore Registers }
  pop    edi
  pop    ebx
end;

function SoundexS(const S : ShortString) : ShortString; assembler;
  {-Return 4 character soundex of an input string}
const
  SoundexTable : array[0..255] of Char =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
    { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
    { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
asm
  push  edi
  mov   edi, edx                 { EDI => output string }
  push  ebx
  push  esi

  mov   esi, eax                 { ESI => input string }
  mov   byte ptr [edi], 4        { Prepare output string to be #4'0000' }
  mov   dword ptr [edi+1], '0000'
  inc   edi

  mov   cl, byte ptr [esi]
  inc   esi
  or    cl, cl                   { Exit if null string }
  jz    @@Done

  xor   eax, eax
  mov   al, [esi]                { Get first character of input string }
  inc   esi

  push  ecx                      { Save ECX across call to CharUpper }
  push  eax                      { Push Char onto stack for CharUpper }
  call  CharUpper                { Uppercase AL }
  pop   ecx                      { Restore saved register }

  mov   [edi], al                { Store first output character }
  inc   edi

  dec   cl                       { One input character used }
  jz    @@Done                   { Was input string one char long? }

  mov   ch, 03h                  { Output max 3 chars beyond first }
  mov   edx, offset SoundexTable { EDX => Soundex table }
  xor   eax, eax                 { Prepare for address calc }
  xor   bl, bl                   { BL will be used to store 'previous char' }

@@Next:
  mov   al, [esi]                { Get next char in AL }
  inc   esi
  mov   al, [edx+eax]            { Get soundex code into AL }
  or    al, al                   { Is AL zero? }
  jz    @@NoStore                { If yes, skip this char }
  cmp   bl, al                   { Is it the same as the previous stored char? }
  je    @@NoStore                { If yes, skip this char }
  mov   [edi], al                { Store char to Dest }
  inc   edi
  dec   ch                       { Decrement output counter }
  jz    @@Done                   { If zero, we're done }
  mov   bl, al                   { New previous character }

@@NoStore:
  dec   cl                       { Decrement input counter }
  jnz   @@Next

@@Done:
  pop   esi
  pop   ebx
  pop   edi
end;

function MakeLetterSetS(const S : ShortString) : Longint;  assembler;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push   ebx                { Save registers }
  push   esi

  mov    esi, eax           { ESI => string }
  xor    ecx, ecx           { Zero ECX }
  xor    edx, edx           { Zero EDX }
  xor    eax, eax           { Zero EAX }
  add    cl, [esi]          { CX = Length(S) }
  jz     @@Exit             { Done if ECX is 0 }
  inc    esi

@@Next:
  mov    al, [esi]          { EAX has next char in S }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  sub    eax, 'A'           { Convert to bit number }
  cmp    eax, 'Z'-'A'       { Was char in range 'A'..'Z'? }
  ja     @@Skip             { Skip it if not }

  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx
  ror    edx, cl
  or     edx, 01h               { Set appropriate bit }
  rol    edx, cl
  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx

@@Skip:
  dec    ecx
  jnz    @@Next             { Get next character }

@@Exit:
  mov    eax, edx           { Move EDX to result }
  pop    esi                { Restore registers }
  pop    ebx
end;

procedure BMMakeTableS(const MatchString : ShortString; var BT : BTable); assembler;
  {-Build a Boyer-Moore link table}
asm
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx

  xor   eax, eax           { Zero EAX }
  xor   ecx, ecx           { Zero ECX }
  mov   cl, [esi]          { ECX has length of MatchString }
  inc   esi

  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  or    eax, ecx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edx+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and EDI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, [esi]           { Length of MatchString in EDX }
  inc   esi
  and   edx, 0FFh

  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  mov   esi, Pos            { Set position in Pos }
  {dec   edi}               { Found, calculate position }
  sub   edi, ebx
  mov   eax, 1              { Set result to True }
  mov   [esi], edi
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

(*
function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
var
  LocalBuffer: pointer;
  LocalMatch : ShortString;
begin
  LocalMatch := UpperCase(MatchString);
  GetMem(LocalBuffer, BufLength);
  Move(Buffer, LocalBuffer^, BufLength);
  CharUpper(PChar(LocalBuffer));
  Result := BMSearchS(LocalBuffer^, BufLength, BT, LocalMatch, Pos);
  FreeMem(LocalBuffer, BufLength);
end;
*)

(**)
function BMSearchUCS(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
var
  BufPtr : Pointer;
  SaveEBX : LongInt;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx
  mov   SaveEBX, ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, byte ptr [esi]  { Length of MatchString in EDX }
  and   edx, 0FFh           { Clean up EDX }
  inc   esi                 { Set ESI to first character }

  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }

  push  ebx                 { Save registers }
  mov   ebx, SaveEBX
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
//  call AnsiStrUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  xor   eax, eax
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  mov   ebx, SaveEBX
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
//  call AnsiStrUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  mov   al, [esi]           { Get MatchString char }
  dec   esi
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;
(**)

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionS(const Name, Ext : ShortString) : ShortString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameS(const PathName : ShortString) : ShortString;
  {-Return just the filename and extension of a pathname.}
var
  I : Longint;
begin
  Result := '';
  if PathName = '' then
    Exit;
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}
  Result := Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameS(const PathName : ShortString) : ShortString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
begin
  Result := JustFileNameS(PathName);
  if HasExtensionS(Result, DotPos) then
    Result := Copy(Result, 1, DotPos-1);
end;

function JustExtensionS(const Name : ShortString) : ShortString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionS(Name, DotPos) then
    Result := Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameS(const PathName : ShortString) : ShortString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Longint;
begin
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    Result [0] := #0
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = PathDelim {'\'}) then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := Copy(PathName, 1, I);
end;

function AddSlashS(const DirName : ShortString) : ShortString;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then begin
    Result := PathDelim;
  end
  else
  if (Result[Length(Result)] <> PathDelim) then
    Result := Result + PathDelim;
end;

function HasExtensionS(const Name : ShortString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    {and (Pos('\', Copy(Name, Succ(DotPos), MaxFileLen)) = 0);}
    and not CharExistsS(Copy(Name, Succ(DotPos), StMaxFileLen), PathDelim {'\'});
end;

  {------------------ Formatting routines --------------------}


function CommaizeChS(L : Longint; Ch : AnsiChar) : ShortString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrS(L);
  Len := Length(Result);
  NumCommas := (Len - 1) div 3;
  for I := 1 to NumCommas do
    System.Insert(Ch, Result, Len-(I * 3)+1);
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeS(L : LongInt) : ShortString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChS(L, ',');
end;

function FormPrimS(const Mask : ShortString; R : TstFloat; const LtCurr,
                   RtCurr : ShortString; Sep, DecPt : AnsiChar;
                   AssumeDP : Boolean) : ShortString;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars : string[8] = '#@*$-+,.';
  PlusArray : array[Boolean] of Char = ('+', '-');
  MinusArray : array[Boolean] of Char = (' ', '-');
  FillArray : array[Blank..Zero] of AnsiChar = (' ', '*', '0');
var
  S : ShortString;         {temporary string}
  Filler : Integer;        {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Word;             {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Places,                  {# of digits after the '.'}
  Blanks,                  {# of blanks returned by Str}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Word;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsS(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result <> '') and (Length(Result) < 255) then begin
    Inc(Result[0]);
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do begin
    if Result[I] = 'C' then begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end else if Result[I] = 'c' then begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result)) and
    not CharExistsS(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    EndF := I;
    case Result[I] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := I;
    else
      goto EndFound;
    end;
    {Inc(EndF);}
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

EndFound:
  {if we jumped to here instead, it wasn't}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R:Digits:Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    FillChar(S[Length(S)+1], Places-MaxPlaces, '0');
    inc(S[0], Places-MaxPlaces);
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillChar(S[1], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if DigitPtr <> 0 then begin
          Result[I] := S[DigitPtr];
          Dec(DigitPtr);
          if (DigitPtr <> 0) and (S[DigitPtr] = '.') then                {!!.01}
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    Dec(Result[0]);
  if RtChars > 0 then begin
    S := RtCurr;
    if Byte(S[0]) > RtChars then
      S[0] := Char(RtChars)
    else
      S := LeftPadS(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Byte(S[0]) > LtChars then
      S[0] := Char(LtChars)
    else
      S := PadS(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormS(const Mask : ShortString ; R : TstFloat ; const LtCurr,
                    RtCurr : ShortString ; Sep, DecPt : AnsiChar) : ShortString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimS(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormS(const Mask : ShortString ; L : LongInt ; const LtCurr,
                      RtCurr : ShortString ; Sep : AnsiChar) : ShortString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimS(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosS(const P : ShortString; C : AnsiChar; var Pos : Cardinal) : Boolean;  assembler;
  {-Return the position of a specified character within a string.}
asm
  push  ebx             { Save registers }
  push  edi

  xor   edi, edi        { Zero counter }
  xor   ebx, ebx
  ////TL!!! Causes a segfault when called add   bl, [eax]       { Get input length }
  jz    @@NotFound
  inc   eax

@@Loop:
  inc   edi             { Increment counter }
  cmp   [eax], dl       { Did we find it? }
  jz    @@Found
  inc   eax             { Increment pointer }

  cmp   edi, ebx        { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   eax, eax        { Not found, zero EAX for False }
  mov   [ecx], eax
  jmp   @@Done

@@Found:
  mov   [ecx], edi      { Set Pos }
  mov   eax, 1          { Set EAX to True }

@@Done:
  pop   edi             { Restore registers }
  pop   ebx
end;

function StrStPosS(const P, S : ShortString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertS(const S : ShortString; C : AnsiChar; Pos : Cardinal) : ShortString;
  {-Insert a character into a string at a specified position.}
var
  Temp : string[2];
begin
  Temp[0] := #1;
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertS(const S1, S2 : ShortString; Pos : Cardinal) : ShortString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteS(const S : ShortString; Pos : Cardinal) : ShortString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteS(const S : ShortString; Pos, Count : Cardinal) : ShortString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;

{-----------------------------  NEW FUNCTIONS (3.00) -------------------------}

function CopyLeftS(const S : ShortString; Len : Cardinal) : ShortString;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;



function CopyMidS(const S : ShortString; First, Len : Cardinal) : ShortString;
  {-Return the mid part of a string}
begin
  if (First > Length(S)) or (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;



function CopyRightS(const S : ShortString; First : Cardinal) : ShortString;
  {-Return the right Len characters of a string}
begin
  if (First > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;

function CopyRightAbsS(const S : ShortString; NumChars : Cardinal) : ShortString;
  {-Return NumChar characters starting from end}
begin
  if (Length(S) > NumChars) then
    Result := Copy(S, (Length(S) - NumChars)+1, NumChars)
  else
    Result := S;
end;


function CopyFromNthWordS(const S, WordDelims : ShortString;
                          AWord : ShortString; N : Cardinal;
                          var SubString : ShortString) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosS(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;



function DeleteFromNthWordS(const S, WordDelims : ShortString;
                            AWord : ShortString; N : Cardinal;
                            var SubString : ShortString) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosS(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyFromToWordS(const S, WordDelims, Word1, Word2 : ShortString;
                         N1, N2 : Cardinal;
                         var SubString : ShortString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosS(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosS(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        SubString := Copy(S, P1, P2-P1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function DeleteFromToWordS(const S, WordDelims, Word1, Word2 : ShortString;
                           N1, N2 : Cardinal;
                           var SubString : ShortString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosS(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosS(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        System.Delete(SubString, P1, P2-P1+1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyWithinS(const S, Delimiter : ShortString;
                     Strip : Boolean) : ShortString;
var
  P1,
  P2     : Cardinal;
  TmpStr : ShortString;
begin
  if (S = '') or (Delimiter = '') or (Pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosS(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, P1 + Length(Delimiter), Length(S));
      if StrStPosS(TmpStr, Delimiter, P2) then begin
        Result := Copy(TmpStr, 1, P2-1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end else begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;



function DeleteWithinS(const S, Delimiter : ShortString) : ShortString;
var
  P1,
  P2     : Cardinal;
  TmpStr : ShortString;
begin
  if (S = '') or (Delimiter = '') or (Pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosS(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, P1 + Length(Delimiter), Length(S));
      if (Pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosS(TmpStr, Delimiter, P2)) then begin
          Result := S;
          P2 := P2 + (2*Length(Delimiter));
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;



function ReplaceWordS(const S, WordDelims, OldWord, NewWord : ShortString;
                      N : Cardinal;
                      var Replacements : Cardinal) : ShortString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    if (WordPosS(S, WordDelims, OldWord, N, P1)) then begin
      Result := S;
      System.Delete(Result, P1, Length(OldWord));
      C := 0;
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) <= 255) then begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
          Inc(P1, Length(NewWord) + 1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function ReplaceWordAllS(const S, WordDelims, OldWord, NewWord : ShortString;
                         var Replacements : Cardinal) : ShortString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    Result := S;
    C := 0;
    while (WordPosS(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) <= 255) then begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


function ReplaceStringS(const S, OldString, NewString : ShortString;
                        N : Cardinal;
                        var Replacements : Cardinal) : ShortString;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : ShortString;
begin
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I  := 1;
  P1 := Pos(OldString, TmpStr);
  C  := P1;
  while (I < N) and (C < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, P1 + Length(OldString));
    Inc(C, P1 + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if ((Length(NewString) + Length(Result)) <= 255) then begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end else begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllS(const S, OldString, NewString : ShortString;
                           var Replacements : Cardinal) : ShortString;
var
  I,
  C,
  P1 : Cardinal;
  Tmp: String;
begin
  Result := S;
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
    Replacements := 0
  else begin
    Tmp := S;
    P1 := Pos(OldString, S);
    if (P1 > 0) then begin
      Result := Copy(Tmp, 1, P1-1);
      C := 0;
      while (P1 > 0) do begin
        for I := 1 to Replacements do begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1+Length(OldString), MaxInt);
        P1 := Pos(OldString, Tmp);
        if (P1 > 0) then begin
          Result := Result + Copy(Tmp, 1, P1-1);
          {Tmp := Copy(Tmp, P1, MaxInt)};
        end else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;

function LastWordS(const S, WordDelims, AWord : ShortString;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : ShortString;
  I      : Cardinal;
begin
  if (S = '') or (WordDelims = '') or
     (AWord = '') or (Pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (Pos(TmpStr[I], WordDelims) > 0) do begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (Pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then begin
      Inc(Position);
      Result := True;
      Exit;
    end;
    System.Delete(TmpStr, Position, Length(TmpStr));
    Position := Length(TmpStr);
  until (Length(TmpStr) = 0);
  Result := False;
  Position := 0;
end;



function LastWordAbsS(const S, WordDelims : ShortString;
                      var Position : Cardinal) : Boolean;
begin
  if (S = '') or (WordDelims = '') then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (Pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then begin
    Result := True;
    Position := 1;
    Exit;
  end;

{find next delimiter character}
  while (Position > 0) and (Pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringS(const S, AString : ShortString;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : ShortString;
  I, C   : Cardinal;
begin
  if (S = '') or (AString = '') or (Pos(AString, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := Pos(AString, TmpStr);
  while (I > 0) do begin
    Inc(C, I + Length(AString));
    System.Delete(TmpStr, 1, I + Length(AString));
    I := Pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsS(const S, Chars : ShortString) : ShortString;
var
  FromInx : Cardinal;
  ToInx   : Cardinal;
begin
  {if either the input string or the list of acceptable chars is empty
   the destination string will also be empty}
  if (S = '') or (Chars = '') then begin
    Result := '';
    Exit;
  end;

  {set the maximum length of the result string (it could be less than
   this, of course}
  Result[0] := AnsiChar(length(S));

  {start off the to index}
  ToInx := 0;

  {in a loop, copy over the chars that match the list}
  for FromInx := 1 to length(S) do
    if CharExistsS(Chars, S[FromInx]) then begin
      inc(ToInx);
      Result[ToInx] := S[FromInx];
    end;

  {make sure that the length of the result string is correct}
  Result[0] := AnsiChar(ToInx);
end;



function RepeatStringS(const RepeatString : ShortString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : ShortString;
var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := '';
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (RepeatString <> '') then begin
    if (MaxLen > 255) then
      MaxLen := 255;
    Len := length(RepeatString);
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      Result[0] := AnsiChar(ActualReps * Len);
      for i := 0 to pred(ActualReps) do
        Move(RepeatString[1], Result[i * Len + 1], Len);
    end;
  end;
end;



function TrimCharsS(const S, Chars : ShortString) : ShortString;
begin
  Result := RightTrimCharsS(S, Chars);
  Result := LeftTrimCharsS(Result, Chars);
end;



function RightTrimCharsS(const S, Chars : ShortString) : ShortString;
var
  CutOff : integer;
begin
  CutOff := length(S);
  while (CutOff > 0) do begin
    if not CharExistsS(Chars, S[CutOff]) then
      Break;
    dec(CutOff);
  end;
  if (CutOff = 0) then
    Result := ''
  else
    Result := Copy(S, 1, CutOff);
end;



function LeftTrimCharsS(const S, Chars : ShortString) : ShortString;
var
  CutOff : integer;
  LenS   : integer;
begin
  LenS := length(S);
  CutOff := 1;
  while (CutOff <= LenS) do begin
    if not CharExistsS(Chars, S[CutOff]) then
      Break;
    inc(CutOff);
  end;
  if (CutOff > LenS) then
    Result := ''
  else
    Result := Copy(S, CutOff, LenS - CutOff + 1);
end;



function ExtractTokensS(const S, Delims: ShortString;
                        QuoteChar  : Char;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : AnsiChar;
  TokenStart : integer;
  Inx        : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or
     (Delims = '') or
     CharExistsS(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart :
        begin
          {if the current char is the quote character, switch states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanQuotedToken

          {if the current char is a delimiter, output a null token}
          else if CharExistsS(Delims, CurChar) then begin

            {if allowed to, output a null token}
            if AllowNulls then begin
              Tokens.Add('');
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);
          end

          {otherwise, the current char is starting a normal token, so
           switch states}
          else
            State := ScanNormalToken
        end;

      ScanQuotedToken :
        begin
          {if the current char is the quote character, switch states}
          if (CurChar = QuoteChar) then
            State := ScanQuotedTokenEnd
        end;

      ScanQuotedTokenEnd :
        begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
          if (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
          else if CharExistsS(Delims, CurChar) then begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
            if ((Inx - TokenStart) = 2) then begin
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end
            end

            {else output the token without the quotes}
            else begin
              Tokens.Add(Copy(S, succ(TokenStart), Inx - TokenStart - 2));
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end

          {otherwise it's a (complex) normal token, so switch states}
          else
            State := ScanNormalToken
        end;

      ScanNormalToken :
        begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token}
          else if CharExistsS(Delims, CurChar) then begin
            Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
            inc(Result);

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end;
        end;

      ScanNormalTokenWithQuote :
        begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
          if (CurChar = QuoteChar) then
            State := ScanNormalToken;
        end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then begin
    inc(TokenStart);
    dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then begin
    Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
    inc(Result);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then begin
    Tokens.Add('');
    inc(Result);
  end;
end;



function ContainsOnlyS(const S, Chars : ShortString;
                       var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (not CharExistsS(Chars, S[I])) then begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanS(const S, Chars : ShortString;
                            var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (CharExistsS(Chars, S[I])) then begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;

function IsChAlphaS(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := IsCharAlpha(@C);
end;

function IsChNumericS(C : Char; Numbers : ShortString) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := CharExistsS(Numbers, C);
end;

function IsChAlphaNumericS(C : Char; Numbers : ShortString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := IsCharAlpha(@C) or CharExistsS(Numbers, C);
end;

function IsStrAlphaS(const S : Shortstring) : Boolean;
  {-Returns true if all characters in string are an alpha}
var
  I : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if not IsCharAlpha(@S[I]) then
        Exit;
    Result := true;
  end;
end;



function IsStrNumericS(const S, Numbers : ShortString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
var
  i : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if (not IsCharAlpha(@S[i])) and
         (not CharExistsS(Numbers, S[i])) then
        Exit;
    Result := true;
  end;
end;


function IsStrAlphaNumericS(const S, Numbers : ShortString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := (IsCharAlpha(@S[I])) or (Pos(S[I], Numbers) > 0);
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;

function StrWithinS(const S, SearchStr : ShortString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : ShortString;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start-1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then begin
    Position := Position + Start - 1;
    Result := True;
  end else
    Result := False;
end;


function WordPosS(const S, WordDelims, AWord : ShortString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : ShortString;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (Pos(AWord, S) = 0) or (N < 1) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I      := 0;
  Len    := Length(AWord);
  P1     := Pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then begin
      if (Pos(TmpStr[P2+1], WordDelims) > 0) then begin
        Inc(I);
      end else
        System.Delete(TmpStr, 1, P2);
    end else if (Pos(TmpStr[P1-1], WordDelims) > 0) and
                ((Pos(TmpStr[P2+1], WordDelims) > 0) or (P2+1 = Length(TmpStr))) then begin
      Inc(I);
    end else if ((P1 + pred(Len)) = Length(TmpStr)) then begin
      if (P1 > 1) and (Pos(TmpStr[P1-1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := Pos(AWord, TmpStr);
  end;
end;


  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := StHexDigits[B shr 4];
  Result[2] := StHexDigits[B and $F];
end;

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := StHexDigits[hi(W) shr 4];
  Result[2] := StHexDigits[hi(W) and $F];
  Result[3] := StHexDigits[lo(W) shr 4];
  Result[4] := StHexDigits[lo(W) and $F];
end;

function HexLL(L : LongInt) : AnsiString;
  {-Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWL(HiWord(L)) + HexWL(LoWord(L));
end;

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLL(LongInt(P));
end;

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLL(L : LongInt) : AnsiString;
  {-Return the binary string for a long integer.}
var
  I : Longint;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLL(L : LongInt) : AnsiString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
end;

function Str2Int16L(const S : AnsiString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

var
  ec : Integer;
begin
  {note the automatic string conversion}
  ValSmallint(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2WordL(const S : AnsiString; var I : Word) : Boolean;
  {-Convert a string to a word.}
var
  ec : Integer;
begin
  {note the automatic string conversion}
  ValWord(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2LongL(const S : AnsiString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

var
  ec : Integer;
begin
  {note the automatic string conversion}
  ValLongint(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

function Str2RealL(const S : AnsiString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);
  if St = '' then Exit;
  Val(ValPrepL(St), R, Code);
  if Code <> 0 then begin
    R := Code;
  end else
    Result := True;
end;

function Str2ExtL(const S : AnsiString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  P := TrimTrailL(S);
  if P = '' then Exit;
  Val(ValPrepL(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
  end else
    Result := True;
end;

function Long2StrL(L : LongInt) : AnsiString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : AnsiString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepL(const S : AnsiString) : AnsiString;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
  C : Longint;
begin
  Result := TrimSpacesL(S);
  if Result <> '' then begin
    if StrChPosL(Result, DecimalSeparator, P) then begin
      C := P;
      Result[C] := '.';
      if C = Length(Result) then
        SetLength(Result, Pred(C));
    end;
  end else
    Result := '0';
end;

  {-------- General purpose string manipulation --------}

function CharStrL(C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Return a string filled with the specified character.}
begin
  SetLength(Result, Len);
  if Len <> 0 then begin
    {SetLength(Result, Len);}
    FillChar(Result[1], Len, C);
  end;
end;

function PadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else begin
    SetLength(Result, Len);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[1], Length(S));
    FillChar(Result[Succ(Length(S))], LongInt(Len)-Length(S), C);
  end;
end;

function PadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChL(S, ' ', Len);
end;

function LeftPadChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);

    { copy current contents (if any) of S to Result }
    if (Length(S) > 0) then                                              {!!.01}
      Move(S[1], Result[Succ(Word(Len))-Length(S)], Length(S));

    { add pad chars }
    FillChar(Result[1], LongInt(Len)-Length(S), C);
  end;
end;

function LeftPadL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChL(S, ' ', Len);
end;

function TrimLeadL(const S : AnsiString) : AnsiString;
  {-Return a string with leading white space removed}
var
  I : Longint;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  SetLength(Result, Length(S)-Pred(I));
  if Length(Result) > 0 then                                             {!!.01}
    Move(S[I], Result[1], Length(S)-Pred(I));
end;

function TrimTrailL(const S : AnsiString) : AnsiString;
  {-Return a string with trailing white space removed.}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));
end;

function TrimL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing white space removed.}
var
  I : Longint;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function TrimSpacesL(const S : AnsiString) : AnsiString;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Longint;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    SetLength(Result, Pred(Length(Result)));
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function CenterChL(const S : AnsiString; C : AnsiChar; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    FillChar(Result[1], Len, C);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[Succ((LongInt(Len)-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChL(S, ' ', Len);
end;

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Convert blanks in a string to tabs.}
var
  InLen, OutLen : Cardinal;
begin
  if S = '' then Exit;
  InLen := Length(S);
  OutLen := 0;
  SetLength(Result, InLen);
asm
  push   ebx                   { Save registers }
  push   edi
  push   esi
  //////TL  mov    edi, [Result]
  mov    edi, [edi]
  xor    ecx, ecx
  add    cl, TabSize
  jz     @@Done

  mov    esi, S
  xor    ebx, ebx              { Zero EBX and EDX }
  xor    edx, edx
  inc    edx                   { Set output length to 1 }

@@Next:
  or     ebx, ebx
  je     @@NoTab               { Jump to NoTab if spacecount is zero }
  mov    eax, edx              { IPos to EAX }
  push   edx
  xor    edx, edx
  div    ecx
  cmp    edx, 1                { Is mod = 1? }
  pop    edx
  jne    @@NoTab               { If not, no tab }

  sub    edi, ebx
  sub    OutLen, ebx
  inc    OutLen
  xor    ebx, ebx              { Reset spacecount }
  mov    byte ptr [edi], 9h    { Store a tab }
  inc    edi

@@NoTab:
  mov    al, [esi]             { Get next input character }
  inc    esi
  cmp    edx, InLen            { End of string? }
  jg     @@Done                { Yes, done }
  inc    ebx                   { Increment SpaceCount }
  cmp    al, 20h               { Is character a space? }
  jz     @@Store               { Yes, store it for now }
  xor    ebx, ebx              { Reset SpaceCount }
  cmp    al, 27h               { Is it a quote? }
  jz     @@Quotes              { Yep, enter quote loop }
  cmp    al, 22h               { Is it a doublequote? }
  jnz    @@Store               { Nope, store it }

@@Quotes:
  mov    ah, al                { Save quote start }

@@NextQ:
  mov    [edi], al             { Store quoted character }
  inc    edi
  inc    OutLen
  mov    al, [esi]             { Get next character }
  inc    esi
  inc    edx                   { Increment Ipos }

  cmp    edx, ecx              { At end of line? }
  jae    @@Store               { If so, exit quote loop }

  cmp    al, ah                { Matching end quote? }
  jnz    @@NextQ               { Nope, stay in quote loop }

  cmp    al, 27h               { Single quote? }
  jz     @@Store               { Exit quote loop }

  cmp    byte ptr [esi-2],'\'  { Previous character an escape? }
  jz     @@NextQ               { Stay in if so }

@@Store:
  mov    [edi], al             { Store last character }
  inc    edi
  inc    OutLen
  inc    edx                   { Increment input position }
  jmp    @@Next                { Repeat while characters left }

@@Done:
  mov    byte ptr [edi], 0h
  pop    esi
  pop    edi
  pop    ebx
end;
  SetLength(Result, OutLen);
end;

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Expand tabs in a string to blanks.}
var
  NumTabs : Integer;
begin
  Result := '';
  if S = '' then Exit;
  if TabSize = 0 then Exit;
  Result := S;
  NumTabs := CharCountL(S, #9);
  if NumTabs = 0 then Exit;
  SetLength(Result, Length(Result)+NumTabs*(Pred(TabSize)));
asm
  push   ebx                { Save registers since we'll be changing them. }
  push   edi
  push   esi

  mov    edi, Result        { EDI => output string. }
  mov    esi, S             { ESI => input string. }
  xor    ebx, ebx
  mov    bl, TabSize
  mov    edi, [edi]
  xor    ecx, ecx           { Default input length = 0. }
  xor    edx, edx           { Zero EDX for output length }
  xor    eax, eax           { Zero EAX }
  mov    ecx, [esi-StrLOffset].LStrRec.Length  { Get input length. }
  or     ebx, ebx           { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx           { Return zero length string if TabSize = 0. }

@@DefLength:
  mov    [edi-StrLOffset].LStrRec.Length, ecx  { Store default output length. }
  or     ecx, ecx
  jz     @@Done             { Done if empty input string. }

@@Next:
  mov    al, [esi]          { Next input character. }
  inc    esi
  cmp    al, 09h            { Is it a tab? }
  jz     @@Tab              { Yes, compute next tab stop. }
  mov    [edi], al          { No, store to output. }
  inc    edi
  inc    edx                { Increment output length. }
  dec    ecx                { Decrement input length. }
  jnz    @@Next
  jmp    @@StoreLen         { Loop termination. }

@@Tab:
  push   ecx                { Save input length. }
  push   edx                { Save output length. }
  mov    eax, edx           { Get current output length in EDX:EAX. }
  xor    edx, edx
  div    ebx                { Output length MOD TabSize in DX. }
  mov    ecx, ebx           { Calc number of spaces to insert... }
  sub    ecx, edx           {  = TabSize - Mod value. }
  pop    edx
  add    edx, ecx           { Add count of spaces into current output length. }

  mov    eax,$2020          { Blank in AH, Blank in AL. }
  shr    ecx, 1             { Store blanks. }
  rep    stosw
  adc    ecx, ecx
  rep    stosb
  pop    ecx                { Restore input length. }
  dec    ecx
  jnz    @@Next
  {jmp    @@Next}           { Back for next input. }

@@StoreLen:
  xor    ebx, ebx
  mov    [edi], bl          { Store terminating null }
  mov    eax, edx
  sub    edi, eax
  mov    [edi-StrLOffset].LStrRec.Length, edx  { Store final length. }

@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;
end;

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  I, J, LKey, LStr : Cardinal;
begin
  Result := S;
  if Key = '' then Exit;
  if S = '' then Exit;
  LKey := Length(Key);
  LStr := Length(S);
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := Char(Byte(S[I]) xor Byte(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteL(const S, FromStr, ToStr : AnsiString) : AnsiString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  I : Cardinal;
  P : Cardinal;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      {P := System.Pos(S[I], FromStr);}
      {if P <> 0 then}
      if StrChPosL(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterL(const S, Filters : AnsiString) : AnsiString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsL(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : AnsiString; C : AnsiChar) : Boolean;  assembler;
  {-Count the number of a given character in a string. }
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrLOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;

function CharCountL(const S : AnsiString; C : AnsiChar) : Cardinal;  assembler;
  {-Count the number of a given character in a string. }
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrLOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;

function WordCountL(const S, WordDelims : AnsiString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I    : Cardinal;
  SLen : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsL(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  Count : Longint;
  I     : Longint;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and not CharExistsL(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordL(N : Cardinal; const S, WordDelims : AnsiString) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  C : Cardinal;
  I, J   : Longint;
begin
  Result := '';
  if WordPositionL(N, S, WordDelims, C) then begin
    I := C;
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not
           CharExistsL(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

function AsciiCountL(const S, WordDelims : AnsiString; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Longint;
  InQuote : Boolean;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= Length(S) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote)
      and CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Result);
    {find the end of the current word}
    while (I <= Length(S)) and
          (InQuote or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionL(N : Cardinal; const S, WordDelims : AnsiString;
                        Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  Count, I : Longint;
  InQuote  : Boolean;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote) and
          CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and (InQuote or not
             CharExistsL(WordDelims, S[I])) do begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractAsciiL(N : Cardinal; const S, WordDelims : AnsiString;
                       Quote : AnsiChar) : AnsiString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  C       : Cardinal;
  I, J    : Longint;
  InQuote : Boolean;
begin
  InQuote := False;
  if AsciiPositionL(N, S, WordDelims, Quote, C) then begin
    I := C;
    J := I;
    {find the end of the current word}
    while (I <= Length(S)) and ((InQuote)
      or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not(InQuote);
      Inc(I);
    end;
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

procedure WordWrapL(const InSt : AnsiString; var OutSt, Overlap : AnsiString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Cardinal;
  EOS, BOS : Cardinal;
begin
  InStLen := Length(InSt);
  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  SetLength(OutSt, EOS);
  Move(InSt[1], OutSt[1], Length(OutSt));

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}

    SetLength(OverLap, InStLen);
    Move(InSt[BOS], Overlap[1], Succ(InStLen-BOS));
    SetLength(OverLap, Succ(InStLen-BOS));
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (Length(OutSt) < LongInt(Margin)) then begin
    SetLength(OutSt, Margin);
    FillChar(OutSt[Succ(Length(OutSt))], LongInt(Margin)-Length(OutSt), ' ');
  end;
end;

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : AnsiString) : Integer;  assembler;
  {-Compare two strings.}
asm
  push   edi
  mov    edi, edx           { EDI points to S2 }
  push   esi
  mov    esi, eax           { ESI points to S1 }

  xor    edx, edx
  xor    ecx, ecx

  or     edi, edi
  jz     @@1
  mov    edx, [edi-StrLOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrLOffset].LStrRec.Length

@@2:
  or     eax, -1            { EAX holds temporary result }

  cmp    ecx, edx           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    eax                { S1 longer than S2 }
  mov    ecx, edx           { Length(S2) in CL }

@@EqLen:
  inc    eax                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

  repe   cmpsb              { Compare until no match or ECX = 0 }
  je     @@Done             { If Equal, result ready based on length }

  mov    eax, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     eax, -1            { Else S1 Less, Return -1 }

@@Done:
  pop    esi
  pop    edi
end;

function CompUCStringL(const S1, S2 : AnsiString) : Integer;  assembler;
  {-Compare two strings. This compare is not case sensitive.}
asm
  push   ebx                { Save registers }
  push   edi
  push   esi

  mov    edi, edx           { EDI points to S2 }
  mov    esi, eax           { ESI points to S1 }

  xor    eax, eax
  xor    ecx, ecx
  xor    edx, edx           { DL chars from S2 }
  or     ebx, -1

  or     edi, edi
  jz     @@1
  mov    eax, [edi-StrLOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrLOffset].LStrRec.Length

@@2:
  cmp    ecx, eax           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    ebx                { S1 longer than S2 }
  mov    ecx, eax           { Shorter length in ECX }

@@EqLen:
  inc    ebx                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if lesser string is empty }

@@Start:
  xor    eax, eax           { EAX holds chars from S1 }
  mov    al, [esi]          { S1[?] into AL }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  mov    dl, [edi]          { S2[?] into DL }
  inc    edi                { Point EDI to next char in S2 }
  mov    dh, al
  mov    al, dl
  mov    dl, dh

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  cmp    dl, al             { Compare until no match }
  jne    @@Output
  dec    ecx
  jnz    @@Start

  je     @@Done             { If Equal, result ready based on length }

@@Output:
  mov    ebx, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     ebx, -1            { Else S1 Less, Return -1 }

@@Done:
  mov    eax, ebx           { Result into EAX }
  pop    esi                { Restore Registers }
  pop    edi
  pop    ebx
end;

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string}
const
  SoundexTable : array[0..255] of Char =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
    { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
    { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
begin
  if S = '' then Exit;
  SetLength(Result, 4);
asm
  push  edi
  /////TL  mov   edi, [Result]            { EDI => output string. }
  mov   edi, [edi]
  push  ebx
  push  esi

  mov   esi, S                   { ESI => input string. }
  mov   dword ptr [edi], '0000'  { Initialize output string to '0000'. }
  xor   eax, eax
  mov   [edi+4], al              { Set null at end of string. }

  mov   ecx, [esi-StrLOffset].LStrRec.Length
  or    ecx, ecx                 { Exit if null string. }
  jz    @@Done

  mov   al, [esi]                { Get first character of input string. }
  inc   esi

  push  ecx                      { Save ECX across call to CharUpper. }
  push  eax                      { Push Char onto stack for CharUpper. }
  call  CharUpper                { Uppercase AL. }
  pop   ecx                      { Restore saved register. }

  mov   [edi], al                { Store first output character. }
  inc   edi

  dec   ecx                      { One input character used. }
  jz    @@Done                   { Was input string one char long?. }

  mov   bh, 03h                  { Output max 3 chars beyond first. }
  mov   edx, offset SoundexTable { EDX => Soundex table. }
  xor   eax, eax                 { Prepare for address calc. }
  xor   bl, bl                   { BL will be used to store 'previous char'. }

@@Next:
  mov   al, [esi]                { Get next char in AL. }
  inc   esi
  mov   al, [edx+eax]            { Get soundex code into AL. }
  or    al, al                   { Is AL zero? }
  jz    @@NoStore                { If yes, skip this char. }
  cmp   bl, al                   { Is it the same as the previous stored char? }
  je    @@NoStore                { If yes, skip this char. }
  mov   [edi], al                { Store char to Dest. }
  inc   edi
  dec   bh                       { Decrement output counter. }
  jz    @@Done                   { If zero, we're done. }
  mov   bl, al                   { New previous character. }

@@NoStore:
  dec   ecx                      { Decrement input counter. }
  jnz   @@Next

@@Done:
  pop   esi
  pop   ebx
  pop   edi
end;
end;

function MakeLetterSetL(const S : AnsiString) : Longint;  assembler;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push   ebx                { Save registers }
  push   esi

  mov    esi, eax           { ESI => string }
  xor    ecx, ecx           { Zero ECX }
  xor    edx, edx           { Zero EDX }
  {or     edx, edx}
  or     eax, eax
  jz     @@Exit
  xor    eax, eax           { Zero EAX }
  add    ecx, [esi-StrLOffset].LStrRec.Length
  jz     @@Exit             { Done if ECX is 0 }

@@Next:
  mov    al, [esi]          { EAX has next char in S }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  sub    eax, 'A'           { Convert to bit number }
  cmp    eax, 'Z'-'A'       { Was char in range 'A'..'Z'? }
  ja     @@Skip             { Skip it if not }

  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx
  ror    edx, cl
  or     edx, 01h               { Set appropriate bit }
  rol    edx, cl
  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx

@@Skip:
  dec    ecx
  jnz    @@Next             { Get next character }

@@Exit:
  mov    eax, edx           { Move EDX to result }
  pop    esi                { Restore registers }
  pop    ebx
end;

procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable);  assembler;
  {-Build a Boyer-Moore link table}
asm
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx

  or    eax, eax
  jz    @@MTDone

  xor   eax, eax           { Zero EAX }
  mov   ecx, [esi-StrLOffset].LStrRec.Length
  cmp   ecx, 0FFh          { If ECX > 255, force to 255 }
  jbe   @@1
  mov   ecx, 0FFh

@@1:
  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  mov   ax, cx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  mov   edi, edx           { Reset EDI to beginning of table }
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edi+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;  assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrLOffset].LStrRec.Length
  cmp   edx, 0FFh          { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  mov   esi, Pos            { Set position in Pos }
  {dec   edi}               { Found, calculate position }
  sub   edi, ebx
  mov   eax, 1              { Set result to True }
  mov   [esi], edi
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;  assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrLOffset].LStrRec.Length
  cmp   edx, 0FFh           { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  xor   eax, eax
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  mov   al, [esi]           { Get MatchString char }
  dec   esi
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionL(const Name, Ext : AnsiString) : AnsiString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameL(const PathName : AnsiString) : AnsiString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
  S      : AnsiString;
begin
  S := JustFileNameL(PathName);
  if HasExtensionL(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;

function JustExtensionL(const Name : AnsiString) : AnsiString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameL(const PathName : AnsiString) : AnsiString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;

  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = PathDelim {'\'}) then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := System.Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := System.Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := System.Copy(PathName, 1, I);
end;

function AddSlashL(const DirName : AnsiString) : AnsiString;
  {-Add a default slash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Result := PathDelim
  else
  if (Result[Length(Result)] <> PathDelim) then
    Result := Result + PathDelim;
end;


function HasExtensionL(const Name : AnsiString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    and not CharExistsL(System.Copy(Name, Succ(DotPos), StMaxFileLen), PathDelim {'\'});
end;

  {------------------ Formatting routines --------------------}


function CommaizeChL(L : Longint; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrL(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeL(L : LongInt) : AnsiString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChL(L, ',');
end;

function FormPrimL(const Mask : AnsiString; R : TstFloat; const LtCurr, RtCurr : AnsiString;
                  Sep, DecPt : AnsiChar; AssumeDP : Boolean) : AnsiString;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars : string[8] = '#@*$-+,.';
  PlusArray : array[Boolean] of Char = ('+', '-');
  MinusArray : array[Boolean] of Char = (' ', '-');
  FillArray : array[Blank..Zero] of AnsiChar = (' ', '*', '0');
var
  S : string;              {temporary string}
  Filler : Integer;        {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Longint;          {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Blanks,                  {# of blanks returned by Str}
  Places,                  {# of digits after the '.'}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsL(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result <> '') then begin
    SetLength(Result, Succ(Length(Result)));
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do begin
    if Result[I] = 'C' then begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end else if Result[I] = 'c' then begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result))
    {and (System.Pos(Result[StartF], FormChars) = 0) do}
    and not CharExistsL(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    EndF := I;
    case Result[EndF] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := EndF;
    else
      goto EndFound;
    end;
    {Inc(EndF);}
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

EndFound:
  {if we jumped to here instead, it wasn't}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R:Digits:Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    I := Length(S);
    SetLength(S, LongInt(I) + (Places-MaxPlaces));
    FillChar(S[Succ(I)], Places-MaxPlaces, '0');
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillChar(S[1], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if DigitPtr <> 0 then begin
          Result[I] := S[DigitPtr];
          Dec(DigitPtr);
          if (DigitPtr <> 0) and (S[DigitPtr] = '.') then                {!!.01}
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    SetLength(Result, Pred(Length(Result)));
  if RtChars > 0 then begin
    S := RtCurr;
    if Length(S) > RtChars then
      SetLength(S, RtChars)
    else
      S := LeftPadL(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Length(S) > LtChars then
      SetLength(S, LtChars)
    else
      S := PadL(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormL(const Mask : AnsiString ; R : TstFloat ; const LtCurr,
                    RtCurr : AnsiString ; Sep, DecPt : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimL(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormL(const Mask : AnsiString ; L : LongInt ; const LtCurr,
                      RtCurr : AnsiString ; Sep : AnsiChar) : AnsiString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimL(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean; assembler;
  {-Return the position of a specified character within a string.}
asm
  push  ebx             { Save registers }
  push  edi

  or    eax, eax        { Protect against null string }
  jz    @@NotFound

  xor   edi, edi        { Zero counter }
  mov   ebx, [eax-StrLOffset].LStrRec.Length  { Get input length }

@@Loop:
  inc   edi             { Increment counter }
  cmp   [eax], dl       { Did we find it? }
  jz    @@Found
  inc   eax             { Increment pointer }

  cmp   edi, ebx        { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   eax, eax        { Not found, zero EAX for False }
  mov   [ecx], eax
  jmp   @@Done

@@Found:
  mov   [ecx], edi      { Set Pos }
  mov   eax, 1          { Set EAX to True }

@@Done:
  pop   edi             { Restore registers }
  pop   ebx
end;

function StrStPosL(const P, S : AnsiString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
  {-Insert a character into a string at a specified position.}
var
  Temp : string;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertL(const S1, S2 : AnsiString; Pos : Cardinal) : AnsiString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteL(const S : AnsiString; Pos : Cardinal) : AnsiString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteL(const S : AnsiString; Pos, Count : Cardinal) : AnsiString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;


{----------------------------------------------------------------------------}

function CopyLeftL(const S : AnsiString; Len : Cardinal) : AnsiString;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;

{----------------------------------------------------------------------------}

function CopyMidL(const S : AnsiString; First, Len : Cardinal) : AnsiString;
  {-Return the mid part of a string}
begin
  if (LongInt(First) > Length(S)) or (LongInt(Len) < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;

{----------------------------------------------------------------------------}

function CopyRightL(const S : AnsiString; First : Cardinal) : AnsiString;
  {-Return the right Len characters of a string}
begin
  if (LongInt(First) > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;

{----------------------------------------------------------------------------}

function CopyRightAbsL(const S : AnsiString; NumChars : Cardinal) : AnsiString;
  {-Return NumChar characters starting from end}
begin
  if (Cardinal(Length(S)) > NumChars) then
    Result := Copy(S, (Cardinal(Length(S)) - NumChars)+1, NumChars)
  else
    Result := S;
end;

{----------------------------------------------------------------------------}

function WordPosL(const S, WordDelims, AWord : AnsiString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : AnsiString;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (pos(AWord, S) = 0) or (N < 1) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I      := 0;
  Len    := Length(AWord);
  P1     := pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then begin
      if (pos(TmpStr[P2+1], WordDelims) > 0) then begin
        Inc(I);
      end else
        System.Delete(TmpStr, 1, P2);
    end else if (pos(TmpStr[P1-1], WordDelims) > 0) and
                ((pos(TmpStr[P2+1], WordDelims) > 0) or
                 (LongInt(P2+1) = Length(TmpStr))) then begin
      Inc(I);
    end else if ((LongInt(P1 + pred(Len))) = Length(TmpStr)) then begin
      if (P1 > 1) and (pos(TmpStr[P1-1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := pos(AWord, TmpStr);
  end;
end;


{----------------------------------------------------------------------------}

function CopyFromNthWordL(const S, WordDelims : AnsiString;
                          AWord : AnsiString; N : Cardinal;
                          var SubString : AnsiString) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromNthWordL(const S, WordDelims : AnsiString;
                            AWord : AnsiString; N : Cardinal;
                            var SubString : AnsiString) : Boolean;
var
  P : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function CopyFromToWordL(const S, WordDelims, Word1, Word2 : AnsiString;
                         N1, N2 : Cardinal;
                         var SubString : AnsiString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        SubString := Copy(S, P1, P2-P1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : AnsiString;
                           N1, N2 : Cardinal;
                           var SubString : AnsiString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        System.Delete(SubString, P1, P2-P1+1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function CopyWithinL(const S, Delimiter : AnsiString;
                     Strip : Boolean) : AnsiString;
var
  P1,
  P2     : Cardinal;
  TmpStr : AnsiString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if StrStPosL(TmpStr, Delimiter, P2) then begin
        Result := Copy(TmpStr, 1, P2-1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end else begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteWithinL(const S, Delimiter : AnsiString) : AnsiString;
var
  P1,
  P2     : Cardinal;
  TmpStr : AnsiString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if (pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosL(TmpStr, Delimiter, P2)) then begin
          Result := S;
          P2 := LongInt(P2) + (2*Length(Delimiter));
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function ReplaceWordL(const S, WordDelims, OldWord, NewWord : AnsiString;
                      N : Cardinal;
                      var Replacements : Cardinal) : AnsiString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;

  if (WordPosL(S, WordDelims, OldWord, N, P1)) then begin
    Result := S;
    System.Delete(Result, P1, Length(OldWord));

    C := 0;
    for I := 1 to Replacements do begin
      if ((Length(NewWord)) + Length(Result)) < MaxLongInt then begin
        Inc(C);
        System.Insert(NewWord, Result, P1);
        Inc(P1, Length(NewWord) + 1);
      end else begin
        Replacements := C;
        Exit;
      end;
    end;
  end else begin
    Result := S;
    Replacements := 0;
  end;
end;


function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : AnsiString;
                         var Replacements : Cardinal) : AnsiString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    Result := S;
    C := 0;
    while (WordPosL(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        {if ((Length(NewWord) + Length(Result)) <= 255) then begin}      {!!.01}
        if ((Length(NewWord) + Length(Result)) < MaxLongInt) then begin  {!!.01}
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


{----------------------------------------------------------------------------}

function ReplaceStringL(const S, OldString, NewString : AnsiString;
                        N : Cardinal;
                        var Replacements : Cardinal) : AnsiString;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : AnsiString;
begin
  if (S = '') or (OldString = '') or (pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I  := 1;
  P1 := pos(OldString, TmpStr);
  C  := P1;
  while (I < N) and (LongInt(C) < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, LongInt(P1) + Length(OldString));
    Inc(C, LongInt(P1) + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if (((Length(NewString)) + Length(Result)) < MaxLongInt) then begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end else begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllL(const S, OldString, NewString : AnsiString;
                           var Replacements : Cardinal) : AnsiString;
var
  I,
  C  : Cardinal;
  P1 : longint;
  Tmp: AnsiString;
begin
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0
  end
  else begin
    Tmp := S;
    P1 := AnsiPos(OldString, S);
    if (P1 > 0) then begin
      Result := Copy(Tmp, 1, P1-1);
      C := 0;
      while (P1 > 0) do begin
        for I := 1 to Replacements do begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1+Length(OldString), MaxLongInt);
        P1 := AnsiPos(OldString, Tmp);
        if (P1 > 0) then begin
          Result := Result + Copy(Tmp, 1, P1-1);
        end else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function LastWordL(const S, WordDelims, AWord : AnsiString;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : AnsiString;
  I      : Cardinal;
begin
  if (S = '') or (WordDelims = '') or
     (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (pos(TmpStr[I], WordDelims) > 0) do begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then begin
      Inc(Position);
      Result := True;
      Exit;
    end;
    System.Delete(TmpStr, Position, Length(TmpStr));
    Position := Length(TmpStr);
  until (Length(TmpStr) = 0);
  Result := False;
  Position := 0;
end;



function LastWordAbsL(const S, WordDelims : AnsiString;
                      var Position : Cardinal) : Boolean;
begin
  if (S = '') or (WordDelims = '') then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then begin
    Result := True;
    Position := 1;
    Exit;
  end;

{find next delimiter character}
  while (Position > 0) and (pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringL(const S, AString : AnsiString;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : AnsiString;
  I, C   : Cardinal;
begin
  if (S = '') or (AString = '') or (pos(AString, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := pos(AString, TmpStr);
  while (I > 0) do begin
    Inc(C, LongInt(I) + Length(AString));
    System.Delete(TmpStr, 1, LongInt(I) + Length(AString));
    I := pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsL(const S, Chars : AnsiString) : AnsiString;
var
  FromInx : Cardinal;
  ToInx   : Cardinal;
begin
  {if either the input string or the list of acceptable chars is empty
   the destination string will also be empty}
  if (S = '') or (Chars = '') then begin
    Result := '';
    Exit;
  end;

  {set the maximum length of the result string (it could be less than
   this, of course}
  SetLength(Result, length(S));

  {start off the to index}
  ToInx := 0;

  {in a loop, copy over the chars that match the list}
  for FromInx := 1 to length(S) do
    if CharExistsL(Chars, S[FromInx]) then begin
      inc(ToInx);
      Result[ToInx] := S[FromInx];
    end;

  {make sure that the length of the result string is correct}
  SetLength(Result, ToInx);
end;



function RepeatStringL(const RepeatString : AnsiString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : AnsiString;
var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := '';
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (RepeatString <> '') then begin
    Len := length(RepeatString);
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      SetLength(Result, ActualReps * Len);
      for i := 0 to pred(ActualReps) do
        Move(RepeatString[1], Result[i * Len + 1], Len);
    end;
  end;
end;



function TrimCharsL(const S, Chars : AnsiString) : AnsiString;
begin
  Result := RightTrimCharsL(S, Chars);
  Result := LeftTrimCharsL(Result, Chars);
end;



function RightTrimCharsL(const S, Chars : AnsiString) : AnsiString;
var
  CutOff : integer;
begin
  CutOff := length(S);
  while (CutOff > 0) do begin
    if not CharExistsL(Chars, S[CutOff]) then
      Break;
    dec(CutOff);
  end;
  if (CutOff = 0) then
    Result := ''
  else
    Result := Copy(S, 1, CutOff);
end;



function LeftTrimCharsL(const S, Chars : AnsiString) : AnsiString;
var
  CutOff : integer;
  LenS   : integer;
begin
  LenS := length(S);
  CutOff := 1;
  while (CutOff <= LenS) do begin
    if not CharExistsL(Chars, S[CutOff]) then
      Break;
    inc(CutOff);
  end;
  if (CutOff > LenS) then
    Result := ''
  else
    Result := Copy(S, CutOff, LenS - CutOff + 1);
end;



function ExtractTokensL(const S, Delims: AnsiString;
                        QuoteChar  : AnsiChar;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;

var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : AnsiChar;
  TokenStart : integer;
  Inx        : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or
     (Delims = '') or
     CharExistsL(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart :
        begin
          {if the current char is the quote character, switch states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanQuotedToken

          {if the current char is a delimiter, output a null token}
          else if CharExistsL(Delims, CurChar) then begin

            {if allowed to, output a null token}
            if AllowNulls then begin
              Tokens.Add('');
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);
          end

          {otherwise, the current char is starting a normal token, so
           switch states}
          else
            State := ScanNormalToken
        end;

      ScanQuotedToken :
        begin
          {if the current char is the quote character, switch states}
          if (CurChar = QuoteChar) then
            State := ScanQuotedTokenEnd
        end;

      ScanQuotedTokenEnd :
        begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
          if (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
          else if CharExistsL(Delims, CurChar) then begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
            if ((Inx - TokenStart) = 2) then begin
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end
            end

            {else output the token without the quotes}
            else begin
              Tokens.Add(Copy(S, succ(TokenStart), Inx - TokenStart - 2));
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end

          {otherwise it's a (complex) normal token, so switch states}
          else
            State := ScanNormalToken
        end;

      ScanNormalToken :
        begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token}
          else if CharExistsL(Delims, CurChar) then begin
            Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
            inc(Result);

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end;
        end;

      ScanNormalTokenWithQuote :
        begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
          if (CurChar = QuoteChar) then
            State := ScanNormalToken;
        end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then begin
    inc(TokenStart);
    dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then begin
    Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
    inc(Result);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then begin
    Tokens.Add('');
    inc(Result);
  end;
end;



function ContainsOnlyL(const S, Chars : AnsiString;
                       var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (not CharExistsL(Chars, S[I])) then begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanL(const S, Chars : AnsiString;
                            var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (CharExistsL(Chars, S[I])) then begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;



function IsChAlphaL(C : AnsiChar) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := IsCharAlpha(@C);
end;

function IsChNumericL(C : AnsiChar; Numbers : AnsiString) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := CharExistsL(Numbers, C);
end;



function IsChAlphaNumericL(C : AnsiChar; Numbers : AnsiString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := IsCharAlpha(@C) or CharExistsL(Numbers, C);
end;



function IsStrAlphaL(const S : AnsiString) : Boolean;
  {-Returns true if all characters in string are an alpha}
var
  I : Cardinal;
begin
  Result := False;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if not IsCharAlpha(@S[I]) then
        Exit;
    Result := True;
  end;
end;



function IsStrNumericL(const S, Numbers : AnsiString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
var
  i : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for i := 1 to Length(S) do
      if not CharExistsL(Numbers, S[i]) then
        Exit;
    Result := true;
  end;
end;



function IsStrAlphaNumericL(const S, Numbers : AnsiString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
var
  i : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if (not IsCharAlpha(@S[i])) and
         (not CharExistsL(Numbers, S[i])) then
        Exit;
    Result := true;
  end;
end;


function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : string;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start-1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then begin
    Position := Position + Start - 1;
    Result := True;
  end else
    Result := False;
end;

  {-------- Numeric conversion -----------}


function HexBW(B : Byte) : WideString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := WideChar(StHexDigits[B shr 4]);
  Result[2] := WideChar(StHexDigits[B and $F]);
end;

function HexWW(W : Word) : WideString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := WideChar(StHexDigits[hi(W) shr 4]);
  Result[2] := WideChar(StHexDigits[hi(W) and $F]);
  Result[3] := WideChar(StHexDigits[lo(W) shr 4]);
  Result[4] := WideChar(StHexDigits[lo(W) and $F]);
end;

function HexLW(L : LongInt) : WideString;
  { -Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWW(HiWord(L)) + HexWW(LoWord(L));
end;

function HexPtrW(P : Pointer) : WideString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLW(LongInt(P));
end;

function BinaryBW(B : Byte) : WideString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(B and (1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function BinaryWW(W : Word) : WideString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(W and (1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function BinaryLW(L : LongInt) : WideString;
  {-Return the binary string for a long integer.}
var
  I : Longint;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function OctalBW(B : Byte) : WideString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := WideChar(StHexDigits[B and 7]);
    B := B shr 3;
  end;
end;

function OctalWW(W : Word) : WideString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := WideChar(StHexDigits[W and 7]);
    W := W shr 3;
  end;
end;

function OctalLW(L : LongInt) : WideString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := WideChar(StHexDigits[L and 7]);
    L := L shr 3;
  end;
end;

function Str2Int16W(const S : WideString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

var
  ec : Integer;
begin
  if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValSmallint(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2WordW(const S : WideString; var I : Word) : Boolean;
  {-Convert a string to a word.}

var
  ec : Integer;
begin
  if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValWord(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2LongW(const S : WideString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

var
  ec : Integer;
begin
   if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValLongint(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2RealW(const S : WideString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailW(S);
  if St = '' then Exit;
  Val(ValPrepW(St), R, Code);
  if Code <> 0 then begin
    R := Code;
  end else
    Result := True;
end;

function Str2ExtW(const S : WideString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P    : WideString;
begin
  Result := False;
  if S = '' then Exit;
  P := TrimTrailW(S);
  if P = '' then Exit;
  Val(ValPrepW(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
  end else
    Result := True;
end;

function Long2StrW(L : LongInt) : WideString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrW(R : Double; Width : Byte; Places : ShortInt) : WideString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrW(R : Extended; Width : Byte; Places : ShortInt) : WideString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepW(const S : WideString) : WideString;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
  C : Longint;
begin
  Result := TrimSpacesW(S);
  if Result <> '' then begin
    if StrChPosW(Result, WideChar(DecimalSeparator), P) then begin
      C := P;
      Result[C] := '.';
      if C = Length(Result) then
        SetLength(Result, Pred(C));
    end;
  end else
    Result := '0';
end;

  {-------- General purpose string manipulation --------}

function CharStrW(C : WideChar; Len : Cardinal) : WideString;
  {-Return a string filled with the specified character.}
var
  I : Longint;
begin
  SetLength(Result, Len);
  if Len <> 0 then begin
    {FillChar does not work for widestring}
    for I := 1 to Len do
      Result[I] := C;
  end;
end;


function PadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the right with a specified character.}
var
  J,
  R  : Longint;
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else begin
    SetLength(Result, Len);

    { copy current contents (if any) of S to Result }
    if (Length(S) > 0) and (Length(Result) > 0) then                     {!!.01}
      Move(S[1], Result[1], Length(S)*SizeOf(WideChar));                 {!!.01}

    R := longint(Len) - Length(S);
    J := Succ(Length(S));
    while (R > 0) do begin
      Result[J] := C;
      Inc(J);
      Dec(R);
   end;
  end;
end;

function PadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChW(S, ' ', Len);
end;

function LeftPadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left with a specified character.}
var
  J,
  R  : Longint;
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    if (Length(S) > 0) and (Length(Result) > 0) then                     {!!.01}
      Move(S[1], Result[Succ(Word(Len))-Length(S)],                      {!!.01}
        Length(S)*SizeOf(WideChar));                                     {!!.01}
    R := longint(Len) - Length(S);
    J := 1;
    while (R > 0) do begin
      Result[J] := C;
      Inc(J);
      Dec(R);
    end;
  end;
end;

function LeftPadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChW(S, ' ', Len);
end;

function TrimLeadW(const S : WideString) : WideString;
  {-Return a string with leading white space removed}
var
  I : Longint;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  SetLength(Result, Length(S)-Pred(I));
  if Length(Result) > 0 then                                             {!!.01}
    Move(S[I], Result[1], Length(S)-Pred(I)*SizeOf(WideChar));           {!!.01}
end;

function TrimTrailW(const S : WideString) : WideString;
  {-Return a string with trailing white space removed.}
var
  L : Longint;
begin
  Result := S;
  L := Length(Result);
  while (L > 0) and (Result[L] <= ' ') do
    Dec(L);
  SetLength(Result, L);
end;

function TrimW(const S : WideString) : WideString;
  {-Return a string with leading and trailing white space removed.}
var
  I : Longint;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] <= ' ') do
    Dec(I);
  SetLength(Result, I);

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if (I > 0) then
    System.Delete(Result, 1, I);
end;

function TrimSpacesW(const S : WideString) : WideString;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Longint;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] = ' ') do
    Dec(I);
  SetLength(Result, I);

  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if (I > 0) then
    System.Delete(Result, 1, I);
end;

function CenterChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    Result := CharStrW(C, Len);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[Succ((LongInt(Len)-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChW(S, ' ', Len);
end;

function EntabW(const S : WideString; TabSize : Byte) : WideString;
  {-Convert blanks in a string to tabs.}
const
  WSpace = WideChar(#32);
  WTab   = WideChar(#9);
var
  Col,
  CP,
  OP,
  Spaces  : Longint;
begin
  if (pos(' ', S) = 0) then begin
    Result := S;
    Exit;
  end;
  Result := '';
  Col := 1;
  repeat
     CP := Col;
     while ((S[CP] <> WSpace) and (CP <= Length(S))) do
       Inc(CP);
     if (CP <> Col) then begin
       OP := Length(Result) + 1;
       SetLength(Result, Length(Result) + (CP-Col));
       Move(S[Col], Result[OP], ((CP-Col) * 2));
       Col := CP;
     end;

     while (S[CP] = WSpace) do begin
       Inc(CP);
       if ((CP mod TabSize) = 1) then begin
         Result := Result + WTab;
         Col := CP;
       end;
     end;
     Spaces := 0;
     while (Col < CP) do begin
       Inc(Spaces);
       Inc(Col);
     end;
     if (Spaces > 0) then
       Result := Result + PadW(WSpace, Spaces);
  until (Col > Length(S));
end;


function DetabW(const S : WideString; TabSize : Byte) : WideString;
  {-Expand tabs in a string to blanks.}
var
  Col,
  CP,
  OP,
  Spaces  : Longint;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end else if (TabSize = 0) then begin
    Result := S;
    Exit;
  end;
  if (CharCountW(S, WideChar(#9)) = 0) then begin
    Result := S;
    Exit;
  end;
  Result := '';

  Col := 1;
  while (Col <= Length(S)) do begin
    if (S[Col] = WideChar(#9)) then begin
      Spaces := 0;
      repeat
        Inc(Spaces);
      until (((Col + Spaces) mod TabSize) = 1);
      Inc(Col);
      Result := PadW(Result, Length(Result) + Spaces);
    end else begin
      CP := Col;
      repeat
        Inc(Col);
      until (Col > Length(S)) or (S[Col] = WideChar(#9));
      OP := Length(Result) + 1;
      SetLength(Result, Length(Result) + (Col - CP));
      Move(S[CP], Result[OP], (Col-CP)*2);
    end;
  end;
end;


function ScrambleW(const S, Key : WideString) : WideString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  I, J, LKey, LStr : Cardinal;
begin
  Result := S;
  if Key = '' then Exit;
  if S = '' then Exit;
  LKey := Length(Key);
  LStr := Length(S);
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := WideChar(Word(S[I]) xor Word(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteW(const S, FromStr, ToStr : WideString) : WideString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  I : Cardinal;
  P : Cardinal;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      if StrChPosW(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterW(const S, Filters : WideString) : WideString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsW(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsW(const S : WideString; C : WideChar) : Boolean;
  {-see if character exists at least one in a string}
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(S) do
    if (S[i] = C) then
      Exit;
  Result := false;
end;

function CharCountW(const S : WideString; C : WideChar) : Cardinal;
  {-Count the number of a given characters in a string. }
var
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do
    if (S[i] = C) then
      inc(Result);
end;

function WordCountW(const S, WordDelims : WideString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I    : Cardinal;
  SLen : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsW(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsW(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionW(N : Cardinal; const S, WordDelims : WideString;
                       var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  Count : Longint;
  I     : Longint;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsW(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and not CharExistsW(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordW(N : Cardinal; const S, WordDelims : WideString) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  C   : Cardinal;
  I,
  J   : Longint;
begin
  Result := '';
  if WordPositionW(N, S, WordDelims, C) then begin
    I := C;
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not
           CharExistsW(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * 2);
  end;
end;

function AsciiCountW(const S, WordDelims : WideString; Quote : WideChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Longint;
  InQuote : Boolean;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= Length(S) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote)
      and CharExistsW(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Result);
    {find the end of the current word}
    while (I <= Length(S)) and
          (InQuote or not CharExistsW(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionW(N : Cardinal; const S, WordDelims : WideString;
                        Quote : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  I,
  Count    : Longint;
  InQuote  : Boolean;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote) and
          CharExistsW(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and
            (InQuote or not CharExistsW(WordDelims, S[I])) do begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractAsciiW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  C       : Cardinal;
  I, J    : Longint;
  InQuote : Boolean;
begin
  InQuote := False;
  if AsciiPositionW(N, S, WordDelims, Quote, C) then begin
    I := C;
    J := I;
    {find the end of the current word}
    while (I <= Length(S)) and ((InQuote)
      or not CharExistsW(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not(InQuote);
      Inc(I);
    end;
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * 2);
  end;
end;

procedure WordWrapW(const InSt : WideString; var OutSt, Overlap : WideString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Cardinal;
  EOS,
  BOS      : Cardinal;
  ASpace   : WideChar;
begin
  InStLen := Length(InSt);
  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  SetLength(OutSt, EOS);
  Move(InSt[1], OutSt[1], Length(OutSt) * 2);

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}

    SetLength(OverLap, InStLen);
    Move(InSt[BOS], Overlap[1], Succ(InStLen-BOS) * 2);
    SetLength(OverLap, Succ(InStLen-BOS));
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (Length(OutSt) < LongInt(Margin)) then begin
    SetLength(OutSt, Margin);
    ASpace := ' ';
    StUtils.FillWord(OutSt[Succ(Length(OutSt))],
                     LongInt(Margin)-Length(OutSt), Word(ASpace));
  end;
end;

  {--------------- String comparison and searching -----------------}

function CompStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings.}
begin
  Result := CompareStr(S1, S2);
end;

function CompUCStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}
begin
  Result := CompareText(S1, S2);
end;

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionW(const Name, Ext : WideString) : WideString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionW(const Name, Ext : WideString) : WideString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := System.Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameW(const PathName : WideString) : WideString;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (pos(PathName[I], DosDelimSetW) > 0)                  {!!.01}
    or (PathName[I] = #0);                                               {!!.01}
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameW(const PathName : WideString) : WideString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
  S      : WideString;
begin
  S := JustFileNameW(PathName);
  if HasExtensionW(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;

function JustExtensionW(const Name : WideString) : WideString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := System.Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameW(const PathName : WideString) : WideString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;

  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (pos(PathName[I], DosDelimSetW) > 0)                  {!!.01}
    or (PathName[I] = #0);                                               {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = PathDelim {'\'}) then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := System.Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := System.Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := System.Copy(PathName, 1, I);
end;


function AddSlashW(const DirName : WideString) : WideString;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then begin
    Result := PathDelim;
  end
  else
  if (Result[Length(Result)] <> PathDelim) then
    Result := Result + PathDelim;
end;

function HasExtensionW(const Name : WideString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    and not CharExistsW(System.Copy(Name, Succ(DotPos), StMaxFileLen), PathDelim {'\'});
end;

  {------------------ Formatting routines --------------------}


function CommaizeChW(L : Longint; Ch : WideChar) : WideString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp       : WideString;
  I,
  Len,
  NumCommas  : Cardinal;
  Neg        : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if (L < 0) then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrW(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeW(L : LongInt) : WideString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChW(L, ',');
end;

function FormPrimW(const Mask     : WideString;
                         R        : TstFloat;
                   const LtCurr,
                         RtCurr   : WideString;
                         Sep,
                         DecPt    : WideChar;
                         AssumeDP : Boolean) : WideString;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars  : string[8] = '#@*$-+,.';
  PlusArray  : array[Boolean] of WideChar = ('+', '-');
  MinusArray : array[Boolean] of WideChar = (' ', '-');
  FillArray  : array[Blank..Zero] of WideChar = (' ', '*', '0');
var
  S            : WideString; {temporary string}
  Filler       : Integer;    {char for unused digit slots: ' ', '*', '0'}
  WontFit,                   {true if number won't fit in the mask}
  AddMinus,                  {true if minus sign needs to be added}
  Dollar,                    {true if floating dollar sign is desired}
  Negative     : Boolean;    {true if B is negative}
  StartF,                    {starting point of the numeric field}
  EndF         : Longint;    {end of numeric field}
  RtChars,                   {# of chars to add to right}
  LtChars,                   {# of chars to add to left}
  DotPos,                    {position of '.' in Mask}
  Digits,                    {total # of digits}
  Blanks,                    {# of blanks returned by Str}
  Places,                    {# of digits after the '.'}
  FirstDigit,                {pos. of first digit returned by Str}
  Extras,                    {# of extra digits needed for special cases}
  DigitPtr     : Byte;       {pointer into temporary string of digits}
  I            : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsW(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result <> '') then begin
    SetLength(Result, Succ(Length(Result)));
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do begin
    if Result[I] = 'C' then begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end else if Result[I] = 'c' then begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result))
    {and (System.Pos(Result[StartF], FormChars) = 0) do}
    and not CharExistsW(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    EndF := I;
    case Result[EndF] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := EndF;
    else
      goto EndFound;
    end;
    {Inc(EndF);}
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

EndFound:
  {if we jumped to here instead, it wasn't}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R:Digits:Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    I := Length(S);
    SetLength(S, LongInt(I) + (Places-MaxPlaces));
    StUtils.FillWord(S[Succ(I)], Places-MaxPlaces, Word(WideChar('0')));
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillWord(S[1], Blanks, Word(FillArray[Filler]));

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if DigitPtr <> 0 then begin
          Result[I] := S[DigitPtr];
          Dec(DigitPtr);
          if (DigitPtr <> 0) and (S[DigitPtr] = '.') then                {!!.01}
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    SetLength(Result, Pred(Length(Result)));
  if RtChars > 0 then begin
    S := RtCurr;
    if Length(S) > RtChars then
      SetLength(S, RtChars)
    else
      S := LeftPadW(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Length(S) > LtChars then
      SetLength(S, LtChars)
    else
      S := PadW(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormW(const Mask     : WideString;
                          R        : TstFloat ;
                    const LtCurr,
                          RtCurr   : WideString;
                          Sep,
                          DecPt    : WideChar) : WideString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimW(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormW(const Mask    : WideString;
                            L       : Longint;
                      const LtCurr,
                            RtCurr  : WideString;
                            Sep     : WideChar) : WideString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimW(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosW(const P : WideString; C : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(P) do
    if (P[i] = C) then begin
      Pos := i;
      Exit;
    end;
  Result := false;
  Pos := 0;
end;

function StrStPosW(const P, S : WideString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertW(const S : WideString; C : WideChar; Pos : Cardinal) : WideString;
  {-Insert a character into a string at a specified position.}
var
  Temp : WideString;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertW(const S1, S2 : WideString; Pos : Cardinal) : WideString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteW(const S : WideString; Pos : Cardinal) : WideString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;




function CopyLeftW(const S : WideString; Len : Cardinal) : WideString;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;



function CopyMidW(const S : WideString; First, Len : Cardinal) : WideString;
  {-Return the mid part of a string}
begin
  if (LongInt(First) > Length(S)) or (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;



function CopyRightW(const S : WideString; First : Cardinal) : WideString;
  {-Return the right Len characters of a string}
begin
  if (LongInt(First) > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;


function CopyRightAbsW(const S : WideString; NumChars : Cardinal) : WideString;
  {-Return NumChar characters starting from end}
begin
  if (Cardinal(Length(S)) > NumChars) then
    Result := Copy(S, (Cardinal(Length(S)) - NumChars)+1, NumChars)
  else
    Result := S;
end;


function WordPosW(const S, WordDelims, AWord : WideString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : WideString;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I      := 0;
  Len    := Length(AWord);
  P1     := pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then begin
      if (pos(TmpStr[P2+1], WordDelims) > 0) then begin
        Inc(I);
      end else
        System.Delete(TmpStr, 1, P2);
    end else if (pos(TmpStr[P1-1], WordDelims) > 0) and
                ((pos(TmpStr[P2+1], WordDelims) > 0) or
                 (LongInt(P2+1) = Length(TmpStr))) then begin
      Inc(I);
    end else if ((LongInt(P1) + LongInt(pred(Len))) = Length(TmpStr)) then begin
      if (P1 > 1) and (pos(TmpStr[P1-1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := pos(AWord, TmpStr);
  end;
end;




function CopyFromNthWordW(const S, WordDelims : WideString;
                          AWord : WideString; N : Cardinal;
                          var SubString : WideString) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosW(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;



function DeleteFromNthWordW(const S, WordDelims : WideString;
                            AWord : WideString; N : Cardinal;
                            var SubString : WideString) : Boolean;
var
  P : Cardinal;
begin
  SubString := S;
  if (WordPosW(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                         N1, N2 : Cardinal;
                         var SubString : WideString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosW(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosW(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        SubString := Copy(S, P1, P2-P1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function DeleteFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                           N1, N2 : Cardinal;
                           var SubString : WideString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosW(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosW(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        System.Delete(SubString, P1, P2-P1+1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyWithinW(const S, Delimiter : WideString;
                     Strip : Boolean) : WideString;
var
  P1,
  P2     : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosW(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if StrStPosW(TmpStr, Delimiter, P2) then begin
        Result := Copy(TmpStr, 1, P2-1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end else begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;



function DeleteWithinW(const S, Delimiter : WideString) : WideString;
var
  P1,
  P2     : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosW(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if (pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosW(TmpStr, Delimiter, P2)) then begin
          P2 := LongInt(P2) + (2*Length(Delimiter));
          Result := S;
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;



function ReplaceWordW(const S, WordDelims, OldWord, NewWord : WideString;
                      N : Cardinal;
                      var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (pos(OldWord, S) = 0) then begin
    Result := '';
    Replacements := 0;
    Exit;
  end;

  if (WordPosW(S, WordDelims, OldWord, N, P1)) then begin
    Result := S;
    System.Delete(Result, P1, Length(OldWord));

    C := 0;
    for I := 1 to Replacements do begin
      if (((Length(NewWord)) + Length(Result)) < MaxLongInt) then begin
        Inc(C);
        System.Insert(NewWord, Result, P1);
        Inc(P1, Length(NewWord) + 1);
      end else begin
        Replacements := C;
        Exit;
      end;
    end;
  end else begin
    Result := S;
    Replacements := 0;
  end;
end;


function ReplaceWordAllW(const S, WordDelims, OldWord, NewWord : WideString;
                         var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    Result := S;
    C := 0;
    while (WordPosW(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) <= 255) then begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


function ReplaceStringW(const S, OldString, NewString : WideString;
                        N : Cardinal;
                        var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (OldString = '') or (pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I  := 1;
  P1 := pos(OldString, TmpStr);
  C  := P1;
  while (I < N) and (LongInt(C) < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, LongInt(P1) + Length(OldString));
    Inc(C, LongInt(P1) + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if (((Length(NewString)) + Length(Result)) < MaxLongInt) then begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end else begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllW(const S, OldString, NewString : WideString;
                           var Replacements : Cardinal) : WideString;
var
  I,
  C  : Cardinal;
  P1 : longint;
  Tmp: WideString;
begin
  Result := S;
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
    Replacements := 0
  else begin
    Tmp := S;
    P1 := AnsiPos(OldString, S);
    if (P1 > 0) then begin
      Result := Copy(Tmp, 1, P1-1);
      C := 0;
      while (P1 > 0) do begin
        for I := 1 to Replacements do begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1+Length(OldString), MaxLongInt);
        P1 := AnsiPos(OldString, Tmp);
        if (P1 > 0) then begin
          Result := Result + Copy(Tmp, 1, P1-1);
        end else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function LastWordW(const S, WordDelims, AWord : WideString;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : WideString;
  I      : Cardinal;
begin
  if (S = '') or (WordDelims = '') or
     (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (pos(TmpStr[I], WordDelims) > 0) do begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then begin
      Inc(Position);
      Result := True;
      Exit;
    end;
    System.Delete(TmpStr, Position, Length(TmpStr));
    Position := Length(TmpStr);
  until (Length(TmpStr) = 0);
  Result := False;
  Position := 0;
end;



function LastWordAbsW(const S, WordDelims : WideString;
                      var Position : Cardinal) : Boolean;
begin
  if (S = '') or (WordDelims = '') then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then begin
    Result := True;
    Position := 1;
    Exit;
  end;

{find next delimiter character}
  while (Position > 0) and (pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringW(const S, AString : WideString;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : WideString;
  I, C   : Cardinal;
begin
  if (S = '') or (AString = '') or (pos(AString, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := pos(AString, TmpStr);
  while (I > 0) do begin
    Inc(C, LongInt(I) + Length(AString));
    System.Delete(TmpStr, 1, LongInt(I) + Length(AString));
    I := pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsW(const S, Chars : WideString) : WideString;
var
  P1,
  P2  : Cardinal;
begin
  if (S = '') or (Chars = '') then begin
    Result := '';
    Exit;
  end;

  Result := '';
  P1 := 1;
  P2 := 1;
  repeat
    while (pos(S[P2], Chars) > 0) and (LongInt(P2) <= Length(S)) do
      Inc(P2);
    Result := Result + Copy(S, P1, P2-P1);
    P1 := P2+1;
    P2 := P1;
    while (pos(S[P2], Chars) = 0) and (LongInt(P2) <= Length(S)) do
      Inc(P2);
    P1 := P2;
  until (LongInt(P1) > Length(S));
end;



function RepeatStringW(const RepeatString : WideString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : WideString;
var
  J,
  Len : Cardinal;
begin
  Len := Length(RepeatString);
  Repetitions := MaxLen div Len;
  SetLength(Result, Repetitions * Len);
  for J := 0 to pred(Repetitions) do
    Move(RepeatString[1], Result[J * Len + 1], Len*2);
end;



function TrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := RightTrimCharsW(S, Chars);
  Result := LeftTrimCharsW(Result, Chars);
end;



function RightTrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := S;
  while (pos(Result[Length(Result)], Chars) > 0) do
    System.Delete(Result, Length(Result), 1);
end;



function LeftTrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := S;
  while (pos(Result[1], Chars) > 0) do
    System.Delete(Result, 1, 1);
end;



function ExtractTokensW(const S, Delims: WideString;
                        QuoteChar  : WideChar;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : WideChar;
  TokenStart : integer;
  Inx        : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or
     (Delims = '') or
     CharExistsW(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart :
        begin
          {if the current char is the quote character, switch states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanQuotedToken

          {if the current char is a delimiter, output a null token}
          else if CharExistsW(Delims, CurChar) then begin

            {if allowed to, output a null token}
            if AllowNulls then begin
              Tokens.Add('');
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);
          end

          {otherwise, the current char is starting a normal token, so
           switch states}
          else
            State := ScanNormalToken
        end;

      ScanQuotedToken :
        begin
          {if the current char is the quote character, switch states}
          if (CurChar = QuoteChar) then
            State := ScanQuotedTokenEnd
        end;

      ScanQuotedTokenEnd :
        begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
          if (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
          else if CharExistsW(Delims, CurChar) then begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
            if ((Inx - TokenStart) = 2) then begin
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end
            end

            {else output the token without the quotes}
            else begin
              Tokens.Add(Copy(S, succ(TokenStart), Inx - TokenStart - 2));
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end

          {otherwise it's a (complex) normal token, so switch states}
          else
            State := ScanNormalToken
        end;

      ScanNormalToken :
        begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token}
          else if CharExistsW(Delims, CurChar) then begin
            Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
            inc(Result);

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end;
        end;

      ScanNormalTokenWithQuote :
        begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
          if (CurChar = QuoteChar) then
            State := ScanNormalToken;
        end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then begin
    inc(TokenStart);
    dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then begin
    Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
    inc(Result);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then begin
    Tokens.Add('');
    inc(Result);
  end;
end;



function ContainsOnlyW(const S, Chars : WideString;
                       var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (not CharExistsW(Chars, S[I])) then begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanW(const S, Chars : WideString;
                            var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (CharExistsW(Chars, S[I])) then begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;



function IsChAlphaW(C : WideChar) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := IsCharAlphaW(@C);
end;



function IsChNumericW(C : WideChar; Numbers : WideString) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := pos(C, Numbers) > 0;
end;



function IsChAlphaNumericW(C : WideChar; Numbers : WideString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := (IsCharAlphaW(@C)) or (pos(C, Numbers) > 0);
end;



function IsStrAlphaW(const S : WideString) : Boolean;
  {-Returns true if all characters in string are an alpha}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := IsCharAlphaW(@S[I]);
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;



function IsStrNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := pos(S[I], Numbers) > 0;
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;


function IsStrAlphaNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := (IsCharAlphaW(@S[I])) or (pos(S[I], Numbers) > 0);
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;

function StrWithinW(const S, SearchStr : WideString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : WideString;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start-1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then begin
    Position := Position + Start - 1;
    Result := True;
  end else
    Result := False;
end;

function HexBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
    {-Return hex string for byte}
begin
  Result := Dest;
  Dest^ := StHexDigits[B shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[B and $F];
  Inc(Dest);
  Dest^ := #0;
end;

function HexWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return hex string for word}
begin
  Result := Dest;
  Dest^ := StHexDigits[hi(W) shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[hi(W) and $F];
  Inc(Dest);
  Dest^ := StHexDigits[lo(W) shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[lo(W) and $F];
  Inc(Dest);
  Dest^ := #0;
end;

function HexLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Return hex string for LongInt}
type
  LH = record L, H : word; end;
var
  T2 : Array[0..4] of AnsiChar;
begin
  Result := StrCat(HexWZ(Dest, LH(L).H), HexWZ(T2, LH(L).L));
end;

function HexPtrZ(Dest : PAnsiChar; P : Pointer) : PAnsiChar;
  {-Return hex string for pointer}
var
  T2 : array[0..8] of AnsiChar;
begin
  StrCopy(Dest, ':');
  Result := StrCat(Dest, HexLZ(T2, LongInt(P)));
end;

function BinaryBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return binary string for byte}
var
  I : Word;
begin
  Result := Dest;
  for I := 7 downto 0 do begin
    Dest^ := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function BinaryWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return binary string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 15 downto 0 do begin
    Dest^ := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function BinaryLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Return binary string for LongInt}
var
  I : Longint;
begin
  Result := Dest;
  for I := 31 downto 0 do begin
    Dest^ := StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function OctalBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return octal string for byte}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 2 do begin
    Dest[2-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
  Dest[3] := #0;
end;

function OctalWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return octal string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 5 do begin
    Dest[5-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
  Dest[6] := #0;
end;

function OctalLZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Return octal string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 11 do begin
    Dest[11-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
  Dest[12] := #0;
end;

function CharStrZ(Dest : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar; assembler;
asm
  push    edi            { Save EDI-about to change it }
  push    eax            { Save Dest pointer for return }
  mov     edi, eax       { Point EDI to Dest }

  mov     dh, dl         { Dup character 4 times }
  mov     eax, edx
  shl     eax, $10
  mov     ax, dx

  mov     edx, ecx       { Save Len }

  shr     ecx, 2         { Store dword char chunks first }
  rep     stosd
  mov     ecx, edx       { Store remaining characters }
  and     ecx, 3
  rep     stosb

  xor     al,al          { Add null terminator }
  mov     [edi], al

  pop     eax            { Return Dest pointer }
  pop     edi            { Restore orig value of EDI }
end;

function PadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar; Assembler;
asm
  push   eax
  push   ebx
  push   edi

  mov    edi, eax
  mov    ebx, ecx
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  dec    edi
  mov    eax, ebx
  sub    eax, ecx
  jbe    @@ExitPoint

  mov    ecx, eax
  mov    eax, edx
  rep    stosb

@@ExitPoint:
  xor    eax, eax
  mov    [edi], al

  pop    edi
  pop    ebx
  pop    eax
end;

function PadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string right-padded to length len with blanks}
begin
  Result := PadChPrimZ(S, ' ', Len);
end;

function LeftPadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar; Assembler;
  {-Return a string left-padded to length len with C}
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, edi
  mov    ebx, ecx

  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    eax, ebx
  mov    edi, esi
  add    edi, ebx
  mov    ebx, esi
  sub    eax, ecx
  jbe    @@ExitPoint

  add    esi, ecx
  inc    ecx
  std
  rep    movsb
  mov    ecx, eax
  mov    eax, edx
  rep    stosb

@@ExitPoint:
  cld
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;

function LeftPadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with blanks}
begin
  Result := LeftPadChPrimZ(S, ' ', Len);
end;

function PadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a PChar right-padded to length Len with C}
begin
  StrCopy(Dest, S);
  Result := PadChPrimZ(Dest, C, Len);
end;

function PadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string right-padded to length len with blanks}
begin
  StrCopy(Dest, S);
  Result := PadPrimZ(Dest, Len);
end;

function LeftPadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with C}
begin
  StrCopy(Dest, S);
  Result := LeftPadChPrimZ(Dest, C, Len);
end;

function LeftPadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with blanks}
begin
  StrCopy(Dest, S);
  Result := LeftPadPrimZ(Dest, Len);
end;

function TrimLeadPrimZ(S : PAnsiChar) : PAnsiChar; Assembler;
  {-Return a string with leading white space removed}
asm
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  mov    edx, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, edx
  jz     @@CopyRest

@@Lo:
  cmp    byte ptr [esi], ' '
  ja     @@CopyRest
  inc    esi
  dec    ecx
  jnz    @@Lo

@@CopyRest:
  inc    ecx
  rep    movsb
  mov    eax, edx

  pop    esi
  pop    edi
end;

function TrimLeadZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed}
begin
  StrCopy(Dest, S);
  Result := TrimLeadPrimZ(Dest);
end;

function TrimTrailPrimZ(S : PAnsiChar) : PAnsiChar; Assembler;
  {-Return a string with trailing white space removed}
asm
  push   edi

  mov    edi, eax
  mov    edx, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  jz     @@ExitPoint
  dec    edi
  dec    edi

@@Lo:
  cmp    BYTE PTR [edi], ' '
  ja     @@AllDone
  dec    edi
  dec    ecx
  jnz    @@Lo

@@AllDone:
  inc    edi
  mov    byte ptr [edi], 0h

@@ExitPoint:
  mov    eax, edx
  pop    edi
end;

function TrimTrailZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed}
begin
  StrCopy(Dest, S);
  Result := TrimTrailPrimZ(Dest);
end;

function TrimPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed}
begin
  Result := TrimTrailPrimZ(TrimLeadPrimZ(S));
end;

function TrimZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed}
begin
  StrCopy(Dest, S);
  Result := TrimPrimZ(Dest);
end;

function TrimSpacesPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed}
var
  I, SLen : Cardinal;
begin
  Result := S;
  SLen := StrLen(S);
  while (SLen > 0) and (S[SLen-1] = ' ') do
    Dec(SLen);
  S[SLen] := #0;
  I := 0;
  while (I < SLen) and (S[I] = ' ') do
    Inc(I);
  if I > 0 then
    StrStDeletePrimZ(S, 0, I);
end;

function TrimSpacesZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed}
begin
  StrCopy(Dest, S);
  Result := TrimSpacesPrimZ(Dest);
end;

function CenterChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar; Assembler;
  {-Return a string centered in a string of C with specified width}
asm
  push   eax                 { save registers }
  push   ebx
  push   edi
  push   esi

  mov    edi, eax            { set EDI and ESI to S }
  mov    esi, eax
  mov    ebx, ecx            { store Len in EBX }
  xor    eax, eax
  or     ecx, -1
  repne  scasb               { Find null terminator in S }
  not    ecx
  dec    ecx                 { ECX has length of S }
  jz     @@SpecialCase       { if zero, jump to special case }

  cmp    ecx, ebx
  jae    @@ExitPoint         { if Len >= Length(S), we're done }

  mov    eax, ebx            { copy Len to EAX }
  sub    ebx, ecx            { EBX = number of pad characters }
  inc    ebx
  shr    ebx, 1              { EBX = number of pad characters on one side }
  sub    eax, ebx
  sub    eax, ecx
  push   eax
  add    esi, ecx            { set ESI to end of text in S }
  mov    edi, esi
  add    edi, ebx            { set EDI to end of destination }
  dec    esi
  push   edi
  dec    edi
  std                        { Backward string ops }
  rep    movsb               { move string }
  mov    eax, edx            { copy pad character to EAX }
  mov    ecx, ebx
  rep    stosb               { pad to left of text }
  pop    edi
  pop    ecx
  cld                        { forward string ops }
  rep    stosb               { pad to right of text }
  jmp    @@AddNull           { add null terminator }

@@SpecialCase:
  mov    ecx, ebx            { fill string with C }
  mov    eax, edx
  mov    edi, esi
  rep    stosb

@@AddNull:
  mov    byte ptr [edi], 0h  { add null at end of string }

@@ExitPoint:
  pop    esi                 { restore registers }
  pop    edi
  pop    ebx
  pop    eax
end;

function CenterChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a string of C with specified width}
begin
  StrCopy(Dest, S);
  Result := CenterChPrimZ(Dest, C, Len);
end;

function CenterPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a blank string of specified width}
begin
  Result := CenterChPrimZ(S, ' ', Len);
end;

function CenterZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a blank string of specified width}
begin
  StrCopy(Dest, S);
  Result := CenterPrimZ(Dest, Len);
end;

function ScramblePrimZ(S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption. This
    primitive version modifies the source string directly.}
var
  SPtr, KPtr, EndPtr : PAnsiChar;
begin
  Result := S;
  if Key^ = #0 then Exit;
  if S^ = #0 then Exit;
  SPtr := S;
  EndPtr := StrEnd(Key);
  Dec(EndPtr);
  KPtr := EndPtr;
  while SPtr^ <> #0 do begin
    if KPtr < Key then
      KPtr := EndPtr;
    if (SPtr^ <> KPtr^) then
      SPtr^ := Char(Byte(SPtr^) xor Byte(KPtr^));
    Inc(SPtr);
    Dec(KPtr);
  end;
end;

function ScrambleZ(Dest, S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
begin
  StrCopy(Dest, S);
  Result := ScramblePrimZ(Dest, Key);
end;

function SubstituteZ(Dest, Src, FromStr, ToStr : PAnsiChar) : PAnsiChar;
  {-Return string S after mapping characters found in FromStr to the
    corresponding ones in ToStr}
var
  I : Cardinal;
  P : Cardinal;
  L : Cardinal;
begin
  StrCopy(Dest, Src);
  if StrLen(FromStr) = StrLen(ToStr) then begin
    L := StrLen(Dest);
    if L > 0 then
      for I := 0 to L-1 do begin
        if StrChPosZ(FromStr, Dest[I], P) then
          Dest[I] := ToStr[P];
      end;
  end;
  Result := Dest;
end;

function FilterZ(Dest, Src, Filters : PAnsiChar) : PAnsiChar;
  {-Return string S after removing all characters in Filters from it}
var
  I : Cardinal;
  Len : Cardinal;
  L : Cardinal;
begin
  Result := Dest;
  StrCopy(Dest, Src);
  Len := 0;
  L := StrLen(Dest);
  if L > 0 then
    for I := 0 to L-1 do
      if not CharExistsZ(Filters, Dest[I]) then begin
        Result[Len] := Dest[I];
        inc(Len);
      end;
  Result[Len] := #0;
end;

function EntabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;  Assembler;
  {-Convert blanks in a string to tabs on spacing TabSize}
asm
  push   eax                { Save registers }
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  and    ecx, 0FFh          { zero all but low byte of ECX }
  jz     @@Done
  mov    esi, edx
  xor    ebx, ebx           { Zero EBX and EDX }
  xor    edx, edx
  inc    edx                { Set EDX to 1 }

@@Next:
  or     ebx, ebx
  je     @@NoTab            { Jump to NoTab if spacecount is zero }
  mov    eax, edx           { IPos to EAX }
  push   edx
  xor    edx, edx
  div    ecx
  cmp    edx, 1             { Is mod = 1? }
  pop    edx
  jne    @@NoTab            { If not, no tab }

  sub    edi, ebx
  mov    byte ptr [edi], 9h { Store a tab }
  inc    edi
  xor    ebx, ebx           { Reset spacecount }

@@NoTab:
  mov    al, [esi]          { Get next input character }
  inc    esi
  or     al, al             { End of string? }
  jz     @@Done             { Yes, done }
  inc    ebx                { Increment SpaceCount }
  cmp    al, 20h            { Is character a space? }
  jz     @@Store            { Yes, store it for now }
  xor    ebx, ebx           { Reset SpaceCount }
  cmp    al, 27h            { Is it a quote? }
  jz     @@Quotes           { Yep, enter quote loop }
  cmp    al, 22h            { Is it a doublequote? }
  jnz    @@Store            { Nope, store it }

@@Quotes:
  mov    ah, al             { Save quote start }

@@NextQ:
  mov    [edi], al          { Store quoted character }
  inc    edi
  mov    al, [esi]          { Get next character }
  inc    esi
  inc    edx                { Increment Ipos }
  cmp    edx, ecx           { At end of line? }
  jae    @@Store            { If so, exit quote loop }

  cmp    al, ah             { Matching end quote? }
  jnz    @@NextQ            { Nope, stay in quote loop }
  cmp    al, 27h            { Single quote? }
  jz     @@Store            { Exit quote loop }
  cmp    byte ptr [esi-2],'\'  { Previous character an escape? }
  jz     @@NextQ            { Stay in if so }

@@Store:
  mov    [edi], al          { Store last character }
  inc    edi
  inc    edx                { Increment input position }
  jmp    @@Next             { Repeat while characters left }

@@Done:
  mov    byte ptr [edi], 0h
  pop    esi
  pop    edi
  pop    ebx
  pop    eax
end;

function DetabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;  Assembler;
  { -Expand tabs in a string to blanks on spacing TabSize- }
asm
  push    eax           { Save Dest for return value }
  push    edi           { Save EDI, ESI and EBX, we'll be changing them }
  push    esi
  push    ebx

  mov     esi, edx      { ESI -> Src }
  mov     edi, eax      { EDI -> Dest }
  xor     ebx, ebx      { Get TabSize in EBX }
  add     bl, cl
  jz      @@Done        { Exit if TabSize is zero }

  xor     edx, edx      { Set output length to zero }

@@Next:
  mov     al, [esi]
  inc     esi           { Get next input character }
  or      al, al        { Is it a null? }
  jz      @@Done        { Yes-all done }
  cmp     al, 09        { Is it a tab? }
  je      @@Tab         { Yes, compute next tab stop }
  mov     [edi], al     { No, store to output }
  inc     edi
  inc     edx           { Increment output length }
  jmp     @@Next        { Next character }

@@Tab:
  push    edx           { Save output length }
  mov     eax, edx      { Get current output length in EDX:EAX }
  xor     edx, edx
  div     ebx           { Output length MOD TabSize in DX }
  mov     ecx, ebx      { Calc number of spaces to insert... }
  sub     ecx, edx      { = TabSize - Mod value }
  pop     edx
  add     edx, ecx      { Add count of spaces into current output length }

  mov     eax,$2020     { Blank in AH, Blank in AL }
  shr     ecx, 1        { Store blanks }
  rep     stosw
  adc     ecx, ecx
  rep     stosb
  jmp     @@Next        { Back for next input }

@@Done:
  mov     byte ptr [edi], 0h { Store final null terminator }

  pop     ebx           { Restore caller's EBX, ESI and EDI }
  pop     esi
  pop     edi
  pop     eax           { Return Dest }
end;

function HasExtensionZ(Name : PAnsiChar; var DotPos : Cardinal) : Boolean;
  {-Return whether and position of extension separator dot in a pathname}
var
  I, L : Integer;
  Pos : Cardinal;
  P : TSmallArray;
begin
  I := -1;
  DotPos := Cardinal(I);
  Result := False;
  L := StrLen(Name);
  if L = 0 then
    Exit;
  for I := L-1 downto 0 do
    if (Name[I] = '.') and (DotPos = Cardinal(-1)) then
      DotPos := I;
  Result := (DotPos <> Cardinal(-1)) and not
    StrChPosZ(StrStCopyZ(P, Name, Succ(DotPos), StMaxFileLen), PathDelim {'\'}, Pos);
end;

function DefaultExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a pathname with the specified extension attached}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    StrCopy(Dest, Name)
  else if StrLen(Name) = 0 then
    Dest[0] := #0
  else begin
    StrCopy(Dest, Name);
    StrCat(Dest, '.');
    StrCat(Dest, Ext);
  end;
  Result := Dest;
end;

function ForceExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a pathname with the specified extension attached}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    Dest := StrCat(StrStCopyZ(Dest, Name, 0, Succ(DotPos)), Ext)
  else if StrLen(Name) = 0 then
    Dest[0] := #0
  else begin
    Dest := StrCopy(Dest, Name);
    Dest := StrCat(Dest, '.');
    Dest := StrCat(Dest, Ext);
  end;
  Result := Dest;
end;


function JustExtensionZ(Dest : PAnsiChar; Name : PAnsiChar) : PAnsiChar;
  {-Return just the extension of a pathname}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    Dest := StrStCopyZ(Dest, Name, Succ(DotPos), StMaxFileLen)
  else
    Dest[0] := #0;
  Result := Dest;
end;

function JustFilenameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the filename of a pathname}
var
  I : Integer;
begin
  I := StrLen(PathName);
  while (I > 0) and (not (PathName[I-1] in DosDelimSet)) do
    Dec(I);
  Dest := StrStCopyZ(Dest, PathName, I, StMaxFileLen);
  Result := Dest;
end;

function JustNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the name (no extension, no path) of a pathname}
var
  DotPos : Cardinal;
  T : TSmallArray;
begin
  JustFileNameZ(T, PathName);
  if HasExtensionZ(T, DotPos) then
    Dest := StrStCopyZ(Dest, T, 0, DotPos)
  else
    StrCopy(Dest, T);
  Result := Dest;
end;

function JustPathnameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the drive:directory portion of a pathname}
var
  I : Longint;
begin
  I := StrLen(PathName);
  repeat
    Dec(I);
  until (I = -1) or (PathName[I] in DosDelimSet);

  if I = -1 then
    {Had no drive or directory name}
    Dest[0] := #0
  else if I = 0 then begin
    {Either the root directory of default drive or invalid pathname}
    Dest[0] := PathName[0];
    Dest[1] := #0;
  end
  else if (PathName[I] = PathDelim {'\'}) then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Dest := StrStCopyZ(Dest, PathName, 0, Succ(I))
    else
      {Subdirectory, remove the trailing backslash}
      Dest := StrStCopyZ(Dest, PathName, 0, I);
  end else
    {Either the default directory of a drive or invalid pathname}
    Dest:= StrStCopyZ(Dest, PathName, 0, Succ(I));
  Result := Dest;
end;

function AddSlashZ(Dest : PAnsiChar; DirName : PAnsiChar) : PAnsiChar;
  {-Add a default backslash to a directory name}
var
  L : Integer;
begin
  Result := Dest;
  StrCopy(Dest, DirName);
  L := StrLen(DirName);
  if (L > 0) then begin
    if (Dest[L-1] <> PathDelim {'\'}) then begin
      Dest[L] := PathDelim;
      Dest[L+1] := #0;
    end;
  end
  else begin
    Dest[0] := PathDelim;
    Dest[1] := #0;
  end;
end;

function CleanFileNameZ(Dest, FileName : PAnsiChar) : PAnsiChar;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos : Cardinal;
  NameLen : Integer;
  P2 : TSmallArray;
begin
  if HasExtensionZ(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := DotPos;
    if NameLen > 8 then
      NameLen := 8;
    StrStCopyZ(Dest, FileName, 0, NameLen);
    StrCat(Dest, StrStCopyZ(P2, FileName, DotPos, 4));
  end else
    {Take the first 8 chars of name}
    StrStCopyZ(Dest, FileName, 0, 8);
  Result := Dest;
end;

function ConvertToShortString(S : PAnsiChar; var SS : ShortString) : integer;

var
  LenS : integer;
begin
  {returns 0 if the string was converted successfully
           1 if the string is nil
           2 if the string length is greater than 255}
  if (S = nil) then begin
    Result := 1;
  end
  else begin
    LenS := StrLen(S);
    if (LenS > 255) then begin
      Result := 2;
    end
    else begin
      {we can't use StrPas in 32-bit since it assumes a long string
       and that would incur too much overhead, so convert to a short
       string from first principles}
      Move(S^, SS[1], LenS);
      SS[0] := char(LenS);
      Result := 0;
    end;
  end;
end;

function Str2Int16Z(S : PAnsiChar; var I : SmallInt) : Boolean;
  {-Convert a string to an integer, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValSmallint(SS, I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

function Str2WordZ(S : PAnsiChar; var I : Word) : Boolean;
  {-Convert a string to a word, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValWord(SS, I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

function Str2LongZ(S : PAnsiChar; var I : LongInt) : Boolean;
  {-Convert a string to a longint, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValLongint(SS, I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

function Str2RealZ(S : PAnsiChar; var R : Double) : Boolean;
  {-Convert a string to a real, returning true if successful}
var
  Code : Integer;
  P : TSmallArray;
begin
  if StrLen(S)+1 > SizeOf(P) then begin
    Result := False;
    R := -1;
    Exit;
  end;
  StrCopy(P, S);
  TrimTrailPrimZ(P);
  Val(ValPrepZ(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
    Result := False;
  end else
    Result := True;
end;

function Str2ExtZ(S : PAnsiChar; var R : Extended) : Boolean;
  {-Convert a string to an extended, returning true if successful}
var
  Code : Integer;
  P : TSmallArray;
begin
  if StrLen(S)+1 > SizeOf(P) then begin
    Result := False;
    R := -1;
    Exit;
  end;
  StrCopy(P, S);
  TrimTrailPrimZ(P);
  Val(ValPrepZ(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
    Result := False;
  end else
    Result := True;
end;

function Long2StrZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
  {-Convert a long/word/integer/byte/shortint to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(L, PCharArray(Dest)^);
  Result := Dest;
end;

function Real2StrZ(Dest : PAnsiChar; R : Double; Width : Byte;
                  Places : ShortInt) : PAnsiChar;
  {-Convert a real to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(R:Width:Places, PCharArray(Dest)^);
  Result := Dest;
end;

function Ext2StrZ(Dest : PAnsiChar; R : Extended; Width : Byte;
                 Places : ShortInt) : PAnsiChar;
  {-Convert an extended to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(R:Width:Places, PCharArray(Dest)^);
  Result := Dest;
end;

function ValPrepZ(S : PAnsiChar) : PAnsiChar;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesPrimZ(S);
  if StrLen(Result) <> 0 then begin
    if StrChPosZ(Result, DecimalSeparator, P) then begin
      Result[P] := '.';
      if Succ(P) = StrLen(Result) then
        Result[P] := #0;
    end;
  end else begin
    Result := '0';
  end;
end;

function CharExistsZ(S : PAnsiChar; C : AnsiChar) : Boolean;  Assembler;
  {-Determine whether the given character exists in a string. }
asm
  xor   dh, dh
  xor   ecx, ecx
@@Loop:
  cmp   dh, [eax+0]
  je    @@Done
  cmp   dl, [eax+0]
  jne   @@1
  inc   ecx
  jmp   @@Done
@@1:
  cmp   dh, [eax+1]
  je    @@Done
  cmp   dl, [eax+1]
  jne   @@2
  inc   ecx
  jmp   @@Done
@@2:
  cmp   dh, [eax+2]
  je    @@Done
  cmp   dl, [eax+2]
  jne   @@3
  inc   ecx
  jmp   @@Done
@@3:
  cmp   dh, [eax+3]
  je    @@Done
  cmp   dl, [eax+3]
  jne   @@4
  inc   ecx
  jmp   @@Done
@@4:
  add   eax, 4
  jmp   @@Loop
@@Done:
  mov   eax, ecx
end;

function CharCountZ(S : PAnsiChar; C : AnsiChar) : Cardinal; Assembler;
  {-Count the number of a given character in a string. }
asm
  xor   dh, dh
  xor   ecx, ecx
@@Loop:
  cmp   dh, [eax+0]
  je    @@Done
  cmp   dl, [eax+0]
  jne   @@1
  inc   ecx
@@1:
  cmp   dh, [eax+1]
  je    @@Done
  cmp   dl, [eax+1]
  jne   @@2
  inc   ecx
@@2:
  cmp   dh, [eax+2]
  je    @@Done
  cmp   dl, [eax+2]
  jne   @@3
  inc   ecx
@@3:
  cmp   dh, [eax+3]
  je    @@Done
  cmp   dl, [eax+3]
  jne   @@4
  inc   ecx
@@4:
  add   eax, 4
  jmp   @@Loop
@@Done:
  mov   eax, ecx
end;

function WordCountZ(S : PAnsiChar; WordDelims : PAnsiChar) : Cardinal;
  {-Given a set of word delimiters, return number of words in S}
var
  Count : Cardinal;
  I     : Cardinal;
  SLen  : Cardinal;

begin
  Count := 0;
  I := 0;
  SLen := StrLen(S);
  while I < SLen do begin
    {skip over delimiters}
    while (I < SLen) and (CharExistsZ(WordDelims, S^)) do begin
      Inc(I);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if I < SLen then
      Inc(Count);

   {find the end of the current word}
    while (I < SLen) and (not CharExistsZ(WordDelims, S^)) do begin
      Inc(I);
      Inc(S);
    end;
  end;

  Result := Count;
end;

function WordPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                      var Pos : Cardinal) : Boolean;
  {-Given a set of word delimiters, return start position of N'th word in S}
var
  Count : Cardinal;
  SLen  : Cardinal;
begin
  Count := 0;
  Pos := 0;
  Result := False;
  SLen := StrLen(S);

  while (Pos < SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (Pos < SLen) and (CharExistsZ(WordDelims, S^)) do begin
      Inc(Pos);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if Pos < SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then begin
      while (Pos < SLen) and (not CharExistsZ(WordDelims, S^)) do begin
        Inc(Pos);
        Inc(S);
      end;
    end
    else
      Result := True;
  end;
end;

function ExtractWordZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                     WordDelims : PAnsiChar) : PAnsiChar;
  {-Given a set of word delimiters, return in Dest the N'th word in Src}
var
  I : Cardinal;
  SLen : Cardinal;
begin
  Result := Dest;
  SLen := StrLen(Src);
  if WordPositionZ(N, Src, WordDelims, I) then begin
    Inc(Src, I);
    {find the end of the current word}
    while (I <= SLen) and (not CharExistsZ(WordDelims, Src^)) do begin
      {add the I'th character to result}
      Dest^ := Src^;
      Inc(Dest);
      Inc(Src);
      Inc(I);
    end;
  end;
  Dest^ := #0;
end;

function AsciiCountZ(S : PAnsiChar; WordDelims : PAnsiChar; Quote : AnsiChar) : Cardinal;
  {-Given a set of word delimiters, return number of words in S}
var
  Count : Cardinal;
  I     : Cardinal;
  SLen  : Cardinal;
  InQuote : Boolean;
begin
  Count := 0;
  I := 1;
  InQuote := False;
  SLen := StrLen(S);
  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and (S^ <> Quote) and CharExistsZ(WordDelims, S^) do begin
      Inc(I);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);
    {find the end of the current word}
    while (I <= SLen) and ((InQuote) or (not CharExistsZ(WordDelims, S^))) do begin
      if S^ = Quote then
        InQuote := not(InQuote);
      Inc(I);
      Inc(S);
    end;
  end;

  Result := Count;
end;

function AsciiPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Given a set of word delimiters, return start position of N'th word in S}
var
  Count : Cardinal;
  SLen  : Cardinal;
  InQuote : Boolean;
begin
  Count := 0;
  Pos := 0;
  InQuote := False;
  Result := False;
  SLen := StrLen(S);
  while (Pos < SLen) and (Count <= N) do begin
   {skip over delimiters}
    while (Pos < SLen) and (S^ <> Quote) and CharExistsZ(WordDelims, S^) do begin
      Inc(Pos);
      Inc(S);
    end;

    {if we're not beyond end of S, we're at the start of a word}
    if Pos < SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (Pos < SLen) and ((InQuote) or (not CharExistsZ(WordDelims, S^))) do begin
        if S^ = Quote then
          InQuote := not(InQuote);
        Inc(Pos);
        Inc(S);
      end
    else begin
      Result := True;
      Exit;
    end;
  end;
end;

function ExtractAsciiZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                      WordDelims : PAnsiChar; Quote : AnsiChar) : PAnsiChar;
  {-Given a set of word delimiters, return in Dest the N'th word in Src}
var
  I : Cardinal;
  Len : Cardinal;
  SLen : Cardinal;
  InQuote : Boolean;
begin
  Len := 0;
  InQuote := False;
  Dest[0] := #0;
  Result := Dest;
  SLen := StrLen(Src);
  if AsciiPositionZ(N, Src, WordDelims, Quote, I) then
    {find the end of the current word}
    while (I < SLen) and ((InQuote) or (not CharExistsZ(WordDelims, Src[I]))) do begin
      {add the I'th character to result}
      if Src[I] = Quote then
        InQuote := Not(InQuote);
      Dest[Len] := Src[I];
      Inc(Len);
      Inc(I);
    end;
  Dest[Len] := #0;
end;

procedure WordWrapZ(Dest : PAnsiChar; InSt, Overlap : PAnsiChar;
                   Margin : Cardinal;
                   PadToMargin : Boolean);
  {-Wrap InSt at Margin, storing the result in Dest and the remainder
    in Overlap}
var
  InStLen : Cardinal;
  OutStLen : Cardinal;
  OvrLen : Cardinal;
  EOS, BOS : Cardinal;
begin
  OutStLen := 0;
  InStLen := StrLen(InSt);
  {find the end of the new output string}
  if InStLen > Margin then begin
    {assume this is a good break point}
    EOS := Margin-1;

    {is this the position of the last character of a word}
    if InSt[EOS+1] <> ' ' then begin  {check next char}
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);
      {when done, EOS points to a space char or is zero}

      {if EOS = 0 then - can't wrap it properly}
      if EOS = 0 then
        EOS := Margin-1  {set up to break line at margin}
      else
        while (InSt[EOS] = ' ') and (EOS > 0) do  {trim trailing blanks}
          Dec(EOS);
    end else
      while (EOS > 0) and (InSt[EOS] = ' ') do  {trim trailing blanks}
        Dec(EOS);
  end
  else
    EOS := InStLen-1;

  {at this point EOS points to the break point, the end of the line,
   or is zero}

  {copy the unwrapped portion of the line}
  if (EOS = 0) and (InSt[EOS] = ' ') then
    Dest[0] := #0
  else begin
    OutStLen := EOS+1;
    Move(InSt^, Dest^, OutStLen);
    Dest[OutStLen] := #0;
  end;

  {find the start of the next word in the line}
  BOS := EOS+1;
  while (BOS < InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS >= InStLen then begin
    OverLap[0] := #0;
  end else begin
    {copy from the start of the next word to the end of the line}
    OvrLen := InStLen-BOS;
    Move(InSt[BOS], Overlap^, OvrLen);
    Overlap[OvrLen] := #0;
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (OutStLen < Margin) then begin
    FillChar(Dest[OutStLen], Margin-OutStLen, ' ');
    Dest[Margin] := #0;
  end;
end;

function CompStringZ(S1, S2 : PAnsiChar) : Integer;  Assembler;
  {-Return -1, 0, 1 if S1<S2, S1=S2, or S1>S2}
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx

  mov    edi, edx
  mov    ebx, edx
  mov    edx, ecx
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, ebx
  or     ebx, -1
  cmp    edx, ecx
  je     @@EqLen
  jb     @@Comp

  inc    ebx
  mov    ecx, edx

@@EqLen:
  inc    ebx

@@Comp:
  or     ecx, ecx
  jz     @@Done

  repe   cmpsb
  je     @@Done

  mov    ebx, 1
  ja     @@Done
  or     ebx, -1

@@Done:
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;

function CompUCStringZ(S1, S2 : PAnsiChar) : Integer;  Assembler;
  {-Return -1, 0, 1 if s1<s2, s1=s2, or s1>s2. Comparison is done in
    uppercase}
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx

  mov    edi, edx
  mov    ebx, edx
  mov    edx, ecx
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, ebx
  or     ebx, -1
  cmp    edx, ecx
  je     @@EqLen
  jb     @@Comp

  inc    ebx
  mov    ecx, edx

@@EqLen:
  inc    ebx

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

@@Start:
  mov    al, [esi]
  inc    esi
  push   ebx                { Save registers }
  push   ecx
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx
  pop    ebx

  mov    edx, eax
  mov    al, [edi]
  inc    edi

  push   ebx                { Save registers }
  push   ecx
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx
  pop    ebx

  cmp    edx, eax
  jne    @@Output
  dec    ecx
  jnz    @@Start
  je     @@Done

@@Output:
  mov    ebx, 1
  ja     @@Done
  or     ebx, -1

@@Done:
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;

function SoundexZ(Dest : PAnsiChar; S : PAnsiChar) : PAnsiChar; assembler;
  {-Return 4 character soundex of input string}
const
  SoundexTable : array[0..255] of AnsiChar =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
    { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
    { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
asm
  push   eax                      { Save registers }
  push   ebx
  push   edi
  push   esi
  mov    edi, edx
  mov    ebx, eax
  mov    esi, edx

  mov    dword ptr [ebx], '0000'  { Initialize output string to '0000'. }
  xor    eax, eax
  mov    [ebx+4], al              { Set null at end of string. }

  or     ecx, -1                  { Set ECX to $FFFFFFFF }
  repne  scasb
  not    ecx
  dec    ecx                      { ECX has length of S }
  jz     @@Done                   { Exit if null string. }

  mov    edi, ebx
  mov    al, [esi]                { Get first character of input string. }
  inc    esi

  push   ecx                      { Save ECX across call to CharUpper. }
  push   eax                      { Push Char onto stack for CharUpper. }
  call   CharUpper                { Uppercase AL. }
  pop    ecx                      { Restore saved register. }

  mov    [edi], al                { Store first output character. }
  inc    edi

  dec    ecx                      { One input character used. }
  jz     @@Done                   { Was input string one char long?. }

  mov    bh, 03h                  { Output max 3 chars beyond first. }
  mov    edx, offset SoundexTable { EDX => Soundex table. }
  xor    eax, eax                 { Prepare for address calc. }
  xor    bl, bl                   { BL will be used to store 'previous char'. }

@@Next:
  mov    al, [esi]                { Get next char in AL. }
  inc    esi
  mov    al, [edx+eax]            { Get soundex code into AL. }
  or     al, al                   { Is AL zero? }
  jz     @@NoStore                { If yes, skip this char. }
  cmp    bl, al                   { Is it the same as the previous stored char? }
  je     @@NoStore                { If yes, skip this char. }
  mov    [edi], al                { Store char to Dest. }
  inc    edi
  dec    bh                       { Decrement output counter. }
  jz     @@Done                   { If zero, we're done. }
  mov    bl, al                   { New previous character. }

@@NoStore:
  dec    ecx                      { Decrement input counter. }
  jnz    @@Next

@@Done:                           { Restore registers }
  pop    esi
  pop    edi
  pop    ebx
  pop    eax
end;

function MakeLetterSetZ(S : PAnsiChar) : Longint;  Assembler;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push   ebx                    { Save registers }
  push   edi
  push   esi
  mov    esi, eax
  mov    edi, eax
  xor    edx, edx
  xor    eax, eax               { Measure S }
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx                    { ECX has length of S }
  jz     @@Exit

@@Next:
  mov    al, [esi]              { EAX has next char in S }
  inc    esi

  push   ecx                    { Save registers }
  push   edx
  push   eax                    { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                    { Restore registers }
  pop    ecx

  sub    eax, 'A'               { Convert to bit number }
  cmp    eax, 'Z'-'A'           { Was char in range 'A'..'Z'? }
  ja     @@Skip                 { Skip it if not }

  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx
  ror    edx, cl
  or     edx, 01h               { Set appropriate bit }
  rol    edx, cl
  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx

@@Skip:
  dec    ecx
  jnz    @@Next                 { Get next character }

@@Exit:
  mov    eax, edx               { Move EDX to result }
  pop    esi
  pop    edi                    { Restore registers }
  pop    ebx
end;

procedure BMMakeTableZ(MatchString : PAnsiChar; var BT : BTable); Assembler;
asm
  push  esi             { Save registers because they will be changed }
  push  edi
  push  ebx

  mov   edi, eax        { Move EAX to ESI & EDI }
  mov   esi, eax
  xor   eax, eax        { Zero EAX }
  or    ecx, -1
  repne scasb           { Search for null terminator }
  not   ecx
  dec   ecx             { ECX is length of search string }
  cmp   ecx, 0FFh       { If ECX > 255, force to 255 }
  jbe   @@1
  mov   ecx, 0FFh

@@1:
  mov   ch, cl          { Duplicate CL in CH }
  mov   eax, ecx        { Fill each byte in EAX with length }
  shl   eax, 16
  mov   ax, cx
  mov   edi, edx        { Point to the table }
  mov   ecx, 64         { Fill table bytes with length }
  rep   stosd
  cmp   al, 1           { If length <= 1, we're done }
  jbe   @@MTDone
  mov   edi, edx        { Reset EDI to beginning of table }
  xor   ebx, ebx        { Zero EBX }
  mov   cl, al          { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   bl, [esi]       { Load table with positions of letters }
  inc   esi             { That exist in the search string }
  mov   [edi+ebx], cl
  dec   ecx
  jnz   @@MTNext

@@MTDone:
  pop   ebx             { Restore registers }
  pop   edi
  pop   esi
end;

function BMSearchZ(var Buffer; BufLength : Cardinal; var BT : BTable;
  MatchString : PAnsiChar; var Pos : Cardinal) : Boolean; assembler;
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx
  push  edx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }

  xor   eax, eax            { Zero out EAX so we can search for null }
  mov   edi, MatchString    { Set EDI to beginning of MatchString }
  or    ecx, -1             { We will be counting down }
  repne scasb               { Find null }
  not   ecx                 { ECX = length of MatchString + null }
  dec   ecx                 { ECX = length of MatchString }
  mov   edx, ecx            { Copy length of MatchString to EDX }

  pop   ecx                 { Pop length of buffer into ECX }
  mov   edi, esi            { Set EDI to beginning of search buffer }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }

  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  dec   edi                 { Found, calculate position }
  sub   edi, ebx
  mov   esi, Pos            { Set position in Pos }
  mov   [esi], edi
  mov   eax, 1              { Set result to True }
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  std                       { Backward string ops }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  mov   esi, Pos
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

function BMSearchUCZ(var Buffer; BufLength : Cardinal; var BT : BTable;
  MatchString : PAnsiChar; var Pos : Cardinal) : Boolean; assembler;
  {- Case-insensitive search of Buffer for MatchString. Return indicates
     success or failure.  Assumes MatchString is already raised to
     uppercase (PRIOR to creating the table) -}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx
  push  edx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, eax
  mov   ebx, ecx            { Copy BufLength to EBX }

  xor   eax, eax            { Zero out EAX so we can search for null }
  mov   edi, MatchString    { Set EDI to beginning of MatchString }
  or    ecx, -1             { We will be counting down }
  repne scasb               { Find null }
  not   ecx                 { ECX = length of MatchString + null }
  dec   ecx                 { ECX = length of MatchString }
  mov   edx, ecx            { Copy length of MatchString to EDX }

  pop   ecx                 { Pop length of buffer into ECX }
  mov   edi, esi            { Set EDI to beginning of search buffer }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }

  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  std                       { Backward string ops }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  lodsb                     { Get MatchString char }
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  mov   esi, Pos
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;

{------------------ Formatting routines --------------------}

function CommaizeChZ(Dest : PAnsiChar; L : Longint; Ch : AnsiChar) : PAnsiChar;
var
  NumCommas, Len, I : Cardinal;
begin
  Result := Dest;
  Long2StrZ(Dest, L);
  Len := StrLen(Dest);
  NumCommas := (Len - 1) div 3;
  for I := 1 to NumCommas do
    StrChInsertPrimZ(Dest, Ch, Len - (I * 3));
end;

function CommaizeZ(Dest : PAnsiChar; L : LongInt) : PAnsiChar;
begin
  Result := CommaizeChZ(Dest, L, ',');
end;

function FormPrimZ(Dest, Mask : PAnsiChar; R : TstFloat; LtCurr,
                   RtCurr : PAnsiChar; Sep, DecPt : AnsiChar;
                   AssumeDP : Boolean) : PAnsiChar;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars : array[0..8] of AnsiChar = '#@*$-+,.';
  PlusArray : array[Boolean] of AnsiChar = ('+', '-');
  MinusArray : array[Boolean] of AnsiChar = (' ', '-');
  FillArray : array[Blank..Zero] of AnsiChar = (' ', '*', '0');
var
  Temp : PAnsiChar;
  S : array[0..20] of AnsiChar; {temporary string}
  Filler : integer;{char for unused digit slots: ' ', '*', '0'}
  WontFit,                      {true if number won't fit in the mask}
  AddMinus,                     {true if minus sign needs to be added}
  Dollar,                       {true if floating dollar sign is desired}
  Negative : Boolean;           {true if B is negative}
  StartF,                       {starting point of the numeric field}
  EndF : Cardinal;              {end of numeric field}
  RtChars,                      {# of chars to add to right}
  LtChars,                      {# of chars to add to left}
  DotPos,                       {position of '.' in Mask}
  Digits,                       {total # of digits}
  Places,                       {# of digits after the '.'}
  Blanks,                       {# of blanks returned by Str}
  FirstDigit,                   {pos. of first digit returned by Str}
  Extras,                       {# of extra digits needed for special cases}
  I : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Dest;
  StrCopy(Result, Mask);
  if (not AssumeDP) and (not CharExistsZ(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result^ <> #0) then
    StrCat(Result, '.');

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Result^ = #0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 0;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  Temp := StrEnd(Result);
  Dec(Temp);
  while Temp >= Result do begin
    if Temp^ = 'C' then begin
      Inc(RtChars);
      StrChDeletePrimZ(Result, Temp - Result);
    end else if Temp^ = 'c' then begin
      Inc(LtChars);
      StrChDeletePrimZ(Result, Temp - Result);
    end;
    Dec(Temp);
  end;

  {find the starting point for the field}
  Temp := Result;
  while (Temp^ <> #0) and not CharExistsZ(FormChars, Temp^) do begin
    Inc(StartF);
    Inc(Temp);
  end;
  if Succ(StartF) > StrLen(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  while (Temp^ <> #0) do begin
    case Temp^ of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := EndF;
    else
      goto EndFound;
    end;
    Inc(Temp);
    Inc(EndF);
  end;

EndFound:
  {correct the off-by-one nature of the loop}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Real2StrZ(S, R, Digits, Places);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    Temp := StrEnd(S);
    CharStrZ(Temp, '0', Places-MaxPlaces);
    while (StrLen(S) > Digits) and (S[0] = ' ') do
      StrChDeletePrimZ(S, 0);
  end;

  {count number of initial blanks}
  Blanks := 0;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (StrLen(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillChar(S[0], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Pred(Blanks)] := LtCurr[0];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Pred(Blanks)] := '-';
  end;

  {put in the digits / signs}
  Temp := StrEnd(S);
  Dec(Temp);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if Temp >= S then begin
          Result[I] := Temp^;
          Dec(Temp);
          if (Temp^ = '.') and (Temp >= S) then
            Dec(Temp);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (Temp < (S + FirstDigit)) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (Temp < (S + FirstDigit)) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    Result[Pred(StrLen(Result))] := #0;
  if RtChars > 0 then begin
    StrLCopy(S, RtCurr, RtChars);
    LeftPadPrimZ(S, RtChars);
    StrCat(Result, S);
  end;
  if LtChars > 0 then begin
    StrLCopy(S, LtCurr, LtChars);
    PadPrimZ(S, LtChars);
    StrStInsertPrimZ(Result, S, 0);
  end;
end;

function FloatFormZ(Dest, Mask : PAnsiChar; R : TstFloat; LtCurr,
                    RtCurr : PAnsiChar; Sep, DecPt : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimZ(Dest, Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormZ(Dest, Mask : PAnsiChar; L : Longint; LtCurr,
                      RtCurr : PAnsiChar; Sep : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimZ(Dest, Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosZ(P : PAnsiChar; C : AnsiChar; var Pos : Cardinal): Boolean;
  {-Sets Pos to position of character C within string P returns True if found}
var
  Temp : PChar;
begin
  Result := False;
  Temp := StrScan(P, C);
  if Temp <> nil then begin
    Pos := Temp - P;
    Result := True;
  end;
end;

function StrStPosZ(P, S : PAnsiChar; var Pos : Cardinal) : boolean;
  {-Sets Pos to position of string S within string P returns True if found}
var
  Temp : PChar;
begin
  Result := False;
  Temp := StrPos(P, S);
  if Temp <> nil then begin
    Pos := Temp - P;
    Result := True;
  end;
end;

function StrChInsertPrimZ(Dest : PAnsiChar; C : AnsiChar;
                          Pos : Cardinal) : PAnsiChar;  Assembler;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi

  mov    esi, eax        {copy Dest to ESI and EDI}
  mov    edi, eax
  mov    ah, dl
  mov    edx, ecx        {move POS to edx}

  xor    al, al          {zero}
  or     ecx, -1         {set ECX to $FFFFFFFF}
  repne  scasb           {find null terminator}

  not    ecx             {calc length (including null)}
  std                    {backwards string ops}
  add    esi, ecx
  dec    esi             {point to end of source string}
  sub    ecx, edx        {calculate number to do}
  jae    @@1             {set ECX to 1 if Pos greater than strlen + 1}
  mov    ecx, 1

@@1:
  rep    movsb           {adjust tail of string}
  mov    [edi], ah       {insert the new character}

@@ExitPoint:
  cld                    {be a good neighbor}

  pop    esi             {restore registers}
  pop    edi
  pop    eax
end;

function StrStInsertPrimZ(Dest : PAnsiChar; S : PAnsiChar;
                         Pos : Cardinal) : PAnsiChar; Assembler;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move POS to ebx}
  mov    esi, eax        {copy Dest to ESI, S to EDI}
  mov    edi, edx

  xor    al, al          {zero}
  or     ecx, -1         {set ECX to $FFFFFFFF}
  repne  scasb           {find null terminator}
  not    ecx             {calc length of source string (including null)}
  dec    ecx             {length without null}
  jz     @@ExitPoint     {if source length = 0, exit}
  push   ecx             {save length for later}

  mov    edi, esi        {reset EDI to Dest}
  or     ecx, -1
  repne  scasb           {find null}
  not    ecx             {length of dest string (including null)}

  cmp    ebx, ecx
  jb     @@1
  mov    ebx, ecx
  dec    ebx

@@1:
  std                    {backwards string ops}
  pop    eax             {restore length of S from stack}
  add    edi, eax        {set EDI S beyond end of Dest}
  dec    edi             {back up one for null}

  add    esi, ecx        {set ESI to end of Dest}
  dec    esi             {back up one for null}
  sub    ecx, ebx        {# of chars in Dest that are past Pos}
  rep    movsb           {adjust tail of string}

  mov    esi, edx        {set ESI to S}
  add    esi, eax        {set ESI to end of S}
  dec    esi             {back up one for null}
  mov    ecx, eax        {# of chars in S}
  rep    movsb           {copy S into Dest}

  cld                    {be a good neighbor}

@@ExitPoint:

  pop    ebx             {restore registers}
  pop    esi
  pop    edi
  pop    eax
end;

function StrStCopyZ(Dest : PAnsiChar; S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
var
  Len : Cardinal;
begin
  Len := StrLen(S);
  if Pos < Len then begin
    if (Len-Pos) < Count then
      Count := Len-Pos;
    Move(S[Pos], Dest^, Count);
    Dest[Count] := #0;
  end else
    Dest[0] := #0;
  Result := Dest;
end;

function StrChDeletePrimZ(P : PAnsiChar; Pos : Cardinal) : PAnsiChar; Assembler;
asm
  push   edi             { Save because we will be changing them }
  push   esi
  push   ebx

  mov    ebx, eax        { Save P to EDI & EBX }
  mov    edi, eax

  xor    al, al          { Zero }
  or     ecx, -1         { Set ECX to $FFFFFFFF }
  repne  scasb           { Find null terminator }
  not    ecx
  dec    ecx
  or     ecx, ecx
  jz     @@ExitPoint
  sub    ecx, edx        { Calc number to move }
  jb     @@ExitPoint     { Exit if Pos > StrLen }

  mov    edi, ebx
  add    edi, edx        { Point to position to adjust }
  mov    esi, edi
  inc    esi             { Offset for source string }
  inc    ecx             { One more to include null terminator }
  rep    movsb           { Adjust the string }

@@ExitPoint:
  mov    eax, ebx
  pop    ebx             { restore registers }
  pop    esi
  pop    edi
end;

function StrStDeletePrimZ(P : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar; Assembler;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move Count to BX}
  mov    esi, eax        {move P to ESI and EDI}
  mov    edi, eax

  xor    eax, eax        {null}
  or     ecx, -1
  repne  scasb           {find null terminator}
  not    ecx             {calc length}
  or     ecx, ecx
  jz     @@ExitPoint

  sub    ecx, ebx        {subtract Count}
  sub    ecx, edx        {subtract Pos}
  jns    @@L1

  mov    edi,esi         {delete everything after Pos}
  add    edi,edx
  mov    [edi], al
  jmp    @@ExitPoint

@@L1:
  mov    edi,esi
  add    edi,edx         {point to position to adjust}
  mov    esi,edi
  add    esi,ebx         {point past string to delete in src}
  inc    ecx             {one more to include null terminator}
  rep    movsb           {adjust the string}

@@ExitPoint:

  pop    ebx            {restore registers}
  pop    esi
  pop    edi
  pop    eax
end;

function StrChDeleteZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  StrCopy(Dest, S);
  Result := StrChDeletePrimZ(Dest, Pos);
end;

function StrStDeleteZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
begin
  StrCopy(Dest, S);
  Result := StrStDeletePrimZ(Dest, Pos, Count);
end;

function StrChInsertZ(Dest, S : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  StrCopy(Dest, S);
  Result := StrChInsertPrimZ(Dest, C, Pos);
end;

function StrStInsertZ(Dest : PAnsiChar; S1, S2 : PAnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  StrCopy(Dest, S1);
  Result := StrStInsertPrimZ(Dest, S2, Pos);
end;


{----------------------------------------------------------------------------}

function CopyLeftZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, 0, Len);
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function CopyMidZ(Dest, S : PAnsiChar; First, Len : Cardinal) : PAnsiChar;
  {-Return the mid part of a string}
begin
  if (First >= StrLen(S)) or (LongInt(Len) < 1) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, First, Len);
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function CopyRightZ(Dest, S : PAnsiChar; First : Cardinal) : PAnsiChar;
  {-Return the right characters of a string}
begin
  if (First >= StrLen(Dest)) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, First, StrLen(S)-First+1);
  Result := Dest;
end;

{----------------------------------------------------------------------------}
function CopyRightAbsZ(Dest, S : PAnsiChar; NumChars : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}
var
  I : Cardinal;
begin
  if (StrLen(S) > NumChars) then begin
    I := StrLen(S) - NumChars;
    Dest := StrStCopyZ(Dest, S, I, NumChars)
  end else
    Dest := S;
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function WordPosZ(S, WordDelims, AWord : PAnsiChar;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a given word within a string}
var
  P,
  TmpStr  : PAnsiChar;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S[0] = #0) or (AWord[0] = #0) or
     (StrPos(S, AWord) = nil) or (N < 1) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  GetMem(TmpStr, StrLen(S)+1);
  try
    StrCopy(TmpStr, S);
    I      := 0;
    Len    := StrLen(AWord);
    P  := StrPos(TmpStr, AWord);
    P1 := P - TmpStr;

    while (StrLen(TmpStr) > 0) do begin
      P2 := P1 + pred(Len);
      if (P1 = 0) then begin
        if (CharExistsZ(WordDelims, TmpStr[P2+1])) then begin
          Inc(I);
        end else
          StrStDeleteZ(TmpStr, TmpStr, 0, P2);
      end else if (CharExistsZ(WordDelims, TmpStr[P1-1])) and
                  ((CharExistsZ(WordDelims, TmpStr[P2+1])) or (P2+1 = StrLen(TmpStr))) then begin
        Inc(I);
      end else if ((P1 + pred(Len)) = StrLen(TmpStr)) then begin
        if (CharExistsZ(WordDelims, TmpStr[P1-1])) then
          Inc(I);
      end;

      if (I = N) then begin
        Result := True;
        Position := Position + P1;
        Exit;
      end;
      StrStDeletePrimZ(TmpStr, 0, P2+1);
      Position := Position + P2+1;
      P := StrPos(TmpStr, AWord);
      if (P <> nil) then
        P1 := P - TmpStr
      else
        break;
    end;
  finally
    FreeMem(TmpStr, StrLen(S)+1);
  end;
end;


{----------------------------------------------------------------------------}

function CopyFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                          N : Cardinal) : Boolean;
var
  P      : Cardinal;
begin
  if (WordPosZ(S, WordDelims, AWord, N, P)) then begin
    StrStCopyZ(Dest, S, P, StrLen(S)-P+1);
    Result := True;
  end else begin
    Dest[0] := #0;
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}


function DeleteFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                            N : Cardinal) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosZ(S, WordDelims, AWord, N, P)) then begin
    StrStDeleteZ(Dest, S, P, StrLen(S)-P+1);
    Result := False;
  end else begin
    Dest[0] := #0;
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}

function CopyFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                         N1, N2 : Cardinal) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosZ(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosZ(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        StrStCopyZ(Dest, S, P1, P2-P1);
      end else begin
        Result := False;
        Dest[0] := #0;
      end;
    end else begin
      Result := False;
      Dest[0] := #0;
    end;
  end else begin
    Result := False;
    Dest[0] := #0;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                           N1, N2 : Cardinal) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosZ(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosZ(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        StrStDeleteZ(Dest, S, P1, P2-P1+1);
      end else begin
        Result := False;
        Dest[0] := #0;
      end;
    end else begin
      Result := False;
      Dest[0] := #0;
    end;
  end else begin
    Result := False;
    Dest[0] := #0;
  end;
end;

{----------------------------------------------------------------------------}

function CopyWithinZ(Dest, S, Delimiter : PAnsiChar; Strip : Boolean) : PAnsiChar;
var
  P1,
  P2     : Cardinal;
  L      : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (Delimiter[0] = #0) or
     (StrPos(S, Delimiter) = nil) then begin
    Dest[0] := #0;
    Result := Dest;
  end else begin
    if (StrStPosZ(S, Delimiter, P1)) then begin
      L := StrLen(S) - (P1 + StrLen(Delimiter)) + 1;
      GetMem(TmpStr, L);
      try
        StrStCopyZ(TmpStr, S, P1 + StrLen(Delimiter), StrLen(S));
        if (StrStPosZ(TmpStr, Delimiter, P2)) then begin
          StrStCopyZ(Dest, TmpStr, 0, P2);
          if (not Strip) then
            StrCat(StrStInsertZ(Dest, Dest, Delimiter, 0), Delimiter)
        end else begin
          StrCopy(Dest, TmpStr);
          if (not Strip) then
            StrStInsertZ(Dest, Dest, Delimiter, 0);
        end;
      finally
        FreeMem(TmpStr, L);
      end;
    end;
    Result := Dest;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteWithinZ(Dest, S, Delimiter : PAnsiChar) : PAnsiChar;
var
  P1,
  P2     : Cardinal;
  L      : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (Delimiter[0] = #0) or
     (StrPos(S, Delimiter) = nil) then begin
    Dest[0] := #0;
    Result := Dest;
  end else begin
    if (StrStPosZ(S, Delimiter, P1)) then begin
      L := StrLen(S) - (P1 + StrLen(Delimiter)) + 1;
      GetMem(TmpStr, L);
      try
        StrStCopyZ(TmpStr, S, P1 + StrLen(Delimiter), StrLen(S));
        if not (StrStPosZ(TmpStr, Delimiter, P2)) then
          StrStCopyZ(Dest, S, 0, P1)
        else begin
          P2 := P2 + (2*StrLen(Delimiter));
          StrStDeleteZ(Dest, S, P1, P2);
        end;
      finally
        FreeMem(TmpStr, L);
      end;
    end;
    Result := Dest;
  end;
end;

{----------------------------------------------------------------------------}

function ReplaceWordZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                      N : Cardinal;
                      var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1  : Cardinal;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or (OldWord[0] = #0) or
     (StrPos(S, OldWord) = nil) then begin
    StrCopy(Dest, S);
    Replacements := 0;
    Result := Dest;
  end else begin
    if (WordPosZ(S, WordDelims, OldWord, N, P1)) then begin
      StrCopy(Dest, S);
      StrStDeleteZ(Dest, Dest, P1, StrLen(OldWord));

      C := 0;
      for I := 1 to Replacements do begin
        if (StrLen(NewWord) + 1 + StrLen(Dest)) < High(Cardinal) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewWord, P1);
          Inc(P1, StrLen(NewWord) + 1);
        end else begin
          Replacements := C;
          Result := Dest;
          Exit;
        end;
      end;
      Result := Dest;
    end else begin
      Replacements := 0;
      Result := Dest;
    end;
  end;
end;


function ReplaceWordAllZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                         var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1  : Cardinal;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or (OldWord[0] = #0) or
     (StrPos(S, OldWord) = nil) then begin
    Replacements := 0;
    StrCopy(Dest, S);
    Result := Dest;
  end else begin
    StrCopy(Dest, S);
    C := 0;
    while (WordPosZ(Dest, WordDelims, OldWord, 1, P1)) do begin
      StrStDeleteZ(Dest, Dest, P1, StrLen(OldWord));
      for I := 1 to Replacements do begin
        if ((StrLen(NewWord) + 1 + StrLen(Dest)) < High(Cardinal)) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewWord, P1);
        end else begin
          Result := Dest;
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
    Result := Dest;
  end;
end;


function ReplaceStringZ(Dest, S, OldString, NewString : PAnsiChar;
                        N : Cardinal;
                        var Replacements : Cardinal) : PAnsiChar;
var
  I,
  L,
  C,
  P1 : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (OldString[0] = #0) or
     (StrPos(S, OldString) = nil) then begin
    StrCopy(Dest, S);
    Replacements := 0;
    Result := Dest;
    Exit;
  end;

  L := StrLen(S) + 1;
  GetMem(TmpStr, L);
  try
    StrCopy(TmpStr, S);

    I  := 1;
    StrStPosZ(TmpStr, OldString, P1);
    C  := P1;
    while (I < N) and (C < StrLen(TmpStr)) do begin
      Inc(I);
      StrStDeleteZ(TmpStr, TmpStr, P1, P1 + StrLen(OldString));
      Inc(C, P1 + StrLen(OldString));
    end;
  finally
    FreeMem(TmpStr, L);
  end;
  StrCopy(Dest, S);
  StrStDeleteZ(Dest, Dest, C, StrLen(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if ((StrLen(NewString) + 1 + StrLen(Dest)) < High(Cardinal)) then begin
      Inc(C);
      StrStInsertZ(Dest, Dest, NewString, P1);
      Inc(P1, StrLen(NewString) + 1);
    end else begin
      Replacements := C;
      Result := Dest;
      Exit;
    end;
  end;
  Result := Dest;
end;


function ReplaceStringAllZ(Dest, S, OldString, NewString : PAnsiChar;
                           var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S[0] = #0) or (OldString[0] = #0) or (StrPos(S, OldString) = nil) then begin
    StrCopy(Dest, S);
    Result := Dest;
    Replacements := 0;
  end else begin
    StrCopy(Dest, S);
    C  := 0;
    while StrStPosZ(Dest, OldString, P1) do begin
      StrStDeleteZ(Dest, Dest, P1, StrLen(OldString));
      for I := 1 to Replacements do begin
        if (LongInt((StrLen(NewString) + 1 + StrLen(Dest))) < MaxLongInt) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewString, P1);
        end else begin
          Replacements := C;
          Result := Dest;
          Exit;
        end;
      end;
    end;
    Replacements := C;
    Result := Dest;
  end;
end;



function LastWordZ(S, WordDelims, AWord : PAnsiChar;
                   var Position : Cardinal) : Boolean;
var
  TmpStr1,
  TmpStr2 : PAnsiChar;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or
     (AWord[0] = #0) or (StrPos(S, AWord) = nil) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  GetMem(TmpStr1, StrLen(S) + 1);
  GetMem(TmpStr2, StrLen(AWord) + 1);
  try
    StrCopy(TmpStr1, S);
    Position := StrLen(TmpStr1)-1;
    while (CharExistsZ(WordDelims, TmpStr1[Position])) do
      Dec(Position);

    Position := StrLen(TmpStr1)-1;
    repeat
      while (not CharExistsZ(WordDelims, TmpStr1[Position])) and (Position > 0) do
        Dec(Position);
      if (CompStringZ(
            StrStCopyZ(TmpStr2, TmpStr1, Position+1, StrLen(AWord)),
            AWord) = 0) then begin
        Inc(Position);
        Result := True;
        Exit;
      end;
      StrStDeleteZ(TmpStr1, TmpStr1, Position, StrLen(TmpStr1));
      Position := StrLen(TmpStr1)-1;
    until (Position = 0);
    Result := False;
    Position := 0;
  finally
    FreeMem(TmpStr1, StrLen(S)+1);
    FreeMem(TmpStr2, StrLen(AWord)+1);
  end;
end;

{----------------------------------------------------------------------------}

function LastWordAbsZ(S, WordDelims : PAnsiChar;
                      var Position : Cardinal) : Boolean;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not, it's a "one word wonder"}
  Position := StrLen(S)-1;
  while (Position > 0) and (CharExistsZ(WordDelims, S[Position])) do
    Dec(Position);

  if (Position = 0) then begin
    if (CharExistsZ(WordDelims, S[Position])) then begin
      Result := True;
      Position := 1;
      Exit;
    end else begin
      Result := True;
      Exit;
    end;
  end;

{find next non-delimiter character}
  Result := True;
  while (Position > 0) and (not CharExistsZ(WordDelims, S[Position])) do
    Dec(Position);
  if (Position = 0) then begin
    if (CharExistsZ(WordDelims, S[Position])) then begin
      Position := 1;
    end else begin
      Position := 0;
    end;
  end else begin
    Inc(Position);
  end;
end;

{----------------------------------------------------------------------------}

function LastStringZ(S, AString : PAnsiChar;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : PAnsiChar;
  I, C   : Cardinal;
begin
  if (S[0] = #0) or (StrPos(S, AString) = nil) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  GetMem(TmpStr, StrLen(S)+1);
  try
    StrCopy(TmpStr, S);
    C := 0;
    while (StrStPosZ(TmpStr, AString, I)) do begin
      Inc(C, I + StrLen(AString));
      StrStDeleteZ(TmpStr, TmpStr, 0, I + StrLen(AString));
    end;

  {Go back the length of AString since the while loop deletes the last instance}
    Dec(C, StrLen(AString));
    Position := C;
    Result := True;
  finally
    FreeMem(TmpStr, StrLen(S)+1);
  end;
end;


{----------------------------------------------------------------------------}

function KeepCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
  while (S^ <> #0) do begin
    if CharExistsZ(Chars, S^) then begin
      Dest^ := S^;
      inc(Dest);
    end;
    inc(S);
  end;
  Dest^ := #0;
end;

{----------------------------------------------------------------------------}

function RepeatStringZ(Dest, RepeatString : PAnsiChar;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : PAnsiChar;
var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := Dest;
  Result^ := #0;
  Len := StrLen(RepeatString);
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (Len <> 0) then begin
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      for i := 0 to pred(ActualReps) do begin
        Move(RepeatString[0], Dest[0], Len);
        inc(Dest, Len);
      end;
      Dest^ := #0;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function TrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
begin
  Result := LeftTrimCharsZ(Dest, RightTrimCharsZ(Dest, S, Chars), Chars);
end;

{----------------------------------------------------------------------------}

function RightTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
var
  EndS : PAnsiChar;
begin
  Result := Dest;
  EndS := StrEnd(S);
  while (EndS <> S) do begin
    dec(EndS);
    if not CharExistsZ(Chars, EndS^) then begin
      if (Dest <> S) then
        StrLCopy(Dest, S, succ(EndS - S))
      else begin
        inc(EndS);
        EndS^ := #0;
      end;
      Exit;
    end;
  end;
  Result^ := #0;
end;

{----------------------------------------------------------------------------}

function LeftTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;
begin
  Result := Dest;
  while (S^ <> #0) and CharExistsZ(Chars, S^) do
    inc(S);
  if (S^ <> #0) then
    StrCopy(Result, S)
  else
    Result^ := #0;
end;

{----------------------------------------------------------------------------}

function ExtractTokensZ(S, Delims    : PAnsiChar;
                        QuoteChar    : AnsiChar;
                        AllowNulls   : Boolean;
                        Tokens       : TStrings) : Cardinal;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : PAnsiChar;
  TokenStart : PAnsiChar;
  TempStr    : PAnsiChar;
  SLen       : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken (4)
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote (4)
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping
    (4) if the quote character is #0, it's taken to mean that the
        routine should not check for quoted substrings. These marked
        transitions are the only places this is checked.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S[0] = #0) or
     (Delims[0] = #0) or
     CharExistsZ(Delims, QuoteChar) then
    Exit;

  {allocate ourselves some scratch space for temporary tokens}
  SLen := StrLen(S);
  GetMem(TempStr, SLen+1);
  try

    {start off in the normal scanning state}
    State := ScanStart;

    {the first token starts at the beginning of the string}
    TokenStart := S;

    {read through the entire string}
    CurChar := S;
    while (CurChar^ <> #0) do begin

      {process the character according to the current state}
      case State of
        ScanStart :
          begin
            {if the current char is the quote character, switch
             states}
            if (QuoteChar <> #0) and (CurChar^ = QuoteChar) then
              State := ScanQuotedToken

            {if the current char is a delimiter, output a null token}
            else if CharExistsZ(Delims, CurChar^) then begin

              {if allowed to, output a null token}
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end;

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;
            end

            {otherwise, the current char is starting a normal token,
             so switch states}
            else
              State := ScanNormalToken
          end;

        ScanQuotedToken :
          begin
            {if the current char is the quote character, switch
             states}
            if (CurChar^ = QuoteChar) then
              State := ScanQuotedTokenEnd
          end;

        ScanQuotedTokenEnd :
          begin
            {if the current char is the quote character, we have a
             token consisting of two (or more) quoted substrings, so
             switch states}
            if (CurChar^ = QuoteChar) then
              State := ScanNormalTokenWithQuote

            {if the current char is a delimiter, output the token
             without the quotes}
            else if CharExistsZ(Delims, CurChar^) then begin

              {if the token is empty without the quotes, output a null
               token only if allowed to}
              if ((CurChar - TokenStart) = 2) then begin
                if AllowNulls then begin
                  Tokens.Add('');
                  inc(Result);
                end
              end

              {else output the token without the quotes}
              else begin
                inc(TokenStart);
                StrLCopy(TempStr, TokenStart, CurChar - TokenStart - 1);
                Tokens.Add(StrPas(TempStr));
                inc(Result);
              end;

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;

              {switch states back to the start state}
              State := ScanStart;
            end

            {otherwise it's a (complex) normal token, so switch
             states}
            else
              State := ScanNormalToken
          end;

        ScanNormalToken :
          begin
            {if the current char is the quote character, we have a
             complex token with at least one quoted substring, so
             switch states}
            if (QuoteChar <> #0) and (CurChar^ = QuoteChar) then
              State := ScanNormalTokenWithQuote

            {if the current char is a delimiter, output the token}
            else if CharExistsZ(Delims, CurChar^) then begin
              StrLCopy(TempStr, TokenStart, CurChar - TokenStart);
              Tokens.Add(StrPas(TempStr));
              inc(Result);

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;

              {switch states back to the start state}
              State := ScanStart;
            end;
          end;

        ScanNormalTokenWithQuote :
          begin
            {if the current char is the quote character, switch states
             back to scanning a normal token}
            if (CurChar^ = QuoteChar) then
              State := ScanNormalToken;
          end;

      end;

      inc(CurChar);
    end;

    {we need to process the (possible) final token}

    {if we are in the scanning quoted token state, we've read an
     opening quote, but no closing one; increment the token start
     value}
    if (State = ScanQuotedToken) then
      inc(TokenStart)

    {if we've finished scanning a quoted token, we've read both
     quotes; increment the token start value, and decrement the
     current index}
    else if (State = ScanQuotedTokenEnd) then begin
      inc(TokenStart);
      dec(CurChar);
    end;

    {if the final token is not empty, output the token}
    if (TokenStart < CurChar) then begin
      StrLCopy(TempStr, TokenStart, CurChar - TokenStart);
      Tokens.Add(StrPas(TempStr));
      inc(Result);
    end
    {otherwise the final token is empty, so output a null token if
     allowed to}
    else if AllowNulls then begin
      Tokens.Add('');
      inc(Result);
    end;

  finally
    FreeMem(TempStr, SLen+1);
  end;
end;

{----------------------------------------------------------------------------}

function ContainsOnlyZ(const S, Chars : PAnsiChar;
                       var BadPos : Cardinal) : Boolean;
var
  Walker : PAnsiChar;
begin
  {if the input string is empty, exit}
  if (S^ = #0) then begin
    Result := false;
    BadPos := 0;
    Exit;
  end;
  {otherwise walk through the string until we reach the end or we find
   the first char not in our list}
  Walker := S;
  while (Walker^ <> #0) do begin
    if not CharExistsZ(Chars, Walker^) then begin
      BadPos := Walker - S;
      Result := false;
      Exit;
    end;
    inc(Walker);
  end;
  {if we reach here, all chars are in the list}
  Result := true;
  BadPos := 0;
end;

{----------------------------------------------------------------------------}

function ContainsOtherThanZ(const S, Chars : PAnsiChar;
                            var BadPos : Cardinal) : Boolean;
var
  Walker : PAnsiChar;
begin
  {if the input string is empty, exit}
  if (S^ = #0) then begin
    Result := false;
    BadPos := 0;
    Exit;
  end;
  {otherwise walk through the string until we reach the end or we find
   the first char not in our list}
  Walker := S;
  while (Walker^ <> #0) do begin
    if not CharExistsZ(Chars, Walker^) then begin
      BadPos := Walker - S;
      Result := true;
      Exit;
    end;
    inc(Walker);
  end;
  {if we reach here, all chars are in the list}
  Result := false;
  BadPos := 0;
end;

{----------------------------------------------------------------------------}

function IsChAlphaZ(C : AnsiChar) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := IsCharAlpha(@C);
end;

{----------------------------------------------------------------------------}

function IsChNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := CharExistsZ(Numbers, C);
end;

{----------------------------------------------------------------------------}

function IsChAlphaNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := IsCharAlpha(@C) or CharExistsZ(Numbers, C);
end;

{----------------------------------------------------------------------------}

function IsStrAlphaZ(S : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are an alpha}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if not IsCharAlpha(S) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;

{----------------------------------------------------------------------------}

function IsStrNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are in numeric set}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if not CharExistsZ(Numbers, S^) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;

{----------------------------------------------------------------------------}

function IsStrAlphaNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if (not IsCharAlpha(S)) and
         (not CharExistsZ(Numbers, S^)) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;


function StrWithinZ(S, SearchStr : PAnsiChar;
                    Start        : Cardinal;
                    var Position : Cardinal) : Boolean;
var
  TmpStr : PAnsiChar;
begin
  GetMem(TmpStr, StrLen(S) + 1);
  try
    StrCopy(TmpStr, S);
    if (Start > 0) then
      StrStDeleteZ(TmpStr, TmpStr, 0, Start);
    Result := StrStPosZ(TmpStr, SearchStr, Position);
    if (Result) then
      Position := Position + Start;
  finally
    FreeMem(TmpStr, StrLen(S) + 1);
  end;
end;

end.
