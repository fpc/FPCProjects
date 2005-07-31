{******************************************************************************}
{                                                                              }
{ JmTypes.pas for Jedi Math Alpha 1.02                                         }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  The Original Code is JmTypes.pas..                                          }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains various routine for manipulating the math coprocessor.    }
{ This includes such things as querying and setting the rounding precision of  }
{ floating point operations and retrieving the coprocessor's status word.      }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains                                                           }
{                                                                              }
{ Unit owner:    Chris Eyre                                                    }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

unit JmTypes;
interface
{$I JediMath.inc}

type
{$ifdef jmDoublePrecision}
  TJmFloat = double;
{$endif jmDoublePrecision}

{$ifdef jmSinglePrecision}
  TJmFloat = single;
{$endif jmSinglePrecision}

{$ifdef jmExtendedPrecision}
  TJmFloat = extended;
{$endif jmExtendedPrecision}

  TJmInteger = longint;

  PLargeInteger = ^TLargeInteger;
  TLargeInteger = record
    case Integer of
    0: (
      LowPart: LongWord;
      HighPart: Longint);
    1: (
      QuadPart: Int64);
  end;

  PULargeInteger = ^TULargeInteger;
  TULargeInteger = record
    case Integer of
    0: (
      LowPart: LongWord;
      HighPart: LongWord);
    1: (
      QuadPart: Int64);
  end;

implementation

end.


