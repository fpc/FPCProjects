{******************************************************************************}
{                                                                              }
{ JmHistogram.inc for Jedi Math Alpha 1.02                                     }
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
{ This include file defines various JmHistogram specific compiler options.     }
{                                                                              }
{ Unit owner: Patrick van Laake                                                }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

// Establishing the compiler version and the default component base. For D6 and
// D7 the default component base is VCL. You can change this to CLX by enabling
// the $DEFINE below, or by using the Project|Options|Conditionals dialog.

{$DEFINE OLDVER}

{$IFDEF VER150}
  {$DEFINE DELPHI7}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE DELPHI6}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DELPHI5}
  {$UNDEF CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$ifndef FPC}
{$IFDEF LINUX}
 {$DEFINE CLX}
{$ENDIF}
{$endif}

{$ifndef FPC}
{$IFDEF OLDVER}
DSpatial will only compile successfully with Delphi 5-7, or Kylix. 
You may be able to patch the code to work with older versions of 
Delphi, but this is not supported.
{$ENDIF}
{$endif}



