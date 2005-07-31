{******************************************************************************}
{                                                                              }
{ JediMath.inc for Jedi Math Alpha 1.02                                        }
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
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{******************************************************************************}

//
// Pull in the JCL/JVCL shared directives
//
//----------------------------------------------------------------------
{$INCLUDE JmJedi.inc}

//
// Math precision selection; these are mutually exclusive; currently
// only single and double precision are supported
//
//----------------------------------------------------------------------
{$DEFINE jmDoublePrecision}
{.$DEFINE jmSinglePrecision}
{.$DEFINE jmExtendedPrecision}
//
// optimized code stuff
//
//----------------------------------------------------------------------
{$DEFINE jmUseAssemblerForSpeed}
//
// Free Pascal specific
//
//----------------------------------------------------------------------
{$IFDEF FPC}
// nothing here yet...
{$ENDIF}
//
// Delphi version-specific definitions
//
//----------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}  // Platform warnings off
{$ENDIF}


