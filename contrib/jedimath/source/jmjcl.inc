{******************************************************************************}

{                                                                              }

{ JmJcl.inc for Jedi Math Alpha 1.02                                           }

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

{ The Original Code is JCL.INC.                                                }

{                                                                              }

{ This include file defines various JCL specific defines. The more generic     }

{ defines are defined in the JEDI.INC file which is shared with the JEDI VCL.  }

{                                                                              }

{ Unit owner: Marcel van Brakel                                                }

{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants
{$T-} // Type checked pointers off

{$INCLUDE JmJedi.inc} // Pull in the JCL/J-VCL shared directives

// Math precision selection, mutually exclusive

{$DEFINE MATH_EXTENDED_PRECISION}
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}

// Math angle selection, mutually exclusive

{.$DEFINE MATH_ANGLES_DEGREES}
{.$DEFINE MATH_ANGLES_GRADS}
{$DEFINE MATH_ANGLES_RADIANS}

// Free Pascal specific

{$DEFINE UseRegAsm}
{$DEFINE UseAsm}

{$IFDEF FPC}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_OVERLOAD}
  {$ASMMODE Intel}
  {$UNDEF UseRegAsm}
{$ENDIF}


