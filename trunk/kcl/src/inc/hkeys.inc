{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Keyboard-related definitions for KCL
//
//	Where possible, all values are compatible to
//	GII, the General Input Interface (see
//	www.ggi-project.org), as defined in
//	gii/keyboard.h
// ===================================================================


const

  // Function Keys

  keyF0  = $e100;
  keyF1  = $e101;
  keyF2  = $e102;
  keyF3  = $e103;
  keyF4  = $e104;
  keyF5  = $e105;
  keyF6  = $e106;
  keyF7  = $e107;
  keyF8  = $e108;
  keyF9  = $e109;
  keyF10 = $e10a;
  keyF11 = $e10b;
  keyF12 = $e10c;
  keyF13 = $e10d;
  keyF14 = $e10e;
  keyF15 = $e10f;
  keyF16 = $e110;
  keyF17 = $e111;
  keyF18 = $e112;
  keyF19 = $e113;
  keyF20 = $e114;
  keyF21 = $e115;
  keyF22 = $e116;
  keyF23 = $e117;
  keyF24 = $e118;
  keyF25 = $e119;
  keyF26 = $e11a;
  keyF27 = $e11b;
  keyF28 = $e11c;
  keyF29 = $e11d;
  keyF30 = $e11e;
  keyF31 = $e11f;
  keyF32 = $e120;
  keyF33 = $e121;
  keyF34 = $e122;
  keyF35 = $e123;
  keyF36 = $e124;
  keyF37 = $e125;
  keyF38 = $e126;
  keyF39 = $e127;
  keyF40 = $e128;
  keyF41 = $e129;
  keyF42 = $e12a;
  keyF43 = $e12b;
  keyF44 = $e12c;
  keyF45 = $e12d;
  keyF46 = $e12e;
  keyF47 = $e12f;
  keyF48 = $e130;
  keyF49 = $e131;
  keyF50 = $e132;
  keyF51 = $e133;
  keyF52 = $e134;
  keyF53 = $e135;
  keyF54 = $e136;
  keyF55 = $e137;
  keyF56 = $e138;
  keyF57 = $e139;
  keyF58 = $e13a;
  keyF59 = $e13b;
  keyF60 = $e13c;
  keyF61 = $e13d;
  keyF62 = $e13e;
  keyF63 = $e13f;


  // Cursor Keys

  keyInsert   = $e022;
  keyUp       = $e032;
  keyDown     = $e033;
  keyLeft     = $e034;
  keyRight    = $e035;
  keyPageUp   = $e036;
  keyPageDown = $e037;
  keyHome     = $e038;
  keyEnd      = $e039;

  // Misc. keys
  keyBackspace = $08;
  keyTab       = $09;
  keyReturn    = $0d;
  keyEscape    = $1b;
  keyDelete    = $7f;


{
  $Log$
  Revision 1.2  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:22  sg
  Initial import

}
