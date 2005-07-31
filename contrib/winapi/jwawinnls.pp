{******************************************************************************}
{                                                       	               }
{ National Language Support API interface Unit for Object Pascal               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winnls.h, released June 2000. The original Pascal      }
{ code is: WinNLS.pas, released December 2000. The initial developer of the    }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{								               }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{								               }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{ 								               }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{ 								               }
{******************************************************************************}

unit JwaWinNLS;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinNls.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType, JwaWinBase;

////////////////////////////////////////////////////////////////////////////
//
//  Constants
//
//  Define all constants for the NLS component here.
//
////////////////////////////////////////////////////////////////////////////

//
//  String Length Maximums.
//

const
  MAX_LEADBYTES   = 12; // 5 ranges, 2 bytes ea., 0 term.
  MAX_DEFAULTCHAR = 2; // single or double byte

//
//  MBCS and Unicode Translation Flags.
//

  MB_PRECOMPOSED       = $00000001; // use precomposed chars
  MB_COMPOSITE         = $00000002; // use composite chars
  MB_USEGLYPHCHARS     = $00000004; // use glyph chars, not ctrl chars
  MB_ERR_INVALID_CHARS = $00000008; // error for invalid chars

  WC_COMPOSITECHECK = $00000200; // convert composite to precomposed
  WC_DISCARDNS      = $00000010; // discard non-spacing chars
  WC_SEPCHARS       = $00000020; // generate separate chars
  WC_DEFAULTCHAR    = $00000040; // replace w/ default char

  WC_NO_BEST_FIT_CHARS = $00000400; // do not use best fit chars

//
//  Character Type Flags.
//

  CT_CTYPE1 = $00000001; // ctype 1 information
  CT_CTYPE2 = $00000002; // ctype 2 information
  CT_CTYPE3 = $00000004; // ctype 3 information

//
//  CType 1 Flag Bits.
//

  C1_UPPER  = $0001; // upper case
  C1_LOWER  = $0002; // lower case
  C1_DIGIT  = $0004; // decimal digits
  C1_SPACE  = $0008; // spacing characters
  C1_PUNCT  = $0010; // punctuation characters
  C1_CNTRL  = $0020; // control characters
  C1_BLANK  = $0040; // blank characters
  C1_XDIGIT = $0080; // other digits
  C1_ALPHA  = $0100; // any linguistic character
  C1_DEFINED = $0200; // defined character

//
//  CType 2 Flag Bits.
//

  C2_LEFTTORIGHT = $0001; // left to right
  C2_RIGHTTOLEFT = $0002; // right to left

  C2_EUROPENUMBER     = $0003; // European number, digit
  C2_EUROPESEPARATOR  = $0004; // European numeric separator
  C2_EUROPETERMINATOR = $0005; // European numeric terminator
  C2_ARABICNUMBER     = $0006; // Arabic number
  C2_COMMONSEPARATOR  = $0007; // common numeric separator

  C2_BLOCKSEPARATOR   = $0008; // block separator
  C2_SEGMENTSEPARATOR = $0009; // segment separator
  C2_WHITESPACE       = $000A; // white space
  C2_OTHERNEUTRAL     = $000B; // other neutrals

  C2_NOTAPPLICABLE = $0000; // no implicit directionality

//
//  CType 3 Flag Bits.
//

  C3_NONSPACING = $0001; // nonspacing character
  C3_DIACRITIC  = $0002; // diacritic mark
  C3_VOWELMARK  = $0004; // vowel mark
  C3_SYMBOL     = $0008; // symbols

  C3_KATAKANA  = $0010; // katakana character
  C3_HIRAGANA  = $0020; // hiragana character
  C3_HALFWIDTH = $0040; // half width character
  C3_FULLWIDTH = $0080; // full width character
  C3_IDEOGRAPH = $0100; // ideographic character
  C3_KASHIDA   = $0200; // Arabic kashida character
  C3_LEXICAL   = $0400; // lexical character

  C3_ALPHA = $8000; // any linguistic char (C1_ALPHA)

  C3_NOTAPPLICABLE = $0000; // ctype 3 is not applicable

//
//  String Flags.
//

  NORM_IGNORECASE     = $00000001; // ignore case
  NORM_IGNORENONSPACE = $00000002; // ignore nonspacing chars
  NORM_IGNORESYMBOLS  = $00000004; // ignore symbols

  NORM_IGNOREKANATYPE = $00010000; // ignore kanatype
  NORM_IGNOREWIDTH    = $00020000; // ignore width

//
//  Locale Independent Mapping Flags.
//

  MAP_FOLDCZONE   = $00000010; // fold compatibility zone chars
  MAP_PRECOMPOSED = $00000020; // convert to precomposed chars
  MAP_COMPOSITE   = $00000040; // convert to composite chars
  MAP_FOLDDIGITS  = $00000080; // all digits to ASCII 0-9

  MAP_EXPAND_LIGATURES = $00002000; // expand all ligatures

//
//  Locale Dependent Mapping Flags.
//

  LCMAP_LOWERCASE = $00000100; // lower case letters
  LCMAP_UPPERCASE = $00000200; // upper case letters
  LCMAP_SORTKEY   = $00000400; // WC sort key (normalize)
  LCMAP_BYTEREV   = $00000800; // byte reversal

  LCMAP_HIRAGANA  = $00100000; // map katakana to hiragana
  LCMAP_KATAKANA  = $00200000; // map hiragana to katakana
  LCMAP_HALFWIDTH = $00400000; // map double byte to single byte
  LCMAP_FULLWIDTH = $00800000; // map single byte to double byte

  LCMAP_LINGUISTIC_CASING = $01000000; // use linguistic rules for casing

  LCMAP_SIMPLIFIED_CHINESE  = $02000000; // map traditional chinese to simplified chinese
  LCMAP_TRADITIONAL_CHINESE = $04000000; // map simplified chinese to traditional chinese

//
//  Language Group Enumeration Flags.
//

  LGRPID_INSTALLED = $00000001; // installed language group ids
  LGRPID_SUPPORTED = $00000002; // supported language group ids

//
//  Locale Enumeration Flags.
//

  LCID_INSTALLED       = $00000001; // installed locale ids
  LCID_SUPPORTED       = $00000002; // supported locale ids
  LCID_ALTERNATE_SORTS = $00000004; // alternate sort locale ids

//
//  Code Page Enumeration Flags.
//

  CP_INSTALLED = $00000001; // installed code page ids
  CP_SUPPORTED = $00000002; // supported code page ids

//
//  Sorting Flags.
//
//    WORD Sort:    culturally correct sort
//                  hyphen and apostrophe are special cased
//                  example: "coop" and "co-op" will sort together in a list
//
//                        co_op     <-------  underscore (symbol)
//                        coat
//                        comb
//                        coop
//                        co-op     <-------  hyphen (punctuation)
//                        cork
//                        went
//                        were
//                        we're     <-------  apostrophe (punctuation)
//
//
//    STRING Sort:  hyphen and apostrophe will sort with all other symbols
//
//                        co-op     <-------  hyphen (punctuation)
//                        co_op     <-------  underscore (symbol)
//                        coat
//                        comb
//                        coop
//                        cork
//                        we're     <-------  apostrophe (punctuation)
//                        went
//                        were
//

  SORT_STRINGSORT = $00001000; // use string sort method

//
//  Compare String Return Values.
//

  CSTR_LESS_THAN    = 1; // string 1 less than string 2
  CSTR_EQUAL        = 2; // string 1 equal to string 2
  CSTR_GREATER_THAN = 3; // string 1 greater than string 2

//
//  Code Page Default Values.
//

  CP_ACP        = 0; // default to ANSI code page
  CP_OEMCP      = 1; // default to OEM  code page
  CP_MACCP      = 2; // default to MAC  code page
  CP_THREAD_ACP = 3; // current thread's ANSI code page
  CP_SYMBOL     = 42; // SYMBOL translations

  CP_UTF7 = 65000; // UTF-7 translation
  CP_UTF8 = 65001; // UTF-8 translation

//
//  Country/Region Codes.
//

  CTRY_DEFAULT = 0;

  CTRY_ALBANIA            = 355; // Albania
  CTRY_ALGERIA            = 213; // Algeria
  CTRY_ARGENTINA          = 54; // Argentina
  CTRY_ARMENIA            = 374; // Armenia
  CTRY_AUSTRALIA          = 61; // Australia
  CTRY_AUSTRIA            = 43; // Austria
  CTRY_AZERBAIJAN         = 994; // Azerbaijan
  CTRY_BAHRAIN            = 973; // Bahrain
  CTRY_BELARUS            = 375; // Belarus
  CTRY_BELGIUM            = 32; // Belgium
  CTRY_BELIZE             = 501; // Belize
  CTRY_BOLIVIA            = 591; // Bolivia
  CTRY_BRAZIL             = 55; // Brazil
  CTRY_BRUNEI_DARUSSALAM  = 673; // Brunei Darussalam
  CTRY_BULGARIA           = 359; // Bulgaria
  CTRY_CANADA             = 2; // Canada
  CTRY_CARIBBEAN          = 1; // Caribbean
  CTRY_CHILE              = 56; // Chile
  CTRY_COLOMBIA           = 57; // Colombia
  CTRY_COSTA_RICA         = 506; // Costa Rica
  CTRY_CROATIA            = 385; // Croatia
  CTRY_CZECH              = 420; // Czech Republic
  CTRY_DENMARK            = 45; // Denmark
  CTRY_DOMINICAN_REPUBLIC = 1; // Dominican Republic
  CTRY_ECUADOR            = 593; // Ecuador
  CTRY_EGYPT              = 20; // Egypt
  CTRY_EL_SALVADOR        = 503; // El Salvador
  CTRY_ESTONIA            = 372; // Estonia
  CTRY_FAEROE_ISLANDS     = 298; // Faeroe Islands
  CTRY_FINLAND            = 358; // Finland
  CTRY_FRANCE             = 33; // France
  CTRY_GEORGIA            = 995; // Georgia
  CTRY_GERMANY            = 49; // Germany
  CTRY_GREECE             = 30; // Greece
  CTRY_GUATEMALA          = 502; // Guatemala
  CTRY_HONDURAS           = 504; // Honduras
  CTRY_HONG_KONG          = 852; // Hong Kong S.A.R., P.R.C.
  CTRY_HUNGARY            = 36; // Hungary
  CTRY_ICELAND            = 354; // Iceland
  CTRY_INDIA              = 91; // India
  CTRY_INDONESIA          = 62; // Indonesia
  CTRY_IRAN               = 981; // Iran
  CTRY_IRAQ               = 964; // Iraq
  CTRY_IRELAND            = 353; // Ireland
  CTRY_ISRAEL             = 972; // Israel
  CTRY_ITALY              = 39; // Italy
  CTRY_JAMAICA            = 1; // Jamaica
  CTRY_JAPAN              = 81; // Japan
  CTRY_JORDAN             = 962; // Jordan
  CTRY_KAZAKSTAN          = 7; // Kazakstan
  CTRY_KENYA              = 254; // Kenya
  CTRY_KUWAIT             = 965; // Kuwait
  CTRY_KYRGYZSTAN         = 996; // Kyrgyzstan
  CTRY_LATVIA             = 371; // Latvia
  CTRY_LEBANON            = 961; // Lebanon
  CTRY_LIBYA              = 218; // Libya
  CTRY_LIECHTENSTEIN      = 41; // Liechtenstein
  CTRY_LITHUANIA          = 370; // Lithuania
  CTRY_LUXEMBOURG         = 352; // Luxembourg
  CTRY_MACAU              = 853; // Macau S.A.R., PRC
  CTRY_MACEDONIA          = 389; // Former Yugoslav Republic of Macedonia
  CTRY_MALAYSIA           = 60; // Malaysia
  CTRY_MALDIVES           = 960; // Maldives
  CTRY_MEXICO             = 52; // Mexico
  CTRY_MONACO             = 33; // Principality of Monaco
  CTRY_MONGOLIA           = 976; // Mongolia
  CTRY_MOROCCO            = 212; // Morocco
  CTRY_NETHERLANDS        = 31; // Netherlands
  CTRY_NEW_ZEALAND        = 64; // New Zealand
  CTRY_NICARAGUA          = 505; // Nicaragua
  CTRY_NORWAY             = 47; // Norway
  CTRY_OMAN               = 968; // Oman
  CTRY_PAKISTAN           = 92; // Islamic Republic of Pakistan
  CTRY_PANAMA             = 507; // Panama
  CTRY_PARAGUAY           = 595; // Paraguay
  CTRY_PERU               = 51; // Peru
  CTRY_PHILIPPINES        = 63; // Republic of the Philippines
  CTRY_POLAND             = 48; // Poland
  CTRY_PORTUGAL           = 351; // Portugal
  CTRY_PRCHINA            = 86; // People's Republic of China
  CTRY_PUERTO_RICO        = 1; // Puerto Rico
  CTRY_QATAR              = 974; // Qatar
  CTRY_ROMANIA            = 40; // Romania
  CTRY_RUSSIA             = 7; // Russia
  CTRY_SAUDI_ARABIA       = 966; // Saudi Arabia
  CTRY_SERBIA             = 381; // Serbia
  CTRY_SINGAPORE          = 65; // Singapore
  CTRY_SLOVAK             = 421; // Slovak Republic
  CTRY_SLOVENIA           = 386; // Slovenia
  CTRY_SOUTH_AFRICA       = 27; // South Africa
  CTRY_SOUTH_KOREA        = 82; // Korea
  CTRY_SPAIN              = 34; // Spain
  CTRY_SWEDEN             = 46; // Sweden
  CTRY_SWITZERLAND        = 41; // Switzerland
  CTRY_SYRIA              = 963; // Syria
  CTRY_TAIWAN             = 886; // Taiwan
  CTRY_TATARSTAN          = 7; // Tatarstan
  CTRY_THAILAND           = 66; // Thailand
  CTRY_TRINIDAD_Y_TOBAGO  = 1; // Trinidad y Tobago
  CTRY_TUNISIA            = 216; // Tunisia
  CTRY_TURKEY             = 90; // Turkey
  CTRY_UAE                = 971; // U.A.E.
  CTRY_UKRAINE            = 380; // Ukraine
  CTRY_UNITED_KINGDOM     = 44; // United Kingdom
  CTRY_UNITED_STATES      = 1; // United States
  CTRY_URUGUAY            = 598; // Uruguay
  CTRY_UZBEKISTAN         = 7; // Uzbekistan
  CTRY_VENEZUELA          = 58; // Venezuela
  CTRY_VIET_NAM           = 84; // Viet Nam
  CTRY_YEMEN              = 967; // Yemen
  CTRY_ZIMBABWE           = 263; // Zimbabwe

//
//  Locale Types.
//
//  These types are used for the GetLocaleInfo NLS API routine.
//  Some of these types are also used for the SetLocaleInfo NLS API routine.
//

//
//  The following LCTypes may be used in combination with any other LCTypes.
//
//    LOCALE_NOUSEROVERRIDE is also used in GetTimeFormat and
//    GetDateFormat.
//
//    LOCALE_USE_CP_ACP is used in many of the A (Ansi) apis that need
//    to do string translation.
//
//    LOCALE_RETURN_NUMBER will return the result from GetLocaleInfo as a
//    number instead of a string.  This flag is only valid for the LCTypes
//    beginning with LOCALE_I.
//

  LOCALE_NOUSEROVERRIDE = DWORD($80000000); // do not use user overrides
  LOCALE_USE_CP_ACP     = $40000000; // use the system ACP

  LOCALE_RETURN_NUMBER = $20000000; // return number instead of string

//
//  The following LCTypes are mutually exclusive in that they may NOT
//  be used in combination with each other.
//

  LOCALE_ILANGUAGE       = $00000001; // language id
  LOCALE_SLANGUAGE       = $00000002; // localized name of language
  LOCALE_SENGLANGUAGE    = $00001001; // English name of language
  LOCALE_SABBREVLANGNAME = $00000003; // abbreviated language name
  LOCALE_SNATIVELANGNAME = $00000004; // native name of language

  LOCALE_ICOUNTRY        = $00000005; // country code
  LOCALE_SCOUNTRY        = $00000006; // localized name of country
  LOCALE_SENGCOUNTRY     = $00001002; // English name of country
  LOCALE_SABBREVCTRYNAME = $00000007; // abbreviated country name
  LOCALE_SNATIVECTRYNAME = $00000008; // native name of country

  LOCALE_IDEFAULTLANGUAGE     = $00000009; // default language id
  LOCALE_IDEFAULTCOUNTRY      = $0000000A; // default country code
  LOCALE_IDEFAULTCODEPAGE     = $0000000B; // default oem code page
  LOCALE_IDEFAULTANSICODEPAGE = $00001004; // default ansi code page
  LOCALE_IDEFAULTMACCODEPAGE  = $00001011; // default mac code page

  LOCALE_SLIST    = $0000000C; // list item separator
  LOCALE_IMEASURE = $0000000D; // 0 = metric, 1 = US

  LOCALE_SDECIMAL      = $0000000E; // decimal separator
  LOCALE_STHOUSAND     = $0000000F; // thousand separator
  LOCALE_SGROUPING     = $00000010; // digit grouping
  LOCALE_IDIGITS       = $00000011; // number of fractional digits
  LOCALE_ILZERO        = $00000012; // leading zeros for decimal
  LOCALE_INEGNUMBER    = $00001010; // negative number mode
  LOCALE_SNATIVEDIGITS = $00000013; // native ascii 0-9

  LOCALE_SCURRENCY       = $00000014; // local monetary symbol
  LOCALE_SINTLSYMBOL     = $00000015; // intl monetary symbol
  LOCALE_SMONDECIMALSEP  = $00000016; // monetary decimal separator
  LOCALE_SMONTHOUSANDSEP = $00000017; // monetary thousand separator
  LOCALE_SMONGROUPING    = $00000018; // monetary grouping
  LOCALE_ICURRDIGITS     = $00000019; // # local monetary digits
  LOCALE_IINTLCURRDIGITS = $0000001A; // # intl monetary digits
  LOCALE_ICURRENCY       = $0000001B; // positive currency mode
  LOCALE_INEGCURR        = $0000001C; // negative currency mode

  LOCALE_SDATE         = $0000001D; // date separator
  LOCALE_STIME         = $0000001E; // time separator
  LOCALE_SSHORTDATE    = $0000001F; // short date format string
  LOCALE_SLONGDATE     = $00000020; // long date format string
  LOCALE_STIMEFORMAT   = $00001003; // time format string
  LOCALE_IDATE         = $00000021; // short date format ordering
  LOCALE_ILDATE        = $00000022; // long date format ordering
  LOCALE_ITIME         = $00000023; // time format specifier
  LOCALE_ITIMEMARKPOSN = $00001005; // time marker position
  LOCALE_ICENTURY      = $00000024; // century format specifier (short date)
  LOCALE_ITLZERO       = $00000025; // leading zeros in time field
  LOCALE_IDAYLZERO     = $00000026; // leading zeros in day field (short date)
  LOCALE_IMONLZERO     = $00000027; // leading zeros in month field (short date)
  LOCALE_S1159         = $00000028; // AM designator
  LOCALE_S2359         = $00000029; // PM designator

  LOCALE_ICALENDARTYPE     = $00001009; // type of calendar specifier
  LOCALE_IOPTIONALCALENDAR = $0000100B; // additional calendar types specifier
  LOCALE_IFIRSTDAYOFWEEK   = $0000100C; // first day of week specifier
  LOCALE_IFIRSTWEEKOFYEAR  = $0000100D; // first week of year specifier

  LOCALE_SDAYNAME1          = $0000002A; // long name for Monday
  LOCALE_SDAYNAME2          = $0000002B; // long name for Tuesday
  LOCALE_SDAYNAME3          = $0000002C; // long name for Wednesday
  LOCALE_SDAYNAME4          = $0000002D; // long name for Thursday
  LOCALE_SDAYNAME5          = $0000002E; // long name for Friday
  LOCALE_SDAYNAME6          = $0000002F; // long name for Saturday
  LOCALE_SDAYNAME7          = $00000030; // long name for Sunday
  LOCALE_SABBREVDAYNAME1    = $00000031; // abbreviated name for Monday
  LOCALE_SABBREVDAYNAME2    = $00000032; // abbreviated name for Tuesday
  LOCALE_SABBREVDAYNAME3    = $00000033; // abbreviated name for Wednesday
  LOCALE_SABBREVDAYNAME4    = $00000034; // abbreviated name for Thursday
  LOCALE_SABBREVDAYNAME5    = $00000035; // abbreviated name for Friday
  LOCALE_SABBREVDAYNAME6    = $00000036; // abbreviated name for Saturday
  LOCALE_SABBREVDAYNAME7    = $00000037; // abbreviated name for Sunday
  LOCALE_SMONTHNAME1        = $00000038; // long name for January
  LOCALE_SMONTHNAME2        = $00000039; // long name for February
  LOCALE_SMONTHNAME3        = $0000003A; // long name for March
  LOCALE_SMONTHNAME4        = $0000003B; // long name for April
  LOCALE_SMONTHNAME5        = $0000003C; // long name for May
  LOCALE_SMONTHNAME6        = $0000003D; // long name for June
  LOCALE_SMONTHNAME7        = $0000003E; // long name for July
  LOCALE_SMONTHNAME8        = $0000003F; // long name for August
  LOCALE_SMONTHNAME9        = $00000040; // long name for September
  LOCALE_SMONTHNAME10       = $00000041; // long name for October
  LOCALE_SMONTHNAME11       = $00000042; // long name for November
  LOCALE_SMONTHNAME12       = $00000043; // long name for December
  LOCALE_SMONTHNAME13       = $0000100E; // long name for 13th month (if exists)
  LOCALE_SABBREVMONTHNAME1  = $00000044; // abbreviated name for January
  LOCALE_SABBREVMONTHNAME2  = $00000045; // abbreviated name for February
  LOCALE_SABBREVMONTHNAME3  = $00000046; // abbreviated name for March
  LOCALE_SABBREVMONTHNAME4  = $00000047; // abbreviated name for April
  LOCALE_SABBREVMONTHNAME5  = $00000048; // abbreviated name for May
  LOCALE_SABBREVMONTHNAME6  = $00000049; // abbreviated name for June
  LOCALE_SABBREVMONTHNAME7  = $0000004A; // abbreviated name for July
  LOCALE_SABBREVMONTHNAME8  = $0000004B; // abbreviated name for August
  LOCALE_SABBREVMONTHNAME9  = $0000004C; // abbreviated name for September
  LOCALE_SABBREVMONTHNAME10 = $0000004D; // abbreviated name for October
  LOCALE_SABBREVMONTHNAME11 = $0000004E; // abbreviated name for November
  LOCALE_SABBREVMONTHNAME12 = $0000004F; // abbreviated name for December
  LOCALE_SABBREVMONTHNAME13 = $0000100F; // abbreviated name for 13th month (if exists)

  LOCALE_SPOSITIVESIGN   = $00000050; // positive sign
  LOCALE_SNEGATIVESIGN   = $00000051; // negative sign
  LOCALE_IPOSSIGNPOSN    = $00000052; // positive sign position
  LOCALE_INEGSIGNPOSN    = $00000053; // negative sign position
  LOCALE_IPOSSYMPRECEDES = $00000054; // mon sym precedes pos amt
  LOCALE_IPOSSEPBYSPACE  = $00000055; // mon sym sep by space from pos amt
  LOCALE_INEGSYMPRECEDES = $00000056; // mon sym precedes neg amt
  LOCALE_INEGSEPBYSPACE  = $00000057; // mon sym sep by space from neg amt

  LOCALE_FONTSIGNATURE    = $00000058; // font signature
  LOCALE_SISO639LANGNAME  = $00000059; // ISO abbreviated language name
  LOCALE_SISO3166CTRYNAME = $0000005A; // ISO abbreviated country name

  LOCALE_IDEFAULTEBCDICCODEPAGE = $00001012; // default ebcdic code page
  LOCALE_IPAPERSIZE             = $0000100A; // 1 = letter, 5 = legal, 8 = a3, 9 = a4
  LOCALE_SENGCURRNAME           = $00001007; // english name of currency
  LOCALE_SNATIVECURRNAME        = $00001008; // native name of currency
  LOCALE_SYEARMONTH             = $00001006; // year month format string
  LOCALE_SSORTNAME              = $00001013; // sort name
  LOCALE_IDIGITSUBSTITUTION     = $00001014; // 0 = context, 1 = none, 2 = national

//
//  Time Flags for GetTimeFormat.
//

  TIME_NOMINUTESORSECONDS = $00000001; // do not use minutes or seconds
  TIME_NOSECONDS          = $00000002; // do not use seconds
  TIME_NOTIMEMARKER       = $00000004; // do not use time marker
  TIME_FORCE24HOURFORMAT  = $00000008; // always use 24 hour format

//
//  Date Flags for GetDateFormat.
//

  DATE_SHORTDATE        = $00000001; // use short date picture
  DATE_LONGDATE         = $00000002; // use long date picture
  DATE_USE_ALT_CALENDAR = $00000004; // use alternate calendar (if any)

  DATE_YEARMONTH  = $00000008; // use year month picture
  DATE_LTRREADING = $00000010; // add marks for left to right reading order layout
  DATE_RTLREADING = $00000020; // add marks for right to left reading order layout

//
//  Calendar Types.
//
//  These types are used for the EnumCalendarInfo and GetCalendarInfo
//  NLS API routines.
//  Some of these types are also used for the SetCalendarInfo NLS API
//  routine.
//

//
//  The following CalTypes may be used in combination with any other CalTypes.
//
//    CAL_NOUSEROVERRIDE
//
//    CAL_USE_CP_ACP is used in the A (Ansi) apis that need to do string
//    translation.
//
//    CAL_RETURN_NUMBER will return the result from GetCalendarInfo as a
//    number instead of a string.  This flag is only valid for the CalTypes
//    beginning with CAL_I.
//

  CAL_NOUSEROVERRIDE = LOCALE_NOUSEROVERRIDE; // do not use user overrides
  CAL_USE_CP_ACP     = LOCALE_USE_CP_ACP; // use the system ACP
  CAL_RETURN_NUMBER  = LOCALE_RETURN_NUMBER; // return number instead of string

//
//  The following CalTypes are mutually exclusive in that they may NOT
//  be used in combination with each other.
//

  CAL_ICALINTVALUE       = $00000001; // calendar type
  CAL_SCALNAME           = $00000002; // native name of calendar
  CAL_IYEAROFFSETRANGE   = $00000003; // starting years of eras
  CAL_SERASTRING         = $00000004; // era name for IYearOffsetRanges
  CAL_SSHORTDATE         = $00000005; // short date format string
  CAL_SLONGDATE          = $00000006; // long date format string
  CAL_SDAYNAME1          = $00000007; // native name for Monday
  CAL_SDAYNAME2          = $00000008; // native name for Tuesday
  CAL_SDAYNAME3          = $00000009; // native name for Wednesday
  CAL_SDAYNAME4          = $0000000a; // native name for Thursday
  CAL_SDAYNAME5          = $0000000b; // native name for Friday
  CAL_SDAYNAME6          = $0000000c; // native name for Saturday
  CAL_SDAYNAME7          = $0000000d; // native name for Sunday
  CAL_SABBREVDAYNAME1    = $0000000e; // abbreviated name for Monday
  CAL_SABBREVDAYNAME2    = $0000000f; // abbreviated name for Tuesday
  CAL_SABBREVDAYNAME3    = $00000010; // abbreviated name for Wednesday
  CAL_SABBREVDAYNAME4    = $00000011; // abbreviated name for Thursday
  CAL_SABBREVDAYNAME5    = $00000012; // abbreviated name for Friday
  CAL_SABBREVDAYNAME6    = $00000013; // abbreviated name for Saturday
  CAL_SABBREVDAYNAME7    = $00000014; // abbreviated name for Sunday
  CAL_SMONTHNAME1        = $00000015; // native name for January
  CAL_SMONTHNAME2        = $00000016; // native name for February
  CAL_SMONTHNAME3        = $00000017; // native name for March
  CAL_SMONTHNAME4        = $00000018; // native name for April
  CAL_SMONTHNAME5        = $00000019; // native name for May
  CAL_SMONTHNAME6        = $0000001a; // native name for June
  CAL_SMONTHNAME7        = $0000001b; // native name for July
  CAL_SMONTHNAME8        = $0000001c; // native name for August
  CAL_SMONTHNAME9        = $0000001d; // native name for September
  CAL_SMONTHNAME10       = $0000001e; // native name for October
  CAL_SMONTHNAME11       = $0000001f; // native name for November
  CAL_SMONTHNAME12       = $00000020; // native name for December
  CAL_SMONTHNAME13       = $00000021; // native name for 13th month (if any)
  CAL_SABBREVMONTHNAME1  = $00000022; // abbreviated name for January
  CAL_SABBREVMONTHNAME2  = $00000023; // abbreviated name for February
  CAL_SABBREVMONTHNAME3  = $00000024; // abbreviated name for March
  CAL_SABBREVMONTHNAME4  = $00000025; // abbreviated name for April
  CAL_SABBREVMONTHNAME5  = $00000026; // abbreviated name for May
  CAL_SABBREVMONTHNAME6  = $00000027; // abbreviated name for June
  CAL_SABBREVMONTHNAME7  = $00000028; // abbreviated name for July
  CAL_SABBREVMONTHNAME8  = $00000029; // abbreviated name for August
  CAL_SABBREVMONTHNAME9  = $0000002a; // abbreviated name for September
  CAL_SABBREVMONTHNAME10 = $0000002b; // abbreviated name for October
  CAL_SABBREVMONTHNAME11 = $0000002c; // abbreviated name for November
  CAL_SABBREVMONTHNAME12 = $0000002d; // abbreviated name for December
  CAL_SABBREVMONTHNAME13 = $0000002e; // abbreviated name for 13th month (if any)

  CAL_SYEARMONTH       = $0000002f; // year month format string
  CAL_ITWODIGITYEARMAX = $00000030; // two digit year max

//
//  Calendar Enumeration Value.
//

  ENUM_ALL_CALENDARS = DWORD($ffffffff); // enumerate all calendars

//
//  Calendar ID Values.
//

  CAL_GREGORIAN              = 1; // Gregorian (localized) calendar
  CAL_GREGORIAN_US           = 2; // Gregorian (U.S.) calendar
  CAL_JAPAN                  = 3; // Japanese Emperor Era calendar
  CAL_TAIWAN                 = 4; // Taiwan Era calendar
  CAL_KOREA                  = 5; // Korean Tangun calendar
  CAL_HIJRI                  = 6; // Hijri (Arabic Lunar) calendar
  CAL_THAI                   = 7; // Thai calendar
  CAL_HEBREW                 = 8; // Hebrew (Lunar) calendar
  CAL_GREGORIAN_ME_FRENCH    = 9; // Gregorian Middle East French calendar
  CAL_GREGORIAN_ARABIC       = 10; // Gregorian Arabic calendar
  CAL_GREGORIAN_XLIT_ENGLISH = 11; // Gregorian Transliterated English calendar
  CAL_GREGORIAN_XLIT_FRENCH  = 12; // Gregorian Transliterated French calendar

//
//  Language Group ID Values.
//

  LGRPID_WESTERN_EUROPE      = $0001; // Western Europe & U.S.
  LGRPID_CENTRAL_EUROPE      = $0002; // Central Europe
  LGRPID_BALTIC              = $0003; // Baltic
  LGRPID_GREEK               = $0004; // Greek
  LGRPID_CYRILLIC            = $0005; // Cyrillic
  LGRPID_TURKISH             = $0006; // Turkish
  LGRPID_JAPANESE            = $0007; // Japanese
  LGRPID_KOREAN              = $0008; // Korean
  LGRPID_TRADITIONAL_CHINESE = $0009; // Traditional Chinese
  LGRPID_SIMPLIFIED_CHINESE  = $000a; // Simplified Chinese
  LGRPID_THAI                = $000b; // Thai
  LGRPID_HEBREW              = $000c; // Hebrew
  LGRPID_ARABIC              = $000d; // Arabic
  LGRPID_VIETNAMESE          = $000e; // Vietnamese
  LGRPID_INDIC               = $000f; // Indic
  LGRPID_GEORGIAN            = $0010; // Georgian
  LGRPID_ARMENIAN            = $0011; // Armenian

////////////////////////////////////////////////////////////////////////////
//
//  Typedefs
//
//  Define all types for the NLS component here.
//
////////////////////////////////////////////////////////////////////////////

//
//  Language Group ID.
//

type
  LGRPID = DWORD;

//
//  Locale type constant.
//

  LCTYPE = DWORD;

//
//  Calendar type constant.
//

  CALTYPE = DWORD;

//
//  Calendar ID.
//

  CALID = DWORD;

//
//  CP Info.
//

  LPCPINFO = ^CPINFO;
  _cpinfo = record
    MaxCharSize: UINT; // max length (in bytes) of a char
    DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character
    LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
  end;
  CPINFO = _cpinfo;
  TCpInfo = CPINFO;
  PCpInfo = LPCPINFO;

  LPCPINFOEXA = ^CPINFOEXA;
  _cpinfoexA = record
    MaxCharSize: UINT; // max length (in bytes) of a char
    DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character (MB)
    LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
    UnicodeDefaultChar: WCHAR; // default character (Unicode)
    CodePage: UINT; // code page id
    CodePageName: array [0..MAX_PATH - 1] of CHAR; // code page name (Unicode)
  end;
  CPINFOEXA = _cpinfoexA;
  TCpInfoExA = CPINFOEXA;
  PCpInfoExA = LPCPINFOEXA;

  LPCPINFOEXW = ^CPINFOEXW;
  _cpinfoexW = record
    MaxCharSize: UINT; // max length (in bytes) of a char
    DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character (MB)
    LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
    UnicodeDefaultChar: WCHAR; // default character (Unicode)
    CodePage: UINT; // code page id
    CodePageName: array [0..MAX_PATH - 1] of WCHAR; // code page name (Unicode)
  end;
  CPINFOEXW = _cpinfoexW;
  TCpInfoExW = CPINFOEXW;
  PCpInfoExW = LPCPINFOEXW;

{$IFDEF UNICODE}
  CPINFOEX = CPINFOEXW;
  LPCPINFOEX = LPCPINFOEXW;
  TCpInfoEx = TCpInfoExW;
  PCpInfoEx = PCpInfoExW;
{$ELSE}
  CPINFOEX = CPINFOEXA;
  LPCPINFOEX = LPCPINFOEXA;
  TCpInfoEx = TCpInfoExA;
  PCpInfoEx = PCpInfoExA;
{$ENDIF}

//
//  Number format.
//

  LPNUMBERFMTA = ^NUMBERFMTA;
  _numberfmtA = record
    NumDigits: UINT; // number of decimal digits
    LeadingZero: UINT; // if leading zero in decimal fields
    Grouping: UINT; // group size left of decimal
    lpDecimalSep: LPSTR; // ptr to decimal separator string
    lpThousandSep: LPSTR; // ptr to thousand separator string
    NegativeOrder: UINT; // negative number ordering
  end;
  NUMBERFMTA = _numberfmtA;
  TNumberFmtA = NUMBERFMTA;
  PNumberFmtA = LPNUMBERFMTA;

  LPNUMBERFMTW = ^NUMBERFMTW;
  _numberfmtW = record
    NumDigits: UINT; // number of decimal digits
    LeadingZero: UINT; // if leading zero in decimal fields
    Grouping: UINT; // group size left of decimal
    lpDecimalSep: LPWSTR; // ptr to decimal separator string
    lpThousandSep: LPWSTR; // ptr to thousand separator string
    NegativeOrder: UINT; // negative number ordering
  end;
  NUMBERFMTW = _numberfmtW;
  TNumberFmtW = NUMBERFMTW;
  PNumberFmtW = LPNUMBERFMTW;

{$IFDEF UNICODE}
  NUMBERFMT = NUMBERFMTW;
  LPNUMBERFMT = LPNUMBERFMTW;
  TNumberFmt = TNumberFmtW;
  PNumberFmt = TNumberFmtW;
{$ELSE}
  NUMBERFMT = NUMBERFMTA;
  LPNUMBERFMT = LPNUMBERFMTA;
  TNumberFmt = TNumberFmtA;
  PNumberFmt = TNumberFmtA;
{$ENDIF}

//
//  Currency format.
//

  LPCURRENCYFMTA = ^CURRENCYFMTA;
  _currencyfmtA = record
    NumDigits: UINT; // number of decimal digits
    LeadingZero: UINT; // if leading zero in decimal fields
    Grouping: UINT; // group size left of decimal
    lpDecimalSep: LPSTR; // ptr to decimal separator string
    lpThousandSep: LPSTR; // ptr to thousand separator string
    NegativeOrder: UINT; // negative currency ordering
    PositiveOrder: UINT; // positive currency ordering
    lpCurrencySymbol: LPSTR; // ptr to currency symbol string
  end;
  CURRENCYFMTA = _currencyfmtA;
  TCurrencyFmtA = CURRENCYFMTA;
  PCurrencyFmtA = LPCURRENCYFMTA;

  LPCURRENCYFMTW = ^CURRENCYFMTW;
  _currencyfmtW = record
    NumDigits: UINT; // number of decimal digits
    LeadingZero: UINT; // if leading zero in decimal fields
    Grouping: UINT; // group size left of decimal
    lpDecimalSep: LPWSTR; // ptr to decimal separator string
    lpThousandSep: LPWSTR; // ptr to thousand separator string
    NegativeOrder: UINT; // negative currency ordering
    PositiveOrder: UINT; // positive currency ordering
    lpCurrencySymbol: LPWSTR; // ptr to currency symbol string
  end;
  CURRENCYFMTW = _currencyfmtW;
  TCurrencyFmtW = CURRENCYFMTW;
  PCurrencyFmtW = LPCURRENCYFMTW;

{$IFDEF UNICODE}
  CURRENCYFMT = CURRENCYFMTW;
  LPCURRENCYFMT = LPCURRENCYFMTW;
  TCurrencyFmt = TCurrencyFmtW;
  PCurrencyFmt = PCurrencyFmtW;
{$ELSE}
  CURRENCYFMT = CURRENCYFMTA;
  LPCURRENCYFMT = LPCURRENCYFMTA;
  TCurrencyFmt = TCurrencyFmtA;
  PCurrencyFmt = PCurrencyFmtA;
{$ENDIF}

//
//  NLS function capabilities
//

const
  COMPARE_STRING    =  $0001;

type
  SYSNLS_FUNCTION = DWORD;
  NLS_FUNCTION = DWORD;
  TSysNlsFunction = SYSNLS_FUNCTION;
  TNlsFunction = NLS_FUNCTION;
 
//
//  NLS version structure.
//

  _nlsversioninfo = record
    dwNLSVersionInfoSize: DWORD;
    dwNLSVersion: DWORD;
    dwDefinedVersion: DWORD;
  end;
  NLSVERSIONINFO = _nlsversioninfo;
  LPNLSVERSIONINFO = ^NLSVERSIONINFO;
  TNlsVersionInfo = NLSVERSIONINFO;
  PNlsVersionInfo = LPNLSVERSIONINFO;

//
//  GEO defines
//

type
  GEOID = LONG;
  GEOTYPE = DWORD;
  GEOCLASS = DWORD;

const
  GEOID_NOT_AVAILABLE = -1;

//
//  GEO information types for clients to query
//

const
  GEO_NATION       = $0001;
  GEO_LATITUDE     = $0002;
  GEO_LONGITUDE    = $0003;
  GEO_ISO2         = $0004;
  GEO_ISO3         = $0005;
  GEO_RFC1766      = $0006;
  GEO_LCID         = $0007;
  GEO_FRIENDLYNAME = $0008;
  GEO_OFFICIALNAME = $0009;
  GEO_TIMEZONES    = $000A;
  GEO_OFFICIALLANGUAGES = $000B;

type
  SYSGEOTYPE = DWORD;

//
//  More GEOCLASS defines will be listed here
//

const
  GEOCLASS_NATION = 16;
  GEOCLASS_REGION = 14;

type
  SYSGEOCLASS = DWORD;

//
//  Enumeration function constants.
//

type
  LANGUAGEGROUP_ENUMPROCA = function (LanguageGroup: LGRPID; lpLanguageGroupString,
    lpLanguageGroupNameSting: LPSTR; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
  LANGGROUPLOCALE_ENUMPROCA = function (LanguageGroup: LGRPID; Locale: LCID;
    lpLocaleString: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  UILANGUAGE_ENUMPROCA = function (lpUILanguageString: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  LOCALE_ENUMPROCA = function (lpLocaleString: LPSTR): BOOL; stdcall;
  CODEPAGE_ENUMPROCA = function (lpCodePAgeString: LPSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCA = function (lpDateFormatString: LPSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCEXA = function (lpDateFormatString: LPSTR; CalenderId: CALID): BOOL; stdcall;
  TIMEFMT_ENUMPROCA = function (lpTimeFormatString: LPSTR): BOOL; stdcall;
  CALINFO_ENUMPROCA = function (lpCalendarInfoString: LPSTR): BOOL; stdcall;
  CALINFO_ENUMPROCEXA = function (lpCalendarInfoString: LPSTR; Calendar: CALID): BOOL; stdcall;

  LANGUAGEGROUP_ENUMPROCW = function (LanguageGroup: LGRPID; lpLanguageGroupString,
    lpLanguageGroupNameSting: LPWSTR; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
  LANGGROUPLOCALE_ENUMPROCW = function (LanguageGroup: LGRPID; Locale: LCID;
    lpLocaleString: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  UILANGUAGE_ENUMPROCW = function (lpUILanguageString: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  LOCALE_ENUMPROCW = function (lpLocaleString: LPWSTR): BOOL; stdcall;
  CODEPAGE_ENUMPROCW = function (lpCodePAgeString: LPWSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCW = function (lpDateFormatString: LPWSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCEXW = function (lpDateFormatString: LPWSTR; CalenderId: CALID): BOOL; stdcall;
  TIMEFMT_ENUMPROCW = function (lpTimeFormatString: LPWSTR): BOOL; stdcall;
  CALINFO_ENUMPROCW = function (lpCalendarInfoString: LPWSTR): BOOL; stdcall;
  CALINFO_ENUMPROCEXW = function (lpCalendarInfoString: LPWSTR; Calendar: CALID): BOOL; stdcall;
  GEO_ENUMPROC = function (geo: GEOID): BOOL; stdcall;

{$IFDEF UNICODE}

  LANGUAGEGROUP_ENUMPROC = function (LanguageGroup: LGRPID; lpLanguageGroupString,
    lpLanguageGroupNameSting: LPWSTR; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
  LANGGROUPLOCALE_ENUMPROC = function (LanguageGroup: LGRPID; Locale: LCID;
    lpLocaleString: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  UILANGUAGE_ENUMPROC = function (lpUILanguageString: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  LOCALE_ENUMPROC = function (lpLocaleString: LPWSTR): BOOL; stdcall;
  CODEPAGE_ENUMPROC = function (lpCodePAgeString: LPWSTR): BOOL; stdcall;
  DATEFMT_ENUMPROC = function (lpDateFormatString: LPWSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCEX = function (lpDateFormatString: LPWSTR; CalenderId: CALID): BOOL; stdcall;
  TIMEFMT_ENUMPROC = function (lpTimeFormatString: LPWSTR): BOOL; stdcall;
  CALINFO_ENUMPROC = function (lpCalendarInfoString: LPWSTR): BOOL; stdcall;
  CALINFO_ENUMPROCEX = function (lpCalendarInfoString: LPWSTR; Calendar: CALID): BOOL; stdcall;

{$ELSE}

  LANGUAGEGROUP_ENUMPROC = function (LanguageGroup: LGRPID; lpLanguageGroupString,
    lpLanguageGroupNameSting: LPSTR; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
  LANGGROUPLOCALE_ENUMPROC = function (LanguageGroup: LGRPID; Locale: LCID;
    lpLocaleString: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  UILANGUAGE_ENUMPROC = function (lpUILanguageString: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  LOCALE_ENUMPROC = function (lpLocaleString: LPSTR): BOOL; stdcall;
  CODEPAGE_ENUMPROC = function (lpCodePAgeString: LPSTR): BOOL; stdcall;
  DATEFMT_ENUMPROC = function (lpDateFormatString: LPSTR): BOOL; stdcall;
  DATEFMT_ENUMPROCEX = function (lpDateFormatString: LPSTR; CalenderId: CALID): BOOL; stdcall;
  TIMEFMT_ENUMPROC = function (lpTimeFormatString: LPSTR): BOOL; stdcall;
  CALINFO_ENUMPROC = function (lpCalendarInfoString: LPSTR): BOOL; stdcall;
  CALINFO_ENUMPROCEX = function (lpCalendarInfoString: LPSTR; Calendar: CALID): BOOL; stdcall;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////
//
//  Macros
//
//  Define all macros for the NLS component here.
//
////////////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////////
//
//  Function Prototypes
//
//  Only prototypes for the NLS APIs should go here.
//
////////////////////////////////////////////////////////////////////////////

//
//  Code Page Dependent APIs.
//

function IsValidCodePage(CodePage: UINT): BOOL; stdcall;

function GetACP: UINT; stdcall;

function GetOEMCP: UINT; stdcall;

function GetCPInfo(CodePage: UINT; var lpCPInfo: CPINFO): BOOL; stdcall;

function GetCPInfoExA(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: CPINFOEXA): BOOL; stdcall;
function GetCPInfoExW(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: CPINFOEXW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: CPINFOEXW): BOOL; stdcall;
{$ELSE}
function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: CPINFOEXA): BOOL; stdcall;
{$ENDIF}

function IsDBCSLeadByte(TestChar: BYTE): BOOL; stdcall;

function IsDBCSLeadByteEx(CodePage: UINT; TestChar: BYTE): BOOL; stdcall;

function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD; lpMultiByteStr: LPCSTR;
  cbMultiByte: Integer; lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer; stdcall;

function WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD; lpWideCharStr: LPCWSTR;
  cchWideChar: Integer; lpMultiByteStr: LPSTR; cbMultiByte: Integer;
  lpDefaultChar: LPCSTR; lpUsedDefaultChar: LPBOOL): Integer; stdcall;

//
//  Locale Dependent APIs.
//

function CompareStringA(Locale: LCID; dwCmpFlags: DWORD; lpString1: LPCSTR;
  cchCount1: Integer; lpString2: LPCSTR; cchCount2: Integer): Integer; stdcall;
function CompareStringW(Locale: LCID; dwCmpFlags: DWORD; lpString1: LPCWSTR;
  cchCount1: Integer; lpString2: LPCWSTR; cchCount2: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function CompareString(Locale: LCID; dwCmpFlags: DWORD; lpString1: LPCWSTR;
  cchCount1: Integer; lpString2: LPCWSTR; cchCount2: Integer): Integer; stdcall;
{$ELSE}
function CompareString(Locale: LCID; dwCmpFlags: DWORD; lpString1: LPCSTR;
  cchCount1: Integer; lpString2: LPCSTR; cchCount2: Integer): Integer; stdcall;
{$ENDIF}

function LCMapStringA(Locale: LCID; dwMapFlags: DWORD; lpSrcStr: LPCSTR;
  cchSrc: Integer; lpDestStr: LPSTR; cchDest: Integer): Integer; stdcall;
function LCMapStringW(Locale: LCID; dwMapFlags: DWORD; lpSrcStr: LPCWSTR;
  cchSrc: Integer; lpDestStr: LPWSTR; cchDest: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function LCMapString(Locale: LCID; dwMapFlags: DWORD; lpSrcStr: LPCWSTR;
  cchSrc: Integer; lpDestStr: LPWSTR; cchDest: Integer): Integer; stdcall;
{$ELSE}
function LCMapString(Locale: LCID; dwMapFlags: DWORD; lpSrcStr: LPCSTR;
  cchSrc: Integer; lpDestStr: LPSTR; cchDest: Integer): Integer; stdcall;
{$ENDIF}

function GetLocaleInfoA(Locale: LCID; LCType: LCTYPE; lpLCData: LPSTR;
  cchData: Integer): Integer; stdcall;
function GetLocaleInfoW(Locale: LCID; LCType: LCTYPE; lpLCData: LPWSTR;
  cchData: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: LPWSTR;
  cchData: Integer): Integer; stdcall;
{$ELSE}
function GetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: LPSTR;
  cchData: Integer): Integer; stdcall;
{$ENDIF}

function SetLocaleInfoA(Locale: LCID; LCType: LCTYPE; lpLCData: LPCSTR): BOOL; stdcall;
function SetLocaleInfoW(Locale: LCID; LCType: LCTYPE; lpLCData: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetCalendarInfoA(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;
function GetCalendarInfoW(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPWSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;

{$IFDEF UNICODE}
function GetCalendarInfo(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPWSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;
{$ELSE}
function GetCalendarInfo(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPSTR; cchData: Integer; lpValue: LPDWORD): Integer; stdcall;
{$ENDIF}

function SetCalendarInfoA(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPCSTR): BOOL; stdcall;
function SetCalendarInfoW(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetCalendarInfo(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetCalendarInfo(Locale: LCID; Calendar: CALID; CalType: CALTYPE;
  lpCalData: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetTimeFormatA(Locale: LCID; dwFlags: DWORD; lpTime: LPSYSTEMTIME;
  lpFormat: LPCSTR; lpTimeStr: LPSTR; cchTime: Integer): Integer; stdcall;
function GetTimeFormatW(Locale: LCID; dwFlags: DWORD; lpTime: LPSYSTEMTIME;
  lpFormat: LPCWSTR; lpTimeStr: LPWSTR; cchTime: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetTimeFormat(Locale: LCID; dwFlags: DWORD; lpTime: LPSYSTEMTIME;
  lpFormat: LPCWSTR; lpTimeStr: LPWSTR; cchTime: Integer): Integer; stdcall;
{$ELSE}
function GetTimeFormat(Locale: LCID; dwFlags: DWORD; lpTime: LPSYSTEMTIME;
  lpFormat: LPCSTR; lpTimeStr: LPSTR; cchTime: Integer): Integer; stdcall;
{$ENDIF}

function GetDateFormatA(Locale: LCID; dwFlags: DWORD; lpDate: LPSYSTEMTIME;
  lpFormat: LPCSTR; lpDateStr: LPSTR; cchDate: Integer): Integer; stdcall;
function GetDateFormatW(Locale: LCID; dwFlags: DWORD; lpDate: LPSYSTEMTIME;
  lpFormat: LPCWSTR; lpDateStr: LPWSTR; cchDate: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetDateFormat(Locale: LCID; dwFlags: DWORD; lpDate: LPSYSTEMTIME;
  lpFormat: LPCWSTR; lpDateStr: LPWSTR; cchDate: Integer): Integer; stdcall;
{$ELSE}
function GetDateFormat(Locale: LCID; dwFlags: DWORD; lpDate: LPSYSTEMTIME;
  lpFormat: LPCSTR; lpDateStr: LPSTR; cchDate: Integer): Integer; stdcall;
{$ENDIF}

function GetNumberFormatA(Locale: LCID; dwFlags: DWORD; lpValue: LPCSTR;
  lpFormat: LPNUMBERFMTA; lpNumberStr: LPSTR; cchNumber: Integer): Integer; stdcall;
function GetNumberFormatW(Locale: LCID; dwFlags: DWORD; lpValue: LPCWSTR;
  lpFormat: LPNUMBERFMTW; lpNumberStr: LPWSTR; cchNumber: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetNumberFormat(Locale: LCID; dwFlags: DWORD; lpValue: LPCWSTR;
  lpFormat: LPNUMBERFMTW; lpNumberStr: LPWSTR; cchNumber: Integer): Integer; stdcall;
{$ELSE}
function GetNumberFormat(Locale: LCID; dwFlags: DWORD; lpValue: LPCSTR;
  lpFormat: LPNUMBERFMTA; lpNumberStr: LPSTR; cchNumber: Integer): Integer; stdcall;
{$ENDIF}

function GetCurrencyFormatA(Locale: LCID; dwFlags: DWORD; lpValue: LPCSTR;
  lpFormat: LPCURRENCYFMTA; lpCurrencyStr: LPSTR; cchCurrency: Integer): Integer; stdcall;
function GetCurrencyFormatW(Locale: LCID; dwFlags: DWORD; lpValue: LPCWSTR;
  lpFormat: LPCURRENCYFMTW; lpCurrencyStr: LPWSTR; cchCurrency: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetCurrencyFormat(Locale: LCID; dwFlags: DWORD; lpValue: LPCWSTR;
  lpFormat: LPCURRENCYFMTW; lpCurrencyStr: LPWSTR; cchCurrency: Integer): Integer; stdcall;
{$ELSE}
function GetCurrencyFormat(Locale: LCID; dwFlags: DWORD; lpValue: LPCSTR;
  lpFormat: LPCURRENCYFMTA; lpCurrencyStr: LPSTR; cchCurrency: Integer): Integer; stdcall;
{$ENDIF}

function EnumCalendarInfoA(lpCalInfoEnumProc: CALINFO_ENUMPROCA; Locale: LCID;
  Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
function EnumCalendarInfoW(lpCalInfoEnumProc: CALINFO_ENUMPROCW; Locale: LCID;
  Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumCalendarInfo(lpCalInfoEnumProc: CALINFO_ENUMPROCW; Locale: LCID;
  Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
{$ELSE}
function EnumCalendarInfo(lpCalInfoEnumProc: CALINFO_ENUMPROCA; Locale: LCID;
  Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
{$ENDIF}

function EnumCalendarInfoExA(lpCalInfoEnumProcEx: CALINFO_ENUMPROCEXA;
  Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
function EnumCalendarInfoExW(lpCalInfoEnumProcEx: CALINFO_ENUMPROCEXW;
  Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumCalendarInfoEx(lpCalInfoEnumProcEx: CALINFO_ENUMPROCEXW;
  Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
{$ELSE}
function EnumCalendarInfoEx(lpCalInfoEnumProcEx: CALINFO_ENUMPROCEXA;
  Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall;
{$ENDIF}

function EnumTimeFormatsA(lpTimeFmtEnumProc: TIMEFMT_ENUMPROCA; Locale:
  LCID; dwFlags: DWORD): BOOL; stdcall;
function EnumTimeFormatsW(lpTimeFmtEnumProc: TIMEFMT_ENUMPROCW; Locale:
  LCID; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumTimeFormats(lpTimeFmtEnumProc: TIMEFMT_ENUMPROCW; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumTimeFormats(lpTimeFmtEnumProc: TIMEFMT_ENUMPROCA; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function EnumDateFormatsA(lpDateFmtEnumProc: DATEFMT_ENUMPROCA; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;
function EnumDateFormatsW(lpDateFmtEnumProc: DATEFMT_ENUMPROCW; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDateFormats(lpDateFmtEnumProc: DATEFMT_ENUMPROCW; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumDateFormats(lpDateFmtEnumProc: DATEFMT_ENUMPROCA; Locale: LCID;
  dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function EnumDateFormatsExA(lpDateFmtEnumProcEx: DATEFMT_ENUMPROCEXA;
  Locale: LCID; dwFlags: DWORD): BOOL; stdcall;
function EnumDateFormatsExW(lpDateFmtEnumProcEx: DATEFMT_ENUMPROCEXW;
  Locale: LCID; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDateFormatsEx(lpDateFmtEnumProcEx: DATEFMT_ENUMPROCEXW;
  Locale: LCID; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumDateFormatsEx(lpDateFmtEnumProcEx: DATEFMT_ENUMPROCEXA;
  Locale: LCID; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function IsValidLanguageGroup(LanguageGroup: LGRPID; dwFlags: DWORD): BOOL; stdcall;

function GetNLSVersion(Function_: NLS_FUNCTION; Locale: LCID; lpVersionInformation: LPNLSVERSIONINFO): BOOL; stdcall;

function IsNLSDefinedString(Function_: NLS_FUNCTION; dwFlags: DWORD; lpVersionInformation: LPNLSVERSIONINFO;
  lpString: LPCWSTR; cchStr: Integer): BOOL; stdcall;

function IsValidLocale(Locale: LCID; dwFlags: DWORD): BOOL; stdcall;

function GetGeoInfoA(Location: GEOID; GeoType: GEOTYPE; lpGeoData: LPSTR;
  cchData: Integer; LangId: LANGID): Integer; stdcall;
function GetGeoInfoW(Location: GEOID; GeoType: GEOTYPE; lpGeoData: LPWSTR;
  cchData: Integer; LangId: LANGID): Integer; stdcall;

{$IFDEF UNICODE}
function GetGeoInfo(Location: GEOID; GeoType: GEOTYPE; lpGeoData: LPWSTR;
  cchData: Integer; LangId: LANGID): Integer; stdcall;
{$ELSE}
function GetGeoInfo(Location: GEOID; GeoType: GEOTYPE; lpGeoData: LPSTR;
  cchData: Integer; LangId: LANGID): Integer; stdcall;
{$ENDIF}

function EnumSystemGeoID(GeoClass: GEOCLASS; ParentGeoId: GEOID; lpGeoEnumProc: GEO_ENUMPROC): BOOL; stdcall;

function GetUserGeoID(GeoClass: GEOCLASS): GEOID; stdcall;

function SetUserGeoID(GeoId: GEOID): BOOL; stdcall;

function ConvertDefaultLocale(Locale: LCID): LCID; stdcall;

function GetThreadLocale: LCID; stdcall;

function SetThreadLocale(Locale: LCID): BOOL; stdcall;

function GetSystemDefaultUILanguage: LANGID; stdcall;

function GetUserDefaultUILanguage: LANGID; stdcall;

function GetSystemDefaultLangID: LANGID; stdcall;

function GetUserDefaultLangID: LANGID; stdcall;

function GetSystemDefaultLCID: LCID; stdcall;

function GetUserDefaultLCID: LCID; stdcall;

//
//  Locale Independent APIs.
//

function GetStringTypeExA(Locale: LCID; dwInfoType: DWORD; lpSrcStr: LPCSTR;
  cchSrc: Integer; lpCharType: LPWORD): BOOL; stdcall;
function GetStringTypeExW(Locale: LCID; dwInfoType: DWORD; lpSrcStr: LPCWSTR;
  cchSrc: Integer; lpCharType: LPWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetStringTypeEx(Locale: LCID; dwInfoType: DWORD; lpSrcStr: LPCWSTR;
  cchSrc: Integer; lpCharType: LPWORD): BOOL; stdcall;
{$ELSE}
function GetStringTypeEx(Locale: LCID; dwInfoType: DWORD; lpSrcStr: LPCSTR;
  cchSrc: Integer; lpCharType: LPWORD): BOOL; stdcall;
{$ENDIF}

//
//  NOTE: The parameters for GetStringTypeA and GetStringTypeW are
//        NOT the same.  The W version was shipped in NT 3.1.  The
//        A version was then shipped in 16-bit OLE with the wrong
//        parameters (ported from Win95).  To be compatible, we
//        must break the relationship between the A and W versions
//        of GetStringType.  There will be NO function call for the
//        generic GetStringType.
//
//        GetStringTypeEx (above) should be used instead.
//

function GetStringTypeA(Locale: LCID; dwInfoType: DWORD; lpSrcStr: LPCSTR;
  cchSrc: Integer; lpCharType: LPWORD): BOOL; stdcall;
function GetStringTypeW(dwInfoType: DWORD; lpSrcStr: LPCWSTR; cchSrc: Integer;
  lpCharType: LPWORD): BOOL; stdcall;

function FoldStringA(dwMapFlags: DWORD; lpSrcStr: LPCSTR; cchSrc: Integer;
  lpDestStr: LPSTR; cchDest: Integer): Integer; stdcall;
function FoldStringW(dwMapFlags: DWORD; lpSrcStr: LPCWSTR; cchSrc: Integer;
  lpDestStr: LPWSTR; cchDest: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function FoldString(dwMapFlags: DWORD; lpSrcStr: LPCWSTR; cchSrc: Integer;
  lpDestStr: LPWSTR; cchDest: Integer): Integer; stdcall;
{$ELSE}
function FoldString(dwMapFlags: DWORD; lpSrcStr: LPCSTR; cchSrc: Integer;
  lpDestStr: LPSTR; cchDest: Integer): Integer; stdcall;
{$ENDIF}

function EnumSystemLanguageGroupsA(lpLanguageGroupEnumProc: LANGUAGEGROUP_ENUMPROCA;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
function EnumSystemLanguageGroupsW(lpLanguageGroupEnumProc: LANGUAGEGROUP_ENUMPROCW;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumSystemLanguageGroups(lpLanguageGroupEnumProc: LANGUAGEGROUP_ENUMPROCW;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumSystemLanguageGroups(lpLanguageGroupEnumProc: LANGUAGEGROUP_ENUMPROCA;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function EnumLanguageGroupLocalesA(lpLangGroupLocaleEnumProc: LANGGROUPLOCALE_ENUMPROCA;
  LanguageGroup: LGRPID; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
function EnumLanguageGroupLocalesW(lpLangGroupLocaleEnumProc: LANGGROUPLOCALE_ENUMPROCW;
  LanguageGroup: LGRPID; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumLanguageGroupLocales(lpLangGroupLocaleEnumProc: LANGGROUPLOCALE_ENUMPROCW;
  LanguageGroup: LGRPID; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumLanguageGroupLocales(lpLangGroupLocaleEnumProc: LANGGROUPLOCALE_ENUMPROCA;
  LanguageGroup: LGRPID; dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function EnumUILanguagesA(lpUILanguageEnumProc: UILANGUAGE_ENUMPROCA;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
function EnumUILanguagesW(lpUILanguageEnumProc: UILANGUAGE_ENUMPROCW;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumUILanguages(lpUILanguageEnumProc: UILANGUAGE_ENUMPROCW;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumUILanguages(lpUILanguageEnumProc: UILANGUAGE_ENUMPROCA;
  dwFlags: DWORD; lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function EnumSystemLocalesA(lpLocaleEnumProc: LOCALE_ENUMPROCA;
  dwFlags: DWORD): BOOL; stdcall;
function EnumSystemLocalesW(lpLocaleEnumProc: LOCALE_ENUMPROCW;
  dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumSystemLocales(lpLocaleEnumProc: LOCALE_ENUMPROCW;
  dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumSystemLocales(lpLocaleEnumProc: LOCALE_ENUMPROCA;
  dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function EnumSystemCodePagesA(lpCodePageEnumProc: CODEPAGE_ENUMPROCA;
  dwFlags: DWORD): BOOL; stdcall;
function EnumSystemCodePagesW(lpCodePageEnumProc: CODEPAGE_ENUMPROCW;
  dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumSystemCodePages(lpCodePageEnumProc: CODEPAGE_ENUMPROCW;
  dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumSystemCodePages(lpCodePageEnumProc: CODEPAGE_ENUMPROCA;
  dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

implementation

const
 kernel32 = 'kernel32.dll';


{$IFDEF DYNAMIC_LINK}
var
  _IsValidCodePage: Pointer;

function IsValidCodePage;
begin
  GetProcedureAddress(_IsValidCodePage, kernel32, 'IsValidCodePage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidCodePage]
  end;
end;
{$ELSE}
function IsValidCodePage; external kernel32 name 'IsValidCodePage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetACP: Pointer;

function GetACP;
begin
  GetProcedureAddress(_GetACP, kernel32, 'GetACP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetACP]
  end;
end;
{$ELSE}
function GetACP; external kernel32 name 'GetACP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOEMCP: Pointer;

function GetOEMCP;
begin
  GetProcedureAddress(_GetOEMCP, kernel32, 'GetOEMCP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOEMCP]
  end;
end;
{$ELSE}
function GetOEMCP; external kernel32 name 'GetOEMCP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCPInfo: Pointer;

function GetCPInfo;
begin
  GetProcedureAddress(_GetCPInfo, kernel32, 'GetCPInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCPInfo]
  end;
end;
{$ELSE}
function GetCPInfo; external kernel32 name 'GetCPInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCPInfoExA: Pointer;

function GetCPInfoExA;
begin
  GetProcedureAddress(_GetCPInfoExA, kernel32, 'GetCPInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCPInfoExA]
  end;
end;
{$ELSE}
function GetCPInfoExA; external kernel32 name 'GetCPInfoExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCPInfoExW: Pointer;

function GetCPInfoExW;
begin
  GetProcedureAddress(_GetCPInfoExW, kernel32, 'GetCPInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCPInfoExW]
  end;
end;
{$ELSE}
function GetCPInfoExW; external kernel32 name 'GetCPInfoExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCPInfoEx: Pointer;

function GetCPInfoEx;
begin
  GetProcedureAddress(_GetCPInfoEx, kernel32, 'GetCPInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCPInfoEx]
  end;
end;
{$ELSE}
function GetCPInfoEx; external kernel32 name 'GetCPInfoExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCPInfoEx: Pointer;

function GetCPInfoEx;
begin
  GetProcedureAddress(_GetCPInfoEx, kernel32, 'GetCPInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCPInfoEx]
  end;
end;
{$ELSE}
function GetCPInfoEx; external kernel32 name 'GetCPInfoExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsDBCSLeadByte: Pointer;

function IsDBCSLeadByte;
begin
  GetProcedureAddress(_IsDBCSLeadByte, kernel32, 'IsDBCSLeadByte');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDBCSLeadByte]
  end;
end;
{$ELSE}
function IsDBCSLeadByte; external kernel32 name 'IsDBCSLeadByte';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsDBCSLeadByteEx: Pointer;

function IsDBCSLeadByteEx;
begin
  GetProcedureAddress(_IsDBCSLeadByteEx, kernel32, 'IsDBCSLeadByteEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDBCSLeadByteEx]
  end;
end;
{$ELSE}
function IsDBCSLeadByteEx; external kernel32 name 'IsDBCSLeadByteEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MultiByteToWideChar: Pointer;

function MultiByteToWideChar;
begin
  GetProcedureAddress(_MultiByteToWideChar, kernel32, 'MultiByteToWideChar');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MultiByteToWideChar]
  end;
end;
{$ELSE}
function MultiByteToWideChar; external kernel32 name 'MultiByteToWideChar';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WideCharToMultiByte: Pointer;

function WideCharToMultiByte;
begin
  GetProcedureAddress(_WideCharToMultiByte, kernel32, 'WideCharToMultiByte');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WideCharToMultiByte]
  end;
end;
{$ELSE}
function WideCharToMultiByte; external kernel32 name 'WideCharToMultiByte';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CompareStringA: Pointer;

function CompareStringA;
begin
  GetProcedureAddress(_CompareStringA, kernel32, 'CompareStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CompareStringA]
  end;
end;
{$ELSE}
function CompareStringA; external kernel32 name 'CompareStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CompareStringW: Pointer;

function CompareStringW;
begin
  GetProcedureAddress(_CompareStringW, kernel32, 'CompareStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CompareStringW]
  end;
end;
{$ELSE}
function CompareStringW; external kernel32 name 'CompareStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CompareString: Pointer;

function CompareString;
begin
  GetProcedureAddress(_CompareString, kernel32, 'CompareStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CompareString]
  end;
end;
{$ELSE}
function CompareString; external kernel32 name 'CompareStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CompareString: Pointer;

function CompareString;
begin
  GetProcedureAddress(_CompareString, kernel32, 'CompareStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CompareString]
  end;
end;
{$ELSE}
function CompareString; external kernel32 name 'CompareStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LCMapStringA: Pointer;

function LCMapStringA;
begin
  GetProcedureAddress(_LCMapStringA, kernel32, 'LCMapStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LCMapStringA]
  end;
end;
{$ELSE}
function LCMapStringA; external kernel32 name 'LCMapStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LCMapStringW: Pointer;

function LCMapStringW;
begin
  GetProcedureAddress(_LCMapStringW, kernel32, 'LCMapStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LCMapStringW]
  end;
end;
{$ELSE}
function LCMapStringW; external kernel32 name 'LCMapStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LCMapString: Pointer;

function LCMapString;
begin
  GetProcedureAddress(_LCMapString, kernel32, 'LCMapStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LCMapString]
  end;
end;
{$ELSE}
function LCMapString; external kernel32 name 'LCMapStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LCMapString: Pointer;

function LCMapString;
begin
  GetProcedureAddress(_LCMapString, kernel32, 'LCMapStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LCMapString]
  end;
end;
{$ELSE}
function LCMapString; external kernel32 name 'LCMapStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetLocaleInfoA: Pointer;

function GetLocaleInfoA;
begin
  GetProcedureAddress(_GetLocaleInfoA, kernel32, 'GetLocaleInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLocaleInfoA]
  end;
end;
{$ELSE}
function GetLocaleInfoA; external kernel32 name 'GetLocaleInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLocaleInfoW: Pointer;

function GetLocaleInfoW;
begin
  GetProcedureAddress(_GetLocaleInfoW, kernel32, 'GetLocaleInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLocaleInfoW]
  end;
end;
{$ELSE}
function GetLocaleInfoW; external kernel32 name 'GetLocaleInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLocaleInfo: Pointer;

function GetLocaleInfo;
begin
  GetProcedureAddress(_GetLocaleInfo, kernel32, 'GetLocaleInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLocaleInfo]
  end;
end;
{$ELSE}
function GetLocaleInfo; external kernel32 name 'GetLocaleInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLocaleInfo: Pointer;

function GetLocaleInfo;
begin
  GetProcedureAddress(_GetLocaleInfo, kernel32, 'GetLocaleInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLocaleInfo]
  end;
end;
{$ELSE}
function GetLocaleInfo; external kernel32 name 'GetLocaleInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetLocaleInfoA: Pointer;

function SetLocaleInfoA;
begin
  GetProcedureAddress(_SetLocaleInfoA, kernel32, 'SetLocaleInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLocaleInfoA]
  end;
end;
{$ELSE}
function SetLocaleInfoA; external kernel32 name 'SetLocaleInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLocaleInfoW: Pointer;

function SetLocaleInfoW;
begin
  GetProcedureAddress(_SetLocaleInfoW, kernel32, 'SetLocaleInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLocaleInfoW]
  end;
end;
{$ELSE}
function SetLocaleInfoW; external kernel32 name 'SetLocaleInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetLocaleInfo: Pointer;

function SetLocaleInfo;
begin
  GetProcedureAddress(_SetLocaleInfo, kernel32, 'SetLocaleInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLocaleInfo]
  end;
end;
{$ELSE}
function SetLocaleInfo; external kernel32 name 'SetLocaleInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetLocaleInfo: Pointer;

function SetLocaleInfo;
begin
  GetProcedureAddress(_SetLocaleInfo, kernel32, 'SetLocaleInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLocaleInfo]
  end;
end;
{$ELSE}
function SetLocaleInfo; external kernel32 name 'SetLocaleInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCalendarInfoA: Pointer;

function GetCalendarInfoA;
begin
  GetProcedureAddress(_GetCalendarInfoA, kernel32, 'GetCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfoA]
  end;
end;
{$ELSE}
function GetCalendarInfoA; external kernel32 name 'GetCalendarInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCalendarInfoW: Pointer;

function GetCalendarInfoW;
begin
  GetProcedureAddress(_GetCalendarInfoW, kernel32, 'GetCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfoW]
  end;
end;
{$ELSE}
function GetCalendarInfoW; external kernel32 name 'GetCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCalendarInfo: Pointer;

function GetCalendarInfo;
begin
  GetProcedureAddress(_GetCalendarInfo, kernel32, 'GetCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfo]
  end;
end;
{$ELSE}
function GetCalendarInfo; external kernel32 name 'GetCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCalendarInfo: Pointer;

function GetCalendarInfo;
begin
  GetProcedureAddress(_GetCalendarInfo, kernel32, 'GetCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCalendarInfo]
  end;
end;
{$ELSE}
function GetCalendarInfo; external kernel32 name 'GetCalendarInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetCalendarInfoA: Pointer;

function SetCalendarInfoA;
begin
  GetProcedureAddress(_SetCalendarInfoA, kernel32, 'SetCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCalendarInfoA]
  end;
end;
{$ELSE}
function SetCalendarInfoA; external kernel32 name 'SetCalendarInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCalendarInfoW: Pointer;

function SetCalendarInfoW;
begin
  GetProcedureAddress(_SetCalendarInfoW, kernel32, 'SetCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCalendarInfoW]
  end;
end;
{$ELSE}
function SetCalendarInfoW; external kernel32 name 'SetCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetCalendarInfo: Pointer;

function SetCalendarInfo;
begin
  GetProcedureAddress(_SetCalendarInfo, kernel32, 'SetCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCalendarInfo]
  end;
end;
{$ELSE}
function SetCalendarInfo; external kernel32 name 'SetCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetCalendarInfo: Pointer;

function SetCalendarInfo;
begin
  GetProcedureAddress(_SetCalendarInfo, kernel32, 'SetCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCalendarInfo]
  end;
end;
{$ELSE}
function SetCalendarInfo; external kernel32 name 'SetCalendarInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTimeFormatA: Pointer;

function GetTimeFormatA;
begin
  GetProcedureAddress(_GetTimeFormatA, kernel32, 'GetTimeFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTimeFormatA]
  end;
end;
{$ELSE}
function GetTimeFormatA; external kernel32 name 'GetTimeFormatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTimeFormatW: Pointer;

function GetTimeFormatW;
begin
  GetProcedureAddress(_GetTimeFormatW, kernel32, 'GetTimeFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTimeFormatW]
  end;
end;
{$ELSE}
function GetTimeFormatW; external kernel32 name 'GetTimeFormatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTimeFormat: Pointer;

function GetTimeFormat;
begin
  GetProcedureAddress(_GetTimeFormat, kernel32, 'GetTimeFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTimeFormat]
  end;
end;
{$ELSE}
function GetTimeFormat; external kernel32 name 'GetTimeFormatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTimeFormat: Pointer;

function GetTimeFormat;
begin
  GetProcedureAddress(_GetTimeFormat, kernel32, 'GetTimeFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTimeFormat]
  end;
end;
{$ELSE}
function GetTimeFormat; external kernel32 name 'GetTimeFormatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDateFormatA: Pointer;

function GetDateFormatA;
begin
  GetProcedureAddress(_GetDateFormatA, kernel32, 'GetDateFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDateFormatA]
  end;
end;
{$ELSE}
function GetDateFormatA; external kernel32 name 'GetDateFormatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDateFormatW: Pointer;

function GetDateFormatW;
begin
  GetProcedureAddress(_GetDateFormatW, kernel32, 'GetDateFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDateFormatW]
  end;
end;
{$ELSE}
function GetDateFormatW; external kernel32 name 'GetDateFormatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDateFormat: Pointer;

function GetDateFormat;
begin
  GetProcedureAddress(_GetDateFormat, kernel32, 'GetDateFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDateFormat]
  end;
end;
{$ELSE}
function GetDateFormat; external kernel32 name 'GetDateFormatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDateFormat: Pointer;

function GetDateFormat;
begin
  GetProcedureAddress(_GetDateFormat, kernel32, 'GetDateFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDateFormat]
  end;
end;
{$ELSE}
function GetDateFormat; external kernel32 name 'GetDateFormatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberFormatA: Pointer;

function GetNumberFormatA;
begin
  GetProcedureAddress(_GetNumberFormatA, kernel32, 'GetNumberFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberFormatA]
  end;
end;
{$ELSE}
function GetNumberFormatA; external kernel32 name 'GetNumberFormatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberFormatW: Pointer;

function GetNumberFormatW;
begin
  GetProcedureAddress(_GetNumberFormatW, kernel32, 'GetNumberFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberFormatW]
  end;
end;
{$ELSE}
function GetNumberFormatW; external kernel32 name 'GetNumberFormatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberFormat: Pointer;

function GetNumberFormat;
begin
  GetProcedureAddress(_GetNumberFormat, kernel32, 'GetNumberFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberFormat]
  end;
end;
{$ELSE}
function GetNumberFormat; external kernel32 name 'GetNumberFormatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberFormat: Pointer;

function GetNumberFormat;
begin
  GetProcedureAddress(_GetNumberFormat, kernel32, 'GetNumberFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberFormat]
  end;
end;
{$ELSE}
function GetNumberFormat; external kernel32 name 'GetNumberFormatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrencyFormatA: Pointer;

function GetCurrencyFormatA;
begin
  GetProcedureAddress(_GetCurrencyFormatA, kernel32, 'GetCurrencyFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrencyFormatA]
  end;
end;
{$ELSE}
function GetCurrencyFormatA; external kernel32 name 'GetCurrencyFormatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrencyFormatW: Pointer;

function GetCurrencyFormatW;
begin
  GetProcedureAddress(_GetCurrencyFormatW, kernel32, 'GetCurrencyFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrencyFormatW]
  end;
end;
{$ELSE}
function GetCurrencyFormatW; external kernel32 name 'GetCurrencyFormatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrencyFormat: Pointer;

function GetCurrencyFormat;
begin
  GetProcedureAddress(_GetCurrencyFormat, kernel32, 'GetCurrencyFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrencyFormat]
  end;
end;
{$ELSE}
function GetCurrencyFormat; external kernel32 name 'GetCurrencyFormatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrencyFormat: Pointer;

function GetCurrencyFormat;
begin
  GetProcedureAddress(_GetCurrencyFormat, kernel32, 'GetCurrencyFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrencyFormat]
  end;
end;
{$ELSE}
function GetCurrencyFormat; external kernel32 name 'GetCurrencyFormatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoA: Pointer;

function EnumCalendarInfoA;
begin
  GetProcedureAddress(_EnumCalendarInfoA, kernel32, 'EnumCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoA]
  end;
end;
{$ELSE}
function EnumCalendarInfoA; external kernel32 name 'EnumCalendarInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoW: Pointer;

function EnumCalendarInfoW;
begin
  GetProcedureAddress(_EnumCalendarInfoW, kernel32, 'EnumCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoW]
  end;
end;
{$ELSE}
function EnumCalendarInfoW; external kernel32 name 'EnumCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfo: Pointer;

function EnumCalendarInfo;
begin
  GetProcedureAddress(_EnumCalendarInfo, kernel32, 'EnumCalendarInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfo]
  end;
end;
{$ELSE}
function EnumCalendarInfo; external kernel32 name 'EnumCalendarInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfo: Pointer;

function EnumCalendarInfo;
begin
  GetProcedureAddress(_EnumCalendarInfo, kernel32, 'EnumCalendarInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfo]
  end;
end;
{$ELSE}
function EnumCalendarInfo; external kernel32 name 'EnumCalendarInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoExA: Pointer;

function EnumCalendarInfoExA;
begin
  GetProcedureAddress(_EnumCalendarInfoExA, kernel32, 'EnumCalendarInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoExA]
  end;
end;
{$ELSE}
function EnumCalendarInfoExA; external kernel32 name 'EnumCalendarInfoExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoExW: Pointer;

function EnumCalendarInfoExW;
begin
  GetProcedureAddress(_EnumCalendarInfoExW, kernel32, 'EnumCalendarInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoExW]
  end;
end;
{$ELSE}
function EnumCalendarInfoExW; external kernel32 name 'EnumCalendarInfoExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoEx: Pointer;

function EnumCalendarInfoEx;
begin
  GetProcedureAddress(_EnumCalendarInfoEx, kernel32, 'EnumCalendarInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoEx]
  end;
end;
{$ELSE}
function EnumCalendarInfoEx; external kernel32 name 'EnumCalendarInfoExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumCalendarInfoEx: Pointer;

function EnumCalendarInfoEx;
begin
  GetProcedureAddress(_EnumCalendarInfoEx, kernel32, 'EnumCalendarInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumCalendarInfoEx]
  end;
end;
{$ELSE}
function EnumCalendarInfoEx; external kernel32 name 'EnumCalendarInfoExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumTimeFormatsA: Pointer;

function EnumTimeFormatsA;
begin
  GetProcedureAddress(_EnumTimeFormatsA, kernel32, 'EnumTimeFormatsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumTimeFormatsA]
  end;
end;
{$ELSE}
function EnumTimeFormatsA; external kernel32 name 'EnumTimeFormatsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumTimeFormatsW: Pointer;

function EnumTimeFormatsW;
begin
  GetProcedureAddress(_EnumTimeFormatsW, kernel32, 'EnumTimeFormatsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumTimeFormatsW]
  end;
end;
{$ELSE}
function EnumTimeFormatsW; external kernel32 name 'EnumTimeFormatsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumTimeFormats: Pointer;

function EnumTimeFormats;
begin
  GetProcedureAddress(_EnumTimeFormats, kernel32, 'EnumTimeFormatsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumTimeFormats]
  end;
end;
{$ELSE}
function EnumTimeFormats; external kernel32 name 'EnumTimeFormatsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumTimeFormats: Pointer;

function EnumTimeFormats;
begin
  GetProcedureAddress(_EnumTimeFormats, kernel32, 'EnumTimeFormatsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumTimeFormats]
  end;
end;
{$ELSE}
function EnumTimeFormats; external kernel32 name 'EnumTimeFormatsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsA: Pointer;

function EnumDateFormatsA;
begin
  GetProcedureAddress(_EnumDateFormatsA, kernel32, 'EnumDateFormatsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsA]
  end;
end;
{$ELSE}
function EnumDateFormatsA; external kernel32 name 'EnumDateFormatsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsW: Pointer;

function EnumDateFormatsW;
begin
  GetProcedureAddress(_EnumDateFormatsW, kernel32, 'EnumDateFormatsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsW]
  end;
end;
{$ELSE}
function EnumDateFormatsW; external kernel32 name 'EnumDateFormatsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormats: Pointer;

function EnumDateFormats;
begin
  GetProcedureAddress(_EnumDateFormats, kernel32, 'EnumDateFormatsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormats]
  end;
end;
{$ELSE}
function EnumDateFormats; external kernel32 name 'EnumDateFormatsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormats: Pointer;

function EnumDateFormats;
begin
  GetProcedureAddress(_EnumDateFormats, kernel32, 'EnumDateFormatsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormats]
  end;
end;
{$ELSE}
function EnumDateFormats; external kernel32 name 'EnumDateFormatsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsExA: Pointer;

function EnumDateFormatsExA;
begin
  GetProcedureAddress(_EnumDateFormatsExA, kernel32, 'EnumDateFormatsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsExA]
  end;
end;
{$ELSE}
function EnumDateFormatsExA; external kernel32 name 'EnumDateFormatsExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsExW: Pointer;

function EnumDateFormatsExW;
begin
  GetProcedureAddress(_EnumDateFormatsExW, kernel32, 'EnumDateFormatsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsExW]
  end;
end;
{$ELSE}
function EnumDateFormatsExW; external kernel32 name 'EnumDateFormatsExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsEx: Pointer;

function EnumDateFormatsEx;
begin
  GetProcedureAddress(_EnumDateFormatsEx, kernel32, 'EnumDateFormatsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsEx]
  end;
end;
{$ELSE}
function EnumDateFormatsEx; external kernel32 name 'EnumDateFormatsExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDateFormatsEx: Pointer;

function EnumDateFormatsEx;
begin
  GetProcedureAddress(_EnumDateFormatsEx, kernel32, 'EnumDateFormatsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDateFormatsEx]
  end;
end;
{$ELSE}
function EnumDateFormatsEx; external kernel32 name 'EnumDateFormatsExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsValidLanguageGroup: Pointer;

function IsValidLanguageGroup;
begin
  GetProcedureAddress(_IsValidLanguageGroup, kernel32, 'IsValidLanguageGroup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidLanguageGroup]
  end;
end;
{$ELSE}
function IsValidLanguageGroup; external kernel32 name 'IsValidLanguageGroup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNLSVersion: Pointer;

function GetNLSVersion;
begin
  GetProcedureAddress(_GetNLSVersion, kernel32, 'GetNLSVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNLSVersion]
  end;
end;
{$ELSE}
function GetNLSVersion; external kernel32 name 'GetNLSVersion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsNLSDefinedString: Pointer;

function IsNLSDefinedString;
begin
  GetProcedureAddress(_IsNLSDefinedString, kernel32, 'IsNLSDefinedString');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsNLSDefinedString]
  end;
end;
{$ELSE}
function IsNLSDefinedString; external kernel32 name 'IsNLSDefinedString';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsValidLocale: Pointer;

function IsValidLocale;
begin
  GetProcedureAddress(_IsValidLocale, kernel32, 'IsValidLocale');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidLocale]
  end;
end;
{$ELSE}
function IsValidLocale; external kernel32 name 'IsValidLocale';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGeoInfoA: Pointer;

function GetGeoInfoA;
begin
  GetProcedureAddress(_GetGeoInfoA, kernel32, 'GetGeoInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGeoInfoA]
  end;
end;
{$ELSE}
function GetGeoInfoA; external kernel32 name 'GetGeoInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGeoInfoW: Pointer;

function GetGeoInfoW;
begin
  GetProcedureAddress(_GetGeoInfoW, kernel32, 'GetGeoInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGeoInfoW]
  end;
end;
{$ELSE}
function GetGeoInfoW; external kernel32 name 'GetGeoInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGeoInfo: Pointer;

function GetGeoInfo;
begin
  GetProcedureAddress(_GetGeoInfo, kernel32, 'GetGeoInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGeoInfo]
  end;
end;
{$ELSE}
function GetGeoInfo; external kernel32 name 'GetGeoInfo';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetGeoInfo: Pointer;

function GetGeoInfo;
begin
  GetProcedureAddress(_GetGeoInfo, kernel32, 'GetGeoInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGeoInfo]
  end;
end;
{$ELSE}
function GetGeoInfo; external kernel32 name 'GetGeoInfo';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemGeoID: Pointer;

function EnumSystemGeoID;
begin
  GetProcedureAddress(_EnumSystemGeoID, kernel32, 'EnumSystemGeoID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemGeoID]
  end;
end;
{$ELSE}
function EnumSystemGeoID; external kernel32 name 'EnumSystemGeoID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserGeoID: Pointer;

function GetUserGeoID;
begin
  GetProcedureAddress(_GetUserGeoID, kernel32, 'GetUserGeoID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserGeoID]
  end;
end;
{$ELSE}
function GetUserGeoID; external kernel32 name 'GetUserGeoID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserGeoID: Pointer;

function SetUserGeoID;
begin
  GetProcedureAddress(_SetUserGeoID, kernel32, 'SetUserGeoID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserGeoID]
  end;
end;
{$ELSE}
function SetUserGeoID; external kernel32 name 'SetUserGeoID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertDefaultLocale: Pointer;

function ConvertDefaultLocale;
begin
  GetProcedureAddress(_ConvertDefaultLocale, kernel32, 'ConvertDefaultLocale');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertDefaultLocale]
  end;
end;
{$ELSE}
function ConvertDefaultLocale; external kernel32 name 'ConvertDefaultLocale';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadLocale: Pointer;

function GetThreadLocale;
begin
  GetProcedureAddress(_GetThreadLocale, kernel32, 'GetThreadLocale');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadLocale]
  end;
end;
{$ELSE}
function GetThreadLocale; external kernel32 name 'GetThreadLocale';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadLocale: Pointer;

function SetThreadLocale;
begin
  GetProcedureAddress(_SetThreadLocale, kernel32, 'SetThreadLocale');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadLocale]
  end;
end;
{$ELSE}
function SetThreadLocale; external kernel32 name 'SetThreadLocale';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDefaultUILanguage: Pointer;

function GetSystemDefaultUILanguage;
begin
  GetProcedureAddress(_GetSystemDefaultUILanguage, kernel32, 'GetSystemDefaultUILanguage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDefaultUILanguage]
  end;
end;
{$ELSE}
function GetSystemDefaultUILanguage; external kernel32 name 'GetSystemDefaultUILanguage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserDefaultUILanguage: Pointer;

function GetUserDefaultUILanguage;
begin
  GetProcedureAddress(_GetUserDefaultUILanguage, kernel32, 'GetUserDefaultUILanguage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserDefaultUILanguage]
  end;
end;
{$ELSE}
function GetUserDefaultUILanguage; external kernel32 name 'GetUserDefaultUILanguage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDefaultLangID: Pointer;

function GetSystemDefaultLangID;
begin
  GetProcedureAddress(_GetSystemDefaultLangID, kernel32, 'GetSystemDefaultLangID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDefaultLangID]
  end;
end;
{$ELSE}
function GetSystemDefaultLangID; external kernel32 name 'GetSystemDefaultLangID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserDefaultLangID: Pointer;

function GetUserDefaultLangID;
begin
  GetProcedureAddress(_GetUserDefaultLangID, kernel32, 'GetUserDefaultLangID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserDefaultLangID]
  end;
end;
{$ELSE}
function GetUserDefaultLangID; external kernel32 name 'GetUserDefaultLangID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDefaultLCID: Pointer;

function GetSystemDefaultLCID;
begin
  GetProcedureAddress(_GetSystemDefaultLCID, kernel32, 'GetSystemDefaultLCID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDefaultLCID]
  end;
end;
{$ELSE}
function GetSystemDefaultLCID; external kernel32 name 'GetSystemDefaultLCID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserDefaultLCID: Pointer;

function GetUserDefaultLCID;
begin
  GetProcedureAddress(_GetUserDefaultLCID, kernel32, 'GetUserDefaultLCID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserDefaultLCID]
  end;
end;
{$ELSE}
function GetUserDefaultLCID; external kernel32 name 'GetUserDefaultLCID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeExA: Pointer;

function GetStringTypeExA;
begin
  GetProcedureAddress(_GetStringTypeExA, kernel32, 'GetStringTypeExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeExA]
  end;
end;
{$ELSE}
function GetStringTypeExA; external kernel32 name 'GetStringTypeExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeExW: Pointer;

function GetStringTypeExW;
begin
  GetProcedureAddress(_GetStringTypeExW, kernel32, 'GetStringTypeExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeExW]
  end;
end;
{$ELSE}
function GetStringTypeExW; external kernel32 name 'GetStringTypeExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeEx: Pointer;

function GetStringTypeEx;
begin
  GetProcedureAddress(_GetStringTypeEx, kernel32, 'GetStringTypeExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeEx]
  end;
end;
{$ELSE}
function GetStringTypeEx; external kernel32 name 'GetStringTypeExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeEx: Pointer;

function GetStringTypeEx;
begin
  GetProcedureAddress(_GetStringTypeEx, kernel32, 'GetStringTypeExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeEx]
  end;
end;
{$ELSE}
function GetStringTypeEx; external kernel32 name 'GetStringTypeExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeA: Pointer;

function GetStringTypeA;
begin
  GetProcedureAddress(_GetStringTypeA, kernel32, 'GetStringTypeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeA]
  end;
end;
{$ELSE}
function GetStringTypeA; external kernel32 name 'GetStringTypeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStringTypeW: Pointer;

function GetStringTypeW;
begin
  GetProcedureAddress(_GetStringTypeW, kernel32, 'GetStringTypeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStringTypeW]
  end;
end;
{$ELSE}
function GetStringTypeW; external kernel32 name 'GetStringTypeW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FoldStringA: Pointer;

function FoldStringA;
begin
  GetProcedureAddress(_FoldStringA, kernel32, 'FoldStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FoldStringA]
  end;
end;
{$ELSE}
function FoldStringA; external kernel32 name 'FoldStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FoldStringW: Pointer;

function FoldStringW;
begin
  GetProcedureAddress(_FoldStringW, kernel32, 'FoldStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FoldStringW]
  end;
end;
{$ELSE}
function FoldStringW; external kernel32 name 'FoldStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FoldString: Pointer;

function FoldString;
begin
  GetProcedureAddress(_FoldString, kernel32, 'FoldStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FoldString]
  end;
end;
{$ELSE}
function FoldString; external kernel32 name 'FoldStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FoldString: Pointer;

function FoldString;
begin
  GetProcedureAddress(_FoldString, kernel32, 'FoldStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FoldString]
  end;
end;
{$ELSE}
function FoldString; external kernel32 name 'FoldStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLanguageGroupsA: Pointer;

function EnumSystemLanguageGroupsA;
begin
  GetProcedureAddress(_EnumSystemLanguageGroupsA, kernel32, 'EnumSystemLanguageGroupsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLanguageGroupsA]
  end;
end;
{$ELSE}
function EnumSystemLanguageGroupsA; external kernel32 name 'EnumSystemLanguageGroupsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLanguageGroupsW: Pointer;

function EnumSystemLanguageGroupsW;
begin
  GetProcedureAddress(_EnumSystemLanguageGroupsW, kernel32, 'EnumSystemLanguageGroupsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLanguageGroupsW]
  end;
end;
{$ELSE}
function EnumSystemLanguageGroupsW; external kernel32 name 'EnumSystemLanguageGroupsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLanguageGroups: Pointer;

function EnumSystemLanguageGroups;
begin
  GetProcedureAddress(_EnumSystemLanguageGroups, kernel32, 'EnumSystemLanguageGroupsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLanguageGroups]
  end;
end;
{$ELSE}
function EnumSystemLanguageGroups; external kernel32 name 'EnumSystemLanguageGroupsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLanguageGroups: Pointer;

function EnumSystemLanguageGroups;
begin
  GetProcedureAddress(_EnumSystemLanguageGroups, kernel32, 'EnumSystemLanguageGroupsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLanguageGroups]
  end;
end;
{$ELSE}
function EnumSystemLanguageGroups; external kernel32 name 'EnumSystemLanguageGroupsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumLanguageGroupLocalesA: Pointer;

function EnumLanguageGroupLocalesA;
begin
  GetProcedureAddress(_EnumLanguageGroupLocalesA, kernel32, 'EnumLanguageGroupLocalesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumLanguageGroupLocalesA]
  end;
end;
{$ELSE}
function EnumLanguageGroupLocalesA; external kernel32 name 'EnumLanguageGroupLocalesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumLanguageGroupLocalesW: Pointer;

function EnumLanguageGroupLocalesW;
begin
  GetProcedureAddress(_EnumLanguageGroupLocalesW, kernel32, 'EnumLanguageGroupLocalesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumLanguageGroupLocalesW]
  end;
end;
{$ELSE}
function EnumLanguageGroupLocalesW; external kernel32 name 'EnumLanguageGroupLocalesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumLanguageGroupLocales: Pointer;

function EnumLanguageGroupLocales;
begin
  GetProcedureAddress(_EnumLanguageGroupLocales, kernel32, 'EnumLanguageGroupLocalesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumLanguageGroupLocales]
  end;
end;
{$ELSE}
function EnumLanguageGroupLocales; external kernel32 name 'EnumLanguageGroupLocalesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumLanguageGroupLocales: Pointer;

function EnumLanguageGroupLocales;
begin
  GetProcedureAddress(_EnumLanguageGroupLocales, kernel32, 'EnumLanguageGroupLocalesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumLanguageGroupLocales]
  end;
end;
{$ELSE}
function EnumLanguageGroupLocales; external kernel32 name 'EnumLanguageGroupLocalesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumUILanguagesA: Pointer;

function EnumUILanguagesA;
begin
  GetProcedureAddress(_EnumUILanguagesA, kernel32, 'EnumUILanguagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumUILanguagesA]
  end;
end;
{$ELSE}
function EnumUILanguagesA; external kernel32 name 'EnumUILanguagesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumUILanguagesW: Pointer;

function EnumUILanguagesW;
begin
  GetProcedureAddress(_EnumUILanguagesW, kernel32, 'EnumUILanguagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumUILanguagesW]
  end;
end;
{$ELSE}
function EnumUILanguagesW; external kernel32 name 'EnumUILanguagesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumUILanguages: Pointer;

function EnumUILanguages;
begin
  GetProcedureAddress(_EnumUILanguages, kernel32, 'EnumUILanguagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumUILanguages]
  end;
end;
{$ELSE}
function EnumUILanguages; external kernel32 name 'EnumUILanguagesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumUILanguages: Pointer;

function EnumUILanguages;
begin
  GetProcedureAddress(_EnumUILanguages, kernel32, 'EnumUILanguagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumUILanguages]
  end;
end;
{$ELSE}
function EnumUILanguages; external kernel32 name 'EnumUILanguagesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLocalesA: Pointer;

function EnumSystemLocalesA;
begin
  GetProcedureAddress(_EnumSystemLocalesA, kernel32, 'EnumSystemLocalesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLocalesA]
  end;
end;
{$ELSE}
function EnumSystemLocalesA; external kernel32 name 'EnumSystemLocalesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLocalesW: Pointer;

function EnumSystemLocalesW;
begin
  GetProcedureAddress(_EnumSystemLocalesW, kernel32, 'EnumSystemLocalesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLocalesW]
  end;
end;
{$ELSE}
function EnumSystemLocalesW; external kernel32 name 'EnumSystemLocalesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLocales: Pointer;

function EnumSystemLocales;
begin
  GetProcedureAddress(_EnumSystemLocales, kernel32, 'EnumSystemLocalesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLocales]
  end;
end;
{$ELSE}
function EnumSystemLocales; external kernel32 name 'EnumSystemLocalesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemLocales: Pointer;

function EnumSystemLocales;
begin
  GetProcedureAddress(_EnumSystemLocales, kernel32, 'EnumSystemLocalesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemLocales]
  end;
end;
{$ELSE}
function EnumSystemLocales; external kernel32 name 'EnumSystemLocalesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemCodePagesA: Pointer;

function EnumSystemCodePagesA;
begin
  GetProcedureAddress(_EnumSystemCodePagesA, kernel32, 'EnumSystemCodePagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemCodePagesA]
  end;
end;
{$ELSE}
function EnumSystemCodePagesA; external kernel32 name 'EnumSystemCodePagesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemCodePagesW: Pointer;

function EnumSystemCodePagesW;
begin
  GetProcedureAddress(_EnumSystemCodePagesW, kernel32, 'EnumSystemCodePagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemCodePagesW]
  end;
end;
{$ELSE}
function EnumSystemCodePagesW; external kernel32 name 'EnumSystemCodePagesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemCodePages: Pointer;

function EnumSystemCodePages;
begin
  GetProcedureAddress(_EnumSystemCodePages, kernel32, 'EnumSystemCodePagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemCodePages]
  end;
end;
{$ELSE}
function EnumSystemCodePages; external kernel32 name 'EnumSystemCodePagesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumSystemCodePages: Pointer;

function EnumSystemCodePages;
begin
  GetProcedureAddress(_EnumSystemCodePages, kernel32, 'EnumSystemCodePagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumSystemCodePages]
  end;
end;
{$ELSE}
function EnumSystemCodePages; external kernel32 name 'EnumSystemCodePagesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
