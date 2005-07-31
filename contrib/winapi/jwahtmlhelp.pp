{******************************************************************************}
{                                                       	               }
{ HTML Help API interface Unit for Object Pascal                               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: htmlhelp.h, released July 2000. The original Pascal    }
{ code is: HtmlHelp.pas, released September 2000. The initial developer of the }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Contributor(s): Robert Chandler  (robert@helpware.net)                       }
{ 		  Kurt Senfer (ks@siemens.dk)  		                       }
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

unit JwaHtmlHelp;

interface

{$WEAKPACKAGEUNIT}

(*$HPPEMIT ''*)
(*$HPPEMIT '#include "htmlhelp.h"'*)
(*$HPPEMIT ''*)
(*$HPPEMIT 'typedef struct tagHH_LAST_ERROR'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '  int     cbStruct ;'*)
(*$HPPEMIT '  HRESULT hr ;'*)
(*$HPPEMIT '  BSTR    description ;'*)
(*$HPPEMIT '} HH_LAST_ERROR ;'*)
(*$HPPEMIT ''*)

{$I WINDEFINES.INC}
{$DEFINE HTMLHELP12}

uses
  JwaWinType, JwaWinUser{$IFDEF VER140}, Variants{$ENDIF};

// Commands to pass to HtmlHelp()

const
  HH_DISPLAY_TOPIC           = $0000;
  HH_HELP_FINDER             = $0000;      // WinHelp equivalent
  HH_DISPLAY_TOC             = $0001;      // not currently implemented
  HH_DISPLAY_INDEX           = $0002;      // not currently implemented
  HH_DISPLAY_SEARCH          = $0003;      // not currently implemented
  HH_SET_WIN_TYPE            = $0004;
  HH_GET_WIN_TYPE            = $0005;
  HH_GET_WIN_HANDLE          = $0006;
  HH_ENUM_INFO_TYPE          = $0007;      // Get Info type name, call repeatedly to enumerate, -1 at end
  HH_SET_INFO_TYPE           = $0008;      // Add Info type to filter.
  HH_SYNC                    = $0009;
{$IFDEF HTMLHELP11}
  HH_ADD_NAV_UI              = $000A;      // not currently implemented
  HH_ADD_BUTTON              = $000B;      // not currently implemented
  HH_GETBROWSER_APP          = $000C;      // not currently implemented
{$ENDIF}
{$IFDEF HTMLHELP12}
  HH_RESERVED1               = $000A;
  HH_RESERVED2               = $000B;
  HH_RESERVED3               = $000C;
{$ENDIF}
  HH_KEYWORD_LOOKUP          = $000D;
  HH_DISPLAY_TEXT_POPUP      = $000E;      // display string resource id or text in a popup window
  HH_HELP_CONTEXT            = $000F;      // display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU     = $0010;      // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP         = $0011;      // text popup help, same as WinHelp HELP_WM_HELP
  HH_CLOSE_ALL               = $0012;      // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP            = $0013;      // ALink version of HH_KEYWORD_LOOKUP
  HH_GET_LAST_ERROR          = $0014;      // not currently implemented, See HHERROR.h
  HH_ENUM_CATEGORY           = $0015;	     // Get category name, call repeatedly to enumerate, -1 at end
  HH_ENUM_CATEGORY_IT        = $0016;      // Get category info type members, call repeatedly to enumerate, -1 at end
  HH_RESET_IT_FILTER         = $0017;      // Clear the info type filter of all info types.
  HH_SET_INCLUSIVE_FILTER    = $0018;      // set inclusive filtering method for untyped topics to be included in display
  HH_SET_EXCLUSIVE_FILTER    = $0019;      // set exclusive filtering method for untyped topics to be excluded from display
{$IFDEF HTMLHELP11}
  HH_SET_GUID                = $001A;      // For Microsoft Installer -- dwData is a pointer to the GUID string
{$ENDIF}
{$IFDEF HTMLHELP12}
  HH_INITIALIZE              = $001C;      // Initializes the help system.
  HH_UNINITIALIZE            = $001D;      // Uninitializes the help system.
  HH_PRETRANSLATEMESSAGE     = $00FD;      // Pumps messages. (NULL, NULL, MSG*).
  HH_SET_GLOBAL_PROPERTY     = $00FC;      // Set a global property. (NULL, NULL, HH_GPROP)
{$ENDIF}

{$IFDEF HTMLHELP11}
  HH_INTERNAL                = $00FF;      // Used internally.
{$ENDIF}

{$IFDEF HTMLHELP12}
  HHWIN_PROP_TAB_AUTOHIDESHOW = (1 shl 0); // Automatically hide/show tri-pane window
{$ENDIF}
  HHWIN_PROP_ONTOP           = (1 shl 1);  // Top-most window (not currently implemented)
  HHWIN_PROP_NOTITLEBAR      = (1 shl 2);  // no title bar
  HHWIN_PROP_NODEF_STYLES    = (1 shl 3);  // no default window styles (only HH_WINTYPE.dwStyles)
  HHWIN_PROP_NODEF_EXSTYLES  = (1 shl 4);  // no default extended window styles (only HH_WINTYPE.dwExStyles)
  HHWIN_PROP_TRI_PANE        = (1 shl 5);  // use a tri-pane window
  HHWIN_PROP_NOTB_TEXT       = (1 shl 6);  // no text on toolbar buttons
  HHWIN_PROP_POST_QUIT       = (1 shl 7);  // post WM_QUIT message when window closes
  HHWIN_PROP_AUTO_SYNC       = (1 shl 8);  // automatically ssync contents and index
  HHWIN_PROP_TRACKING        = (1 shl 9);  // send tracking notification messages
  HHWIN_PROP_TAB_SEARCH      = (1 shl 10); // include search tab in navigation pane
  HHWIN_PROP_TAB_HISTORY     = (1 shl 11); // include history tab in navigation pane
{$IFDEF HTMLHELP11}
  HHWIN_PROP_TAB_BOOKMARKS   = (1 shl 12); // include bookmark tab in navigation pane
{$ENDIF}
{$IFDEF HTMLHELP12}
  HHWIN_PROP_TAB_FAVORITES   = (1 shl 12); // include favorites tab in navigation pane
{$ENDIF}
  HHWIN_PROP_CHANGE_TITLE    = (1 shl 13); // Put current HTML title in title bar
  HHWIN_PROP_NAV_ONLY_WIN    = (1 shl 14); // Only display the navigation window
  HHWIN_PROP_NO_TOOLBAR      = (1 shl 15); // Don't display a toolbar
  HHWIN_PROP_MENU            = (1 shl 16); // Menu
  HHWIN_PROP_TAB_ADVSEARCH   = (1 shl 17); // Advanced FTS UI.
  HHWIN_PROP_USER_POS        = (1 shl 18); // After initial creation, user controls window size/Position
{$IFDEF HTMLHELP12}
  HHWIN_PROP_TAB_CUSTOM1     = (1 shl 19); // Use custom tab #1
  HHWIN_PROP_TAB_CUSTOM2     = (1 shl 20); // Use custom tab #2
  HHWIN_PROP_TAB_CUSTOM3     = (1 shl 21); // Use custom tab #3
  HHWIN_PROP_TAB_CUSTOM4     = (1 shl 22); // Use custom tab #4
  HHWIN_PROP_TAB_CUSTOM5     = (1 shl 23); // Use custom tab #5
  HHWIN_PROP_TAB_CUSTOM6     = (1 shl 24); // Use custom tab #6
  HHWIN_PROP_TAB_CUSTOM7     = (1 shl 25); // Use custom tab #7
  HHWIN_PROP_TAB_CUSTOM8     = (1 shl 26); // Use custom tab #8
  HHWIN_PROP_TAB_CUSTOM9     = (1 shl 27); // Use custom tab #9
  HHWIN_TB_MARGIN            = (1 shl 28); // the window type has a margin
{$ENDIF}

  HHWIN_PARAM_PROPERTIES     = (1 shl 1);  // valid fsWinProperties
  HHWIN_PARAM_STYLES         = (1 shl 2);  // valid dwStyles
  HHWIN_PARAM_EXSTYLES       = (1 shl 3);  // valid dwExStyles
  HHWIN_PARAM_RECT           = (1 shl 4);  // valid rcWindowPos
  HHWIN_PARAM_NAV_WIDTH      = (1 shl 5);  // valid iNavWidth
  HHWIN_PARAM_SHOWSTATE      = (1 shl 6);  // valid nShowState
  HHWIN_PARAM_INFOTYPES      = (1 shl 7);  // valid apInfoTypes
  HHWIN_PARAM_TB_FLAGS       = (1 shl 8);  // valid fsToolBarFlags
  HHWIN_PARAM_EXPANSION      = (1 shl 9);  // valid fNotExpanded
  HHWIN_PARAM_TABPOS         = (1 shl 10); // valid tabpos
  HHWIN_PARAM_TABORDER       = (1 shl 11); // valid taborder
  HHWIN_PARAM_HISTORY_COUNT  = (1 shl 12); // valid cHistory
  HHWIN_PARAM_CUR_TAB        = (1 shl 13); // valid curNavType

  HHWIN_BUTTON_EXPAND        = (1 shl 1);  // Expand/contract button
  HHWIN_BUTTON_BACK          = (1 shl 2);  // Back button
  HHWIN_BUTTON_FORWARD       = (1 shl 3);  // Forward button
  HHWIN_BUTTON_STOP          = (1 shl 4);  // Stop button
  HHWIN_BUTTON_REFRESH       = (1 shl 5);  // Refresh button
  HHWIN_BUTTON_HOME          = (1 shl 6);  // Home button
  HHWIN_BUTTON_BROWSE_FWD    = (1 shl 7);  // not implemented
  HHWIN_BUTTON_BROWSE_BCK    = (1 shl 8);  // not implemented
  HHWIN_BUTTON_NOTES         = (1 shl 9);  // not implemented
  HHWIN_BUTTON_CONTENTS      = (1 shl 10); // not implemented
  HHWIN_BUTTON_SYNC          = (1 shl 11); // Sync button
  HHWIN_BUTTON_OPTIONS       = (1 shl 12); // Options button
  HHWIN_BUTTON_PRINT         = (1 shl 13); // Print button
  HHWIN_BUTTON_INDEX         = (1 shl 14); // not implemented
  HHWIN_BUTTON_SEARCH        = (1 shl 15); // not implemented
  HHWIN_BUTTON_HISTORY       = (1 shl 16); // not implemented
{$IFDEF HTMLHELP11}
  HHWIN_BUTTON_BOOKMARKS     = (1 shl 17); // not implemented
{$ENDIF}
{$IFDEF HTMLHELP12}
  HHWIN_BUTTON_FAVORITES     = (1 shl 17); // not implemented
{$ENDIF}
  HHWIN_BUTTON_JUMP1         = (1 shl 18);
  HHWIN_BUTTON_JUMP2         = (1 shl 19);
  HHWIN_BUTTON_ZOOM          = (1 shl 20);
  HHWIN_BUTTON_TOC_NEXT      = (1 shl 21);
  HHWIN_BUTTON_TOC_PREV      = (1 shl 22);

  HHWIN_DEF_BUTTONS = HHWIN_BUTTON_EXPAND or HHWIN_BUTTON_BACK or
                      HHWIN_BUTTON_OPTIONS or HHWIN_BUTTON_PRINT;

// Button IDs

  IDTB_EXPAND       = 200;
  IDTB_CONTRACT     = 201;
  IDTB_STOP         = 202;
  IDTB_REFRESH      = 203;
  IDTB_BACK         = 204;
  IDTB_HOME         = 205;
  IDTB_SYNC         = 206;
  IDTB_PRINT        = 207;
  IDTB_OPTIONS      = 208;
  IDTB_FORWARD      = 209;
  IDTB_NOTES        = 210;                 // not implemented
  IDTB_BROWSE_FWD   = 211;
  IDTB_BROWSE_BACK  = 212;
  IDTB_CONTENTS     = 213;                 // not implemented
  IDTB_INDEX        = 214;                 // not implemented
  IDTB_SEARCH       = 215;                 // not implemented
  IDTB_HISTORY      = 216;                 // not implemented
{$IFDEF HTMLHELP11}
  IDTB_BOOKMARKS    = 217;                 // not implemented
{$ENDIF}
{$IFDEF HTMLHELP12}
  IDTB_FAVORITES    = 217;                 // not implemented
{$ENDIF}
  IDTB_JUMP1        = 218;
  IDTB_JUMP2        = 219;
  IDTB_CUSTOMIZE    = 221;
  IDTB_ZOOM         = 222;
  IDTB_TOC_NEXT     = 223;
  IDTB_TOC_PREV     = 224;

// Notification codes

  HHN_FIRST         = -860;
  HHN_LAST          = -879;

  HHN_NAVCOMPLETE   = (HHN_FIRST - 0);
  HHN_TRACK         = (HHN_FIRST - 1);
  HHN_WINDOW_CREATE = (HHN_FIRST - 2);

type
  PHHLastError = ^THHLastError;
  tagHH_LAST_ERROR = packed record
    cbStruct: INT;          // size of this structure
    hr: HRESULT;            // the last error code.
    description: LPWSTR;    // a description of the error (unicode string - BSTR).
  end;
  THHLastError = tagHH_LAST_ERROR;

  PHHNNotify = ^THHNNotify;
  tagHHN_NOTIFY = packed record
    hdr: NMHDR;
    pszUrl: PCSTR;                         // multibyte null-terminated string
  end;
  HHN_NOTIFY = tagHHN_NOTIFY;
  THHNNotify = tagHHN_NOTIFY;

  PHHPopup = ^THHPopup;
  tagHH_POPUP = packed record
    cbStruct: Integer;                     // sizeof this structure
    hinst_: HINSTANCE;                     // instance handle for string resource
    idString: UINT;                        // string resource id, or text id if pszFile is specified in HtmlHelp call
    pszText: LPCTSTR;                      // used if idString is zero
    pt: POINT;                             // top center of popup window
    clrForeGround: COLORREF;               // use -1 for default
    clrBackground: COLORREF;               // use -1 for default
    rcMargins: RECT;                       // amount of space between edges of window and text, -1 for each member to ignore
    pszFont: LPCTSTR;                      // facename, point size, char set, BOLD ITALIC UNDERLINE
  end;
  HH_POPUP = tagHH_POPUP;
  THHPopup = tagHH_POPUP;

  PHHAKLink = ^THHAKLink;
  tagHH_AKLINK = packed record
    cbStruct: Integer;                     // sizeof this structure
    fReserved: BOOL;                       // must be FALSE (really!)
    pszKeywords: LPCTSTR;                  // semi-colon separated keywords
    pszUrl: LPCTSTR;                       // URL to jump to if no keywords found (may be NULL)
    pszMsgText: LPCTSTR;                   // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    pszMsgTitle: LPCTSTR;                  // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    pszWindow: LPCTSTR;                    // Window to display URL in
    fIndexOnFail: BOOL;                    // Displays index if keyword lookup fails.
  end;
  HH_AKLINK = tagHH_AKLINK;
  THHAKLink = tagHH_AKLINK;

const
  HHWIN_NAVTYPE_TOC          = 0;
  HHWIN_NAVTYPE_INDEX        = 1;
  HHWIN_NAVTYPE_SEARCH       = 2;
{$IFDEF HTMLHELP11}
  HHWIN_NAVTYPE_BOOKMARKS    = 3;
  HHWIN_NAVTYPE_HISTORY      = 4;          //not implemented
{$ENDIF}
{$IFDEF HTMLHELP12}
  HHWIN_NAVTYPE_FAVORITES    = 3;
  HHWIN_NAVTYPE_HISTORY      = 4;          //not implemented
  HHWIN_NAVTYPE_AUTHOR       = 5;
  HHWIN_NAVTYPE_CUSTOM_FIRST = 11;
{$ENDIF}

  IT_INCLUSIVE = 0;
  IT_EXCLUSIVE = 1;
  IT_HIDDEN    = 2;

type
  PHHEnumIT = ^THHEnumIT;
  tagHH_ENUM_IT = packed record
    cbStruct: Integer;        // size of this structure
    iType: Integer;           // the type of the information type ie. Inclusive, Exclusive, or Hidden
    pszCatName: LPCSTR;       // Set to the name of the Category to enumerate the info types in a category; else NULL
    pszITName: LPCSTR;        // volitile pointer to the name of the infotype. Allocated by call. Caller responsible for freeing
    pszITDescription: LPCSTR; // volitile pointer to the description of the infotype.
  end;
  HH_ENUM_IT = tagHH_ENUM_IT;
  PHH_ENUM_IT = ^tagHH_ENUM_IT;
  THHEnumIT = tagHH_ENUM_IT;

  PHHEnumCat = ^THHEnumCat;
  tagHH_ENUM_CAT = packed record
    cbStruct: Integer;         // size of this structure
    pszCatName: LPCSTR;        // volitile pointer to the category name
    pszCatDescription: LPCSTR; // volitile pointer to the category description
  end;
  HH_ENUM_CAT = tagHH_ENUM_CAT;
  PHH_ENUM_CAT = ^tagHH_ENUM_CAT;
  THHEnumCat = tagHH_ENUM_CAT;

  PHHSetInfoType = ^THHSetInfoType;
  tagHH_SET_INFOTYPE = packed record
    cbStruct: Integer;        // the size of this structure
    pszCatName: LPCSTR;       // the name of the category, if any, the InfoType is a member of.
    pszInfoTypeName: LPCSTR;  // the name of the info type to add to the filter
  end;
  HH_SET_INFOTYPE = tagHH_SET_INFOTYPE;
  PHH_SET_INFOTYPE = ^tagHH_SET_INFOTYPE;
  THHSetInfoType = tagHH_SET_INFOTYPE;

  HH_INFOTYPE = DWORD;
  PHH_INFOTYPE = ^HH_INFOTYPE;
  PHHInfoType = ^THHInfoType;
  THHInfoType = HH_INFOTYPE;

const
  HHWIN_NAVTAB_TOP    = 0;
  HHWIN_NAVTAB_LEFT   = 1;
  HHWIN_NAVTAB_BOTTOM = 2;

  HH_MAX_TABS         = 19;

  HH_TAB_CONTENTS     = 0;
  HH_TAB_INDEX        = 1;
  HH_TAB_SEARCH       = 2;
{$IFDEF HTMLHELP11}
  HH_TAB_BOOKMARKS    = 3;
  HH_TAB_HISTORY      = 4;
{$ENDIF}
{$IFDEF HTMLHELP12}
  HH_TAB_FAVORITES    = 3;
  HH_TAB_HISTORY      = 4;
  HH_TAB_AUTHOR       = 5;

  HH_TAB_CUSTOM_FIRST = 11;
  HH_TAB_CUSTOM_LAST  = HH_MAX_TABS;

  HH_MAX_TABS_CUSTOM  = HH_TAB_CUSTOM_LAST - HH_TAB_CUSTOM_FIRST + 1;
{$ENDIF}

// HH_DISPLAY_SEARCH Command Related Structures and Constants

  HH_FTS_DEFAULT_PROXIMITY = -1;

type
  PHHFtsQuery = ^THHFtsQuery;
  tagHH_FTS_QUERY = packed record
    cbStruct: Integer;         // Sizeof structure in bytes.
    fUniCodeStrings: BOOL;     // TRUE if all strings are unicode.
    pszSearchQuery: LPCTSTR;   // String containing the search query.
    iProximity: LONG;          // Word proximity.
    fStemmedSearch: BOOL;      // TRUE for StemmedSearch only.
    fTitleOnly: BOOL;          // TRUE for Title search only.
    fExecute: BOOL;            // TRUE to initiate the search.
    pszWindow: LPCTSTR;        // Window to display in
  end;
  HH_FTS_QUERY = tagHH_FTS_QUERY;
  THHFtsQuery = tagHH_FTS_QUERY;

  PHHWinType = ^THHWinType;
  tagHH_WINTYPE = packed record
    cbStruct: Integer;      // IN: size of this structure including all Information Types
    fUniCodeStrings: BOOL;  // IN/OUT: TRUE if all strings are in UNICODE
    pszType: LPCTSTR;       // IN/OUT: Name of a type of window
    fsValidMembers: DWORD;  // IN: Bit flag of valid members (HHWIN_PARAM_)
    fsWinProperties: DWORD; // IN/OUT: Properties/attributes of the window (HHWIN_)

    pszCaption: LPCTSTR;    // IN/OUT: Window title
    dwStyles: DWORD;        // IN/OUT: Window styles
    dwExStyles: DWORD;      // IN/OUT: Extended Window styles
    rcWindowPos: RECT;      // IN: Starting position, OUT: current position
    nShowState: Integer;    // IN: show state (e.g., SW_SHOW)

    hwndHelp: HWND;         // OUT: window handle
    hwndCaller: HWND;       // OUT: who called this window

    paInfoTypes: PHHInfoType; // IN: Pointer to an array of Information Types

    { The following members are only valid if HHWIN_PROP_TRI_PANE is set }

    hwndToolBar: HWND;      // OUT: toolbar window in tri-pane window
    hwndNavigation: HWND;   // OUT: navigation window in tri-pane window
    hwndHTML: HWND;         // OUT: window displaying HTML in tri-pane window
    iNavWidth: Integer;     // IN/OUT: width of navigation window
    rcHTML: RECT;           // OUT: HTML window coordinates

    pszToc: LPCTSTR;        // IN: Location of the table of contents file
    pszIndex: LPCTSTR;      // IN: Location of the index file
    pszFile: LPCTSTR;       // IN: Default location of the html file
    pszHome: LPCTSTR;       // IN/OUT: html file to display when Home button is clicked
    fsToolBarFlags: DWORD;  // IN: flags controling the appearance of the toolbar
    fNotExpanded: BOOL;     // IN: TRUE/FALSE to contract or expand, OUT: current state
    curNavType: Integer;    // IN/OUT: UI to display in the navigational pane
    tabpos: Integer;        // IN/OUT: HHWIN_NAVTAB_TOP, HHWIN_NAVTAB_LEFT, or HHWIN_NAVTAB_BOTTOM
    idNotify: Integer;      // IN: ID to use for WM_NOTIFY messages
    tabOrder: array[0..HH_MAX_TABS] of Byte; // IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs
    cHistory: Integer;      // IN/OUT: number of history items to keep (default is 30)
    pszJump1: LPCTSTR;      // Text for HHWIN_BUTTON_JUMP1
    pszJump2: LPCTSTR;      // Text for HHWIN_BUTTON_JUMP2
    pszUrlJump1: LPCTSTR;   // URL for HHWIN_BUTTON_JUMP1
    pszUrlJump2: LPCTSTR;   // URL for HHWIN_BUTTON_JUMP2
    rcMinSize: RECT;        // Minimum size for window (ignored in version 1)
    cbInfoTypes: Integer;   // size of paInfoTypes;
{$IFDEF HTMLHELP12}
    pszCustomTabs: LPCTSTR; // multiple zero-terminated Strings
{$ENDIF}
  end;
  HH_WINTYPE = tagHH_WINTYPE;
  PHH_WINTYPE = ^tagHH_WINTYPE;
  THHWinType = tagHH_WINTYPE;

const
  HHACT_TAB_CONTENTS  = 0;
  HHACT_TAB_INDEX     = 1;
  HHACT_TAB_SEARCH    = 2;
  HHACT_TAB_HISTORY   = 3;
  HHACT_TAB_FAVORITES = 4;
  HHACT_EXPAND        = 5;
  HHACT_CONTRACT      = 6;
  HHACT_BACK          = 7;
  HHACT_FORWARD       = 8;
  HHACT_STOP          = 9;
  HHACT_REFRESH       = 10;
  HHACT_HOME          = 11;
  HHACT_SYNC          = 12;
  HHACT_OPTIONS       = 13;
  HHACT_PRINT         = 14;
  HHACT_HIGHLIGHT     = 15;
  HHACT_CUSTOMIZE     = 16;
  HHACT_JUMP1         = 17;
  HHACT_JUMP2         = 18;
  HHACT_ZOOM          = 19;
  HHACT_TOC_NEXT      = 20;
  HHACT_TOC_PREV      = 21;
  HHACT_NOTES         = 22;
  HHACT_LAST_ENUM     = 23;

type
  PHHNTrack = ^THHNTrack;
  tagHHNTRACK = packed record
    hdr: NMHDR;
    pszCurUrl: PCSTR;        // Multi-byte, null-terminated string
    idAction: Integer;       // HHACT_ value
    phhWinType: PHHWinType;  // Current window type structure
  end;
  HHNTRACK = tagHHNTRACK;
  THHNTrack = tagHHNTRACK;

function HtmlHelpA(hwndCaller: HWND; pszFile: LPCSTR; uCommand: UINT; dwData: DWORD_PTR): HWND; stdcall;
function HtmlHelpW(hwndCaller: HWND; pszFile: LPCWSTR; uCommand: UINT; dwData: DWORD_PTR): HWND; stdcall;

{$IFDEF UNICODE}
function HtmlHelp(hwndCaller: HWND; pszFile: LPCSTR; uCommand: UINT; dwData: DWORD_PTR): HWND; stdcall;
{$ELSE}
function HtmlHelp(hwndCaller: HWND; pszFile: LPCWSTR; uCommand: UINT; dwData: DWORD_PTR): HWND; stdcall;
{$ENDIF}

// Use the following for GetProcAddress to load from hhctrl.ocx

const
  ATOM_HTMLHELP_API_ANSI    = LPTSTR(DWORD(WORD(14)));
  ATOM_HTMLHELP_API_UNICODE = LPTSTR(DWORD(WORD(15)));

{$IFDEF HTMLHELP12}
// Global Control Properties

const
  HH_GPROPID_SINGLETHREAD     = 1; // VARIANT_BOOL: True for single thread
  HH_GPROPID_TOOLBAR_MARGIN   = 2; // long: Provides a left/right margin around the toolbar.
  HH_GPROPID_UI_LANGUAGE      = 3; // long: LangId of the UI.
  HH_GPROPID_CURRENT_SUBSET   = 4; // BSTR: Current subset.
  HH_GPROPID_CONTENT_LANGUAGE = 5; // long: LandId for desired content.

type
  HH_GPROPID = HH_GPROPID_SINGLETHREAD..HH_GPROPID_CONTENT_LANGUAGE;
  THHGPropID = HH_GPROPID;

// Global Property structure

  PHHGlobalProperty = ^THHGlobalProperty;
  tagHH_GLOBAL_PROPERTY = record
    id: THHGPropID;
    Dummy: Integer;                        // MVB: Added to enforce 8-byte packing
    var_: OleVariant;
  end;
  HH_GLOBAL_PROPERTY = tagHH_GLOBAL_PROPERTY;
  THHGlobalProperty = tagHH_GLOBAL_PROPERTY;
{$ENDIF}

implementation

{$IFDEF DYNAMIC_LINK}

uses
  JwaWinBase, JwaWinError, JwaWinNT, JwaWinReg;

function GetOCXPath: string;
const
  HHPathRegKey = 'CLSID\{adb880a6-d8ff-11cf-9377-00aa003b7a11}\InprocServer32';
var
  HHKey: HKEY;
  R, PathSize, ValueType: DWORD;
  Path: string;
begin
  R := ERROR_PATH_NOT_FOUND;
  if RegOpenKeyExA(HKEY_CLASSES_ROOT, PChar(HHPathRegKey), 0, KEY_QUERY_VALUE, HHKey) = ERROR_SUCCESS then
  begin
    ValueType := 0;
    PathSize := 0;
    if RegQueryValueExA(HHKey, PChar(''), nil, @ValueType, nil, @PathSize) = ERROR_SUCCESS then
    begin
      if ValueType = REG_SZ then
      begin
        SetLength(Path, PathSize);
        R := RegQueryValueExA(HHKey, PChar(''), nil, @ValueType, PByte(Path), @PathSize);
        Result := PChar(Path);
      end;
    end;
    RegCloseKey(HHKey);
  end;
  if R <> ERROR_SUCCESS then Result := 'hhctrl.ocx';
end;

var
  _HtmlHelpA: Pointer;
  _HtmlHelpW: Pointer;
  _HtmlHelp: Pointer;

function HtmlHelpA;
begin
  GetProcedureAddress(_HtmlHelpA, GetOCXPath, 'HtmlHelpA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HtmlHelpA]
  end;
end;

function HtmlHelpW;
begin
  GetProcedureAddress(_HtmlHelpA, GetOCXPath, 'HtmlHelpW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HtmlHelpW]
  end;
end;

function HtmlHelp;
begin
  GetProcedureAddress(_HtmlHelp, GetOCXPath, {$IFDEF UNICODE}'HtmlHelpW'{$ELSE}'HtmlHelpA'{$ENDIF});
  asm
    mov esp, ebp
    pop ebp
    jmp [_HtmlHelp]
  end;
end;

{$ELSE}

const
  hhctrl = 'hhctrl.ocx';

function HtmlHelpA; external hhctrl name 'HtmlHelpA';
function HtmlHelpW; external hhctrl name 'HtmlHelpW';
function HtmlHelp; external hhctrl name {$IFDEF UNICODE}'HtmlHelpW'{$ELSE}'HtmlHelpA'{$ENDIF};

{$ENDIF DYNAMIC_LINK}

end.
