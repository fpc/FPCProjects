{******************************************************************************}
{                                                       	               }
{ Win32 color dialog API interface Unit for Object Pascal                      }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: colordlg.h, released June 2000. The original Pascal    }
{ code is: ColorDlg.pas, released December 2000. The initial developer of the  }
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

unit JwaColorDlg;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "colordlg.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

//
//  Constant Declarations.
//

const
  DLG_COLOR = 10;

  COLOR_HUESCROLL = 700; // color dialog
  COLOR_SATSCROLL = 701;
  COLOR_LUMSCROLL = 702;
  COLOR_HUE       = 703;
  COLOR_SAT       = 704;
  COLOR_LUM       = 705;
  COLOR_RED       = 706;
  COLOR_GREEN     = 707;
  COLOR_BLUE      = 708;
  COLOR_CURRENT   = 709;
  COLOR_RAINBOW   = 710;
  COLOR_SAVE      = 711;
  COLOR_ADD       = 712;
  COLOR_SOLID     = 713;
  COLOR_TUNE      = 714;
  COLOR_SCHEMES   = 715;
  COLOR_ELEMENT   = 716;
  COLOR_SAMPLES   = 717;
  COLOR_PALETTE   = 718;
  COLOR_MIX       = 719;
  COLOR_BOX1      = 720;
  COLOR_CUSTOM1   = 721;

  COLOR_HUEACCEL   = 723;
  COLOR_SATACCEL   = 724;
  COLOR_LUMACCEL   = 725;
  COLOR_REDACCEL   = 726;
  COLOR_GREENACCEL = 727;
  COLOR_BLUEACCEL  = 728;

  COLOR_SOLID_LEFT  = 730;
  COLOR_SOLID_RIGHT = 731;

  NUM_BASIC_COLORS  = 48;
  NUM_CUSTOM_COLORS = 16;

implementation

end.
