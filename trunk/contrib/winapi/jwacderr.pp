{******************************************************************************}
{                                                       	               }
{ Common dialog error return codes API interface Unit for Object Pascal        }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: cderr.h, released June 2000. The original Pascal       }
{ code is: CdErr.pas, released December 2000. The initial developer of the     }
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

unit JwaCdErr;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "cderr.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

const
  CDERR_DIALOGFAILURE = $FFFF;

  CDERR_GENERALCODES    = $0000;
  CDERR_STRUCTSIZE      = $0001;
  CDERR_INITIALIZATION  = $0002;
  CDERR_NOTEMPLATE      = $0003;
  CDERR_NOHINSTANCE     = $0004;
  CDERR_LOADSTRFAILURE  = $0005;
  CDERR_FINDRESFAILURE  = $0006;
  CDERR_LOADRESFAILURE  = $0007;
  CDERR_LOCKRESFAILURE  = $0008;
  CDERR_MEMALLOCFAILURE = $0009;
  CDERR_MEMLOCKFAILURE  = $000A;
  CDERR_NOHOOK          = $000B;
  CDERR_REGISTERMSGFAIL = $000C;

  PDERR_PRINTERCODES     = $1000;
  PDERR_SETUPFAILURE     = $1001;
  PDERR_PARSEFAILURE     = $1002;
  PDERR_RETDEFFAILURE    = $1003;
  PDERR_LOADDRVFAILURE   = $1004;
  PDERR_GETDEVMODEFAIL   = $1005;
  PDERR_INITFAILURE      = $1006;
  PDERR_NODEVICES        = $1007;
  PDERR_NODEFAULTPRN     = $1008;
  PDERR_DNDMMISMATCH     = $1009;
  PDERR_CREATEICFAILURE  = $100A;
  PDERR_PRINTERNOTFOUND  = $100B;
  PDERR_DEFAULTDIFFERENT = $100C;

  CFERR_CHOOSEFONTCODES = $2000;
  CFERR_NOFONTS         = $2001;
  CFERR_MAXLESSTHANMIN  = $2002;

  FNERR_FILENAMECODES   = $3000;
  FNERR_SUBCLASSFAILURE = $3001;
  FNERR_INVALIDFILENAME = $3002;
  FNERR_BUFFERTOOSMALL  = $3003;

  FRERR_FINDREPLACECODES = $4000;
  FRERR_BUFFERLENGTHZERO = $4001;

  CCERR_CHOOSECOLORCODES = $5000;

implementation

end.
