{******************************************************************************}
{                                                       	               }
{ Windows Shutdown Reason Codes interface Unit for Object Pascal               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: reason.h, released June 2000. The original Pascal      }
{ code is: Reason.pas, released December 2000. The initial developer of the    }
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

unit JwaReason;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinReg.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

// Reason flags

// Flags used by the various UIs.

const
  SHTDN_REASON_FLAG_COMMENT_REQUIRED          = $01000000;
  SHTDN_REASON_FLAG_DIRTY_PROBLEM_ID_REQUIRED = $02000000;
  SHTDN_REASON_FLAG_CLEAN_UI                  = $04000000;
  SHTDN_REASON_FLAG_DIRTY_UI                  = $08000000;

// Flags that end up in the event log code.

  SHTDN_REASON_FLAG_USER_DEFINED = $40000000;
  SHTDN_REASON_FLAG_PLANNED      = DWORD($80000000);

// Microsoft major reasons.

  SHTDN_REASON_MAJOR_OTHER           = $00000000;
  SHTDN_REASON_MAJOR_NONE            = $00000000;
  SHTDN_REASON_MAJOR_HARDWARE        = $00010000;
  SHTDN_REASON_MAJOR_OPERATINGSYSTEM = $00020000;
  SHTDN_REASON_MAJOR_SOFTWARE        = $00030000;
  SHTDN_REASON_MAJOR_APPLICATION     = $00040000;
  SHTDN_REASON_MAJOR_SYSTEM          = $00050000;
  SHTDN_REASON_MAJOR_POWER           = $00060000;
  SHTDN_REASON_MAJOR_LEGACY_API      = $00070000;

// Microsoft minor reasons.

  SHTDN_REASON_MINOR_OTHER           = $00000000;
  SHTDN_REASON_MINOR_NONE            = $000000ff;
  SHTDN_REASON_MINOR_MAINTENANCE     = $00000001;
  SHTDN_REASON_MINOR_INSTALLATION    = $00000002;
  SHTDN_REASON_MINOR_UPGRADE         = $00000003;
  SHTDN_REASON_MINOR_RECONFIG        = $00000004;
  SHTDN_REASON_MINOR_HUNG            = $00000005;
  SHTDN_REASON_MINOR_UNSTABLE        = $00000006;
  SHTDN_REASON_MINOR_DISK            = $00000007;
  SHTDN_REASON_MINOR_PROCESSOR       = $00000008;
  SHTDN_REASON_MINOR_NETWORKCARD     = $00000009;
  SHTDN_REASON_MINOR_POWER_SUPPLY    = $0000000a;
  SHTDN_REASON_MINOR_CORDUNPLUGGED   = $0000000b;
  SHTDN_REASON_MINOR_ENVIRONMENT     = $0000000c;
  SHTDN_REASON_MINOR_HARDWARE_DRIVER = $0000000d;
  SHTDN_REASON_MINOR_OTHERDRIVER     = $0000000e;
  SHTDN_REASON_MINOR_BLUESCREEN      = $0000000F;
  SHTDN_REASON_MINOR_SERVICEPACK           = $00000010;
  SHTDN_REASON_MINOR_HOTFIX                = $00000011;
  SHTDN_REASON_MINOR_SECURITYFIX           = $00000012;
  SHTDN_REASON_MINOR_SECURITY              = $00000013;
  SHTDN_REASON_MINOR_NETWORK_CONNECTIVITY  = $00000014;
  SHTDN_REASON_MINOR_WMI                   = $00000015;
  SHTDN_REASON_MINOR_SERVICEPACK_UNINSTALL = $00000016;
  SHTDN_REASON_MINOR_HOTFIX_UNINSTALL      = $00000017;
  SHTDN_REASON_MINOR_SECURITYFIX_UNINSTALL = $00000018;
  SHTDN_REASON_MINOR_MMC                   = $00000019;
  SHTDN_REASON_MINOR_TERMSRV               = $00000020;
  SHTDN_REASON_MINOR_DC_PROMOTION          = $00000021;
  SHTDN_REASON_MINOR_DC_DEMOTION           = $00000022;

  SHTDN_REASON_UNKNOWN = SHTDN_REASON_MINOR_NONE;
  SHTDN_REASON_LEGACY_API = (SHTDN_REASON_MAJOR_LEGACY_API or SHTDN_REASON_FLAG_PLANNED);

// This mask cuts out UI flags.

  SHTDN_REASON_VALID_BIT_MASK = DWORD($c0ffffff);

// Convenience flags.

  PCLEANUI = (SHTDN_REASON_FLAG_PLANNED or SHTDN_REASON_FLAG_CLEAN_UI);
  UCLEANUI = (SHTDN_REASON_FLAG_CLEAN_UI);
  PDIRTYUI = (SHTDN_REASON_FLAG_PLANNED or SHTDN_REASON_FLAG_DIRTY_UI);
  UDIRTYUI = (SHTDN_REASON_FLAG_DIRTY_UI);

(*
 * Maximum character lengths for reason name, description, problem id, and
 * comment respectively.
 *)

  MAX_REASON_NAME_LEN    = 64;
  MAX_REASON_DESC_LEN    = 256;
  MAX_REASON_BUGID_LEN   = 32;
  MAX_REASON_COMMENT_LEN = 512;
  SHUTDOWN_TYPE_LEN      = 32;

(*
 *	S.E.T. policy value
 *
 *)

  POLICY_SHOWREASONUI_NEVER	      = 0;
  POLICY_SHOWREASONUI_ALWAYS	      = 1;
  POLICY_SHOWREASONUI_WORKSTATIONONLY =	2;
  POLICY_SHOWREASONUI_SERVERONLY      = 3;

(*
 * Snapshot policy values
 *)

  SNAPSHOT_POLICY_NEVER           = 0;
  SNAPSHOT_POLICY_ALWAYS          = 1;
  SNAPSHOT_POLICY_UNPLANNED       = 2;

(*
 * Maximue user defined reasons
 *)

  MAX_NUM_REASONS = 256;

implementation

end.
