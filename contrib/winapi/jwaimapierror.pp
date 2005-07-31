{******************************************************************************}
{                                                       	               }
{ Image Mastering API interface Unit for Object Pascal                         }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: imapierror.h, released Aug 2002. The original Pascal   }
{ code is: ImapiError.pas, released November 2002. The initial developer of the}
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

unit JwaImapiError;

interface

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "imapi.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

uses
  JwaWinError, JwaWinType;

const
  IMAPI_S_PROPERTIESIGNORED     = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or $200);
  IMAPI_S_BUFFER_TO_SMALL       = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or $201);
  IMAPI_E_NOTOPENED             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 11);
  IMAPI_E_NOTINITIALIZED        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 12);
  IMAPI_E_USERABORT             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 13);
  IMAPI_E_GENERIC               = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 14);
  IMAPI_E_MEDIUM_NOTPRESENT     = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 15);
  IMAPI_E_MEDIUM_INVALIDTYPE    = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 16);
  IMAPI_E_DEVICE_NOPROPERTIES   = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 17);
  IMAPI_E_DEVICE_NOTACCESSIBLE  = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 18);
  IMAPI_E_DEVICE_NOTPRESENT     = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 19);
  IMAPI_E_DEVICE_INVALIDTYPE    = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 20);
  IMAPI_E_INITIALIZE_WRITE      = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 21);
  IMAPI_E_INITIALIZE_ENDWRITE   = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 22);
  IMAPI_E_FILESYSTEM            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 23);
  IMAPI_E_FILEACCESS            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 24);
  IMAPI_E_DISCINFO              = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 25);
  IMAPI_E_TRACKNOTOPEN          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 26);
  IMAPI_E_TRACKOPEN             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 27);
  IMAPI_E_DISCFULL              = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 28);
  IMAPI_E_BADJOLIETNAME         = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 29);
  IMAPI_E_INVALIDIMAGE          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 30);
  IMAPI_E_NOACTIVEFORMAT        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 31);
  IMAPI_E_NOACTIVERECORDER      = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 32);
  IMAPI_E_WRONGFORMAT           = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 33);
  IMAPI_E_ALREADYOPEN           = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 34);
  IMAPI_E_WRONGDISC             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 35);
  IMAPI_E_FILEEXISTS            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 36);
  IMAPI_E_STASHINUSE            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 37);
  IMAPI_E_DEVICE_STILL_IN_USE   = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 38);
  IMAPI_E_LOSS_OF_STREAMING     = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 39);
  IMAPI_E_COMPRESSEDSTASH       = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 40);
  IMAPI_E_ENCRYPTEDSTASH        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 41);
  IMAPI_E_NOTENOUGHDISKFORSTASH = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 42);
  IMAPI_E_REMOVABLESTASH        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 43);
  IMAPI_E_CANNOT_WRITE_TO_MEDIA = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 44);
  IMAPI_E_TRACK_NOT_BIG_ENOUGH  = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 45);
  IMAPI_E_BOOTIMAGE_AND_NONBLANK_DISC = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or $200 + 46);

implementation

end.
