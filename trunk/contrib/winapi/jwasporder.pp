{******************************************************************************}
{                                                       	               }
{ Winsock2 Service Provider ordering API interface Unit for Object Pascal      }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: sporder.h, released June 2000. The original Pascal     }
{ code is: SpOrder.pas, released June 2001. The initial developer of the       }
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

unit JwaSpOrder;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "sporder.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;
  
function WSCWriteProviderOrder(lpwdCatalogEntryId: LPDWORD; dwNumberOfEntries: DWORD): Integer; stdcall;

type
  LPWSCWRITEPROVIDERORDER = function (lpwdCatalogEntryId: LPDWORD; dwNumberOfEntries: DWORD): Integer; stdcall;

function WSCWriteNameSpaceOrder(const lpProviderId: TGUID; dwNumberOfEntries: DWORD): Integer; stdcall;

type
  LPWSCWRITENAMESPACEORDER = function (const lpProviderId: TGUID; dwNumberOfEntries: DWORD): Integer; stdcall;

implementation

const
  sporder_lib = 'sporder.dll';


{$IFDEF DYNAMIC_LINK}
var
  _WSCWriteProviderOrder: Pointer;

function WSCWriteProviderOrder;
begin
  GetProcedureAddress(_WSCWriteProviderOrder, sporder_lib, 'WSCWriteProviderOrder');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSCWriteProviderOrder]
  end;
end;
{$ELSE}
function WSCWriteProviderOrder; external sporder_lib name 'WSCWriteProviderOrder';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSCWriteNameSpaceOrder: Pointer;

function WSCWriteNameSpaceOrder;
begin
  GetProcedureAddress(_WSCWriteNameSpaceOrder, sporder_lib, 'WSCWriteNameSpaceOrder');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSCWriteNameSpaceOrder]
  end;
end;
{$ELSE}
function WSCWriteNameSpaceOrder; external sporder_lib name 'WSCWriteNameSpaceOrder';
{$ENDIF DYNAMIC_LINK}

end.
