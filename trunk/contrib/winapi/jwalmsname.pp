{******************************************************************************}
{                                                       	               }
{ Lan Manager Service Names API interface Unit for Object Pascal               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: lmsname.h, released November 2001. The original Pascal }
{ code is: LmSName.pas, released Februari 2002. The initial developer of the   }
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

unit JwaLmSName;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "lmsname.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//
//  Standard LAN Manager service names.
//

const
  SERVICE_WORKSTATION      = TEXT('LanmanWorkstation');
  SERVICE_LM20_WORKSTATION = TEXT('WORKSTATION');
  WORKSTATION_DISPLAY_NAME = TEXT('Workstation');

  SERVICE_SERVER      = TEXT('LanmanServer');
  SERVICE_LM20_SERVER = TEXT('SERVER');
  SERVER_DISPLAY_NAME = TEXT('Server');

  SERVICE_BROWSER      = TEXT('BROWSER');
  SERVICE_LM20_BROWSER = SERVICE_BROWSER;

  SERVICE_MESSENGER      = TEXT('MESSENGER');
  SERVICE_LM20_MESSENGER = SERVICE_MESSENGER;

  SERVICE_NETRUN      = TEXT('NETRUN');
  SERVICE_LM20_NETRUN = SERVICE_NETRUN;

  SERVICE_SPOOLER      = TEXT('SPOOLER');
  SERVICE_LM20_SPOOLER = SERVICE_SPOOLER;

  SERVICE_ALERTER      = TEXT('ALERTER');
  SERVICE_LM20_ALERTER = SERVICE_ALERTER;

  SERVICE_NETLOGON      = TEXT('NETLOGON');
  SERVICE_LM20_NETLOGON = SERVICE_NETLOGON;

  SERVICE_NETPOPUP      = TEXT('NETPOPUP');
  SERVICE_LM20_NETPOPUP = SERVICE_NETPOPUP;

  SERVICE_SQLSERVER      = TEXT('SQLSERVER');
  SERVICE_LM20_SQLSERVER = SERVICE_SQLSERVER;

  SERVICE_REPL      = TEXT('REPLICATOR');
  SERVICE_LM20_REPL = SERVICE_REPL;

  SERVICE_RIPL      = TEXT('REMOTEBOOT');
  SERVICE_LM20_RIPL = SERVICE_RIPL;

  SERVICE_TIMESOURCE      = TEXT('TIMESOURCE');
  SERVICE_LM20_TIMESOURCE = SERVICE_TIMESOURCE;

  SERVICE_AFP      = TEXT('AFP');
  SERVICE_LM20_AFP = SERVICE_AFP;

  SERVICE_UPS      = TEXT('UPS');
  SERVICE_LM20_UPS = SERVICE_UPS;

  SERVICE_XACTSRV      = TEXT('XACTSRV');
  SERVICE_LM20_XACTSRV = SERVICE_XACTSRV;

  SERVICE_TCPIP      = TEXT('TCPIP');
  SERVICE_LM20_TCPIP = SERVICE_TCPIP;

  SERVICE_NBT      = TEXT('NBT');
  SERVICE_LM20_NBT = SERVICE_NBT;

  SERVICE_LMHOSTS      = TEXT('LMHOSTS');
  SERVICE_LM20_LMHOSTS = SERVICE_LMHOSTS;

  SERVICE_TELNET      = TEXT('Telnet');
  SERVICE_LM20_TELNET = SERVICE_TELNET;

  SERVICE_SCHEDULE      = TEXT('Schedule');
  SERVICE_LM20_SCHEDULE = SERVICE_SCHEDULE;

  SERVICE_NTLMSSP = TEXT('NtLmSsp');

  SERVICE_DHCP      = TEXT('DHCP');
  SERVICE_LM20_DHCP = SERVICE_DHCP;

  SERVICE_NWSAP      = TEXT('NwSapAgent');
  SERVICE_LM20_NWSAP = SERVICE_NWSAP;
  NWSAP_DISPLAY_NAME = TEXT('NW Sap Agent');

  SERVICE_NWCS      = TEXT('NWCWorkstation');
  SERVICE_DNS_CACHE = TEXT('DnsCache');

  SERVICE_W32TIME     = TEXT('w32time');
  SERVCE_LM20_W32TIME = SERVICE_W32TIME;

  SERVICE_KDC      = TEXT('kdc');
  SERVICE_LM20_KDC = SERVICE_KDC;

  SERVICE_RPCLOCATOR      = TEXT('RPCLOCATOR');
  SERVICE_LM20_RPCLOCATOR = SERVICE_RPCLOCATOR;

  SERVICE_TRKSVR      = TEXT('TrkSvr');
  SERVICE_LM20_TRKSVR = SERVICE_TRKSVR;

  SERVICE_TRKWKS      = TEXT('TrkWks');
  SERVICE_LM20_TRKWKS = SERVICE_TRKWKS;

  SERVICE_NTFRS      = TEXT('NtFrs');
  SERVICE_LM20_NTFRS = SERVICE_NTFRS;

  SERVICE_ISMSERV      = TEXT('IsmServ');
  SERVICE_LM20_ISMSERV = SERVICE_ISMSERV;

implementation

end.
