{******************************************************************************}
{                                                       	               }
{ Active Directory Class Names API interface Unit for Object Pascal            }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: adsnms.h, released June 2000. The original Pascal      }
{ code is: AdsNms.pas, released December 2000. The initial developer of the    }
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

unit JwaAdsnms;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "adsnms.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

//  Contents:   Class Names and schema definitions for ADS objects

const
  NAMESPACE_CLASS_NAME        = 'Namespace';
  COUNTRY_CLASS_NAME          = 'Country';
  LOCALITY_CLASS_NAME         = 'Locality';
  ORGANIZATION_CLASS_NAME     = 'Organization';
  ORGANIZATIONUNIT_CLASS_NAME = 'Organizational Unit';
  DOMAIN_CLASS_NAME           = 'Domain';
  COMPUTER_CLASS_NAME         = 'Computer';
  USER_CLASS_NAME             = 'User';
  GROUP_CLASS_NAME            = 'Group';
  GLOBALGROUP_CLASS_NAME      = 'GlobalGroup';
  LOCALGROUP_CLASS_NAME       = 'LocalGroup';
  SERVICE_CLASS_NAME          = 'Service';
  FILESERVICE_CLASS_NAME      = 'FileService';
  SESSION_CLASS_NAME          = 'Session';
  RESOURCE_CLASS_NAME         = 'Resource';
  FILESHARE_CLASS_NAME        = 'FileShare';
  PRINTER_CLASS_NAME          = 'PrintQueue';
  PRINTJOB_CLASS_NAME         = 'PrintJob';
  SCHEMA_CLASS_NAME           = 'Schema';
  CLASS_CLASS_NAME            = 'Class';
  PROPERTY_CLASS_NAME         = 'Property';
  SYNTAX_CLASS_NAME           = 'Syntax';
  ROOTDSE_CLASS_NAME          = 'RootDSE';

  NO_SCHEMA                    = '';
  DOMAIN_SCHEMA_NAME           = 'Domain';
  COMPUTER_SCHEMA_NAME         = 'Computer';
  USER_SCHEMA_NAME             = 'User';
  GROUP_SCHEMA_NAME            = 'Group';
  GLOBALGROUP_SCHEMA_NAME      = 'GlobalGroup';
  LOCALGROUP_SCHEMA_NAME       = 'LocalGroup';
  SERVICE_SCHEMA_NAME          = 'Service';
  PRINTER_SCHEMA_NAME          = 'PrintQueue';
  PRINTJOB_SCHEMA_NAME         = 'PrintJob';
  FILESERVICE_SCHEMA_NAME      = 'FileService';
  SESSION_SCHEMA_NAME          = 'Session';
  RESOURCE_SCHEMA_NAME         = 'Resource';
  FILESHARE_SCHEMA_NAME        = 'FileShare';
  FPNW_FILESERVICE_SCHEMA_NAME = 'FPNWFileService';
  FPNW_SESSION_SCHEMA_NAME     = 'FPNWSession';
  FPNW_RESOURCE_SCHEMA_NAME    = 'FPNWResource';
  FPNW_FILESHARE_SCHEMA_NAME   = 'FPNWFileShare';

implementation

end.
