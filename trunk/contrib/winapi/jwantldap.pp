{******************************************************************************}
{                                                       	               }
{ NT LDAP API interface Unit for Object Pascal                                 }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: ntldap.h, released June 2000. The original Pascal      }
{ code is: NtLDAP.pas, released December 2000. The initial developer of the    }
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

unit JwaNtLDAP;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "ntldap.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

//
//
// Server controls section
//

//
// Permissive Modify Control.  No Data.
//

const
  LDAP_SERVER_PERMISSIVE_MODIFY_OID   = '1.2.840.113556.1.4.1413';
  LDAP_SERVER_PERMISSIVE_MODIFY_OID_W = '1.2.840.113556.1.4.1413';

//
// Show Deleted Control.  No Data.
//

  LDAP_SERVER_SHOW_DELETED_OID   = '1.2.840.113556.1.4.417';
  LDAP_SERVER_SHOW_DELETED_OID_W = '1.2.840.113556.1.4.417';

//
// Cross Domain Move Control. Data as follows
//      SEQUENCE {
//          Name OCTET STRING
//      }
//

  LDAP_SERVER_CROSSDOM_MOVE_TARGET_OID   = '1.2.840.113556.1.4.521';
  LDAP_SERVER_CROSSDOM_MOVE_TARGET_OID_W = '1.2.840.113556.1.4.521';

//
// Notification. No Data.
//

  LDAP_SERVER_NOTIFICATION_OID   = '1.2.840.113556.1.4.528';
  LDAP_SERVER_NOTIFICATION_OID_W = '1.2.840.113556.1.4.528';

//
// Lazy Commit. No Data.
//

  LDAP_SERVER_LAZY_COMMIT_OID   = '1.2.840.113556.1.4.619';
  LDAP_SERVER_LAZY_COMMIT_OID_W = '1.2.840.113556.1.4.619';

//
// Security Descriptor Flag. Data as follows
//      SEQUENCE {
//          Flags INTEGER
//      }
//

  LDAP_SERVER_SD_FLAGS_OID   = '1.2.840.113556.1.4.801';
  LDAP_SERVER_SD_FLAGS_OID_W = '1.2.840.113556.1.4.801';

//
// Tree Delete. No Data.
//

  LDAP_SERVER_TREE_DELETE_OID   = '1.2.840.113556.1.4.805';
  LDAP_SERVER_TREE_DELETE_OID_W = '1.2.840.113556.1.4.805';

//
// DirSync operation. Data as follows
//      SEQUENCE {
//          Flags   INTEGER
//          Size    INTEGER
//          Cookie  OCTET STRING
//      }
//

  LDAP_SERVER_DIRSYNC_OID   = '1.2.840.113556.1.4.841';
  LDAP_SERVER_DIRSYNC_OID_W = '1.2.840.113556.1.4.841';

//
// Return extended DNs. No Data.
//

  LDAP_SERVER_EXTENDED_DN_OID   = '1.2.840.113556.1.4.529';
  LDAP_SERVER_EXTENDED_DN_OID_W = '1.2.840.113556.1.4.529';

//
// Tell DC which server to verify with that a DN exist. Data as follows
//      SEQUENCE {
//          Flags   INTEGER,
//          ServerName OCTET STRING     // unicode server string
//      }
//

  LDAP_SERVER_VERIFY_NAME_OID   = '1.2.840.113556.1.4.1338';
  LDAP_SERVER_VERIFY_NAME_OID_W = '1.2.840.113556.1.4.1338';

//
// Tells server not to generate referrals
//

  LDAP_SERVER_DOMAIN_SCOPE_OID   = '1.2.840.113556.1.4.1339';
  LDAP_SERVER_DOMAIN_SCOPE_OID_W = '1.2.840.113556.1.4.1339';

//
// Server Search Options. Allows the client to pass in flags to control
// various search behaviours. Data as follows
//      SEQUENCE {
//          Flags   INTEGER
//      }
//

  LDAP_SERVER_SEARCH_OPTIONS_OID   = '1.2.840.113556.1.4.1340';
  LDAP_SERVER_SEARCH_OPTIONS_OID_W = '1.2.840.113556.1.4.1340';

//
// search option flags
//

  SERVER_SEARCH_FLAG_DOMAIN_SCOPE = $1; // no referrals generated
  SERVER_SEARCH_FLAG_PHANTOM_ROOT = $2; // search all NCs subordinate to search base

//
// End of Server controls
//

//
//
// Operational Attributes
//

  LDAP_OPATT_BECOME_DOM_MASTER   = 'becomeDomainMaster';
  LDAP_OPATT_BECOME_DOM_MASTER_W = 'becomeDomainMaster';

  LDAP_OPATT_BECOME_RID_MASTER   = 'becomeRidMaster';
  LDAP_OPATT_BECOME_RID_MASTER_W = 'becomeRidMaster';

  LDAP_OPATT_BECOME_SCHEMA_MASTER   = 'becomeSchemaMaster';
  LDAP_OPATT_BECOME_SCHEMA_MASTER_W = 'becomeSchemaMaster';

  LDAP_OPATT_RECALC_HIERARCHY   = 'recalcHierarchy';
  LDAP_OPATT_RECALC_HIERARCHY_W = 'recalcHierarchy';

  LDAP_OPATT_SCHEMA_UPDATE_NOW   = 'schemaUpdateNow';
  LDAP_OPATT_SCHEMA_UPDATE_NOW_W = 'schemaUpdateNow';

  LDAP_OPATT_BECOME_PDC   = 'becomePdc';
  LDAP_OPATT_BECOME_PDC_W = 'becomePdc';

  LDAP_OPATT_FIXUP_INHERITANCE   = 'fixupInheritance';
  LDAP_OPATT_FIXUP_INHERITANCE_W = 'fixupInheritance';

  LDAP_OPATT_INVALIDATE_RID_POOL   = 'invalidateRidPool';
  LDAP_OPATT_INVALIDATE_RID_POOL_W = 'invalidateRidPool';

  LDAP_OPATT_ABANDON_REPL   = 'abandonReplication';
  LDAP_OPATT_ABANDON_REPL_W = 'abandonReplication';

  LDAP_OPATT_DO_GARBAGE_COLLECTION   = 'doGarbageCollection';
  LDAP_OPATT_DO_GARBAGE_COLLECTION_W = 'doGarbageCollection';

//
//  Root DSE Attributes
//

  LDAP_OPATT_SUBSCHEMA_SUBENTRY   = 'subschemaSubentry';
  LDAP_OPATT_SUBSCHEMA_SUBENTRY_W = 'subschemaSubentry';

  LDAP_OPATT_CURRENT_TIME   = 'currentTime';
  LDAP_OPATT_CURRENT_TIME_W = 'currentTime';

  LDAP_OPATT_SERVER_NAME   = 'serverName';
  LDAP_OPATT_SERVER_NAME_W = 'serverName';

  LDAP_OPATT_NAMING_CONTEXTS   = 'namingContexts';
  LDAP_OPATT_NAMING_CONTEXTS_W = 'namingContexts';

  LDAP_OPATT_DEFAULT_NAMING_CONTEXT   = 'defaultNamingContext';
  LDAP_OPATT_DEFAULT_NAMING_CONTEXT_W = 'defaultNamingContext';

  LDAP_OPATT_SUPPORTED_CONTROL   = 'supportedControl';
  LDAP_OPATT_SUPPORTED_CONTROL_W = 'supportedControl';

  LDAP_OPATT_HIGHEST_COMMITTED_USN   = 'highestCommitedUSN';
  LDAP_OPATT_HIGHEST_COMMITTED_USN_W = 'highestCommitedUSN';

  LDAP_OPATT_SUPPORTED_LDAP_VERSION   = 'supportedLDAPVersion';
  LDAP_OPATT_SUPPORTED_LDAP_VERSION_W = 'supportedLDAPVersion';

  LDAP_OPATT_SUPPORTED_LDAP_POLICIES   = 'supportedLDAPPolicies';
  LDAP_OPATT_SUPPORTED_LDAP_POLICIES_W = 'supportedLDAPPolicies';

  LDAP_OPATT_SCHEMA_NAMING_CONTEXT   = 'schemaNamingContext';
  LDAP_OPATT_SCHEMA_NAMING_CONTEXT_W = 'schemaNamingContext';

  LDAP_OPATT_CONFIG_NAMING_CONTEXT   = 'configurationNamingContext';
  LDAP_OPATT_CONFIG_NAMING_CONTEXT_W = 'configurationNamingContext';

  LDAP_OPATT_ROOT_DOMAIN_NAMING_CONTEXT   = 'rootDomainNamingContext';
  LDAP_OPATT_ROOT_DOMAIN_NAMING_CONTEXT_W = 'rootDomainNamingContext';

  LDAP_OPATT_SUPPORTED_SASL_MECHANISM   = 'supportedSASLMechanisms';
  LDAP_OPATT_SUPPORTED_SASL_MECHANISM_W = 'supportedSASLMechanisms';

  LDAP_OPATT_DNS_HOST_NAME   = 'dnsHostName';
  LDAP_OPATT_DNS_HOST_NAME_W = 'dnsHostName';

  LDAP_OPATT_LDAP_SERVICE_NAME   = 'ldapServiceName';
  LDAP_OPATT_LDAP_SERVICE_NAME_W = 'ldapServiceName';

  LDAP_OPATT_DS_SERVICE_NAME   = 'dsServiceName';
  LDAP_OPATT_DS_SERVICE_NAME_W = 'dsServiceName';

  LDAP_OPATT_SUPPORTED_CAPABILITIES   = 'supportedCapabilities';
  LDAP_OPATT_SUPPORTED_CAPABILITIES_W = 'supportedCapabilities';

//
// End of Operational attributes
//



//
//
// Server Capabilities
//

//
// NT5 Active Directory
//

  LDAP_CAP_ACTIVE_DIRECTORY_OID   = '1.2.840.113556.1.4.800';
  LDAP_CAP_ACTIVE_DIRECTORY_OID_W = '1.2.840.113556.1.4.800';

//
//  End of capabilities
//


//
//
// Matching Rules
//

//
// BIT AND
//

  LDAP_MATCHING_RULE_BIT_AND   = '1.2.840.113556.1.4.803';
  LDAP_MATCHING_RULE_BIT_AND_W = '1.2.840.113556.1.4.803';

//
// BIT OR
//

  LDAP_MATCHING_RULE_BIT_OR   = '1.2.840.113556.1.4.804';
  LDAP_MATCHING_RULE_BIT_OR_W = '1.2.840.113556.1.4.804';

implementation

end.
