{******************************************************************************}
{                                                       	               }
{  IIS Public Metdata IDs API interface Unit for Object Pascal                 }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: iiscnfg.h, released November 2002. The original Pascal }
{ code is: IisCnfg.pas, released March 2002. The initial developer of the      }
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

unit JwaIisCnfg;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "iiscnfg.h"'}
{$HPPEMIT ''}

interface

uses
  JwaWinType;

//
// Paths
//

const
  IIS_MD_LOCAL_MACHINE_PATH = 'LM';

//
// Name of the default publishing root under an instance
//

  IIS_MD_INSTANCE_ROOT = 'Root';

//
//  ISAPI Filters are kept in a list under the instances and the service (for
//  global filters) in the following format:
//
//  LM/W3Svc/<Instance>/Filters
//      MD_FILTER_LOAD_ORDER  "Filter1, Filter2, Filter3"
//
//  LM/W3Svc/<Instance>/Filters/Filter1
//      MD_FILTER_IMAGE_PATH  "d:\inetsrv\myfilter.dll"
//
//  LM/W3Svc/<Instance>/Filters/Filter2
//      MD_FILTER_IMAGE_PATH  "d:\inetsrv\otherfilter.dll"
//

  IIS_MD_ISAPI_FILTERS = '/Filters';

//
// Path below each service to the key that publishes service information
//

  IIS_MD_SVC_INFO_PATH = 'Info';

//
// ADSI schema properties path
//

  IIS_MD_ADSI_SCHEMA_PATH_A = '/Schema';
  IIS_MD_ADSI_SCHEMA_PATH_W = WideString('/Schema');
  IIS_MD_ADSI_METAID_BEGIN  = 130000;

//
// user types
//
// There are two user types:
//
//   Server configuration - All the properties for configuring the server that
//      are not applicable to files and directories - such as Port, Host name,
//      Server comment, Connection timeout etc.
//
//  File/Dir configuration - All the properties that can be configured down to
//      the files and directories - such as Access permissions (Read, Write etc),
//      Extension mapping, IP Security etc.
//

  IIS_MD_UT_SERVER       = 1; // Server configuration parameters
  IIS_MD_UT_FILE         = 2; // File/Dir inheritable properties
  IIS_MD_UT_WAM          = 100; // Web Application configuration parameters
  ASP_MD_UT_APP          = 101; // ASP application configuration parameters
  IIS_MD_UT_END_RESERVED = 2000; // All user types below this are reserved for IIS services

//
//  Metabase property IDs must be unique.  This table defines reserved ranges
//

  IIS_MD_ID_BEGIN_RESERVED    = $00000001; // IIS reserved range
  IIS_MD_ID_END_RESERVED      = $00007fff;
  ASP_MD_ID_BEGIN_RESERVED    = $00007000; // ASP reserved range, subrange of IIS.
  ASP_MD_ID_END_RESERVED      = $000074ff;
  WAM_MD_ID_BEGIN_RESERVED    = $00007500; // ASP reserved range, subrange of IIS.
  WAM_MD_ID_END_RESERVED      = $00007fff;
  FP_MD_ID_BEGIN_RESERVED     = $00008000; // Front page reserved range
  FP_MD_ID_END_RESERVED       = $00008fff;
  SMTP_MD_ID_BEGIN_RESERVED   = $00009000;
  SMTP_MD_ID_END_RESERVED     = $00009fff;
  POP3_MD_ID_BEGIN_RESERVED   = $0000a000;
  POP3_MD_ID_END_RESERVED     = $0000afff;
  NNTP_MD_ID_BEGIN_RESERVED   = $0000b000;
  NNTP_MD_ID_END_RESERVED     = $0000bfff;
  IMAP_MD_ID_BEGIN_RESERVED   = $0000c000;
  IMAP_MD_ID_END_RESERVED     = $0000cfff;
  MSCS_MD_ID_BEGIN_RESERVED   = $0000d000;
  MSCS_MD_ID_END_RESERVED     = $0000dfff;
  APPCTR_MD_ID_BEGIN_RESERVED = $0000e000;
  APPCTR_MD_ID_END_RESERVED   = $0000efff;

  USER_MD_ID_BASE_RESERVED    = $0000ffff;

//
//  General server related attributes - these should be added in the metabase
//  with a user type of IIS_MD_UT_SERVER
//

  IIS_MD_SERVER_BASE = 1000;

//
//  These are global to all services and should only be set at
//  the IIS root
//

  MD_MAX_BANDWIDTH         = (IIS_MD_SERVER_BASE+0);
  MD_KEY_TYPE              = (IIS_MD_SERVER_BASE+2);
  MD_MAX_BANDWIDTH_BLOCKED = (IIS_MD_SERVER_BASE+3);
  MD_SCHEMA_METAID         = (IIS_MD_SERVER_BASE+4);

//
//  These properties are applicable to both HTTP and FTP virtual
//  servers
//

  MD_SERVER_COMMAND            = (IIS_MD_SERVER_BASE+12);
  MD_CONNECTION_TIMEOUT        = (IIS_MD_SERVER_BASE+13);
  MD_MAX_CONNECTIONS           = (IIS_MD_SERVER_BASE+14);
  MD_SERVER_COMMENT            = (IIS_MD_SERVER_BASE+15);
  MD_SERVER_STATE              = (IIS_MD_SERVER_BASE+16);
  MD_SERVER_AUTOSTART          = (IIS_MD_SERVER_BASE+17);
  MD_SERVER_SIZE               = (IIS_MD_SERVER_BASE+18);
  MD_SERVER_LISTEN_BACKLOG     = (IIS_MD_SERVER_BASE+19);
  MD_SERVER_LISTEN_TIMEOUT     = (IIS_MD_SERVER_BASE+20);
  MD_DOWNLEVEL_ADMIN_INSTANCE  = (IIS_MD_SERVER_BASE+21);
  MD_LEVELS_TO_SCAN            = (IIS_MD_SERVER_BASE+22);
  MD_SERVER_BINDINGS           = (IIS_MD_SERVER_BASE+23);
  MD_MAX_ENDPOINT_CONNECTIONS  = (IIS_MD_SERVER_BASE+24);
  MD_SERVER_CONFIGURATION_INFO = (IIS_MD_SERVER_BASE+27);
  MD_IISADMIN_EXTENSIONS       = (IIS_MD_SERVER_BASE+28);
  MD_DISABLE_SOCKET_POOLING    = (IIS_MD_SERVER_BASE+29);
  MD_METADATA_ID_REGISTRATION  = (IIS_MD_SERVER_BASE+30);


//
//  These properties are specific to HTTP and belong to the website
//

  IIS_MD_HTTP_BASE = 2000;

  MD_SECURE_BINDINGS = (IIS_MD_HTTP_BASE+21);

  MD_FILTER_LOAD_ORDER   = (IIS_MD_HTTP_BASE+40);
  MD_FILTER_IMAGE_PATH   = (IIS_MD_HTTP_BASE+41);
  MD_FILTER_STATE        = (IIS_MD_HTTP_BASE+42);
  MD_FILTER_ENABLED      = (IIS_MD_HTTP_BASE+43);
  MD_FILTER_FLAGS        = (IIS_MD_HTTP_BASE+44);
  MD_FILTER_DESCRIPTION  = (IIS_MD_HTTP_BASE+45);
  MD_FILTER_ENABLE_CACHE = (IIS_MD_HTTP_BASE+46);

  MD_ADV_NOTIFY_PWD_EXP_IN_DAYS = (IIS_MD_HTTP_BASE+63);
  MD_ADV_CACHE_TTL              = (IIS_MD_HTTP_BASE+64);
  MD_AUTH_CHANGE_FLAGS          = (IIS_MD_HTTP_BASE+68);

  MD_PROCESS_NTCR_IF_LOGGED_ON = (IIS_MD_HTTP_BASE+70);

  MD_FRONTPAGE_WEB         = (IIS_MD_HTTP_BASE+72);
  MD_IN_PROCESS_ISAPI_APPS = (IIS_MD_HTTP_BASE+73);

  MD_ALLOW_PATH_INFO_FOR_SCRIPT_MAPPINGS = (IIS_MD_HTTP_BASE+95);

  MD_APP_FRIENDLY_NAME             = (IIS_MD_HTTP_BASE+102);
  MD_APP_ROOT                      = (IIS_MD_HTTP_BASE+103);
  MD_APP_ISOLATED                  = (IIS_MD_HTTP_BASE+104);
  MD_APP_WAM_CLSID                 = (IIS_MD_HTTP_BASE+105);
  MD_APP_PACKAGE_ID                = (IIS_MD_HTTP_BASE+106);
  MD_APP_PACKAGE_NAME              = (IIS_MD_HTTP_BASE+107);
  MD_APP_OOP_RECOVER_LIMIT         = (IIS_MD_HTTP_BASE+110);
  MD_APP_PERIODIC_RESTART_TIME     = (IIS_MD_HTTP_BASE+111);
  MD_APP_PERIODIC_RESTART_REQUESTS = (IIS_MD_HTTP_BASE+112);
  MD_APP_PERIODIC_RESTART_SCHEDULE = (IIS_MD_HTTP_BASE+113);
  MD_APP_SHUTDOWN_TIME_LIMIT       = (IIS_MD_HTTP_BASE+114);


  MD_ADMIN_INSTANCE = (IIS_MD_HTTP_BASE+115);

  MD_CUSTOM_ERROR_DESC = (IIS_MD_HTTP_BASE+120);

//
//  Client Access License parameters
//

//
// CPU Accounting and Throttling Properties
//

//
// The enabled flags are per Application or CGI
//

  MD_CPU_RESET_INTERVAL   = (IIS_MD_HTTP_BASE+144);

//
//  Site Server properties
//

//
// Properties to disable/restrict request handlers.
//

  MD_ISAPI_RESTRICTION_LIST         = (IIS_MD_HTTP_BASE+163);
  MD_CGI_RESTRICTION_LIST           = (IIS_MD_HTTP_BASE+164);
  MD_APP_DEPENDENCIES               = (IIS_MD_HTTP_BASE+167);
  MD_WEB_SVC_EXT_RESTRICTION_LIST   = (IIS_MD_HTTP_BASE+168);

  MD_MD_SERVER_SS_AUTH_MAPPING = (IIS_MD_HTTP_BASE+200);

//
// valid values for MD_CERT_CHECK_MODE
//

  MD_CERT_NO_REVOC_CHECK                  = $00000001;
  MD_CERT_CACHE_RETRIEVAL_ONLY            = $00000002;
  MD_CERT_CHECK_REVOCATION_FRESHNESS_TIME = $00000004;
  MD_CERT_NO_USAGE_CHECK                  = $00010000;

//
// HTTP Compression properties.  All are global and unheritable.
//

  MD_HC_COMPRESSION_DIRECTORY       = (IIS_MD_HTTP_BASE+210);
  MD_HC_CACHE_CONTROL_HEADER        = (IIS_MD_HTTP_BASE+211);
  MD_HC_EXPIRES_HEADER              = (IIS_MD_HTTP_BASE+212);
  MD_HC_DO_DYNAMIC_COMPRESSION      = (IIS_MD_HTTP_BASE+213);
  MD_HC_DO_STATIC_COMPRESSION       = (IIS_MD_HTTP_BASE+214);
  MD_HC_DO_ON_DEMAND_COMPRESSION    = (IIS_MD_HTTP_BASE+215);
  MD_HC_DO_DISK_SPACE_LIMITING      = (IIS_MD_HTTP_BASE+216);
  MD_HC_NO_COMPRESSION_FOR_HTTP_10  = (IIS_MD_HTTP_BASE+217);
  MD_HC_NO_COMPRESSION_FOR_PROXIES  = (IIS_MD_HTTP_BASE+218);
  MD_HC_NO_COMPRESSION_FOR_RANGE    = (IIS_MD_HTTP_BASE+219);
  MD_HC_SEND_CACHE_HEADERS          = (IIS_MD_HTTP_BASE+220);
  MD_HC_MAX_DISK_SPACE_USAGE        = (IIS_MD_HTTP_BASE+221);
  MD_HC_IO_BUFFER_SIZE              = (IIS_MD_HTTP_BASE+222);
  MD_HC_COMPRESSION_BUFFER_SIZE     = (IIS_MD_HTTP_BASE+223);
  MD_HC_MAX_QUEUE_LENGTH            = (IIS_MD_HTTP_BASE+224);
  MD_HC_FILES_DELETED_PER_DISK_FREE = (IIS_MD_HTTP_BASE+225);
  MD_HC_MIN_FILE_SIZE_FOR_COMP      = (IIS_MD_HTTP_BASE+226);

  MD_HC_COMPRESSION_DLL           = (IIS_MD_HTTP_BASE+237);
  MD_HC_FILE_EXTENSIONS           = (IIS_MD_HTTP_BASE+238);
  //MD_HC_MIME_TYPE                 = (IIS_MD_HTTP_BASE+239);
  //{$EXTERNALSYM MD_HC_MIME_TYPE}
  MD_HC_PRIORITY                  = (IIS_MD_HTTP_BASE+240);
  MD_HC_DYNAMIC_COMPRESSION_LEVEL = (IIS_MD_HTTP_BASE+241);
  MD_HC_ON_DEMAND_COMP_LEVEL      = (IIS_MD_HTTP_BASE+242);
  MD_HC_CREATE_FLAGS              = (IIS_MD_HTTP_BASE+243);
  MD_HC_SCRIPT_FILE_EXTENSIONS    = (IIS_MD_HTTP_BASE+244);

  MD_HC_DO_NAMESPACE_DYNAMIC_COMPRESSION = (IIS_MD_HTTP_BASE+255);
  MD_HC_DO_NAMESPACE_STATIC_COMPRESSION  = (IIS_MD_HTTP_BASE+256);

//
// Generic property indicating a failure status code - Can be used under
// any component that can fail (virtual directory, filters, applications etc)
//

  MD_WIN32_ERROR = (IIS_MD_SERVER_BASE+99);

//
// Virtual root properties - note MD_ACCESS_PERM is also generally set at
// the virtual directory.  These are used for both HTTP and FTP
//

  IIS_MD_VR_BASE = 3000;

  MD_VR_PATH             = (IIS_MD_VR_BASE+1);
  MD_VR_USERNAME         = (IIS_MD_VR_BASE+2);
  MD_VR_PASSWORD         = (IIS_MD_VR_BASE+3);
  MD_VR_PASSTHROUGH      = (IIS_MD_VR_BASE+6);
  MD_VR_NO_CACHE         = (IIS_MD_VR_BASE+7);
  //MD_VR_IGNORE_TRANSLATE = (IIS_MD_VR_BASE+8);
  //{$EXTERNALSYM MD_VR_IGNORE_TRANSLATE}


//
//  Logging related attributes
//

  IIS_MD_LOG_BASE = 4000;

  MD_LOG_TYPE              = (IIS_MD_LOG_BASE+0);
  MD_LOGFILE_DIRECTORY     = (IIS_MD_LOG_BASE+1);
  MD_LOG_UNUSED1           = (IIS_MD_LOG_BASE+2);
  MD_LOGFILE_PERIOD        = (IIS_MD_LOG_BASE+3);
  MD_LOGFILE_TRUNCATE_SIZE = (IIS_MD_LOG_BASE+4);
  MD_LOG_PLUGIN_MOD_ID     = (IIS_MD_LOG_BASE+5);
  MD_LOG_PLUGIN_UI_ID      = (IIS_MD_LOG_BASE+6);
  MD_LOGSQL_DATA_SOURCES   = (IIS_MD_LOG_BASE+7);
  MD_LOGSQL_TABLE_NAME     = (IIS_MD_LOG_BASE+8);
  MD_LOGSQL_USER_NAME      = (IIS_MD_LOG_BASE+9);
  MD_LOGSQL_PASSWORD       = (IIS_MD_LOG_BASE+10);
  MD_LOG_PLUGIN_ORDER      = (IIS_MD_LOG_BASE+11);
  MD_LOG_PLUGINS_AVAILABLE = (IIS_MD_LOG_BASE+12);
  MD_LOGEXT_FIELD_MASK     = (IIS_MD_LOG_BASE+13);
  MD_LOGEXT_FIELD_MASK2    = (IIS_MD_LOG_BASE+14);

//
// Allow W3C logging file naming and rollover based on Local Time
//

  MD_LOGFILE_LOCALTIME_ROLLOVER = (IIS_MD_LOG_BASE+15);

  IIS_MD_LOG_LAST = MD_LOGFILE_LOCALTIME_ROLLOVER;

//
// Global Flag to denote that IIS will generate one centralized
// binary log file rather than a separate file per web site
//

  MD_GLOBAL_BINARY_LOGGING_ENABLED = (IIS_MD_LOG_BASE+16);

//
// Log type
//

  MD_LOG_TYPE_DISABLED = 0;
  MD_LOG_TYPE_ENABLED  = 1;

//
// LOGGING values
//

  MD_LOGFILE_PERIOD_NONE    = 0;
  MD_LOGFILE_PERIOD_MAXSIZE = 0;
  MD_LOGFILE_PERIOD_DAILY   = 1;
  MD_LOGFILE_PERIOD_WEEKLY  = 2;
  MD_LOGFILE_PERIOD_MONTHLY = 3;
  MD_LOGFILE_PERIOD_HOURLY  = 4;

//
// Field masks for extended logging
// Fields are logged in order of increasing mask value
//

  MD_EXTLOG_DATE             = $00000001;
  MD_EXTLOG_TIME             = $00000002;
  MD_EXTLOG_CLIENT_IP        = $00000004;
  MD_EXTLOG_USERNAME         = $00000008;
  MD_EXTLOG_SITE_NAME        = $00000010;
  MD_EXTLOG_COMPUTER_NAME    = $00000020;
  MD_EXTLOG_SERVER_IP        = $00000040;
  MD_EXTLOG_METHOD           = $00000080;
  MD_EXTLOG_URI_STEM         = $00000100;
  MD_EXTLOG_URI_QUERY        = $00000200;
  MD_EXTLOG_HTTP_STATUS      = $00000400;
  MD_EXTLOG_WIN32_STATUS     = $00000800;
  MD_EXTLOG_BYTES_SENT       = $00001000;
  MD_EXTLOG_BYTES_RECV       = $00002000;
  MD_EXTLOG_TIME_TAKEN       = $00004000;
  MD_EXTLOG_SERVER_PORT      = $00008000;
  MD_EXTLOG_USER_AGENT       = $00010000;
  MD_EXTLOG_COOKIE           = $00020000;
  MD_EXTLOG_REFERER          = $00040000;
  MD_EXTLOG_PROTOCOL_VERSION = $00080000;
  MD_EXTLOG_HOST             = $00100000;
  MD_EXTLOG_HTTP_SUB_STATUS  = $00200000;

  MD_DEFAULT_EXTLOG_FIELDS = (MD_EXTLOG_CLIENT_IP or MD_EXTLOG_TIME or MD_EXTLOG_METHOD or  MD_EXTLOG_URI_STEM or MD_EXTLOG_HTTP_STATUS or MD_EXTLOG_HTTP_SUB_STATUS);

//
// Custom Logging related attributes
//

  IIS_MD_LOGCUSTOM_BASE = 4500;

//
// Custom Logging configuration attributes
//

  MD_LOGCUSTOM_PROPERTY_NAME     = (IIS_MD_LOGCUSTOM_BASE+1);
  MD_LOGCUSTOM_PROPERTY_HEADER   = (IIS_MD_LOGCUSTOM_BASE+2);
  MD_LOGCUSTOM_PROPERTY_ID       = (IIS_MD_LOGCUSTOM_BASE+3);
  MD_LOGCUSTOM_PROPERTY_MASK     = (IIS_MD_LOGCUSTOM_BASE+4);
  MD_LOGCUSTOM_PROPERTY_DATATYPE = (IIS_MD_LOGCUSTOM_BASE+5);
  MD_LOGCUSTOM_SERVICES_STRING   = (IIS_MD_LOGCUSTOM_BASE+6); // MultiSZ List of services that the property is applicable to.

  MD_LOGCUSTOM_PROPERTY_NODE_ID  = (IIS_MD_LOGCUSTOM_BASE+8);

  IIS_MD_LOGCUSTOM_LAST = MD_LOGCUSTOM_PROPERTY_NODE_ID;

//
// Valid values for Custom Logging's MD_LOGCUSTOM_PROPERTY_DATATYPE field
//

  MD_LOGCUSTOM_DATATYPE_INT    = 0;
  MD_LOGCUSTOM_DATATYPE_UINT   = 1;
  MD_LOGCUSTOM_DATATYPE_LONG   = 2;
  MD_LOGCUSTOM_DATATYPE_ULONG  = 3;
  MD_LOGCUSTOM_DATATYPE_FLOAT  = 4;
  MD_LOGCUSTOM_DATATYPE_DOUBLE = 5;
  MD_LOGCUSTOM_DATATYPE_LPSTR  = 6;
  MD_LOGCUSTOM_DATATYPE_LPWSTR = 7;


//
//  ISAPI Filter Notification Flags
//

  MD_NOTIFY_SECURE_PORT    = $00000001;
  MD_NOTIFY_NONSECURE_PORT = $00000002;

  MD_NOTIFY_READ_RAW_DATA      = $00008000;
  MD_NOTIFY_PREPROC_HEADERS    = $00004000;
  MD_NOTIFY_AUTHENTICATION     = $00002000;
  MD_NOTIFY_URL_MAP            = $00001000;
  MD_NOTIFY_ACCESS_DENIED      = $00000800;
  MD_NOTIFY_SEND_RESPONSE      = $00000040;
  MD_NOTIFY_SEND_RAW_DATA      = $00000400;
  MD_NOTIFY_LOG                = $00000200;
  MD_NOTIFY_END_OF_REQUEST     = $00000080;
  MD_NOTIFY_END_OF_NET_SESSION = $00000100;
  MD_NOTIFY_AUTH_COMPLETE      = $04000000;

//
//  ISAPI Filter ordering flags
//

  MD_NOTIFY_ORDER_HIGH    = $00080000;
  MD_NOTIFY_ORDER_MEDIUM  = $00040000;
  MD_NOTIFY_ORDER_LOW     = $00020000;
  MD_NOTIFY_ORDER_DEFAULT = MD_NOTIFY_ORDER_LOW;

  MD_NOTIFY_ORDER_MASK = (MD_NOTIFY_ORDER_HIGH or MD_NOTIFY_ORDER_MEDIUM or MD_NOTIFY_ORDER_LOW);


//
//  These are FTP specific properties
//

  IIS_MD_FTP_BASE = 5000;

  MD_EXIT_MESSAGE            = (IIS_MD_FTP_BASE+1);
  MD_GREETING_MESSAGE        = (IIS_MD_FTP_BASE+2);
  MD_MAX_CLIENTS_MESSAGE     = (IIS_MD_FTP_BASE+3);
  MD_MSDOS_DIR_OUTPUT        = (IIS_MD_FTP_BASE+4);
  MD_ALLOW_ANONYMOUS         = (IIS_MD_FTP_BASE+5);
  MD_ANONYMOUS_ONLY          = (IIS_MD_FTP_BASE+6);
  MD_LOG_ANONYMOUS           = (IIS_MD_FTP_BASE+7);
  MD_LOG_NONANONYMOUS        = (IIS_MD_FTP_BASE+8);
  MD_ALLOW_REPLACE_ON_RENAME = (IIS_MD_FTP_BASE+9);
  MD_SHOW_4_DIGIT_YEAR       = (IIS_MD_FTP_BASE+10);
  MD_BANNER_MESSAGE          = (IIS_MD_FTP_BASE+11);
  MD_USER_ISOLATION          = (IIS_MD_FTP_BASE+12);
  MD_FTP_LOG_IN_UTF_8        = (IIS_MD_FTP_BASE+13);
  MD_AD_CONNECTIONS_USERNAME = (IIS_MD_FTP_BASE+14);
  MD_AD_CONNECTIONS_PASSWORD = (IIS_MD_FTP_BASE+15);
  MD_PASSIVE_PORT_RANGE      = (IIS_MD_FTP_BASE+16);

//
//  These are SSL specific properties
//

  IIS_MD_SSL_BASE = 5500;

  MD_SSL_PUBLIC_KEY   = (IIS_MD_SSL_BASE+0);
  MD_SSL_PRIVATE_KEY  = (IIS_MD_SSL_BASE+1);
  MD_SSL_KEY_PASSWORD = (IIS_MD_SSL_BASE+2);
  MD_SSL_KEY_REQUEST  = (IIS_MD_SSL_BASE+3);

//
// These are server certificate properties
//
//
// These are Certificate Trust List properties
//

//
// Metabase property that defines whether to use DS mapper or not
//

  MD_SSL_USE_DS_MAPPER = (IIS_MD_SSL_BASE+19);

  MD_SSL_ALWAYS_NEGO_CLIENT_CERT = (IIS_MD_SSL_BASE+21);

//
// Metabase properties that are used by the CertWiz ActiveX control, that
// is used for the Certificate/CTL UI management tool
//

//
// Metabase properties used for Fortezza certificates
//

//
// Metabase properties that are used by the CertWiz ActiveX control to keep
// track of the user's entry history, and whether DEBUG is enabled.  We keep
// these private properties on a per VS basis.
//

//  File and Directory related properties - these should be added in the
//  metabase with a user type of IIS_MD_UT_FILE
//

  IIS_MD_FILE_PROP_BASE = 6000;

  MD_AUTHORIZATION              = (IIS_MD_FILE_PROP_BASE);
  MD_REALM                      = (IIS_MD_FILE_PROP_BASE+1);
  MD_HTTP_EXPIRES               = (IIS_MD_FILE_PROP_BASE+2);
  MD_HTTP_PICS                  = (IIS_MD_FILE_PROP_BASE+3);
  MD_HTTP_CUSTOM                = (IIS_MD_FILE_PROP_BASE+4);
  MD_DIRECTORY_BROWSING         = (IIS_MD_FILE_PROP_BASE+5);
  MD_DEFAULT_LOAD_FILE          = (IIS_MD_FILE_PROP_BASE+6);
  MD_CUSTOM_ERROR               = (IIS_MD_FILE_PROP_BASE+8);
  MD_FOOTER_DOCUMENT            = (IIS_MD_FILE_PROP_BASE+9);
  MD_FOOTER_ENABLED             = (IIS_MD_FILE_PROP_BASE+10);
  MD_HTTP_REDIRECT              = (IIS_MD_FILE_PROP_BASE+11);
  MD_DEFAULT_LOGON_DOMAIN       = (IIS_MD_FILE_PROP_BASE+12);
  MD_LOGON_METHOD               = (IIS_MD_FILE_PROP_BASE+13);
  MD_SCRIPT_MAPS                = (IIS_MD_FILE_PROP_BASE+14);
  MD_MIME_MAP                   = (IIS_MD_FILE_PROP_BASE+15);
  MD_ACCESS_PERM                = (IIS_MD_FILE_PROP_BASE+16);
  MD_IP_SEC                     = (IIS_MD_FILE_PROP_BASE+19);
  MD_ANONYMOUS_USER_NAME        = (IIS_MD_FILE_PROP_BASE+20);
  MD_ANONYMOUS_PWD              = (IIS_MD_FILE_PROP_BASE+21);
  MD_ANONYMOUS_USE_SUBAUTH      = (IIS_MD_FILE_PROP_BASE+22);
  MD_DONT_LOG                   = (IIS_MD_FILE_PROP_BASE+23);
  MD_ADMIN_ACL                  = (IIS_MD_FILE_PROP_BASE+27);
  MD_SSI_EXEC_DISABLED          = (IIS_MD_FILE_PROP_BASE+28);
  MD_DO_REVERSE_DNS             = (IIS_MD_FILE_PROP_BASE+29);
  MD_SSL_ACCESS_PERM            = (IIS_MD_FILE_PROP_BASE+30);
  MD_AUTHORIZATION_PERSISTENCE  = (IIS_MD_FILE_PROP_BASE+31);
  MD_NTAUTHENTICATION_PROVIDERS = (IIS_MD_FILE_PROP_BASE+32);
  MD_SCRIPT_TIMEOUT             = (IIS_MD_FILE_PROP_BASE+33);
  MD_CACHE_EXTENSIONS           = (IIS_MD_FILE_PROP_BASE+34);
  MD_CREATE_PROCESS_AS_USER     = (IIS_MD_FILE_PROP_BASE+35);
  MD_CREATE_PROC_NEW_CONSOLE    = (IIS_MD_FILE_PROP_BASE+36);
  MD_POOL_IDC_TIMEOUT           = (IIS_MD_FILE_PROP_BASE+37);
  MD_ALLOW_KEEPALIVES           = (IIS_MD_FILE_PROP_BASE+38);
  MD_IS_CONTENT_INDEXED         = (IIS_MD_FILE_PROP_BASE+39);
  MD_CC_NO_CACHE                = (IIS_MD_FILE_PROP_BASE+41);
  MD_CC_MAX_AGE                 = (IIS_MD_FILE_PROP_BASE+42);
  MD_CC_OTHER                   = (IIS_MD_FILE_PROP_BASE+43);
  MD_REDIRECT_HEADERS           = (IIS_MD_FILE_PROP_BASE+44);
  MD_UPLOAD_READAHEAD_SIZE      = (IIS_MD_FILE_PROP_BASE+45);
  MD_PUT_READ_SIZE              = (IIS_MD_FILE_PROP_BASE+46);
  MD_USE_DIGEST_SSP             = (IIS_MD_FILE_PROP_BASE+47);

  MD_ENABLE_URL_AUTHORIZATION     = (IIS_MD_FILE_PROP_BASE+48);
  MD_URL_AUTHORIZATION_STORE_NAME = (IIS_MD_FILE_PROP_BASE+49);
  MD_URL_AUTHORIZATION_SCOPE_NAME = (IIS_MD_FILE_PROP_BASE+50);

  MD_MAX_REQUEST_ENTITY_ALLOWED   = (IIS_MD_FILE_PROP_BASE+51);

  MD_PASSPORT_REQUIRE_AD_MAPPING  = (IIS_MD_FILE_PROP_BASE+52);

  MD_URL_AUTHORIZATION_IMPERSONATION_LEVEL   = (IIS_MD_FILE_PROP_BASE+53);

  ASP_MD_SERVER_BASE = 7000;

  MD_ASP_BUFFERINGON               = (ASP_MD_SERVER_BASE + 0);
  MD_ASP_LOGERRORREQUESTS          = (ASP_MD_SERVER_BASE + 1);
  MD_ASP_SCRIPTERRORSSENTTOBROWSER = (ASP_MD_SERVER_BASE + 2);
  MD_ASP_SCRIPTERRORMESSAGE        = (ASP_MD_SERVER_BASE + 3);
  MD_ASP_SCRIPTFILECACHESIZE       = (ASP_MD_SERVER_BASE + 4);
  MD_ASP_SCRIPTENGINECACHEMAX      = (ASP_MD_SERVER_BASE + 5);
  MD_ASP_SCRIPTTIMEOUT             = (ASP_MD_SERVER_BASE + 6);
  MD_ASP_SESSIONTIMEOUT            = (ASP_MD_SERVER_BASE + 7);
  MD_ASP_ENABLEPARENTPATHS         = (ASP_MD_SERVER_BASE + 8);
  MD_ASP_MEMFREEFACTOR             = (ASP_MD_SERVER_BASE + 9); // OBSOLETE
  MD_ASP_MINUSEDBLOCKS             = (ASP_MD_SERVER_BASE + 10); // OBSOLETE
  MD_ASP_ALLOWSESSIONSTATE         = (ASP_MD_SERVER_BASE + 11);
  MD_ASP_SCRIPTLANGUAGE            = (ASP_MD_SERVER_BASE + 12);
  MD_ASP_QUEUETIMEOUT              = (ASP_MD_SERVER_BASE + 13);
  MD_ASP_ALLOWOUTOFPROCCOMPONENTS  = (ASP_MD_SERVER_BASE + 14);
  MD_ASP_ALLOWOUTOFPROCCMPNTS      = (MD_ASP_ALLOWOUTOFPROCCOMPONENTS); // Deprecated.  Use MD_ASP_ALLOWOUTOFPROCCMPNTS
  MD_ASP_EXCEPTIONCATCHENABLE      = (ASP_MD_SERVER_BASE + 15);
  MD_ASP_CODEPAGE                  = (ASP_MD_SERVER_BASE + 16);
  MD_ASP_SCRIPTLANGUAGELIST        = (ASP_MD_SERVER_BASE + 17);
  MD_ASP_ENABLESERVERDEBUG         = (ASP_MD_SERVER_BASE + 18);
  MD_ASP_ENABLECLIENTDEBUG         = (ASP_MD_SERVER_BASE + 19);
  MD_ASP_TRACKTHREADINGMODEL       = (ASP_MD_SERVER_BASE + 20);
// added for IIS 5.0
  MD_ASP_ENABLEASPHTMLFALLBACK    = (ASP_MD_SERVER_BASE + 21);
  MD_ASP_ENABLECHUNKEDENCODING    = (ASP_MD_SERVER_BASE + 22);
  MD_ASP_ENABLETYPELIBCACHE       = (ASP_MD_SERVER_BASE + 23);
  MD_ASP_ERRORSTONTLOG            = (ASP_MD_SERVER_BASE + 24);
  MD_ASP_PROCESSORTHREADMAX       = (ASP_MD_SERVER_BASE + 25);
  MD_ASP_REQEUSTQUEUEMAX          = (ASP_MD_SERVER_BASE + 26);
  MD_ASP_ENABLEAPPLICATIONRESTART = (ASP_MD_SERVER_BASE + 27);
  MD_ASP_QUEUECONNECTIONTESTTIME  = (ASP_MD_SERVER_BASE + 28);
  MD_ASP_SESSIONMAX               = (ASP_MD_SERVER_BASE + 29);

// thread gate
//  MD_ASP_THREADGATEENABLED    = (ASP_MD_SERVER_BASE + 30);
//  {$EXTERNALSYM MD_ASP_THREADGATEENABLED}
//  MD_ASP_THREADGATETIMESLICE  = (ASP_MD_SERVER_BASE + 31);
//  {$EXTERNALSYM MD_ASP_THREADGATETIMESLICE}
//  MD_ASP_THREADGATESLEEPDELAY = (ASP_MD_SERVER_BASE + 32);
//  {$EXTERNALSYM MD_ASP_THREADGATESLEEPDELAY}
//  MD_ASP_THREADGATESLEEPMAX   = (ASP_MD_SERVER_BASE + 33);
//  {$EXTERNALSYM MD_ASP_THREADGATESLEEPMAX}
//  MD_ASP_THREADGATELOADLOW    = (ASP_MD_SERVER_BASE + 34);
//  {$EXTERNALSYM MD_ASP_THREADGATELOADLOW}
//  MD_ASP_THREADGATELOADHIGH   = (ASP_MD_SERVER_BASE + 35);
//  {$EXTERNALSYM MD_ASP_THREADGATELOADHIGH}

// added IIS5.1

// persist template cache
  MD_ASP_DISKTEMPLATECACHEDIRECTORY = (ASP_MD_SERVER_BASE + 36);
  MD_ASP_MAXDISKTEMPLATECACHEFILES  = (ASP_MD_SERVER_BASE + 40);
  MD_ASP_EXECUTEINMTA               = (ASP_MD_SERVER_BASE + 41);
  MD_ASP_LCID                       = (ASP_MD_SERVER_BASE + 42);
  MD_ASP_KEEPSESSIONIDSECURE        = (ASP_MD_SERVER_BASE + 43);

// added IIS6.0

// Services without components integration
  MD_ASP_SERVICE_FLAGS           = (ASP_MD_SERVER_BASE + 44);
  MD_ASP_SERVICE_FLAG_TRACKER    = (ASP_MD_SERVER_BASE + 45);
  MD_ASP_SERVICE_FLAG_FUSION     = (ASP_MD_SERVER_BASE + 46);
  MD_ASP_SERVICE_FLAG_PARTITIONS = (ASP_MD_SERVER_BASE + 47);
  MD_ASP_SERVICE_PARTITION_ID    = (ASP_MD_SERVER_BASE + 48);
  MD_ASP_SERVICE_SXS_NAME        = (ASP_MD_SERVER_BASE + 49);

// Valid flags for MD_ASP_SERVICE_FLAGS property

  MD_ASP_SERVICE_ENABLE_TRACKER  = 1;
  MD_ASP_SERVICE_ENABLE_SXS      = 2;
  MD_ASP_SERVICE_USE_PARTITION   = 4;

// Line number calculation flag.

  MD_ASP_CALCLINENUMBER             = (ASP_MD_SERVER_BASE + 50);

  MD_ASP_RUN_ONEND_ANON             = (ASP_MD_SERVER_BASE + 51);

  MD_ASP_BUFFER_LIMIT               = (ASP_MD_SERVER_BASE + 52);

  MD_ASP_MAX_REQUEST_ENTITY_ALLOWED = (ASP_MD_SERVER_BASE + 53);

  MD_ASP_ID_LAST                    = (ASP_MD_SERVER_BASE + 53);

//
//  Valid values for WAM
//

  WAM_MD_SERVER_BASE = 7500;

  MD_WAM_USER_NAME = (WAM_MD_SERVER_BASE+1);
  MD_WAM_PWD       = (WAM_MD_SERVER_BASE+2);

// added IIS6

//
//  Valid values for APP POOL
//

  IIS_MD_APPPOOL_BASE = 9000;

  MD_APPPOOL_PERIODIC_RESTART_TIME          = (IIS_MD_APPPOOL_BASE + 1);
  MD_APPPOOL_PERIODIC_RESTART_REQUEST_COUNT = (IIS_MD_APPPOOL_BASE + 2);
  MD_APPPOOL_MAX_PROCESS_COUNT              = (IIS_MD_APPPOOL_BASE + 3);
  MD_APPPOOL_PINGING_ENABLED                = (IIS_MD_APPPOOL_BASE + 4);
  MD_APPPOOL_IDLE_TIMEOUT                   = (IIS_MD_APPPOOL_BASE + 5);
  MD_APPPOOL_RAPID_FAIL_PROTECTION_ENABLED  = (IIS_MD_APPPOOL_BASE + 6);
  MD_APPPOOL_SMP_AFFINITIZED                = (IIS_MD_APPPOOL_BASE + 7);
  MD_APPPOOL_SMP_AFFINITIZED_PROCESSOR_MASK = (IIS_MD_APPPOOL_BASE + 8);
  MD_APPPOOL_ORPHAN_PROCESSES_FOR_DEBUGGING = (IIS_MD_APPPOOL_BASE + 9);

  //MD_APPPOOL_RUN_AS_LOCALSYSTEM                 = (IIS_MD_APPPOOL_BASE + 10);
  //{$EXTERNALSYM MD_APPPOOL_RUN_AS_LOCALSYSTEM}
  MD_APPPOOL_STARTUP_TIMELIMIT                  = (IIS_MD_APPPOOL_BASE + 11);
  MD_APPPOOL_SHUTDOWN_TIMELIMIT                 = (IIS_MD_APPPOOL_BASE + 12);
  MD_APPPOOL_PING_INTERVAL                      = (IIS_MD_APPPOOL_BASE + 13);
  MD_APPPOOL_PING_RESPONSE_TIMELIMIT            = (IIS_MD_APPPOOL_BASE + 14);
  MD_APPPOOL_DISALLOW_OVERLAPPING_ROTATION      = (IIS_MD_APPPOOL_BASE + 15);
  //MD_APPPOOL_ORPHAN_ACTION                      = (IIS_MD_APPPOOL_BASE + 16);
  //{$EXTERNALSYM MD_APPPOOL_ORPHAN_ACTION}
  MD_APPPOOL_UL_APPPOOL_QUEUE_LENGTH            = (IIS_MD_APPPOOL_BASE + 17);
  MD_APPPOOL_DISALLOW_ROTATION_ON_CONFIG_CHANGE = (IIS_MD_APPPOOL_BASE + 18);
  //MD_APPPOOL_FRIENDLY_NAME                      = (IIS_MD_APPPOOL_BASE + 19);
  //{$EXTERNALSYM MD_APPPOOL_FRIENDLY_NAME}
  MD_APPPOOL_PERIODIC_RESTART_SCHEDULE          = (IIS_MD_APPPOOL_BASE + 20);
  MD_APPPOOL_IDENTITY_TYPE                      = (IIS_MD_APPPOOL_BASE + 21);
  MD_CPU_ACTION                                 = (IIS_MD_APPPOOL_BASE + 22);
  MD_CPU_LIMIT                                  = (IIS_MD_APPPOOL_BASE + 23);
  MD_APPPOOL_PERIODIC_RESTART_MEMORY            = (IIS_MD_APPPOOL_BASE + 24);
  //MD_DISABLE_PUBLISHING                         = (IIS_MD_APPPOOL_BASE + 25);
  //{$EXTERNALSYM MD_DISABLE_PUBLISHING}
  MD_APPPOOL_COMMAND                            = (IIS_MD_APPPOOL_BASE + 26);
  MD_APPPOOL_STATE                              = (IIS_MD_APPPOOL_BASE + 27);
  MD_APPPOOL_AUTO_START                         = (IIS_MD_APPPOOL_BASE + 28);
  MD_RAPID_FAIL_PROTECTION_INTERVAL             = (IIS_MD_APPPOOL_BASE + 29);
  MD_RAPID_FAIL_PROTECTION_MAX_CRASHES          = (IIS_MD_APPPOOL_BASE + 30);
  MD_APPPOOL_ORPHAN_ACTION_EXE                  = (IIS_MD_APPPOOL_BASE + 31);
  MD_APPPOOL_ORPHAN_ACTION_PARAMS               = (IIS_MD_APPPOOL_BASE + 32);

//
// Load balancer properties
//

  MD_LOAD_BALANCER_CAPABILITIES                 = (IIS_MD_APPPOOL_BASE + 34);

//
//  Valid values for APP POOL
//

  MD_APPPOOL_AUTO_SHUTDOWN_EXE                = (IIS_MD_APPPOOL_BASE + 35);
  MD_APPPOOL_AUTO_SHUTDOWN_PARAMS             = (IIS_MD_APPPOOL_BASE + 36);
  MD_APP_POOL_LOG_EVENT_ON_RECYCLE            = (IIS_MD_APPPOOL_BASE + 37);
  MD_APPPOOL_PERIODIC_RESTART_PRIVATE_MEMORY  = (IIS_MD_APPPOOL_BASE + 38);

//
// Valid values for MD_APP_POOL_LOG_EVENT_ON_RECYCLE
//

  MD_APP_POOL_RECYCLE_TIME                    = 1;
  MD_APP_POOL_RECYCLE_REQUESTS                = 2;
  MD_APP_POOL_RECYCLE_SCHEDULE                = 4;
  MD_APP_POOL_RECYCLE_MEMORY                  = 8;
  MD_APP_POOL_RECYCLE_ISAPI_UNHEALTHY         = 16;
  MD_APP_POOL_RECYCLE_ON_DEMAND               = 32;
  MD_APP_POOL_RECYCLE_CONFIG_CHANGE           = 64;
  MD_APP_POOL_RECYCLE_PRIVATE_MEMORY          = 128;

//
// Valid values for MD_CPU_ACTION
//

  MD_CPU_NO_ACTION = 0;
  MD_CPU_KILL_W3WP = 1;
  MD_CPU_TRACE     = 2;
  MD_CPU_THROTTLE  = 3;

//
// Valid values for MD_APPPOOL_COMMAND
//

  MD_APPPOOL_COMMAND_START = 1;
  MD_APPPOOL_COMMAND_STOP  = 2;

//
// Valid values for MD_APPPOOL_STATE
//

  MD_APPPOOL_STATE_STARTING = 1;
  MD_APPPOOL_STATE_STARTED  = 2;
  MD_APPPOOL_STATE_STOPPING = 3;
  MD_APPPOOL_STATE_STOPPED  = 4;

//
// Valid values for MD_APPPOOL_IDENTITY_TYPE
//

  MD_APPPOOL_IDENTITY_TYPE_LOCALSYSTEM        = 0;
  MD_APPPOOL_IDENTITY_TYPE_LOCALSERVICE       = 1;
  MD_APPPOOL_IDENTITY_TYPE_NETWORKSERVICE     = 2;
  MD_APPPOOL_IDENTITY_TYPE_SPECIFICUSER       = 3;

//
// Valid values for MD_LOAD_BALANCER_CAPABILITIES
//

  MD_LOAD_BALANCER_CAPABILITIES_BASIC         = 1;
  MD_LOAD_BALANCER_CAPABILITIES_SOPHISTICATED = 2;

//
// Valid values for MD_APPPOOL_STATE
//

  IIS_MD_APP_BASE                         = 9100;
  MD_APP_APPPOOL_ID                       = (IIS_MD_APP_BASE+1);
  MD_APP_ALLOW_TRANSIENT_REGISTRATION     = (IIS_MD_APP_BASE+2);
  MD_APP_AUTO_START                       = (IIS_MD_APP_BASE+3);
  MD_APPPOOL_PERIODIC_RESTART_CONNECTIONS = (IIS_MD_APP_BASE+4);

//
// TODO: These are duplicate definitions. Remove them if no one is using it.
//

  MD_APPPOOL_APPPOOL_ID                   = (IIS_MD_APP_BASE + 101);
  MD_APPPOOL_ALLOW_TRANSIENT_REGISTRATION = (IIS_MD_APP_BASE + 102);
// commented out so we can build
//#define MD_APPPOOL_AUTO_START                         (IIS_MD_APP_BASE + 103)


  IIS_MD_GLOBAL_BASE                            = 9200;
  MD_MAX_GLOBAL_BANDWIDTH                       = (IIS_MD_GLOBAL_BASE+1);
  MD_GLOBAL_STANDARD_APP_MODE_ENABLED           = (IIS_MD_GLOBAL_BASE+3);
  MD_HEADER_WAIT_TIMEOUT                        = (IIS_MD_GLOBAL_BASE+4);
  MD_MIN_FILE_BYTES_PER_SEC                     = (IIS_MD_GLOBAL_BASE+5);
  MD_GLOBAL_LOG_IN_UTF_8                        = (IIS_MD_GLOBAL_BASE+6);
  MD_DEMAND_START_THRESHOLD                     = (IIS_MD_GLOBAL_BASE+7);

  MD_GLOBAL_SESSIONKEY                              = 9999;
  MD_ROOT_ENABLE_EDIT_WHILE_RUNNING                 = 9998;
  MD_GLOBAL_CHANGE_NUMBER                           = 9997;
  MD_ROOT_ENABLE_HISTORY                            = 9996;
  MD_ROOT_MAX_HISTORY_FILES                         = 9995;
  MD_GLOBAL_EDIT_WHILE_RUNNING_MAJOR_VERSION_NUMBER = 9994;
  MD_GLOBAL_EDIT_WHILE_RUNNING_MINOR_VERSION_NUMBER = 9993;
  MD_GLOBAL_XMLSCHEMATIMESTAMP                      = 9992;
  MD_GLOBAL_BINSCHEMATIMESTAMP                      = 9991;
  MD_COMMENTS                                       = 9990;
  MD_LOCATION                                       = 9989;
  MD_MAX_ERROR_FILES                                = 9988;

//
//  Valid values for MD_AUTHORIZATION
//

  MD_AUTH_ANONYMOUS = $00000001;
  MD_AUTH_BASIC     = $00000002;
  MD_AUTH_NT        = $00000004; // Use NT auth provider (like NTLM)
  MD_AUTH_PASSPORT  = $00000040;

//
//  Valid values for MD_AUTHORIZATION_PERSISTENCE
//


  MD_AUTH_SINGLEREQUEST              = $00000040;
  MD_AUTH_SINGLEREQUESTIFPROXY       = $00000080;
  MD_AUTH_SINGLEREQUESTALWAYSIFPROXY = $00000100;

//
//  Valid values for MD_ACCESS_PERM
//

  MD_ACCESS_READ              = $00000001; // Allow for Read
  MD_ACCESS_WRITE             = $00000002; // Allow for Write
  MD_ACCESS_EXECUTE           = $00000004; // Allow for Execute
  MD_ACCESS_SOURCE            = $00000010; // Apply access mask to source
  MD_ACCESS_SCRIPT            = $00000200; // Allow for Script execution
  MD_ACCESS_NO_REMOTE_WRITE   = $00000400; // Local host access only
  MD_ACCESS_NO_REMOTE_READ    = $00001000; // Local host access only
  MD_ACCESS_NO_REMOTE_EXECUTE = $00002000; // Local host access only
  MD_ACCESS_NO_REMOTE_SCRIPT  = $00004000; // Local host access only
  MD_ACCESS_NO_PHYSICAL_DIR   = $00008000; // VR maps to no physical path

  MD_NONSSL_ACCESS_MASK = (MD_ACCESS_READ or MD_ACCESS_WRITE or MD_ACCESS_EXECUTE or
    MD_ACCESS_SOURCE or MD_ACCESS_SCRIPT or MD_ACCESS_NO_REMOTE_READ or
    MD_ACCESS_NO_REMOTE_WRITE or MD_ACCESS_NO_REMOTE_EXECUTE or MD_ACCESS_NO_REMOTE_SCRIPT or
    MD_ACCESS_NO_PHYSICAL_DIR);

//
//  Valid values for MD_SSL_ACCESS_PERM
//

  MD_ACCESS_SSL          = $00000008; // Require SSL
  MD_ACCESS_NEGO_CERT    = $00000020; // Allow client SSL certs
  MD_ACCESS_REQUIRE_CERT = $00000040; // Require client SSL certs
  MD_ACCESS_MAP_CERT     = $00000080; // Map SSL cert to NT account
  MD_ACCESS_SSL128       = $00000100; // Require 128 bit SSL

  MD_SSL_ACCESS_MASK = (MD_ACCESS_SSL or MD_ACCESS_NEGO_CERT or MD_ACCESS_REQUIRE_CERT or MD_ACCESS_MAP_CERT or MD_ACCESS_SSL128);

  MD_ACCESS_MASK = $0000ffff;

//
//  Valid values for MD_DIRECTORY_BROWSING
//

  MD_DIRBROW_SHOW_DATE      = $00000002;
  MD_DIRBROW_SHOW_TIME      = $00000004;
  MD_DIRBROW_SHOW_SIZE      = $00000008;
  MD_DIRBROW_SHOW_EXTENSION = $00000010;
  MD_DIRBROW_LONG_DATE      = $00000020;

  MD_DIRBROW_ENABLED     = DWORD($80000000); // Allow directory browsing
  MD_DIRBROW_LOADDEFAULT = $40000000; // Load default doc if exists

  MD_DIRBROW_MASK = (MD_DIRBROW_SHOW_DATE or MD_DIRBROW_SHOW_TIME or MD_DIRBROW_SHOW_SIZE or
    MD_DIRBROW_SHOW_EXTENSION or MD_DIRBROW_LONG_DATE or MD_DIRBROW_LOADDEFAULT or MD_DIRBROW_ENABLED);

//
//  Valid values for MD_LOGON_METHOD
//

  MD_LOGON_INTERACTIVE       = 0;
  MD_LOGON_BATCH             = 1;
  MD_LOGON_NETWORK           = 2;
  MD_LOGON_NETWORK_CLEARTEXT = 3;

//
//  Valid values for MD_PASSPORT_REQUIRE_AD_MAPPING
//

  MD_PASSPORT_NO_MAPPING    = 0;
  MD_PASSPORT_TRY_MAPPING   = 1;
  MD_PASSPORT_NEED_MAPPING  = 2;

//
// Valid values for MD_NOTIFY_EXAUTH
//

  MD_NOTIFEXAUTH_NTLMSSL = 1;

//
//  Valid values for MD_FILTER_STATE
//

  MD_FILTER_STATE_LOADED   = 1;
  MD_FILTER_STATE_UNLOADED = 4;

//
//  Valid values for MD_SERVER_STATE
//

  MD_SERVER_STATE_STARTING   = 1;
  MD_SERVER_STATE_STARTED    = 2;
  MD_SERVER_STATE_STOPPING   = 3;
  MD_SERVER_STATE_STOPPED    = 4;
  MD_SERVER_STATE_PAUSING    = 5;
  MD_SERVER_STATE_PAUSED     = 6;
  MD_SERVER_STATE_CONTINUING = 7;

//
//  Valid values for MD_SERVER_COMMAND
//

  MD_SERVER_COMMAND_START    = 1;
  MD_SERVER_COMMAND_STOP     = 2;
  MD_SERVER_COMMAND_PAUSE    = 3;
  MD_SERVER_COMMAND_CONTINUE = 4;

//
//  Valid values for MD_SERVER_SIZE
//

  MD_SERVER_SIZE_SMALL  = 0;
  MD_SERVER_SIZE_MEDIUM = 1;
  MD_SERVER_SIZE_LARGE  = 2;

//
// Valid values for MD_SERVER_CONFIG_INFO
//

  MD_SERVER_CONFIG_SSL_40        = $00000001;
  MD_SERVER_CONFIG_SSL_128       = $00000002;
  MD_SERVER_CONFIG_ALLOW_ENCRYPT = $00000004;
  MD_SERVER_CONFIG_AUTO_PW_SYNC  = $00000008;

  // todo MD_SERVER_CONFIG_ENCRYPT unknown!
  //MD_SERVER_CONFIGURATION_MASK = (MD_SERVER_CONFIG_SSL_40 or MD_SERVER_CONFIG_SSL_128 or MD_SERVER_CONFIG_ENCRYPT or MD_SERVER_CONFIG_AUTO_PW_SYNC);
  //{$EXTERNALSYM MD_SERVER_CONFIGURATION_MASK}

//
// Valid values for MD_SCRIPT_MAPS flag field
//

  MD_SCRIPTMAPFLAG_SCRIPT          = $00000001;
  MD_SCRIPTMAPFLAG_CHECK_PATH_INFO = $00000004;

{$IFDEF REMOVE}   // SteveBr

//
//  Bogus value - do not use
//

  MD_SCRIPTMAPFLAG_ALLOWED_ON_READ_DIR = $00000001;

{$ENDIF}

//
// Valid values for MD_AUTH_CHANGE_ENABLE
//

  MD_AUTH_CHANGE_UNSECURE   = $00000001;
  MD_AUTH_CHANGE_DISABLE    = $00000002;
  MD_AUTH_ADVNOTIFY_DISABLE = $00000004;

//
//  Valide substatus errors for MD_CUSTOM_ERROR
//

  MD_ERROR_SUB401_LOGON        = 1;
  MD_ERROR_SUB401_LOGON_CONFIG = 2;
  MD_ERROR_SUB401_LOGON_ACL    = 3;
  MD_ERROR_SUB401_FILTER       = 4;
  MD_ERROR_SUB401_APPLICATION  = 5;
  MD_ERROR_SUB401_URLAUTH_POLICY = 7;

  MD_ERROR_SUB403_EXECUTE_ACCESS_DENIED = 1;
  MD_ERROR_SUB403_READ_ACCESS_DENIED    = 2;
  MD_ERROR_SUB403_WRITE_ACCESS_DENIED   = 3;
  MD_ERROR_SUB403_SSL_REQUIRED          = 4;
  MD_ERROR_SUB403_SSL128_REQUIRED       = 5;
  MD_ERROR_SUB403_ADDR_REJECT           = 6;
  MD_ERROR_SUB403_CERT_REQUIRED         = 7;
  MD_ERROR_SUB403_SITE_ACCESS_DENIED    = 8;
  MD_ERROR_SUB403_TOO_MANY_USERS        = 9;
  MD_ERROR_SUB403_INVALID_CNFG          = 10;
  MD_ERROR_SUB403_PWD_CHANGE            = 11;
  MD_ERROR_SUB403_MAPPER_DENY_ACCESS    = 12;
  MD_ERROR_SUB403_CERT_REVOKED          = 13;
  MD_ERROR_SUB403_DIR_LIST_DENIED       = 14;
  MD_ERROR_SUB403_CAL_EXCEEDED          = 15;
  MD_ERROR_SUB403_CERT_BAD              = 16;
  MD_ERROR_SUB403_CERT_TIME_INVALID     = 17;
  MD_ERROR_SUB403_APPPOOL_DENIED        = 18;
  MD_ERROR_SUB403_INSUFFICIENT_PRIVILEGE_FOR_CGI = 19;
  MD_ERROR_SUB403_PASSPORT_LOGIN_FAILURE = 20;

  MD_ERROR_SUB404_SITE_NOT_FOUND       = 1;
  MD_ERROR_SUB404_DENIED_BY_POLICY     = 2;
  MD_ERROR_SUB404_DENIED_BY_MIMEMAP    = 3;

  MD_ERROR_SUB500_UNC_ACCESS           = 16;
  MD_ERROR_SUB500_URLAUTH_NO_STORE     = 17;
  MD_ERROR_SUB500_URLAUTH_STORE_ERROR  = 18;
  MD_ERROR_SUB500_BAD_METADATA         = 19;
  MD_ERROR_SUB500_URLAUTH_NO_SCOPE     = 20;

  MD_ERROR_SUB502_TIMEOUT        = 1;
  MD_ERROR_SUB502_PREMATURE_EXIT = 2;

  MD_ERROR_SUB503_CPU_LIMIT = 1;

//
// Valid access rights for ACE entries in MD_ADMIN_ACL
//

  MD_ACR_READ                = $00000001;
  MD_ACR_WRITE               = $00000002;
  MD_ACR_RESTRICTED_WRITE    = $00000020;
  MD_ACR_UNSECURE_PROPS_READ = $00000080;
  MD_ACR_ENUM_KEYS           = $00000008;
  MD_ACR_WRITE_DAC           = $00040000;

//
// Valid modes for MD_USER_ISOLATION
//

  MD_USER_ISOLATION_NONE    = 0;
  MD_USER_ISOLATION_BASIC   = 1;
  MD_USER_ISOLATION_AD      = 2;
  MD_USER_ISOLATION_LAST    = 2;

//
// MD_IP_SEC binary format description
//

(*

  This object is composed of 4 lists : 2 lists ( deny & grant ) of network addresses,
  the only allowed family is AF_INET.
  Each of this list is composed of sublists, one for each ( network address family,
  significant subnet mask ) combination. The significant subnet mask is stored as
  ( number of bytes all 1 ( 0xff ), bitmask in last byte ).
  This is followed by 2 lists ( deny & grant ) of DNS names. Each of these lists is
  composed of sublists, based on then number of components in the DNS name
  e.g. "microsoft.com" has 2 components, "www.msft.com" has 3.

Header:
    SELFREFINDEX    iDenyAddr;      // address deny list
                                    // points to ADDRESS_HEADER
    SELFREFINDEX    iGrantAddr;     // address grant list
                                    // points to ADDRESS_HEADER
    SELFREFINDEX    iDenyName;      // DNS name deny list
                                    // points to NAME_HEADER
    SELFREFINDEX    iGrantName;     // DNS name grant list
                                    // points to NAME_HEADER
    DWORD           dwFlags;
    DWORD           cRefSize;       // size of reference area ( in bytes )

ADDRESS_HEADER :
    DWORD               cEntries;   // # of Entries[]
    DWORD               cAddresses; // total # of addresses in all
                                    // ADDRESS_LIST_ENTRY
    ADDRESS_LIST_ENTRY  Entries[];

ADDRESS_LIST_ENTRY :
    DWORD           iFamily;
    DWORD           cAddresses;
    DWORD           cFullBytes;
    DWORD           LastByte;
    SELFREFINDEX    iFirstAddress;  // points to array of addresses

NAME_HEADER :
    DWORD           cEntries;
    DWORD           cNames;         // total # of names for all Entries[]
    NAME_LIST_ENTRY Entries[];

Name list entry :
    DWORD           cComponents;    // # of DNS components
    DWORD           cNames;
    SELFREFINDEX    iName[];        // array of references to DNS names

This is followed by address arrays & names pointed to by iFirstAddress & iName
Names are '\0' delimited

SELFREFINDEX is a DWORD offset from start of structure with high bit set to 1

*)

//
// Macros
//

(* todo
#define MD_SET_DATA_RECORD(_pMDR, _id, _attr, _utype, _dtype, _dlen, _pData) \
            { \
            (_pMDR)->dwMDIdentifier=(_id);      \
            (_pMDR)->dwMDAttributes=(_attr);    \
            (_pMDR)->dwMDUserType=(_utype);     \
            (_pMDR)->dwMDDataType=(_dtype);     \
            (_pMDR)->dwMDDataLen=(_dlen);       \
            (_pMDR)->pbMDData=(LPBYTE)(_pData); \
            }
*)

//
// IIS ADSI Admin Object class names
//

  IIS_CLASS_COMPUTER         = 'IIsComputer';
  IIS_CLASS_WEB_SERVICE      = 'IIsWebService';
  IIS_CLASS_WEB_SERVER       = 'IIsWebServer';
  IIS_CLASS_WEB_INFO         = 'IIsWebInfo';
  IIS_CLASS_WEB_DIR          = 'IIsWebDirectory';
  IIS_CLASS_WEB_VDIR         = 'IIsWebVirtualDir';
  IIS_CLASS_WEB_FILE         = 'IIsWebFile';
  IIS_CLASS_FTP_SERVICE      = 'IIsFtpService';
  IIS_CLASS_FTP_SERVER       = 'IIsFtpServer';
  IIS_CLASS_FTP_INFO         = 'IIsFtpInfo';
  IIS_CLASS_FTP_VDIR         = 'IIsFtpVirtualDir';
  IIS_CLASS_FILTERS          = 'IIsFilters';
  IIS_CLASS_FILTER           = 'IIsFilter';
  IIS_CLASS_LOG_MODULES      = 'IIsLogModules';
  IIS_CLASS_LOG_MODULE       = 'IIsLogModule';
  IIS_CLASS_MIMEMAP          = 'IIsMimeMap';
  IIS_CLASS_CERTMAPPER       = 'IIsCertMapper';
  IIS_CLASS_COMPRESS_SCHEMES = 'IIsCompressionSchemes';
  IIS_CLASS_COMPRESS_SCHEME  = 'IIsCompressionScheme';

  IIS_CLASS_COMPUTER_W         = WideString('IIsComputer');
  IIS_CLASS_WEB_SERVICE_W      = WideString('IIsWebService');
  IIS_CLASS_WEB_SERVER_W       = WideString('IIsWebServer');
  IIS_CLASS_WEB_INFO_W         = WideString('IIsWebInfo');
  IIS_CLASS_WEB_DIR_W          = WideString('IIsWebDirectory');
  IIS_CLASS_WEB_VDIR_W         = WideString('IIsWebVirtualDir');
  IIS_CLASS_WEB_FILE_W         = WideString('IIsWebFile');
  IIS_CLASS_FTP_SERVICE_W      = WideString('IIsFtpService');
  IIS_CLASS_FTP_SERVER_W       = WideString('IIsFtpServer');
  IIS_CLASS_FTP_INFO_W         = WideString('IIsFtpInfo');
  IIS_CLASS_FTP_VDIR_W         = WideString('IIsFtpVirtualDir');
  IIS_CLASS_FILTERS_W          = WideString('IIsFilters');
  IIS_CLASS_FILTER_W           = WideString('IIsFilter');
  IIS_CLASS_LOG_MODULES_W      = WideString('IIsLogModules');
  IIS_CLASS_LOG_MODULE_W       = WideString('IIsLogModule');
  IIS_CLASS_MIMEMAP_W          = WideString('IIsMimeMap');
  IIS_CLASS_CERTMAPPER_W       = WideString('IIsCertMapper');
  IIS_CLASS_COMPRESS_SCHEMES_W = WideString('IIsCompressionSchemes');
  IIS_CLASS_COMPRESS_SCHEME_W  = WideString('IIsCompressionScheme');

implementation

end.

