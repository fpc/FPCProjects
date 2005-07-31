{******************************************************************************}
{                                                       	               }
{ Internet Authentication Extensions API interface Unit for Object Pascal      }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: authif.h, released June 2000. The original Pascal      }
{ code is: Authif.pas, released December 2000. The initial developer of the    }
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

unit JwaAuthif;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "authif.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//
//  Enumerates the attribute types that are passed to the extension DLL.  The
//  RADIUS standard attributes are included for convenience and should not be
//  considered exhaustive.
//

type
  _RADIUS_ATTRIBUTE_TYPE = DWORD;
  RADIUS_ATTRIBUTE_TYPE = _RADIUS_ATTRIBUTE_TYPE;
  TRadiusAttributeType = RADIUS_ATTRIBUTE_TYPE;

const
  // Used to terminate attribute arrays.

  ratMinimum = 0;

  // RADIUS standard attributes.

  ratUserName = 1;
  ratUserPassword = 2;
  ratCHAPPassword = 3;
  ratNASIPAddress = 4;
  ratNASPort = 5;
  ratServiceType = 6;
  ratFramedProtocol = 7;
  ratFramedIPAddress = 8;
  ratFramedIPNetmask = 9;
  ratFramedRouting = 10;
  ratFilterId = 11;
  ratFramedMTU = 12;
  ratFramedCompression = 13;
  ratLoginIPHost = 14;
  ratLoginService = 15;
  ratLoginPort = 16;
  ratReplyMessage = 18;
  ratCallbackNumber = 19;
  ratCallbackId = 20;
  ratFramedRoute = 22;
  ratFramedIPXNetwork = 23;
  ratState = 24;
  ratClass = 25;
  ratVendorSpecific = 26;
  ratSessionTimeout = 27;
  ratIdleTimeout = 28;
  ratTerminationAction = 29;
  ratCalledStationId = 30;
  ratCallingStationId = 31;
  ratNASIdentifier = 32;
  ratProxyState = 33;
  ratLoginLATService = 34;
  ratLoginLATNode = 35;
  ratLoginLATGroup = 36;
  ratFramedAppleTalkLink = 37;
  ratFramedAppleTalkNetwork = 38;
  ratFramedAppleTalkZone = 39;
  ratAcctStatusType = 40;
  ratAcctDelayTime = 41;
  ratAcctInputOctets = 42;
  ratAcctOutputOctets = 43;
  ratAcctSessionId = 44;
  ratAcctAuthentic = 45;
  ratAcctSessionTime = 46;
  ratAcctInputPackets = 47;
  ratAcctOutputPackets = 48;
  ratAcctTerminationCause = 49;
  ratCHAPChallenge = 60;
  ratNASPortType = 61;
  ratPortLimit = 62;

  // Extended attribute types used to pass additional information.

  ratCode = 262;             // Request type code.
  ratIdentifier = 263;       // Request identifier.
  ratAuthenticator = 264;    // Request authenticator.
  ratSrcIPAddress = 265;     // Source IP address.
  ratSrcPort = 266;          // Source IP port.
  ratProvider = 267;         // Authentication provider.
  ratStrippedUserName = 268; // User-Name with realm stripped.
  ratFQUserName = 269;       // Fully-Qualified-User-Name.
  ratPolicyName = 270;       // Remote Access Policy name.
  ratUniqueId   = 271;       // Unique ID identifying the request.
  ratExtensionState = 272;   // Used to pass state between extensions.

//
//  Enumerates the different RADIUS packet codes. Used for the ratCode extended
//  attribute.
//

type
  _RADIUS_CODE = DWORD;
  RADIUS_CODE = _RADIUS_CODE;
  TRadiusCode = RADIUS_CODE;

const
  rcUnknown = 0;
  rcAccessRequest = 1;
  rcAccessAccept = 2;
  rcAccessReject = 3;
  rcAccountingRequest = 4;
  rcAccountingResponse = 5;
  rcAccessChallenge = 11;
  rcDiscard = 256;

//
//  Enumerates the different authentication providers used for processing a
//  request. Used for the ratProvider extended attribute.
//

type
  _RADIUS_AUTHENTICATION_PROVIDER = (
    rapUnknown,
    rapUsersFile,
    rapProxy,
    rapWindowsNT,
    rapMCIS,
    rapODBC,
    rapNone);
  RADIUS_AUTHENTICATION_PROVIDER = _RADIUS_AUTHENTICATION_PROVIDER;
  TRadiusAuthenticationProvider = RADIUS_AUTHENTICATION_PROVIDER;

//
//  Enumerates the different RADIUS data types. A type of 'rdtUnknown' means
//  the attribute was not recognized by the dictionary.
//

  _RADIUS_DATA_TYPE = (
   rdtUnknown,
   rdtString,
   rdtAddress,
   rdtInteger,
   rdtTime);
  RADIUS_DATA_TYPE = _RADIUS_DATA_TYPE;
  TRadiusDataType = RADIUS_DATA_TYPE;

//
//  Struct representing a RADIUS or extended attribute.
//

  _RADIUS_ATTRIBUTE  = record
    dwAttrType: DWORD;            // Attribute type
    fDataType: RADIUS_DATA_TYPE;  // RADIUS_DATA_TYPE of the value
    cbDataLength: DWORD;          // Length of the value (in bytes)
    case Integer of
      0: (dwValue: DWORD);        // For rdtAddress, rdtInteger, and rdtTime
      1: (lpValue: PCSTR);        // For rdtUnknown, and rdtString
  end;
  RADIUS_ATTRIBUTE = _RADIUS_ATTRIBUTE;
  PRADIUS_ATTRIBUTE = ^RADIUS_ATTRIBUTE;
  TRadiusAttribute = RADIUS_ATTRIBUTE;
  PRadiusAttribute = PRADIUS_ATTRIBUTE;

//
//  Struct representing the layout of a RADIUS Vendor-Specific attribute. This
//  is useful when interpreting the RADIUS_ATTRIBUTE lpValue field when
//  dwAttrType is ratVendorSpecific.
//

  _RADIUS_VSA_FORMAT = record
   VendorId: array [0..3] of BYTE;
   VendorType: BYTE;
   VendorLength: BYTE;
   AttributeSpecific: array [0..0] of BYTE;
  end;
  RADIUS_VSA_FORMAT = _RADIUS_VSA_FORMAT;
  TRadiusVsaFormat = RADIUS_VSA_FORMAT;

//
//  Enumerates the different actions an extension DLL can generate in
//  response to an Access-Request.
//

  _RADIUS_ACTION = (
   raContinue,
   raReject,
   raAccept);
  RADIUS_ACTION = _RADIUS_ACTION;
  PRADIUS_ACTION = ^RADIUS_ACTION;
  TRadiusAction = RADIUS_ACTION;
  PRadiusAction = PRADIUS_ACTION;

//
// Routines exported by a RADIUS extension DLL.
//

//
// RadiusExtensionInit is optional. If it exists, it will be invoked prior to
// the service coming on-line. A return value other than NO_ERROR prevents the
// service from initializing.
//

const
  RADIUS_EXTENSION_INIT = 'RadiusExtensionInit';

type
  PRADIUS_EXTENSION_INIT = function: DWORD; stdcall;
  PRadiusExtensionInit = PRADIUS_EXTENSION_INIT;

//
// RadiusExtensionTerm is optional. If it exists, it will be invoked prior to
// unloading the DLL to give the extension a chance to clean-up.
//

const
  RADIUS_EXTENSION_TERM = 'RadiusExtensionTerm';

type
  PRADIUS_EXTENSION_TERM = procedure; stdcall;
  PRadiusExtensionTerm = PRADIUS_EXTENSION_TERM;

//
// RadiusExtensionProcess is mandatory for NT4. For Windows 2000, an
// extension may export RadiusExtensionProcessEx (q.v.) instead.
//
// Parameters:
//   pAttrs      Array of attributes from the request. It is terminated by an
//               attribute with dwAttrType set to ratMinimum. These attributes
//               should be treated as read-only and must not be referenced
//               after the function returns.
//   pfAction    For Access-Requests, this parameter will be non-NULL with
//               *pfAction == raContinue. The extension DLL can set *pfAction
//               to abort further processing and force an Access-Accept or
//               Access-Reject.  For all other request types, this parameter
//               will be NULL.
//
// Return Value:
//     A return value other than NO_ERROR causes the request to be discarded.
//

const
  RADIUS_EXTENSION_PROCESS = 'RadiusExtensionProcess';

type
  PRADIUS_EXTENSION_PROCESS = function (pAttrs: PRADIUS_ATTRIBUTE; pfAction: PRADIUS_ACTION): DWORD; stdcall;
  PRadiusExtensionProcess = PRADIUS_EXTENSION_PROCESS;

//
// RadiusExtensionProcessEx is only supported on Windows 2000. If it exits,
// RadiusExtensionProcess is ignored.
//
// Parameters:
//   pInAttrs    Array of attributes from the request. It is terminated by an
//               attribute with dwAttrType set to ratMinimum. These attributes
//               should be treated as read-only and must not be referenced
//               after the function returns.
//   pOutAttrs   Array of attributes to add to the response. It is terminated
//               by an attribute with dwAttrType set to ratMinimum.
//               *pOutAttrs may be set to NULL if no attributes are returned.
//   pfAction    For Access-Requests, this parameter will be non-NULL with
//               *pfAction == raContinue. The extension DLL can set *pfAction
//               to abort further processing and force an Access-Accept or
//               Access-Reject.  For all other request types, this parameter
//               will be NULL.
//
// Return Value:
//     A return value other than NO_ERROR causes the request to be discarded.
//

const
  RADIUS_EXTENSION_PROCESS_EX = 'RadiusExtensionProcessEx';

type
  PRADIUS_EXTENSION_PROCESS_EX = function (pInAttrs: PRADIUS_ATTRIBUTE;
    pOutAttrs: PRADIUS_ATTRIBUTE; pfAction: PRADIUS_ACTION): DWORD; stdcall;
  PRadiusExtensionProcessEx = PRADIUS_EXTENSION_PROCESS_EX;
  
//
// RadiusExtensionFreeAttributes must be defined if RadiusExtensionProcessEx
// is defined. It is used to free the attributes returned by
// RadiusExtensionProcessEx
//
// Parameters:
//   pAttrs     Array of attributes to be freed.
//

const
  RADIUS_EXTENSION_FREE_ATTRIBUTES = 'RadiusExtensionFreeAttributes';

type
  PRADIUS_EXTENSION_FREE_ATTRIBUTES = procedure (pAttrs: PRADIUS_ATTRIBUTE); stdcall;
  PRadiusExtensionFreeAttributes = PRADIUS_EXTENSION_FREE_ATTRIBUTES;

//
//  Defines used for installation of an extension DLL.
//  The following registry values are used for loading extensions:
//
//      HKLM\System\CurrentControlSet\Services\AuthSrv\Parameters
//          ExtensionDLLs      (REG_MULTI_SZ)  <list of DLL paths>
//          AuthorizationDLLs  (REG_MULTI_SZ)  <list of DLL paths>
//
//  ExtensionDLLs are invoked before any of the built-in authentication
//  providers. They receive all the attributes from the request plus all
//  the extended attribute types.
//
//  AuthorizationDLLs are invoked after the built-in authentication and
//  authorization providers. They receive all the attributes from the
//  response plus all the extended attributes types. AuthorizationDLLs may
//  not return an action of raAccept.
//

const
  AUTHSRV_PARAMETERS_KEY_W = WideString('System\CurrentControlSet\Services\AuthSrv\Parameters');

  AUTHSRV_EXTENSIONS_VALUE_W = WideString('ExtensionDLLs');

  AUTHSRV_AUTHORIZATION_VALUE_W = WideString('AuthorizationDLLs');

// #if _WIN32_WINNT >= 0x0501

// Version of this spec.

const
  RADIUS_EXTENSION_VERSION = 1;

//
//  Enumerates the different points during request processing where an
//  extension can be invoked.
//

type
  _RADIUS_EXTENSION_POINT = (
   repAuthentication,     // ExtensionDLLs
   repAuthorization);     // AuthorizationDLLs
  RADIUS_EXTENSION_POINT = _RADIUS_EXTENSION_POINT;
  TRadiusExtensionPoint = RADIUS_EXTENSION_POINT;

//
// Struct representing an array of RADIUS_ATTRIBUTE structs. All the functions
// for adding attributes to a request copy the supplied memory, so there is no
// need for the extension to export RadiusExtensionFreeAttributes.  The
// extension must not modify this struct. All changes must be made by using the
// supplied callback functions.
//

  PRADIUS_ATTRIBUTE_ARRAY = ^RADIUS_ATTRIBUTE_ARRAY;
  PPRADIUS_ATTRIBUTE_ARRAY = ^PRADIUS_ATTRIBUTE_ARRAY;
  _RADIUS_ATTRIBUTE_ARRAY = record

   // Size of this structure in bytes.

   cbSize: DWORD;

   // Adds a new attribute to the end of the array.

   Add: function (This: PRADIUS_ATTRIBUTE_ARRAY; pAttr: PRADIUS_ATTRIBUTE): DWORD; stdcall;

   //
   // Returns a const pointer to the specified attribute within the array or
   // NULL if the index is out of range.
   //

   AttributeAt: function (This: PPRADIUS_ATTRIBUTE_ARRAY; dwIndex: DWORD): PRADIUS_ATTRIBUTE; stdcall;

   //
   // Returns the size of the array. Since indexes are zero-based, the size is
   // 1 greater than the largest index.
   //

   GetSize: function (This: PRADIUS_ATTRIBUTE_ARRAY): DWORD; stdcall;

   //
   // Inserts a new attribute at a specified index in the array. In the
   // process, it shifts up (by incrementing the index) the existing attribute
   // at this index, and it shifts up all the attributes above it. Returns
   // ERROR_INVALID_PARAMETER if the index is out of range.
   //

   InsertAt: function (This: PRADIUS_ATTRIBUTE_ARRAY; dwIndex: DWORD; pAttr: PRADIUS_ATTRIBUTE): DWORD; stdcall;

   //
   // Removes the attribute at the specified index in the array. In the
   // process, it shifts down all the attributes above the removed attribute.
   // Returns ERROR_ACCESS_DENIED if the specified attribute is read-only.
   // Returns ERROR_INVALID_PARAMETER if the index is out of range.
   //

   RemoveAt: function (This: PRADIUS_ATTRIBUTE_ARRAY; dwIndex: DWORD): DWORD; stdcall;

   //
   // Sets the array element at the specified index, replacing the existing
   // attribute.  Returns ERROR_INVALID_PARAMETER if the index is out of range.
   //

   SetAt: function (This: PPRADIUS_ATTRIBUTE_ARRAY; dwIndex: DWORD; pAttr: PRADIUS_ATTRIBUTE): DWORD; stdcall;

  end;
  RADIUS_ATTRIBUTE_ARRAY = _RADIUS_ATTRIBUTE_ARRAY;
  TRadiusAttributeArray = RADIUS_ATTRIBUTE_ARRAY;
  PRadiusAttributeArray = PRADIUS_ATTRIBUTE_ARRAY;  

//
// Struct used to exchange information with the extension during request
// processing. The extension must not modify this struct. All changes must be
// made by using the supplied callback functions.
//

  PRADIUS_EXTENSION_CONTROL_BLOCK = ^RADIUS_EXTENSION_CONTROL_BLOCK;
  _RADIUS_EXTENSION_CONTROL_BLOCK = record

    // Size of this structure.

    cbSize: DWORD;

    // Version info of this specification.

    dwVersion: DWORD;

    // Point during request processing where the extension is being invoked.

    repPoint: RADIUS_EXTENSION_POINT;

    // Type of RADIUS request being processed.

    rcRequestType: RADIUS_CODE;

    //
    // Final disposition of the request. This field must not be modified
    // directly; use the SetResponseType callback function instead. At the
    // repAuthentication point, this may be set to rcUnknown to indicate that no
    // decision has been made yet.
    //

    rcResponseType: RADIUS_CODE;

    //
    // Returns the attributes received in the RADIUS request and any internal
    // attributes describing the request state. The extenstion can modify the
    // request attributes. For example, when IAS is acting as a RADIUS proxy, an
    // extension could filter which attributes are forwarded to a remote RADIUS
    // server.
    //

    GetRequest: function (This: PRADIUS_EXTENSION_CONTROL_BLOCK): PRADIUS_ATTRIBUTE_ARRAY; stdcall;

    //
    // Returns the attributes that will be sent in the response if the final
    // outcome of request processing matches the specified response type.
    // Returns NULL if rcResponseType is invalid. Note that an extension may
    // retrieve and modify the attributes for any valid response type regardless
    // of the request's current disposition. For example, an extension can set
    // the response type to rcAccessAccept, but still add attributes to the
    // Access-Reject in case the response type is overridden during further
    // processing.
    //

    GetResponse: function (This: PRADIUS_EXTENSION_CONTROL_BLOCK; rcResponseType: RADIUS_CODE): PRADIUS_ATTRIBUTE_ARRAY; stdcall;

    //
    // Sets the final disposition of the request.
    // Returns ERROR_INVALID_PARAMETER if the specified response type is invalid
    // for the request type.
    //

    SetResponseType: function (This: PRADIUS_EXTENSION_CONTROL_BLOCK; rcResponseType: RADIUS_CODE): DWORD; stdcall;
  end;
  RADIUS_EXTENSION_CONTROL_BLOCK = _RADIUS_EXTENSION_CONTROL_BLOCK;
  TRadiusExtensionControlBlock = RADIUS_EXTENSION_CONTROL_BLOCK;
  PRadiusExtensionControlBlock = PRADIUS_EXTENSION_CONTROL_BLOCK;

//
// If RadiusExtensionProcess2 exists, RadiusExtensionProcess and
// RadiusExtensionProcessEx are ignored.
//
// Parameters:
//   pECB      Info exchanged with the extension.
//
// Return Value:
//     A return value other than NO_ERROR causes the request to be discarded.
///

const
  RADIUS_EXTENSION_PROCESS2 = 'RadiusExtensionProcess2';

type
  PRADIUS_EXTENSION_PROCESS_2 = function (pECB: PRADIUS_EXTENSION_CONTROL_BLOCK): DWORD; stdcall;

implementation

end.
