{******************************************************************************}
{                                                       	               }
{ Winsock2 QOS Service Provider API interface Unit for Object Pascal           }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: qossp.h, released August 2001. The original Pascal     }
{ code is: QosSp.pas, released December 2000. The initial developer of the     }
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

unit JwaQosSp;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "qossp.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
{$IFDEF STANDALONE}
  JwaWinTypes,
{$ELSE}
  Windows,
{$ENDIF}
  JwaWinSock2, JwaQos;

(*
 * Definition of object Types
 *
 *
 * define the values for ObjectType above - RSVP Objects ids start at an
 * offset from zero to allow for ATM objects that might be defined in the
 * lower number range.
 *)

const
  RSVP_OBJECT_ID_BASE     = 1000;
  RSVP_OBJECT_STATUS_INFO = ($00000000 + RSVP_OBJECT_ID_BASE);
  (* RSVP_STATUS_INFO structure passed *)
  RSVP_OBJECT_RESERVE_INFO = ($00000001 + RSVP_OBJECT_ID_BASE);
  (* RSVP_RESERVE_INFO structure passed *)
  RSVP_OBJECT_ADSPEC = ($00000002 + RSVP_OBJECT_ID_BASE);
  (* RSVP_ADSPEC structure passed *)
  RSVP_OBJECT_POLICY_INFO = ($00000003 + RSVP_OBJECT_ID_BASE);
  (* RSVP POLICY ELEMENT(S) retrieved *)
  RSVP_OBJECT_FILTERSPEC_LIST = ($00000004 + RSVP_OBJECT_ID_BASE);
  (* RSVP SENDER LIST returned *)


(*
 * IPV4 addressing for RSVP FILTERSPECS
 *)

type
  _IN_ADDR_IPV4 = record
    Addr: ULONG;
    AddrBytes: array [0..3] of UCHAR;
  end;
  IN_ADDR_IPV4 = _IN_ADDR_IPV4;
  LPIN_ADDR_IPV4 = ^IN_ADDR_IPV4;
  TInAddrIPV4 = IN_ADDR_IPV4;
  PInAddrIPV4 = LPIN_ADDR_IPV4;

(*
 * IPV6 addressing for RSVP FILTERSPECS
 *)

  _IN_ADDR_IPV6 = record
    Addr: array [0..15] of UCHAR;               //IPV6 address
  end;
  IN_ADDR_IPV6 = _IN_ADDR_IPV6;
  LPIN_ADDR_IPV6 = ^IN_ADDR_IPV6;
  TInAddrIPV6 = IN_ADDR_IPV6;
  PInAddrIPV6 = LPIN_ADDR_IPV6;

  LPCIN_ADDR_IPV6 = ^IN_ADDR_IPV6;

(*
 * IPV4 addressing for RSVP FILTERSPECS
 *)

  LPRSVP_FILTERSPEC_V4 = ^RSVP_FILTERSPEC_V4;
  _RSVP_FILTERSPEC_V4 = record
    Address: IN_ADDR_IPV4;
    Unused: USHORT;
    Port: USHORT;
  end;
  RSVP_FILTERSPEC_V4 = _RSVP_FILTERSPEC_V4;
  TRsvpFilterSpecV4 = RSVP_FILTERSPEC_V4;
  PRsvpFilterSpecV4 = LPRSVP_FILTERSPEC_V4;

  LPRSVP_FILTERSPEC_V6 = ^RSVP_FILTERSPEC_V6;
  _RSVP_FILTERSPEC_V6 = record
    Address: IN_ADDR_IPV6;
    UnUsed: USHORT;
    Port: USHORT;
  end;
  RSVP_FILTERSPEC_V6 = _RSVP_FILTERSPEC_V6;
  TRsvpFilterSpecV6 = RSVP_FILTERSPEC_V6;
  PRsvpFilterSpecV6 = LPRSVP_FILTERSPEC_V6;

  LPRSVP_FILTERSPEC_V6_FLOW = ^RSVP_FILTERSPEC_V6_FLOW;
  _RSVP_FILTERSPEC_V6_FLOW = record
    Address: IN_ADDR_IPV6;
    UnUsed: UCHAR;
    FlowLabel: array [0..2] of UCHAR;
  end;
  RSVP_FILTERSPEC_V6_FLOW = _RSVP_FILTERSPEC_V6_FLOW;
  TRsvpFilterSpecV6Flow = RSVP_FILTERSPEC_V6_FLOW;
  PRsvpFilterSpecV6Flow = LPRSVP_FILTERSPEC_V6_FLOW;

  LPRSVP_FILTERSPEC_V4_GPI = ^RSVP_FILTERSPEC_V4_GPI;
  _RSVP_FILTERSPEC_V4_GPI = record
    Address: IN_ADDR_IPV4;
    GeneralPortId: ULONG;
  end;
  RSVP_FILTERSPEC_V4_GPI = _RSVP_FILTERSPEC_V4_GPI;
  TRsvpFilterSpecV4Gpi = RSVP_FILTERSPEC_V4_GPI;
  PRsvpFilterSpecV4Gpi = LPRSVP_FILTERSPEC_V4_GPI;

  LPRSVP_FILTERSPEC_V6_GPI = ^RSVP_FILTERSPEC_V6_GPI;
  _RSVP_FILTERSPEC_V6_GPI = record
    Address: IN_ADDR_IPV6;
    GeneralPortId: ULONG;
  end;
  RSVP_FILTERSPEC_V6_GPI = _RSVP_FILTERSPEC_V6_GPI;
  TRsvpFilterSpecV6Gpi = RSVP_FILTERSPEC_V6_GPI;
  PRsvpFilterSpecV6Gpi = LPRSVP_FILTERSPEC_V6_GPI;

(*
 * FILTERSPEC TYPES used in making reservations.
 *)

const
  FILTERSPECV4 = 1;
  FILTERSPECV6 = 2;
  FILTERSPECV6_FLOW = 3;
  FILTERSPECV4_GPI = 4;
  FILTERSPECV6_GPI = 5;
  FILTERSPEC_END = 6;

type
  FilterType = DWORD;

type
  LPRSVP_FILTERSPEC = ^RSVP_FILTERSPEC;
  _RSVP_FILTERSPEC = record
    Type_: FilterType;
    case Integer of
      0: (FilterSpecV4: RSVP_FILTERSPEC_V4);
      1: (FilterSpecV6: RSVP_FILTERSPEC_V6);
      2: (FilterSpecV6Flow: RSVP_FILTERSPEC_V6_FLOW);
      3: (FilterSpecV4Gpi: RSVP_FILTERSPEC_V4_GPI);
      4: (FilterSpecV6Gpi: RSVP_FILTERSPEC_V6_GPI);
  end;
  RSVP_FILTERSPEC = _RSVP_FILTERSPEC;
  TRsvpFilterSpec = RSVP_FILTERSPEC;
  PRsvpFilterSpec = LPRSVP_FILTERSPEC;

(*
 * FLOWDESCRIPTOR Structure used for specifying one or more
 * Filters per Flowspec.
 *)

  LPFLOWDESCRIPTOR = ^FLOWDESCRIPTOR;
  _FLOWDESCRIPTOR = record
    FlowSpec: FLOWSPEC;
    NumFilters: ULONG;
    FilterList: LPRSVP_FILTERSPEC;
  end;
  FLOWDESCRIPTOR = _FLOWDESCRIPTOR;
  TFlowDescriptor = FLOWDESCRIPTOR;
  PFlowDescriptor = LPFLOWDESCRIPTOR;

(*
 * RSVP_POLICY contains undefined policy data.  RSVP transports this
 * data on behalf of the Policy Control component.
 *)

  LPRSVP_POLICY = ^RSVP_POLICY;
  _RSVP_POLICY = record
    Len: USHORT;
    Type_: USHORT;
    Info: array [0..4 - 1] of UCHAR;
  end;
  RSVP_POLICY = _RSVP_POLICY;
  TRsvpPolicy = RSVP_POLICY;
  PRsvpPolicy = LPRSVP_POLICY;

  LPCRSVP_POLICY = ^RSVP_POLICY;

const
  RSVP_POLICY_HDR_LEN = SizeOf(USHORT) + SizeOf(USHORT);

(*
 * RSVP_POLICY_INFO contains undefined policy element(s) retrieved from RSVP.  
 *)

type
  LPRSVP_POLICY_INFO = ^RSVP_POLICY_INFO;
  _RSVP_POLICY_INFO = record
    ObjectHdr: QOS_OBJECT_HDR;
    NumPolicyElement: ULONG; // count of the number of policy elements
    PolicyElement: array [0..0] of RSVP_POLICY; // a list of the policy elements retrieved
  end;
  RSVP_POLICY_INFO = _RSVP_POLICY_INFO;
  TRsvpPolicyInfo = RSVP_POLICY_INFO;
  PRsvpPolicyInfo = LPRSVP_POLICY_INFO;


(*
 * RSVP_RESERVE_INFO Structure used for storing RSVP specific
 * information for fine tuning interaction via the Winsock2
 * Generic QoS API via the provider specific buffer. This structure
 * includes the QOS_OBJECT_HDR structure directly
 *)

  LPRSVP_RESERVE_INFO = ^RSVP_RESERVE_INFO;
  _RSVP_RESERVE_INFO = record
    ObjectHdr: QOS_OBJECT_HDR; // type and length of this object
    Style: ULONG; // RSVP Style (FF,WF,SE)
    ConfirmRequest: ULONG; // Non Zero for Confirm Request (receive only)
    PolicyElementList: LPRSVP_POLICY_INFO; // Points to the set of policy elements
    NumFlowDesc: ULONG; // Number of FlowDesc
    FlowDescList: LPFLOWDESCRIPTOR; // Points to the FlowDesc list
  end;
  RSVP_RESERVE_INFO = _RSVP_RESERVE_INFO;
  TRsvpReserveInfo = RSVP_RESERVE_INFO;
  PRsvpReserveInfo = LPRSVP_RESERVE_INFO;

  LPCRSVP_RESERVE_INFO = ^RSVP_RESERVE_INFO;

(*
 * definitions for the ulStyle in the previous structure
 *)

const
  RSVP_DEFAULT_STYLE         = $00000000;
  RSVP_WILDCARD_STYLE        = $00000001;
  RSVP_FIXED_FILTER_STYLE    = $00000002;
  RSVP_SHARED_EXPLICIT_STYLE = $00000003;

(*
 * RSVP_STATUS_INFO Structure used for storing RSVP specific
 * error of status indications.  This also serves as a header
 * for additional objects in the provider specific buffer when
 * interacting via Winsock2 Generic QoS API. This structure includes
 * the QOS_OBJECT_COUNT and QOS_OBJECT_HDR structures directly It is
 * expected to be the first structure in the provider specific structure
 * since it includes the QOS_OBJECT_COUNT
 *)


type
  LPRSVP_STATUS_INFO = ^RSVP_STATUS_INFO;
  _RSVP_STATUS_INFO = record
    ObjectHdr: QOS_OBJECT_HDR; // Object Hdr
    StatusCode: ULONG; // Error or Status Information see Winsock2.h
    ExtendedStatus1: ULONG; // Provider specific status extension
    ExtendedStatus2: ULONG; // Provider specific status extension
  end;
  RSVP_STATUS_INFO = _RSVP_STATUS_INFO;
  TRsvpStatusInfo = RSVP_STATUS_INFO;
  PRsvpStatusInfo = LPRSVP_STATUS_INFO;

  LPCRSVP_STATUS_INFO = ^RSVP_STATUS_INFO;

(*
 * QOS_DestAddr structure -- used for WSAIoctl(SIO_SET_QOS) when we
 * do not want to issue a connect for a sending socket.  The destination
 * address is required so we can generate the session information for
 * RSVP signalling.
*)

  LPQOS_DESTADDR = ^QOS_DESTADDR;
  _QOS_DESTADDR = record
    ObjectHdr: QOS_OBJECT_HDR; // Object header
    SocketAddress: PSockAddr; // Destination socket address
    SocketAddressLength: ULONG; // Length of the address structure
  end;
  QOS_DESTADDR = _QOS_DESTADDR;
  TQosDestAddr = QOS_DESTADDR;
  PQosDestAddr = LPQOS_DESTADDR;

  LPCQOS_DESTADDR = ^QOS_DESTADDR;

(*
 * this structure defines the "General Characterization Parameters" contained in
 * the RSVP Adspec object
 *)

  LPAD_GENERAL_PARAMS = ^AD_GENERAL_PARAMS;
  _AD_GENERAL_PARAMS = record
    IntServAwareHopCount: ULONG; // number of hops that conform to
    // Integrated Services requirements *)
    PathBandwidthEstimate: ULONG; // minimum bandwidth available from
    // sender to receiver *)
    MinimumLatency: ULONG; // sum of minimum latency of the packet
    // forwarding process in routers
    // (in usec)*)
    PathMTU: ULONG; // max transmission unit end to end that
    // will not incur fragmentation *)
    Flags: ULONG; // used to hold break bits.*)
  end;
  AD_GENERAL_PARAMS = _AD_GENERAL_PARAMS;
  TAdGeneralParams = AD_GENERAL_PARAMS;
  PAdGeneralParams = LPAD_GENERAL_PARAMS;

(*
 * Minimum Latency may be set to this "undefined" value
 *)

const
  INDETERMINATE_LATENCY = DWORD($FFFFFFFF);

(*
 * This Flag is used to indicate the existence of a network element not 
 * supporting  QoS control services somewhere in the data path. If this bit 
 * is set in the specific service override then it indicates that that
 * service was not supported at at least one hop.
 *)

  AD_FLAG_BREAK_BIT = $00000001;

(*
 * this structure describes the Guaranteed service parameters
 *)

type
  LPAD_GUARANTEED = ^AD_GUARANTEED;
  _AD_GUARANTEED = record
    CTotal: ULONG;
    DTotal: ULONG;
    CSum: ULONG;
    DSum: ULONG;
  end;
  AD_GUARANTEED = _AD_GUARANTEED;
  TAdGuaranteed = AD_GUARANTEED;
  PAdGuaranteed = LPAD_GUARANTEED;

(*
 * this structure describes the format of the parameter buffer that can be
 * included in the Service_Type structure below.  This structure allows an
 * application to include any valid Int Serv service parameter in the Buffer 
 * value, after providing the Int Serv parameter id in the ParameterId field.
 *)

  LPPARAM_BUFFER = ^PARAM_BUFFER;
  _PARAM_BUFFER = record
    ParameterId: ULONG; // Int Server parameter ID
    Length: ULONG; // total length of this structure
    // ( 8 bytes + length of Buffer )
    Buffer: array [0..1 - 1] of UCHAR; // Paramter itself
  end;
  PARAM_BUFFER = _PARAM_BUFFER;
  TParamBuffer = PARAM_BUFFER;
  PParamBuffer = LPPARAM_BUFFER;


(*
 * this structure contains the service types supported
 *)

  LPCONTROL_SERVICE = ^CONTROL_SERVICE;
  _CONTROL_SERVICE = record

    //
    // the length of this entire structure including the following buffer.
    // This length value can be added to the ptr to the structure to get the ptr
    // to the next SERVICE_TYPE structure in the list, until the
    // NumberOfServices count has been exhausted.
    //

    Length: ULONG;
    Service: SERVICETYPE;
    Overrides: AD_GENERAL_PARAMS;

    //
    // service specific information ( controlled load has no service specific
    // info here )
    //

    case Integer of
      0: (Guaranteed: AD_GUARANTEED);
      1: (ParamBuffer: array [0..0] of PARAM_BUFFER); // allows for other services down the road

  end;
  CONTROL_SERVICE = _CONTROL_SERVICE;
  TControlService = CONTROL_SERVICE;
  PControlService = LPCONTROL_SERVICE;

(*
 * This structure defines the information which is carried in the Rsvp 
 * Adspec.  This Rsvp object typically indicates which service types are 
 * available ( Controlled Load and/or Guaranteed Service ), if a non-Rsvp
 * hop has been encountered by the Path message, and the minumum MTU along 
 * the path. The services array indicates which services are supported
 *)

  LPRSVP_ADSPEC = ^RSVP_ADSPEC;
  _RSVP_ADSPEC = record
    ObjectHdr: QOS_OBJECT_HDR;
    GeneralParams: AD_GENERAL_PARAMS; // contains the general characterization paramters
    NumberOfServices: ULONG; // count of the number of services
    Services: array [0..0] of CONTROL_SERVICE; // a list of the services supported/requested
  end;
  RSVP_ADSPEC = _RSVP_ADSPEC;
  TRsvpAdSpec = RSVP_ADSPEC;
  PRsvpAdSpec = LPRSVP_ADSPEC;

//
// Opcode for the SIO_CHK_QOS ioctl
// (specific for the Microsoft QOS Service Provider
//
// Bascially:
//
// SIO_CHK_QOS = _WSAIORW(IOC_VENDOR,1)
//             = mIOC_IN | mIOC_OUT | mIOC_VENDOR | mCOMPANY | ioctl_code
//    where
//         mIOC_IN     = 0x80000000
//         mIOC_OUT    = 0x40000000
//         mIOC_VENDOR = 0x04000000
//         mCOMPANY    = 0x18000000
//         ioctl_code  = 0x00000001
//         
// See WSAIoctl man page for details.
//

const
  mIOC_IN     = DWORD($80000000);
  mIOC_OUT    = $40000000;
  mIOC_VENDOR = $04000000;
  mCOMPANY    = $18000000;
  ioctl_code  = $00000001;

  SIO_CHK_QOS = mIOC_IN or mIOC_OUT or mIOC_VENDOR or mCOMPANY or ioctl_code;

//
// The following may be specified in the input buffer 
// of the SIO_CHK_IOCTL ioctl call 
//

  QOSSPBASE = 50000;

  ALLOWED_TO_SEND_DATA = (QOSSPBASE+1); // query the SBM/BEST_EFFORT limit
                                                //   -- result is based on a comparison between
                                                //      the user-specified token rate and the
                                                //      SBM/BEST_EFFORT bandwidth parameters in
                                                //      the system
                                                //   -- result is a YES(1) or NO(0) answer
                                                //      in the output buffer 

  ABLE_TO_RECV_RSVP = (QOSSPBASE+2); // query the SBM/BEST_EFFORT limit
                                                //   -- result is based on a comparison between
                                                //      the user-specified token rate and the
                                                //      SBM/BEST_EFFORT bandwidth parameters in
                                                //      the system
                                                //   -- result is a YES(1) or NO(0) answer
                                                //      in the output buffer 

  LINE_RATE = (QOSSPBASE+3); // query the interface capacity
                                                //   -- result returned in the output buffer
                                                //      in kilo-bits per second

  LOCAL_TRAFFIC_CONTROL = (QOSSPBASE+4); // check if Kernel Traffic Control is available or not
                                                //   -- 0 if not available
                                                //   -- 1 if avaiable
                                                //   -- INFO_NOT_AVAILABLE if there is no way to check

  LOCAL_QOSABILITY = (QOSSPBASE+5); // The followings are for
                                                // the Discovery of Local QOSability
                                                //   -- 0 if no local QOS support
                                                //   -- 1 if local QOS support available
                                                //   -- INFO_NOT_AVAILABLE if there is no way to check

  END_TO_END_QOSABILITY = (QOSSPBASE+6); // The followings are for
                                                // the Discovery of End-to-End QOSability
                                                //   -- 0 if no end-to-end QOS support
                                                //   -- 1 if end-to-end QOS support available
                                                //   -- INFO_NOT_AVAILABLE if there is no way to check

  INFO_NOT_AVAILABLE = DWORD($FFFFFFFF); // used when LINE_RATE is not known


  ANY_DEST_ADDR = DWORD($FFFFFFFF); // for QOS_OBJECT_DEST_ADDR


// The followings are for Guaranteed_Service+Latency -> ISSLOW_Traffic_Class Mapping

  MODERATELY_DELAY_SENSITIVE = DWORD($FFFFFFFD);

  HIGHLY_DELAY_SENSITIVE = DWORD($FFFFFFFE);

//
// QOSSP Error Code/Value
//

  QOSSP_ERR_BASE = (QOSSPBASE+6000);

// No error

  GQOS_NO_ERRORCODE = (0);

  GQOS_NO_ERRORVALUE = (0);

// Unknown error

  GQOS_ERRORCODE_UNKNOWN = DWORD($FFFFFFFF);

  GQOS_ERRORVALUE_UNKNOWN = DWORD($FFFFFFFF);

// Admission (Resource) Error

  GQOS_NET_ADMISSION = (QOSSP_ERR_BASE+100);

  GQOS_OTHER     = (QOSSP_ERR_BASE+100+1);
  GQOS_DELAYBND  = (QOSSP_ERR_BASE+100+2);
  GQOS_BANDWIDTH = (QOSSP_ERR_BASE+100+3);
  GQOS_MTU       = (QOSSP_ERR_BASE+100+4);
  GQOS_FLOW_RATE = (QOSSP_ERR_BASE+100+5);
  GQOS_PEAK_RATE = (QOSSP_ERR_BASE+100+6);
  GQOS_AGG_PEAK_RATE = (QOSSP_ERR_BASE+100+7);

// Policy Error

  GQOS_NET_POLICY = (QOSSP_ERR_BASE+200);

  GQOS_POLICY_ERROR_UNKNOWN = (QOSSP_ERR_BASE+200+0);

  GQOS_POLICY_GLOBAL_DEF_FLOW_COUNT      = (QOSSP_ERR_BASE+200+1);
  GQOS_POLICY_GLOBAL_GRP_FLOW_COUNT      = (QOSSP_ERR_BASE+200+2);
  GQOS_POLICY_GLOBAL_USER_FLOW_COUNT     = (QOSSP_ERR_BASE+200+3);
  GQOS_POLICY_GLOBAL_UNK_USER_FLOW_COUNT = (QOSSP_ERR_BASE+200+4);
  GQOS_POLICY_SUBNET_DEF_FLOW_COUNT      = (QOSSP_ERR_BASE+200+5);
  GQOS_POLICY_SUBNET_GRP_FLOW_COUNT      = (QOSSP_ERR_BASE+200+6);
  GQOS_POLICY_SUBNET_USER_FLOW_COUNT     = (QOSSP_ERR_BASE+200+7);
  GQOS_POLICY_SUBNET_UNK_USER_FLOW_COUNT = (QOSSP_ERR_BASE+200+8);

  GQOS_POLICY_GLOBAL_DEF_FLOW_DURATION      = (QOSSP_ERR_BASE+200+9);
  GQOS_POLICY_GLOBAL_GRP_FLOW_DURATION      = (QOSSP_ERR_BASE+200+10);
  GQOS_POLICY_GLOBAL_USER_FLOW_DURATION     = (QOSSP_ERR_BASE+200+11);
  GQOS_POLICY_GLOBAL_UNK_USER_FLOW_DURATION = (QOSSP_ERR_BASE+200+12);
  GQOS_POLICY_SUBNET_DEF_FLOW_DURATION      = (QOSSP_ERR_BASE+200+13);
  GQOS_POLICY_SUBNET_GRP_FLOW_DURATION      = (QOSSP_ERR_BASE+200+14);
  GQOS_POLICY_SUBNET_USER_FLOW_DURATION     = (QOSSP_ERR_BASE+200+15);
  GQOS_POLICY_SUBNET_UNK_USER_FLOW_DURATION = (QOSSP_ERR_BASE+200+16);

  GQOS_POLICY_GLOBAL_DEF_FLOW_RATE      = (QOSSP_ERR_BASE+200+17);
  GQOS_POLICY_GLOBAL_GRP_FLOW_RATE      = (QOSSP_ERR_BASE+200+18);
  GQOS_POLICY_GLOBAL_USER_FLOW_RATE     = (QOSSP_ERR_BASE+200+19);
  GQOS_POLICY_GLOBAL_UNK_USER_FLOW_RATE = (QOSSP_ERR_BASE+200+20);
  GQOS_POLICY_SUBNET_DEF_FLOW_RATE      = (QOSSP_ERR_BASE+200+21);
  GQOS_POLICY_SUBNET_GRP_FLOW_RATE      = (QOSSP_ERR_BASE+200+22);
  GQOS_POLICY_SUBNET_USER_FLOW_RATE     = (QOSSP_ERR_BASE+200+23);
  GQOS_POLICY_SUBNET_UNK_USER_FLOW_RATE = (QOSSP_ERR_BASE+200+24);

  GQOS_POLICY_GLOBAL_DEF_PEAK_RATE      = (QOSSP_ERR_BASE+200+25);
  GQOS_POLICY_GLOBAL_GRP_PEAK_RATE      = (QOSSP_ERR_BASE+200+26);
  GQOS_POLICY_GLOBAL_USER_PEAK_RATE     = (QOSSP_ERR_BASE+200+27);
  GQOS_POLICY_GLOBAL_UNK_USER_PEAK_RATE = (QOSSP_ERR_BASE+200+28);
  GQOS_POLICY_SUBNET_DEF_PEAK_RATE      = (QOSSP_ERR_BASE+200+29);
  GQOS_POLICY_SUBNET_GRP_PEAK_RATE      = (QOSSP_ERR_BASE+200+30);
  GQOS_POLICY_SUBNET_USER_PEAK_RATE     = (QOSSP_ERR_BASE+200+31);
  GQOS_POLICY_SUBNET_UNK_USER_PEAK_RATE = (QOSSP_ERR_BASE+200+32);

  GQOS_POLICY_GLOBAL_DEF_SUM_FLOW_RATE      = (QOSSP_ERR_BASE+200+33);
  GQOS_POLICY_GLOBAL_GRP_SUM_FLOW_RATE      = (QOSSP_ERR_BASE+200+34);
  GQOS_POLICY_GLOBAL_USER_SUM_FLOW_RATE     = (QOSSP_ERR_BASE+200+35);
  GQOS_POLICY_GLOBAL_UNK_USER_SUM_FLOW_RATE = (QOSSP_ERR_BASE+200+36);
  GQOS_POLICY_SUBNET_DEF_SUM_FLOW_RATE      = (QOSSP_ERR_BASE+200+37);
  GQOS_POLICY_SUBNET_GRP_SUM_FLOW_RATE      = (QOSSP_ERR_BASE+200+38);
  GQOS_POLICY_SUBNET_USER_SUM_FLOW_RATE     = (QOSSP_ERR_BASE+200+39);
  GQOS_POLICY_SUBNET_UNK_USER_SUM_FLOW_RATE = (QOSSP_ERR_BASE+200+40);

  GQOS_POLICY_GLOBAL_DEF_SUM_PEAK_RATE      = (QOSSP_ERR_BASE+200+41);
  GQOS_POLICY_GLOBAL_GRP_SUM_PEAK_RATE      = (QOSSP_ERR_BASE+200+42);
  GQOS_POLICY_GLOBAL_USER_SUM_PEAK_RATE     = (QOSSP_ERR_BASE+200+43);
  GQOS_POLICY_GLOBAL_UNK_USER_SUM_PEAK_RATE = (QOSSP_ERR_BASE+200+44);
  GQOS_POLICY_SUBNET_DEF_SUM_PEAK_RATE      = (QOSSP_ERR_BASE+200+45);
  GQOS_POLICY_SUBNET_GRP_SUM_PEAK_RATE      = (QOSSP_ERR_BASE+200+46);
  GQOS_POLICY_SUBNET_USER_SUM_PEAK_RATE     = (QOSSP_ERR_BASE+200+47);
  GQOS_POLICY_SUBNET_UNK_USER_SUM_PEAK_RATE = (QOSSP_ERR_BASE+200+48);

  GQOS_POLICY_UNKNOWN_USER       = (QOSSP_ERR_BASE+200+49);
  GQOS_POLICY_NO_PRIVILEGES      = (QOSSP_ERR_BASE+200+50);
  GQOS_POLICY_EXPIRED_USER_TOKEN = (QOSSP_ERR_BASE+200+51);
  GQOS_POLICY_NO_RESOURCES       = (QOSSP_ERR_BASE+200+52);
  GQOS_POLICY_PRE_EMPTED         = (QOSSP_ERR_BASE+200+53);
  GQOS_POLICY_USER_CHANGED       = (QOSSP_ERR_BASE+200+54);
  GQOS_POLICY_NO_ACCEPTS         = (QOSSP_ERR_BASE+200+55);
  GQOS_POLICY_NO_MEMORY          = (QOSSP_ERR_BASE+200+56);
  GQOS_POLICY_CRAZY_FLOWSPEC     = (QOSSP_ERR_BASE+200+57);

  GQOS_POLICY_NO_MORE_INFO                = (QOSSP_ERR_BASE+200+58);
  GQOS_POLICY_UNSUPPORTED_CREDENTIAL_TYPE = (QOSSP_ERR_BASE+200+59);
  GQOS_POLICY_INSUFFICIENT_PRIVILEGES     = (QOSSP_ERR_BASE+200+60);
  GQOS_POLICY_EXPIRED_CREDENTIAL          = (QOSSP_ERR_BASE+200+61);
  GQOS_POLICY_IDENTITY_CHANGED            = (QOSSP_ERR_BASE+200+62);
  GQOS_POLICY_NO_QOS_PROVIDED             = (QOSSP_ERR_BASE+200+63);
  GQOS_POLICY_DO_NOT_SEND                 = (QOSSP_ERR_BASE+200+64);

  GQOS_POLICY_ERROR_USERID = (QOSSP_ERR_BASE+200+99);

// RSVP Error

  GQOS_RSVP = (QOSSP_ERR_BASE+300);

  GQOS_NO_PATH        = (QOSSP_ERR_BASE+300+1);
  GQOS_NO_SENDER      = (QOSSP_ERR_BASE+300+2);
  GQOS_BAD_STYLE      = (QOSSP_ERR_BASE+300+3);
  GQOS_UNKNOWN_STYLE  = (QOSSP_ERR_BASE+300+4);
  GQOS_BAD_DSTPORT    = (QOSSP_ERR_BASE+300+5);
  GQOS_BAD_SNDPORT    = (QOSSP_ERR_BASE+300+6);
  GQOS_AMBIG_FILTER   = (QOSSP_ERR_BASE+300+7);
  GQOS_PREEMPTED      = (QOSSP_ERR_BASE+300+8);
  GQOS_UNKN_OBJ_CLASS = (QOSSP_ERR_BASE+300+9);
  GQOS_UNKNOWN_CTYPE  = (QOSSP_ERR_BASE+300+10);
  GQOS_INVALID        = (QOSSP_ERR_BASE+300+11);

// API Error

  GQOS_API = (QOSSP_ERR_BASE+400);

  GQOS_API_BADSEND  = (QOSSP_ERR_BASE+400+1); (* Sender addr not my interface *)
  GQOS_API_BADRECV  = (QOSSP_ERR_BASE+400+2); (* Recv addr not my interface *)
  GQOS_API_BADSPORT = (QOSSP_ERR_BASE+400+3); (* Sport !=0 but Dport == 0 *)

// TC System Error

  GQOS_KERNEL_TC_SYS = (QOSSP_ERR_BASE+500);

  GQOS_TC_GENERIC       = (QOSSP_ERR_BASE+500+1);
  GQOS_TC_INVALID       = (QOSSP_ERR_BASE+500+2);
  GQOS_NO_MEMORY        = (QOSSP_ERR_BASE+500+3);
  GQOS_BAD_ADDRESSTYPE  = (QOSSP_ERR_BASE+500+4);
  GQOS_BAD_DUPLICATE    = (QOSSP_ERR_BASE+500+5);
  GQOS_CONFLICT         = (QOSSP_ERR_BASE+500+6);
  GQOS_NOTREADY         = (QOSSP_ERR_BASE+500+7);
  GQOS_WOULDBLOCK       = (QOSSP_ERR_BASE+500+8);
  GQOS_INCOMPATIBLE     = (QOSSP_ERR_BASE+500+9);
  GQOS_BAD_SDMODE       = (QOSSP_ERR_BASE+500+10);
  GQOS_BAD_QOSPRIORITY  = (QOSSP_ERR_BASE+500+11);
  GQOS_BAD_TRAFFICCLASS = (QOSSP_ERR_BASE+500+12);
  GQOS_NO_SYS_RESOURCES = (QOSSP_ERR_BASE+500+13);

// RSVP System Error

  GQOS_RSVP_SYS = (QOSSP_ERR_BASE+600);

  GQOS_OTHER_SYS            = (QOSSP_ERR_BASE+600+1);
  GQOS_MEMORY_SYS           = (QOSSP_ERR_BASE+600+2);
  GQOS_API_SYS              = (QOSSP_ERR_BASE+600+3);
  GQOS_SETQOS_NO_LOCAL_APPS = (QOSSP_ERR_BASE+600+4);

// TC Error

  GQOS_KERNEL_TC = (QOSSP_ERR_BASE+700);

  GQOS_CONFLICT_SERV = (QOSSP_ERR_BASE+700+1);
  GQOS_NO_SERV       = (QOSSP_ERR_BASE+700+2);
  GQOS_BAD_FLOWSPEC  = (QOSSP_ERR_BASE+700+3);
  GQOS_BAD_TSPEC     = (QOSSP_ERR_BASE+700+4);
  GQOS_BAD_ADSPEC    = (QOSSP_ERR_BASE+700+5);

// GQOS API ERRORS

// WSAIoctl Error

  GQOS_IOCTL_SYSTEMFAILURE   = (QOSSP_ERR_BASE+800+1);
  GQOS_IOCTL_NOBYTESRETURNED = (QOSSP_ERR_BASE+800+2);
  GQOS_IOCTL_INVALIDSOCKET   = (QOSSP_ERR_BASE+800+3);
  GQOS_IOCTL_INV_FUNCPTR     = (QOSSP_ERR_BASE+800+4);
  GQOS_IOCTL_INV_OVERLAPPED  = (QOSSP_ERR_BASE+800+5);

// SIO_SET_QOS error

  GQOS_SETQOS_BADINBUFFER      = (QOSSP_ERR_BASE+810+1);
  GQOS_SETQOS_BADFLOWSPEC      = (QOSSP_ERR_BASE+810+2);
  GQOS_SETQOS_COLLISION        = (QOSSP_ERR_BASE+810+3);
  GQOS_SETQOS_BADPROVSPECBUF   = (QOSSP_ERR_BASE+810+4);
  GQOS_SETQOS_ILLEGALOP        = (QOSSP_ERR_BASE+810+5);
  GQOS_SETQOS_INVALIDADDRESS   = (QOSSP_ERR_BASE+810+6);
  GQOS_SETQOS_OUTOFMEMORY      = (QOSSP_ERR_BASE+810+7);
  GQOS_SETQOS_EXCEPTION        = (QOSSP_ERR_BASE+810+8);
  GQOS_SETQOS_BADADDRLEN       = (QOSSP_ERR_BASE+810+9);
  GQOS_SETQOS_NOSOCKNAME       = (QOSSP_ERR_BASE+810+10);
  GQOS_SETQOS_IPTOSFAIL        = (QOSSP_ERR_BASE+810+11);
  GQOS_SETQOS_OPENSESSIONFAIL  = (QOSSP_ERR_BASE+810+12);
  GQOS_SETQOS_SENDFAIL         = (QOSSP_ERR_BASE+810+13);
  GQOS_SETQOS_RECVFAIL         = (QOSSP_ERR_BASE+810+14);
  GQOS_SETQOS_BADPOLICYOBJECT  = (QOSSP_ERR_BASE+810+15);
  GQOS_SETQOS_UNKNOWNFILTEROBJ = (QOSSP_ERR_BASE+810+16);
  GQOS_SETQOS_BADFILTERTYPE    = (QOSSP_ERR_BASE+810+17);
  GQOS_SETQOS_BADFILTERCOUNT   = (QOSSP_ERR_BASE+810+18);
  GQOS_SETQOS_BADOBJLENGTH     = (QOSSP_ERR_BASE+810+19);
  GQOS_SETQOS_BADFLOWCOUNT     = (QOSSP_ERR_BASE+810+20);
  GQOS_SETQOS_UNKNOWNPSOBJ     = (QOSSP_ERR_BASE+810+21);
  GQOS_SETQOS_BADPOLICYOBJ     = (QOSSP_ERR_BASE+810+22);
  GQOS_SETQOS_BADFLOWDESC      = (QOSSP_ERR_BASE+810+23);
  GQOS_SETQOS_BADPROVSPECOBJ   = (QOSSP_ERR_BASE+810+24);
  GQOS_SETQOS_NOLOOPBACK       = (QOSSP_ERR_BASE+810+25);
  GQOS_SETQOS_MODENOTSUPPORTED = (QOSSP_ERR_BASE+810+26);
  GQOS_SETQOS_MISSINGFLOWDESC  = (QOSSP_ERR_BASE+810+27);

// SIO_GET_QOS error

  GQOS_GETQOS_BADOUTBUFFER    = (QOSSP_ERR_BASE+840+1);
  GQOS_GETQOS_SYSTEMFAILURE   = (QOSSP_ERR_BASE+840+2);
  GQOS_GETQOS_EXCEPTION       = (QOSSP_ERR_BASE+840+3);
  GQOS_GETQOS_INTERNALFAILURE = (QOSSP_ERR_BASE+840+4);

// SIO_CHK_QOS error

  GQOS_CHKQOS_BADINBUFFER     = (QOSSP_ERR_BASE+850+1);
  GQOS_CHKQOS_BADOUTBUFFER    = (QOSSP_ERR_BASE+850+2);
  GQOS_CHKQOS_SYSTEMFAILURE   = (QOSSP_ERR_BASE+850+3);
  GQOS_CHKQOS_INTERNALFAILURE = (QOSSP_ERR_BASE+850+4);
  GQOS_CHKQOS_BADPARAMETER    = (QOSSP_ERR_BASE+850+5);
  GQOS_CHKQOS_EXCEPTION       = (QOSSP_ERR_BASE+850+6);

implementation

end.




























