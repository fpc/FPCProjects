{******************************************************************************}
{                                                       	               }
{ Winsock API interface Unit for Object Pascal                                 }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winsock.h, released June 2000. The original Pascal     }
{ code is: WinSock.pas, released December 2000. The initial developer of the   }
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

unit JwaWinSock;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "winsock.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType, JwaWinBase;

(*
 * Basic system type definitions, taken from the BSD file sys/types.h.
 *)

type
  u_char = Byte;
  u_short = Word;
  u_int = Cardinal;
  u_long = Cardinal;

(*
 * The new type to be used in all
 * instances which refer to sockets.
 *)

type
  TSocket = UINT_PTR;

(*
 * Select uses arrays of SOCKETs.  These macros manipulate such
 * arrays.  FD_SETSIZE may be defined by the user before including
 * this file, but the default here should be >= 64.
 *
 * CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
 * INCLUDED IN WINSOCK.H EXACTLY AS SHOWN HERE.
 *)

const
  FD_SETSIZE = 64;

type
  fd_set = record
    fd_count: u_int;                                 // how many are SET?
    fd_array: array [0..FD_SETSIZE - 1] of TSocket;   // an array of SOCKETs
  end;
  TFdSet = fd_set;
  PFdSet = ^fd_set;

function __WSAFDIsSet(s: TSocket; var FDSet: TFDSet): Integer; stdcall;

procedure FD_CLR(fd: TSocket; var fdset: TFdSet);

procedure _FD_SET(fd: TSocket; var fdset: TFDSet);
//{$EXTERNALSYM FD_SET}

procedure FD_ZERO(var fdset: TFdSet);

function FD_ISSET(fd: TSocket; var fdset: TFdSet): Boolean;

(*
 * Structure used in select() call, taken from the BSD file sys/time.h.
 *)

type
  timeval = record
    tv_sec: Longint;         // seconds
    tv_usec: Longint;        // and microseconds
  end;
  TTimeVal = timeval;
  PTimeVal = ^timeval;

(*
 * Operations on timevals.
 *
 * NB: timercmp does not work for >= or <=.
 *)

function timerisset(const tvp: TTimeVal): Boolean;

//function timercmp(const tvp, uvp: TTimeVal; cmp): Boolean;
//{$EXTERNALSYM timercmp}

procedure timerclear(var tvp: TTimeVal);

(*
 * Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
 *
 *
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 *)

const
  IOCPARM_MASK = $7f;              // parameters must be < 128 bytes
  IOC_VOID     = $20000000;        // no parameters
  IOC_OUT      = $40000000;        // copy out parameters
  IOC_IN       = DWORD($80000000); // copy in parameters
  IOC_INOUT    = DWORD(IOC_IN or IOC_OUT);

  // 0x20000000 distinguishes new & old ioctl's

function _IO(x, y: DWORD): DWORD;

function _IOR(x, y, t: DWORD): DWORD;

function _IOW(x, y, t: DWORD): DWORD;

const
  FIONREAD = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 127; // get # bytes to read
  FIONBIO = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 126; // set/clear non-blocking i/o
  FIOASYNC = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 125; // set/clear async i/o

(* Socket I/O Controls *)

  SIOCSHIWAT = DWORD(IOC_IN or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 0); // set high watermark
  SIOCGHIWAT = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 1; // get high watermark
  SIOCSLOWAT = DWORD(IOC_IN or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 2); // set low watermark
  SIOCGLOWAT = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 3; // get low watermark
  SIOCATMARK = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 7; // at oob mark?

(*
 * Structures returned by network data base library, taken from the
 * BSD file netdb.h.  All addresses are supplied in host order, and
 * returned in network order (suitable for use in system calls).
 *)

type
  hostent = record
    h_name: PChar;           // official name of host
    h_aliases: PPChar;  // alias list
    h_addrtype: Smallint;             // host address type
    h_length: Smallint;               // length of address
    case Integer of
      0: (h_addr_list: PPChar); // list of addresses
      1: (h_addr: PPChar);          // address, for backward compat
  end;
  THostEnt = hostent;
  PHostEnt = ^hostent;

(*
 * It is assumed here that a network number
 * fits in 32 bits.
 *)

type
  netent = record
    n_name: PChar;           // official name of net
    n_aliases: PPChar;  // alias list
    n_addrtype: Smallint;             // net address type
    n_net: u_long;                  // network #
  end;
  TNetEnt = netent;
  PNetEnt = ^netent;

  servent = record
    s_name: PChar;           // official service name
    s_aliases: PPChar;  // alias list
    s_port: Smallint;                 // port #
    s_proto: PChar;          // protocol to use
  end;
  TServEnt = servent;
  PServEnt = ^servent;

  protoent = record
    p_name: PChar;           // official protocol name
    p_aliases: PPChar;  // alias list
    p_proto: Smallint;                // protocol #
  end;
  TProtoEnt = protoent;
  PProtoEnt = ^protoent;

(*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
 *)

(*
 * Protocols
 *)

const
  IPPROTO_IP   = 0; // dummy for IP
  IPPROTO_ICMP = 1; // control message protocol
  IPPROTO_IGMP = 2; // internet group management protocol
  IPPROTO_GGP  = 3; // gateway^2 (deprecated)
  IPPROTO_TCP  = 6; // tcp
  IPPROTO_PUP  = 12; // pup
  IPPROTO_UDP  = 17; // user datagram protocol
  IPPROTO_IDP  = 22; // xns idp
  IPPROTO_ND   = 77; // UNOFFICIAL net disk proto

  IPPROTO_RAW  = 255; // raw IP packet
  IPPROTO_MAX  = 256;

(*
 * Port/socket numbers: network standard functions
 *)

  IPPORT_ECHO       = 7;
  IPPORT_DISCARD    = 9;
  IPPORT_SYSTAT     = 11;
  IPPORT_DAYTIME    = 13;
  IPPORT_NETSTAT    = 15;
  IPPORT_FTP        = 21;
  IPPORT_TELNET     = 23;
  IPPORT_SMTP       = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS      = 43;
  IPPORT_MTP        = 57;

(*
 * Port/socket numbers: host specific functions
 *)

  IPPORT_TFTP    = 69;
  IPPORT_RJE     = 77;
  IPPORT_FINGER  = 79;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP  = 95;

(*
 * UNIX TCP sockets
 *)

  IPPORT_EXECSERVER  = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER   = 514;
  IPPORT_EFSSERVER   = 520;

(*
 * UNIX UDP sockets
 *)

  IPPORT_BIFFUDP     = 512;
  IPPORT_WHOSERVER   = 513;
  IPPORT_ROUTESERVER = 520;

  (* 520+1 also used *)

(*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 *)

  IPPORT_RESERVED = 1024;

(*
 * Link numbers
 *)

  IMPLINK_IP        = 155;
  IMPLINK_LOWEXPER  = 156;
  IMPLINK_HIGHEXPER = 158;

(*
 * Internet address (old style... should be updated)
 *)

type
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;

  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  in_addr = record
    case Integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
    // #define s_addr  S_un.S_addr // can be used for most tcp & ip code
    // #define s_host  S_un.S_un_b.s_b2 // host on imp
    // #define s_net   S_un.S_un_b.s_b1  // netword
    // #define s_imp   S_un.S_un_w.s_w2 // imp
    // #define s_impno S_un.S_un_b.s_b4 // imp #
    // #define s_lh    S_un.S_un_b.s_b3 // logical host
  end;
  TInAddr = in_addr;
  PInAddr = ^in_addr;

(*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 *)

function IN_CLASSA(i: DWORD): Boolean;

const
  IN_CLASSA_NET    = DWORD($ff000000);
  IN_CLASSA_NSHIFT = 24;
  IN_CLASSA_HOST   = $00ffffff;
  IN_CLASSA_MAX    = 128;

function IN_CLASSB(i: DWORD): Boolean;

const
  IN_CLASSB_NET    = DWORD($ffff0000);
  IN_CLASSB_NSHIFT = 16;
  IN_CLASSB_HOST   = $0000ffff;
  IN_CLASSB_MAX    = 65536;

function IN_CLASSC(i: DWORD): Boolean;

const
  IN_CLASSC_NET    = DWORD($ffffff00);
  IN_CLASSC_NSHIFT = 8;
  IN_CLASSC_HOST   = $000000ff;

const
  INADDR_ANY       = u_long($00000000);
  INADDR_LOOPBACK  = $7f000001;
  INADDR_BROADCAST = u_long($ffffffff);
  INADDR_NONE      = DWORD($ffffffff);

(*
 * Socket address, internet style.
 *)

type
  sockaddr_in = record
    sin_family: Smallint;
    sin_port: u_short;
    sin_addr: in_addr;
    sin_zero: array [0..7] of Char;
  end;
  TSockAddrIn = sockaddr_in;
  PSockAddrIn = ^sockaddr_in;

const
  WSADESCRIPTION_LEN = 256;
  WSASYS_STATUS_LEN  = 128;

type
  WSAData = record
    wVersion: WORD;
    wHighVersion: WORD;
    szDescription: array [0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array [0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;
  LPWSADATA = ^WSAData;
  TWsaData = WSAData;
  PWsaData = LPWSADATA;

(*
 * Options for use with [gs]etsockopt at the IP level.
 *)

const
  IP_OPTIONS         = 1;           (* set/get IP per-packet options    *)
  IP_MULTICAST_IF    = 2;           (* set/get IP multicast interface   *)
  IP_MULTICAST_TTL   = 3;           (* set/get IP multicast timetolive  *)
  IP_MULTICAST_LOOP  = 4;           (* set/get IP multicast loopback    *)
  IP_ADD_MEMBERSHIP  = 5;           (* add  an IP group membership      *)
  IP_DROP_MEMBERSHIP = 6;           (* drop an IP group membership      *)
  IP_TTL             = 7;           (* set/get IP Time To Live          *)
  IP_TOS             = 8;           (* set/get IP Type Of Service       *)
  IP_DONTFRAGMENT    = 9;           (* set/get IP Don't Fragment flag   *)

  IP_DEFAULT_MULTICAST_TTL  = 1;    (* normally limit m'casts to 1 hop  *)
  IP_DEFAULT_MULTICAST_LOOP = 1;    (* normally hear sends if a member  *)
  IP_MAX_MEMBERSHIPS        = 20;   (* per socket; must fit in one mbuf *)

(*
 * Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP.
 *)

type
  ip_mreq = record
    imr_multiaddr: in_addr;  (* IP multicast address of group *)
    mr_interface: in_addr;  (* local IP address of interface *)
  end;
  TIpMReq = ip_mreq;
  PIpMReq = ^ip_mreq;  

(*
 * Definitions related to sockets: types, address families, options,
 * taken from the BSD file sys/socket.h.
 *)

(*
 * This is used instead of -1, since the
 * SOCKET type is unsigned.
 *)

const
  INVALID_SOCKET = TSocket(not 0);
  SOCKET_ERROR   = -1;

(*
 * Types
 *)

  SOCK_STREAM    = 1; // stream socket
  SOCK_DGRAM     = 2; // datagram socket
  SOCK_RAW       = 3; // raw-protocol interface
  SOCK_RDM       = 4; // reliably-delivered message
  SOCK_SEQPACKET = 5; // sequenced packet stream

(*
 * Option flags per-socket.
 *)

  SO_DEBUG       = $0001; // turn on debugging info recording
  SO_ACCEPTCONN  = $0002; // socket has had listen()
  SO_REUSEADDR   = $0004; // allow local address reuse
  SO_KEEPALIVE   = $0008; // keep connections alive
  SO_DONTROUTE   = $0010; // just use interface addresses
  SO_BROADCAST   = $0020; // permit sending of broadcast msgs
  SO_USELOOPBACK = $0040; // bypass hardware when possible
  SO_LINGER      = $0080; // linger on close if data present
  SO_OOBINLINE   = $0100; // leave received OOB data in line

  SO_DONTLINGER       = DWORD(not SO_LINGER);

(*
 * Additional options.
 *)

  SO_SNDBUF   = $1001; // send buffer size
  SO_RCVBUF   = $1002; // receive buffer size
  SO_SNDLOWAT = $1003; // send low-water mark
  SO_RCVLOWAT = $1004; // receive low-water mark
  SO_SNDTIMEO = $1005; // send timeout
  SO_RCVTIMEO = $1006; // receive timeout
  SO_ERROR    = $1007; // get error status and clear
  SO_TYPE     = $1008; // get socket type

(*
 * Options for connect and disconnect data and options.  Used only by
 * non-TCP/IP transports such as DECNet, OSI TP4, etc.
 *)

  SO_CONNDATA    = $7000;
  SO_CONNOPT     = $7001;
  SO_DISCDATA    = $7002;
  SO_DISCOPT     = $7003;
  SO_CONNDATALEN = $7004;
  SO_CONNOPTLEN  = $7005;
  SO_DISCDATALEN = $7006;
  SO_DISCOPTLEN  = $7007;

(*
 * Option for opening sockets for synchronous access.
 *)

  SO_OPENTYPE    = $7008;

  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;

(*
 * Other NT-specific options.
 *)

  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  SO_CONNECT_TIME = $700C;

(*
 * TCP options.
 *)

  TCP_NODELAY = $0001;
  TCP_BSDURGENT = $7000;

(*
 * Address families.
 *)

  AF_UNSPEC = 0; // unspecified
  AF_UNIX      = 1; // local to host (pipes, portals
  AF_INET      = 2; // internetwork: UDP, TCP, etc.
  AF_IMPLINK   = 3; // arpanet imp addresses
  AF_PUP       = 4; // pup protocols: e.g. BSP
  AF_CHAOS     = 5; // mit CHAOS protocols
  AF_NS        = 6; // XEROX NS protocols
  AF_IPX       = AF_NS; // IPX protocols: IPX, SPX, etc.
  AF_ISO       = 7; // ISO protocols
  AF_OSI       = AF_ISO; // OSI is ISO
  AF_ECMA      = 8; // european computer manufacturers
  AF_DATAKIT   = 9; // datakit protocols
  AF_CCITT     = 10; // CCITT protocols, X.25 etc
  AF_SNA       = 11; // IBM SNA
  AF_DECnet    = 12; // DECnet
  AF_DLI       = 13; // Direct data link interface
  AF_LAT       = 14; // LAT
  AF_HYLINK    = 15; // NSC Hyperchannel
  AF_APPLETALK = 16; // AppleTalk
  AF_NETBIOS   = 17; // NetBios-style addresses
  AF_VOICEVIEW = 18; // VoiceView
  AF_FIREFOX   = 19; // Protocols from Firefox
  AF_UNKNOWN1  = 20; // Somebody is using this!
  AF_BAN       = 21; // Banyan

  AF_MAX = 22;

(*
 * Structure used by kernel to store most
 * addresses.
 *)

type
  sockaddr = record
    sa_family: u_short;              // address family
    sa_data: array [0..13] of Char;            // up to 14 bytes of direct address
  end;
  TSockAddr = sockaddr;
  PSockAddr = ^sockaddr;

(*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 *)

  sockproto = record
    sp_family: u_short;              // address family
    sp_protocol: u_short;            // protocol
  end;
  TSockProto = sockproto;
  PSockProto = ^sockproto;

(*
 * Protocol families, same as address families for now.
 *)

const
  PF_UNSPEC    = AF_UNSPEC;
  PF_UNIX      = AF_UNIX;
  PF_INET      = AF_INET;
  PF_IMPLINK   = AF_IMPLINK;
  PF_PUP       = AF_PUP;
  PF_CHAOS     = AF_CHAOS;
  PF_NS        = AF_NS;
  PF_IPX       = AF_IPX;
  PF_ISO       = AF_ISO;
  PF_OSI       = AF_OSI;
  PF_ECMA      = AF_ECMA;
  PF_DATAKIT   = AF_DATAKIT;
  PF_CCITT     = AF_CCITT;
  PF_SNA       = AF_SNA;
  PF_DECnet    = AF_DECnet;
  PF_DLI       = AF_DLI;
  PF_LAT       = AF_LAT;
  PF_HYLINK    = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_VOICEVIEW = AF_VOICEVIEW;
  PF_FIREFOX   = AF_FIREFOX;
  PF_UNKNOWN1  = AF_UNKNOWN1;
  PF_BAN       = AF_BAN;

  PF_MAX = AF_MAX;


(*
 * Structure used for manipulating linger option.
 *)

type
  linger = record
    l_onoff: u_short;                // option on/off
    l_linger: u_short;               // linger time
  end;
  TLinger = linger;
  PLinger = ^linger;

(*
 * Level number for (get/set)sockopt() to apply to socket itself.
 *)

const
  SOL_SOCKET = $ffff; // options for socket level

(*
 * Maximum queue length specifiable by listen.
 *)

  SOMAXCONN = $7fffffff;

  MSG_OOB       = $1; // process out-of-band data
  MSG_PEEK      = $2; // peek at incoming message
  MSG_DONTROUTE = $4; // send without using routing tables

  MSG_MAXIOVLEN = 16;

  MSG_PARTIAL   = $8000; // partial send or recv for message xport

(*
 * Define constant based on rfc883, used by gethostbyxxxx() calls.
 *)

  MAXGETHOSTSTRUCT = 1024;

(*
 * Define flags to be used with the WSAAsyncSelect() call.
 *)

  FD_READ        = $01;
  FD_WRITE       = $02;
  FD_OOB         = $04;
  FD_ACCEPT      = $08;
  FD_CONNECT     = $10;
  FD_CLOSE       = $20;

(*
 * All Windows Sockets error constants are biased by WSABASEERR from
 * the "normal"
 *)

  WSABASEERR = 10000;

(*
 * Windows Sockets definitions of regular Microsoft C error constants
 *)

  WSAEINTR  = (WSABASEERR+4);
  WSAEBADF  = (WSABASEERR+9);
  WSAEACCES = (WSABASEERR+13);
  WSAEFAULT = (WSABASEERR+14);
  WSAEINVAL = (WSABASEERR+22);
  WSAEMFILE = (WSABASEERR+24);

(*
 * Windows Sockets definitions of regular Berkeley error constants
 *)

  WSAEWOULDBLOCK     = (WSABASEERR+35);
  WSAEINPROGRESS     = (WSABASEERR+36);
  WSAEALREADY        = (WSABASEERR+37);
  WSAENOTSOCK        = (WSABASEERR+38);
  WSAEDESTADDRREQ    = (WSABASEERR+39);
  WSAEMSGSIZE        = (WSABASEERR+40);
  WSAEPROTOTYPE      = (WSABASEERR+41);
  WSAENOPROTOOPT     = (WSABASEERR+42);
  WSAEPROTONOSUPPORT = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT = (WSABASEERR+44);
  WSAEOPNOTSUPP      = (WSABASEERR+45);
  WSAEPFNOSUPPORT    = (WSABASEERR+46);
  WSAEAFNOSUPPORT    = (WSABASEERR+47);
  WSAEADDRINUSE      = (WSABASEERR+48);
  WSAEADDRNOTAVAIL   = (WSABASEERR+49);
  WSAENETDOWN        = (WSABASEERR+50);
  WSAENETUNREACH     = (WSABASEERR+51);
  WSAENETRESET       = (WSABASEERR+52);
  WSAECONNABORTED    = (WSABASEERR+53);
  WSAECONNRESET      = (WSABASEERR+54);
  WSAENOBUFS         = (WSABASEERR+55);
  WSAEISCONN         = (WSABASEERR+56);
  WSAENOTCONN        = (WSABASEERR+57);
  WSAESHUTDOWN       = (WSABASEERR+58);
  WSAETOOMANYREFS    = (WSABASEERR+59);
  WSAETIMEDOUT       = (WSABASEERR+60);
  WSAECONNREFUSED    = (WSABASEERR+61);
  WSAELOOP           = (WSABASEERR+62);
  WSAENAMETOOLONG    = (WSABASEERR+63);
  WSAEHOSTDOWN       = (WSABASEERR+64);
  WSAEHOSTUNREACH    = (WSABASEERR+65);
  WSAENOTEMPTY       = (WSABASEERR+66);
  WSAEPROCLIM        = (WSABASEERR+67);
  WSAEUSERS          = (WSABASEERR+68);
  WSAEDQUOT          = (WSABASEERR+69);
  WSAESTALE          = (WSABASEERR+70);
  WSAEREMOTE         = (WSABASEERR+71);

  WSAEDISCON         = (WSABASEERR+101);

(*
 * Extended Windows Sockets error constant definitions
 *)

  WSASYSNOTREADY         = (WSABASEERR+91);
  WSAVERNOTSUPPORTED     = (WSABASEERR+92);
  WSANOTINITIALISED      = (WSABASEERR+93);

(*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (when using the resolver). Note that these errors are
 * retrieved via WSAGetLastError() and must therefore follow
 * the rules for avoiding clashes with error numbers from
 * specific implementations or language run-time systems.
 * For this reason the codes are based at WSABASEERR+1001.
 * Note also that [WSA]NO_ADDRESS is defined only for
 * compatibility purposes.
 *)

// Authoritative Answer: Host not found

  WSAHOST_NOT_FOUND = (WSABASEERR+1001);

// Non-Authoritative: Host not found, or SERVERFAIL

  WSATRY_AGAIN = (WSABASEERR+1002);

// Non-recoverable errors, FORMERR, REFUSED, NOTIMP

  WSANO_RECOVERY = (WSABASEERR+1003);

// Valid name, no data record of requested type

  WSANO_DATA = (WSABASEERR+1004);

(*
 * Compatibility macros.
 *)

function h_errno: Integer;

const
  HOST_NOT_FOUND = WSAHOST_NOT_FOUND;
  TRY_AGAIN      = WSATRY_AGAIN;
  NO_RECOVERY    = WSANO_RECOVERY;
  NO_DATA        = WSANO_DATA;

// no address, look for MX record

  WSANO_ADDRESS = WSANO_DATA;
  NO_ADDRESS    = WSANO_ADDRESS;

(*
 * Windows Sockets errors redefined as regular Berkeley error constants.
 * These are commented out in Windows NT to avoid conflicts with errno.h.
 * Use the WSA constants instead.
 *)

{$IFDEF FALSE}

const
  EWOULDBLOCK     = WSAEWOULDBLOCK;
  EINPROGRESS     = WSAEINPROGRESS;
  EALREADY        = WSAEALREADY;
  ENOTSOCK        = WSAENOTSOCK;
  EDESTADDRREQ    = WSAEDESTADDRREQ;
  EMSGSIZE        = WSAEMSGSIZE;
  EPROTOTYPE      = WSAEPROTOTYPE;
  ENOPROTOOPT     = WSAENOPROTOOPT;
  EPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT;
  EOPNOTSUPP      = WSAEOPNOTSUPP;
  EPFNOSUPPORT    = WSAEPFNOSUPPORT;
  EAFNOSUPPORT    = WSAEAFNOSUPPORT;
  EADDRINUSE      = WSAEADDRINUSE;
  EADDRNOTAVAIL   = WSAEADDRNOTAVAIL;
  ENETDOWN        = WSAENETDOWN;
  ENETUNREACH     = WSAENETUNREACH;
  ENETRESET       = WSAENETRESET;
  ECONNABORTED    = WSAECONNABORTED;
  ECONNRESET      = WSAECONNRESET;
  ENOBUFS         = WSAENOBUFS;
  EISCONN         = WSAEISCONN;
  ENOTCONN        = WSAENOTCONN;
  ESHUTDOWN       = WSAESHUTDOWN;
  ETOOMANYREFS    = WSAETOOMANYREFS;
  ETIMEDOUT       = WSAETIMEDOUT;
  ECONNREFUSED    = WSAECONNREFUSED;
  ELOOP           = WSAELOOP;
  ENAMETOOLONG    = WSAENAMETOOLONG;
  EHOSTDOWN       = WSAEHOSTDOWN;
  EHOSTUNREACH    = WSAEHOSTUNREACH;
  ENOTEMPTY       = WSAENOTEMPTY;
  EPROCLIM        = WSAEPROCLIM;
  EUSERS          = WSAEUSERS;
  EDQUOT          = WSAEDQUOT;
  ESTALE          = WSAESTALE;
  EREMOTE         = WSAEREMOTE;

{$ENDIF}

(* Socket function prototypes *)

function accept(s: TSocket; addr: PSockAddr; addrlen: PINT): TSocket; stdcall;
function bind(s: TSocket; name: PSockAddr; namelen: Integer): Integer; stdcall;
function closesocket(s: TSocket): Integer; stdcall;
function connect(s: TSocket; name: PSockAddr; namelen: Integer): Integer; stdcall;
function ioctlsocket(s: TSocket; cmd: Longint; var argp: u_long): Integer; stdcall;
function getpeername(s: TSocket; name: PSockAddr; var namelen: Integer): Integer; stdcall;
function getsockname(s: TSocket; name: PSockAddr; var namelen: Integer): Integer; stdcall;
function getsockopt(s: TSocket; level, optname: Integer; optval: PChar; var optlen: Integer): Integer; stdcall;
function htonl(hostlong: u_long): u_long; stdcall;
function htons(hostshort: u_short): u_short; stdcall;
function inet_addr(cp: PChar): u_long; stdcall;
function inet_ntoa(inaddr: in_addr): PChar; stdcall;
function listen(s: TSocket; backlog: Integer): Integer; stdcall;
function ntohl(netlong: u_long): u_long; stdcall;
function ntohs(netshort: u_short): u_short; stdcall;
function recv(s: TSocket; var buf; len, flags: Integer): Integer; stdcall;
function recvfrom(s: TSocket; var buf; len, flags: Integer; from: PSockAddr; var fromlen: Integer): Integer; stdcall;
function select(nfds: Integer; readfds, writefds, exceptfds: PFdSet; timeout: PTimeVal): Integer; stdcall;
function send(s: TSocket; var buf; len, flags: Integer): Integer; stdcall;
function sendto(s: TSocket; var buf; len, flags: Integer; toaddr: PSockAddr; tolen: Integer): Integer; stdcall;
function setsockopt(s: TSocket; level, optname: Integer; optval: PChar; optlen: Integer): Integer; stdcall;
function shutdown(s: TSocket; how: Integer): Integer; stdcall;
function socket(af, type_, protocol: Integer): TSocket; stdcall;

(* Database function prototypes *)

function gethostbyaddr(addr: PChar; len, type_: Integer): PHostEnt; stdcall;
function gethostbyname(name: PChar): PHostEnt; stdcall;
function gethostname(name: PChar; namelen: Integer): Integer; stdcall;
function getservbyport(port: Integer; proto: PChar): PServEnt; stdcall;
function getservbyname(name, proto: PChar): PServEnt; stdcall;
function getprotobynumber(number: Integer): PProtoEnt; stdcall;
function getprotobyname(name: PChar): PProtoEnt; stdcall;

(* Microsoft Windows Extension function prototypes *)

function WSAStartup(wVersionRequired: WORD; var lpWSAData: TWSAData): Integer; stdcall;
function WSACleanup: Integer; stdcall;
procedure WSASetLastError(iError: Integer); stdcall;
function WSAGetLastError: Integer; stdcall;
function WSAIsBlocking: BOOL; stdcall;
function WSAUnhookBlockingHook: Integer; stdcall;
function WSASetBlockingHook(lpBlockFunc: FARPROC): FARPROC; stdcall;
function WSACancelBlockingCall: Integer; stdcall;
function WSAAsyncGetServByName(hWnd: HWND; wMsg: u_int; name, proto, buf: PChar;
  buflen: Integer): HANDLE; stdcall;
function WSAAsyncGetServByPort(hWnd: HWND; wMsg: u_int; port: Integer;
  proto, buf: PChar; buflen: Integer): HANDLE; stdcall;
function WSAAsyncGetProtoByName(hWnd: HWND; wMsg: u_int; name, buf: PChar;
  buflen: Integer): HANDLE; stdcall;
function WSAAsyncGetProtoByNumber(hWnd: HWND; wMsg: u_int; number: Integer;
  buf: PChar; buflen: Integer): HANDLE; stdcall;
function WSAAsyncGetHostByName(hWnd: HWND; wMsg: u_int; name, buf: PChar;
  buflen: Integer): HANDLE; stdcall;
function WSAAsyncGetHostByAddr(hWnd: HWND; wMsg: u_int; addr: PChar;
  len, type_: Integer; buf: PChar; buflen: Integer): HANDLE; stdcall;
function WSACancelAsyncRequest(hAsyncTaskHandle: HANDLE): Integer; stdcall;
function WSAAsyncSelect(s: TSocket; hWnd: HWND; wMsg: u_int; lEvent: Longint): Integer; stdcall;

function WSARecvEx(s: TSocket; var buf; len: Integer; var flags: Integer): Integer; stdcall;

type
  _TRANSMIT_FILE_BUFFERS = record
    Head: LPVOID;
    HeadLength: DWORD;
    Tail: LPVOID;
    TailLength: DWORD;
  end;
  TRANSMIT_FILE_BUFFERS = _TRANSMIT_FILE_BUFFERS;
  PTRANSMIT_FILE_BUFFERS = ^TRANSMIT_FILE_BUFFERS;
  LPTRANSMIT_FILE_BUFFERS = ^TRANSMIT_FILE_BUFFERS;
  TTransmitFileBuffers = TRANSMIT_FILE_BUFFERS;
  PTransmitFileBuffers = LPTRANSMIT_FILE_BUFFERS;

const
  TF_DISCONNECT           = $01;
  TF_REUSE_SOCKET         = $02;
  TF_WRITE_BEHIND         = $04;

function TransmitFile(hSocket: TSocket; hFile: HANDLE; nNumberOfBytesToWrite: DWORD;
  nNumberOfBytesPerSend: DWORD; lpOverlapped: LPOVERLAPPED;
  lpTransmitBuffers: LPTRANSMIT_FILE_BUFFERS; dwReserved: DWORD): BOOL; stdcall;
function AcceptEx(sListenSocket, sAcceptSocket: TSocket;
  lpOutputBuffer: LPVOID; dwReceiveDataLength, dwLocalAddressLength,
  dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD;
  lpOverlapped: LPOVERLAPPED): BOOL; stdcall;
procedure GetAcceptExSockaddrs(lpOutputBuffer: Pointer;
  dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
  out LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
  out RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;

(* Microsoft Windows Extended data types *)

type
  PSOCKADDR_IN = ^sockaddr_in;
  LPSOCKADDR_IN = ^sockaddr_in;
  LPLINGER = PLINGER;
  PIN_ADDR = ^in_addr;
  LPIN_ADDR = ^in_addr;
  PFD_SET = ^fd_set;
  LPFD_SET = ^fd_set;
  LPHOSTENT = PHOSTENT;
  LPSERVENT = PSERVENT;
  LPPROTOENT = PPROTOENT;
  LPTIMEVAL = PTIMEVAL;

(*
 * Windows message parameter composition and decomposition
 * macros.
 *
 * WSAMAKEASYNCREPLY is intended for use by the Windows Sockets implementation
 * when constructing the response to a WSAAsyncGetXByY() routine.
 *)

function WSAMAKEASYNCREPLY(buflen, error: WORD): DWORD;

(*
 * WSAMAKESELECTREPLY is intended for use by the Windows Sockets implementation
 * when constructing the response to WSAAsyncSelect().
 *)

function WSAMAKESELECTREPLY(event, error: WORD): DWORD;

(*
 * WSAGETASYNCBUFLEN is intended for use by the Windows Sockets application
 * to extract the buffer length from the lParam in the response
 * to a WSAGetXByY().
 *)

function WSAGETASYNCBUFLEN(lParam: DWORD): WORD;

(*
 * WSAGETASYNCERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAGetXByY().
 *)

function WSAGETASYNCERROR(lParam: DWORD): WORD;

(*
 * WSAGETSELECTEVENT is intended for use by the Windows Sockets application
 * to extract the event code from the lParam in the response
 * to a WSAAsyncSelect().
 *)

function WSAGETSELECTEVENT(lParam: DWORD): WORD;

(*
 * WSAGETSELECTERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAAsyncSelect().
 *)

function WSAGETSELECTERROR(lParam: DWORD): WORD;

implementation

const
  wsock32 = 'wsock32.dll';


{$IFDEF DYNAMIC_LINK}
var
  ___WSAFDIsSet: Pointer;

function __WSAFDIsSet;
begin
  GetProcedureAddress(___WSAFDIsSet, wsock32, '__WSAFDIsSet');
  asm
    mov esp, ebp
    pop ebp
    jmp [___WSAFDIsSet]
  end;
end;
{$ELSE}
function __WSAFDIsSet; external wsock32 name '__WSAFDIsSet';
{$ENDIF DYNAMIC_LINK}

procedure FD_CLR(fd: TSocket; var fdset: TFdSet);
var
  I: Cardinal;
begin
  I := 0;
  while I < fdset.fd_count do
  begin
    if fdset.fd_array[I] = fd then
    begin
      while I < fdset.fd_count - 1 do
      begin
        fdset.fd_array[I] := fdset.fd_array[I + 1];
        Inc(I);
      end;
      fdset.fd_count := fdset.fd_count - 1;
      Break;
    end;
    Inc(I);
  end;
end;

procedure _FD_SET(fd: TSocket; var fdset: TFDSet);
var
  I: Cardinal;
begin
  I := 0;
  while I < fdset.fd_count do
  begin
    if fdset.fd_array[I] = fd then Break;
    Inc(I);
  end;
  if I = fdset.fd_count then
  begin
    if fdset.fd_count < FD_SETSIZE then
    begin
      fdset.fd_array[I] := fd;
      fdset.fd_count := fdset.fd_count + 1;
    end;
  end;
end;

procedure FD_ZERO(var fdset: TFdSet);
begin
  fdset.fd_count := 0;
end;

function FD_ISSET(fd: TSocket; var fdset: TFdSet): Boolean;
begin
  Result := __WSAFDIsSet(fd, fdset) <> 0;
end;

function timerisset(const tvp: TTimeVal): Boolean;
begin
  Result := (tvp.tv_sec <> 0) or (tvp.tv_usec <> 0);
end;

procedure timerclear(var tvp: TTimeVal);
begin
  tvp.tv_sec := 0;
  tvp.tv_usec := 0;
end;

function _IO(x, y: DWORD): DWORD;
begin
  Result := IOC_VOID or (x shl 8) or y;
end;

function _IOR(x, y, t: DWORD): DWORD;
begin
  Result := IOC_OUT or ((T and IOCPARM_MASK) shl 16) or (x shl 8) or y;
end;

function _IOW(x, y, t: DWORD): DWORD;
begin
  Result := DWORD(IOC_IN or ((T and IOCPARM_MASK) shl 16) or (x shl 8) or y);
end;

function IN_CLASSA(i: DWORD): Boolean;
begin
  Result := i and DWORD($80000000) = 0;
end;

function IN_CLASSB(i: DWORD): Boolean;
begin
  Result := i and DWORD($C0000000) = DWORD($80000000);
end;

function IN_CLASSC(i: DWORD): Boolean;
begin
  Result := (i and DWORD($e0000000)) = DWORD($C0000000);
end;

function h_errno: Integer;
begin
  Result := WSAGetLastError;
end;


{$IFDEF DYNAMIC_LINK}
var
  _accept: Pointer;

function accept;
begin
  GetProcedureAddress(_accept, wsock32, 'accept');
  asm
    mov esp, ebp
    pop ebp
    jmp [_accept]
  end;
end;
{$ELSE}
function accept; external wsock32 name 'accept';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _bind: Pointer;

function bind;
begin
  GetProcedureAddress(_bind, wsock32, 'bind');
  asm
    mov esp, ebp
    pop ebp
    jmp [_bind]
  end;
end;
{$ELSE}
function bind; external wsock32 name 'bind';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _closesocket: Pointer;

function closesocket;
begin
  GetProcedureAddress(_closesocket, wsock32, 'closesocket');
  asm
    mov esp, ebp
    pop ebp
    jmp [_closesocket]
  end;
end;
{$ELSE}
function closesocket; external wsock32 name 'closesocket';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _connect: Pointer;

function connect;
begin
  GetProcedureAddress(_connect, wsock32, 'connect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_connect]
  end;
end;
{$ELSE}
function connect; external wsock32 name 'connect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ioctlsocket: Pointer;

function ioctlsocket;
begin
  GetProcedureAddress(_ioctlsocket, wsock32, 'ioctlsocket');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ioctlsocket]
  end;
end;
{$ELSE}
function ioctlsocket; external wsock32 name 'ioctlsocket';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getpeername: Pointer;

function getpeername;
begin
  GetProcedureAddress(_getpeername, wsock32, 'getpeername');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getpeername]
  end;
end;
{$ELSE}
function getpeername; external wsock32 name 'getpeername';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getsockname: Pointer;

function getsockname;
begin
  GetProcedureAddress(_getsockname, wsock32, 'getsockname');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getsockname]
  end;
end;
{$ELSE}
function getsockname; external wsock32 name 'getsockname';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getsockopt: Pointer;

function getsockopt;
begin
  GetProcedureAddress(_getsockopt, wsock32, 'getsockopt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getsockopt]
  end;
end;
{$ELSE}
function getsockopt; external wsock32 name 'getsockopt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _htonl: Pointer;

function htonl;
begin
  GetProcedureAddress(_htonl, wsock32, 'htonl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_htonl]
  end;
end;
{$ELSE}
function htonl; external wsock32 name 'htonl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _htons: Pointer;

function htons;
begin
  GetProcedureAddress(_htons, wsock32, 'htons');
  asm
    mov esp, ebp
    pop ebp
    jmp [_htons]
  end;
end;
{$ELSE}
function htons; external wsock32 name 'htons';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _inet_addr: Pointer;

function inet_addr;
begin
  GetProcedureAddress(_inet_addr, wsock32, 'inet_addr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_inet_addr]
  end;
end;
{$ELSE}
function inet_addr; external wsock32 name 'inet_addr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _inet_ntoa: Pointer;

function inet_ntoa;
begin
  GetProcedureAddress(_inet_ntoa, wsock32, 'inet_ntoa');
  asm
    mov esp, ebp
    pop ebp
    jmp [_inet_ntoa]
  end;
end;
{$ELSE}
function inet_ntoa; external wsock32 name 'inet_ntoa';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _listen: Pointer;

function listen;
begin
  GetProcedureAddress(_listen, wsock32, 'listen');
  asm
    mov esp, ebp
    pop ebp
    jmp [_listen]
  end;
end;
{$ELSE}
function listen; external wsock32 name 'listen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ntohl: Pointer;

function ntohl;
begin
  GetProcedureAddress(_ntohl, wsock32, 'ntohl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ntohl]
  end;
end;
{$ELSE}
function ntohl; external wsock32 name 'ntohl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ntohs: Pointer;

function ntohs;
begin
  GetProcedureAddress(_ntohs, wsock32, 'ntohs');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ntohs]
  end;
end;
{$ELSE}
function ntohs; external wsock32 name 'ntohs';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _recv: Pointer;

function recv;
begin
  GetProcedureAddress(_recv, wsock32, 'recv');
  asm
    mov esp, ebp
    pop ebp
    jmp [_recv]
  end;
end;
{$ELSE}
function recv; external wsock32 name 'recv';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _recvfrom: Pointer;

function recvfrom;
begin
  GetProcedureAddress(_recvfrom, wsock32, 'recvfrom');
  asm
    mov esp, ebp
    pop ebp
    jmp [_recvfrom]
  end;
end;
{$ELSE}
function recvfrom; external wsock32 name 'recvfrom';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _select: Pointer;

function select;
begin
  GetProcedureAddress(_select, wsock32, 'select');
  asm
    mov esp, ebp
    pop ebp
    jmp [_select]
  end;
end;
{$ELSE}
function select; external wsock32 name 'select';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _send: Pointer;

function send;
begin
  GetProcedureAddress(_send, wsock32, 'send');
  asm
    mov esp, ebp
    pop ebp
    jmp [_send]
  end;
end;
{$ELSE}
function send; external wsock32 name 'send';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _sendto: Pointer;

function sendto;
begin
  GetProcedureAddress(_sendto, wsock32, 'sendto');
  asm
    mov esp, ebp
    pop ebp
    jmp [_sendto]
  end;
end;
{$ELSE}
function sendto; external wsock32 name 'sendto';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _setsockopt: Pointer;

function setsockopt;
begin
  GetProcedureAddress(_setsockopt, wsock32, 'setsockopt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_setsockopt]
  end;
end;
{$ELSE}
function setsockopt; external wsock32 name 'setsockopt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _shutdown: Pointer;

function shutdown;
begin
  GetProcedureAddress(_shutdown, wsock32, 'shutdown');
  asm
    mov esp, ebp
    pop ebp
    jmp [_shutdown]
  end;
end;
{$ELSE}
function shutdown; external wsock32 name 'shutdown';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _socket: Pointer;

function socket;
begin
  GetProcedureAddress(_socket, wsock32, 'socket');
  asm
    mov esp, ebp
    pop ebp
    jmp [_socket]
  end;
end;
{$ELSE}
function socket; external wsock32 name 'socket';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _gethostbyaddr: Pointer;

function gethostbyaddr;
begin
  GetProcedureAddress(_gethostbyaddr, wsock32, 'gethostbyaddr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_gethostbyaddr]
  end;
end;
{$ELSE}
function gethostbyaddr; external wsock32 name 'gethostbyaddr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _gethostbyname: Pointer;

function gethostbyname;
begin
  GetProcedureAddress(_gethostbyname, wsock32, 'gethostbyname');
  asm
    mov esp, ebp
    pop ebp
    jmp [_gethostbyname]
  end;
end;
{$ELSE}
function gethostbyname; external wsock32 name 'gethostbyname';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _gethostname: Pointer;

function gethostname;
begin
  GetProcedureAddress(_gethostname, wsock32, 'gethostname');
  asm
    mov esp, ebp
    pop ebp
    jmp [_gethostname]
  end;
end;
{$ELSE}
function gethostname; external wsock32 name 'gethostname';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getservbyport: Pointer;

function getservbyport;
begin
  GetProcedureAddress(_getservbyport, wsock32, 'getservbyport');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getservbyport]
  end;
end;
{$ELSE}
function getservbyport; external wsock32 name 'getservbyport';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getservbyname: Pointer;

function getservbyname;
begin
  GetProcedureAddress(_getservbyname, wsock32, 'getservbyname');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getservbyname]
  end;
end;
{$ELSE}
function getservbyname; external wsock32 name 'getservbyname';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getprotobynumber: Pointer;

function getprotobynumber;
begin
  GetProcedureAddress(_getprotobynumber, wsock32, 'getprotobynumber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getprotobynumber]
  end;
end;
{$ELSE}
function getprotobynumber; external wsock32 name 'getprotobynumber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _getprotobyname: Pointer;

function getprotobyname;
begin
  GetProcedureAddress(_getprotobyname, wsock32, 'getprotobyname');
  asm
    mov esp, ebp
    pop ebp
    jmp [_getprotobyname]
  end;
end;
{$ELSE}
function getprotobyname; external wsock32 name 'getprotobyname';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _WSAStartup: Pointer;

function WSAStartup;
begin
  GetProcedureAddress(_WSAStartup, wsock32, 'WSAStartup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAStartup]
  end;
end;
{$ELSE}
function WSAStartup; external wsock32 name 'WSAStartup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSACleanup: Pointer;

function WSACleanup;
begin
  GetProcedureAddress(_WSACleanup, wsock32, 'WSACleanup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSACleanup]
  end;
end;
{$ELSE}
function WSACleanup; external wsock32 name 'WSACleanup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSASetLastError: Pointer;

procedure WSASetLastError;
begin
  GetProcedureAddress(_WSASetLastError, wsock32, 'WSASetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSASetLastError]
  end;
end;
{$ELSE}
procedure WSASetLastError; external wsock32 name 'WSASetLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAGetLastError: Pointer;

function WSAGetLastError;
begin
  GetProcedureAddress(_WSAGetLastError, wsock32, 'WSAGetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAGetLastError]
  end;
end;
{$ELSE}
function WSAGetLastError; external wsock32 name 'WSAGetLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAIsBlocking: Pointer;

function WSAIsBlocking;
begin
  GetProcedureAddress(_WSAIsBlocking, wsock32, 'WSAIsBlocking');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAIsBlocking]
  end;
end;
{$ELSE}
function WSAIsBlocking; external wsock32 name 'WSAIsBlocking';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAUnhookBlockingHook: Pointer;

function WSAUnhookBlockingHook;
begin
  GetProcedureAddress(_WSAUnhookBlockingHook, wsock32, 'WSAUnhookBlockingHook');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAUnhookBlockingHook]
  end;
end;
{$ELSE}
function WSAUnhookBlockingHook; external wsock32 name 'WSAUnhookBlockingHook';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSASetBlockingHook: Pointer;

function WSASetBlockingHook;
begin
  GetProcedureAddress(_WSASetBlockingHook, wsock32, 'WSASetBlockingHook');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSASetBlockingHook]
  end;
end;
{$ELSE}
function WSASetBlockingHook; external wsock32 name 'WSASetBlockingHook';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSACancelBlockingCall: Pointer;

function WSACancelBlockingCall;
begin
  GetProcedureAddress(_WSACancelBlockingCall, wsock32, 'WSACancelBlockingCall');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSACancelBlockingCall]
  end;
end;
{$ELSE}
function WSACancelBlockingCall; external wsock32 name 'WSACancelBlockingCall';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetServByName: Pointer;

function WSAAsyncGetServByName;
begin
  GetProcedureAddress(_WSAAsyncGetServByName, wsock32, 'WSAAsyncGetServByName');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetServByName]
  end;
end;
{$ELSE}
function WSAAsyncGetServByName; external wsock32 name 'WSAAsyncGetServByName';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetServByPort: Pointer;

function WSAAsyncGetServByPort;
begin
  GetProcedureAddress(_WSAAsyncGetServByPort, wsock32, 'WSAAsyncGetServByPort');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetServByPort]
  end;
end;
{$ELSE}
function WSAAsyncGetServByPort; external wsock32 name 'WSAAsyncGetServByPort';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetProtoByName: Pointer;

function WSAAsyncGetProtoByName;
begin
  GetProcedureAddress(_WSAAsyncGetProtoByName, wsock32, 'WSAAsyncGetProtoByName');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetProtoByName]
  end;
end;
{$ELSE}
function WSAAsyncGetProtoByName; external wsock32 name 'WSAAsyncGetProtoByName';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetProtoByNumber: Pointer;

function WSAAsyncGetProtoByNumber;
begin
  GetProcedureAddress(_WSAAsyncGetProtoByNumber, wsock32, 'WSAAsyncGetProtoByNumber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetProtoByNumber]
  end;
end;
{$ELSE}
function WSAAsyncGetProtoByNumber; external wsock32 name 'WSAAsyncGetProtoByNumber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetHostByName: Pointer;

function WSAAsyncGetHostByName;
begin
  GetProcedureAddress(_WSAAsyncGetHostByName, wsock32, 'WSAAsyncGetHostByName');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetHostByName]
  end;
end;
{$ELSE}
function WSAAsyncGetHostByName; external wsock32 name 'WSAAsyncGetHostByName';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncGetHostByAddr: Pointer;

function WSAAsyncGetHostByAddr;
begin
  GetProcedureAddress(_WSAAsyncGetHostByAddr, wsock32, 'WSAAsyncGetHostByAddr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncGetHostByAddr]
  end;
end;
{$ELSE}
function WSAAsyncGetHostByAddr; external wsock32 name 'WSAAsyncGetHostByAddr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSACancelAsyncRequest: Pointer;

function WSACancelAsyncRequest;
begin
  GetProcedureAddress(_WSACancelAsyncRequest, wsock32, 'WSACancelAsyncRequest');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSACancelAsyncRequest]
  end;
end;
{$ELSE}
function WSACancelAsyncRequest; external wsock32 name 'WSACancelAsyncRequest';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WSAAsyncSelect: Pointer;

function WSAAsyncSelect;
begin
  GetProcedureAddress(_WSAAsyncSelect, wsock32, 'WSAAsyncSelect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSAAsyncSelect]
  end;
end;
{$ELSE}
function WSAAsyncSelect; external wsock32 name 'WSAAsyncSelect';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _WSARecvEx: Pointer;

function WSARecvEx;
begin
  GetProcedureAddress(_WSARecvEx, wsock32, 'WSARecvEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WSARecvEx]
  end;
end;
{$ELSE}
function WSARecvEx; external wsock32 name 'WSARecvEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TransmitFile: Pointer;

function TransmitFile;
begin
  GetProcedureAddress(_TransmitFile, wsock32, 'TransmitFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TransmitFile]
  end;
end;
{$ELSE}
function TransmitFile; external wsock32 name 'TransmitFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AcceptEx: Pointer;

function AcceptEx;
begin
  GetProcedureAddress(_AcceptEx, wsock32, 'AcceptEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AcceptEx]
  end;
end;
{$ELSE}
function AcceptEx; external wsock32 name 'AcceptEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAcceptExSockaddrs: Pointer;

procedure GetAcceptExSockaddrs;
begin
  GetProcedureAddress(_GetAcceptExSockaddrs, wsock32, 'GetAcceptExSockaddrs');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAcceptExSockaddrs]
  end;
end;
{$ELSE}
procedure GetAcceptExSockaddrs; external wsock32 name 'GetAcceptExSockaddrs';
{$ENDIF DYNAMIC_LINK}

function WSAMAKEASYNCREPLY(buflen, error: WORD): DWORD;
begin
  Result := MAKELONG(buflen, error);
end;

function WSAMAKESELECTREPLY(event, error: WORD): DWORD;
begin
  Result := MAKELONG(event, error);
end;

function WSAGETASYNCBUFLEN(lParam: DWORD): WORD;
begin
  Result := LOWORD(lParam);
end;

function WSAGETASYNCERROR(lParam: DWORD): WORD;
begin
  Result := HIWORD(lParam);
end;

function WSAGETSELECTEVENT(lParam: DWORD): WORD;
begin
  Result := LOWORD(lParam);
end;

function WSAGETSELECTERROR(lParam: DWORD): WORD;
begin
  Result := HIWORD(lParam);
end;

end.
