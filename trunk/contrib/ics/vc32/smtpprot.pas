{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       Fran�ois PIETTE
Object:       TSmtpCli class implements the SMTP protocol (RFC-821)
              Support file attachement using MIME format (RFC-1521, RFC-2045)
              Support authentification (RFC-2104)
              Support HTML mail with embedded images.
Creation:     09 october 1997
Version:      2.38
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2003 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

How to use the HTML feature:
    An HTML mail message is composed of 3 parts: an HTML formatted message,
    a plain text message and a collection of document files which may be
    reference in the HTML (for example images).

    The HTML mail message is formatted by the THtmlSmtpCli component using the
    MIME multipart format. There are two level of parts. An external level
    with the message as part one and the documents in one or more parts.
    The first part is itself a multipart message. The first part in the first
    part is the plain text message, the second part in the first part is the
    HTML message.

    So to build a correct HTML mail message, you have to supply an ascii text
    for the plain text part (PlainText property), a HTML message (HtmlText
    property) and zero or more document filenames (EmailFiles property).

    In the HTML part, you may reference the files (for example images) using
    the special URL beginning with 'cid:'. For example to include an image,
    you use: <IMG SRC="cid:IMAGE1">. And the image file has to be the first
    file listed in EmailFiles property. The second image would be referenced
    <IMG SRC="cid:IMAGE2">, and so on. It is always "cid:IMAGEn" with n
    replaced by the position in EmailFiles property.

    If you wants to have nromal attached files, just put them and the end
    of EmailFiles list. They will be shown as attached files.

Updates:
Oct 25, 1997  Added the OnHeaderLine event to allow modification/deletion of
              header lines.
Oct 26, 1997  V1.00 Released
              Changed the OnGetData event arguments to have code compatible
              between 16 and 32 bit versions (replaced string with PChar).
Jan 10, 1998  V1.01 Added a Port property
Feb 14, 1998  V1.02 Added an intermeditae TCustomSmtpClient in order to
              support MIME in the TSmtpCli. I implemented MIME with the
              help of code donated by Brad Choate <choate@delphiexchange.com>
              Mime is used for file attachement.
              Added a SetRcptName to copy values from a string list in place
              of copying the string list reference.
Feb 15, 1998  V1.03 Added a CharSet property, defaulting to iso-8859-1
Mar 02, 1998  V1.04 Corrected result for QUIT command.
              Marcus Schmutz <schmutz@kwsoft.de>
Mar 06, 1998  V1.05 Use OnDataSent event to prenvent over-buffering
Mar 15, 1998  V1.06 Implemented the Date header line
Apr 01, 1998  V1.07 Adapted for BCB V3
Apr 10, 1998  V1.08 Corrected DayNames: sunday is day 1, saturday is day 7.
              Changed UUEncode procedures to virtual methods to ease component
              inheritance.
Apr 26, 1998  V1.09 Ignore any empty file name (a very common error !)
              Check if file exists and raise an exception if not.
              Made Rfc822DateTime public.
              Added Rset method from Victor Garcia Aprea <vga@overnet.com.ar>
              Added Abort procedure to close the socket and abort any operation
              Made the underlaying TWSocket accessible using a property.
Apr 28, 1998  V1.10 Reset FTimeOutFlag in the mail procedure.
May 05, 1998  V1.11 Handled correctly lines beginning with a dot.
May 21, 1998  V1.12 Check for nil argument in SetEMailFiles
              Added OnCommand and OnResponse events.
              Added SendDataLine procedure (same as SendCommand, but do not
              trigger OnCommand event) used for header and message lines.
Jul 29, 1998  V2.00 Asynchronous functions and new TSyncSmtpCli component
              to be a placer holder for synchronous version.
              Renamed source file from SmtpCli to SmtpProt.
Aug 06, 1998  V2.01 Made HighLevelAsync public and added smtpCustom to be used
              for custom calls to HighLevelAsync.
Sep 22, 1998  V2.02 Removed useless Wait unit from the uses clause.
Oct 04, 1998  V2.03 Checked for Error in TriggerRequestDone.
Oct 11, 1998  V2.04 Removed -1 in DataNext. Thanks to Dennis V. Turov
              <chip@quorum.ru> for finding this bug.
Nov 22, 1998  V2.05 Implemented VRFY command with code proposed by
              DZ-Jay <dz@caribe.net> but use HdrTo property as name to verify.
Nov 29, 1998  V2.06 Added SetErrorMessage in WSocketSessionConnected when an
              error occured. Thanks to DZ-Jay.
              Changed FMimeBoundary format to use numbered month instead of
              month names. Thanks to Dmitry Kislov <kislov@tekom.odessa.ua> who
              found that some foreign charsets are invalid in mime boundaries.
Dec 22, 1998  V2.07 Handle exception when connecting (will be triggered when
              an invalid port has been given).
              Force readonly when reading attached files.
              Added ContentType property as suggested by Henri Fournier
              <hfournier@home.com>
Feb 13, 1999  V2.08 Published the state property and OnSessionConnected,
              OnSessionClosed events.
Feb 27, 1999  V2.09 Added Connected property.
              Added code from Larry Pesyna <ldpesyna@aep.com> to handle time
              zone bias.
              Added OnAttachContentType event. Thanks to Vladimir M.
              Zakharychev <zak@dzbjaro.bertelsmann.de> for his suggestion.
              Added ReplyTo and ReturnPath properties. Thanks to Eric Bullen
              <eric@thedeepsky.com> for his code.
Mar 06, 1999  V2.10 Conditional compile to remove timezone code unsupported by
              Delphi 1.
Mar 09, 1999  V2.11 Made state property [really] published.
Mar 27, 1999  V2.12 Published OnProcessHeader
              Changed sign for time zone bias (thanks to Larry Pesyna).
May 10, 1999  V2.13 'daylight' functionality for timezonebias function.
              Thanks to Bernhard Goebel <Bernhard.Goebel@t-online.de>
              Do not set FRequestType in Connect when called from HighLevel
              function. Thanks to Eugene V. Krapivin <evk@tagil.ru>.
May 18, 1999  V2.14 Added Sender field.  If ommited, the sender is becomes
              HdrFrom. Jon Glazer <jglazer@adconn.com>
Jul 30, 1999  V2.15 Added MailMessage property by Thomas Kvamme
              <thokvamm@online.no>. MailMessage property can be used with
              OnGetData event. If both are used, MailMessages lines appears
              before lines got by OnGetData.
Oct 02, 1999  V2.16 Added OnAttachHeader event as suggested by Vladimir M.
              Zakharychev <zak@dzbjaro.bertelsmann.de>
              Accept friendly EMail addresses. Thanks to Thierry De Leeuw
              <thierry.deleeuw@proxis.be> for his code.
Nov 01, 1999  V2.17 Made all fields protected to easy component inheritance.
Oct 15, 2000  V2.18 Check for too long lines in TriggerGetData.
              Thanks to Steve Williams <stevewilliams@kromestudios.com>
Jun 18, 2001  V2.19 Use AllocateHWnd and DeallocateHWnd from wsocket.
              Renamed property WSocket to CtrlSocket (this require code change
              in user application too).
Jul 26, 2001  V2.20  Angus Robertson <angus@magsys.co.uk> found a problem when
              using the MailSync method that it's not possible to send a body
              that takes longer than the timeout in WaitUntilReady. Timeout has
              to be reevaluated in TriggerGetData.
              Jake Traynham <jake@comm-unity.net> added authentification and
              EHLO code. Well done job.
Aug 18, 2001  V2.21 Angus V2.21 added OwnHeaders property flag which allows
              mail relaying where the body includes all headers and none are
              added by the component
Sep 09, 2001  V2.22 Beat Boegli <leeloo999@bluewin.ch> added LocalAddr property
              for multihomed hosts.
Dec 24, 2001  V2.23 Added support for NOFORMS (console mode without Forms unit).
              Added X-Mailer header line.
Jan 09, 2001  V2.24 Corrected WSocketDnsLookupDone where FRequestResult was not
              properly set with errorcode. Corrected DoHighLevelAsync to set
              RequestResult to 426 when aborting. Found by "SJF" <bcb@daqing.net>.
Mar 17, 2002  V2.25 Check for FRequestType = smtpQuit in NextExecAsync to avoid
              calling OnRequestDone before remote server has closed connection.
              And in WSocketSessionClosed, check if last command was smtpQuit
              to select proper error code for OnRequestDone event.
              Lot of peoples helped find this one (Alphabetical order):
              David Colliver <david.colliver@revilloc.com>
              DZ-Jay <dz@caribe.net>
              Max Terentiev <support@nexus6.ru>
              Roger Morton <roger@chez-morton.com>
              Wilfried Mestdagh <wilfried@mestdagh.biz>
Apr 01, 2002  V2.26 TriggerRequestDone with correct winsock error in
              WSocketDnsLookupDone. Thanks to DZ [dz@caribe.net] and
              Roger Morton [roger@chez-morton.com] for fixing this bug.
Apr 20, 2002  V2.27 Enhance NOFORMS mode.
Apr 24, 2002  V2.28 Return real error code in case of error in
              WSocketDnsLookupDone. Thanks to DZ-Jay <dz@caribe.net>.
Sep 07, 2002  V2.29 Added HdrCc property to send mail to CC.
Oct 26, 2002  V2.30 Revised Rfc822DateTime function.
Nov 01, 2002  V2.31 Changed arguments for event from PChar to Pointer to avoid
              Delphi 7 bug with PCHar <-> AnsiChar. This will require small
              changes in your application: change PChar to Pointer in your
              event handler and probably add a PChar cast when using the args.
Nov 11, 2002  V2.32 Revised for Delphi 1
Apr 08, 2003  V2.33 Arno Garrels <arno.garrels@gmx.de> made some useful
              changes:
              ThreadDetach/ThreadAttach added.
              Fixed: A possibly open file handle was not closed on abnormal
              termination (I'm not quite happy with this fix).
              AUTH AutoSelection added.
              Global var FileMode is not thread-safe.
              Modified UUEncoding in a way that it uses a TFileStream.
              New property ShareMode was added.
              Made AuthTypesSupported public.
Apr 18, 2003  V2.34 added THtmlSmtpCli to send HTML formatted mail with
              embedded images or documents.
Apr 19, 2003  V2.35 Arno Garrels <arno.garrels@gmx.de> made some useful changes:
              As Jake Traynham <jake@comm-unity.net> suggested, in case of
              AuthType is set to smtpAuthAutoSelect the component tries each
              of the AuthTypes one after the other until one works (or not).
              That should make it closer to RFC2554. Added X-Priority header,
              proc FoldHdrLine (Reply-To, To, CC headers are *folded* now,
              see TCustomSmtpClient.Data, accoording to RFC822), modified
              Base64 encoded line length from 60 to 72 chars.
Apr 19, 2003  V2.36 Added RcptNameAdd procedure.
              Added ParseEmail function.
              Added Priority (RFC1327) and X-MSMail-Priority header line.
              Added smtpPriorityNone.
              Replaced all spaces in FSignon by underscore before sending.
May 03, 2003  V2.37 Moved MIME routines to MimeUtil unit.
              Renamed UUEncode to EncFileBase64 because it was really base64
              encoding. The routines are now in MimeUtil.
              Break quoted-printable at column 76 as stated in RFC2045
May 09, 2003  V2.38 Revised FoldHdrLine and RcptNameAdd to accept both coma
              and semicolon as email addresses separators.
              Thanks to Arno Garrels <arno.garrels@gmx.de> for his help.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit SmtpProt;

interface

{$i icsdef.inc}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} { C++ Builder V5.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER140} { C++ Builder V6.0                    }
    {$ObjExportAll On}
{$ENDIF}

uses
    Messages,{$ifdef usewindows} Windows,{$else} WinTypes, WinProcs, {$endif}SysUtils, Classes,
{$IFNDEF NOFORMS}
    Forms, Controls,
{$ENDIF}
    WSocket, WinSock, MD5, MimeUtil;

const
  SmtpCliVersion     = 238;
  CopyRight : String = ' SMTP component (c) 1997-2003 Francois Piette V2.38 ';
{$IFDEF VER80}
  { Delphi 1 has a 255 characters string limitation }
  SMTP_RCV_BUF_SIZE = 255;
{$ELSE}
  SMTP_RCV_BUF_SIZE = 4096;
{$ENDIF}
  WM_SMTP_REQUEST_DONE = WM_USER + 1;

type
    SmtpException    = class(Exception);
    TSmtpState       = (smtpReady,           smtpDnsLookup,
                        smtpConnecting,      smtpConnected,
                        smtpInternalReady,   smtpWaitingBanner,
                        smtpWaitingResponse, smtpAbort);
    TSmtpMimeState   = (smtpMimeIntro,       smtpMimePlainText,
                        smtpMimeHtmlText,    smtpMimeImages,    smtpMimeDone);
    TSmtpRequest     = (smtpConnect,         smtpHelo,          smtpMailFrom,
                        smtpVrfy,            smtpRcptTo,        smtpData,
                        smtpQuit,            smtpRset,          smtpOpen,
                        smtpMail,            smtpEhlo,          smtpAuth,
                        smtpCustom);
    TSmtpFct         = (smtpFctNone,         smtpFctHelo,       smtpFctConnect,
                        smtpFctMailFrom,     smtpFctRcptTo,     smtpFctData,
                        smtpFctVrfy,         smtpFctQuit,       smtpFctRset,
                        smtpFctEhlo,         smtpFctAuth);
    TSmtpFctSet      = set of TSmtpFct;
    TSmtpContentType = (smtpHtml,            smtpPlainText);
    TSmtpAuthType    = (smtpAuthNone,        smtpAuthPlain,     smtpAuthLogin,
                        smtpAuthCramMD5,     smtpAuthAutoSelect);
    TSmtpShareMode   = (smtpShareCompat,     smtpShareExclusive,
                        smtpShareDenyWrite,  smtpShareDenyRead,
                        smtpShareDenyNone);
    TSmtpPriority    = (smtpPriorityNone,    smtpPriorityHighest,
                        smtpPriorityHigh,    smtpPriorityNormal,
                        smtpPriorityLow,     smtpPriorityLowest);
    TSmtpDisplay               = procedure(Sender  : TObject;
                                           Msg     : String) of object;
    TSmtpHeaderLineEvent       = procedure(Sender  : TObject;
                                           Msg     : Pointer;
                                           Size    : Integer) of object;
    TSmtpProcessHeaderEvent    = procedure(Sender  : TObject;
                                           HdrLines  : TStrings) of object;
    TSmtpGetDataEvent          = procedure(Sender  : TObject;
                                           LineNum : Integer;
                                           MsgLine : Pointer;
                                           MaxLen  : Integer;
                                           var More: Boolean) of object;
    TSmtpRequestDone           = procedure(Sender    : TObject;
                                           RqType    : TSmtpRequest;
                                           ErrorCode : Word) of object;
    TSmtpAttachmentContentType = procedure(Sender          : TObject;
                                           FileNumber      : Integer;
                                           var FileName    : String;
                                           var ContentType : String) of object;
    TSmtpAttachHeader          = procedure(Sender          : TObject;
                                           FileNumber      : Integer;
                                           FileName        : String;
                                           HdrLines        : TStrings) of object;
    TSmtpNextProc              = procedure of object;

    { Base component, implementing the transport, without MIME support }
    TCustomSmtpClient = class(TComponent)
    protected
        FWSocket             : TWSocket;     { Underlaying socket          }
        FHost                : String;       { SMTP server hostname or IP  }
        FLocalAddr           : String; {bb}  { Local Address for mulithome }
        FPort                : String;       { Should be 'smtp'            }
        FSignOn              : String;       { Used for the 'HELO' command }
        FUsername            : String;       { Used with the 'AUTH' command }
        FPassword            : String;       { Used with the 'AUTH' command }
        FAuthType            : TSmtpAuthType;{ Used with the 'AUTH' command }
        FAuthTypesSupported  : TStrings;     { AuthTypes supported by server}
        FFromName            : String;       { Sender's EMail               }
        FRcptName            : TStrings;     { Recepients EMails list       }
        FMailMessage         : TStrings;
        FHdrFrom             : String;
        FHdrTo               : String;
        FHdrCc               : String;
        FHdrReplyTo          : String;
        FHdrReturnPath       : String;
        FHdrSubject          : String;
        FHdrSender           : String;       { Mail Sender's Email          }
        FHdrPriority         : TSmtpPriority;
        FState               : TSmtpState;
        FCharSet             : String;
        FContentType         : TSmtpContentType;
        FContentTypeStr      : String;
        FLastResponse        : String;
        FErrorMessage        : String;
        FTag                 : LongInt;
        FConnected           : Boolean;
        FESmtpSupported      : Boolean;
        FRequestType         : TSmtpRequest;
        FRequestDoneFlag     : Boolean;
        FReceiveLen          : Integer;
        FRequestResult       : Integer;
        FStatusCode          : Integer;
        FReceiveBuffer       : array [0..SMTP_RCV_BUF_SIZE - 1] of char;
        FNext                : TSmtpNextProc;
        FWhenConnected       : TSmtpNextProc;
        FFctSet              : TSmtpFctSet;
        FFctPrv              : TSmtpFct;
        FHighLevelResult     : Integer;
        FHighLevelFlag       : Boolean;
        FNextRequest         : TSmtpNextProc;
        FLastResponseSave    : String;
        FStatusCodeSave      : Integer;
        FRestartFlag         : Boolean;
        FOkResponses         : array [0..15] of Integer;
        FDoneAsync           : TSmtpNextProc;
        FWindowHandle        : HWND;
        FItemCount           : LongInt;
        FHdrLines            : TStrings;
        FLineNum             : Integer;
        FMoreLines           : Boolean;
        FOwnHeaders          : Boolean ;  { Angus V2.21 }
        FOnDisplay           : TSmtpDisplay;
        FOnCommand           : TSmtpDisplay;
        FOnResponse          : TSmtpDisplay;
        FOnGetData           : TSmtpGetDataEvent;
        FOnHeaderLine        : TSmtpHeaderLineEvent;
        FOnProcessHeader     : TSmtpProcessHeaderEvent;
        FOnRequestDone       : TSmtpRequestDone;
        FOnStateChange       : TNotifyEvent;
        FOnSessionConnected  : TSessionConnected;
        FOnSessionClosed     : TSessionClosed;
        FStream              : TStream;
        FShareMode           : Word;
        function    SmtpClientAllocateHWnd(Method: TWndMethod): HWND;
        procedure   AuthGetType;          { parse Ehlo response for AuthTypes }
        procedure   SetShareMode(newValue: TSmtpShareMode);
        function    GetShareMode: TSmtpShareMode;
        procedure   SmtpClientDeallocateHWnd(WHandle: HWND);
        procedure   TriggerDisplay(Msg : String); virtual;
        procedure   TriggerCommand(Msg : String); virtual;
        procedure   TriggerResponse(Msg : String); virtual;
        procedure   TriggerRequestDone(ErrorCode : Word); virtual;
        procedure   TriggerStateChange; virtual;
        procedure   TriggerGetData(LineNum  : Integer;
                                   MsgLine  : Pointer;
                                   MaxLen   : Integer;
                                   var More : Boolean); virtual;
        procedure   TriggerHeaderLine(Line : Pointer; Size : Integer); virtual;
        procedure   TriggerProcessHeader(HdrLines : TStrings); virtual;
        procedure   TriggerSessionConnected(ErrorCode : Word); virtual;
        procedure   TriggerSessionClosed(ErrorCode : Word); virtual;
        procedure   ClearErrorMessage;
        procedure   SetErrorMessage;
        procedure   StateChange(NewState : TSmtpState);
        procedure   SendCommand(Cmd : String); virtual;
        procedure   SetRcptName(newValue : TStrings);
        procedure   SetMailMessage(newValue : TStrings);
        procedure   CheckReady;
        procedure   WSocketDnsLookupDone(Sender: TObject; ErrorCode: Word);
        procedure   WSocketSessionConnected(Sender: TObject; ErrorCode: Word);
        procedure   WSocketDataAvailable(Sender: TObject; ErrorCode: Word);
        procedure   WSocketDataSent(Sender : TObject; ErrorCode : Word);
        procedure   WSocketSessionClosed(Sender : TObject; ErrorCode : WORD);
        procedure   DisplayLastResponse;
        procedure   DoHighLevelAsync;
        procedure   ExecAsync(RqType      : TSmtpRequest;
                              Cmd         : String;
                              OkResponses : array of Word;
                              DoneAsync   : TSmtpNextProc);
        procedure   NextExecAsync;
        procedure   EhloNext;
        procedure   DoAuthPlain;
        procedure   AuthNextPlain;
        procedure   AuthNextLogin;
        procedure   AuthNextLoginNext;
        procedure   AuthNextCramMD5;
        procedure   RcptToNext;
        procedure   RcptToDone;
        procedure   DataNext;
        procedure   WndProc(var MsgRec: TMessage); virtual;
        procedure   WMSmtpRequestDone(var msg: TMessage);
                        message WM_SMTP_REQUEST_DONE;

    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;
        procedure   Connect;  virtual;    { Connect to the mail server }
        procedure   Helo;     virtual;    { Send the HELO command      }
        procedure   Ehlo;     virtual;    { Send the EHLO command      }
        procedure   Auth;     virtual;    { Send the AUTH command      }
        procedure   Vrfy;     virtual;    { Send the VRFY command      }
        procedure   MailFrom; virtual;    { Send the MAILFROM command  }
        procedure   RcptTo;   virtual;    { Send RECPTTO command       }
        procedure   Data;     virtual;    { Send DATA command          }
        procedure   Quit;     virtual;    { Send QUITE command, close  }
        procedure   Rset;     virtual;    { Send RSET command          }
        procedure   Abort;    virtual;    { Abort opertaion, close     }
        procedure   Open;     virtual;    { Connect, Helo/Ehlo, Auth   }
        procedure   Mail;     virtual;    { MailFrom, RcptTo, Data     }
{$IFNDEF VER80}
        procedure   ThreadAttach;
        procedure   ThreadDetach;
{$ENDIF}
        property    ShareMode  : TSmtpShareMode      read  GetShareMode
                                                     write SetShareMode;
        property    CtrlSocket : TWSocket            read  FWSocket;
        property    Handle     : HWND                read  FWindowHandle;
        property    Connected  : Boolean             read  FConnected;
        procedure   HighLevelAsync(RqType : TSmtpRequest; Fcts : TSmtpFctSet);
        procedure   SetContentType(newValue : TSmtpContentType); virtual;
        procedure   RcptNameAdd(const ToList  : String;
                                const CcList  : String;
                                const BccList : String);
    protected
        property Host : String                       read  FHost
                                                     write FHost;
        property LocalAddr : String                  read  FLocalAddr  {bb}
                                                     write FLocalAddr; {bb}
        property Port : String                       read  FPort
                                                     write FPort;
        property SignOn : String                     read  FSignOn
                                                     write FSignOn;
        property Username : String                   read  FUsername
                                                     write FUsername;
        property Password : String                   read  FPassword
                                                     write FPassword;
        property AuthType : TSmtpAuthType            read  FAuthType
                                                     write FAuthType;
        property AuthTypesSupported : TStrings       read  FAuthTypesSupported;
        property FromName : String                   read  FFromName
                                                     write FFromName;
        property RcptName : TStrings                 read  FRcptName
                                                     write SetRcptName;
        property MailMessage : TStrings              read  FMailMessage
                                                     write SetMailMessage;
        property HdrFrom : String                    read  FHdrFrom
                                                     write FHdrFrom;
        property HdrTo : String                      read  FHdrTo
                                                     write FHdrTo;
        property HdrCc : String                      read  FHdrCc
                                                     write FHdrCc;
        property HdrReplyTo : String                 read  FHdrReplyTo
                                                     write FHdrReplyTo;
        property HdrReturnPath : String              read  FHdrReturnPath
                                                     write FHdrReturnPath;
        property HdrSubject : String                 read  FHdrSubject
                                                     write FHdrSubject;
        property HdrSender: String                   read  FHdrSender
                                                     write FHdrSender;
        property HdrPriority  : TSmtpPriority        read  FHdrPriority
                                                     write FHdrPriority;
        property CharSet      : String               read  FCharSet
                                                     write FCharSet;
        property ContentType  : TSmtpContentType     read  FContentType
                                                     write SetContentType;
        property ErrorMessage : String               read  FErrorMessage;
        property LastResponse : String               read  FLastResponse;
        property State        : TSmtpState           read  FState;
        property Tag          : LongInt              read  FTag
                                                     write FTag;
        property OwnHeaders   : Boolean              read  FOwnHeaders
                 { Angus V2.21 }                     write FOwnHeaders;
        property OnDisplay : TSmtpDisplay            read  FOnDisplay
                                                     write FOnDisplay;
        property OnCommand: TSmtpDisplay             read  FOnCommand
                                                     write FOnCommand;
        property OnResponse: TSmtpDisplay            read  FOnResponse
                                                     write FOnResponse;
        property OnGetData : TSmtpGetDataEvent       read  FOnGetData
                                                     write FOnGetData;
        property OnHeaderLine : TSmtpHeaderLineEvent read  FOnHeaderLine
                                                     write FOnHeaderLine;
        property OnProcessHeader  : TSmtpProcessHeaderEvent
                                                     read  FOnProcessHeader
                                                     write FOnProcessHeader;
        property OnRequestDone : TSmtpRequestDone    read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnStateChange : TNotifyEvent        read  FOnStateChange
                                                     write FOnStateChange;
        property OnSessionConnected : TSessionConnected
                                                     read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed
                                                     read  FOnSessionClosed
                                                     write FOnSessionClosed;
    end;

    { Descending component adding MIME (file attach) support }
    TSmtpCli = class(TCustomSmtpClient)
    protected
        FEmailBody    : TStrings; { Message body text         }
        FEmailFiles   : TStrings; { File names for attachment }
        FCurrentFile  : Integer;  { Current file being sent   }
        FMimeBoundary : String;   { Message parts boundary    }
        FFileStarted  : Boolean;
        FBodyFlag     : Boolean;
        FBodyLine     : Integer;
        FOnAttachContentType : TSmtpAttachmentContentType;
        FOnAttachHeader      : TSmtpAttachHeader;
        procedure   TriggerAttachContentType(FileNumber      : Integer;
                                             var FileName    : String;
                                             var ContentType : String); virtual;
        procedure   TriggerAttachHeader(FileNumber : Integer;
                                        FileName   : String;
                                        HdrLines   : TStrings); virtual;
        procedure   TriggerGetData(LineNum  : Integer;
                                   MsgLine  : Pointer;
                                   MaxLen   : Integer;
                                   var More : Boolean); override;
        procedure   TriggerHeaderLine(Line : Pointer; Size : Integer); override;
        procedure   SetEMailFiles(newValue : TStrings);
        procedure   PrepareEMail; virtual;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;
        procedure   Data;                        override;
        property AuthTypesSupported;
    published
        property ShareMode;
        property Host;
        property LocalAddr; {bb}
        property Port;
        property SignOn;
        property Username;
        property Password;
        property AuthType;
        property FromName;
        property RcptName;
        property MailMessage;
        property HdrFrom;
        property HdrTo;
        property HdrCc;
        property HdrReplyTo;
        property HdrReturnPath;
        property HdrSubject;
        property HdrSender;
        property HdrPriority;
        property State;
        property CharSet;
        property ContentType;
        property ErrorMessage;
        property LastResponse;
        property Tag;
        property OwnHeaders ;             { Angus V2.21 }
        property OnDisplay;
        property OnCommand;
        property OnResponse;
        property OnGetData;
        property OnHeaderLine;
        property OnProcessHeader;
        property OnRequestDone;
        property OnSessionConnected;
        property OnSessionClosed;
        property EmailFiles : TStrings               read  FEmailFiles
                                                     write SetEmailFiles;
        property OnAttachContentType : TSmtpAttachmentContentType
                                                     read  FOnAttachContentType
                                                     write FOnAttachContentType;
        property OnAttachHeader  : TSmtpAttachHeader read  FOnAttachHeader
                                                     write FOnAttachHeader;

    end;

    { TSyncSmtpCli add synchronous functions. You should avoid using this   }
    { component because synchronous function, apart from being easy, result }
    { in lower performance programs.                                        }
    TSyncSmtpCli = class(TSmtpCli)
    protected
        FTimeout       : Integer;                 { Given in seconds }
        FTimeStop      : LongInt;                 { Milli-seconds    }
        FMultiThreaded : Boolean;
        function WaitUntilReady : Boolean; virtual;
        function Synchronize(Proc : TSmtpNextProc) : Boolean;
        procedure TriggerGetData(LineNum  : Integer;
                                 MsgLine  : Pointer;
                                 MaxLen   : Integer;
                                 var More : Boolean); override;
    public
        constructor Create(AOwner : TComponent); override;
        function    ConnectSync  : Boolean; virtual;
        function    HeloSync     : Boolean; virtual;
        function    EhloSync     : Boolean; virtual;
        function    AuthSync     : Boolean; virtual;
        function    VrfySync     : Boolean; virtual;
        function    MailFromSync : Boolean; virtual;
        function    RcptToSync   : Boolean; virtual;
        function    DataSync     : Boolean; virtual;
        function    QuitSync     : Boolean; virtual;
        function    RsetSync     : Boolean; virtual;
        function    AbortSync    : Boolean; virtual;
        function    OpenSync     : Boolean; virtual;
        function    MailSync     : Boolean; virtual;
    published
        property Timeout : Integer       read  FTimeout
                                         write FTimeout;
        property MultiThreaded : Boolean read  FMultiThreaded
                                         write FMultiThreaded;
    end;

    THtmlSmtpCli = class(TSmtpCli)
    private
        FPlainText       : TStrings;
        FHtmlText        : TStrings;
        FStreamArray     : TList;
        FOutsideBoundary : String;
        FInsideBoundary  : String;
        FMimeState       : TSmtpMimeState;
        FHtmlCharSet     : String;
        FLineOffset      : Integer;
        FImageNumber     : Integer;
        procedure SetPlainText(const newValue: TStrings);
        procedure SetHtmlText(const newValue: TStrings);
        function  GetImageStream(Index: Integer): TStream;
        procedure SetImageStream(Index: Integer; const Value: TStream);
        function  GetImageStreamCount: Integer;
    protected
        procedure   TriggerGetData(LineNum  : Integer;
                                   MsgLine  : Pointer;
                                   MaxLen   : Integer;
                                   var More : Boolean); override;
        procedure   TriggerProcessHeader(HdrLines : TStrings); override;
        procedure   GenerateBoundaries; virtual;
        procedure   PrepareEMail; override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure ClearImageStreamArray;
        { ImageStream is not completely implemented. Do not use ! }
        property ImageStream[Index : Integer] : TStream
                                       read  GetImageStream
                                       write SetImageStream;
        property ImageStreamCount : Integer
                                       read  GetImageStreamCount;
    published
        property PlainText : TStrings  read  FPlainText
                                       write SetPlainText;
        property HtmlText  : TStrings  read  FHtmlText
                                       write SetHtmlText;
    end;

{ Function to convert a TDateTime to an RFC822 timestamp string }
function Rfc822DateTime(t : TDateTime) : String;
{ Function to parse a friendly email and extract friendly name and email }
{ "Fran�ois PIETTE" <francois.piette@overbyte.be>                        }
{ The function returns as result the email address and in the var        }
{ parameter the friendly name without quote, if any.                     }
{ The function take care of various common ways to build friendly email  }
{ addresses, or even non friendly addresses.                             }
function ParseEmail(FriendlyEmail    : String;
                    var FriendlyName : String) : String;

{ List of separators accepted between email addresses }
const
    SmtpEMailSeparators = [';', ','];

procedure Register;

implementation

{$B-} { Partial boolean evaluation }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RTrim(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LTrim(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := LTrim(Rtrim(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$I+}   { Activate I/O check (EInOutError exception generated) }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetShareMode(newValue: TSmtpShareMode);
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case newValue of
    smtpShareCompat    : FShareMode := fmShareCompat;
    smtpShareExclusive : FShareMode := fmShareExclusive;
    smtpShareDenyWrite : FShareMode := fmShareDenyWrite;
    smtpShareDenyRead  : FShareMode := fmShareDenyRead;
    smtpShareDenyNone  : FShareMode := fmShareDenyNone;
    else
        FShareMode := fmShareDenyWrite;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSmtpClient.GetShareMode: TSmtpShareMode;
begin
   begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case FShareMode of
    fmShareCompat    : Result := smtpShareCompat;
    fmShareExclusive : Result := smtpShareExclusive;
    fmShareDenyWrite : Result := smtpShareDenyWrite;
    fmShareDenyRead  : Result := smtpShareDenyRead;
    fmShareDenyNone  : Result := smtpShareDenyNone;
    else
        Result := smtpShareDenyWrite;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NOFORMS}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function SmtpClientWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    { At window creation asked windows to store a pointer to our object     }
    Obj := TObject(GetWindowLong(ahWnd, 0));

    { If the pointer doesn't represent a TCustomSmtpClient, just call the default procedure}
    if not (Obj is TCustomSmtpClient) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass parameter to his own kind of   }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        { May be a try/except around next line is needed. Not sure ! }
        TCustomSmtpClient(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF VER80}
procedure TCustomSmtpClient.ThreadAttach;
begin
    FWSocket.ThreadAttach;
    FWindowHandle := SmtpClientAllocateHWnd(WndProc);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.ThreadDetach;
begin
    FWSocket.ThreadDetach;
    SmtpClientDeallocateHWnd(FWindowHandle);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSmtpClient.SmtpClientAllocateHWnd(Method: TWndMethod) : HWND;
begin
{$IFDEF NOFORMS}
    Result := XSocketAllocateHWnd(Self);
    SetWindowLong(Result, GWL_WNDPROC, LongInt(@SmtpClientWindowProc));
{$ELSE}
     Result := WSocket.AllocateHWnd(Method);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SmtpClientDeallocateHWnd(WHandle : HWND);
begin
{$IFDEF NOFORMS}
    XSocketDeallocateHWnd(WHandle);
{$ELSE}
    WSocket.DeallocateHWnd(WHandle);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSmtpClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle            := SmtpClientAllocateHWnd(WndProc);
    FWSocket                 := TWSocket.Create(nil);
    FWSocket.OnSessionClosed := WSocketSessionClosed;
    FState                   := smtpReady;
    FRcptName                := TStringList.Create;
    FMailMessage             := TStringList.Create;
    FAuthTypesSupported      := TStringList.Create;
    FPort                    := 'smtp';
    FCharSet                 := 'iso-8859-1';
    FAuthType                := smtpAuthNone;
    FLocalAddr               := '0.0.0.0';
    SetContentType(smtpPlainText);
    FShareMode               := fmShareDenyWrite;
    FHdrPriority             := smtpPriorityNone;
    SetContentType(smtpPlainText);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomSmtpClient.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    if Assigned(FHdrLines) then begin
        FHdrLines.Destroy;
        FHdrLines := nil;
    end;

    if Assigned(FAuthTypesSupported) then begin
        FAuthTypesSupported.Destroy;
        FAuthTypesSupported := nil;
    end;

    FMailMessage.Destroy;
    FRcptName.Destroy;

    SmtpClientDeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_SMTP_REQUEST_DONE : WMSmtpRequestDone(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WMSmtpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(Data : PChar; var Number : Integer) : PChar;
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data);

    if (Result = nil) then
        Exit;

    { Remember the sign }
    if Result^ in ['-', '+'] then begin
        bSign := (Result^ = '-');
        Inc(Result);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (Result^ <> #0) and (Result^ in ['0'..'9']) do begin
        Number := Number * 10 + ord(Result^) - ord('0');
        Inc(Result);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.CheckReady;
begin
    if not (FState in [smtpReady, smtpInternalReady]) then
        raise SmtpException.Create('SMTP component not ready');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerSessionConnected(ErrorCode : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, ErrorCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerSessionClosed(ErrorCode : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, ErrorCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerRequestDone(ErrorCode: Word);
begin
    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;

{ --Jake Traynham, 06/12/01  Bug - removed "(ErrorCode = 0) and" because we  }
{                            want DoHighLevelAsync to handle any errors  }
{                            we get while doing a High Level function:   }
{ *bug* if (ErrorCode = 0) and Assigned(FNextRequest) then begin             }
        if Assigned(FNextRequest) then begin
            if FState <> smtpAbort then
                StateChange(smtpInternalReady);
            FNextRequest;
        end
        else begin
            StateChange(smtpReady);
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            PostMessage(Handle, WM_SMTP_REQUEST_DONE, 0, ErrorCode);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.StateChange(NewState : TSmtpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DisplayLastResponse;
begin
     TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDataAvailable(Sender: TObject; ErrorCode: Word);
var
    Len : Integer;
    I   : Integer;
    p   : PChar;
begin
    Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen],
                            sizeof(FReceiveBuffer) - FReceiveLen);

    if Len <= 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
        I := Pos(#13#10, FReceiveBuffer);
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;

        FLastResponse := Copy(FReceiveBuffer, 1, I - 1);
        TriggerResponse(FLastResponse);

{$IFDEF DUMP}
        FDumpBuf := '>|';
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
        FDumpBuf := '|' + #13#10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
{$IFDEF VER80}
        { Add a nul byte at the end of string for Delphi 1 }
        FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
        FReceiveLen := FReceiveLen - I - 1;
        if FReceiveLen > 0 then
            Move(FReceiveBuffer[I + 1], FReceiveBuffer[0], FReceiveLen + 1);

        if FState = smtpWaitingBanner then begin
            DisplayLastResponse;
            p := GetInteger(@FLastResponse[1], FStatusCode);
            if p^ = '-' then
                Continue;  { Continuation line, ignore }
            if FStatusCode <> 220 then begin
                SetErrorMessage;
                FRequestResult := FStatusCode;
                FWSocket.Close;
                Exit;
            end;

            StateChange(smtpConnected);
            TriggerSessionConnected(ErrorCode);

            if Assigned(FWhenConnected) then
                FWhenConnected
            else begin
                TriggerRequestDone(0);
            end;
        end
        else if FState = smtpWaitingResponse then begin
            if Assigned(FNext) then
                FNext
            else
                raise SmtpException.Create('Program error: FNext is nil');
        end
        else begin
            { Unexpected data received }
            DisplayLastResponse;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketSessionConnected(Sender: TObject; ErrorCode: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if ErrorCode <> 0 then begin
        FLastResponse := '500 ' + WSocketErrorDesc(ErrorCode) +
                         ' (Winsock error #' + IntToStr(ErrorCode) + ')';
        FStatusCode   := 500;
        FConnected    := FALSE;
{ --Jake Traynham, 06/12/01  Bug - Need to set FRequestResult so High    }
{                            Level Open will exit out. (See also         }
{                            TriggerRequestDone bug.)                    }
        FRequestResult:= 500;
        SetErrorMessage;
        TriggerRequestDone(ErrorCode);
        FWSocket.Close;
        StateChange(smtpReady);
    end
    else begin
        FConnected := TRUE;
        StateChange(smtpWaitingBanner);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDnsLookupDone(
    Sender    : TObject;
    ErrorCode : Word);
begin
    if ErrorCode <> 0 then begin
        FLastResponse := '500 ' + WSocketErrorDesc(ErrorCode) +
                         ' (Winsock error #' + IntToStr(ErrorCode) + ')';
        FStatusCode   := 500;
        SetErrorMessage;
        FRequestResult := ErrorCode;
        TriggerRequestDone(ErrorCode);
    end
    else begin
        FWSocket.Addr               := FWSocket.DnsResult;
        FWSocket.LocalAddr          := FLocalAddr; {bb}
        FWSocket.Proto              := 'tcp';
        FWSocket.Port               := FPort;
        FWSocket.OnSessionConnected := WSocketSessionConnected;
        FWSocket.OnDataAvailable    := WSocketDataAvailable;
        StateChange(smtpConnecting);
        try
            FWSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse  := '500 ' + E.ClassName + ': ' + E.Message;
                FStatusCode    := 500;
                FRequestResult := FStatusCode;
                SetErrorMessage;
                { TriggerRequestDone(FStatusCode); }
                TriggerRequestDone(FWSocket.LastError); { Apr 01, 2002 }
            end;
        end
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SendCommand(Cmd : String);
begin
    TriggerCommand(Cmd);
    TriggerDisplay('> ' + Cmd);
    if FWSocket.State = wsConnected then
        FWSocket.SendStr(Cmd + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.ExecAsync(
    RqType      : TSmtpRequest;
    Cmd         : String;         { Command to execute                      }
    OkResponses : array of Word;  { List of responses like '200 221 342'    }
    DoneAsync   : TSmtpNextProc); { What to do when done                    }
var
    I : Integer;
begin
    CheckReady;

    if not FConnected then
        raise SmtpException.Create('SMTP component not connected');

    if not FHighLevelFlag then
        FRequestType := RqType;

    for I := 0 to High(OkResponses) do
        FOkResponses[I] := OkResponses[I];
    FOkResponses[High(OkResponses) + 1] := 0;

    FRequestDoneFlag  := FALSE;
    FNext             := NextExecAsync;
    FDoneAsync        := DoneAsync;
    StateChange(smtpWaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.NextExecAsync;
var
    I : Integer;
    p : PChar;
begin
    DisplayLastResponse;
    p := GetInteger(@FLastResponse[1], FStatusCode);
    if p^ = '-' then
        Exit; { Continuation line, nothing to do }

    if FOkResponses[0] = 0 then begin
        { The list of ok responses is empty }
        if FStatusCode >= 500 then begin
            { Not a good response }
            FRequestResult := FStatusCode;
            SetErrorMessage;
        end
        else
            FRequestResult := 0;
    end
    else begin
        { We have a list of ok response codes }
        for I := 0 to High(FOkResponses) do begin
            if FOkResponses[I] = 0 then begin
                { No good response found }
                FRequestResult := FStatusCode;
                SetErrorMessage;
                break;
            end;
            if FOkResponses[I] = FStatusCode then begin
                { Good response found }
                FRequestResult := 0;
                Break;
            end;
        end;
    end;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else if (FRequestType <> smtpQuit) or (FConnected = FALSE) then
        TriggerRequestDone(FRequestResult)
    else begin
        { We have to wait until remote host close connection before }
        { calling TriggerRequestDone. See WSocketSessionClosed.     }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Helo;
var
    I   : Integer;
    Buf : String;
begin
    FFctPrv := smtpFctHelo;
    if FSignOn = '' then
        Buf := LocalHostName
    else
        Buf := FSignOn;
    { Replace any space by underscore }
    for I := 1 to Length(Buf) do begin
        if Buf[I] = ' ' then
            Buf[I] := '_';
    end;
    ExecAsync(smtpHelo, 'HELO ' + Buf, [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Ehlo;
var
    I   : Integer;
    Buf : String;
begin
    FAuthTypesSupported.Clear;
    FFctPrv := smtpFctEhlo;
    if FSignOn = '' then
        Buf := LocalHostName
    else
        Buf := FSignOn;
    { Replace any space by underscore }
    for I := 1 to Length(Buf) do begin
        if Buf[I] = ' ' then
            Buf[I] := '_';
    end;
    ExecAsync(smtpEhlo, 'EHLO ' + Buf, [250], EhloNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.EhloNext;
begin
    { It's possible that some really old mail servers will disconnect you }
    { if you use the 'EHLO' command.  If we've been disconnected, then do }
    { nothing. RequestDone event handler is called from socket            }
    { SessionClose event.                                                 }
    if not FConnected
      then Exit;

    if (FRequestResult = 0)
      then FESmtpSupported := TRUE;

    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DoAuthPlain;
var
    AuthPlain : String;
begin
    AuthPlain := FUserName;
    AuthPlain := AuthPlain + #0;
    if FFromName <> '' then { FromName should be set before calling Auth }
        AuthPlain := AuthPlain + FFromName
    else
        AuthPlain := AuthPlain + FUserName;
    AuthPlain := AuthPlain + #0;
    AuthPlain := AuthPlain + FPassword;
    AuthPlain := Base64Encode(AuthPlain);
    ExecAsync(smtpAuth, 'AUTH PLAIN ' + AuthPlain, [235], AuthNextPlain);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Auth;
var
    tmpAuthType: TSmtpAuthType;
begin
    { If ESMTP is not supported, or if we didn't start with EHLO, then we }
    { can't use the AUTH command.                                         }
    if not FESmtpSupported then begin
        FLastResponse := '500 ESMTP not supported.';
        SetErrorMessage;
        if not FHighLevelFlag then begin
            FRequestDoneFlag := FALSE;
            FRequestType     := smtpAuth;
        end;
        TriggerRequestDone(500);
        Exit;
    end;

    FFctPrv := smtpFctAuth;

    tmpAuthType := FAuthType;
    if FAuthType = smtpAuthAutoSelect then begin
        tmpAuthType := smtpAuthNone;
        if FAuthTypesSupported.IndexOf('PLAIN')    <> -1 then
            tmpAuthType := smtpAuthPlain;
        if FAuthTypesSupported.IndexOf('LOGIN')    <> -1 then
            tmpAuthType := smtpAuthLogin;
        if FAuthTypesSupported.IndexOf('CRAM-MD5') <> -1 then
            tmpAuthType := smtpAuthCramMD5;
        { RFC2554: If an AUTH command fails, the client may try another }
        { authentication mechanism by issuing another AUTH command.     }
        { If an AUTH command fails, the server MUST behave the same as  }
        { if the client had not issued the AUTH command.                }
        { We start the first trial with most secure CRAM-MD5 even       }
        { though the AuthType could not be determined.}
        if tmpAuthType = smtpAuthNone then
            tmpAuthType := smtpAuthCramMD5;
    end;

    case tmpAuthType of
    smtpAuthNone :
        begin
            { shouldn't happen }
            FLastResponse := '500 No Authentication Type Selected.';
            SetErrorMessage;
            if not FHighLevelFlag then begin
                FNextRequest     := nil;
                FRequestType     := smtpAuth;
            end;
            FRequestDoneFlag := FALSE;
            TriggerRequestDone(500);
            Exit;
        end;
    smtpAuthPlain :
        DoAuthPlain;
    smtpAuthLogin :
        ExecAsync(smtpAuth, 'AUTH LOGIN', [334], AuthNextLogin);
    smtpAuthCramMD5 :
        ExecAsync(smtpAuth, 'AUTH CRAM-MD5', [334], AuthNextCramMD5);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextPlain;
begin
    if (FAuthType = smtpAuthAutoSelect) and (FRequestResult <> 0) then
        if FRequestResult <> 504 then
            FErrorMessage := '500 Authentication Type could not be determined.';

    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextLogin;
begin
    if FRequestResult <> 0 then begin
        if FAuthType = smtpAuthAutoSelect then begin
            { OK, AUTH LOGIN failed we'll try AUTH PLAIN.                   }
            if FRequestResult = 504 then begin
                TriggerRequestDone(FRequestResult);
                DoAuthPlain;
                Exit;
            end;
            FErrorMessage  := '500 Authentication Type could not be determined.';
        end;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(FUsername), [334], AuthNextLoginNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextLoginNext;
begin
    { If there was an error, tell the user and exit.                      }
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(FPassword), [235], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextCramMD5;
var
    Challenge  : String;
    Response   : String;
    HexDigits  : String;
    MD5Digest  : TMD5Digest;
    MD5Context : TMD5Context;
    Count      : Integer;
    IPAD       : array[0..63] of Byte;
    OPAD       : array[0..63] of Byte;
begin
    if FRequestResult <> 0 then begin
        if (FAuthType = smtpAuthAutoSelect) then begin
           { We'll fall back to AUTH LOGIN and see whether we succeed.        }
           if FRequestResult = 504 then begin
               TriggerRequestDone(FRequestResult);
               ExecAsync(smtpAuth, 'AUTH LOGIN', [334], AuthNextLogin);
           end
           else begin
               FErrorMessage  := '500 Authentication Type could not be determined.';
               TriggerRequestDone(FRequestResult);
           end;
        end
        else
            TriggerRequestDone(FRequestResult);
        Exit;
    end;

    { Server should be returning something like                           }
    {   334 PDEyMzc5MTU3NTAtNjMwNTcxMzRAZm9vLmJhci5jb20+                  }
    { If it does not, then exit.                                          }
    if Length(FLastResponse) < 5 then begin
        FLastResponse := '500 Malformed MD5 Challege: ' + FLastResponse;
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
    end;

    Challenge := Copy(FLastResponse, 5, Length(FLastResponse) - 4);
    Challenge := Base64Decode(Challenge);

    { See RFC2104 }
    for Count := 0 to 63 do begin
        if ((Count+1) <= Length(FPassword)) then begin
            IPAD[Count] := Byte(FPassword[Count+1]) xor $36;
            OPAD[Count] := Byte(FPassword[Count+1]) xor $5C;
        end
        else begin
            IPAD[Count] := 0 xor $36;
            OPAD[Count] := 0 xor $5C;
        end;
    end;

    MD5Init(MD5Context);
    MD5Update(MD5Context, IPAD, 64);
    MD5UpdateBuffer(MD5Context, @Challenge[1], Length(Challenge));
    MD5Final(MD5Digest, MD5Context);

    MD5Init(MD5Context);
    MD5Update(MD5Context, OPAD, 64);
    MD5Update(MD5Context, MD5Digest, 16);
    MD5Final(MD5Digest, MD5Context);

    HexDigits := '0123456789abcdef';
    Response := FUsername;
    Response := Response + ' ';
    for Count := 0 to 15 do begin
        Response := Response + HexDigits[((Byte(MD5Digest[Count]) and $F0) shr 4)+1];
        Response := Response + HexDigits[(Byte(MD5Digest[Count]) and $0F)+1];
    end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(Response), [235], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthGetType;
var
    I      : Integer;
    S      : String;
    aEntry : String;
begin
    { Not sure whether UpperCase & Trims are needed or not, anyway - should }
    { not hurt. Find SmtpAuthTypes in Ehlo response lines.                  }
    S := UpperCase(Trim(FLastResponse));
    Delete(S, 1, 4);
    if (CompareText(Copy(S, 1, 5), 'AUTH ') = 0) or
       (CompareText(Copy(S, 1, 5), 'AUTH=') = 0) then begin
        S := Copy(S, 5, Length(S));
        { supported by all versions??? }
        {S := StringReplace(S, '=', ' ', [rfReplaceAll]);}
        for I := 1 to Length(S) do begin
            if S[I] = '=' then
                S[I] := ' ';
        end;
        while TRUE do begin
            I := Pos(' ', S);
            if I <= 0 then begin
                aEntry := Trim(S);
                if FAuthTypesSupported.IndexOf(aEntry) = -1 then
                    FAuthTypesSupported.Add(aEntry);
                break;
            end
            else begin
                aEntry := Trim(Copy(S, 1, I - 1));
                if FAuthTypesSupported.IndexOf(aEntry) = -1 then
                    FAuthTypesSupported.Add(aEntry);
                Delete(S, 1, I);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Vrfy;
begin
    FFctPrv := smtpFctVrfy;
    ExecAsync(smtpVrfy, 'VRFY ' + FHdrTo, [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.MailFrom;
var
    FriendlyName : String;
begin
    FFctPrv := smtpFctMailFrom;
    ExecAsync(smtpMailFrom,
              'MAIL FROM:<' + ParseEmail(FFromName, FriendlyName) + '>',
              [250], nil)
{
    if (Pos('<', FFromName) <> 0) and (Pos('>', FFromName) <> 0) then
        ExecAsync(smtpMailFrom, 'MAIL FROM: ' + Trim(FFromName), [250], nil)
    else
        ExecAsync(smtpMailFrom,
                  'MAIL FROM: <' + Trim(FFromName) + '>', [250], nil)
}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Rset;
begin
    FFctPrv := smtpFctRset;
    ExecAsync(smtpRset, 'RSET', [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptTo;
begin
    if FRcptName.Count <= 0 then
        raise SmtpException.Create('RcptName list is empty');

    FItemCount := -1;
    RcptToNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ FriendlyEmail                  FriendlyName   Result                      }
{ ----------------------------   ------------   --------------              }
{ myname <name@domain.com>       'myname'       name@domain.com             }
{ myname name@domain.com         'myname'       name@domain.com             }
{ "my name" <name@domain.com>    'my name'      name@domain.com             }
{ 'my name' <name@domain.com>    'my name'      name@domain.com             }
{ name@domain.com                empty          name@domain.com             }
{ <name@domain.com>              empty          name@domain.com             }
{ "name@domain.com"              empty          name@domain.com             }
function ParseEmail(
    FriendlyEmail    : String;
    var FriendlyName : String) : String;
var
    I, J  : Integer;
    Flag  : Boolean;
    Delim : Char;
begin
    Result       := '';
    FriendlyName := '';
    Flag         := (Pos('<', FriendlyEmail) > 0);
    { Skip spaces }
    I := 1;
    while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] = ' ') do
        Inc(I);
    if I > Length(FriendlyEmail) then
        Exit;
    { Check if quoted string }
    if FriendlyEmail[I] in ['"', ''''] then begin
        Delim := FriendlyEmail[I];
        { Skip opening quote }
        Inc(I);
        { Go to closing quote }
        J := I;
        while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> Delim) do
            Inc(I);
        FriendlyName := Copy(FriendlyEmail, J, I - J);
        Inc(I);
        if Flag then begin
            { Go to less-than sign }
            while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '<') do
                Inc(I);
            Inc(I);
            J := I;
            while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '>') do
                Inc(I);
            Result := Copy(FriendlyEmail, J, I - J);
        end
        else
            Result := Trim(Copy(FriendlyEmail, I, Length(FriendlyEmail)));
    end
    else begin
        if Flag then begin
            { Go to less-than sign }
            J := I;
            while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '<') do
                Inc(I);
            FriendlyName := Trim(Copy(FriendlyEmail, J, I - J));
            Inc(I);
            { Go to greater-than sign }
            J := I;
            while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> '>') do
                Inc(I);
            Result := Copy(FriendlyEmail, J, I - J);
        end
        else begin
            { No <..>, goto next space }
            J := I;
            while (I <= Length(FriendlyEmail)) and (FriendlyEmail[I] <> ' ') do
                Inc(I);
            FriendlyName := Trim(Copy(FriendlyEmail, J, I - J));
            Result       := Trim(Copy(FriendlyEmail, I + 1, Length(FriendlyEmail)));
        end;
    end;
    if (Result = '') and (Pos('@', FriendlyName) > 0) then begin
        Result       := FriendlyName;
        FriendlyName := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptNameAdd(
    const ToList  : String;
    const CcList  : String;
    const BccList : String);
var
    Buf  : String;
    I, J : Integer;
    Adr  : String;
begin
    Buf := ToList + ';' + CcList + ';' + BccList;
    I   := 1;
    while (I <= Length(Buf)) do begin
        { Skip spaces }
        if Buf[I] = ' ' then begin
            Inc(I);
            continue;
        end;
        J := I;
        while I <= Length(Buf) do begin
            if Buf[I] = '"' then begin
                { Start of quoted string, skip until end of quote }
                Inc(I);
                while (I <= Length(Buf)) and (Buf[I] <> '"') do
                    Inc(I);
                Inc(I);
            end;
            if (I >= Length(Buf)) or (Buf[I] in SmtpEMailSeparators) then begin
                if Buf[I] in SmtpEMailSeparators then
                    Adr := Trim(Copy(Buf, J, I - J))
                else
                    Adr := Trim(Copy(Buf, J, I - J + 1));
                if Adr > '' then
                    RcptName.Add(Adr);
                Inc(I);
                break;
            end;
            Inc(I);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptToNext;
var
    WhenDone     : TSmtpNextProc;
    FriendlyName : String;
begin
    Inc(FItemCount);
    if FItemCount >= (FRcptName.Count - 1) then
        WhenDone := nil
    else
        WhenDone := RcptToDone;
    FFctPrv    := smtpFctRcptTo;
    ExecAsync(smtpRcptTo,
              'RCPT TO:<' +
              ParseEmail(FRcptName.Strings[FItemCount], FriendlyName) + '>',
              [250, 251], WhenDone)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptToDone;
begin
    FState := smtpInternalReady;
    RcptToNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetContentType(newValue : TSmtpContentType);
begin
    if FContentType = newValue then
        Exit;
    FContentType := newValue;
    if FContentType = smtpPlainText then
        FContentTypeStr := 'text/plain'
    else
        FContentTypeStr := 'text/html';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FoldHdrLine(
    HdrLines : TStrings;
    HdrName  : String;
    HdrValue : String);
var
    I, J       : Integer;
    S          : String;
    FirstFound : Boolean;
begin
    FirstFound := False;
    HdrName    := Trim(HdrName);
    I          := 1;
    while I <= Length(HdrValue) do begin
         { Skip spaces }
         if HdrValue[I] = ' ' then begin
             Inc(I);
             continue;
         end;
         J := I;
         while I <= Length(HdrValue) do begin
             if HdrValue[I] = '"' then begin
                 { Start of quoted string, skip until end of quote }
                 Inc(I);
                 while (I <= Length(HdrValue)) and (HdrValue[I] <> '"') do
                     Inc(I);
                 Inc(I);
             end;
             if (I >= Length(HdrValue)) or
                (HdrValue[I] in SmtpEMailSeparators) then begin
                 if (HdrValue[I] in SmtpEMailSeparators) then begin
                     S := Trim(Copy(HdrValue, J, I - J));
                     if S > '' then begin
                         if FirstFound then
                             S := #09 + S + ','
                         else
                             S := HdrName + ' ' + S + ','
                     end;
                 end
                 else begin
                     S := Trim(Copy(HdrValue, J, I - J + 1));
                     if S > '' then begin
                         if FirstFound then
                             S := #09 + S
                         else
                             S := HdrName + ' ' + S;
                     end;
                 end;
                 if S > '' then begin
                     HdrLines.Add(s);
                     FirstFound := TRUE;
                 end;
                 Inc(I);
                 break;
             end;
             Inc(I);
         end;
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
procedure FoldHdrLine(
    HdrLines : TStrings;
    HdrName  : String;
    HdrValue : String);
var
    I         : Integer;
    QuotedStr : String;
    Found     : Boolean;
begin
    Found    := FALSE;
    HdrValue := Trim(HdrValue);
    HdrName  := Trim(HdrName);
    while TRUE do begin
        { i.e alias may contain a ',' }
        QuotedStr := '';
        I         := Pos('"', HdrValue);
        if I = 1 then begin  { quoted string found }
            Delete(HdrValue, 1, 1);
            I := Pos('"', HdrValue);
            if I > 0 then begin
                QuotedStr := '"' + Copy(HdrValue, 1, I) + ' ';
                Delete(HdrValue, 1, I);
                HdrValue := Trim(HdrValue);
            end;
        end;

        I := Pos(',', HdrValue);
        if I <= 0 then begin
            if not Found then { the only one }
                HdrLines.Add(HdrName + ' ' + QuotedStr + HdrValue)
            else
                HdrLines.Add(#09 + QuotedStr + HdrValue); { the last one }
            Break;
        end
        else begin
            if not Found then { the first one }
                HdrLines.Add(HdrName + ' ' + QuotedStr +
                             Trim(Copy(HdrValue, 1, I - 1)) + ',')
            else
                HdrLines.Add(#09 + QuotedStr +
                             Trim(Copy(HdrValue, 1, I - 1)) + ',');
            Delete(HdrValue, 1, I);
            Found := TRUE;
        end;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Data;
begin
    FLineNum   := 0;
    FMoreLines := TRUE;
    FItemCount := -1;
    if not Assigned(FHdrLines) then
        FHdrLines := TStringList.Create
    else
        FHdrLines.Clear;
    if not FOwnHeaders then begin
        { Angus V2.21 - the body must contain all the headers }
        if Length(Trim(FHdrReplyTo)) > 0 then
            FoldHdrLine(FHdrLines, 'Reply-To: ', FHdrReplyTo);
        if Length(Trim(FHdrReturnPath)) > 0 then
            FHdrLines.Add('Return-Path: '    + FHdrReturnPath);
        if Length(FHdrFrom) > 0 then
            FHdrLines.Add('From: '    + FHdrFrom);
        if Length(FHdrTo) > 0 then
            FoldHdrLine(FHdrLines, 'To: ', FHdrTo);
        if Length(FHdrCc) > 0 then
            FoldHdrLine(FHdrLines, 'Cc: ', FHdrCc);
        FHdrLines.Add('Subject: ' + FHdrSubject);
        if Length(Trim(FHdrSender)) > 0 then
            FHdrLines.Add('Sender: ' + FHdrSender)
        else if Length(Trim(FHdrFrom)) > 0 then
            FHdrLines.Add('Sender: ' + FHdrFrom);
        FHdrLines.Add('Mime-Version: 1.0');
        FHdrLines.Add('Content-Type: ' + FContentTypeStr +
                      '; charset="' + FCharSet + '"');
        FHdrLines.Add('Date: ' + Rfc822DateTime(Now));
        if FHdrPriority <> smtpPriorityNone then begin
            FHdrLines.Add('X-Priority: ' + IntToStr(Ord(FHdrPriority)));
            if FHdrPriority < smtpPriorityNormal then begin
                FHdrLines.Add('Priority: urgent');
                FHdrLines.Add('X-MSMail-Priority: High');
            end
            else if FHdrPriority = smtpPriorityNormal then begin
                FHdrLines.Add('Priority: Normal');
                FHdrLines.Add('X-MSMail-Priority: Normal');
            end
            else begin
                FHdrLines.Add('Priority: non-urgent');
                FHdrLines.Add('X-MSMail-Priority: Low');
            end;
        end;
        FHdrLines.Add('X-Mailer: ICS SMTP Component V' +
                      IntToStr(SmtpCliVersion div 100) + '.' +
                      IntToStr(SmtpCliVersion mod 100));
        TriggerProcessHeader(FHdrLines);
        { An empty line mark the header's end }
        FHdrLines.Add('');
    end
    else
        FItemCount := 0;
    FFctPrv := smtpFctData;
    ExecAsync(smtpData, 'DATA', [354], DataNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DataNext;
var
    MsgLine  : array [0..1023] of char;
begin
    { If we have been disconnected, then do nothing.                      }
    { RequestDone event handler is called from socket SessionClose event. }
    if not FConnected then begin
        FWSocket.OnDataSent := nil;
        Exit;
    end;

    Inc(FItemCount);
    if FItemCount < FHdrLines.Count then begin
        { There are still header lines to send }
        StrPCopy(@MsgLine, FHdrLines.Strings[FItemCount]);
        TriggerHeaderLine(@MsgLine, SizeOf(MsgLine));
        TriggerDisplay('> ' + StrPas(MsgLine));
        FWSocket.OnDataSent := WSocketDataSent;
        FWSocket.PutDataInSendBuffer(@MsgLine, strlen(MsgLine));
        FWSocket.SendStr(#13+#10);
    end
    else begin
        { Now we need to send data lines }
        if FMoreLines then begin
            try
                Inc(FLineNum);
                MsgLine[0] := #0;
                TriggerGetData(FLineNum, @MsgLine, High(MsgLine), FMoreLines);
            except
                FMoreLines := FALSE;
            end;
        end;

        if FMoreLines then begin
            if MsgLine[0] = '.' then
                Move(MsgLine[0], MsgLine[1], StrLen(MsgLine) + 1);
            TriggerDisplay('> ' + StrPas(MsgLine));
            FWSocket.OnDataSent := WSocketDataSent;
            FWSocket.PutDataInSendBuffer(@MsgLine, StrLen(MsgLine));
            FWSocket.SendStr(#13 + #10);
        end
        else begin
            { Send the last message line }
            FWSocket.OnDataSent := nil;
            ExecAsync(smtpData, '.', [250], nil);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDataSent(Sender : TObject; ErrorCode : Word);
begin
    FState := smtpInternalReady;
    DataNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Abort;
begin
    EndFileEncBase64(FStream);
    StateChange(smtpAbort);
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
    StateChange(smtpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Connect;
begin
    CheckReady;
    if FConnected then
        raise SmtpException.Create('SMTP component already connected');

    if not FHighLevelFlag then
        FRequestType  := smtpConnect;   { 10/05/99 }
    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequestResult    := 0;
    FESmtpSupported   := FALSE;
    FErrorMessage     := '';
    FLastResponse     := '';
    StateChange(smtpDnsLookup);
    FWSocket.OnDataSent      := nil;
    FWSocket.OnDnsLookupDone := WSocketDnsLookupDone;
    FWSocket.DnsLookup(FHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Quit;
begin
    CheckReady;
    FFctPrv := smtpFctQuit;
    if not FConnected then begin
        { We are not connected, it's ok... }
        FRequestType     := smtpQuit;
        FRequestDoneFlag := FALSE;
        TriggerRequestDone(0);
        Exit;
    end;
    ExecAsync(smtpQuit, 'QUIT', [221], nil); { Should I force a FWSocket.Close }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DoHighLevelAsync;
begin
{$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = smtpAbort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FRequestResult   := 426;  { SJF }
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        { EHLO wasn't supported, so just log in with HELO }
        if FFctPrv = smtpFctEhlo then
            FFctSet := [smtpFctHelo]
        else begin
            FHighLevelResult := FRequestResult;
            if (FFctPrv = smtpFctQuit) or (not (smtpFctQuit in FFctSet)) then
                FFctSet := []
            else
                FFctSet := [smtpFctQuit];
        end;
    end;

    if smtpFctConnect in FFctSet then begin
        FFctPrv := smtpFctConnect;
        FFctSet := FFctSet - [FFctPrv];
        Connect;
        Exit;
    end;

    if smtpFctHelo in FFctSet then begin
        FFctPrv := smtpFctHelo;
        FFctSet := FFctSet - [FFctPrv];
        Helo;
        Exit;
    end;

    if smtpFctEhlo in FFctSet then begin
        FFctPrv := smtpFctEhlo;
        FFctSet := FFctSet - [FFctPrv];
        Ehlo;
        Exit;
    end;

    if smtpFctAuth in FFctSet then begin
        FFctPrv := smtpFctAuth;
        FFctSet := FFctSet - [FFctPrv];
        Auth;
        Exit;
    end;

    if smtpFctVrfy in FFctSet then begin
        FFctPrv := smtpFctVrfy;
        FFctSet := FFctSet - [FFctPrv];
        Vrfy;
        Exit;
    end;

    if smtpFctMailFrom in FFctSet then begin
        FFctPrv := smtpFctMailFrom;
        FFctSet := FFctSet - [FFctPrv];
        MailFrom;
        Exit;
    end;

    if smtpFctRcptTo in FFctSet then begin
        FFctPrv := smtpFctRcptTo;
        FFctSet := FFctSet - [FFctPrv];
        RcptTo;
        Exit;
    end;

    if smtpFctData in FFctSet then begin
        FFctPrv := smtpFctData;
        FFctSet := FFctSet - [FFctPrv];
        Data;
        Exit;
    end;

    if smtpFctQuit in FFctSet then begin
        FFctPrv := smtpFctQuit;
        FFctSet := FFctSet - [FFctPrv];
        Quit;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.HighLevelAsync(
    RqType : TSmtpRequest; Fcts : TSmtpFctSet);
begin
    if FConnected and (smtpFctConnect in Fcts) then
        raise SmtpException.Create('SMTP component already connected');
    CheckReady;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := smtpFctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FErrorMessage     := '';
    FRestartFlag      := FALSE;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Open;
begin
    if FAuthType <> smtpAuthNone then
        HighLevelAsync(smtpOpen, [smtpFctConnect, smtpFctEhlo, smtpFctAuth])
    else
        HighLevelAsync(smtpOpen, [smtpFctConnect, smtpFctHelo]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Mail;
begin
    HighLevelAsync(smtpMail, [smtpFctMailFrom, smtpFctRcptTo, smtpFctData]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketSessionClosed(Sender : TObject; ErrorCode : WORD);
begin
    FConnected := FALSE;
    TriggerSessionClosed(ErrorCode);
    if FRequestType = smtpQuit then
        TriggerRequestDone(0)           { We where just waiting for close }
    else
        TriggerRequestDone(WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerHeaderLine(Line : Pointer; Size : Integer);
begin
    if Assigned(FOnHeaderLine) then
        FOnHeaderLine(Self, Line, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerGetData(
    LineNum  : Integer;
    MsgLine  : Pointer;
    MaxLen   : Integer;
    var More : Boolean);
begin
    if not Assigned(FOnGetData) then
        More := FALSE
    else
        FOnGetData(Self, LineNum, MsgLine, MaxLen, More);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetRcptName(newValue : TStrings);
var
    I : Integer;
begin
    FRcptName.Clear;
    for I := 0 to newValue.Count - 1 do
        FRcptName.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetMailMessage(newValue : TStrings);
var
    I : Integer;
begin
    FMailMessage.Clear;
    for I := 0 to newValue.Count - 1 do
        FMailMessage.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
{ Delphi 1 lack this function. The time will be wrong ! }
function TimeZoneBias : TDateTime;
begin
    Result := 0;
end;
{$ELSE}
function TimeZoneBias : TDateTime;
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;         { Minutes }
begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := 0
    else begin
         if TZIResult = Time_Zone_ID_DayLight then
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := EncodeTime(Abs(aBias) div 60, Abs(aBias) mod 60, 0, 0);
         if aBias < 0 then
             Result := -Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Rfc822DateTime(t : TDateTime) : String;
var
    Year, Month, Day     : WORD;
    Hour, Min, Sec, MSec : WORD;
    Gmt                  : TDateTime;
const
    Days : array [1..7] of string =
        ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    Months : array [1..12] of string =
        ('Jan', 'Feb', 'Mar', 'Apr',
         'May', 'Jun', 'Jul', 'Aug',
         'Sep', 'Oct', 'Nov', 'Dec');
begin
    Gmt := T + TimeZoneBias;
    DecodeDate(Gmt, Year, Month, Day);
    DecodeTime(Gmt, Hour, Min, Sec, MSec);
    { Format is 'ddd, d mmm yyyy hh:mm:ss GMT' with english names }
    Result := Format('%s, %d %s %4d %02.2d:%02.2d:%02.2d GMT',
                     [Days[DayOfWeek(Gmt)],
                      Day, Months[Month], Year,
                      Hour, Min, Sec]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
function TimeZoneBias : String;
{$IFDEF VER80}  { Delphi 1 doesn't support timezone API }
begin
    Result := '-0000';
end;
{$ELSE}
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;
begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := '-0000'
    else begin
         if TZIResult = Time_Zone_ID_DayLight then   { 10/05/99 }
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := Format('-%.2d%.2d', [Abs(aBias) div 60, Abs(aBias) mod 60]);
         if aBias < 0 then
             Result[1] := '+';
    end;
end;
{$ENDIF}
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
function Rfc822DateTime(t : TDateTime) : String;
var
    I                   : Integer;
    SaveShortDayNames   : array[1..7] of string;
    SaveShortMonthNames : array[1..12] of string;
const
    MyShortDayNames: array[1..7] of string =
        ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    MyShortMonthNames: array[1..12] of string =
        ('Jan', 'Feb', 'Mar', 'Apr',
         'May', 'Jun', 'Jul', 'Aug',
         'Sep', 'Oct', 'Nov', 'Dec');
begin
    if ShortDayNames[1] = MyShortDayNames[1] then
        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias
    else begin
        { We used a localized Delphi version, the day and month names are no }
        { more english names ! We need to save and replace them              }
        for I := Low(ShortDayNames) to High(ShortDayNames) do begin
            SaveShortDayNames[I] := ShortDayNames[I];
            ShortDayNames[I]     := MyShortDayNames[I];
        end;

        for I := Low(ShortMonthNames) to High(ShortMonthNames) do begin
            SaveShortMonthNames[I] := ShortMonthNames[I];
            ShortMonthNames[I]     := MyShortMonthNames[I];
        end;

        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias;

        for I := Low(ShortDayNames) to High(ShortDayNames) do
            ShortDayNames[I] := SaveShortDayNames[I];
        for I := Low(ShortMonthNames) to High(ShortMonthNames) do
            ShortMonthNames[I] := SaveShortMonthNames[I];
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerProcessHeader(HdrLines : TStrings);
begin
    if Assigned(FOnProcessHeader) then
        FOnProcessHeader(Self, HdrLines);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerCommand(Msg : String);
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerResponse(Msg : String);
begin
    { Catch multi-line Ehlo response, parse it for AuthTypes supported}
    if FFctPrv = smtpFctEhlo then
      AuthGetType;

    if Assigned(FOnResponse) then
        FOnResponse(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.ClearErrorMessage;
begin
    FErrorMessage := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSmtpCli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FEmailBody  := TStringList.Create;
    FEmailFiles := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSmtpCli.Destroy;
begin
    if Assigned(FEmailBody) then begin
        FEMailBody.Destroy;
        FEMailBody := nil;
    end;
    if Assigned(FEmailFiles) then begin
        FEmailFiles.Destroy;
        FEmailFiles := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerAttachContentType(
    FileNumber      : Integer;
    var FileName    : String;
    var ContentType : String);
begin
    if Assigned(FOnAttachContentType) then
        FOnAttachContentType(Self, FileNumber, FileName, ContentType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerAttachHeader(
    FileNumber : Integer;
    FileName   : String;
    HdrLines   : TStrings);
begin
    if Assigned(FOnAttachHeader) then
        FOnAttachHeader(Self, FileNumber, FileName, HdrLines);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerGetData(
    LineNum  : Integer;
    MsgLine  : Pointer;
    MaxLen   : Integer;
    var More : Boolean);
var
    sLine        : String;
    FileName     : String;
    sFileName    : String;
    sContentType : String;
begin
    if FEmailBody.Count > 0 then begin
        if MaxLen > 1023 then
            MaxLen := 1023;  { RFC say 1024 char max, including nul char }
        StrPLCopy(MsgLine, FEmailBody[0], MaxLen);
        FEmailBody.Delete(0);
        More := TRUE;
        Exit;
    end;

    if FBodyFlag then begin
        Inc(FBodyLine);
        inherited TriggerGetData(FBodyLine, MsgLine, MaxLen, More);
        if More then
            Exit;
        FBodyFlag := FALSE;
    end;

    if not FFileStarted then begin
        if (not Assigned(FEMailFiles)) or
           (FEmailFiles.Count <= FCurrentFile) then begin
            { No file to send }
            More := FALSE;
            Exit;
        end;

        StrPCopy(MsgLine, '');
        FileName     := FEmailFiles[FCurrentFile];
        FStream      := InitFileEncBase64(FileName, FShareMode);
        sFileName    := ExtractFileName(FileName);
        sContentType := FilenameToContentType(sFileName);
        TriggerAttachContentType(FCurrentFile, sFileName, sContentType);
        FEmailBody.Add('--' + FMimeBoundary);
        FEmailBody.Add('Content-Type: ' + sContentType + ';');
        FEmailBody.Add(#9'name="' + sFileName + '"');
        FEmailBody.Add('Content-Transfer-Encoding: base64');
        FEmailBody.Add('Content-Disposition: attachment;');
        FEmailBody.Add(#9'filename="' + sFileName + '"');
        TriggerAttachHeader(FCurrentFile, sFileName, FEmailBody);
        FEmailBody.Add('');
        FFileStarted := TRUE;
        More         := TRUE;
        Exit;
    end;
    sLine := DoFileEncBase64(FStream, More);
    StrPCopy(MsgLine, sLine);
    if not More then begin  { we hit the end of file. }
        EndFileEncBase64(FStream);
        FFileStarted := FALSE;
        Inc(FCurrentFile);
        if (FEmailFiles.Count <= FCurrentFile) then begin
            FEmailBody.Add('');
            FEmailBody.Add('--' + FMimeBoundary + '--');
        end;
        More := TRUE;
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerHeaderLine(Line : Pointer; Size : Integer);
begin
    { if we have a MIME type message, then replace the content-type }
    { header with the proper MIME content-type.                     }
    if FMimeBoundary <> '' then begin
        if StrLIComp('CONTENT-TYPE:', Line, 13) = 0 then
            StrPCopy(Line, 'Content-Type: multipart/mixed;'#13#10#9'boundary="'
                     + FMimeBoundary + '"');
    end;
    inherited TriggerHeaderLine(Line, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.SetEMailFiles(newValue : TStrings);
var
    I        : Integer;
    FilePath : String;
begin
    FEMailFiles.Clear;
    if not Assigned(newValue) then
        Exit;
    for I := 0 to newValue.Count - 1 do begin
        FilePath := Trim(newValue.Strings[I]);
        { Ignore any empty file name (a very common error !) }
        if FilePath > '' then begin
            { Check if file exists and raise an exception if not }
            if FileExists(FilePath) then
                FEMailFiles.Add(FilePath)
            else
                raise SmtpException.Create('File not found ''' + FilePath + '''');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.Data;
begin
    PrepareEMail;
    inherited Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.PrepareEMail;
var
    I : Integer;
begin
    FBodyFlag    := TRUE;
    FCurrentFile := 0;
    FBodyLine    := 0;
    FFileStarted := FALSE;

    FEmailBody.Clear;
    if Assigned(FEMailFiles) and (FEmailFiles.Count > FCurrentFile) then begin
        FMimeBoundary := '= Multipart Boundary ' +
                         FormatDateTime('mmddyyhhnn', Now);
        FEmailBody.Add('This is a multipart MIME message.');
        FEmailBody.Add('');
        FEmailBody.Add('--' + FMimeBoundary);
        FEmailBody.Add('Content-Type: ' + FContentTypeStr +
                       '; charset="' + FCharSet + '"');
        FEmailBody.Add('Content-Transfer-Encoding: 7bit');
        FEmailBody.Add('');
    end
    else
        FMimeBoundary := '';

    for I := 0 to FMailMessage.Count - 1 do
      FEmailBody.Add(FMailMessage.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSyncSmtpCli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.WaitUntilReady : Boolean;
begin
    Result := TRUE;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
    while TRUE do begin
        if FState = smtpReady then begin
            { Back to ready state, the command is finiched }
            Result := (FRequestResult = 0);
            break;
        end;

        if  {$IFNDEF NOFORMS} Application.Terminated or {$ENDIF}
            ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) then begin
            { Application is terminated or timeout occured }
            inherited Abort;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
{$IFNDEF VER80}
        if FMultiThreaded then
            FWSocket.ProcessMessages
        else
{$ENDIF}
{$IFNDEF NOFORMS}
            Application.ProcessMessages;
{$ELSE}
            FWSocket.ProcessMessages;
{$ENDIF}
{$IFNDEF VER80}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.Synchronize(Proc : TSmtpNextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.ConnectSync : Boolean;
begin
    Result := Synchronize(Connect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.HeloSync : Boolean;
begin
    Result := Synchronize(Helo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.EhloSync : Boolean;
begin
    Result := Synchronize(Ehlo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.AuthSync : Boolean;
begin
    Result := Synchronize(Auth);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.VrfySync : Boolean;
begin
    Result := Synchronize(Vrfy);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.OpenSync : Boolean;
begin
    Result := Synchronize(Open);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.MailFromSync : Boolean;
begin
    Result := Synchronize(MailFrom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.RcptToSync : Boolean;
begin
    Result := Synchronize(RcptTo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.DataSync : Boolean;
begin
    Result := Synchronize(Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.MailSync : Boolean;
begin
    Result := Synchronize(Mail);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.QuitSync : Boolean;
begin
    Result := Synchronize(Quit);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.RsetSync : Boolean;
begin
    Result := Synchronize(RSet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.AbortSync : Boolean;
begin
    Result := Synchronize(Abort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSyncSmtpCli.TriggerGetData(
    LineNum  : Integer;
    MsgLine  : Pointer;
    MaxLen   : Integer;
    var More : Boolean);
begin
    inherited TriggerGetData(LineNum, MsgLine, MaxLen, More);
    { Evaluate the timeout period again }
    if FTimeout > 0 then
        FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THtmlSmtpCli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FPlainText   := TStringList.Create;
    FHtmlText    := TStringList.Create;
    FHtmlCharSet := 'iso-8859-1';
    SetContentType(smtpHtml);
    Randomize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THtmlSmtpCli.Destroy;
begin
    ClearImageStreamArray;
    if Assigned(FPlainText) then begin
        FPlainText.Destroy;
        FPlainText := nil;
    end;
    if Assigned(FHtmlText) then begin
        FHtmlText.Destroy;
        FHtmlText := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THtmlSmtpCli.GetImageStreamCount: Integer;
begin
    if not Assigned(FStreamArray) then
        Result := 0
    else
        Result := FStreamArray.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.ClearImageStreamArray;
begin
    if Assigned(FStreamArray) then begin
        FStreamArray.Destroy;
        FStreamArray := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THtmlSmtpCli.GetImageStream(Index: Integer): TStream;
begin
    if not Assigned(FStreamArray) then
        Result := nil
    else if Index >= FStreamArray.Count then
        Result := nil
    else
        Result := FStreamArray[Index];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.SetImageStream(
    Index       : Integer;
    const Value : TStream);
begin
    if not Assigned(Value) then
        Exit;
    if not Assigned(FStreamArray) then
        FStreamArray := TList.Create;
    while Index >= FStreamArray.Count do
        FStreamArray.Add(nil);
    FStreamArray.Items[Index] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.SetHtmlText(const newValue: TStrings);
var
    I : Integer;
begin
    FHtmlText.Clear;
    if Assigned(newValue) then
        for I := 0 to newValue.Count - 1 do
            FHtmlText.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.SetPlainText(const newValue: TStrings);
var
    I : Integer;
begin
    FPlainText.Clear;
    if Assigned(newValue) then
        for I := 0 to newValue.Count - 1 do
            FPlainText.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.TriggerGetData(
    LineNum  : Integer;
    MsgLine  : Pointer;
    MaxLen   : Integer;
    var More : Boolean);
var
    Len      : Integer;
    LineBuf  : String;
    FileName : String;
begin
    if FContentType = smtpPlainText then begin
        { Strange but the user ask to send plain text message }
        if FEmailBody.Count > 0 then begin
            if MaxLen > 1023 then
                MaxLen := 1023;  { RFC say 1024 char max, including nul char }
            StrPLCopy(MsgLine, FEmailBody[0], MaxLen);
            FEmailBody.Delete(0);
            if FEmailBody.Count = 0 then
                FLineOffset := LineNum;
            More := TRUE;
            Exit;
        end;

        if FBodyFlag then begin
            Inc(FBodyLine);
            if (LineNum - FLineOffset) >= FPlainText.Count then
                More := FALSE
            else begin
                Len := Length(FPlainText[LineNum - FLineOffset - 1]);
                if Len >= MaxLen then
                    StrPCopy(MsgLine, Copy(FPlainText[LineNum - FLineOffset - 1], 1, MaxLen - 1))
                else
                    StrPCopy(MsgLine, FPlainText[LineNum - FLineOffset - 1]);
            end;
            if More then
                Exit;
            FBodyFlag := FALSE;
        end;

        inherited TriggerGetData(LineNum, MsgLine, MaxLen, More);
        Exit;
    end;

    if FMimeState = smtpMimeIntro then begin
        case LineNum of
        1: StrPCopy(MsgLine, 'This is a multipart MIME formatted message.');
        2, 5, 9: StrPCopy(MsgLine, '');
        3: StrPCopy(MsgLine, '--' + FOutsideBoundary);
        4: StrPCopy(MsgLine, 'Content-Type: multipart/alternative; ' +
                             'boundary="' + FInsideBoundary + '"');
        6: StrPCopy(MsgLine, '--' + FInsideBoundary);
        7: StrPCopy(MsgLine, 'Content-Type: text/plain; charset="' + FCharSet + '"');
        8: StrPCopy(MsgLine, 'Content-Transfer-Encoding: quoted-printable');
        10: begin
                FMimeState  := smtpMimePlainText;
                FLineOffset := LineNum - 1;
            end;
        end;
    end;
    if FMimeState = smtpMimeIntro then
        Exit;

    if FMimeState = smtpMimePlainText then begin
        if (LineNum - FLineOffset) > FPlainText.Count then begin
            case LineNum - FLineOffset - FPlainText.Count of
            1: StrPCopy(MsgLine, '');
            2: StrPCopy(MsgLine, '--' + FInsideBoundary);
            3: StrPCopy(MsgLine, 'Content-Type: text/html; charset="' +
                                 FHtmlCharSet + '"');
            4: StrPCopy(MsgLine, 'Content-Transfer-Encoding: quoted-printable');
            5: StrPCopy(MsgLine, '');
            else
                FMimeState := smtpMimeHtmlText;
                FLineOffset := LineNum - 1;
            end;
        end
        else begin
            LineBuf := EncodeQuotedPrintable(FPlainText[LineNum - FLineOffset - 1]);
            if Length(LineBuf) > 76 then
                { RFC2045 say that the longest line is 76 characters }
                LineBuf := SplitQuotedPrintableString(LineBuf);
            Len := Length(LineBuf);
            { Truncate the line if too long (should wrap to next line) }
            if Len >= MaxLen then
                LineBuf := Copy(LineBuf, 1, MaxLen - 1);
            DotEscape(LineBuf);
            StrPCopy(MsgLine, LineBuf);
        end;
    end;
    if FMimeState = smtpMimePlainText then
        Exit;

    if FMimeState = smtpMimeHtmlText then begin
        if (LineNum - FLineOffset) > FHtmlText.Count then begin
            case LineNum - FLineOffset - FHtmlText.Count of
            1: StrPCopy(MsgLine, '');
            2: StrPCopy(MsgLine, '--' + FInsideBoundary + '--');
            3: StrPCopy(MsgLine, '');
            else
                FMimeState   := smtpMimeImages;
                FImageNumber := 1;
                FLineOffset  := LineNum - 1;
            end;
        end
        else begin
            LineBuf := EncodeQuotedPrintable(FHtmlText[LineNum - FLineOffset - 1]);
            if Length(LineBuf) > 76 then
                { RFC2045 say that the longest line is 76 characters }
                LineBuf := SplitQuotedPrintableString(LineBuf);
            Len := Length(LineBuf);
            { Truncate the line if too long (should wrap to next line) }
            if Len >= MaxLen then
                LineBuf := Copy(LineBuf, 1, MaxLen - 1);
            DotEscape(LineBuf);
            StrPCopy(MsgLine, LineBuf);
        end;
    end;
    if FMimeState = smtpMimeHtmlText then
        Exit;

    if FMimeState = smtpMimeImages then begin
        if FImageNumber > FEmailFiles.Count then begin
            case LineNum - FLineOffset of
            1:  StrPCopy(MsgLine, '--' + FOutsideBoundary + '--');
            else
                FMimeState := smtpMimeDone;
            end;
        end
        else begin
            case LineNum - FLineOffset of
            1:  StrPCopy(MsgLine, '--' + FOutsideBoundary);
            2:  begin
                    FileName := ExtractFileName(FEmailFiles[FImageNumber - 1]);
                    StrPCopy(MsgLine, 'Content-Type: ' +
                                      FilenameToContentType(FileName) +
                                      '; name="' + FileName + '"');
                end;
            3:  StrPCopy(MsgLine, 'Content-Transfer-Encoding: base64');
            4:  StrPCopy(MsgLine, 'Content-Disposition: inline; filename="' +
                                  ExtractFileName(FEmailFiles[FImageNumber - 1])
                                   + '"');
            5:  StrPCopy(MsgLine, 'Content-ID: <IMAGE' +
                                  IntToStr(FImageNumber) + '>');
            6:  begin
                    StrPCopy(MsgLine, '');
                    FStream := InitFileEncBase64(FEmailFiles[FImageNumber - 1],
                                                 FShareMode);
                end;
            else
                if Assigned(FStream) then begin
                    LineBuf := DoFileEncBase64(FStream, More);
                    StrPCopy(MsgLine, LineBuf);
                    if not More then begin
                        { We hit the end-of-file }
                        EndFileEncBase64(FStream);
                        More := TRUE;
                    end;
                end
                else begin
                    { We hit the end of image file }
                    FLineOffset  := LineNum;
                    FImageNumber := FImageNumber + 1;
                    StrPCopy(MsgLine, '');
                end
            end;
        end;
    end;
    if FMimeState = smtpMimeImages then
        Exit;

    More := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.TriggerProcessHeader(HdrLines: TStrings);
var
    I : Integer;
begin
    if FContentType = smtpPlainText then begin
        { Strange but the user ask to send plain text message }
        inherited TriggerProcessHeader(HdrLines);
        Exit;
    end;

    FMimeState := smtpMimeIntro;
    GenerateBoundaries;
    { We must replace the Content-Type from parent component to our own }
    for I := 0 to HdrLines.Count - 1 do begin
        if CompareText(Copy(HdrLines[I], 1, 13), 'Content-Type:') = 0 then begin
            HdrLines[I] := 'Content-Type: multipart/related; ' +
                           'type="multipart/alternative"; ' +
                           'boundary="' + FOutsideBoundary + '"';
            break;
        end;
    end;
    inherited TriggerProcessHeader(HdrLines);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.GenerateBoundaries;
var
    TickPart : String;
    RandPart : String;
begin
    TickPart := '----=_NextPart_000_' + IntToHex(LongInt(GetTickCount), 8);
    RandPart := IntToHex(Random(High(Integer)), 8);
    FOutsideBoundary := TickPart + '_0.' + RandPart;
    FInsideBoundary  := TickPart + '_1.' + RandPart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlSmtpCli.PrepareEMail;
begin
    if FContentType = smtpPlainText then begin
        { Strange but the user ask to send plain text message }
        inherited PrepareEMail;
        Exit;
    end;
    { Nothing to do here }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TSmtpCli, TSyncSmtpCli, THtmlSmtpCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

