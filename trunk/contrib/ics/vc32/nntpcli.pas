{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE
Description:  TNntpCli is a client for the NNTP protocol (RFC-977)
Creation:     December 19, 1997
Version:      1.15
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

Updates:
Dec 30, 1997  V0.91 Bug: StatusCode was not updated for Connect
              Added PostingPermited property and ParseListLine procedure as
              suggested by J. Peter Mugaas <oma00215@mail.wvnet.edu>
Dec 31, 1997  V0.92 Added XOVER, LIST OVERVIEW.FMT and DATE commands
Jan 10, 1998  V0.93 Added OnStateChange event as suggested by J. Peter Mugaas
              <oma00215@mail.wvnet.edu>
Jan 13, 1998  V0.94 Added readonly property State
Feb 02, 1998  V0.95 Corrected a message in the Quit method.
              Added the NntpCliVersion constant.
Feb 03, 1998  V0.96 Added Authenticate method, UserName and PassWord properties.
Apr 13, 1998  V1.00 Added an intermediate message for OnRequestDone event
              Created the Handle property and related WndProc stuff
Apr 21, 1998  V1.01 Corrected buffer overflow in the OnDataAvailable event.
              Thanks to Tim Skinner tim@palacecs.demon.co.uk who found that bug.
Sep 29, 1998  V1.02 Checked length of FLastResponse before writing it to stream.
              Thanks to Michael Bartos <MBartos@ExpoMedia.de> for the hint.
Feb 01, 1999  V1.03 Added nntpConnect to solve connection problem after an
              abort. Thanks to Eric Fortier <efortier@videotron.ca>.
Feb 27, 1999  V1.04 Made Connect, Abort and Quit method virtual so that they
              can be overriden in descending components.
              Checked line length in ParseListLine.
Mar 31, 1999  V1.05 Made all methods virtual.
Aug 14, 1999  V1.06 Implemented MODE READER and XHDR
Aug 20, 1999  V1.07 Revised conditional compilation, adapted for BCB4, set
              compile options same as TWSocket.
Jun 18, 2001  V1.08 Use AllocateHWnd and DeallocateHWnd from wsocket.
              Renamed property WSocket to CtrlSocket (this require code change
              in user application too).
Sep 28, 2002  V1.09 Arnaud SUBTIL <arnaud_courrier@yahoo.fr> changed
              WSocketDataAvailable  so that it doesn't fail when a line is
              longer than the receive buffer.
Oct 26, 2002  V1.10 Fixed double dot problem in GetArticleLineNext.
              Thanks to Steve Blinch <steve@blitzaffe.com> who found it.
              Use TWSocket LineMode to avoid line length limit.
              Introduced LineLimit to allow user to limit max line (accepting
              unlimited lines can result in a Denial Of Service security hole.
Nov 02, 2002  V1.11 Added OnSendData event, OnRcvdData event, SendCount property
              and RcvdCount property to easy progress bar update.
Nov 11, 2002  V1.12 Revised for Delphi 1
Nov 23, 2002  V1.13 Added a port property to allow use of something else than
              NNTP port.
Nov 27, 2002  V1.14 Changed NntpCliVersion to an integer.
Apr 22, 2003  V1.15 Christophe Thiaux <tophet@free.fr> added Port property.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit NntpCli;

{.DEFINE DUMP}

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$Q-}           { Disqble overflow checking           }
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
    WinTypes, WinProcs, SysUtils, Messages, Classes, Forms, WinSock, WSocket,
    MimeUtil;

const
    NntpCliVersion     = 115;
    CopyRight : String = ' TNntpCli (c) 1997-2003 F. Piette V1.15 ';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    NNTP_SND_BUF_SIZE = 255;
{$ELSE}
    NNTP_SND_BUF_SIZE = 4096;
{$ENDIF}
  WM_NNTP_REQUEST_DONE = WM_USER + 1;

type
    TNntpContentType = (nntpHtml,            nntpPlainText);
    TNntpMimeState   = (nntpMimeHeader,      nntpMimeIntro,
                        nntpMimePlainText,   nntpMimeHtmlText,
                        nntpMimeImages,      nntpMimeDone);
    TNntpShareMode   = (nntpShareCompat,     nntpShareExclusive,
                        nntpShareDenyWrite,  nntpShareDenyRead,
                        nntpShareDenyNone);
    TNntpDisplay = procedure(Sender : TObject;
                             MsgBuf : Pointer;
                             MsgLen : Integer) of object;
    TNntpState = (nntpNotConnected, nntpDnsLookup, nntpWaitingBanner,
                  nntpReady, nntpWaitingResponse);
    TNntpRequest = (nntpGroup,           nntpList,         nntpConnect,
                    nntpPost,            nntpHelp,
                    nntpNewGroups,       nntpNewNews,
                    nntpArticleByNumber, nntpArticleByID,
                    nntpBodyByID,        nntpBodyByNumber,
                    nntpHeadByID,        nntpHeadByNumber,
                    nntpStatByID,        nntpStatByNumber,
                    nntpNext,            nntpLast,
                    nntpQuit,            nntpAbort,
                    nntpXOver,           nntpListOverViewFmt,
                    nntpDate,            nntpAuthenticate,
                    nntpModeReader,      nntpXHdr);
    TRequestDone = procedure(Sender: TObject;
                             RqType: TNntpRequest; ErrCode: Word) of object;

    NntpException = class(Exception);

    TNntpCli = class(TComponent)
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Connect; virtual;
        procedure   Abort; virtual;
        procedure   Quit; virtual;
        procedure   Group(NewsGroupName : String); virtual;
        procedure   ArticleByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   ArticleByID(ID : String; DestStream : TStream); virtual;
        procedure   HeadByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   HeadByID(ID : String; DestStream : TStream); virtual;
        procedure   BodyByNumber(Number : Integer; DestStream : TStream); virtual;
        procedure   BodyByID(ID : String; DestStream : TStream); virtual;
        procedure   StatByNumber(Number : Integer); virtual;
        procedure   StatByID(ID : String); virtual;
        procedure   Next; virtual;
        procedure   Last; virtual;     { It is really Prior, but RFC-977 call it Last !}
        procedure   List(DestStream : TStream); virtual;
        procedure   Post(FromStream : TStream); virtual;
        procedure   Help(DestStream : TStream); virtual;
        procedure   Authenticate; virtual;
        procedure   XOver(Articles : String; DestStream : TStream); virtual;
        procedure   ListOverViewFmt(DestStream : TStream); virtual;
        procedure   Date; virtual;
        procedure   ModeReader; virtual;
        procedure   XHdr(DestStream : TStream;
                         Header     : String;
                         Range      : String); virtual;
        procedure   NewGroups(When          : TDateTime;
                              GMTFLag       : Boolean;
                              Distributions : String;
                              DestStream    : TStream);  virtual;
        procedure   NewNews(When          : TDateTime;
                            GMTFLag       : Boolean;
                            NewsGroupName : String;
                            Distributions : String;
                            DestStream    : TStream); virtual;
    protected
        FWindowHandle       : HWND;
{$IFDEF DUMP}
        FDumpStream         : TFileStream;
        FDumpBuf            : String;
{$ENDIF}
        FHost               : String;
        FPort               : String;
        FState              : TNntpState;
        FWSocket            : TWSocket;
        FRequest            : String;
        FRequestType        : TNntpRequest;
        FRequestDoneFlag    : Boolean;
        FSentFlag           : Boolean;
        FStatusCode         : Integer;
        FSendCount          : Integer;  { Count sent bytes     }
        FRcvdCount          : Integer;  { Count received bytes }
        FSendBuffer         : array [0..NNTP_SND_BUF_SIZE - 1] of char;
        FLastResponse       : String;
        FLastCmdResponse    : String;
        FErrorMessage       : String;
        FArticleEstimated   : Integer;
        FArticleFirst       : Integer;
        FArticleLast        : Integer;
        FArticleNumber      : Integer;
        FArticleID          : String;
        FServerDate         : TDateTime;
        FDataStream         : TStream;
        FUserName           : String;
        FPassWord           : String;
        FLineLimit          : Integer;
        FNext               : procedure of object;
        FPostingPermited    : Boolean;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnDataAvailable    : TDataAvailable;
        FOnRequestDone      : TRequestDone;
        FOnDisplay          : TNntpDisplay;
        FOnMessageBegin     : TNotifyEvent;
        FOnMessageEnd       : TNotifyEvent;
        FOnMessageLine      : TNotifyEvent;
        FOnXHdrBegin        : TNotifyEvent;
        FOnXHdrEnd          : TNotifyEvent;
        FOnXHdrLine         : TNotifyEvent;
        FOnStateChange      : TNotifyEvent;
        FOnSendData         : TNotifyEvent;
        FOnRcvdData         : TNotifyEvent;
        procedure SetLineLimit(NewValue : Integer);
        procedure WndProc(var MsgRec: TMessage);
        procedure WMNntpRequestDone(var msg: TMessage); message WM_NNTP_REQUEST_DONE;
        procedure WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WSocketSessionClosed(Sender: TObject; Error: Word);
        procedure WSocketDataSent(Sender: TObject; Error: Word);
        procedure DelayedRequestDone(Error: Word); virtual;
        procedure TriggerStateChange; virtual;
        procedure TriggerRequestDone(Request: TNntpRequest; ErrCode: Word); virtual;
        procedure StateChange(NewState : TNntpState); virtual;
        procedure SendRequest; virtual;
        procedure GroupNext; virtual;
        procedure QuitNext; virtual;
        procedure XHdrLineNext; virtual;
        procedure GetArticleNext; virtual;
        procedure GetArticleLineNext; virtual;
        procedure GetArticleByNumber(RqType: TNntpRequest; Number : Integer; DestStream : TStream); virtual;
        procedure GetArticleByID(RqType: TNntpRequest; ID : String; DestStream : TStream); virtual;
        procedure GetArticle(RqType: TNntpRequest; ID : String; DestStream : TStream); virtual;
        procedure PostNext; virtual;
        procedure PostDone; virtual;
        procedure PostBlock; virtual;
        procedure PostSendNext; virtual;
        procedure DateNext; virtual;
        procedure ModeReaderNext; virtual;
        procedure XHdrNext; virtual;
        procedure AuthenticateNext1; virtual;
        procedure AuthenticateNext2; virtual;
        procedure TriggerSendData; virtual;
        procedure TriggerRcvdData; virtual;
        property  Handle        : HWND                  read  FWindowHandle;
        property  SendCount     : Integer               read  FSendCount
                                                        write FSendCount;
        property  RcvdCount     : Integer               read  FRcvdCount
                                                        write FRcvdCount;
    published
        property CtrlSocket : TWSocket                  read  FWSocket;
        property State      : TNntpState                read  FState;
        property Host       : String                    read  FHost
                                                        write FHost;
        property ErrorMessage : String                  read  FErrorMessage;
        property LastResponse : String                  read  FLastResponse;
        property StatusCode : Integer                   read  FStatusCode;
        property PostingPermited    : Boolean           read  FPostingPermited;
        property ArticleEstimated   : Integer           read  FArticleEstimated;
        property ArticleFirst       : Integer           read  FArticleFirst;
        property ArticleLast        : Integer           read  FArticleLast;
        property ArticleNumber      : Integer           read  FArticleNumber;
        property ArticleID          : String            read  FArticleID;
        property ServerDate         : TDateTime         read  FServerDate;
        property UserName           : String            read  FUserName
                                                        write FUserName;
        property PassWord           : String            read  FPassWord
                                                        write FPassWord;
        property Port               : String            read  FPort
                                                        write FPort;
        property LineLimit          : Integer           read  FLineLimit
                                                        write SetLineLimit;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                        write FOnSessionClosed;
        property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                        write FOnDataAvailable;
        property OnRequestDone : TRequestDone           read  FOnRequestDone
                                                        write FOnRequestDone;
        property OnDisplay : TNntpDisplay               read  FOnDisplay
                                                        write FOnDisplay;
        property OnMessageBegin : TNotifyEvent          read  FOnMessageBegin
                                                        write FOnMessageBegin;
        property OnMessageEnd : TNotifyEvent            read  FOnMessageEnd
                                                        write FOnMessageEnd;
        property OnMessageLine : TNotifyEvent           read  FOnMessageLine
                                                        write FOnMessageLine;
        property OnXHdrBegin : TNotifyEvent             read  FOnXHdrBegin
                                                        write FOnXHdrBegin;
        property OnXHdrEnd : TNotifyEvent               read  FOnXHdrEnd
                                                        write FOnXHdrEnd;
        property OnXHdrLine : TNotifyEvent              read  FOnXHdrLine
                                                        write FOnXHdrLine;
        property OnStateChange : TNotifyEvent           read  FOnStateChange
                                                        write FOnStateChange;
        { Event intended for progress bar update (send)    }
        property OnSendData    : TNotifyEvent           read  FOnSendData
                                                        write FOnSendData;
        { Event intended for progress bar update (receive) }
        property OnRcvdData    : TNotifyEvent           read  FOnRcvdData
                                                        write FOnRcvdData;
    end;

    THtmlNntpCli = class(TNntpCli)
    protected
        FPlainText           : TStrings;
        FHtmlText            : TStrings;
        FAttachedFiles       : TStrings; { File names for attachment }
        FOutsideBoundary     : String;
        FInsideBoundary      : String;
        FMimeState           : TNntpMimeState;
        FHtmlCharSet         : String;
        FLineNumber          : Integer;
        FLineOffset          : Integer;
        FImageNumber         : Integer;
        FContentType         : TNntpContentType;
        FContentTypeStr      : String;
        FHdrSubject          : String;
        FHdrFrom             : String;
        FHdrGroup            : String;
        FHeader              : TStringList;
        FCharSet             : String;
        FShareMode           : Word;
        FStream              : TStream;
        procedure SetPlainText(const newValue: TStrings); virtual;
        procedure SetHtmlText(const newValue: TStrings); virtual;
        procedure SetAttachedFiles(const newValue: TStrings); virtual;
        procedure SetContentType(newValue : TNntpContentType); virtual;
        function  GetShareMode: TNntpShareMode; virtual;
        procedure SetShareMode(newValue: TNntpShareMode); virtual;
        procedure PostBlock; override;
        procedure BuildHeader; virtual;
        procedure GenerateBoundaries; virtual;
        procedure Display(const Msg: String); virtual;
        procedure SendLine(const MsgLine: String); virtual;
        procedure TriggerRequestDone(Request: TNntpRequest; ErrCode: Word); override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   Post(FromStream : TStream); override;
    published
        property PlainText     : TStrings            read  FPlainText
                                                     write SetPlainText;
        property HtmlText      : TStrings            read  FHtmlText
                                                     write SetHtmlText;
        property AttachedFiles : TStrings            read  FAttachedFiles
                                                     write SetAttachedFiles;
        property ContentType   : TNntpContentType    read  FContentType
                                                     write SetContentType;
        property HdrSubject    : String              read  FHdrSubject
                                                     write FHdrSubject;
        property HdrGroup      : String              read  FHdrGroup
                                                     write FHdrGroup;
        property HdrFrom       : String              read  FHdrFrom
                                                     write FHdrFrom;
        property CharSet       : String               read  FCharSet
                                                     write FCharSet;
        property ShareMode     : TNntpShareMode      read  GetShareMode
                                                     write SetShareMode;
    end;

procedure ParseListLine(const Line          : String;
                        var NewsGroupName   : String;
                        var LastArticle     : Integer;
                        var FirstArticle    : Integer;
                        var PostingFlag     : Char);
procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
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
    Result := TrimLeft(TrimRight(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Step over blank spaces                                                    }
function StpBlk(Data : PChar) : PChar;
begin
    Result := Data;
    if Result <> nil then begin
        while (Result^ <> #0) and (Result^ in [' ', #9, #13, #10]) do
            Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  GetInteger(Data : PChar; var Number : Integer) : PChar;
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
function GetMessageID(Data : PChar; var ID : String) : PChar;
begin
    ID     := '';
    Result := StpBlk(Data);
    if Data = nil then
        Exit;

    while not (Result^ in [#0, '<']) do
        Inc(Result);
    if Result^ = '<' then begin
        while Result^ <> #0 do begin
            Inc(Result);
            if Result^ = '>' then
                break;
            ID := ID + Result^;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNewsGroupName(Data : PChar; var GroupName : String) : PChar;
begin
    GroupName := '';
    Result    := StpBlk(Data);
    if Data = nil then
        Exit;

    { Copy until first white space }
    while (Result^ <> #0) and (not (Result^ in [' ', #9])) do begin
        GroupName := GroupName + Result^;
        Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetChar(Data : PChar; var Ch : Char) : PChar;
begin
    Ch     := #0;
    Result := StpBlk(Data);
    if Data = nil then
        Exit;

    Ch := Result^;
    if Ch <> #0 then
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(Data : String) : Integer;
begin
{$IFDEF VER80}
    { Nul terminate string for Delphi 1 }
    Data[Length(Data) + 1] := #0;
{$ENDIF}
    GetInteger(@Data[1], Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TNntpCli, THtmlNntpCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TNntpCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFDEF DUMP}
    FDumpStream := TFileStream.Create('c:\temp\nntpcli.log', fmCreate);
    FDumpBuf    := '---- START -----' + #13 + #10;
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
    FWindowHandle               := WSocket.AllocateHWnd(WndProc);
    FState                      := nntpNotConnected;
    FArticleNumber              := -1;
    FArticleID                  := '';
    FArticleFirst               := -1;
    FArticleLast                := -1;
    FArticleEstimated           := -1;
    FStatusCode                 := 503;  { program fault }
{$IFDEF VER80}
    FLineLimit                  := 255;
{$ELSE}
    FLineLimit                  := 65536;
{$ENDIF}
    FPort                       := 'nntp';
    FWSocket                    := TWSocket.Create(Self);
    FWSocket.LineMode           := TRUE;
    FWSocket.LineEnd            := #13#10;
    FWSocket.LineLimit          := FLineLimit;
    FWSocket.ComponentOptions   := FWSocket.ComponentOptions + [wsoNoReceiveLoop];
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionClosed    := WSocketSessionClosed;
    FWSocket.OnDnsLookupDone    := WSocketDnsLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TNntpCli.Destroy;
begin
{$IFDEF DUMP}
    if Assigned(FDumpStream) then begin
        FDumpBuf := '---- STOP -----' + #13 + #10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.Destroy;
    end;
{$ENDIF}
    if Assigned(FWSocket) then
        FWSocket.Destroy;
    WSocket.DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.SetLineLimit(NewValue: Integer);
begin
    if FLineLimit <> NewValue then begin
        FLineLimit         := NewValue;
        FWSocket.LineLimit := FLineLimit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_NNTP_REQUEST_DONE : WMNntpRequestDone(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WMNntpRequestDone(var msg: TMessage);
begin
    TriggerRequestDone(TNntpRequest(Msg.WParam), Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerRequestDone(
    Request : TNntpRequest;
    ErrCode : Word);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, Request, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StateChange(NewState : TNntpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.SendRequest;
begin
    FLastCmdResponse := '';
{$IFDEF DUMP}
    FDumpBuf := '<|';
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
    FDumpStream.WriteBuffer(FRequest[1], Length(FRequest));
    FDumpBuf := '|' + #13#10;
    FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
    FWSocket.SendStr(FRequest + #13 + #10);
    FSendCount := (FSendCount + Length(FRequest) + 2) and $7FFFFFF;
    TriggerSendData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Connect;
begin
    if FState <> nntpNotConnected then
         raise NntpException.Create('Already connected');

    FRequestType      := nntpConnect;
    FRequestDoneFlag  := FALSE;
    FRequest          := '';
    FArticleNumber    := -1;
    FArticleID        := '';
    FArticleFirst     := -1;
    FArticleLast      := -1;
    FArticleEstimated := -1;
    StateChange(nntpDnsLookup);
    FWSocket.DnsLookup(FHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Group(NewsGroupName : String);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for GROUP');

    FRequestDoneFlag := FALSE;
    FRequestType     := nntpGroup;
    FRequest         := 'GROUP ' + Trim(NewsGroupName);
    FNext            := GroupNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GroupNext;
var
    Data  : PChar;
    Error : Integer;
begin
    Data := GetInteger(@FLastResponse[1], FStatusCode);
    Data := GetInteger(Data, FArticleEstimated);
    Data := GetInteger(Data, FArticleFirst);
    GetInteger(Data, FArticleLast);
    if FStatusCode = 211 then
        Error := 0
    else
        Error := FStatusCode;
    DelayedRequestDone(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ArticleByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpArticleByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ArticleByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpArticleByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.BodyByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpBodyByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.BodyByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpBodyByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.HeadByNumber(Number : Integer; DestStream : TStream);
begin
    GetArticleByNumber(nntpHeadByNumber, Number, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.HeadByID(ID : String; DestStream : TStream);
begin
    GetArticleByID(nntpHeadByID, ID, DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StatByNumber(Number : Integer);
begin
    GetArticleByNumber(nntpStatByNumber, Number, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.StatByID(ID : String);
begin
    GetArticleByID(nntpStatByID, ID, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleByID(
    RqType     : TNntpRequest;
    ID         : String;
    DestStream : TStream);
begin
    GetArticle(RqType, ' <' + ID + '>', DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleByNumber(
    RqType     : TNntpRequest;
    Number     : Integer;
    DestStream : TStream);
begin
    if Number > 0 then
        GetArticle(RqType, ' ' + IntToStr(Number), DestStream)
    else
        GetArticle(RqType, '', DestStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticle(
    RqType     : TNntpRequest;
    ID         : String;
    DestStream : TStream);
var
    Cmd : String;
begin
    case RqType of
    nntpArticleByID, nntpArticleByNumber:
        Cmd := 'ARTICLE';
    nntpBodyByID, nntpBodyByNumber:
        Cmd := 'BODY';
    nntpHeadByID, nntpHeadByNumber:
        Cmd := 'HEAD';
    nntpStatByID, nntpStatByNumber:
        Cmd := 'STAT';
    else
        raise NntpException.Create('Internal error: Invalid Request Type');
    end;

    if FState <> nntpReady then
        raise NntpException.Create('Not ready for ' + Cmd);
    FDataStream      := DestStream;
    FRequestType     := RqType;
    FRequestDoneFlag := FALSE;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := Cmd + ID;
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleNext;
var
    Data  : PChar;
begin
    Data := GetInteger(@FLastResponse[1], FStatusCode);
    if not (FStatusCode in [100, 215, 220, 221,
                            222, 223, 224, 231]) then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;

    Data := GetInteger(Data, FArticleNumber);
    GetMessageID(Data, FArticleID);

    if FStatusCode in [223] then
        DelayedRequestDone(0)
    else begin
        FNext            := GetArticleLineNext;
        FLastCmdResponse := FLastResponse;;
        StateChange(nntpWaitingResponse);

        if Assigned(FOnMessageBegin) then
            FOnMessageBegin(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.GetArticleLineNext;
const
    CrLf : String[2] = #13#10;
begin
    if FLastResponse = '.' then begin
        if FLastCmdResponse <> '' then begin
            FLastResponse    := FLastCmdResponse;
            FLastCmdResponse := '';
        end;
        if Assigned(FOnMessageEnd) then
            FOnMessageEnd(Self);
        DelayedRequestDone(0);
    end
    else begin
        if (Length(FLastResponse) > 1) and  { 26/10/02 }
           (FLastResponse[1] ='.') and (FLastResponse[2] ='.') then
            Delete(FLastResponse, 1, 1);
        if Assigned(FDataStream) then begin
            if Length(FLastResponse) > 0 then
                FDataStream.Write(FLastResponse[1], Length(FLastResponse));
            FDataStream.Write(CrLf[1], Length(CrLf));
        end;
        if Assigned(FOnMessageLine) then
            FOnMessageLine(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Next;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEXT');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNext;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := 'NEXT';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Last;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LAST');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpLast;
    FArticleNumber   := -1;
    FArticleID       := '';
    FRequest         := 'LAST';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.List(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LIST');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpList;
    FRequest         := 'LIST';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Help(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for HELP');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpHelp;
    FRequest         := 'HELP';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Quit;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for QUIT');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpQuit;
    FRequest         := 'QUIT';
    FNext            := QuitNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.QuitNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    DelayedRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Abort;
begin
    FRequestType     := nntpAbort;
    FWSocket.Close;
    FLastResponse    := '205 Closing connection - goodbye';
    FStatusCode      := 205;
    FRequestDoneFlag := FALSE;
    DelayedRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Post(FromStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for POST');
    FDataStream      := FromStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpPost;
    FRequest         := 'POST';
    FSentFlag        := FALSE;
    FNext            := PostNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode <> 340 then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;
    FNext               := PostSendNext;
    FWSocket.OnDataSent := WSocketDataSent;
    PostBlock;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostBlock;
var
    Len  : Integer;
begin
    if FDataStream = nil then
        Len := 0                { No data to send }
    else
        Len := FDataStream.Read(FSendBuffer, SizeOf(FSendBuffer));

    if Len <= 0 then begin
        if FSentFlag then
            Exit;
        FSentFlag := TRUE;
        StrCopy(@FSendBuffer, #13#10 + '.' + #13#10);
        Len := 5;
    end;
    FWSocket.Send(@FSendBuffer, Len);
    Inc(FSendCount, Len);
    TriggerSendData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostSendNext;
begin
    FWSocket.OnDataSent := nil;
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode = 240 then
        DelayedRequestDone(0)
    else
        DelayedRequestDone(FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.PostDone;
begin
    FLastResponse := '441 posting failed';
    PostSendNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.NewGroups(
    When          : TDateTime;
    GMTFLag       : Boolean;
    Distributions : String;
    DestStream    : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEWGROUPS');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNewGroups;
    if When = 0 then
        When := Now;
    FRequest         := 'NEWGROUPS ' + FormatDateTime('yymmdd hhnnss', When);
    if GMTFlag then
        FRequest := FRequest + ' GMT';
    if Length(Distributions) > 0 then
        FRequest     := FRequest + ' <' + Distributions + '>';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.NewNews(
    When          : TDateTime;
    GMTFLag       : Boolean;
    NewsGroupName : String;
    Distributions : String;
    DestStream    : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for NEWNEWS');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpNewNews;
    if When = 0 then
        When := Now;
    if NewsGroupName = '' then
        NewsGroupName := '*';
    FRequest         := 'NEWNEWS ' + NewsGroupName + ' ' +
                        FormatDateTime('yymmdd hhnnss', When);
    if GMTFlag then
        FRequest := FRequest + ' GMT';
    if Length(Distributions) > 0 then
        FRequest     := FRequest + ' <' + Distributions + '>';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Articles can be: a) a single (positive) article number                    }
{                  b) an article number followed by a dash                  }
{                  c) two article numbers separated by a dash               }
procedure TNntpCli.XOver(
    Articles   : String;
    DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for XOVER');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpXOver;
    FRequest         := 'XOVER ' + Articles;
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ListOverViewFmt(DestStream : TStream);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for LIST OVERVIEW.FMT');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpListOverViewFmt;
    FRequest         := 'LIST OVERVIEW.FMT';
    FNext            := GetArticleNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.DateNext;
var
    Data  : PChar;
    Buf   : String;
    Year, Month, Day, Hour, Min, Sec : Word;
begin
    Data := StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 111 then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;
    Buf := Trim(StrPas(Data));
    if Length(Buf) = 14 then begin
        Year  := atoi(Copy(Buf, 1, 4));
        Month := atoi(Copy(Buf, 5, 2));
        Day   := atoi(Copy(Buf, 7, 2));
        Hour  := atoi(Copy(Buf, 9, 2));
        Min   := atoi(Copy(Buf, 11, 2));
        Sec   := atoi(Copy(Buf, 13, 2));
        FServerDate := EncodeDate(Year, Month, Day) +
                       EncodeTime(Hour, Min, Sec, 0);
    end;
    DelayedRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Date;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for DATE');
    FServerDate      := 0;
    FDataStream      := nil;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpDate;
    FRequest         := 'DATE';
    FNext            := DateNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ModeReaderNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode in [200, 201] then
        DelayedRequestDone(0)
    else
        DelayedRequestDone(FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.ModeReader;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for ModeReader');
    FServerDate      := 0;
    FDataStream      := nil;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpModeReader;
    FRequest         := 'MODE READER';
    FNext            := ModeReaderNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.XHdrLineNext;
const
    CrLf : String[2] = #13#10;
begin
    if FLastResponse = '.' then begin
        if FLastCmdResponse <> '' then begin
            FLastResponse    := FLastCmdResponse;
            FLastCmdResponse := '';
        end;
        if Assigned(FOnXHdrEnd) then
            FOnXHdrEnd(Self);
        DelayedRequestDone(0);
    end
    else begin
        if Assigned(FDataStream) then begin
            if Length(FLastResponse) > 0 then
                FDataStream.Write(FLastResponse[1], Length(FLastResponse));
            FDataStream.Write(CrLf[1], Length(CrLf));
        end;
        if Assigned(FOnXHdrLine) then
            FOnXHdrLine(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.XHdrNext;
begin
    GetInteger(@FLastResponse[1], FStatusCode);
    if FStatusCode <> 221 then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;

    FNext            := XHdrLineNext;
    FLastCmdResponse := FLastResponse;;
    StateChange(nntpWaitingResponse);

    if Assigned(FOnXHdrBegin) then
        FOnXHdrBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Header is a header line such as "subject".                                }
{ Range is either:                                                          }
{   an article number                                                       }
{   an article number followed by a dash to indicate all following          }
{   an article number followed by a dash followed by another article number }
{ Range can be replaced by a message id.                                    }
{ If range is empty current article is used.                                }
procedure TNntpCli.XHdr(DestStream : TStream; Header : String; Range : String);
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for XHDR');
    FDataStream      := DestStream;
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpXHdr;
    FRequest         := 'XHDR ' + Header;
    if Length(Range) > 0 then
        Frequest     := FRequest + ' ' + Range;
    FNext            := XHdrNext;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.AuthenticateNext1;
begin
    StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 381 then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;
    FRequestDoneFlag := FALSE;
    FRequest         := 'AUTHINFO PASS ' + FPassWord;
    FNext            := AuthenticateNext2;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.AuthenticateNext2;
begin
    StpBlk(GetInteger(@FLastResponse[1], FStatusCode));
    if FStatusCode <> 281 then begin
        DelayedRequestDone(FStatusCode);
        Exit;
    end;
    DelayedRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.Authenticate;
begin
    if FState <> nntpReady then
        raise NntpException.Create('Not ready for DATE');
    FRequestDoneFlag := FALSE;
    FRequestType     := nntpAuthenticate;
    FRequest         := 'AUTHINFO USER ' + FUserName;
    FNext            := AuthenticateNext1;
    StateChange(nntpWaitingResponse);
    SendRequest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ParseListLine(
    const Line          : String;
    var NewsGroupName   : String;
    var LastArticle     : Integer;
    var FirstArticle    : Integer;
    var PostingFlag     : Char);
var
    Data : PChar;
begin
    if Length(Line) = 0 then
        Exit;
    Data := GetNewsGroupName(@Line[1], NewsGroupName);
    Data := GetInteger(Data, LastArticle);
    Data := GetInteger(Data, FirstArticle);
    GetChar(Data, PostingFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDataSent(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        PostDone;
        Exit;
    end;
    PostBlock;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        DelayedRequestDone(Error)
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := FPort;
        StateChange(nntpWaitingBanner);
        FWSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if Error <> 0 then begin
        DelayedRequestDone(Error);
        FWSocket.Close
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
begin
    Len := FWSocket.RcvdCount;
    if Len < 0 then
        Exit;

    if Len = 0 then begin
        FWSocket.Close;
        Exit;
    end;

    { We use line mode, we will receive complete lines }
    FLastResponse := FWSocket.ReceiveStr;
    FRcvdCount := (FRcvdCount + Length(FLastResponse)) and $7FFFFFF;
    TriggerRcvdData;

    { Remove ending CR/LF, if any }
    if (Length(FLastResponse) >= 1) and
       (FLastResponse[Length(FLastResponse)] = #10) then
        SetLength(FLastResponse, Length(FLastResponse) - 1);
    if (Length(FLastResponse) >= 1) and
       (FLastResponse[Length(FLastResponse)] = #13) then
        SetLength(FLastResponse, Length(FLastResponse) - 1);

    if FRequestType = nntpAbort then
        Exit;

    if Assigned(FOnDisplay) then
        FOnDisplay(Self, @FLastResponse[1], Length(FLastResponse));

{$IFDEF VER80}
    { Add a nul byte at the end of string for Delphi 1 }
    FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
    if FState = nntpWaitingBanner then begin
        StateChange(nntpReady);
        GetInteger(@FLastResponse[1], FStatusCode);
        FPostingPermited := (FStatusCode = 200);
        if Assigned(FOnSessionConnected) then
            FOnSessionConnected(Self, Error);
        PostMessage(Handle, WM_NNTP_REQUEST_DONE, WORD(FRequestType), Error);
    end
    else if FState = nntpWaitingResponse then begin
        if Assigned(FNext) then
            FNext
        else
            StateChange(nntpReady);
    end
    else begin
        if Assigned(FOnDataAvailable) then
            FOnDataAvailable(Self, Error);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    if not (FRequestType in [nntpAbort]) then
        DelayedRequestDone(Error);
    if Assigned(FOnSessionClosed) then
        OnSessionClosed(Self, Error);
    StateChange(nntpNotConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.DelayedRequestDone(Error: Word);
begin
    if FRequestDoneFlag = FALSE then
        PostMessage(Handle, WM_NNTP_REQUEST_DONE, WORD(FRequestType), Error);
    FRequestDoneFlag := TRUE;
    FNext            := nil;
    if FWSocket.State = wsConnected then
        StateChange(nntpReady)
    else
        StateChange(nntpNotConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerSendData;
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNntpCli.TriggerRcvdData;
begin
    if Assigned(FOnRcvdData) then
        FOnRcvdData(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THtmlNntpCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FAttachedFiles := TStringList.Create;
    FPlainText     := TStringList.Create;
    FHtmlText      := TStringList.Create;
    FHtmlCharSet   := 'iso-8859-1';
    FCharSet       := 'iso-8859-1';
    SetContentType(nntpHtml);
    Randomize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THtmlNntpCli.Destroy;
begin
    if Assigned(FHeader) then begin
        FHeader.Destroy;
        FHeader := nil;
    end;
    if Assigned(FAttachedFiles) then begin
        FAttachedFiles.Destroy;
        FAttachedFiles := nil;
    end;
    if Assigned(FPlainText) then begin
        FPlainText.Destroy;
        FPlainText := nil;
    end;
    if Assigned(FHtmlText) then begin
        FHtmlText.Destroy;
        FHtmlText := nil;
    end;
    if Assigned(FStream) then begin
        FStream.Destroy;
        FStream := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SetContentType(newValue: TNntpContentType);
begin
    if FContentType = newValue then
        Exit;
    FContentType := newValue;
    if FContentType = nntpPlainText then
        FContentTypeStr := 'text/plain'
    else
        FContentTypeStr := 'text/html';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SetHtmlText(const newValue: TStrings);
var
    I : Integer;
begin
    FHtmlText.Clear;
    if Assigned(newValue) then
        for I := 0 to newValue.Count - 1 do
            FHtmlText.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SetPlainText(const newValue: TStrings);
var
    I : Integer;
begin
    FPlainText.Clear;
    if Assigned(newValue) then
        for I := 0 to newValue.Count - 1 do
            FPlainText.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SetAttachedFiles(const newValue: TStrings);
var
    I : Integer;
begin
    FAttachedFiles.Clear;
    if Assigned(newValue) then
        for I := 0 to newValue.Count - 1 do
            FAttachedFiles.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SetShareMode(newValue: TNntpShareMode);
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case newValue of
    nntpShareCompat    : FShareMode := fmShareCompat;
    nntpShareExclusive : FShareMode := fmShareExclusive;
    nntpShareDenyWrite : FShareMode := fmShareDenyWrite;
    nntpShareDenyRead  : FShareMode := fmShareDenyRead;
    nntpShareDenyNone  : FShareMode := fmShareDenyNone;
    else
        FShareMode := fmShareDenyWrite;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THtmlNntpCli.GetShareMode: TNntpShareMode;
begin
   begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case FShareMode of
    fmShareCompat    : Result := nntpShareCompat;
    fmShareExclusive : Result := nntpShareExclusive;
    fmShareDenyWrite : Result := nntpShareDenyWrite;
    fmShareDenyRead  : Result := nntpShareDenyRead;
    fmShareDenyNone  : Result := nntpShareDenyNone;
    else
        Result := nntpShareDenyWrite;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.GenerateBoundaries;
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
procedure THtmlNntpCli.BuildHeader;
begin
    if not Assigned(FHeader) then
        FHeader := TStringList.Create
    else
        FHeader.Clear;
    GenerateBoundaries;
    FHeader.Add('From: '       + FHdrFrom);
    FHeader.Add('Newsgroups: ' + FHdrGroup);
    FHeader.Add('Subject: '    + FHdrSubject);
    FHeader.Add('Organization: None');
    FHeader.Add('X-Newsreader: ICS NNTP component (http://www.overbyte.be)');
    FHeader.Add('MIME-Version: 1.0');
    FHeader.Add('Content-Type: multipart/related; ' +
                'type="multipart/alternative"; ' +
                'boundary="' + FOutsideBoundary + '";');
    FHeader.Add('');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.Post(FromStream: TStream);
begin
    FMimeState  := nntpMimeHeader;
    FSentFlag   := FALSE;
    FLineNumber := 0;
    BuildHeader;
    inherited Post(FromStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.Display(const Msg : String);
const
    NulString : String = #0;
begin
    if Assigned(FOnDisplay) then begin
        if Msg <> '' then
            FOnDisplay(Self, @Msg[1], Length(Msg))
        else
            FOnDisplay(Self, @NulString[1], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.SendLine(const MsgLine : String);
begin
    FWSocket.SendStr(MsgLine + #13#10);
    Display(MsgLine);
    Inc(FSendCount, Length(MsgLine) + 2);
    Inc(FLineNumber);
    TriggerSendData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.PostBlock;
var
    FileName : String;
    LineBuf  : String;
    More     : Boolean;
begin
    if FMimeState = nntpMimeHeader then begin
        if FLineNumber < FHeader.Count then
            SendLine(FHeader.Strings[FLineNumber])
        else begin
            FMimeState  := nntpMimeIntro;
            FLineNumber := 0;
        end;
    end;
    if FMimeState = nntpMimeHeader then
        Exit;

    if FMimeState = nntpMimeIntro then begin
        case FLineNumber of
        0: SendLine('This is a multipart MIME formatted message.');
        1, 4, 8: SendLine('');
        2: SendLine('--' + FOutsideBoundary);
        3: SendLine('Content-Type: multipart/alternative; ' +
                    'boundary="' + FInsideBoundary + '"');
        5: SendLine('--' + FInsideBoundary);
        6: SendLine('Content-Type: text/plain; charset="' + FCharSet + '"');
        7: SendLine('Content-Transfer-Encoding: quoted-printable');
        9: begin
                FMimeState  := nntpMimePlainText;
                FLineNumber := 0;
            end;
        end;
    end;
    if FMimeState = nntpMimeIntro then
        Exit;

    if FMimeState = nntpMimePlainText then begin
        if FLineNumber < FPlainText.Count then begin
            LineBuf := FPlainText[FLineNumber];
            if LineBuf > '' then begin  { Common optimisation }
                LineBuf := SplitQuotedPrintableString(
                               EncodeQuotedPrintable(LineBuf));
                DotEscape(LineBuf);
            end;
            SendLine(LineBuf);
        end
        else begin
            case FLineNumber - FPlainText.Count of
            0: SendLine('');
            1: SendLine('--' + FInsideBoundary);
            2: SendLine('Content-Type: text/html; charset="' +
                        FHtmlCharSet + '"');
            3: SendLine('Content-Transfer-Encoding: quoted-printable');
            4: SendLine('');
            else
                FMimeState  := nntpMimeHtmlText;
                FLineNumber := 0;
            end;
        end;
    end;
    if FMimeState = nntpMimePlainText then
        Exit;

    if FMimeState = nntpMimeHtmlText then begin
        if FLineNumber < FHtmlText.Count then begin
            LineBuf := FHtmlText[FLineNumber];
            if LineBuf > '' then begin  { Common optimisation }
                LineBuf := SplitQuotedPrintableString(
                               EncodeQuotedPrintable(LineBuf));
                DotEscape(LineBuf);
            end;
            SendLine(LineBuf);
        end
        else begin
            case FLineNumber - FHtmlText.Count of
            0: SendLine('');
            1: SendLine('--' + FInsideBoundary + '--');
            2: SendLine('');
            else
                FMimeState   := nntpMimeImages;
                FImageNumber := 1;
                FLineNumber  := 0;
            end;
        end;
    end;
    if FMimeState = nntpMimeHtmlText then
        Exit;

    if FMimeState = nntpMimeImages then begin
        if FImageNumber > FAttachedFiles.Count then begin
            case FLineNumber of
            0:  SendLine('--' + FOutsideBoundary + '--');
            else
                FMimeState := nntpMimeDone;
            end;
        end
        else begin
            case FLineNumber of
            0:  SendLine('--' + FOutsideBoundary);
            1:  begin
                    FileName := ExtractFileName(FAttachedFiles[FImageNumber - 1]);
                    SendLine('Content-Type: ' +
                             FilenameToContentType(FileName) +
                             '; name="' + FileName + '"');
                end;
            2:  SendLine('Content-Transfer-Encoding: base64');
            3:  SendLine('Content-Disposition: inline; filename="' +
                         ExtractFileName(FAttachedFiles[FImageNumber - 1])
                         + '"');
            4:  SendLine('Content-ID: <IMAGE' +
                         IntToStr(FImageNumber) + '>');
            5:  begin
                    SendLine('');
                    if Assigned(FStream) then
                        FStream.Destroy;
                    FStream := InitFileEncBase64(FAttachedFiles[FImageNumber - 1],
                                            FShareMode);
                end;
            else
                if Assigned(FStream) then begin
                    LineBuf := DoFileEncBase64(FStream, More);
                    SendLine(LineBuf);
                    if not More then begin
                        { We hit the end-of-file }
                        EndFileEncBase64(FStream);
                    end;
                end
                else begin
                    { We hit the end of image file }
                    SendLine('');
                    FLineNumber  := 0;
                    FImageNumber := FImageNumber + 1;
                end
            end;
        end;
    end;
    if FMimeState = nntpMimeImages then
        Exit;

    if FSentFlag then
        Exit;
    FSentFlag := TRUE;
    SendLine('.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNntpCli.TriggerRequestDone(
    Request : TNntpRequest;
    ErrCode : Word);
begin
    if Assigned(FStream) then begin
        FStream.Destroy;
        FStream := nil;
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

