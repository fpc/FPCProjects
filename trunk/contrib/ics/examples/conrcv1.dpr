{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE (original)
              Marco van de Voort (ugly hack to console)
Object:       Show how to use TPop3Prot (POP3 protocol, RFC-1225)
              Written with a service that checks a pop3box for
                commands/input in mind.

Creation:     03 october 1997
Version:      1.03-fpc
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
              francois.piette@pophost.eunet.be
              marco@freepascal.org
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues:
FPiette:      Copyright (C) 1997-2003 by Fran�ois PIETTE
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

MvdV:         Modifications (C) Marco van de Voort.
              License: the BSDish license stated above.

Updates:
Nov 12, 1997  V1.01 Added a GetAll button to get all messages waiting in the
              POP3 server, copying them to a file using the UIDL to build
              the file name (sorry, wont work with D1 because of long file
              name). The message is *NOT* deleted from the POP3 server.
Jan 10, 1998  V1.02 Added port selection
Jul 05, 2002  V1.03 Added header display in separate UI gadget
sep   , 2003  V.03-fpc Console rework

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program ConRcv1;

{$i icsdef.inc}

{$DEFINE DOHEADERS}     // true: find subject with "RUN SCRIPT"
                        // false: list all subjects
{$UNDEF DEBUG}      // debug output.

uses
  {$ifdef UseWindows}Windows, {$else} WinTypes, WinProcs, {$endif}
  Messages, SysUtils, Classes, ConApp, IniFiles, Pop3Prot;

const
    MailRcvVersion = 103;
    CopyRight : String = ' MailRcv demo (c) 1997-2003 F. Piette V1.03 ';

type
  TPOP3ExercizerApp = class(TConApplication)
    Msg       : TStringList;
    Pop3Client: TSyncPop3Cli;
    procedure ReadDataFromIni(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Connectevent     (Sender: TObject);
    procedure Quittevent       (Sender: TObject);
    procedure Userevent        (Sender: TObject);
    procedure Passevent        (Sender: TObject);
    procedure Retrevent        (Sender: TObject);
    procedure Statevent        (Sender: TObject);
    procedure ListAllevent     (Sender: TObject);
    procedure Listevent        (Sender: TObject);
    procedure Deleteevent      (Sender: TObject);
    procedure Noopevent        (Sender: TObject);
    procedure Lastevent        (Sender: TObject);
    procedure Resetevent       (Sender: TObject);
    procedure Topevent         (Sender: TObject);
    procedure Rpopevent        (Sender: TObject);
    procedure Apopevent        (Sender: TObject);
    procedure Nextevent        (Sender: TObject);
    procedure GetAllevent      (Sender: TObject);

    procedure Pop3ClientDisplay(Sender: TObject; Msg: String);
    procedure Uidlevent(Sender: TObject);
    procedure Pop3ClientUidlBegin(Sender: TObject);
    procedure Pop3ClientUidlEnd(Sender: TObject);
    procedure Pop3ClientUidlLine(Sender: TObject);
    procedure Pop3ClientMessageBegin(Sender: TObject);
    procedure Pop3ClientMessageEnd(Sender: TObject);
    procedure Pop3ClientMessageLine(Sender: TObject);
    procedure Pop3ClientListBegin(Sender: TObject);
    procedure Pop3ClientListEnd(Sender: TObject);
    procedure Pop3ClientListLine(Sender: TObject);

    function  ConnectSyncEvent (sender: TObject):boolean;
    function  OpenSyncEvent    (sender: TObject):boolean;
    function  UserSyncEvent    (sender: TObject):boolean;
    function  PassSyncEvent    (sender: TObject):boolean;
    function  QuittSyncEvent   (sender: TObject):boolean;
    function  AbortSyncEvent   (sender: TObject):boolean;
    function  RetrSyncEvent    (sender: TObject):boolean;
    function  StatSyncEvent    (sender: TObject):boolean;
    function  ListAllSyncEvent (sender: TObject):boolean;
    function  ListSyncEvent    (sender: TObject):boolean;
    function  DeleteSyncEvent  (sender: TObject):boolean;
    function  NoopSyncEvent    (sender: TObject):boolean;
    function  LastSyncEvent    (sender: TObject):boolean;
    function  ResetSyncEvent   (sender: TObject):boolean;
    function  TopSyncEvent     (sender: TObject):boolean;
    function  RpopSyncEvent    (sender: TObject):boolean;
    function  UidlSyncEvent    (sender: TObject):boolean;
    function  ApopSyncEvent    (sender: TObject):boolean;

    procedure Pop3ClientRequestDone(Sender: TObject; RqType: TPop3Request;
      Error: Word);
    procedure Openevent(Sender: TObject);
    procedure Abortevent(Sender: TObject);
    procedure Pop3ClientHeaderEnd(Sender: TObject);
  private
    OkMsgnumber : TList;
    SubjectOk    : Boolean;
    Waitfor      : TPop3Request;
    FFile        : TextFile;
    FFileName    : String;
    FFileOpened  : Boolean;
    FGetAllState : Integer;
    FMsgPath     : String;
    fHost,
    fPort,
    fUser,
    fPasswd      : string;
    MsgCount,
    Lines,
    msgnum       : integer;
    procedure Exec(MethodPtr  : TPop3NextProc;
                   MethodName : String); overload;
    Function Exec(MethodPtr  : TPop3Method;
                   MethodName  : String):Boolean; overload;
    procedure MessageBegin(Sender: TObject);
    procedure MessageLine(Sender: TObject);
    procedure GetAllMessageLine(Sender: TObject);
    procedure GetAllRequestDone(Sender: TObject;
                                RqType: TPop3Request; Error: Word);
    procedure NextMessageRequestDone(Sender: TObject;
                                     RqType: TPop3Request; Error: Word);
   public
    { D�clarations publiques }
     Constructor Create(AOwner:TComponent); override;
     Destructor  Destroy; override;
     procedure   Wait;
     procedure   WaitReady;
     procedure   waitfordone(ev: TPop3Request);
  end;

var
  POP3ExcercizerForm: TPOP3ExercizerApp;

const
    IniFileName = 'MAILRCV.INI';
    MsgFileName = 'MAILRCV.TXT';


Constructor TPOP3ExercizerApp.create(AOwner:TComponent);

begin
   inherited create(aowner);
   pop3client:=TSyncPop3Cli.create(self);
   OkMsgNumber:=TList.Create;
   Lines:=0;
end;

Destructor  TPop3ExercizerApp.Destroy;

begin
  OkMsgNumber.Free;
  pop3client.Free;
  inherited;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Restore some data from the INI file                                       }
procedure TPOP3ExercizerApp.ReadDataFromIni(Sender: TObject);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    FHost   := IniFile.ReadString('Data', 'Host',     '');
    FPort   := IniFile.ReadString('Data', 'Port',     '');
    FUser   := IniFile.ReadString('Data', 'UserName', '');
    FPasswd := IniFile.ReadString('Data', 'Password', '');
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Save data to INI file   (unused)                                          }
procedure TPOP3ExercizerApp.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteString('Data', 'Host',     fHost);
    IniFile.WriteString('Data', 'Port',     fPort);
    IniFile.WriteString('Data', 'UserName', fUser);
    IniFile.WriteString('Data', 'Password', fPassWd);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the TPop3Client object wants to display }
{ some information such as connection progress or errors.                   }
procedure TPOP3ExercizerApp.Pop3ClientDisplay(Sender: TObject;
  Msg: String);
begin
  {$IFDEF DEBUG}
  Writeln('TPop3Cli: ',Msg);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All the TPop3Client method are of the same type. The "sync" versions      }
{ are a different type from the other (async) methods, so we need two       }
{ execs()                                                                   }

{ To simplify quickly "scripting" together certain POP3 actions, the Exec
{ methods stuffs some commonly used handlers and fields in
load default values a lot of methods that are dispatched
{ application, we al

Exec transfert the parameters form the various EditBoxes     }
{ to the Pop3Client instance and then call the appropriate method, showing  }
{ the result in the InfoLabel.Caption.                                      }

procedure TPOP3ExercizerApp.Exec(
    MethodPtr  : TPop3NextProc;
    MethodName : String);

begin
    Pop3Client.Host           := fHost;
    Pop3Client.Port           := fPort;
    Pop3Client.UserName       := fUser;
    Pop3Client.PassWord       := fPassWd;
    Pop3Client.MsgNum         := msgnum; //    MsgNumEdit.Text);
    Pop3Client.MsgLines       := Lines; //Msg.count;
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    Pop3Client.OnRequestDone  := Pop3ClientRequestDone;
    Pop3Client.OnMessageBegin := Pop3ClientMessageBegin;
    Pop3Client.OnMessageEnd   := Pop3ClientMessageEnd;
    Pop3Client.OnMessageLine  := Pop3ClientMessageLine;
    Pop3Client.OnListLine     := Pop3ClientListLine;
    {$IFDEF DEBUG}
    writeln('status :', MethodName + ' started');
    {$ENDIF}
    try
        MethodPtr;
        {$IFDEF DEBUG}
        Writeln('status :',MethodName, ' ok');
        {$ENDIF}
    except
        on E:Exception do begin
            {$IFDEF DEBUG}
             Writeln('status :',MethodName, ' failed ',E.Message, ')');
            {$ENDIF}
        end;
    end;
end;


function TPOP3ExercizerApp.Exec(
    MethodPtr  : TPop3Method;
    MethodName : String):boolean;

begin
    Pop3Client.Host           := fHost;
    Pop3Client.Port           := fPort;
    Pop3Client.UserName       := fUser;
    Pop3Client.PassWord       := fPassWd;
    Pop3Client.MsgNum         := msgnum; //    MsgNumEdit.Text);
    Pop3Client.MsgLines       := Lines; //Msg.count;
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    Pop3Client.OnRequestDone  := Pop3ClientRequestDone;
    Pop3Client.OnMessageBegin := Pop3ClientMessageBegin;
    Pop3Client.OnMessageEnd   := Pop3ClientMessageEnd;
    Pop3Client.OnMessageLine  := Pop3ClientMessageLine;
    Pop3Client.OnListLine     := Pop3ClientListLine;

    {$IFDEF DEBUG}
    writeln('status :', MethodName + ' started');
    {$ENDIF}
    try
        Result:=MethodPtr;
        {$IFDEF DEBUG}
        Writeln('status :',MethodName, ' ok');
        {$ENDIF}
    except
        on E:Exception do begin
            {$IFDEF DEBUG}
            Writeln('status :',MethodName, ' failed ',E.Message, ')');
            {$ENDIF}
            Result:=false;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Connectevent(Sender: TObject);
begin
    Exec(Pop3Client.Connect, 'Connect');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Openevent(Sender: TObject);
begin
    Exec(Pop3Client.Open, 'Open');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Userevent(Sender: TObject);
begin
    Exec(Pop3Client.User, 'User');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Passevent(Sender: TObject);
begin
    Exec(Pop3Client.Pass, 'Pass');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Quittevent(Sender: TObject);
begin
    Exec(Pop3Client.Quit, 'Quit');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Abortevent(Sender: TObject);
begin
    Exec(Pop3Client.Abort, 'Abort');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Retrevent(Sender: TObject);
begin
    Exec(Pop3Client.Retr, 'Retr');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Statevent(Sender: TObject);
begin
    Exec(Pop3Client.Stat, 'Stat');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.ListAllevent(Sender: TObject);
begin
    MsgNum:=0;
    Exec(Pop3Client.List, 'List All');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Listevent(Sender: TObject);
begin
    WaitFor:=Pop3Custom;
    Exec(Pop3Client.List, 'List');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Deleteevent(Sender: TObject);
begin
    Exec(Pop3Client.Dele, 'Delete');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Noopevent(Sender: TObject);
begin
    Exec(Pop3Client.Noop, 'Noop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Lastevent(Sender: TObject);
begin
    Exec(Pop3Client.Last, 'Last');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Resetevent(Sender: TObject);
begin
    Exec(Pop3Client.RSet, 'Rset');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Topevent(Sender: TObject);
begin
    Exec(Pop3Client.Top, 'Top');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Rpopevent(Sender: TObject);
begin
    Exec(Pop3Client.RPop, 'Rpop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Uidlevent(Sender: TObject);
begin
    Exec(Pop3Client.Uidl, 'Uidl');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Apopevent(Sender: TObject);
begin
    Exec(Pop3Client.APop, 'Apop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ConnectSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.ConnectSync, 'ConnectSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.OpenSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.OpenSync, 'OpenSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.UserSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.UserSync, 'UserSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.PassSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.PassSync, 'PassSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.QuittSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.QuitSync, 'QuitSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.AbortSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.AbortSync, 'AbortSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.RetrSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.RetrSync, 'RetrSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.StatSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.StatSync, 'StatSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ListAllSyncEvent(Sender: TObject):Boolean;
begin
    MsgNum:=0;
    Result:=Exec(Pop3Client.ListSync, 'List AllSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ListSyncEvent(Sender: TObject):Boolean;
begin
    WaitFor:=Pop3Custom;
    Result:=Exec(Pop3Client.ListSync, 'ListSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.DeleteSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.DeleSync, 'DeleteSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.NoopSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.NoopSync, 'NoopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.LastSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.LastSync, 'LastSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ResetSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.RSetSync, 'RsetSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.TopSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.TopSync, 'TopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.RpopSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.RPopSync, 'RpopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.UidlSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.UidlSync, 'UidlSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPOP3ExercizerApp.ApopSyncEvent(Sender: TObject):Boolean;
begin
    Result:=Exec(Pop3Client.APopSync, 'ApopSync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ message. The MsgNum property gives the message number.                    }
{ This event handler could be used to open the file used to store the msg.  }
{ The file handle could be stored in the TPop3Client.Tag property to be     }
{ easily retrieved by the OnMessageLine and OnMessageEnd event handlers.    }
procedure TPOP3ExercizerApp.Pop3ClientMessageBegin(Sender: TObject);
begin
    SubjectOk:=False;
    {$IFDEF DEBUG}
    Writeln('*** Message ' +IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has detected the end of a   }
{ message, even if there is an error or exception, this event gets called.  }
{ This event handler could be used to close the file used to store the msg. }
procedure TPOP3ExercizerApp.Pop3ClientMessageEnd(Sender: TObject);
begin
    If SubjectOk Then
      Begin
        OkMsgNumber.add(Pointer(msgnum));  // no beauty price here, no dynarr
        {$IFDEF DEBUG}
         Writeln('THIS (',(Sender as TPop3Cli).MsgNum,') IS A GOOD ONE!');
        {$ENDIF}
      END;
    {$IFDEF DEBUG}
    Writeln('*** Message ' +
                          IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' end ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each message line that TPop3Client is    }
{ receiveing. This could be used to write the message lines to a file.      }
procedure TPOP3ExercizerApp.Pop3ClientMessageLine(Sender: TObject);

VAR S : String;

begin
    S:=(Sender as TPop3Cli).LastResponse;
    If Copy(S,1,8)='Subject:' Then
      Begin
        S:=Uppercase(S);
        SubjectOk:=Pos('RUN SCRIPT',S)>0;
      End;
    // Some extra checks on origin here.

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ list line. The MsgNum property gives the message number.                  }
procedure TPOP3ExercizerApp.Pop3ClientListBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    Writeln('*** List begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has received the last list  }
{ line.                                                                     }
procedure TPOP3ExercizerApp.Pop3ClientListEnd(Sender: TObject);
begin

    {$IFDEF DEBUG}
    writeln('*** List End ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each list line received by TPop3Client.  }
procedure TPOP3ExercizerApp.Pop3ClientListLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgSize = ' + IntToStr((Sender as TPop3Cli).MsgSize) + ' ' +
              'Line = ''' + (Sender as TPop3Cli).LastResponse + '''';
    {$IFDEF DEBUG}
    writeln(Buffer);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    Writeln('*** Uidl begin ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlEnd(Sender: TObject);
begin
    {$IFDEF DEBUG}
    writeln('*** Uidl end ***');
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientUidlLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgUidl = ' + (Sender as TPop3Cli).MsgUidl + '''';
    {$IFDEF DEBUG}
    writeln(Buffer);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.MessageBegin(Sender: TObject);
begin
    {$IFDEF DEBUG}
    writeln('Message ' +
                           IntToStr((Sender as TPop3Cli).MsgNum));
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.MessageLine(Sender: TObject);
begin
    {$IFDEF DEBUG}
    writeln((Sender as TPop3Cli).LastResponse);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Nextevent(Sender: TObject);
begin

    Pop3Client.OnMessageBegin := MessageBegin;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := MessageLine;
    Pop3Client.OnRequestDone  := NextMessageRequestDone;
    Pop3Client.MsgNum         := MsgNum;
    Pop3Client.Retr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.NextMessageRequestDone(
    Sender: TObject;
    RqType: TPop3Request; Error: Word);
begin
    if Error <> 0 then
        Exit;
    inc(MsgNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.GetAllMessageLine(Sender: TObject);
begin
    Writeln(FFile, (Sender as TPop3Cli).LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The procedure here after will start an event chain that will eventually   }
{ download all messages for the POP3 server. We cannot simply loop because  }
{ the POP3 compomnet is asynchronous: it will not wait for operation done   }
{ before returning. We must "chain" operations one after the other using    }
{ the OnRequestDone event handler. We use the variable FGetAllState to keep }
{ track of where we are.                                                    }
{ To get all messages, we must first call Stat to know how many messages    }
{ are on the server, then for each message we call Uidl to get a unique     }
{ identifier for each message to build a file name and know if we already   }
{ have a message, then we retrieve the message, then we increment the       }
{ message number and continue until the number of messages is reached.      }
{ We should start a TTimer to handle timeout...                             }
procedure TPOP3ExercizerApp.GetAllevent(Sender: TObject);
var
    IniFile : TIniFile;
begin
    { Get path from INI file }
    IniFile := TIniFile.Create(IniFileName);
    FMsgPath    := IniFile.ReadString('Data', 'MsgPath',
                                  ExtractFilePath(ParamStr(0)));
    IniFile.Free;

    { Be sure to have an ending backslash }
    if (Length(FMsgPath) > 0) and (FMsgPath[Length(FMsgPath)] <> '\') then
        FMsgPath := FMsgPath + '\';

    FGetAllState := 0;
    FFileOpened  := FALSE;
    Pop3Client.OnRequestDone  := GetAllRequestDone;
    Pop3Client.OnMessageBegin := nil;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := GetAllMessageLine;
    Pop3Client.Stat;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a request related to GetAll is done.    }
{ We check for errors and our state variable FGetAllState which tells us    }
{ where we are (stat, uidl or retr which are the 4 commands we use.         }
{ Note that we also could use Dele to remove the messages from the server.  }
procedure TPOP3ExercizerApp.GetAllRequestDone(
    Sender: TObject;
    RqType: TPop3Request; Error: Word);
begin
    if Error <> 0 then begin
        if FFileOpened then begin
            FFileOpened := FALSE;
            CloseFile(FFile);
        end;
        writeln('Error ' + Pop3Client.ErrorMessage);
        Exit;
    end;

    try
        case FGetAllState of
        0: begin     { Comes from the Stat command }
                if Pop3Client.MsgCount < 1 then begin
                    writeln('No message to download.');
                    Exit;
                end;
                Pop3Client.MsgNum := 1;    { Start with first message }
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        1: begin     { Comes from the Uidl command }
                FFileName := FMsgPath + 'Msg ' + Pop3Client.MsgUidl + '.txt';
                if FileExists(FFileName) then begin
                    writeln('Message ' + IntToStr(Pop3Client.MsgNum) + ' already here');
                    if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                        writeln('Finished');
                        Exit;
                    end;
                    Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                    FGetAllState := 1;
                    Pop3Client.Uidl;
                end
                else begin
                    writeln('Message ' + IntToStr(Pop3Client.MsgNum));
                    AssignFile(FFile, FFileName);
                    Rewrite(FFile);
                    FFileOpened  := TRUE;
                    FGetAllState := 2;
                    Pop3Client.Retr;
                end;
           end;
        2: begin     { Comes from the Retr command }
                FFileOpened := FALSE;
                CloseFile(FFile);
                if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                    writeln('Finished');
                    Exit;
                end;
                Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        else
            writeln('Invalid state');
            Exit;
        end;
    except
        on E:Exception do begin
            if FFileOpened then begin
                FFileOpened := FALSE;
                CloseFile(FFile);
            end;
            writeln('Error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientRequestDone(Sender: TObject;
  RqType: TPop3Request; Error: Word);
begin
    WaitFor:=rqtype;
    {$IFDEF DEBUG}
    writeln('Request Done Rq=' + IntToStr(Integer(RqType)) +
                          ' Error=' + IntToStr(Error));
    {$ENDIF}

    if RqType = pop3Stat then begin
                                  MsgCount:=Pop3Client.MsgCount;
        Writeln('Stat ok, ' +
                             IntToStr(Pop3Client.MsgCount) + ' messages ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes');
    end
    else if RqType = pop3List then begin
        Writeln('List ok, ' +
                             IntToStr(Pop3Client.MsgNum)  + ' message ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes');
    end
    else if RqType = pop3Last then begin
        Writeln('Last = ' + IntToStr(Pop3Client.MsgNum));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExercizerApp.Pop3ClientHeaderEnd(Sender: TObject);
begin
//    SubjectEdit.Text := Pop3Client.HeaderSubject;
//    FromEdit.Text    := Pop3Client.HeaderFrom;
//    ToEdit.Text      := Pop3Client.HeaderTo;
end;

procedure TPOP3ExercizerApp.Wait;

Var St : TPop3State;

begin
 st:=Pop3Client.State;
 Writeln('pre state: ',ORD(pop3client.state));
 while Pop3Client.State=st DO ProcessMessages;
 Writeln('post state: ',ORD(pop3client.state));
end;

procedure TPOP3ExercizerApp.WaitReady;

Var Current:TPop3State;

begin
 Current:=Pop3Client.state;
 while (Pop3Client.State<>Pop3Ready) DO
  Begin
     If Current<>Pop3Client.State Then
        begin
           Current:=Pop3Client.State;
           writeln('stage change: ',ORD(current));
        end;
  ProcessMessages;
  end;
end;

procedure TPop3ExercizerApp.waitfordone(ev: TPop3Request);

begin
  While (Waitfor<>ev) AND (pop3client.state<>Pop3Abort) DO
    ProcessMessages;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

var
  App : TPOP3ExercizerApp;
  I   : Integer;

begin
  App:=TPOP3ExercizerApp.Create(NIL);
  App.ReadDataFromIni(NIL);
  App.ConnectSyncEvent(app);
  App.UserSyncEvent(app);
  App.PassSyncEvent(app);
  App.StatSyncEvent(app);                   // loads nr of msgs in App.MsgCount
{$IFDEF DOHEADERS}
  App.OkMsgNumber.Clear;
  App.Lines:=0;                             // Iterate through all headers.
  If App.MsgCount>0 Then
    For I:=1 To App.MsgCount Do
      Begin
        App.msgnum:=I;
        App.TopSyncEvent(app);
      End;
  If App.OkMsgNumber.Count>0 Then
    For I:=0 To App.OkMsgNumber.Count-1 Do
      Begin
        App.msgnum:=Integer(App.OkMsgNumber[I]); // no beauty either
        App.RetrSyncEvent(App);
      End;
{$ELSE}
  App.ListAllEvent(app);              // async is also possible.
  App.WaitForDone(pop3list);          // but for the linear sequence like this not interesting.
{$ENDIF}
  App.AbortEvent(app);
  App.Free;
end.
