{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (GUI)
              Marco van de Voort (Console
Description:  This is a demo program showing how to use the TFtpServer
              component to build a console FTP server.
              Waring: As this demo is written, full access is given to all
              users to all files accessible by the computer running the demo.
              In production program, you should add code to implement
              security issues.
Creation:     April 21, 1998
Version:      1.07 (console)
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@overbyte.be>

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

              MvdV: Same license as above.

History:
Apr 29, 1998  V0.90 Released for beta testing.
Apr 30, 1998  V0.91 Added an example of virtual file (see the code for
              FtpServer1RetrSessionConnected.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Added tools menu.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Show how to refuse a client in OnClientConnected
Oct 25, 2000  V1.02 Added "List Clients" menu item and coding.
Jun 18, 2001  V1.03 Display version informations
Jul 30, 2001  V1.04 Add Trim function for Delphi 1
Feb 26, 2002  V1.05 Add DisconectAll in main menu
Jun 07, 2002  V1.06 Added a processing thread (not for Delphi 1) for Get
sep 13, 2003  V1.07 First console version, still issues.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
Program conftpsrv;

{$include icsdef.inc}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UseWindows}Windows{$ELSE}WinTypes, WinProcs{$ENDIF}, Messages,
  SysUtils, Classes, IniFiles, FtpSrv, FtpSrvC, WSocket, Winsock,
  ConApp;

{$IFDEF VER80}
 Console apps not supported under Delphi 1
{$ENDIF}


const
  FtpServVersion      = 106;
  CopyRight : String  = ' FtpServer (c) 1998-2002 F. Piette V1.06 ';
  WM_APPSTARTUP       = WM_USER + 2;

type
  TLogMsg = class(TComponent)
  public
     procedure Text(Prefix : Char; Msg : String);
  end;

  TGetProcessingThread = class; { Forward declaration }
  { We use our own client class to hold our thread }
  TMyClient  = class(TFtpCtrlSocket)
  private
      FWorkerThread : TGetProcessingThread;
  end;

  TGetProcessingThread = class(TThread)
  protected
    Server : TFtpServer;
    Client : TMyClient;
  public
      procedure Execute; override;
  end;

  TConFtpServer = class(TConApplication)
    FtpServer1: TFtpServer;
    procedure FtpServer1ClientConnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1ClientDisconnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1Start(Sender: TObject);
    procedure FtpServer1Stop(Sender: TObject);
    procedure FtpServer1ClientCommand(Sender: TObject;
      Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
    procedure FtpServer1StorSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1StorSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrDataSent(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);

    procedure FtpServer1AnswerToClient(Sender: TObject;
      Client: TFtpCtrlSocket; var Answer: TFtpString);
    procedure FtpServer1Authenticate(Sender: TObject;
      Client: TFtpCtrlSocket; UserName, Password: TFtpString;
      var Authenticated: Boolean);
    procedure FtpServer1ChangeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);

    procedure FtpServer1BuildDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure FtpServer1AlterDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure Cleardisplay1Click(Sender: TObject);
    procedure DisconnectAllMnuClick(Sender: TObject);
    procedure FtpServer1GetProcessing(Sender: TObject;
      Client: TFtpCtrlSocket; var DelayedSend : Boolean);
    procedure   WndProc(var Msg: TMsg); override;
  private
    FInitialized      : Boolean;
    FIniFileName      : String;
    FPort             : String;
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure StartServer;
    procedure StopServer;
    procedure UpdateClientCount;
    procedure WorkerThreadTerminated(Sender : TObject);
  public
     Constructor Create(AOwner:TComponent); override;
     procedure   Execute; override;
     Destructor  Destroy; override;

  end;


const
    MainTitle         = 'FTP Server - http://www.overbyte.be';

    { Ini file layout }
    SectionData       = 'Data';
    KeyPort           = 'Port';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLogMsg.Text(Prefix : Char; Msg : String);
begin

end;

Var
  Log          : TLogMsg;

Constructor TConFtpServer.Create(AOwner:TComponent);

Begin
  inherited;
    { Build Ini file name }
    FIniFileName := LowerCase(ExtractFileName(ConApplication.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    { Create the Log object }
    Log := TLogMsg.Create(Self);
    FTPServer1:=TFtpServer.Create(Self);
{    BigConsole(80, 100); }
End;

Destructor  TConFtpServer.Destroy;
var
    IniFile : TIniFile;

begin
    try
        StopServer;
        IniFile := TIniFile.Create(FIniFileName);
        IniFile.WriteString(SectionData,    KeyPort,   FPort);
        IniFile.Free;
    except
        { Ignore any exception when we are closing }
    end;
  inherited;
End;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TConFtpServer.WndProc(var Msg: TMsg);
var
    AMsg : TMessage;
begin
    if Msg.hwnd <> 0 then begin       // Ignore any non-thread message
        inherited;                    // and let parent handle it
        Exit;
    end;

    { We will either process the messages ourself, or let the paranet }
    { handle them.                                                    }
    { Transfert message structure to from Windows-like structure to   }
    { Delpki-like TMessage structure                                  }
    AMsg.Msg    := Msg.Message;
    AMsg.LParam := Msg.lParam;
    AMsg.WParam := Msg.wParam;
    AMsg.Result := 0;
    case Msg.message of
    WM_APPSTARTUP: WMAppStartup(AMsg);
    { Here come other messages we would like to handle }
    else
        inherited;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.LoadConfig;
var
    IniFile : TIniFile;
begin
    Writeln('Loading .INI');
    IniFile := TIniFile.Create(FIniFileName);
    FPort   := IniFile.ReadString(SectionData,    KeyPort,   'ftp');
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.SaveConfig;
var
    IniFile : TIniFile;
begin
    Writeln('Saveing .INI');
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyPort, FPort);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This message handler is triggered by the first execute call.}
procedure TConFtpServer.WMAppStartup(var msg: TMessage);
var
    PrvWnd  : HWND;
    Buf     : String;
begin
    { Prevent the server from running twice }
    Buf    := ClassName + #0;
    PrvWnd := FindWindow(@Buf[1], MainTitle);
    if PrvWnd <> 0 then begin
        Log.Text('E', 'Server already running. Shutdown.');
        // Close; {FormClose?}
        Exit;
    end;
    StartServer;
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ To debug event driven programs, it is often handy to just use writeln to  }
{ write debug messages to the console. To get a console, just ask the       }
{ linker to build a console mode application. Then you'll get the default   }
{ console. The function below will make it the size you like...             }
procedure BigConsole(nCols, nLines : Integer);
var
    sc : TCoord;
    N  : DWord;
begin

    if not IsConsole then
        Exit;
    sc.x := nCols;
    sc.y := nLines;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), sc);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                            BACKGROUND_BLUE or BACKGROUND_GREEN or
                            BACKGROUND_RED or BACKGROUND_INTENSITY);
    sc.x := 0;
    sc.y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                               BACKGROUND_BLUE or BACKGROUND_GREEN or
                               BACKGROUND_RED or BACKGROUND_INTENSITY,
                               nCols * nLines, sc, N);

end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.StartServer;
var
    wsi     : TWSADATA;
begin
    //Update;

    { Display version info for program and use components }
    wsi := WinsockInfo;
    writeln('info',Trim(CopyRight));
    writeln('info','Using:');
    writeln('info','   ' + WSocket.CopyRight);
    writeln('info','   ' + FtpSrv.CopyRight);
    writeln('info','    Winsock:');
    writeln('info','        Version ' +
            Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                             WinsockInfo.wHighVersion and 15]));
    writeln('info','        ' + StrPas(@wsi.szDescription));
    writeln('info','        ' + StrPas(@wsi.szSystemStatus));
    {$IFNDEF VER100}
    { A bug in Delphi 3 makes lpVendorInfo invalid }
    if wsi.lpVendorInfo <> nil then
        writeln('info','        ' + StrPas(wsi.lpVendorInfo));
    {$ENDIF}
    { If not running 16 bits, we use our own client class }
    FtpServer1.ClientClass := TMyClient;
    { Use a custom multiline banner }
    FtpServer1.Banner := '220-Welcome to my Server' + #13#10 +
                         '220-' + #13#10 +
                         '220 ICS FTP Server ready.';
    FtpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.StopServer;
begin
    FtpServer1.Stop;
    FtpServer1.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.UpdateClientCount;
begin

    if FtpServer1.ClientCount = 0 then
        Writeln('No user')
    else
        Writeln(IntToStr(FtpServer1.ClientCount) + ' users');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1ClientConnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    { The next test shows how to refuse a client }
    if Client.GetPeerAddr = '193.121.12.25' then begin
        Client.SendStr('421 Connection not allowed.' + #13#10);
        Client.Close;
        Exit;
    end;
    writeln('info','! ' + Client.GetPeerAddr + ' connected');
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1ClientDisconnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    writeln('info','! ' + Client.GetPeerAddr + ' disconnected');
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1Start(Sender: TObject);
begin
    writeln('info','! Server started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1Stop(Sender: TObject);
begin
    writeln('info','! Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1StorSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        writeln('info','! ' + Client.GetPeerAddr +
                           ' Data session failed to open. Error #' +
                           IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1StorSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        writeln('info','! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1RetrDataSent(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        writeln('info','! ' + Client.GetPeerAddr +
                           ' Data sent. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the data session for a get file has     }
{ been opened. This is a good place build a file or a stream if the data    }
{ requested is not already stored in a file on the file system.             }
{ This feature is very powerfull and enable the FTP protocol to be used to  }
{ retrieve any kind of data. It this sample, we just check for C:\VIRTUAL   }
{ directory. If this directory is curent, then a TMemoryStream is created   }
{ on the fly with some data. If another directory is selected, the FTP      }
{ server works as any other: just send the requested file, if it exist !    }
{ This event handler is also a place where you can abort the file transfer. }
{ Simply trigger an exception and transfer will not take place.             }
{ Note that if you just wants to prohibe access to some directory or file,  }
{ the best place to code that is in the OnValidateGet or OnValidatePut      }
{ event handlers.                                                           }
procedure TConFtpServer.FtpServer1RetrSessionConnected(Sender: TObject;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Error  : Word);
var
    Buf : String;
begin
    if Error <> 0 then
        writeln('info','! ' + Client.GetPeerAddr +
                           ' Data session connected. Error #' + IntToStr(Error))
    else if Copy(UpperCase(Client.FilePath), 1, 19) = 'C:\VIRTUAL\FORBIDEN' then
        raise Exception.Create('Access prohibed !')
    else if Copy(UpperCase(Client.FilePath), 1, 11) = 'C:\VIRTUAL\' then begin
        writeln('info','! VIRTUAL FILE');
        Client.UserData   := 1;        { Remember we created a stream }
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy; { Prevent memory leaks         }
        Client.DataStream := TMemoryStream.Create;
        Buf := 'This is a file created on the fly by the FTP server' + #13#10 +
               'It could result of a query to a database or anything else.' + #13#10 +
               'The request was: ''' + Client.FilePath + '''' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
        Client.DataStream.Seek(0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1RetrSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        writeln('info','! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
    if Client.UserData = 1 then begin
        { We created a stream for a virtual file or dir. Delete the TStream }
        if Assigned(Client.DataStream) then begin
            { There is no reason why we should not come here, but who knows ? }
            Client.DataStream.Destroy;
            Client.DataStream := nil;
        end;
        Client.UserData   := 0;     { Reset the flag }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the FTP component needs to build a      }
{ directory listing. You can just return without doing anything then the    }
{ component will build the directory for you, based on the actual disk      }
{ content. But you can also build your own directory listing with anything  }
{ you like in it. Just create a stream with the required content. The       }
{ example below construct a virtual directory when the user is on the       }
{ C:\VIRTUAL subdirectory (use elsewhere in this sample program).           }
procedure TConFtpServer.FtpServer1BuildDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\VIRTUAL\' then
        Exit;
    writeln('info','! VIRTUAL DIR');
    Client.UserData   := 1;        { Remember we created a stream }
    if Assigned(Client.DataStream) then
        Client.DataStream.Destroy; { Prevent memory leaks         }
    Client.DataStream := TMemoryStream.Create;
    if Detailed then
        { We need to format directory lines according to the Unix standard }
        Buf :=
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 FORBIDEN' + #13#10 +
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 TEST' + #13#10 +
      'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 SOME DIR' + #13#10
    else
        Buf := 'FORBIDEN' + #13#10 +
               'TEST' + #13#10;
    Client.DataStream.Write(Buf[1], Length(Buf));
    Client.DataStream.Seek(0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by the FTP component once it has built the   }
{ directory listing. We can use this handler to alter the listing, adding   }
{ or removing some info. This sample add the 'virtual' directory.           }
procedure TConFtpServer.FtpServer1AlterDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\' then
        Exit;
    { Add our 'virtual' directory to the list }
    if Detailed then begin
        { We need to format directory lines according to the Unix standard }
        Buf :=
        'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1ClientCommand(Sender: TObject;
  Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
begin
    writeln('info','< ' + Client.GetPeerAddr + ' ' +
                       Keyword + ' ' + Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1AnswerToClient(Sender: TObject;
  Client: TFtpCtrlSocket; var Answer: TFtpString);
begin
    writeln('info','> ' + Client.GetPeerAddr + ' ' + Answer)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1Authenticate(Sender: TObject;
  Client: TFtpCtrlSocket; UserName, Password: TFtpString;
  var Authenticated: Boolean);
begin
    { You should place here the code needed to authenticate the user. }
    { For example a text file with all permitted username/password.   }
    { If the user can't be authenticated, just set Authenticated to   }
    { false before returning.                                         }
    { It is also the right place to setup Client.HomeDir              }
    { If you need to store info about the client for later processing }
    { you can use Client.UserData to store a pointer to an object or  }
    { a record with the needed info.                                  }
    writeln('info','! ' + Client.GetPeerAddr +
                       ' User ''' + UserName + ''' is authenticated');
    if Password = 'bad' then
        Authenticated := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.FtpServer1ChangeDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
begin
{$IFDEF NEVER}
    { It the right place to check if a user has access to a given directory }
    { The example below disable C:\ access to non root user.                }
    if (UpperCase(Client.UserName) <> 'ROOT') and
       (UpperCase(Client.Directory) = 'C:\') then
       Allowed := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.Cleardisplay1Click(Sender: TObject);
begin

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.DisconnectAllMnuClick(Sender: TObject);
begin
    FtpServer1.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TConFtpServer.FtpServer1GetProcessing(
    Sender          : TObject;
    Client          : TFtpCtrlSocket;
    var DelayedSend : Boolean);
var
    MyServer : TFtpServer;
    MyClient : TMyClient;
begin
    MyServer := Sender as TFtpServer;
    MyClient := Client as TMyClient;
    { If client request a *.ZZZ file, then start a thread to do some      }
    { processing (here the thread just sleep 10 sec to show other clients }
    { are not blocked.                                                    }
    if UpperCase(ExtractFileExt(MyClient.FileName)) = '.ZZZ' then begin
        MyClient.FWorkerThread := TGetProcessingThread.Create(TRUE);
        MyClient.FWorkerThread.Server          := MyServer;
        MyClient.FWorkerThread.Client          := MyClient;
        MyClient.FWorkerThread.FreeOnTerminate := TRUE;
        MyClient.FWorkerThread.OnTerminate     := WorkerThreadTerminated;
        MyClient.FWorkerThread.Resume;
        { Ask server component to not start sending immediately           }
        { We will ask to start sending from WorkerThreadTerminated event  }
        DelayedSend := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConFtpServer.WorkerThreadTerminated(Sender : TObject);
var
    MyThread : TGetProcessingThread;
begin
    MyThread := Sender as TGetProcessingThread;
    MyThread.Server.DoStartSendData(MyThread.Client);
end;

procedure TConFtpServer.Execute;


var
    IniFile : TIniFile;
    msg:tmessage;
    Minim   : Integer;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        Writeln('Starting ',MainTitle);

        LoadConfig;
        SaveConfig;    { Create the inifile keys if they don't exists }

        { We use a custom message to initialize things once the form }
        { is visible               }
        PostMessage(0, WM_APPSTARTUP, 0, 0);
//        WMAppStartup( msg);
    end;

//  FormShow(self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGetProcessingThread.Execute;
begin
    Sleep(10000);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
  FtpS: TConFtpServer;

begin
  try
    TConApplication.CreateInstance(TConFtpServer);
    TConApplication.Run;
    TConApplication.Done;
  except
    on E:Exception do
        WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
  end;
end.


