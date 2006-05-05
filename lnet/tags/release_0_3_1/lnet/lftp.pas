{ lFTP CopyRight (C) 2005-2006 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE for more inFormation.
  Should you find these sources withOut a LICENSE File, please contact
  me at ales@chello.sk
}

unit lFTP;

{$mode objfpc}{$H+}
//{$define debug}

interface

uses
  Classes, lNet, lTelnet;
  
const
  MAX_STATUSCOUNT = 10;

type
  TLFTPStatus = (fsNone, fsCon, fsAuth, fsPasv, fsPort, fsList, fsRetr, fsStor,
                 fsType, fsCWD, fsMKD, fsRMD, fsDEL, fsRNFR, fsRNTO, fsSYS,
                 fsFeat, fsPWD, fsHEL, fsLast);
                 
  TLFTPStatusRec = record
    Status: TLFTPStatus;
    Args: array[1..2] of string;
  end;
                 
  TLFTPClient = class;
                 
  TLFTPClientProgressCallback = procedure (Sender: TLFTPClient; const Bytes: Integer) of object;

  TLFTPClientCallback = procedure (Sender: TLFTPClient) of object;
                 
  { TLFTPStatusStack }

  { TLFTPStatusFront }

  TLFTPStatusFront = class // it's a quque ladies and gents
   protected
    FItems: array[0..MAX_STATUSCOUNT-1] of TLFTPStatusRec;
    FTop, FBottom: Integer;
    FCount: Integer;
    function GetEmpty: Boolean;
   public
    constructor Create;
    function First: TLFTPStatusRec;
    function Remove: TLFTPStatusRec;
    function Insert(const Value: TLFTPStatusRec): Boolean;
    procedure Clear;
    property Count: Integer read FCount;
    property Empty: Boolean read GetEmpty;
  end;

  { TLFTPTelnetClient }
  TLFTPTelnetClient = class(TLTelnetClient)
   protected
    procedure React(const Operation, Command: Char); override;
  end;

  { TLFTPClient }

  TLFTPClient = class(TComponent, ILClient)
  private
    GetStartPort: Word;
   protected
    FControl: TLFTPTelnetClient;
    FData: TLTcp;//TLTcpList;
    FStatus: TLFTPStatusFront;
    FCommandFront: TLFTPStatusFront;
    FStoreFile: TFileStream;
    FTimeout: DWord;
    FExpectedBinary: Boolean;
    FPipeLine: Boolean;
    FStatusFlags: array[TLFTPStatus] of Boolean;
    FSending: Boolean;
    FOnReceive: TLFTPClientCallback;
    FOnSent: TLFTPClientProgressCallback;
    FOnControl: TLFTPClientCallback;
    FOnConnect: TLFTPClientCallback;
    FOnError: TLErrorProc;
    FUsePORT: Boolean;
    FLastPort: Word;
    FStartPort: Word;
    FSL: TStringList; // for evaluation, I want to prevent constant create/free
    function GetTransfer: Boolean;
    function GetEcho: Boolean;
    function GetConnected: Boolean;
    function GetBinary: Boolean;
    function CanContinue(const aStatus: TLFTPStatus; const Arg1, Arg2: string): Boolean;
    function CleanInput(var s: string): Integer;
    procedure SetStartPor(const Value: Word);
    procedure SetEcho(const Value: Boolean);
    procedure SetBinary(const Value: Boolean);
    procedure OnCo(aSocket: TLSocket);
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    procedure OnSe(aSocket: TLSocket);
    procedure OnControlRe(aSocket: TLSocket);
    procedure EvaluateAnswer(const Ans: string);
    procedure PasvPort;
    procedure SendChunk(const CallBack: Boolean);
    procedure ExecuteFrontCommand;
   public
    constructor Create(aOwner: TComponent); override;
    constructor Create(aOwner: TComponent; SocketClass: TLSocketClass);
    destructor Destroy; override;
   public
    function Get(var aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
    function Connect(const HostName: string; const Port: Word = 21): Boolean;
    function Authenticate(const aUsername, aPassword: string): Boolean;
    function GetData(var aData; const aSize: Integer): Integer;
    function GetDataMessage: string;
    function Retrieve(const FileName: string): Boolean;
    function Put(const FileName: string): Boolean; virtual; // because of LCLsocket
    function ChangeDirectory(const DestPath: string): Boolean;
    function MakeDirectory(const DirName: string): Boolean;
    function RemoveDirectory(const DirName: string): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function Rename(const FromName, ToName: string): Boolean;
   public
    procedure List(const FileName: string = '');
    procedure Nlst(const FileName: string = '');
    procedure SystemInfo;
    procedure FeatureList;
    procedure PresentWorkingDirectory;
    procedure Help(const Arg: string);
    procedure Disconnect;
    procedure CallAction;
   public
    property Connected: Boolean read GetConnected;
    property Binary: Boolean read GetBinary write SetBinary;
    property PipeLine: Boolean read FPipeLine write FPipeLine;
    property Timeout: DWord read FTimeout write FTimeout;
    property Echo: Boolean read GetEcho write SetEcho;
    property StartPort: Word read FStartPort write GetStartPort;
    property Transfer: Boolean read GetTransfer;
    property UsePORT: Boolean read FUsePORT write FUsePORT;
    property OnConnect: TLFTPClientCallback read FOnConnect write FOnConnect;
    property OnSent: TLFTPCLientProgressCallback read FOnSent write FOnSent;
    property OnReceive: TLFTPCLientCallback read FOnReceive write FOnReceive;
    property OnControl: TLFTPClientCallback read FOnControl write FOnControl;
    property OnError: TLErrorProc read FOnError write FOnError;
  end;

implementation

uses
  SysUtils;

const
  FLE             = #13#10;
  DEFAULT_TIMEOUT = 10000;
  DEFAULT_PORT    = 1024;
  EMPTY_REC: TLFTPStatusRec = (Status: fsNone; Args: ('', ''));

  FTPStatusStr: array[TLFTPStatus] of string = ('None', 'Connect', 'Authenticate',
                                                'Passive', 'Active', 'List', 'Retrieve',
                                                'Store', 'Type', 'CWD', 'MKDIR',
                                                'RMDIR', 'Delete', 'RenameFrom',
                                                'RenameTo', 'System', 'Features',
                                                'PWD', 'HELP', 'LAST');

procedure Writedbg(const ar: array of const);
{$ifdef debug}
var
  i: Integer;
begin
  if High(ar) >= 0 then
    for i:=0 to High(ar) do
      case ar[i].vtype of
        vtInteger: Write(ar[i].vinteger);
        vtString: Write(ar[i].vstring^);
        vtAnsiString: Write(AnsiString(ar[i].vpointer));
        vtBoolean: Write(ar[i].vboolean);
        vtChar: Write(ar[i].vchar);
        vtExtended: Write(Extended(ar[i].vpointer^));
      end;
  Writeln;
end;
{$else}
begin
end;
{$endif}

function MakeStatusRec(const aStatus: TLFTPStatus; const Arg1, Arg2: string): TLFTPStatusRec;
begin
  Result.Status:=aStatus;
  Result.Args[1]:=Arg1;
  Result.Args[2]:=Arg2;
end;

{ TLFTPStatusFront }

function TLFTPStatusFront.GetEmpty: Boolean;
begin
  Result:=FCount = 0;
end;

constructor TLFTPStatusFront.Create;
begin
  Clear;
end;

function TLFTPStatusFront.First: TLFTPStatusRec;
begin
  Result:=EMPTY_REC;
  if FCount > 0 then
    Result:=FItems[FBottom];
end;

function TLFTPStatusFront.Remove: TLFTPStatusRec;
begin
  Result:=EMPTY_REC;
  if FCount > 0 then begin
    Writedbg(['Removing: ', FTPStatusStr[FItems[FBottom].Status], ' Bottom: ', FBottom, ' Top: ', FTop, ' Count: ', FCount]);
    Result:=FItems[FBottom];
    Dec(FCount);
    Inc(FBottom);
    if FBottom >= MAX_STATUSCOUNT then
      FBottom:=0;
    Writedbg(['FRONT FIRST: ', FTPStatusStr[First.Status], ' Bottom: ', FBottom,
              ' Top: ', FTop, ' Count: ', FCount]);
  end;
end;

function TLFTPStatusFront.Insert(const Value: TLFTPStatusRec): Boolean;
begin
  Result:=False;
  if FCount < MAX_STATUSCOUNT then begin
    if FTop >= MAX_STATUSCOUNT then
      FTop:=0;
    Writedbg(['FRONT: Insering: ', FTPStatusStr[Value.Status], ' Bottom: ',
              FBottom, ' Top: ', FTop, ' Count: ', FCount]);
    FItems[FTop]:=Value;
    Inc(FCount);
    Inc(FTop);
    Writedbg(['FRONT FIRST: ', FTPStatusStr[First.Status], ' Bottom: ', FBottom,
              ' Top: ', FTop, ' Count: ', FCount]);
    Result:=True;
  end;
end;

procedure TLFTPStatusFront.Clear;
begin
  Writedbg(['FRONT: Clearing']);
  FCount:=0;
  FBottom:=0;
  FTop:=0;
end;

{ TLFTPTelnetClient }

procedure TLFTPTelnetClient.React(const Operation, Command: Char);
begin
  // don't do a FUCK since they broke Telnet in FTP as per-usual
end;

{ TLFTPClient }

constructor TLFTPClient.Create(aOwner: TComponent);
begin
  Create(aOwner, nil);
end;

constructor TLFTPClient.Create(aOwner: TComponent; SocketClass: TLSocketClass);
var
  s: TLFTPStatus;
begin
  inherited Create(aOwner);
  FOnError:=nil;
  FStartPort:=DEFAULT_PORT;
  FSL:=TStringList.Create;
  FLastPort:=FStartPort;
  FUsePort:=False;
  FSending:=False;
  FControl:=TLFTPTelnetClient.Create(nil, SocketClass);
  FControl.OnReceive:=@OnControlRe;
  FControl.OnConnect:=@OnCo;
  FControl.OnError:=@OnEr;
  FOnConnect:=nil;
  FOnSent:=nil;
  FOnReceive:=nil;
  for s:=fsNone to fsDEL do
    FStatusFlags[s]:=False;
  FStatus:=TLFTPStatusFront.Create;
  FCommandFront:=TLFTPStatusFront.Create;
  FStoreFile:=nil;
  FData:=TLTcp.Create(nil, SocketClass);
  FData.OnReceive:=@OnRe;
  FData.OnError:=@OnEr;
  FData.OnDisconnect:=@OnDs;
  FData.OnCanSend:=@OnSe;
  FPipeLine:=False;
  FTimeout:=DEFAULT_TIMEOUT;
end;

destructor TLFTPClient.Destroy;
begin
  Disconnect;
  FSL.Free;
  FControl.Free;
  FStatus.Free;
  FCommandFront.Free;
  FData.Free;
  if Assigned(FStoreFile) then
    FreeAndNil(FStoreFile);
  inherited Destroy;
end;

function TLFTPClient.GetTransfer: Boolean;
begin
  Result:=FData.Connected;
end;

function TLFTPClient.GetEcho: Boolean;
begin
  Result:=FControl.OptionIsSet(TS_ECHO);
end;

function TLFTPClient.GetConnected: Boolean;
begin
  Result:=FStatusFlags[fsCon] and FControl.Connected;
end;

function TLFTPClient.GetBinary: Boolean;
begin
  Result:=FStatusFlags[fsType];
end;

function TLFTPClient.CanContinue(const aStatus: TLFTPStatus; const Arg1,
  Arg2: string): Boolean;
begin
  Result:=FPipeLine or FStatus.Empty;

  if not Result then
    FCommandFront.Insert(MakeStatusRec(aStatus, Arg1, Arg2));
end;

function TLFTPClient.CleanInput(var s: string): Integer;
var
  i: Integer;
begin
  FSL.Text:=s;
  if FSL.Count > 0 then
    for i:=0 to FSL.Count-1 do
      if Length(FSL[i]) > 0 then EvaluateAnswer(FSL[i]);
  s:=StringReplace(s, FLE, LineEnding, [rfReplaceAll]);
  i:=Pos('PASS', s);
  if i > 0 then
    s:=Copy(s, 1, i-1) + 'PASS';
  Result:=Length(s);
end;

procedure TLFTPClient.SetStartPor(const Value: Word);
begin
  FStartPort:=Value;
  if Value > FLastPort then
    FLastPort:=Value;
end;

procedure TLFTPClient.SetEcho(const Value: Boolean);
begin
  if Value then
    FControl.SetOption(TS_ECHO)
  else
    FControl.UnSetOption(TS_ECHO);
end;

procedure TLFTPClient.SetBinary(const Value: Boolean);
const
  TypeBool: array[Boolean] of string = ('A', 'I');
begin
  if CanContinue(fsType, BoolToStr(Value), '') then begin
    FExpectedBinary:=Value;
    FControl.SendMessage('TYPE ' + TypeBool[Value] + FLE);
    FStatus.Insert(MakeStatusRec(fsType, '', ''));
  end;
end;

procedure TLFTPClient.OnCo(aSocket: TLSocket);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TLFTPClient.OnEr(const msg: string; aSocket: TLSocket);
begin
  FSending:=False;
  if Assigned(FOnError) then
    FOnError(msg, aSocket);
end;

procedure TLFTPClient.OnRe(aSocket: TLSocket);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Self);
end;

procedure TLFTPClient.OnDs(aSocket: TLSocket);
begin
  // TODO: figure it out brainiac
  FSending:=False;
  Writedbg(['Disconnected']);
end;

procedure TLFTPClient.OnSe(aSocket: TLSocket);
begin
  if Connected and FSending then
    SendChunk(True);
end;

procedure TLFTPClient.OnControlRe(aSocket: TLSocket);
begin
  if Assigned(FOnControl) then
    FOnControl(Self);
end;

procedure TLFTPClient.EvaluateAnswer(const Ans: string);

  function GetNum: Integer;
  begin
    try
      Result:=StrToInt(Copy(Ans, 1, 3));
    except
      Result:=-1;
    end;
  end;

  procedure ParsePortIP(s: string);
  var
    i, l: Integer;
    aIP: string;
    aPort: Word;
    sl: TStringList;
  begin
    if Length(s) >= 15 then begin
      sl:=TStringList.Create;
      for i:=Length(s) downto 5 do
        if s[i] = ',' then Break;
      while (i <= Length(s)) and (s[i] in ['0'..'9', ',']) do Inc(i);
      if not (s[i] in ['0'..'9', ',']) then Dec(i);
      l:=0;
      while s[i] in ['0'..'9', ','] do begin
        Inc(l);
        Dec(i);
      end;
      Inc(i);
      s:=Copy(s, i, l);
      sl.CommaText:=s;
      aIP:=sl[0] + '.' + sl[1] + '.' + sl[2] + '.' + sl[3];
      try
        aPort:=(StrToInt(sl[4]) * 256) + StrToInt(sl[5]);
      except
        aPort:=0;
      end;
      Writedbg(['Server PASV addr/port - ', aIP, ' : ', aPort]);
      if (aPort > 0) and FData.Connect(aIP, aPort) then
        Writedbg(['Connected after PASV']);
      sl.Free;
      FStatus.Remove;
    end;
  end;
  
  procedure SendFile;
  begin
    FStoreFile.Position:=0;
    FSending:=True;
    SendChunk(False);
  end;
  
  function ValidResponse(const Answer: string): Boolean; inline;
  begin
    Result:=(Length(Ans) >= 3) and
            (Ans[1] in ['1'..'5']) and
            (Ans[2] in ['0'..'9']) and
            (Ans[3] in ['0'..'9']);
            
    if Result then
      Result:=(Length(Ans) = 3) or ((Length(Ans) > 3) and (Ans[4] = ' '));
  end;
  
var
  x: Integer;
begin
  x:=GetNum;
  Writedbg(['WOULD EVAL: ', FTPStatusStr[FStatus.First.Status], ' with value: ',
            x, ' from "', Ans, '"']);
  if ValidResponse(Ans) then
    if not FStatus.Empty then begin
      Writedbg(['EVAL: ', FTPStatusStr[FStatus.First.Status], ' with value: ', x]);
      case FStatus.First.Status of
        fsCon  : case x of
                   220:
                     begin
                       FStatusFlags[FStatus.First.Status]:=True;
                       FStatus.Remove;
                     end;
                   else
                     begin
                       FStatusFlags[FStatus.First.Status]:=False;
                       FStatus.Remove;
                     end;
                 end;
        fsAuth : case x of
                   230:
                     begin
                       FStatusFlags[FStatus.First.Status]:=True;
                       FStatus.Remove;
                     end;
                   331,
                   332: FStatusFlags[FStatus.First.Status]:=False;
                   else
                     begin
                       FStatusFlags[FStatus.First.Status]:=False;
                       FStatus.Remove;
                     end;
                 end;
        fsPasv : case x of
                   227: ParsePortIP(Ans);
                   300..600: FStatus.Remove;
                 end;
        fsPort : case x of
                   200: FStatus.Remove;
                   else FStatus.Remove;
                 end;
        fsType : case x of
                   200:
                     begin
                       FStatusFlags[FStatus.First.Status]:=FExpectedBinary;
                       Writedbg(['Binary mode: ', FExpectedBinary]);
                       FStatus.Remove;
                     end;
                   else FStatus.Remove;
                 end;
        fsRetr : case x of
                   150: begin { Do nothing } end;
                   226: FStatus.Remove;
                   else
                     begin
                       FData.Disconnect;
                       Writedbg(['Disconnecting data connection']);
                       FStatus.Remove; // error after connection established
                     end;
                 end;
        fsStor : case x of
                   150: SendFile;
                   
                   226: FStatus.Remove;
                   else FStatus.Remove;
                 end;
        fsCWD  : case x of
                   200, 250:
                     begin
                       FStatusFlags[FStatus.First.Status]:=True;
                       FStatus.Remove;
                     end;
                   else
                     begin
                       FStatusFlags[FStatus.First.Status]:=False;
                       FStatus.Remove;
                     end;
                 end;
        fsList : case x of
                   150: begin end;
                   226: FStatus.Remove;
                   else FStatus.Remove;
                 end;
        fsMKD  : case x of
                   250, 257:
                      begin
                        FStatusFlags[FStatus.First.Status]:=True;
                        FStatus.Remove;
                      end;
                   else
                      begin
                        FStatusFlags[FStatus.First.Status]:=False;
                        FStatus.Remove;
                      end;
                 end;
        fsRMD,
        fsDEL  : case x of
                   250:
                      begin
                        FStatusFlags[FStatus.First.Status]:=True;
                        FStatus.Remove;
                      end;
                   else
                      begin
                        FStatusFlags[FStatus.First.Status]:=False;
                        FStatus.Remove;
                      end;
                 end;
        fsRNFR : case x of
                   350:
                      begin
                        FStatusFlags[FStatus.First.Status]:=True;
                        FStatus.Remove;
                      end;
                   else FStatus.Remove;
                 end;
        fsRNTO : case x of
                   250:
                      begin
                        FStatusFlags[FStatus.First.Status]:=True;
                        FStatus.Remove;
                      end;
                   else FStatus.Remove;
                 end;
      end;
    end;
  if FStatus.Empty and not FCommandFront.Empty then
    ExecuteFrontCommand;
end;

procedure TLFTPClient.PasvPort;

  function StringPair(const aPort: Word): string;
  begin
    Result:=IntToStr(aPort div 256);
    Result:=Result + ',' + IntToStr(aPort mod 256);
  end;
  
  function StringIP: string;
  begin
    Result:=StringReplace(FControl.Connection.Iterator.LocalAddress, '.', ',',
                          [rfReplaceAll]) + ',';
  end;
  
begin
  if FUsePORT then begin
    Writedbg(['Sent PORT']);
    FData.Disconnect;
    FData.Listen(FLastPort);
    FControl.SendMessage('PORT ' + StringIP + StringPair(FLastPort) + FLE);
    FStatus.Insert(MakeStatusRec(fsPort, '', ''));

    if FLastPort < 65535 then
      Inc(FLastPort)
    else
      FLastPort:=FStartPort;
  end else begin
    Writedbg(['Sent PASV']);
    FControl.SendMessage('PASV' + FLE);
    FStatus.Insert(MakeStatusRec(fsPasv, '', ''));
  end;
end;

procedure TLFTPClient.SendChunk(const CallBack: Boolean);
const
  BUFFER_SIZE = 65535;
var
  Buf: array[0..BUFFER_SIZE-1] of Byte;
  n: Integer;
  Sent: Integer;
begin
  repeat
    n:=FStoreFile.Read(Buf, BUFFER_SIZE);
    if n > 0 then begin
      Sent:=FData.Send(Buf, n);
      if CallBack and Assigned(FOnSent) and (Sent > 0) then
        FOnSent(Self, Sent);
      if Sent < n then
        FStoreFile.Position:=FStoreFile.Position - (n - Sent); // so it's tried next time
    end else begin
      if Assigned(FOnSent) then
        FOnSent(Self, 0);
      FreeAndNil(FStoreFile);
      FSending:=False;
      FData.Disconnect;
    end;
  until (n = 0) or (Sent = 0);
end;

procedure TLFTPClient.ExecuteFrontCommand;
begin
  with FCommandFront.First do
    case Status of
      fsNone : Exit;
      fsAuth : Authenticate(Args[1], Args[2]);
      fsList : List(Args[1]);
      fsRetr : Retrieve(Args[1]);
      fsStor : Put(Args[1]);
      fsCWD  : ChangeDirectory(Args[1]);
      fsMKD  : MakeDirectory(Args[1]);
      fsRMD  : RemoveDirectory(Args[1]);
      fsDEL  : DeleteFile(Args[1]);
      fsRNFR : Rename(Args[1], Args[2]);
      fsSYS  : SystemInfo;
      fsPWD  : PresentWorkingDirectory;
      fsHEL  : Help(Args[1]);
      fsType : SetBinary(StrToBool(Args[1]));
      fsFeat : FeatureList;
    end;
  FCommandFront.Remove;
end;

function TLFTPClient.Get(var aData; const aSize: Integer; aSocket: TLSocket): Integer;
var
  s: string;
begin
  Result:=FControl.Get(aData, aSize, aSocket);
  if Result > 0 then begin
    SetLength(s, Result);
    Move(aData, PChar(s)^, Result);
    CleanInput(s);
  end;
end;

function TLFTPClient.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result:=FControl.GetMessage(msg, aSocket);
  if Result > 0 then
    Result:=CleanInput(msg);
end;

function TLFTPClient.Send(const aData; const aSize: Integer; aSocket: TLSocket
  ): Integer;
begin
  Result:=FControl.Send(aData, aSize);
end;

function TLFTPClient.SendMessage(const msg: string; aSocket: TLSocket
  ): Integer;
begin
  Result:=FControl.SendMessage(msg);
end;

function TLFTPClient.GetData(var aData; const aSize: Integer): Integer;
begin
  Result:=FData.Iterator.Get(aData, aSize);
end;

function TLFTPClient.GetDataMessage: string;
begin
  Result:='';
  if Assigned(FData.Iterator) then
    FData.Iterator.GetMessage(Result);
end;

function TLFTPClient.Connect(const HostName: string; const Port: Word): Boolean;
begin
  Result:=False;
  Disconnect;
  if FControl.Connect(HostName, Port) then begin
    FStatus.Insert(MakeStatusRec(fsCon, '', ''));
    Result:=True;
  end;
end;

function TLFTPClient.Authenticate(const aUsername, aPassword: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsAuth, aUserName, aPassword) then begin
    FControl.SendMessage('USER ' + aUserName + FLE + 'PASS ' + aPassword + FLE);
    FStatus.Insert(MakeStatusRec(fsAuth, '', ''));
    Result:=True;
  end;
end;

function TLFTPClient.Retrieve(const FileName: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsRetr, FileName, '') then begin
    PasvPort;
    FControl.SendMessage('RETR ' + FileName + FLE);
    FStatus.Insert(MakeStatusRec(fsRetr, '', ''));
    Result:=True;
  end;
end;

function TLFTPClient.Put(const FileName: string): Boolean;
begin
  Result:=not FPipeLine;
  if FileExists(FileName) and CanContinue(fsStor, FileName, '') then begin
    FStoreFile:=TFileStream.Create(FileName, fmOpenRead);
    PasvPort;
    FControl.SendMessage('STOR ' + ExtractFileName(FileName) + FLE);
    FStatus.Insert(MakeStatusRec(fsStor, '', ''));
    Result:=True;
  end;
end;

function TLFTPClient.ChangeDirectory(const DestPath: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsCWD, DestPath, '') then begin
    FControl.SendMessage('CWD ' + DestPath + FLE);
    FStatus.Insert(MakeStatusRec(fsCWD, '', ''));
    FStatusFlags[fsCWD]:=False;
    Result:=True;
  end;
end;

function TLFTPClient.MakeDirectory(const DirName: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsMKD, DirName, '') then begin
    FControl.SendMessage('MKD ' + DirName + FLE);
    FStatus.Insert(MakeStatusRec(fsMKD, '', ''));
    FStatusFlags[fsMKD]:=False;
    Result:=True;
  end;
end;

function TLFTPClient.RemoveDirectory(const DirName: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsRMD, DirName, '') then begin
    FControl.SendMessage('RMD ' + DirName + FLE);
    FStatus.Insert(MakeStatusRec(fsRMD, '', ''));
    FStatusFlags[fsRMD]:=False;
    Result:=True;
  end;
end;

function TLFTPClient.DeleteFile(const FileName: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsDEL, FileName, '') then begin
    FControl.SendMessage('DELE ' + FileName + FLE);
    FStatus.Insert(MakeStatusRec(fsDEL, '', ''));
    FStatusFlags[fsDEL]:=False;
    Result:=True;
  end;
end;

function TLFTPClient.Rename(const FromName, ToName: string): Boolean;
begin
  Result:=not FPipeLine;
  if CanContinue(fsRNFR, FromName, ToName) then begin
    FControl.SendMessage('RNFR ' + FromName + FLE);
    FStatus.Insert(MakeStatusRec(fsRNFR, '', ''));
    FStatusFlags[fsRNFR]:=False;

    FControl.SendMessage('RNTO ' + ToName + FLE);
    FStatus.Insert(MakeStatusRec(fsRNTO, '', ''));
    FStatusFlags[fsRNTO]:=False;

    Result:=True;
  end;
end;

procedure TLFTPClient.List(const FileName: string = '');
begin
  if CanContinue(fsList, FileName, '') then begin
    PasvPort;
    FStatus.Insert(MakeStatusRec(fsList, '', ''));
    if Length(FileName) > 0 then
      FControl.SendMessage('LIST ' + FileName + FLE)
    else
      FControl.SendMessage('LIST' + FLE);
  end;
end;

procedure TLFTPClient.Nlst(const FileName: string);
begin
  if CanContinue(fsList, FileName, '') then begin
    PasvPort;
    FStatus.Insert(MakeStatusRec(fsList, '', ''));
    if Length(FileName) > 0 then
      FControl.SendMessage('NLST ' + FileName + FLE)
    else
      FControl.SendMessage('NLST' + FLE);
  end;
end;

procedure TLFTPClient.SystemInfo;
begin
  if CanContinue(fsSYS, '', '') then
    FControl.SendMessage('SYST' + FLE);
end;

procedure TLFTPClient.FeatureList;
begin
  if CanContinue(fsFeat, '', '') then
    FControl.SendMessage('FEAT' + FLE);
end;

procedure TLFTPClient.PresentWorkingDirectory;
begin
  if CanContinue(fsPWD, '', '') then
    FControl.SendMessage('PWD' + FLE);
end;

procedure TLFTPClient.Help(const Arg: string);
begin
  if CanContinue(fsHEL, Arg, '') then
    FControl.SendMessage('HELP ' + Arg + FLE);
end;

procedure TLFTPClient.Disconnect;
var
  s: TLFTPStatus;
begin
  FControl.Disconnect;
  FStatus.Clear;
  FData.Disconnect;
  FLastPort:=FStartPort;
  for s:=fsNone to fsLast do
    FStatusFlags[s]:=False;
  FCommandFront.Clear;
end;

procedure TLFTPClient.CallAction;
begin
  FControl.CallAction;
  FData.CallAction;
end;

initialization
  Randomize;

end.

