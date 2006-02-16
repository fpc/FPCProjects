program lFTPClient;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Crt, lFTP, lNet, lTelnet;
  
type

  { TProgresser }

  { TClient }

  TClient = class
   private
    procedure OnConnect(Sender: TLFTPClient);
    procedure OnReceive(Sender: TLFTPClient);
    procedure OnControl(Sender: TLFTPClient);
    procedure OnSent(Sender: TLFTPClient; const Bytes: Integer);
    procedure OnError(const msg: string; aSocket: TLSocket);
   protected
    FCon: TLFTPClient;
    FConnected: Boolean;
    FQuit: Boolean;
    FGetting: Boolean;
    FFile: TFileStream;
    function UserString: string;
    function GetAnswer(const s: string; const NoEcho: Boolean = False): string;
    procedure PrintHelp;
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run(const Host: string; const Port: Word);
  end;

procedure TClient.OnConnect(Sender: TLFTPClient);
begin
  FConnected:=True;
  Writeln('Connected succesfuly');
end;

procedure TClient.OnReceive(Sender: TLFTPClient);
const
  BUFFER_SIZE = 65535;
var
  n: Integer;
  Buf: array[0..BUFFER_SIZE-1] of Byte;
begin
  if FGetting then begin
    Write('.');
    n:=FCon.GetData(Buf, BUFFER_SIZE);
    if n = 0 then begin
      FGetting:=False;
      FreeAndNil(FFile);
    end else
      FFile.Write(Buf, n);
  end else Write(FCon.GetDataMessage);
end;

procedure TClient.OnControl(Sender: TLFTPClient);
var
  s: string;
begin
  if Sender.GetMessage(s) > 0 then
    Writeln(s);
end;

procedure TClient.OnSent(Sender: TLFTPClient; const Bytes: Integer);
begin
  Write('.');
end;

procedure TClient.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
end;

constructor TClient.Create;
begin
  FConnected:=False;
  FCon:=TLFTPClient.Create(nil);
  FCon.OnConnect:=@OnConnect;
  FCon.OnReceive:=@OnReceive;
  FCon.OnControl:=@OnControl;
  FCon.OnSent:=@OnSent;
  FCon.OnError:=@OnError;
end;

destructor TClient.Destroy;
begin
  FCon.Free;
end;

procedure TClient.PrintHelp;
begin
  Writeln('lNet example FTP client copyright (c) 2005 by Ales Katona');
  Writeln('Commands:');
  Writeln('?   - Print this help');
  Writeln('ESC - Quit');
  Writeln('l - List remote directory');
  Writeln('L - Nlst remote directory (lists only files sometimes)');
  Writeln('g/G - Get remote file');
  Writeln('p/P - Put local file');
  Writeln('b/B - Change mode (binary on/off)');
  Writeln('s/S - Get server system info');
  Writeln('h/H - Print server help');
  Writeln('x/X - Print current working directory');
  Writeln('c/C - Change remote directory');
  Writeln('m/M - Create new remote directory');
  Writeln('r/R - Remove remote directory');
  Writeln('n/N - Rename remote file/directory');
  Writeln('d/D - Delete remote file');
  Writeln('e/E - Echo on/off');
  Writeln('f/F - Feature list');
end;

procedure TClient.Run(const Host: string; const Port: Word);
var
  s, Name, Pass, Dir: string;
begin
  Dir:=ExtractFilePath(ParamStr(0));
  FFile:=nil;
  FGetting:=False;
  Name:=GetAnswer('USER [' + GetEnvironmentVariable(UserString) + ']', False);
  if Length(Name) = 0 then
    Name:=GetEnvironmentVariable('USER');
  Pass:=GetAnswer('PASS', True);

  if FCon.Connect(Host, PORT) then begin
    Writeln('Connecting... press any key to cancel');
    repeat
      FCon.CallAction;
      Sleep(1);
      if KeyPressed then Exit;
    until FConnected;
  end else Halt;

  if FCon.Authenticate(Name, Pass) then begin
    FCon.Binary:=True;
    s:='';
    Writeln('Press "?" for help');
    while not FQuit do begin
      if KeyPressed then case ReadKey of
             #27: FQuit:=True;
        'l': FCon.List;
        'L': FCon.Nlst;
             '?': PrintHelp;
        'g', 'G': begin
                    s:=GetAnswer('Filename');
                    if Length(s) > 0 then begin
                      s:=ExtractFileName(s);
                      if FileExists(Dir + s) then
                        DeleteFile(Dir + s);
                      FreeAndNil(FFile);
                      FFile:=TFileStream.Create(Dir + s, fmOpenWrite or fmCreate);
                      FGetting:=True;
                      FCon.Retrieve(s);
                    end;
                  end;
        'p', 'P': begin
                    s:=GetAnswer('Filename');
                    if FileExists(Dir + s) then
                      FCon.Put(Dir + s)
                    else
                      Writeln('No such file "', s, '"');
                  end;
        'b', 'B': FCon.Binary:=not FCon.Binary;
        's', 'S': FCon.SystemInfo;
        'h', 'H': FCon.Help(GetAnswer('Help verb'));
        'x', 'X': FCon.PresentWorkingDirectory;
        'c', 'C': FCon.ChangeDirectory(GetAnswer('New dir'));
        'm', 'M': FCon.MakeDirectory(GetAnswer('New dir'));
        'n', 'N': FCon.Rename(GetAnswer('From'), GetAnswer('To'));
        'r', 'R': FCon.RemoveDirectory(GetAnswer('Dirname'));
        'd', 'D': FCon.DeleteFile(GetAnswer('Filename'));
        'e', 'E': FCon.Echo:=not FCon.Echo;
        'f', 'F': FCon.FeatureList;
      end;
      FCon.CallAction;
      Sleep(1);
    end;
  end else FCon.GetMessage(s);
  if Length(s) > 0 then
    Write(s);
  FreeAndNil(FFile);
end;

function TClient.UserString: string;
begin
  {$ifdef win32}
    Result:='USERNAME';
  {$else}
    Result:='USER';
  {$endif}
end;

function TClient.GetAnswer(const s: string; const NoEcho: Boolean = False): string;
var
  c: Char;
begin
  Result:='';
  Write(s, ': ');
  while True do begin
    FCon.CallAction;
    if KeyPressed then begin
      c:=ReadKey;
      case c of
        #13, #27 : begin
                     Writeln;
                     Exit;
                   end;
        #8       : if Length(Result) > 0 then begin
                     SetLength(Result, Length(Result)-1);
                     if not NoEcho then begin
                       GotoXY(WhereX-1, WhereY);
                       Write(' ');
                       GotoXY(WhereX-1, WhereY);
                     end;
                   end;
      else
        begin
          Result:=Result + c;
          if not NoEcho then
            Write(c);
        end;
      end;
    end;
    Sleep(1);
  end;
end;

var
  aClient: TClient;
  IP: string;
  Port: Word = 21;
begin
  if Paramcount > 0 then begin
    IP:=ParamStr(1);
    PORT:=21;
    if ParamCount > 1 then try
      Port:=Word(StrToInt(ParamStr(2)));
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;

    aClient:=TClient.Create;
    aClient.Run(IP, Port);
    aClient.Free;
  end else Writeln('Usage: ', ParamStr(0), ' IP [PORT]');
end.

