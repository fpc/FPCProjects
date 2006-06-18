program fpget;

{$mode objfpc}
{$h+}

uses
  sysutils, strutils, lnet, lhttpserver;

var
  HttpClient: TLHTTPClient;
  OutputFile: file;
  Done: boolean;

type
  THTTPHandler = class
  public
    procedure ClientDisconnect(ASocket: TLSocket);
    procedure ClientDoneInput(ASocket: TLSocket);
    function ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; 
      ASize: dword): dword;
  end;

procedure THTTPHandler.ClientDisconnect(ASocket: TLSocket);
begin
  writeln('disconnected.');
  done := true;
end;
  
procedure THTTPHandler.ClientDoneInput(ASocket: TLSocket);
begin
  writeln('...done');
  close(OutputFile);
  ASocket.Disconnect;
end;

function THTTPHandler.ClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: dword): dword;
begin
  blockwrite(outputfile, ABuffer^, ASize, Result);
  write('...' + IntToStr(ASize));
end;

var
  URL, Host, URI, FileName, AltFileName: string;
  index, Port: integer;
  dummy: THTTPHandler;
begin
  if ParamCount = 0 then
  begin
    writeln('Specify URL (and optionally, filename).');
    exit;
  end;

  { parse URL }
  URL := ParamStr(1);
  if not (Copy(URL, 1, 7) = 'http://') then
  begin
    writeln('URL should start with http://.');
    exit;
  end;

  index := PosEx('/', URL, 8);
  if index = 0 then
  begin
    writeln('URL should have server/location');
    exit;
  end;

  Host := Copy(URL, 8, index-8);
  URI := Copy(URL, index, Length(URL)+1-index);
  index := Pos(':', Host);
  if index > 0 then
  begin
    Port := StrToIntDef(Copy(Host, index+1, Length(Host)-index), -1);
    if (Port < 0) or (Port > 65535) then
    begin
      writeln('Port number out of range.');
      exit;
    end;
    SetLength(Host, index-1);
  end else
    Port := 80;

  if ParamCount >= 2 then
    FileName := ParamStr(2)
  else begin
    index := RPos('/', URI);
    if index > 0 then
      FileName := Copy(URI, index+1, Length(URI)-index);
    if Length(FileName) = 0 then
      FileName := 'index.html';
  end;

  if FileExists(FileName) then
  begin
    index := 1;
    repeat
      AltFileName := FileName + '.' + IntToStr(index);
      inc(index);
    until not FileExists(AltFileName);
    writeln('"', FileName, '" exists, writing to "', AltFileName, '"');
    FileName := AltFileName;
  end;

  assign(OutputFile, FileName);
  rewrite(OutputFile, 1);

  HttpClient := TLHTTPClient.Create(nil);
  HttpClient.Host := Host;
  HttpClient.Method := hmGet;
  HttpClient.Port := Port;
  HttpClient.URI := URI;
  HttpClient.Timeout := 30000;
  HttpClient.OnDisconnect := @dummy.ClientDisconnect;
  HttpClient.OnDoneInput := @dummy.ClientDoneInput;
  HttpClient.OnInput := @dummy.ClientInput;
  HttpClient.SendRequest;
  Done := false;
  while not Done do
    HttpClient.CallAction;
  HttpClient.Free;
end.
