unit RepositoryTest;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  cnocStackBinaryClient;

type

  { TTestConnect }

  TTestConnect = class(TTestCase)
  published
    procedure TestRepoTest;
  end;

implementation

procedure TTestConnect.TestRepoTest;
var
  ClientConnection: TcnocStackBinaryClient;
  Data: TBytes;
  Response: PcnocStackMessage;
  s: String;
begin
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnection.Connect();

    Data := [];
    s := '{"name": "package", "method":"get","fpcversion":"3.1.1", "repository":"production"}';
    SetLength(Data, Length(s));
    move(s[1], data[0], length(s));

    writeln(GetTickCount64);
    check(ClientConnection.SendMessage(smtToStack, 'Repository', [sfRequestDirectAck, sfExpectDirectResponse], 0, Data, nil, nil, nil, Response)=ecNone, 'send message');
    writeln(GetTickCount64);
  finally
    ClientConnection.Free;
  end;
end;

initialization
  RegisterTest(TTestConnect);
end.

