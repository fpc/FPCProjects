unit TestMessageFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  cnocStackMessageTypes;

type

  { TTestMessageFormat }

  TTestMessageFormat = class(TTestCase)
  published
    procedure TestExtAccessKey;
  end;

implementation

procedure TTestMessageFormat.TestExtAccessKey;
var
  Data: TBytes;
  Message: PcnocStackMessage;
const
  ExtAccessKeys: TStringArray = ('Key1', 'TokenKey2');
begin
  Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));

  Message := TcnocStackMessage.CreateMessage(smtToStack, 'TestStack', [sfRequestDirectAck, sfExpectDirectResponse], 0, Data, ExtAccessKeys, nil, nil);
  try
    CheckEquals(2, Message^.Header.ExtAccessKeyCount, 'length of the accesskeys data');
    CheckEquals(ExtAccessKeys[0], Message^.GetExtAccessKey(0), 'Content of first accesskeys');
    CheckEquals(ExtAccessKeys[1], Message^.GetExtAccessKey(1), 'Content of second accesskeys');
  finally
    TcnocStackMessage.DestroyMessage(Message);
  end;
end;

initialization
  RegisterTest(TTestMessageFormat);
end.

