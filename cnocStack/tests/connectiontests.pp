unit ConnectionTests;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  syncobjs,
  ssockets,
  cnocStackMessageTypes,
  cnocStackErrorCodes,
  cnocStackbinaryclient;

type

  { TTestConnect }

  TTestConnect = class(TTestCase)
  published
    procedure TestLowLevelSendMessageWithAck;
    procedure TestLowLevelSendMessageCloseBeforeAck;
    procedure TestSendMessage;
    procedure TestSendMessageNoDirectAck;
    procedure TestSendMessageWithInvalidMessageType;
    procedure TestSendMessageToUnexistingStack;
    procedure TestSendMessageMultipleClients;
    procedure TestSendMessageWaitForResponse;
    procedure TestExtAccessKey;
  end;

implementation

procedure TTestConnect.TestSendMessage;
var
  ClientConnection: TcnocStackBinaryClient;
  Data: TBytes;
  Msg: PcnocStackMessage;
  i: Integer;
begin
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnection.Connect();

    Check(ClientConnection.SubscribeToStack('TestStack')=ecnone, 'subscribe to stack');

    Data := [];
    SetLength(Data, 5);
    Data[0] := Ord('h');
    Data[1] := Ord('a');
    Data[2] := Ord('l');
    Data[3] := Ord('l');
    Data[4] := Ord('o');

    for i := 0 to 3 do
      begin
      check(ClientConnection.SendMessage(smtToStack, 'TestStack', [sfRequestDirectAck], 0, Data)=ecNone, 'send message');

      Check(ClientConnection.WaitForMessage(Msg)=wrSignaled, 'received message');

      Data := Msg^.GetContents;
      CheckEquals(5, Length(Data), 'length of the returned data');
      CheckEquals(ord('h'), Data[0], 'contents, byte 1' );
      CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
      CheckEquals(ord('l'), Data[2], 'contents, byte 3' );
      CheckEquals(ord('l'), Data[3], 'contents, byte 4' );
      CheckEquals(ord('o'), Data[4], 'contents, byte 5' );
      end;
  finally
    ClientConnection.Free;
  end;
end;

procedure TTestConnect.TestSendMessageWithInvalidMessageType;
var
  ClientConnection: TcnocStackBinaryClient;
  Data: TBytes;
begin
  Data := [];
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnection.Connect();
    check(ClientConnection.SendMessage(smtNack, 'Stack1', [sfRequestDirectAck], 0, Data)=ecInvalidMessageType, 'Server detected invalid message type');
  finally
    ClientConnection.Free;
  end;
end;

procedure TTestConnect.TestSendMessageNoDirectAck;
var
  ClientConnection: TcnocStackBinaryClient;
  Data: TBytes;
  Msg: PcnocStackMessage;
begin
  Data := [];
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    SetLength(Data, 5);
    Data[0] := Ord('h');
    Data[1] := Ord('a');
    Data[2] := Ord('l');
    Data[3] := Ord('l');
    Data[4] := Ord('o');
    ClientConnection.Connect();

    ClientConnection.SubscribeToStack('TestStack');

    check(ClientConnection.SendMessage(smtToStack, 'TestStack', [], 0, Data)=ecNone, 'send message');

    Check(ClientConnection.WaitForMessage(Msg)=wrSignaled, 'received message');

    Data := Msg^.GetContents;
    CheckEquals(5, Length(Data), 'length of the returned data');
    CheckEquals(ord('h'), Data[0], 'contents, byte 1' );
    CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
    CheckEquals(ord('l'), Data[2], 'contents, byte 3' );
    CheckEquals(ord('l'), Data[3], 'contents, byte 4' );
    CheckEquals(ord('o'), Data[4], 'contents, byte 5' );
  finally
    ClientConnection.Free;
  end;
end;

procedure TTestConnect.TestLowLevelSendMessageWithAck;
var
  Socket: TInetSocket;
  Data: TBytes;
  Message: PcnocStackMessage;
  i: Integer;
  len: LongInt;
begin
  Socket := TInetSocket.Create('localhost', 5425, 1000);
  try
    Socket.Connect;

    Message := TcnocStackMessage.CreateMessage(smtSubscribeToStack, 'TestStack', [], 0, []);
    Socket.Write(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
    TcnocStackMessage.DestroyMessage(Message);

    sleep(1); // Why?!?
    for i := 0 to 3 do
      begin
      Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));
      Message := TcnocStackMessage.CreateMessage(smtToStack, 'TestStack', [sfRequestDirectAck], 0, Data);
      Socket.Write(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
      TcnocStackMessage.DestroyMessage(Message);

      Message := TcnocStackMessage.AllocateMessage(0);
      len := Socket.Read(Message^, SizeOf(TcnocStackMessageHeader));
      CheckEquals(QWord(smtAck), QWord(Message^.Header.MessageType), 'messagetype of ACK');
      CheckEquals(0, Message^.Header.ContentLength, 'contentlength of ACK');
      TcnocStackMessage.DestroyMessage(Message);

      Message := TcnocStackMessage.AllocateMessage(14);
      len := Socket.Read(Message^, SizeOf(TcnocStackMessageHeader));
      CheckEquals(QWord(smtFromStack), QWord(Message^.Header.MessageType), 'messagetype');
      CheckEquals(14, Message^.Header.ContentLength, 'contentlength of message');
      Socket.Read(Message^.Contents[0], 14);

      Data := Message^.GetContents;
      CheckEquals(5, Length(Data), 'length of the returned data');
      CheckEquals(ord('h'), Data[0], 'contents, byte 1' );
      CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
      CheckEquals(ord('l'), Data[2], 'contents, byte 3' );
      CheckEquals(ord('l'), Data[3], 'contents, byte 4' );
      CheckEquals(ord('o'), Data[4], 'contents, byte 5' );

      TcnocStackMessage.DestroyMessage(Message);

      end;
  finally
    Socket.Free;
  end;
end;

Procedure RespondThread(AData : Pointer);
var
  ClientConnectionReceiver: TcnocStackBinaryClient;
  Message: PcnocStackMessage;
  Response: TBytes;
begin
  ClientConnectionReceiver := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnectionReceiver.Connect();
    ClientConnectionReceiver.SubscribeToStack('TestStack');
    if ClientConnectionReceiver.WaitForMessage(Message, 1000) = wrSignaled then
      begin
      ClientConnectionReceiver.SendResponseMessage(Message, [], TBytes.Create(ord('d'), ord('a'), ord('g')));
      end;
  finally
    ClientConnectionReceiver.Free;
  end;
end;

procedure TTestConnect.TestSendMessageWaitForResponse;

var
  ClientConnectionSender: TcnocStackBinaryClient;
  Data: TBytes;
  ResponseMessage: PcnocStackMessage;
begin
  ClientConnectionSender := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    TThread.ExecuteInThread(@RespondThread);

    ClientConnectionSender.Connect();

    Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));

    check(ClientConnectionSender.SendMessage(smtToStack, 'TestStack', [sfRequestDirectAck, sfExpectDirectResponse], 0, Data, nil, nil, nil, ResponseMessage)=ecNone, 'failed to send a message and wait for the result');

    // Make sure thread is finished...
    sleep(100);

    Check(Assigned(ResponseMessage), 'Response was empty');
    Data := ResponseMessage^.GetContents;
    CheckEquals(3, Length(Data), 'length of the returned data');
    CheckEquals(ord('d'), Data[0], 'contents, byte 1' );
    CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
    CheckEquals(ord('g'), Data[2], 'contents, byte 3' );
  finally
    ClientConnectionSender.Free;
  end;
end;

procedure TTestConnect.TestSendMessageMultipleClients;
var
  ClientConnectionSender, ClientConnectionReceiver: TcnocStackBinaryClient;
  Data: TBytes;
  Msg: PcnocStackMessage;
begin
  ClientConnectionSender := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnectionSender.Connect();
    ClientConnectionReceiver := TcnocStackBinaryClient.Create('localhost', 5425);
    try
      ClientConnectionReceiver.Connect();

      Check(ClientConnectionReceiver.SubscribeToStack('TestStack')=ecnone, 'subscribe to stack');

      Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));

      check(ClientConnectionSender.SendMessage(smtToStack, 'TestStack', [sfRequestDirectAck], 0, Data, nil, nil, nil)=ecNone, 'send message');

      Check(ClientConnectionReceiver.WaitForMessage(Msg)=wrSignaled, 'received message');

      Data := Msg^.GetContents;
      CheckEquals(5, Length(Data), 'length of the returned data');
      CheckEquals(ord('h'), Data[0], 'contents, byte 1' );
      CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
      CheckEquals(ord('l'), Data[2], 'contents, byte 3' );
      CheckEquals(ord('l'), Data[3], 'contents, byte 4' );
      CheckEquals(ord('o'), Data[4], 'contents, byte 5' );
    finally
      ClientConnectionReceiver.Free;
    end;
  finally
    ClientConnectionSender.Free;
  end;
end;

procedure TTestConnect.TestSendMessageToUnexistingStack;
var
  ClientConnection: TcnocStackBinaryClient;
begin
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnection.Connect();
    check(ClientConnection.SendMessage(smtToStack, 'Stack1', [sfRequestDirectAck], 0, [])=ecStackDoesNotExist, 'Send message with ACK');
    check(ClientConnection.SendMessage(smtToStack, 'Stack1', [], 0, [])=ecNone, 'Send message without ACK');
    check(ClientConnection.SubscribeToStack('Stack1')=ecStackDoesNotExist, 'Send subscribe message');
  finally
    ClientConnection.Free;
  end;
end;

procedure TTestConnect.TestLowLevelSendMessageCloseBeforeAck;
var
  SendSocket: TInetSocket;
  ReceiveSocket: TInetSocket;
  Data: TBytes;
  Message: PcnocStackMessage;
begin
  ReceiveSocket := TInetSocket.Create('localhost', 5425, 1000);
  try
    ReceiveSocket.Connect;

    Message := TcnocStackMessage.CreateMessage(smtSubscribeToStack, 'TestStack', [], 0, []);
    ReceiveSocket.Write(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
    TcnocStackMessage.DestroyMessage(Message);

    SendSocket := TInetSocket.Create('localhost', 5425, 1000);
    try
      Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));
      Message := TcnocStackMessage.CreateMessage(smtToStack, 'TestStack', [sfRequestDirectAck], 0, Data);
      SendSocket.Write(Message^, SizeOf(TcnocStackMessageHeader) + Message^.Header.ContentLength);
      TcnocStackMessage.DestroyMessage(Message);
    finally
      SendSocket.Free;
    end;

    // Even while the ACK has not been received, the message should be posted
    // and thus received by the receiver.

    Message := TcnocStackMessage.AllocateMessage(14);
    ReceiveSocket.Read(Message^, SizeOf(TcnocStackMessageHeader));
    CheckEquals(QWord(smtFromStack), QWord(Message^.Header.MessageType), 'messagetype');
    CheckEquals(14, Message^.Header.ContentLength, 'contentlength of message');
    ReceiveSocket.Read(Message^.Contents[0], 14);

    Data := Message^.GetContents;
    CheckEquals(5, Length(Data), 'length of the returned data');
    CheckEquals(ord('h'), Data[0], 'contents, byte 1' );
    CheckEquals(ord('a'), Data[1], 'contents, byte 2' );
    CheckEquals(ord('l'), Data[2], 'contents, byte 3' );
    CheckEquals(ord('l'), Data[3], 'contents, byte 4' );
    CheckEquals(ord('o'), Data[4], 'contents, byte 5' );

    TcnocStackMessage.DestroyMessage(Message);
  finally
    ReceiveSocket.Free;
  end;
end;

procedure TTestConnect.TestExtAccessKey;
var
  ClientConnection: TcnocStackBinaryClient;
  Data: TBytes;
  ResponseMessage: PcnocStackMessage;
const
  ExtAccessKeys: TStringArray = ('Key1', 'TokenKey2');
begin
  ClientConnection := TcnocStackBinaryClient.Create('localhost', 5425);
  try
    ClientConnection.Connect();

    Data := TBytes.Create(Ord('h'),Ord('a'),Ord('l'),Ord('l'),Ord('o'));

    check(ClientConnection.SendMessage(smtToStack, 'TestStack', [], 0, Data, ExtAccessKeys, nil, nil, ResponseMessage)=ecNone, 'failed to send a message and wait for the result');

    ClientConnection.SubscribeToStack('TestStack');
    Check(ClientConnection.WaitForMessage(ResponseMessage)=wrSignaled, 'received message');
    try
      Check(Assigned(ResponseMessage), 'Response was empty');
      CheckEquals(2, ResponseMessage^.Header.ExtAccessKeyCount, 'length of the accesskeys data');
      CheckEquals(ExtAccessKeys[0], ResponseMessage^.GetExtAccessKey(0), 'length of the accesskeys data');
      CheckEquals(ExtAccessKeys[1], ResponseMessage^.GetExtAccessKey(1), 'length of the accesskeys data');
    finally
      TcnocStackMessage.DestroyMessage(ResponseMessage);
    end;
  finally
    ClientConnection.Free;
  end;
end;

initialization
  RegisterTest(TTestConnect);
end.

