unit DCSClientThread;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  fpjson,
  ssockets,
  BaseUnix,
  strutils,
  cnocQueue;

type
  TDCSOnReceiveDataProc = procedure(Data: TJSONData) of object;
  TDCSOnLostConnectionProc = procedure(ErrMessage: string) of object;
  TDCSThreadedQueueJSONData = specialize TcnocThreadedQueue<TJSONData>;

  { TDCSClientThread }

  TDCSClientThread = class(TThread)
  private
    FOnLostConnection: TDCSOnLostConnectionProc;
    FOnReceiveData: TDCSOnReceiveDataProc;
    FPort: integer;
    FHostName: string;
    FConnectionIdentifier: integer;
    FSendQueue: TDCSThreadedQueueJSONData;
    FErrMessage: string;
  protected
    procedure ReceivedData(Data: PtrInt);
    procedure LostConnection(Data: PtrInt);
    procedure Execute; override;
  public
    constructor Create(AHostName: string; APort: integer; AnOnReceiveData: TDCSOnReceiveDataProc;
      AnOnLostConnectionProc: TDCSOnLostConnectionProc);
    procedure SendData(Data: TJSONData);
    destructor Destroy; override;
    property ConnectionIdentifier: integer read FConnectionIdentifier;
    property OnReceiveData: TDCSOnReceiveDataProc read FOnReceiveData;
    property OnLostConnection: TDCSOnLostConnectionProc read FOnLostConnection;
  end;

implementation

{ TDCSClientThread }

procedure TDCSClientThread.ReceivedData(Data: PtrInt);
begin
  if assigned(OnReceiveData) then
    OnReceiveData(TJSONData(Data))
  else
    TObject(Data).Free;
end;

procedure TDCSClientThread.LostConnection(Data: PtrInt);
begin
  if assigned(FOnLostConnection) then
    OnLostConnection(FErrMessage);
end;

procedure TDCSClientThread.Execute;
const
  InputBufferSize = 4096;
var
  SendStr: string;
  s: string;
  i,j: integer;
  InputStr: string;
  JSonData: TJSONData;
  ASocket: TInetSocket;
  CanHaveMoreData: Boolean;
  Startpos: Integer;

  function ScanForEndStr(StartPos, MaxLen: Integer) : Integer;
  var
    k: integer;
  begin
    Result := 0;
    for k := StartPos+1 to StartPos+MaxLen do
      if InputStr[k]=#10 then
        begin
        Result := k;
        break;
        end;
  end;

  function ReadString(out DataWaiting: Boolean): string;
  var
    s: string;
  begin
    // First see if there is a string left in the input-buffer.
    if CanHaveMoreData then
      begin
      i := pos(#10, InputStr);
      if i > 0 then
        begin
        s := copy(InputStr, 1, i-1);
        delete(InputStr,1,i);
        Startpos := Length(InputStr);
        DataWaiting := Startpos>0;
        CanHaveMoreData := DataWaiting;
        result := s;
        exit;
        end;
      end;

    DataWaiting := False;
    CanHaveMoreData := False;
    result := '';
    SetLength(InputStr, Startpos + InputBufferSize);
    i := ASocket.Read(InputStr[1+Startpos], InputBufferSize);
    if i=0 then
      begin
      // Connection closed
      FErrMessage := 'Connection with server closed.';
      Terminate;
      end
    else if i<0 then
      begin
      if ASocket.LastError<>ESysEAGAIN then
        begin
        FErrMessage := 'Error during write to server. Socket-error: '+inttostr(ASocket.LastError);
        Terminate;
        end
      end
    else if i > 0 then
      begin
      SetLength(InputStr,Startpos+i);
      j := ScanForEndStr(Startpos, i);
      if j > 0 then
        begin
        s := copy(InputStr, 1, j-1);
        delete(InputStr,1,j);
        Startpos := Length(InputStr);
        DataWaiting := Startpos>0;
        CanHaveMoreData := DataWaiting;
        result := s;
        end
      else
        begin
        Startpos := Startpos+i;
        DataWaiting := True;
        end;
      end;
  end;

  function ReadStringTimeout(ATimeout: integer): string;
  var
    tc: int64;
    DataWaiting: Boolean;
  begin
    tc := GetTickCount64;
    result := ReadString(DataWaiting);
    while not terminated and (result='') and ((GetTickCount64-tc)<ATimeout) do
      begin
      if not DataWaiting then
        ThreadSwitch();
      result := ReadString(DataWaiting);
      end;
  end;

var
  IsConnected: boolean;
  PopResult: TWaitResult;
  DataWaiting: boolean;
begin
  IsConnected:=false;
  CanHaveMoreData:=false;
  InputStr:='';
  Startpos := 0;
  FErrMessage:='';
  try
    ASocket := TInetSocket.Create(FHostName, FPort);
    try
      if not assigned(ASocket) then
        begin
        FErrMessage:='Failed to connect to server at '+FHostName+':'+IntToStr(FPort);
        Terminate;
        end
      else
        begin
        // Set non-blocking
        fpfcntl(ASocket.Handle,F_SETFL,O_NONBLOCK);

        // Read and check FPDebug Server greeting
        s := ReadStringTimeout(100);
        if s='Welcome to FPDebug-server.' then
          begin
          // Read connection-identifier
          s := ReadStringTimeout(100);
          delete(s,length(s),1);
          s := copy(s, rpos(' ',s)+1, 5);
          FConnectionIdentifier:=StrToIntDef(s,-1);
          if FConnectionIdentifier>-1 then
            begin
            // Skip help-message
            s := ReadStringTimeout(100);
            IsConnected:=True;
            end;
          end;

        if not IsConnected then
          begin
          FErrMessage:='Connected to '+FHostName+':'+inttostr(FPort)+', but failed to negotiate handshake.';
          Terminate;
          end;
        end;

      while not terminated do
        begin
        s:=ReadString(DataWaiting);
        if s<>'' then
          begin
          JSonData := GetJSON(s);
          if JSonData is TJSONObject then
            ReceivedData(PtrInt(JSonData))
          else
            raise exception.CreateFmt('JSon-command %s is not a JSON-Object.',[s]);
          end;

        if not terminated then
          begin
          // If there is more data wating in the tcpip-inputbuffer, do not wait
          // for the timeout. This way the input-buffer will be processed as
          // soon as possible.
          if DataWaiting then
            PopResult := FSendQueue.PopItemTimeout(JSonData, 0)
          else
            PopResult := FSendQueue.PopItem(JSonData);

          if PopResult = wrSignaled then
            begin
            SendStr := JSonData.AsJSON + #10;
            i := ASocket.Write(SendStr[1], length(SendStr));

            if i < 0 then
              begin
              if ASocket.LastError=32 then
                begin
                // Lost connection
                end
              else
                FErrMessage := Format('Error during write. Socket-error: %d',[ASocket.LastError]);
              Terminate;
              end
            else if i < length(SendStr) then
              raise exception.create('Message has not been send to client entirely');
            end;
          end;
        end;
    finally
      ASocket.Free;
    end;
  except
    on E: Exception do
      begin
      FErrMessage:='Exception on connection with server: ' + E.Message;
      end;
  end;

  LostConnection(0);
end;

constructor TDCSClientThread.Create(AHostName: string; APort: integer;
  AnOnReceiveData: TDCSOnReceiveDataProc; AnOnLostConnectionProc: TDCSOnLostConnectionProc);
begin
  FHostName:=AHostName;
  FPort:=APort;
  FOnReceiveData:=AnOnReceiveData;
  FOnLostConnection:=AnOnLostConnectionProc;
  FSendQueue:=TDCSThreadedQueueJSONData.create(100, INFINITE, 100);
  inherited create(false);
end;

procedure TDCSClientThread.SendData(Data: TJSONData);
begin
  if Assigned(Data) then
    FSendQueue.PushItem(Data);
end;

destructor TDCSClientThread.Destroy;
begin
  FSendQueue.Free;
  inherited Destroy;
end;

end.

