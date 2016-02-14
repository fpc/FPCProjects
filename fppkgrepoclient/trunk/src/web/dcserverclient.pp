unit dcserverclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, dcsclientthread;


Const
  DefCommandTimeout = 60*1000; // 1 minute, in milliseconds
  DefServerPort     = 9250;    // FPPKg repo server

type
  // Data is freed after handler returns.
  TServerNotificationEvent = Procedure(Sender : TObject; Const NotificationType : String; Data : TJSONObject) of object;
  // Data is freed after handler returns.
  TLineNoiseEvent = Procedure(Sender : TObject; Const Data : TJSONObject) of object;

  TClientState = (csidle, csWaiting, csCommandQueued, csCommandCompleted, csCommandError, csLostConnection);
  TClientStates = Set of TClientState;

  TLoglevel  = (llDebug,llWarning,llInfo);
  TLogLine = Record
    Level : TLogLevel;
    Message : String;
  end;
  TLogLines = Array of TLogLine;

  { ECommand }

  { EDCClient }

  EDCClient = Class(Exception)
  private
    FConnID: INteger;
  Public
    Property ConnectionID : INteger Read FConnID Write FConnID;
  end;
  ECommand = Class(EDCClient)
  private
    FLisID: Integer;
    FUniqueID: Integer;
  Public
    Property UniqueID : Integer Read FUniqueID Write FUniqueID;
    Property LisID : Integer Read FLisID Write FLisID;
  end;

  { TDCServerClient }

  TDCServerClient = class(TComponent)
  private
    FASync: Boolean;
    FCommandID : Int64;
    FCommandTimeOut: Integer;
    FHostname: String;
    FOnNoise: TLineNoiseEvent;
    FOnServerNotification: TServerNotificationEvent;
    FPort: Word;
    FTerminated: Boolean;
    FClientThread: TDCSClientThread;
    FMessage: string;
    FState : TClientState;
    FResultData : TJSONObject;
    FCOmmandLog : TJSONArray;
    procedure HandleNoise(Obj: TJSONObject);
  protected
    Procedure SetResultData(Data : TJSONObject);
    Function CreateClientException(Const Fmt: String; Const Args: Array of const): EDCClient;
    Function CreateCommandException(AlisID,AUniqueid : integer;Const Fmt: String; Const Args: Array of const): ECommand;
    Function CreateCommandException(AData : TJSONObject) : ECommand;
    Function DoRun(Const ACommand : String) : TJSONObject;
    Function DoRun(Const ACommand : TJSONObject) : TJSONObject;
    procedure HandleLostConnection(ErrMessage: string);
    procedure HandleReceiveData(Data: TJSONData);
    Procedure SetCommandLog(A : TJSONArray);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The TJSONObject commands will be freed by the component.
    Function ExecuteCommand(Const ACommand : String) : TJSONObject;
    Function ExecuteCommand(Const ACommand : TJSONObject) : TJSONObject;
    Function ExecuteCommandAsync(Const ACommand : String) : Int64;
    Function ExecuteCommandAsync(Const ACommand : TJSONObject) : Int64;
    Function GetCommandLog : TLogLines;
    // Get the raw command log as an TJSONArray
    Procedure GetRawCommandLog(Var LogLines : TJSONArray);
    Procedure Terminate;
    Property Terminated : Boolean Read FTerminated;
    Property OnServerNotification : TServerNotificationEvent Read FOnServerNotification Write FOnServerNotification;
    Property OnNoise : TLineNoiseEvent Read FOnNoise Write FOnNoise;
    Property HostName : String Read FHostname Write FHostname;
    Property Port : Word Read FPort Write FPort;
    Property CommandTimeOut : Integer Read FCommandTimeOut Write FCommandTimeOut;
  end;

implementation

uses typinfo, dateutils;

{ TDCServerClientExample }

Procedure TDCServerClient.SetResultData(Data: TJSONObject);
begin
  FreeAndNil(FResultData);
  FResultData:=Data;
end;

Function TDCServerClient.CreateClientException(Const Fmt: String; Const Args : Array of const) : EDCClient;

begin
  Result:=EDCClient.CreateFmt(Fmt,Args);
  if Assigned(FClientThread) then
    Result.ConnectionID:=FClientThread.ConnectionIdentifier;
end;

Function TDCServerClient.CreateCommandException(AlisID, AUniqueid: integer;
  Const Fmt: String; Const Args: Array of const): ECommand;
begin
  Result:=ECommand.CreateFmt(Fmt,Args);
  if Assigned(FClientThread) then
    Result.ConnectionID:=FClientThread.ConnectionIdentifier;
  Result.UniqueID:=AUniqueID;
  Result.LisID:=AlisID;
end;

Function TDCServerClient.CreateCommandException(AData: TJSONObject): ECommand;
begin
  With AData do
  Result:=CreateCommandException(Get('lisId',-1),
                                Get('UniqueId',-1),
                                AData.Get('message','No error message available.'),
                                []);
end;

Function TDCServerClient.DoRun(Const ACommand: String) : TJSONObject;

Var
  CmdData : TJSONData;

begin
  CmdData := GetJSON(ACommand);
  if Not (CmdData is TJSONObject) then
     begin
     CmdData.Free;
     Raise EDCClient.CreateFmt('Invalid command: %s',[ACommand]);
     end;
  Result:=DoRun(TJSONObject(CmdData))
end;

Function TDCServerClient.DoRun(Const ACommand: TJSONObject) : TJSONObject;

var
  C : TClientState;
  WaitStates : TClientStates;
  EC : ECommand;
  N : TDateTime;
  TimeOut : Boolean;

  Function IsTimeout : Boolean;

  begin
    TimeOut:=(FCommandTimeOut<>0) and (MillisecondsBetween(Now,N)>FCommandTimeOut);
    Result:=TimeOut;
  end;

begin
  Result:=Nil;
  FCommandID:=-1;
   try
    FClientThread := TDCSClientThread.Create(HostName,Port, @HandleReceiveData, @HandleLostConnection);
    FState:=csWaiting;
    WaitStates:=[csWaiting];
    if Not FAsync then
      Include(WaitStates,csCommandQueued);
    FClientThread.SendData(ACommand);
    N:=Now;
    while (FState in WaitStates) and Not (Terminated or IsTimeOut) do
      CheckSynchronize(10);
    if TimeOut then
      Raise CreateClientException('Timeout waiting for command result: %d ms. passed',[FCommandTimeout]);
  finally
    //CmdData.Free;
    C:=FState;
    FState:=csIdle;
    // Writeln('Terminating loop (state : ',C,')');
    FClientThread.Terminate;
    FClientThread.WaitFor;
    FClientThread.Free;
  end;
  Case C of
   csCommandError :
      begin
      Raise CreateCommandException(FResultData);
      end;
   csCommandCompleted :
     begin
     Result:=FResultData;
     FResultData:=Nil;
     end;
   csCommandQueued :
     begin
     Result:=FResultData;
     FResultData:=Nil;
     end;
   csLostConnection :
     if Assigned(FResultData) then
       Raise CreateClientException('Connection lost on id %d: %s',[FClientThread.ConnectionIdentifier,FResultData.AsJSON])
     else
       Raise CreateClientException('Connection lost on id %d.',[FClientThread.ConnectionIdentifier]);

  end;
end;

procedure TDCServerClient.HandleLostConnection(ErrMessage: string);
begin
  // If the error message is empty, it was a normal end of loop
  if (ErrMessage<>'') then
    begin
    // Writeln('Handle lost connection.');
    FState:=csLostConnection;
    FMessage:=ErrMessage;
    end;
end;

procedure TDCServerClient.HandleNoise(Obj: TJSONObject);

begin
  // Writeln('Handle noise '+Obj.AsJSON);
  if Assigned(OnNoise) then
    OnNoise(Self,Obj);
  Obj.Free;
end;

procedure TDCServerClient.HandleReceiveData(Data: TJSONData);

  Procedure ExtractCommandLog(Obj : TJSONObject);

  begin
    if (Obj.IndexOfName('LogLineList')<>-1) and
       (Obj.Types['LogLineList']=jtArray) then
     SetCommandLog(Obj.Extract('LogLineList') as TJSONArray);
  end;

  Function IsMyNotification(Obj : TJSONObject) : Boolean;

  begin
    Result:=(Obj.Get('type','')='Notification')
            and (Obj.Get('lisId',-1)=FClientThread.ConnectionIdentifier)
  end;

var
  Obj : TJSONObject;
  NotificationType: string;
  handled : Boolean;

begin
  if Data.JSONType<>jtObject then
    FreeAndNil(Data);
  Obj:=TJSONObject(Data);
  if Not IsMyNotification(obj) then
    HandleNoise(Obj) // Will free data
  else
    begin
    // Writeln('Handle received data');
    Handled:=True;
    NotificationType :=Obj.Get('notificationType','');
    if not (FState in [csCommandError,csCommandQueued]) then
       begin
       if NotificationType='InvalidCommand' then
        begin
        FState:=csCommandError;
        SetResultData(Obj);
          begin
          // Writeln('Ext Terminating loop: invalid command');
          FClientThread.Terminate;
          end;
        end
      else if NotificationType='ReceivedCommand' then
        begin
        FState:=csCommandQueued;
        ExtractCommandLog(Obj);
        SetResultData(Obj);
        FCommandID:=FResultData.Get('UniqueId',Int64(-1));
        if FASync then
          begin
          // Writeln('Ext Terminating loop: async');
          FClientThread.Terminate;
          end;
        end
      else
        Handled:=False;
      end
    else if (NotificationType='ExecutedCommand') then
      begin
      ExtractCommandLog(Obj);
      SetResultData(Obj);
      if (FCommandID<>-1) and (FCommandID=FResultData.Get('UniqueId',Int64(-1))) then
        begin
        FState:=csCommandCompleted;
        // Writeln('Ext Terminating loop: executed command');
        FClientThread.Terminate;
        end;
      end
    else if (NotificationType='FailedCommand') then
      begin
      ExtractCommandLog(Obj);
      SetResultData(Obj);
      FState:=csCommandError;
      // Writeln('Ext Terminating loop: failed command');
      FClientThread.Terminate;
      end
    else
      Handled:=False;
    If Not handled and Assigned(OnServerNotification) then
       OnServerNotification(Self,NotificationType,FResultData);
    end;
end;

Procedure TDCServerClient.SetCommandLog(A: TJSONArray);
begin
  FreeAndNil(FCommandLog);
  FCommandLog:=A;
end;

constructor TDCServerClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandTimeout:=DefCommandTimeout;
  FPort:=DefServerPort;
end;

destructor TDCServerClient.Destroy;
begin
  FreeAndNil(FResultData);
  inherited Destroy;
end;

Function TDCServerClient.ExecuteCommand(Const ACommand: String): TJSONObject;

begin
  FAsync:=False;
  Result:=DoRun(ACommand);
end;

Function TDCServerClient.ExecuteCommand(Const ACommand: TJSONObject
  ): TJSONObject;
begin
  FAsync:=False;
  Result:=DoRun(ACommand);
end;

Function TDCServerClient.ExecuteCommandAsync(Const ACommand: String): Int64;

begin
  FAsync:=true;
  DoRun(ACommand).Free;
  Result:=FCommandID;
end;

Function TDCServerClient.ExecuteCommandAsync(Const ACommand: TJSONObject
  ): Int64;
begin
  FAsync:=true;
  DoRun(ACommand).Free;
  Result:=FCommandID;
end;

Function TDCServerClient.GetCommandLog: TLogLines;

  Function JSONToLogLine(O : TJSONObject) : TLogLine;

  begin
    Result.Message:=O.Strings['Msg'];
    Result.Level:=TLogLevel(GetEnumValue(TypeInfo(TLogLevel),O.Strings['LogLevel']));
  end;

Var
  A : TJSONArray;
  D : TJSONEnum;

begin
  SetLength(Result,0);
  GetRawCommandLog(A);
  if Not Assigned(A) then
    exit;
  try
    SetLength(Result,A.Count);
    For D in A do
      if D.Value is TJSONOBject then
        Result[D.KeyNum]:=JSONToLogLine(D.Value as TJSONObject);
  finally
    A.Free;
  end;
end;

Procedure TDCServerClient.GetRawCommandLog(Var LogLines: TJSONArray);
begin
  LogLines:=FCommandLog;
  FCommandLog:=Nil;
end;

Procedure TDCServerClient.Terminate;
begin
  FTerminated:=True;
end;

end.

