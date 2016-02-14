unit authmantis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TUserData = Record
    ID : Int64;
    FullName : String;
    Email : string;
  end;

  { TMantisAuthentication }

  TMantisAuthentication = Class(TComponent)
  private
    FDBHostName: String;
    FDBPassWord: String;
    FDBUserName: String;
    FSoapURL: String;
    FUseSOAP: Boolean;
  Protected
    Function CheckMantisDB(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;
    Function CheckMantisSoap(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;
  Public
    Procedure ConfigMantis(AFileName : String);
    Function CheckMantis(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;
    Property UseSoap : Boolean Read FUseSOAP Write FUseSOAP;
    Property DBHostName : String Read FDBHostName Write FDBHostName;
    Property DBDatabaseName : String Read FDBHostName Write FDBHostName;
    Property DBUserName : String Read FDBUserName Write FDBUserName;
    Property DBPassword : String Read FDBPassWord Write FDBPassword;
    Property SoapURL : String Read FSoapURL Write FSOAPURL;
  end;

Function MantisAuthentication : TMantisAuthentication;

implementation

uses
 inifiles, db, sqldb, md5, wst_core, wst_synapse, pqconnection, mantis,
 mantis_proxy, synapse_http_protocol, base_soap_formatter;

Var
  Mauth : TMantisAuthentication;

Function MantisAuthentication : TMantisAuthentication;

begin
  if Mauth=Nil then
    Mauth:=TMantisAuthentication.Create(Nil);
  Result:=Mauth;
end;

Procedure TMantisAuthentication.ConfigMantis(AFileName : String);

Const
  SMantis           = 'Mantis';
  KeyUseSOAP        = 'UseSoap';
  KeySoapURL        = 'SoapURL';
  KeyDBDatabaseName = 'DB';
  KeyDBUserName     = 'DBUser';
  KeyDBPassword     = 'DBPassword';
  KeyDBHostName     = 'DBHost';

Var
  AINi : TMemIniFile;

begin
  AIni:=TMemIniFile.Create(AFileName);
  try
    UseSoap:=AIni.ReadBool(SMantis,KeyUseSoap,UseSoap);
    SoapURL:=AIni.ReadString(SMantis,KeySoapURL,SoapURl);
    DBHostName:=AIni.ReadString(SMantis,KeyDBHostName,DBHostName);
    DBDatabaseName:=AIni.ReadString(SMantis,KeyDBDatabaseName,DBDatabaseName);
    DBUserName:=AIni.ReadString(SMantis,KeyDBUserName,DBUserName);
    DBPassword:=AIni.ReadString(SMantis,KeyDBPassword,DBPassword);
  finally
    AIni.Free;
  end;
end;

Function TMantisAuthentication.CheckMantis(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;

begin
  if UseSoap then
    Result:=CheckMantisSoap(Auser,APassword,AUserData)
  else
    Result:=CheckMantisDB(Auser,APassword,AUserData);
end;

Function TMantisAuthentication.CheckMantisDB(Const AUser, APassword: String;
  Out AUserData: TUserData): Boolean;
begin

end;

Function TMantisAuthentication.CheckMantisSoap(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;

Var
  Mt : MantisConnectPortType;
  D : UserData;

begin
  SYNAPSE_RegisterHTTP_Transport();
  mt:=wst_CreateInstance_MantisConnectPortType('SOAP:','HTTP:',SOAPURL);
  D:=mt.mc_login(AUser,APassword);
  Result:=Assigned(D);
  if Result then
    if Assigned(D.account_data) then
      begin
      AUserData.ID:=D.account_data.id;
      AUserData.FullName:=D.account_data.real_name;
      AUserData.Email:=D.account_data.email;
      end
    else
    begin
    AUserData.ID:=-1;
    AUserData.FullName:='unknown';
    AUserData.Email:='unknown';
    end
end;

Function CheckMantisDB(Const AUser,APassword : String; Out AUserData : TUserData) : Boolean;

Var
  Conn : TPQConnection;
  Q : TSQLQuery;

begin
  Conn:=TPQConnection.Create(Nil);
  try
    Conn.Transaction:=TSQLTransaction.Create(Conn);
    Conn.HostName:='idefix.freepascal.org';
    Conn.UserName:='fpc';
    Conn.Password:='Shimrod';
    Conn.DatabaseName:='bugtracker';
    Conn.Connected:=True;
    Q:=TSQLQuery.Create(Conn);
    Q.Transaction:=COnn.Transaction;
    Q.SQL.Text:='Select id,realname,email from mantis_user_table where (username=:username) and (password=:password)';
    Q.ParamByName('username').AsString:=AUser;
    Q.ParamByName('password').AsString:=MD5Print(MD5String(APassword));
    Q.Open;
    Result:=Not(Q.EOF and Q.BOF);
    if Result then
      begin
      AUserData.ID:=Q.FieldByname('id').AsInteger;
      AUserData.FullName:=Q.FieldByname('realname').AsString;
      AUserData.Email:=Q.FieldByname('email').AsString;
      end;
    Q.Close;
  finally
    Conn.Free;
  end;
end;

finalization
  FreeAndNil(Mauth)
end.

