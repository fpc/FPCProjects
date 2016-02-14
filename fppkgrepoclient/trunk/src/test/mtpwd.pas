program mtpwd;

uses
 db, sqldb, md5, wst_core, wst_synapse, pqconnection, mantis,
 mantis_proxy, synapse_http_protocol, base_soap_formatter;

Procedure CheckMantisSoap(Const AUser,APassword : String);

Var
  Mt : MantisConnectPortType;
  D : UserData;

begin
  SYNAPSE_RegisterHTTP_Transport();
  mt:=wst_CreateInstance_MantisConnectPortType();
  D:=mt.mc_login(AUser,APassword);
  if Assigned(D) then
    Writeln('Login match, welcome "',D.account_data.real_name,'"')
  else
    Writeln('No Login match')
end;

Procedure CheckMantisDB(Const AUser,APassword : String);

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
    Q.SQL.Text:='Select * from mantis_user_table where (username=:username)';
    Q.Params[0].AsString:=AUser;
    Q.Open;
    if Q.EOF and Q.BOF then
      Writeln('User ',Paramstr(1),' does not exist')
    else
      if Q.FieldByname('password').AsString=MD5Print(MD5String(APassword)) then
        Writeln('password match for user '+AUser)
      else
        Writeln('NO password match for user '+AUser);
    Q.Close;
  finally
    Conn.Free;
  end;
end;

begin
  CheckMantisSoap(ParamStr(1),ParamStr(2));
//  CheckMantisDB(ParamStr(1),ParamStr(2));
end.

