unit wmfppkg;

{$mode objfpc}{$H+}

interface

uses
  Typinfo, SysUtils, Classes, httpdefs, fpHTTP, fpWeb, dcserverclient, fpjson;

type
  { TFPPKg }

  TFPPKg = class(TFPWebModule)
    Procedure DefaultRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    Procedure LoginRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    Procedure LogOutRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
    Procedure UploadRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; Var Handled: Boolean);
  private
    Function  CreateTemplatePage(Const APage,ATitle: String; Const Vars: Array of string): String;
    Procedure DoTemplateParams(Sender: TObject; Const ParamName: String; Out AValue: String);
    Function  FormatCommand(O: TJSONObject): String;
    Function  FormatLogLines(Log: TLogLines): String;
    Function GetLoginTag: String;
    Function  GetTemplateFileName(Const APage: String): String;
    Procedure LoadConfig(ARequest: TRequest);
    Function  AnalyzeZipFile(FN: String): String;
    Function  IsAuthorized(ARequest: TRequest): Boolean;
    Procedure SendError(AResponse: TResponse; ACode: Word; ACodeText: String);
    Procedure TestFile(AFileName,APAckageName: String; AResponse: TResponse);
    Function  ValidateUser(Const AUserName, APassword: String): Boolean;
    { private declarations }
  public
    { public declarations }
  end;

var
  FPPKg: TFPPKg;

implementation

uses
  {$ifdef unix}baseunix,{$endif} zipper, authmantis, strutils,
  base64, cfgpkgfront, fptemplate, custapp, fpcgi, webutil;


Const


  SUserNameParam = '_username_';
  SPasswordParam = '_password_';
  SUserNameField = 'username';
  SPasswordField = 'password';


{$R *.lfm}

{ TFPPKg }

Function TFPPKg.AnalyzeZipFile(FN : String) : String;

  Function AllowedExt (E : String) : Boolean;

  begin
    Result:=Pos(E,'.com.exe.zip')=0
  end;

Var
  Z : TUnZipper;
  I,fpmi : Integer;
  E : TFullZipFileEntry;
  Ext : String;

begin
  Result:='';
  fpmi:=-1;
  Z:=TUnZipper.Create;
  try
    Z.FileName:=FN;
    try
       Z.Examine;
    except
      on E : EZipError do
        Exit('Invalid zip file structure.');
    end;
    if Z.Entries.Count=0 then
      Exit('Empty zip file.');
    For I:=0 to Z.Entries.Count-1 do
      begin
      E:=Z.Entries[i];
      CustomApplication.Log(etInfo,Format('Examining : %d archive "%s" disk "%s"',[I,E.ArchiveFileName,E.DiskFileName]));
      Ext:=LowerCase(ExtractFileExt(E.ArchiveFileName));
      if Not AllowedExt(Ext) then
        Exit('This file type is not allowed: '+E.ArchiveFIleName);
{$ifdef unix}
      if E.OS=3 {OS_UNIX } then
        ; // if baseunix.FP E.Attributes;
{$endif}
      if 0=CompareText(E.ArchiveFileName,'fpmake.pp') then
        fpmi:=i;
      end;
    if fpmi=-1 then
      Exit('No fpmake.pp file found');
    E:=Z.entries[fpmi];
    if E.ArchiveFileName<>'fpmake.pp' then
      Exit('fpmake.pp is not the proper casing: '+E.ArchiveFileName);
  finally
    Z.Free;
  end;
end;

Function TFPPKg.GetTemplateFileName(Const APage : String) : String;

begin
  Result:=TSettings.Global.TemplateDir+APage+'.html';
end;

Function TFPPKg.CreateTemplatePage(Const APage,ATitle : String; Const Vars : Array of string) : String;

Var
  T :  TTemplateParser;
  Fin : TFileStream;
  Fout : TStringStream;
  I : integer;

begin
  Fin:=nil;
  FOut:=Nil;
  T:=TTemplateParser.Create;
  try
    T.AllowTagParams:=False;
    T.OnGetParam:=@DoTemplateParams;
    T.StartDelimiter:='{{';
    T.EndDelimiter:='}}';
    Fin:=TFileStream.Create(GetTemplateFileName(APage),fmOpenRead);
    Fout:=TStringStream.Create('');
    I:=0;
    While I<Length(Vars) do
      begin
      T.Values[Vars[i]]:=Vars[I+1];
      Inc(i,2);
      end;
    T.ParseStream(Fin,Fout);
    T.Clear;
    T.Values['Content']:=Fout.DataString;
    T.Values['Title']:=ATitle;
    FreeAndNil(Fin);
    FreeAndNil(Fout);
    Fout:=TStringStream.Create('');
    Fin:=TFileStream.Create(GetTemplateFileName('template'),fmOpenRead);
    T.ParseStream(Fin,Fout);
    Result:=Fout.Datastring;
  finally
    FreeAndNil(Fin);
    FreeAndNil(Fout);
    T.Free;
  end;
end;

Function TFPPKg.GetLoginTag : String;

begin
  if StrToIntDef(Session.Variables['UserID'],-1)=-1 then
    Result:='<a href="'+TSettings.Global.BaseURL+'/FPPKG/login">Log in</a>'
  else
    begin
    Result:=Session.Variables['UserFullName'];
    If Result='' then
      Result:=Session.Variables['UserName'];
    Result:=Result+'&nbsp;&nbsp;<a href="'+TSettings.Global.BaseURL+'/FPPKG/logout">Log out</a>'
    end;

end;

Procedure TFPPKg.DoTemplateParams(Sender: TObject; Const ParamName: String; Out
  AValue: String);

Var
  S : String;

begin
  if CompareText(ParamName,'LOGIN')=0 then
    AValue:=GetLoginTag
  else if CompareText(ParamName,'BaseURL')=0 then
    begin
    AValue:=TSettings.Global.BaseURL;
    if AValue='' then
      AValue:=Application.ApplicationURL;
    While (AValue<>'') and (AValue[length(AValue)]='/') do
      AValue:=Copy(AValue,1,Length(AValue)-1);
    end
  else
    begin
    S:=Request.ContentFields.Values[ParamName];
    if S='' then
      begin
      Request.QueryFields.Values[ParamName];
      if S='' then
        S:=Session.Variables[ParamName];
      end;
    AValue:=S;
    end;
end;

Procedure TFPPKg.SendError(AResponse : TResponse; ACode : Word; ACodeText : String);

begin
  AResponse.Code:=400;
  AResponse.CodeText:=ACodeText;
  AResponse.Content:=CreateTemplatePage('error','An error occurred',['ErrMessage',ACodeText,'ErrCode',IntToStr(ACode)]);
  AResponse.SendContent;
end;


Function TFPPKg.ValidateUser(Const AUserName,APassword : String) : Boolean ;

Var
  AUserData : TUserData;
  MU : String;

begin
  Result:=False;
  MU:=TSettings.Global.MasterUser;
  CustomApplication.Log(etInfo,Format('Authenticating : %s %s',[AUserName,APassword]));
  if (MU<>'') then
    Result:=(AUserName=MU) and (APassword=TSettings.Global.MasterPassword);
  if Result then
    begin
    AuserData.ID:=0;
    AuserData.FullName:='FPC master account';
    AUserData.Email:='webmaster@freepascal.org';
    end
  else
    begin
    CustomApplication.Log(etInfo,Format('Authenticating mantis : (soap %s)',[BoolToStr(MantisAuthentication.UseSoap,'true','false')]));
    Result:=MantisAuthentication.CheckMantis(AUserName,APassword,AUserData);
    end;
  if Result then
    begin
    Session.Variables['UserID']:=IntToStr(AUserData.ID);
    Session.Variables['UserFullName']:=AUserData.FullName;
    Session.Variables['UserEmail']:=AUserData.Email;
    Session.Variables['UserName']:=AUserName;
    end
  else
    begin
    Session.Variables['UserID']:='-1';
    Session.Variables['UserFullName']:='';
    Session.Variables['UserEmail']:='';
    Session.Variables['UserName']:='';
    end
end;

Procedure TFPPKg.DefaultRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);

Var
  TN : String;

begin
  Handled:=true;
  LoadConfig(ARequest);
  if IsAuthorized(ARequest) then
    TN:='upload'
  else
    TN:='uploadlogin';
  AResponse.Content:=CreateTemplatePage(TN,'Package upload',[]);
  AResponse.SendContent;
end;

Procedure TFPPKg.LoginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);

Var
  UN,PW : String;

begin
  LoadConfig(ARequest);
  UN:=ARequest.QueryFields.Values[SUserNameParam];
  if UN='' then
    UN:=ARequest.ContentFields.Values[SUserNameField];
  PW:=ARequest.QueryFields.Values[SPasswordParam];
  if (PW='') then
    PW:=ARequest.ContentFields.Values[SPasswordField];
  If (UN='') or (PW='') then
    begin
    if (UN<>'') then
      AResponse.Content:=CreateTemplatePage('login','Log in',['ErrMessage','Missing password','username',un])
    else
      AResponse.Content:=CreateTemplatePage('login','Log in',['ErrMessage','','username',un]);
    AResponse.SendContent;
    Handled:=True;
    end
  else if Not ValidateUser(UN,PW) then
    begin
    AResponse.Content:=CreateTemplatePage('login','Log in',['ErrMessage','Invalid username and/or password','username',un]);
    AResponse.SendContent;
    Handled:=True;
    end
  else
    begin
    AResponse.Code:=200;
    DefaultRequest(Sender,ARequest,AResponse,Handled);
    end;

end;

Procedure TFPPKg.LogOutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);
begin
  LoadConfig(ARequest);
  Session.Variables['UserID']:='-1';
  Session.Variables['UserFullName']:='';
  Session.Variables['UserEmail']:='';
  Session.Variables['UserName']:='';
  AResponse.Content:=CreateTemplatePage('login','Log in',['ErrMessage','','username','']);
  AResponse.SendContent;
  Handled:=True;
end;



Function TFPPKg.IsAuthorized(ARequest: TRequest) : Boolean;

{
  4 ways to authenticate, in decreasing precedence
  - Login call, user ID saved in sessction
  - HTTP basic authentication
  - HTTP GET parameters _username_, _password_
  - HTTP POST variables username, password
}

Var
  H,R,AUser,APassword : String;

begin
  // 1. Session ?
  Result:=StrToIntDef(Session.Variables['UserID'],-1)>=0;
  if Result then
    begin
    customApplication.Log(etInfo,Format('Detected session, user name "%s"',[Session.Variables['UserName']]));
    exit;
    end;
  // HTTP BASIC Authorization
  if (ARequest.Authorization<>'') then
    begin// Realm and value
    R:=ExtractWord(1,H,[' ',#8]);
    H:=ExtractWord(2,H,[' ',#8]);
    if (H<>'') then
      begin
      H:=Decodestringbase64(H);
      AUser:=ExtractWord(1,H,[':']);
      APassword:=ExtractWord(2,H,[':']);
      end;
    end
  else
    begin
    // Query fields
    AUser:=ARequest.QueryFields.Values[SUserNameParam];
    APassword:=ARequest.QueryFields.Values[SPasswordParam];
    if (AUser='') then
      begin
      // Use POST variables
      AUser:=ARequest.ContentFields.Values[SUserNameField];
      APassword:=ARequest.ContentFields.Values[SPasswordField];
      end;
    end;
  customApplication.Log(etInfo,Format('Detected user name "%s" and password "%s"',[AUser,APassword]));
  if (AUser<>'') then
    Result:=ValidateUser(Auser,APassword);
end;

Function TFPPKg.FormatLogLines(Log : TLogLines) : String;

  Function LLStr(Level : TlogLevel) : String;

  begin
    Result:=GetEnumName(TypeInfo(TLogLevel),Ord(Level));
    Delete(Result,1,2);
  end;

Var
  L : TStrings;
  LL : TLogLine;


begin
  Result:='';
  if Length(Log)=0 then
    exit('No log available.');
  L:=TStringList.Create;
  try
    L.Add('<table border="0" class="commandlog">');
    L.Add('<tr><th class="loglevel">Level</th><th class="logmessage">Message</th></tr>');
    for LL in Log do
      begin
      L.Add('<tr class="logline">');
      L.Add('<td class="loglevel">'+LLStr(LL.Level)+'</td>');
      // Correct
      L.Add('<td class="logmessage">'+LL.Message+'</td>');
      L.Add('</tr>');
      end;
    L.Add('</table>');
    Result:=L.Text;
  finally
    L.Free;
  end;
end;

Function TFPPKg.FormatCommand(O : TJSONObject) : String;

Var
  L : TStrings;
  D : TJSONEnum;
  V : String;


begin
  Result:='';
  if Not Assigned(O) then
    exit('No command available.');
  L:=TStringList.Create;
  try
    for d in O do
      begin
      if (D.Key<>'command') then
        begin
        L.Add('<tr><td class="cmdoptionname">'+D.key+'</td>');
        if D.Value.JSONType in [jtNull,jtObject,jtArray] then
          V:=D.Value.AsJSON
        else
          V:=D.Value.AsString;
        L.Add('<td class="cmdoptionname">'+V+'</td></tr>');
        end;
      end;
    if L.Count>0 then
      begin
      L.Add('</table>');
      L.insert(0,'<table border="0" class="commandoptions">');
      end;
    L.Insert(0,'With command options:');
    L.Insert(0,'Command: <div id="pkgcommand" class="pkgcommand">'+O.Get('command','unknown')+'</div><br>');
    Result:=L.Text;
  finally
    L.Free;
  end;
end;

Procedure TFPPKg.TestFile(AFileName, APAckageName: String; AResponse: TResponse
  );

Var
  C : TDCServerClient;
  URL,Cmd,ResStr,LogStr,DisplayCmd : String;
  CmdObj,Res : TJSONObject;
  Log : TLogLines;

begin
  Res:=Nil;
  C:=TDCServerClient.Create(Self);
  try
    C.CommandTimeOut:=TSettings.Global.CommandTimeOut;
    C.HostName:=TSettings.Global.FPPkgServerHost;
    C.Port:=TSettings.Global.FPPkgServerPort;
    URL:=TSettings.Global.PackageURL+ExtractFileName(AFileName);
    CmdObj:=TJSONObject.Create(['command','test',
//                                'PackageName',APackageName,
                                'packageurl',URL]);
    try
      DisplayCmd:=FormatCommand(CmdObj);
      Cmd:=CmdObj.AsJSON;
    finally
      CmdObj.Free;
    end;
    try
      Res:=C.ExecuteCommand(Cmd);
      ResStr:=Res.FormatJSON;
      Log:=C.GetCommandLog;
    except
      On E : Exception do
        begin
        ResStr:=E.Message;
        Log:=C.GetCommandLog;
        end;
    end;
    AResponse.Content:=CreateTemplatePage('success','Package tested succesfully',
      ['command',DisplayCmd,
       'response',ResStr,
       'commandlog',FormatLogLines(Log)]);
  finally
    Res.Free;
    C.Free;
  end;

end;

Procedure TFPPKg.UploadRequest(Sender: TObject; ARequest: TRequest;  AResponse: TResponse; Var Handled: Boolean);

Var
  F :  TUploadedFile;
  DestFn,S,PackageName : String;
  Info : TSearchRec;
  ASize : Int64;

begin
  LoadConfig(ARequest);
  Handled:=True;
  if not IsAuthorized(ARequest) then
    begin
    SendError(AResponse,401,'Unauthorized');
    Exit;
    end;
  PackageName:=ARequest.ContentFields.Values['packageName'];
  If (PackageName='') then
    begin
    SendError(AResponse,400,'A package name is required');
    Exit;
    end;
  If (ARequest.Files.Count=0) then
    begin
    SendError(AResponse,400,'Need at least 1 file');
    Exit;
    end;
  F:=ARequest.Files[0];
  If (CompareText(ExtractFileExt(F.FileName),'.zip')<>0) then
    begin
    SendError(AResponse,400,'Need a zip file');
    Exit;
    end;
  if FindFirst(F.LocalFileName,0,Info)<>0 then
    begin
    SendError(AResponse,500,'Uploaded file does not exist');
    Exit;
    end
  else
    begin
    ASize:=Info.Size;
    FindClose(Info);
    if (ASize>TSettings.Global.PackageMaxSize*1024) then
      begin
      SendError(AResponse,400,Format('File size exceeds limit of %d Kb.',[TSettings.Global.PackageMaxSize]));
      Exit;
      end;
    end;
  S:=AnalyzeZipFile(F.LocalFileName);
  If S<>'' then
    begin
    SendError(AResponse,400,'Invalid zip file: '+S);
    Exit;
    end;
  // Move file to a place where the package server can reach it.
  DestFN:=TSettings.Global.PackageDir+ExtractFileName(F.LocalFileName);
  DestFN:=ChangeFileExt(DestFN,'.zip');
  if Not SameFileName(F.LocalFileName,DestFN) then
    if not RenameFile(F.LocalFileName,DestFN) then
      begin
      SendError(AResponse,500,'Failed to move zip file to download location');
      Exit;
      end;
  TestFile(DestFN,PackageName,AResponse)
end;

Procedure TFPPKg.LoadConfig(ARequest : TRequest);

Var
  FN : String;
  P,L : Integer;
begin
  TSettings.Global.CheckConfig;
  if TSettings.Global.TemplateDir='' then
    TSettings.Global.TemplateDir:=ExtractFilePath(Paramstr(0));
  if (TSettings.Global.BaseURL='') and (ARequest.URL<>'') then
    begin
    P:=Pos('.cgi',ARequest.URL);
    if P<>0 then
      L:=3
    else
      begin
      P:=Pos('.fcgi',ARequest.URL);
      l:=4;
      end;
    if P<>0 then
      TSettings.Global.BaseURL:=Copy(ARequest.URL,1,P+L)+'/';
    end;
end;

initialization
  RegisterHTTPModule('FPPKG', TFPPKg);
end.

