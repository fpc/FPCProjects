{$mode objfpc}
{$h+}
unit utests;

interface

uses cgiapp,sysutils,mysqlDB,whtml,dbwhtml;

Type
  TTestSuite = Class(TCgiApplication)
  Private
    FHTMLWriter : THtmlWriter;
    FComboBoxProducer : TComboBoxProducer;
    FDB : TMySQLDatabase;
    FVersion,
    FCPU,
    FOS  : String;
    FDate : TDateTime;
    FDebug,
    FOnlyFailed : Boolean;
  Public
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Function  GetSingleTon(Const Qry : String) : String;
    Function InitCGIVars : Boolean;
    Procedure DoRun; override;
    Procedure EmitForm;
    Procedure ShowResults;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure EmitTitle(ATitle : String);
  end;

implementation

Const
  DefDatabase = 'FPC';
  DefHost     = '';
  DefDBUser   = ''; // fill this in when compiling.
  DefPassword = ''; // fill this in, too.

Procedure TTestSuite.DoRun;

begin
  Try
    Try
      If Not InitCGIVars then
        EmitForm
      else
        ShowResults;
    finally
        DisConnectFromDB;
    end;
  Finally
    Terminate;
  end;
end;


Function TTestSuite.InitCGIVars : Boolean;

Var
  S : String;

begin
  DateSeparator:='/';
  Result:=False;
  FVersion:=RequestVariables['TESTVERSION'];
  FOS:=RequestVariables['TESTOS'];
  FCPU:=RequestVariables['TESTCPU'];
  S:=RequestVariables['TESTDATE'];
  If (S<>'') then
    Try
      FDate:=StrToDate(S);
    except
      FDate:=0;
    end;
  S:=RequestVariables['TESTFAILEDONLY'];
  FOnlyFailed:=(S='1');
  S:=RequestVariables['DEBUGCGI'];
  FDebug:=(S='1');
  Result:=(FVersion<>'') and (FOS<>'') and (FCPU<>'')
          and (Round(FDate)<>0);
  FHtmlWriter:=THTMLWriter.Create(Response);
  FComboBoxProducer:=TComboBoxProducer.Create(Self);
end;

Function TTestSuite.ConnectToDB : Boolean;

begin
  Result:=False;
  FDB:=TMySQLDatabase.Create(Self);
  FDB.HostName:=DefHost;
  FDB.DatabaseName:=DefDatabase;
  FDB.UserName:=DefDBUser;
  FDB.Password:=DefPassword;
  FDB.Connected:=True;
  Result:=True;
end;

procedure TTestSuite.DisconnectFromDB;

begin
  If Assigned(FDB) then
    begin
    if (FDB.Connected) then
      FDB.Connected:=False;
    FreeAndNil(FDB);
    end;
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry: String);

begin
  ComboBoxFromQuery(ComboName,Qry,'')
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry,Value : String);

Var
  Q : TMySQLDataset;

begin
  Q:=TMySQLDataset.Create(Self);
  try
    Q.Database:=FDB;
    Q.SQL.Text:=Qry;
    Q.Open;
    FComboboxProducer.Dataset:=Q;
    FComboBoxProducer.ValueField:=Q.Fields[0].FieldName;
    FComboBoxProducer.DataField:=Q.Fields[1].FieldName;
    FComboBoxProducer.Value:=Value;
    FComboBoxProducer.InputName:=ComboName;
    FComboBoxProducer.CreateComboBox(Response);
  Finally
    Q.Free;
  end;
end;

Function TTestSuite.GetSingleton(Const Qry : String) : String;

Var
  Q : TMySQLDataset;

begin
  Result:='';
  Q:=TMySQLDataset.Create(Self);
  try
    Q.Database:=FDB;
    Q.SQL.Text:=Qry;
    Q.Open;
    Try
      If Not (Q.EOF and Q.BOF) then
        Result:=Q.Fields[0].AsString;
    Finally
      Q.Close;
    end;
  finally
    Q.Free;
  end;
end;
Procedure TTestSuite.EmitTitle(ATitle : String);

begin
  AddResponseLn('<HTML>');
  AddResponseLn('<TITLE>'+ATitle+'</TITLE>');
  AddResponseLn('<BODY>');
end;

Procedure TTestSuite.EmitForm;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title);
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('View Test suite results');
    HeaderEnd(1);
    Write('Please specify search criteria:');
    ParagraphStart;
    FormStart('testsuite.cgi','');
    TableStart(2,true);
    RowStart;
      CellStart;
        Write('Operating system:');
      CellNext;
        ComboBoxFromQuery('TESTOS','SELECT TO_ID,TO_NAME FROM TESTOS',FOS);
      CellEnd;
    RowNext;
      CellStart;
        Write('Processor:');
      CellNext;
        ComboBoxFromQuery('TESTCPU','SELECT TC_ID,TC_NAME FROM TESTCPU',FCPU);
      CellEnd;
    RowNext;
      CellStart;
        Write('Version');
      CellNext;
        ComboBoxFromQuery('TESTVERSION','SELECT TV_ID,TV_VERSION FROM TESTVERSION',FVERSION);
      CellEnd;
    RowNext;
      CellStart;
        Write('Date');
      CellNext;
        If (FDate=0) then
          EmitInput('TESTDATE','')
        else
          EmitInput('TESTDATE',DateToStr(FDate));
      CellEnd;
    RowNext;
      CellStart;
        Write('Only failed tests');
      CellNext;
        EmitCheckBox('TESTFAILEDONLY','1',FonlyFailed);
      CellEnd;
    RowEnd;
    TableEnd;
    ParaGraphStart;
    EmitSubmitButton('','Search');
    EmitResetButton('','Reset form');
    FormEnd;
    end;
  AddResponseLn('</BODY>');
  AddResponseLn('</HTML>');
end;

Procedure TTestSuite.ShowResults;

Var
  Q : TMySQLDataset;
  S : String;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Search Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('View Test suite results: Result');
    HeaderEnd(1);
    Write('Search criteria: ');
    TableStart(2,true);
    RowStart;
      CellStart;
        Write('Operating system:');
      CellNext;
        Write(GetSingleTon('SELECT TO_NAME FROM TESTOS WHERE TO_ID='+FOS));
      CellEnd;
    RowNext;
      CellStart;
        Write('Processor:');
      CellNext;
        Write(GetSingleTon('SELECT TC_NAME FROM TESTCPU WHERE TC_ID='+FCPU));
      CellEnd;
    RowNext;
      CellStart;
        Write('Version');
      CellNext;
        Write(GetSingleton('SELECT TV_VERSION FROM TESTVERSION WHERE TV_ID='+FVERSION));
      CellEnd;
    RowNext;
      CellStart;
        Write('Date');
      CellNext;
        If (FDate<>0) then
          Write(DateToStr(FDate));
      CellEnd;
    RowEnd;
    TableEnd;
    ParaGraphStart;
    Q:=TMySQLDataset.Create(Self);
    Try
      Q.Database:=FDB;
      S:='SELECT T_NAME,T_FULLNAME,TR_SKIP,TR_OK FROM ';
      S:=S+' TESTRESULTS,TESTS WHERE ';
      S:=S+' (TR_TEST_FK=T_ID)';
      S:=S+' AND (TR_CPU_FK='+FCPU+')';
      S:=S+' AND (TR_VERSION_FK='+FVERSION+')';
      S:=S+' AND (TR_OS_FK='+FOS+')';
      S:=S+' AND (TR_DATE="'+FormatDateTime('YYYY/MM/DD',FDate)+'")';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      Q.SQL.Text:=S;
      Q.Open;
      If FDebug then
        Write('Query : '+S);
      ParaGraphStart;
      Try
        With TTableProducer.Create(Self) do
          try
            Dataset:=Q;
            Border:=True;
            CreateTable(Response);
          Finally
            Free;
          end;
       Write('Record count:'+IntTostr(Q.RecordCount));
      Finally
        Q.Close;
      end;
    Finally
      Q.Free;
    end;
    end;
end;

end.
