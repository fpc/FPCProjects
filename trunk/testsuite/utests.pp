{$mode objfpc}
{$h+}
unit utests;

interface

uses cgiapp,sysutils,mysqlDB,whtml,dbwhtml,db;

Type
  TTestSuite = Class(TCgiApplication)
  Private
    FHTMLWriter : THtmlWriter;
    FComboBoxProducer : TComboBoxProducer;
    FDB : TMySQLDatabase;
    FRunID,
    FVersion,
    FCPU,
    FOS  : String;
    FDate : TDateTime;
    FDebug,
    FNoSkipped,
    FOnlyFailed : Boolean;
    FAction : Integer;
    FTestLastDays : Integer;
    Procedure GetOverviewRowAttr(Sender : TObject; Var BGColor : String;
                                   Var Align : THTMLAlign; Var VAlign : THTMLValign;
                                   Var CustomAttr : String) ;
    Procedure GetRunRowAttr(Sender : TObject; Var BGColor : String;
                            Var Align : THTMLAlign; Var VAlign : THTMLValign;
                            Var CustomAttr : String) ;
    Procedure FormatFailedOverview(Sender : TObject; Var CellData : String);
  Public
    Function CreateDataset(Qry : String) : TMySQLDataset;
    Function CreateTableProducer(DS : TDataset) :TTableProducer;
    Procedure DefaultTableFromQuery(Qry,ALink : String; IncludeRecordCount : Boolean);
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Function  GetSingleTon(Const Qry : String) : String;
    Function GetOSName(ID : String) : String;
    Function GetCPUName(ID : String) : String;
    Function GetVersionName(ID : String) : String;
    Function InitCGIVars : Integer;
    Procedure DoRun; override;
    Procedure EmitForm;
    Procedure ShowRunResults;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure EmitTitle(ATitle : String);
    Procedure ShowRunOverview;
    Function  ShowRunData : Boolean;
  end;

implementation

Const
{$i utests.cfg}

{ if utests.cfg is missed, create one with the following contents:
  DefDatabase = 'TESTSUITE';
  DefHost     = '';
  DefDBUser   = ''; // fill this in when compiling.
  DefPassword = ''; // fill this in, too.
}

Const
  SDetailsURL = 'testsuite.cgi?TESTACTION=1&TESTRUN=%s';

Procedure TTestSuite.DoRun;

begin
  Try
    Try
      Case InitCGIVars of
        0 : EmitForm;
        1 : ShowRunResults;
      end;
    finally
        DisConnectFromDB;
    end;
  Finally
    Terminate;
  end;
end;


Function TTestSuite.InitCGIVars : Integer;

Var
  S : String;

begin
  FHtmlWriter:=THTMLWriter.Create(Response);
  FComboBoxProducer:=TComboBoxProducer.Create(Self);
  DateSeparator:='/';
  Result:=0;
  FAction:=StrToIntDef(RequestVariables['TESTACTION'],0);
  FVersion:=RequestVariables['TESTVERSION'];
  FOS:=RequestVariables['TESTOS'];
  FCPU:=RequestVariables['TESTCPU'];
  S:=RequestVariables['TESTDATE'];
  FRunID:=RequestVariables['TESTRUN'];
  FTestLastDays:=StrToIntDef(RequestVariables['TESTLASTDAYS'],31);
  If (S<>'') then
    Try
      FDate:=StrToDate(S);
    except
      FDate:=0;
    end;
  S:=RequestVariables['TESTFAILEDONLY'];
  FOnlyFailed:=(S='1');
  S:=RequestVariables['TESTNOSKIPPED'];
  FNoSkipped:=(S='1');
  S:=RequestVariables['DEBUGCGI'];
  FDebug:=(S='1');
  Result:=FAction;
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
    RowNext;
      CellStart;
        Write('No skipped tests');
      CellNext;
        EmitCheckBox('TESTNOSKIPPED','1',FNoSkipped);
      CellEnd;
    RowEnd;
    TableEnd;
    ParaGraphStart;
    EmitSubmitButton('','Search');
    EmitResetButton('','Reset form');
    FormEnd;
    end;
  ShowRunOverview;
  AddResponseLn('</BODY>');
  AddResponseLn('</HTML>');
end;

procedure TTestSuite.GetOverviewRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
begin
  If ((Sender as TTAbleProducer).CurrentRow mod 2=0) then
    BGColor:='#EEEEEE'
end;


Function TTestSuite.CreateDataset(Qry : String) : TMySQLDataset;

begin
  Result:=TMySQLdataset.Create(Self);
  With Result do
    begin
    Database:=FDB;
    SQL.Text:=Qry;
    end;
end;

Function TTestSuite.CreateTableProducer(DS : TDataset) :TTableProducer;

begin
  Result:=TTableProducer.Create(Self);
  Result.Dataset:=DS;
end;

Procedure TTestSuite.DefaultTableFromQuery(Qry,Alink : String; IncludeRecordCount : Boolean);

Var
  Q : TMySQLDataset;

begin
  If FDebug then
    Write('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            If (Alink<>'') then
              begin
              CreateColumns(Nil);
              If TableColumns.Count>0 then
                (TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
              end;
            CreateTable(Response);
          Finally
            Free;
          end;
        If IncludeRecordCount then
          Write('Record count: '+IntTostr(Q.RecordCount));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;

Procedure TTestSuite.ShowRunOverview;

Const
  SOverview = 'SELECT TU_ID,TU_DATE,TC_NAME,TO_NAME,TV_VERSION,COUNT(TR_ID) as RESULTCOUNT,'+
              '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN) AS OK,'+
              '(TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as FAILED,'+              
              '(TU_SUCCESSFULLYFAILED+TU_SUCCESFULLYCOMPILED+TU_SUCCESSFULLYRUN+'+
                'TU_FAILEDTOCOMPILE+TU_FAILEDTORUN+TU_FAILEDTOFAIL) as TOTAL,'+
              'TU_SUBMITTER as SUBMITTER, TU_MACHINE as MACHINE, TU_COMMENT as COMMENT'+
              ' FROM TESTRESULTS,TESTRUN,TESTCPU,TESTOS,TESTVERSION '+
              'WHERE '+
              ' (TC_ID=TU_CPU_FK) AND '+
              ' (TO_ID=TU_OS_FK) AND '+
              ' (TV_ID=TU_VERSION_FK) AND '+
              ' (TR_TESTRUN_FK=TU_ID) '+
              ' %s '+
              ' GROUP BY TU_ID ';


Var
  S,A,Qry : String;
  Q : TMySQLDataset;

begin
   S:='';
   If (FCPU<>'') then
     S:=S+' AND (TU_CPU_FK='+FCPU+')';
   If (FVersion<>'') then
     S:=S+' AND (TU_VERSION_FK='+FVERSION+')';
   if (FOS<>'') then
     S:=S+' AND (TU_OS_FK='+FOS+')';
   If (Round(FDate)<>0) then
     S:=S+' AND (TU_DATE="'+FormatDateTime('YYYY/MM/DD',FDate)+'")'
   else
     S:=S+' AND (TU_DATE>="'+FormatDateTime('YYYY/MM/DD',Date-FTESTLASTDAYS)+'")';
   If FOnlyFailed then
     S:=S+' AND (TR_OK="-")';
   A:=SDetailsURL;
   If FOnlyFailed then
     A:=A+'&TESTFAILEDONLY=1';
   If FNoSkipped then
     A:=A+'&TESTNOSKIPPED=1';
  Qry:=Format(SOverview,[S]);
  If FDebug then
    Write('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            OnGetRowAttributes:=@GetOverViewRowAttr;
            CreateColumns(Nil);
            TableColumns.ColumnByName('TU_ID').ActionURL:=A;
            TableColumns.ColumnByNAme('FAILED').OnGetCellContents:=@FormatFailedOverview;
            CreateTable(Response);
          Finally
            Free;
          end;
        Write('Record count: '+IntTostr(Q.RecordCount));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;


Function TTestSuite.GetOSName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TO_NAME FROM TESTOS WHERE TO_ID='+ID);
end;

Function TTestSuite.GetCPUName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TC_NAME FROM TESTCPU WHERE TC_ID='+ID);
end;

Function TTestSuite.GetVersionName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleton('SELECT TV_VERSION FROM TESTVERSION WHERE TV_ID='+ID);
end;

Function TTestSuite.ShowRunData : Boolean;

COnst
  SGetRunData = 'SELECT TU_ID,TU_DATE,TC_NAME,TO_NAME,TV_VERSION '+
                ' FROM TESTRESULTS,TESTRUN,TESTCPU,TESTOS,TESTVERSION '+
                'WHERE '+
                ' (TC_ID=TU_CPU_FK) AND '+
                ' (TO_ID=TU_OS_FK) AND '+
                ' (TV_ID=TU_VERSION_FK) AND '+
                ' (TU_ID=%s)';


Var
  Q : TmYSQLDataset;

begin
  Result:=(FRunID<>'');
  If Result then
    begin
    Q:=CreateDataset(Format(SGetRunData,[FRunID]));
    Try
      Q.Open;
      Result:=Not (Q.EOF and Q.BOF);
      If Result then
        With FHTMLWriter do
          begin
          TableStart(2,true);
          RowStart;
            CellStart;
              Write('Operating system:');
            CellNext;
              Write(Q.FieldByName('TO_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Processor:');
            CellNext;
              Write(Q.FieldByName('TC_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Version');
            CellNext;
              Write(Q.FieldByNAme('TV_VERSION').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Date');
            CellNext;
              Write(Q.FieldByNAme('TU_DATE').AsString);
            CellEnd;
          RowEnd;
          TableEnd;
          ParaGraphStart;
          end;
    Finally
      Q.Close;
      Q.Free;
    end;
    end;
end;

Procedure TTestSuite.ShowRunResults;

Var
  S : String;
  Qry : String;
  Q : TMySQLDataset;
  FL : String;
  
begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Search Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for run '+FRunID);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data : ');
    HeaderEnd(2);
    If ShowRunData then
      begin
      HeaderStart(2);
      Write('Detailed test run results:');

      FL:='';
      If FOnlyFailed or FNoSkipped then
        begin
        FL:='';
        If FOnlyFailed then
          FL:='failed';
        if FNoSkipped then
          begin
          If (FL<>'') then
            FL:=FL+',';
          FL:=FL+'not skipped';
          end;
        Write(' (only '+FL+' tests are shown)');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      S:='SELECT T_NAME as Test,T_FULLNAME as FileName ,TR_SKIP as Skipped,TR_OK as OK FROM ';
      S:=S+' TESTRESULTS,TESTS WHERE ';
      S:=S+' (TR_TEST_FK=T_ID) ';
      S:=S+'  AND (TR_TESTRUN_FK='+FRunID+') ';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      If FNoSkipped then
        S:=S+' AND (TR_SKIP="-")';
      Qry:=S;
      If FDebug then
        Write('Query : '+Qry);
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Test,FileName';
                If Not FNoSkipped then
                  FL:=FL+',Skipped';
                If Not FOnlyFailed then
                  FL:=FL+',OK';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
            Write('Record count: '+IntTostr(Q.RecordCount));
          Finally
            Close;
          end;
        finally
          Free;
        end;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

procedure TTestSuite.GetRunRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
  
Var
  P : TTableProducer;
  
begin
  P:=(Sender as TTAbleProducer);
  If (FOnlyFailed and FNoSkipped) then
    begin
    If (P.CurrentRow Mod 2)=0 then
      BGColor:='#EEEEEE'
    end
  else
    If P.Dataset.FieldByName('Skipped').AsString='+' then
      BGColor:='yellow'    // Yellow
    else If P.Dataset.FieldByName('OK').AsString='+' then
      BGColor:='#98FB98'    // pale Green
    else
      BGColor:='#FF82AB';   // Light red
end;

procedure TTestSuite.FormatFailedOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('TU_ID').AsString]);
  S:=S+'&TESTFAILEDONLY=1&TESTNOSKIPPED=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

end.
