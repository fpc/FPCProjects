program InterbaseTest;

{$linklib dl}
{$linklib crypt}

uses SysUtils, Classes, fpGUI, DB, Interbase;

type
  TFieldDataLink = class(TDataLink)
  private
    FWidget: TWidget;
    FField: TField;
    FFieldName: String;
    FOnDataChange: TNotifyEvent;
    procedure SetFieldName(const AFieldName: String);
    procedure UpdateField;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(AField: TField); override;
  public
    constructor Create(AWidget: TWidget);
    property Field: TField read FField;
    property FieldName: String read FFieldName write SetFieldName;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;

  TDBText = class(TCustomLabel)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: String;
    procedure SetDataField(const ADataField: String);
    function GetDataSource: TDataSource;
    procedure SetDataSource(ADataSource: TDataSource);
    procedure DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text;
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TMainForm = class(TForm)
    Database: TIBDatabase;
    Transaction: TIBTransaction;
    Query: TIBQuery;
    DataSource: TDataSource;
    Box, ConnectionBox: TBoxLayout;
    ConnectionLabel, ConnectionStateLabel: TLabel;
    ListBox: TListBox;
    CurDatasetLabel: TLabel;
    CurNameText, CurEMailText: TDBText;
    Navi: TBoxLayout;
    FirstDataset, PrevDataset, NextDataset, LastDataset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FirstDatasetClick(Sender: TObject);
    procedure PrevDatasetClick(Sender: TObject);
    procedure NextDatasetClick(Sender: TObject);
    procedure LastDatasetClick(Sender: TObject);
  end;



constructor TFieldDataLink.Create(AWidget: TWidget);
begin
  inherited Create;
  FWidget := AWidget;
end;

procedure TFieldDataLink.ActiveChanged;
begin
  UpdateField;
end;

procedure TFieldDataLink.RecordChanged(AField: TField);
begin
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

procedure TFieldDataLink.SetFieldName(const AFieldName: String);
begin
  if AFieldName <> FieldName then
  begin
    FFieldName := AFieldName;
    UpdateField;
  end;
end;

procedure TFieldDataLink.UpdateField;
begin
WriteLn('##############UpdateField. DataSet: ', DataSource.DataSet.ClassName);
  FField := DataSource.DataSet.FindField(FieldName);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;


constructor TDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create(Self);
  FDataLink.OnDataChange := @DataChange;
end;

destructor TDBText.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TDBText.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBText.SetDataField(const ADataField: String);
begin
  FDataLink.FieldName := ADataField;
end;

function TDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBText.SetDataSource(ADataSource: TDataSource);
begin
  FDataLink.DataSource := ADataSource;
end;

procedure TDBText.DataChange(Sender: TObject);
begin
WriteLn('TDBText.DataChange');
  if Assigned(FDataLink.Field) then
    Text := FDataLink.Field.DisplayText
  else
    WriteLn('  Field ist NIL');
end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  x: Integer;
  s: String;
begin
  Query := TIBQuery.Create(Self);

  Database.Connected := True;
  Database.Transaction := Transaction;
  Query.Database := Database;
  DataSource.DataSet := Query;
  Transaction.Action := caRollback;
  Transaction.Active := True;

  if Database.Connected then
    ConnectionStateLabel.Text := 'Yes'
  else
    ConnectionStateLabel.Text := 'No';

  CurNameText.DataSource := DataSource;
  CurNameText.DataField := 'UserName';
  CurEMailText.DataSource := DataSource;
  CurEMailText.DataField := 'InstEmail';

  Query.SQL.Add('select * from fpdev');
WriteLn('Query.Active? ', Query.Active);
  Query.Open;

WriteLn('Query.Active? ', Query.Active);

  while not Query.EOF do
  begin
    SetLength(s, 0);
    for x := 0 to Query.FieldCount - 2 do
      s := s + Query.Fields[x].AsString + ', ';
    s := s + Query.Fields[Query.FieldCount - 1].AsString;
    ListBox.Items.Add(s);
    Query.Next;
  end;

  Query.First;
end;

procedure TMainForm.FirstDatasetClick(Sender: TObject);
begin
  Query.First;
end;

procedure TMainForm.PrevDatasetClick(Sender: TObject);
begin
  Query.Prior;
end;

procedure TMainForm.NextDatasetClick(Sender: TObject);
begin
  Query.Next;
end;

procedure TMainForm.LastDatasetClick(Sender: TObject);
begin
  Query.Last;
end;



var
  MainForm: TMainForm;
begin
  Application.Title := 'Interbase Test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
