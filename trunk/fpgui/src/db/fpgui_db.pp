{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    fpGUI database support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit fpGUI_DB;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses Classes, fpGUI, DB;

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


// ===================================================================
// ===================================================================

implementation


constructor TFieldDataLink.Create(AWidget: TWidget);
begin
  inherited Create;
  FWidget := AWidget;
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
  Text := FDataLink.Field.DisplayText;
end;


end.


{
  $Log$
  Revision 1.1  2000/12/23 23:20:16  sg
  * First public CVS version...

}
