unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, lNetComponents;

type

  { TMainForm }

  TMainForm = class(TForm)
    ConnectButton: TButton;
    ConnectionBox: TGroupBox;
    SMTP: TLSMTPClientComponent;
    PortEdit: TEdit;
    PortLabel: TLabel;
    ServerLabel: TLabel;
    ServerEdit: TEdit;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SMTPConnect(Sender: TLSMTPClient);
    procedure SMTPError(const msg: string; aSocket: TLSocket);
    procedure SMTPReceive(Sender: TLSMTPClient);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  SMTP.Connect(ServerEdit.Text, Word(StrToInt(PortEdit.Text)));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SMTP.OnError:=@SMTPError;
end;

procedure TMainForm.SMTPConnect(Sender: TLSMTPClient);
begin
  ShowMessage('connected');
end;

procedure TMainForm.SMTPError(const msg: string;
  aSocket: TLSocket);
begin
  ShowMessage(msg);
end;

procedure TMainForm.SMTPReceive(Sender: TLSMTPClient);
var
  s: string;
begin
  SMTP.GetMessage(s);
end;

initialization
  {$I main.lrs}

end.

