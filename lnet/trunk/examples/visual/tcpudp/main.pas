unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, lNetComponents, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DiconnectButton: TButton;
    ConnectButton: TButton;
    ListenButton: TButton;
    LTCPComponent: TLTCPComponent;
    LUDPComponent: TLUDPComponent;
    PortEdit: TEdit;
    IPEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ConnectionGroup: TRadioGroup;
    RBTCP: TRadioButton;
    RBUDP: TRadioButton;
    SendButton: TButton;
    SendEdit: TEdit;
    TextArea: TMemo;
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure ListenButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DiconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure LTcpComponentDisconnect(aSocket: TLSocket);
    procedure RBTCPChange(Sender: TObject);
    procedure RBUDPChange(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
  private
    FNet: TLNetComponent;
    FIsServer: Boolean;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  if FNet.Connect(IPEdit.Text, StrToInt(PortEdit.Text)) then
    FIsServer:=False;
end;

procedure TForm1.ListenButtonClick(Sender: TObject);
begin
  if FNet.Listen(StrToInt(PortEdit.Text)) then begin
    TextArea.Append('Accepting connections');
    FIsServer:=True;
  end;
end;

procedure TForm1.LTCPComponentConnect(aSocket: TLSocket);
begin
  TextArea.Append('Connected to remote host');
end;

procedure TForm1.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  TextArea.Append(msg);
  TextArea.SelStart:=Length(TextArea.Lines.Text);
end;

procedure TForm1.LTCPComponentAccept(aSocket: TLSocket);
begin
  TextArea.Append('Connection accepted');
  TextArea.SelStart:=Length(TextArea.Lines.Text);
end;

procedure TForm1.LTCPComponentReceive(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin
    TextArea.Append(s);
    TextArea.SelStart:=Length(TextArea.Lines.Text);
    FNet.IterReset;
    if FIsServer then repeat
      FNet.SendMessage(s, FNet.Iterator);
    until not FNet.IterNext;
  end;
end;

procedure TForm1.LTcpComponentDisconnect(aSocket: TLSocket);
begin
  TextArea.Append('Connection lost');
  TextArea.SelStart:=Length(TextArea.Lines.Text);
end;

procedure TForm1.SendButtonClick(Sender: TObject);
var
  AllOK: Boolean;
  n: Integer;
begin
  if Length(SendEdit.Text) > 0 then begin
    AllOk:=True;
    if Assigned(FNet.Iterator) then repeat
      n:=FNet.SendMessage(SendEdit.Text, FNet.Iterator);
      if n < Length(SendEdit.Text) then begin
        TextArea.Append('Error on send [' + IntToStr(n) + ']');
        AllOK:=False;
      end;
    until not FNet.IterNext;
    if FIsServer and AllOK then
      TextArea.Append(SendEdit.Text);
    SendEdit.Text:='';
  end;
end;

procedure TForm1.DiconnectButtonClick(Sender: TObject);
begin
  FNet.Disconnect;
  TextArea.Append('Disconnected');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FNet:=LTcpComponent;
  FIsServer:=False;
end;

procedure TForm1.RBTCPChange(Sender: TObject);
begin
  FNet.Disconnect;
  FNet:=LTcpComponent;
end;

procedure TForm1.RBUDPChange(Sender: TObject);
begin
  FNet.Disconnect;
  FNet:=LUdpComponent;
end;

procedure TForm1.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    SendButtonClick(Sender);
end;

initialization
  {$I main.lrs}

end.

