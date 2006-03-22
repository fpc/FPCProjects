unit Main;

{$mode objfpc}{$H+}

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  LResources,
  StdCtrls,
  Symbolic,
  SysUtils;

type
  { TForm1 }
  TForm1 = class(TForm)
    ResultEdit: TEdit;
    ExpressionEdit: TEdit;
    Label1:    TLabel;
    Label2:    TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Eval:    TEvaluator;
  Expr:    TExpression;
  Form1:   TForm1;
  VarName: TStringList;
  Vars:    array[0..1] of ArbFloat;

implementation

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Eval.Free;
  Expr.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Expr := TExpression.Create(ExpressionEdit.Text);

  VarName := TStringList.Create;
  VarName.Add('X');
  VarName.Add('T');

  {Create the Evaluator Object}
  Eval := TEvaluator.Create(VarName, Expr);

  if Expr.SymbolicValueNames.IndexOf('PI') <> -1 then
    Eval.SetConstant('PI', 3.14159265359);

  if Expr.SymbolicValueNames.IndexOf('X0') <> -1 then
    Eval.SetConstant('X0', 42);
end;

procedure TForm1.TrackBarChange(Sender: TObject);
begin
  if TTrackBar(Sender).Tag = 0 then
    Label1.Caption := 'x: ' + FloatToStr(TTrackBar(Sender).Position / 100)
  else
    Label2.Caption := 't: ' + FloatToStr(TTrackBar(Sender).Position / 100);

  Vars[0] := TrackBar1.Position / 100;
  Vars[1] := TrackBar2.Position / 100;
  ResultEdit.Text := FloatToStr(Eval.Evaluate(Vars));
end;

initialization
  {$I main.lrs}

end.
