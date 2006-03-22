unit Main;

{$mode objfpc}{$H+}

interface

uses
  Buttons,
  Classes,
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
    Button1:    TButton;
    ExpressionEdit: TEdit;
    OutputMemo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Expr: TExpression;
  SymVars: TStringList;
  I:    longint;
  VarName: TStringList;
  Eval: TEvaluator;
  Vars: array[0..1] of ArbFloat;
begin
  OutputMemo.Clear;

 {Lets create in a nice equation. Totally nonsense. Don't try to make sense of it}

  Expr := TExpression.Create(ExpressionEdit.Text);
  OutputMemo.Lines.Add('Expression after parsing :' + Expr.InfixExpr);
  OutputMemo.Lines.Add('');

 {Hmm. But the user could have typed that in. Let's check if he used
  symbolic values}

  SymVars := Expr.SymbolicValueNames;

  if SymVars.Count > 0 then
    for I := 0 to SymVars.Count - 1 do
      OutputMemo.Lines.Add(IntToStr(I) + ' ' + Symvars[I]);

  OutputMemo.Lines.Add('');

  {Assume the user selected X and T from above stringlist as our variables}

  VarName := TStringList.Create;
  VarName.Add('X');
  VarName.Add('T');

  {Create the Evaluator Object}

  Eval := TEvaluator.Create(VarName, Expr);

  {My HP48g provided this value for PI:}

  if Symvars.IndexOf('PI') <> -1 then      {If PI exists, then assume it is the
                                         circle radius vs diameter ratio}
    Eval.SetConstant('PI', 3.14159265359);

  if Symvars.IndexOf('X0') <> -1 then      {Set X0 to Douglas' number}
    Eval.SetConstant('X0', 42);

 {All this above looks slow isn't? It probably even is. Unit symbolic has
  evaluations as plus, not as target. The evaluation is built for
  fast repeated evaluations, not just one.
  However the Evaluate method is hopefully reasonably fast.
  Under FPC TEvaluator.Evaluate is about 600-700 assembler instructions,
  without operation on pointer trees and without recursion.
  If your compiler (and hopefully FPC too) can inline the math unit functions,
  the speed gain could be dramatic.}

  OutputMemo.Lines.Add('Stackdepth needed for evaluation: ' + IntToStr(eval.EvalDepth));
  OutputMemo.Lines.Add('');

  for I := 1 to 50 do
  begin
    Vars[0] := 1 / I * 1.1;
    Vars[1] := 1 / I * 2;
    OutputMemo.Lines.Add(VarName.Strings[0] + '=' + FloatToStrF(Vars[0], ffFixed, 4, 4) +
      ' ' + VarName.Strings[1] + '=' + FloatToStrF(Vars[1], ffFixed, 4, 4) +
      ' = ' + FloatToStrF(Eval.Evaluate(Vars),
      ffFixed, 4, 4));
  end;

  Eval.Free;
  Expr.Free;
  SymVars.Free;
  // VarName.Free;  {Is freed by TEvaluator.Destroy. Should TEvaluator copy it?}
end;

initialization
  {$I main.lrs}

end.
