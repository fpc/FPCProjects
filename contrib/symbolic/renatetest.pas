{
    $ id:                                                       $
    Copyright (c) 2000 by Marco van de Voort (marco@freepascal.org)
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    Most basic test for TEvaluator class.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

program EvalTest;

{$AppType Console}

Uses Classes,Symbolic;

VAR Expr    : TExpression;
    SymVars : TStringList;
    I       : Longint;
    VarName : TStringList;
    Eval    : TEvaluator;
    Vars    : Array[0..2] OF ArbFloat;

begin
 {Lets create in a nice equation. Totally nonsense. Don't try to
                 make sense of it}

 Expr:=TExpression.Create('exp(-x*y/(sqr(sqr(z))+sin(z/exp(x+sqrt(y)))))');
 Writeln('Expression after parsing :',Expr.InfixExpr);

 {Hmm. But the user could have typed that in. Let's check if he used
  symbolic values}

 SymVars:=Expr.SymbolicValueNames;

 If SymVars.Count>0 Then
   For I:=0 TO SymVars.Count-1 DO
     Writeln(I:5,' ',Symvars[I]);

 {Assume the user selected X,Y,Z from above stringlist with symbolics
 as our variables}

  VarName:=TStringList.Create;
  VarName.Add('X');
  VarName.Add('Y');
  VarName.Add('Z');

 {Create the Evaluator Object}

 Eval:=TEvaluator.Create(VarName,Expr);

 {My HP48g provided this value for PI:}

 IF Symvars.IndexOf('PI')<>-1 THEN      {If PI exists, then assume it is the
                                         circle radius vs diameter ratio}
    Eval.SetConstant('PI',3.14159265359);

 IF Symvars.IndexOf('X0')<>-1 THEN      {Set X0 to Douglas' number}
    Eval.SetConstant('X0',42);

 {All this above looks slow isn't? It probably even is. Unit symbolic has
  evaluations as plus, not as target. The evaluation is built for
  fast repeated evaluations, not just one.
  However the Evaluate method is hopefully reasonably fast.
  Under FPC TEvaluator.Evaluate is about 600-700 assembler instructions,
  without operation on pointer trees and without recursion.
  If your compiler (and hopefully FPC too) can inline the math unit functions,
  the speed gain could be dramatic.}

 Vars[0]:=2; Vars[1]:=3; Vars[2]:=4;
 Writeln('Stackdepth needed for evaluation: ',eval.EvalDepth);
 FOR I:=1 TO 100000 DO
   Eval.Evaluate(Vars);
 Eval.Free;
 Expr.Free;
 SymVars.Free;
// VarName.Free;  {Is freed by TEvaluator.Destroy. Should TEvaluator copy it?}
end.
