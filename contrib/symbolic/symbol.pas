Program Derive;
{
    $ id:                                                       $
    Copyright (c) 2000 by Marco van de Voort (marco@freepascal.org)
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    Performs symbolic derivation of simple expressions. Alpha version.
    Derivation routine based on conceptual code provided by Osmo Ronkanen.
    Also some very basic simplification routines.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Problems:

- Often untested.
- RPN to Infix adds ()'s when not necessary.
-
}

Uses ParsExpr,Symbolic;

{Symbolic derivative with respect to X}
function Deriv(t:pnode):pnode;

var x,y     : ArbFloat;
    p,p1,p2 : pnode;

begin
  Deriv:=nil;
  if (t=nil) then exit;
  with t^ do begin
   case nodetype of
     VarNode:    if upcase(variable[1])='X' then
                  Deriv:=NewiConst(ArbInt(1))
                 else
                  Deriv:=NIL; {NewConst(0);} {NIL?}
     ConstNode: Deriv:=NIL; {NewConst(0);}
     IConstNode: Deriv:=NIL;
     CalcNode:  begin
                  case op of
                   add,sub: deriv:=NewCalc(op,Deriv(left),Deriv(right));
                   mul:  Deriv:=NewCalc(add,
                                 NewCalc(mul,Deriv(left),copyTree(right)),
                                 NewCalc(mul,Deriv(right),copytree(left)));

                   dvd:  Deriv:=NewCalc(dvd,
                                  NewCalc(sub,
                                   NewCalc(mul,Deriv(left),copyTree(right)),
                                   NewCalc(mul,Deriv(right),copytree(left))),
                                   NewFunc(sqrx,CopyTree(right)));
                  pow: begin
                         p1:=Deriv(Right);
                         if P1<>NIL then
                          p1:=NewCalc(mul,p1,NewFunc(Lnx,CopyTree(Left))); { ln(l)*r'}
                         p2:=Deriv(Left);
                         if P2<>NIL then
                          p2:=Newcalc(Mul,CopyTree(Right),newcalc(mul,p2,
                                newfunc(invx,CopyTree(left))));
                         IF (P1<>nil) and (p2<>nil) then
                          deriv:=newcalc(mul,CopyTree(t),newcalc(add,p1,p2))
                         else
                          if (P1=NIL) and (P2=NIL) then {Simplify first to avoid this!}
                           deriv:=NIL
                          else
                           begin
                            if P1=NIL THEN { one of both is constant}
                             P1:=P2;       {The appopriate term is now in P1}
                            deriv:=newcalc(mul,CopyTree(t),p1);
                           end;
                        end;
                     end;
                  end;
     FuncNode: begin
                 case fun of
                   invx:  Deriv:=NewCalc(dvd,
                                  NewFunc(Minus,NewCalc(mul,Deriv(right),copytree(left))),
                                   NewFunc(sqrx,CopyTree(right)));
                   minus: Deriv:=NewFunc(minus,Deriv(son));
                   sinx:  Deriv:=NewCalc(Mul,
                            NewFunc(cosx,Copytree(son)),
                            Deriv(son));
                   cosx:  deriv:=NewCalc(mul,
                            NewFunc(minus,NewFunc(sinx,copytree(son))),
                            Deriv(son));
                   tanx:  deriv:=Newcalc(dvd,
                            deriv(son),
                            newfunc(sqrx,newfunc(cosx,copytree(son))));
                   sqrx:  deriv:=newcalc(mul, newconst(2),
                                  newcalc(mul,copytree(son),deriv(son)));
                   lnx :  deriv:=newcalc(mul,newcalc(dvd,newconst(1),CopyTree(son)),
                                             deriv(son)); { dln(x)=x' * 1/x}
                   expx:  deriv:=newcalc(mul,newfunc(expx,copytree(son)),deriv(son));

                    end;
                  end;
           end;
    end; {WITH}
end;

procedure simplify(expr:pnode);

function isconst(p:pnode):boolean;

begin
 isconst:=(p^.nodetype=iconstnode) or (p^.nodetype=constnode);
end;

var val1,val2 : ArbFloat;
    ival1,
    ival2,
    tmpval2,
    tmpval    : ArbInt;
    IsInteger : boolean;

procedure setupoperation(auto:boolean);

begin
 with expr^ do
 begin
 if (right^.nodetype=iconstnode) and (left^.nodetype=iconstnode) then
   begin
    IsInteger:=true;
    if auto then
     nodetype:=iconstnode;
    ival2:=right^.ivalue;
    ival1:=left^.Ivalue;
    end
  else
   begin
    IsInteger:=false;
    if (right^.nodetype=iconstnode) then
     val2:=right^.ivalue
    else
     val2:=right^.value;
    if (left^.nodetype=iconstnode) then
     val1:=left^.ivalue
    else
     val1:=left^.value;
    nodetype:=constnode;
   end;
  if auto or not isinteger then
   begin
    dispose(right);
    dispose(left);
   end;
 end;
end;

begin
 case expr^.nodetype of
  calcnode : begin
            simplify(expr^.left);
            simplify(expr^.right);
            if isconst(expr^.left) and isconst(expr^.right) then
             begin
              with expr^ do
              case op of
               add : begin
                      SetupOperation(true);
                      if IsInteger then
                       ivalue:=ival1+ival2
                      else
                       value:=val1+val2;
                     end;
               sub  : begin
                       SetupOperation(true);
                       if IsInteger then
                        ivalue:=ival1-ival2
                       else
                        value:=val1-val2;
                      end;
               mul  : begin
                       SetupOperation(true);
                       if IsInteger then
                        ivalue:=ival1*ival2
                       else
                        value:=val1*val2;
                      end;
               dvd  : begin
                       SetupOperation(false);
                       if IsInteger then
                        begin
                         tmpval:=ival1 div ival2;
                         if (tmpval*ival2)=ival1 then {no rounding, OK!}
                          begin
                           nodetype:=iconstnode;
                           ivalue:=tmpval;
                           disposeExpr(expr^.right);
                           disposeExpr(expr^.left);
                          end;
                        end
                        else
                          value:=val1 / val2;
                       end;
               pow :  begin
                       SetupOperation(False);
                       If IsInteger then
                        begin
                         If ival2=1 then
                          begin
                           NodeType:=iconstnode;
                           ivalue:=ival1;
                          end
                         else
                          if ival2>1 then
                           begin
                            tmpval2:=ival1;
                            for tmpval:=2 to ival2 do ival1:=Ival1*tmpval2;
                            nodetype:=iconstnode;
                            ivalue:=ival1;
                           end;
                        end
                      else
                        value:=exp(val2*ln(val1));
                    end;
                end; {case}
            end; {if}
          end; {case calcnode}

  funcnode: begin
             Simplify(expr^.son);
             if (expr^.son^.nodetype=constnode) and {only func(real) is simplified}
                ((expr^.fun<>minus) and (Expr^.fun<>invx)) then
                                     {minus or inv(real) is not simplifyable}
              begin
               with expr^ do
               begin
                case fun of
                  cosx : val1:=Cos(son^.value);
                  sinx : val1:=sin(son^.value);
                  tanx : val1:=sin(son^.value)/cos(son^.value);
                  sqrx : val1:=sqr(son^.value);
                 end;
               expr^.nodetype:=constnode;
               value:=val1;
               dispose(son);
              end;
             end;
       end;
 end;
end;

function IsolatePlusMinTerms(x:pnode):PTerms;
{ Walk tree "X", and isolate terms delimited by + and -
something like x*(x-5+x*x)+y*x*1234*(x+y)+342
will be splitted into
x*(x-5+x*x)    ,     y*x*1234*(x+y)            and 342}

var TermStack : array[0..255] of pnode;
    TermNr    : ArbInt;
    I         : ArbInt;
    TheTerms     : PTerms;

procedure SlipIn(node:Pnode);
begin
  if TermNr<255 then
    begin
     TermStack[TermNr]:=node;
     Inc(TermNr);
    end;
end;

procedure CheckForTerms(tree:pnode);

begin
 If (Tree^.NodeType=CalcNode) then
  begin
   if ((tree^.op=sub) or (tree^.op=add)) then
    begin
     CheckForTerms(Tree^.Left);
     CheckForTerms(tree^.Right);
    end
   else
    SlipIn(tree)
  end
 else
  SlipIn(Tree);
end;

begin
  TermNr:=0;
  CheckForTerms(x);
  If TermNr=0 Then
   IsolatePlusMinTerms:=NIL
  else
   begin
    GetMem(TheTerms,Sizeof(ArbInt)+TermNr*sizeof(Pnode));
    TheTerms^.NrTerms:=TermNr;
    For I:=0 TO TheTerms^.NrTerms-1 DO
     begin
      Getmem(TheTerms^.Terms[I],sizeof(ArbInt)+Sizeof(pnode));
      WITH TheTerms^.Terms[I]^ DO
       begin
        NrTerms:=1;
        Terms[0]:=TermStack[I];
       end;
      end;
    IsolatePlusMinTerms:=TheTerms;
   end;
end;

procedure IsolateMulDivTerms(var x:PTermNode);
{To be run after a IsolatePlusMinTerms. Two separate procedures because it
might be done with user feedback (only expand the term the user clicks)

Same as PlusMin but for MulDiv:

x*x*y*(5324+x)*(5+y) is split up into
X   ,  x     ,        y      ,      (5324+x)        and (5+y)

This can be used to implement collection of likewise terms, or eliminate
factors.  (so x*x -> sqr(x) or x^2.   Or, in the example above, kill (5434+x)
by multiplying all other terms with 5434, and once with x. (thus creating new
terms)

}
var TermStack : array[0..255] of pnode;
    TermNr    : ArbInt;
    I         : ArbInt;

procedure SlipIn(node:Pnode);
begin
  if TermNr<255 then
    begin
     TermStack[TermNr]:=node;
     Inc(TermNr);
    end;
end;

procedure CheckForTerms(tree:pnode);

begin
 If (Tree^.NodeType=CalcNode) then
  begin
   if ((tree^.op=mul) or (tree^.op=dvd)) then
    begin
     CheckForTerms(Tree^.Left);
     CheckForTerms(tree^.Right);
    end
   else
    SlipIn(tree)
  end
 else
  SlipIn(Tree);
end;

begin
 IF X<>NIL THEN
  begin
  TermNr:=0;
  CheckForTerms(X^.Terms[0]);
  If TermNr<>0 Then
   begin
{    FreeMem(x,Sizeof(ArbInt)+x^.NrTerms*sizeof(Pnode));}
    ReAllocMem(x,Sizeof(ArbInt)+TermNr*sizeof(Pnode));
    x^.NrTerms:=TermNr;
    Move (TermStack[0],x^.Terms,TermNr*Sizeof(PNode));
   end;
  end;
end;

procedure ShowTerms(S:String;p:pnode);

var PT : PTerms;
    I,I2  : ArbInt;

begin
  PT:=IsolatePlusMinTerms(p);           {Split expresssion into terms}
  IsolateMulDivTerms(pT^.terms[0]);      {Split first term further}
  IF PT<>NIL then
   begin
    WritelN(S,' ',pt^.nrterms);
    For I:=0 To PT^.NrTerms-1 DO
      if PT^.Terms[I]^.Nrterms=1 then
        writeln(i,' ',ExpressionToString(PT^.Terms[I]^.terms[0]))
       else
        for I2:=0 TO PT^.Terms[I]^.NrTerms-1 DO
          writeln(i,' -  ',ExpressionToString(PT^.Terms[I]^.terms[I2]));

    FreeMem(PT,Sizeof(ArbInt)+PT^.NrTerms*sizeof(Pnode));
   end;
end;

var Expr,
    Derivative  : pnode;
    RPNExpr,
    Expression  : string;

begin
 {Some expressions. Must be all caps for TP, FPC has overloaded upcase for
 strings. Allowed are cos(),sin(),tan(),sqr(),exp(),ln(),inv() (=1/x),
                      + - * / ^}

 { Expression:='exp(X*Y*14224*(124+X*(12+9/3))*(2*X*X)+(2+3)^9)';}
{   Expression:='-X^5';}
 { Readln(expression); }
{  Expression:='SIN(X)*256+X*COS(X)';}
  Expression:='x*y*x*(1+x)+(1+y)*x*x*x+x*x';

  Writeln('Initial Expression: ',Expression);
  Expr:=Evaluate(Expression,RPNExpr);
  WriteLn('RPN Expression    : ',RPNExpr);
  Simplify(Expr);
  Writeln('Main simplified)  : ',Expression);
  writeln('Infix Eq of RPN   : ',ExpressionToString(expr));
  derivative:=deriv(expr);
  writeln('And its derivative: ',ExpressionToString(derivative));
  Writeln('RPN of derivative : ',TreeToRPN(derivative));
  ShowTerms('Terms of org. expression:',Expr);
  ShowTerms('Terms of deriv. expression:',derivative);
  DisposeExpr(expr);
  DisposeExpr(derivative);
end.
{
  $Log$
  Revision 1.1  2002/12/15 21:01:26  marco
  Initial revision

}
