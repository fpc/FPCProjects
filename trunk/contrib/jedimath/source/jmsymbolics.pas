{******************************************************************************}
{                                                                              }
{ JmSymbolics.pas for Jedi Math Alpha 1.02                                     }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file is dual licenced under the MPL and LGPL.           }
{ The user can choose which to use.                                            }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/          }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{    This program is distributed in the hope that it will be useful,           }
{    but WITHOUT ANY WARRANTY; without even the implied warranty of            }
{    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                      }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The Original Code is SymbolicTest.pas.                                       }
{  Initial code :                                                              }
{    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)            }
{    member of the Free Pascal development team                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit was based upon the jcl code.                                       }
{ This unit contains                                                           }
{                                                                              }
{ Unit owner: Chris Eyre (chris@chris-eyre.demon.co.uk)                        }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

unit JmSymbolics;

{

    Base types for expression trees, and some small procs to create them.


Seems not to have memory leaks atm. If you experience them, check procedure
newcalc first.
}

interface

{$i jedimath.inc}

Uses
  Classes,
  Sysutils,
  JmTypes;

Type
  {Should be somewhere in the JCLMath or even in JCLtypes unit}

  calcop=(addo,subo,mulo,dvdo,powo);     {real operators}

  FuncOp=(cosx,sinx,tanx,sqrx,sqrtx,expx,lnx,invx,minus,cotanx,arcsinx,arccosx,
          arctanx,sinhx,coshx,tanhx,arcsinhx,arccoshx,arctanhx,log10x,
          log2x,lnxpix,faculx,arctan2x,stepx,powerx,hypotx,lognx);
  {functions, both one and two parameter ones. Including pseudo function minus}

  Operation=(VarNode,ConstNode,iconstnode,CalcNode,FuncNode,func2node,VLIWVar);

  pnode =^treenode;
  treenode=record
    case nodetype:operation of
         iconstnode: (ivalue:TJmInteger);
         VarNode:    (variable:string[11]);
         VLIWVar:    (vliwindex:TJmInteger);                       {^float?}
         ConstNode:  (value:TJmFloat);
         CalcNode:   (op:calcop;left,right:pnode);
         FuncNode:   (fun:funcop;son:pnode);
         Func2Node:  (fun2:funcop;son2left,son2right:pnode);
    end;

  ERPNStack       = Class(Exception);     { There is a problem with the RPN parser's stack. }
  EIError         = Class(Exception);     { This exception covers all internal errors. Most often this
                                            means that a function name is misspelled or the function you
                                            are trying to call does not exist.                           }
  EDiv0           = Class(Exception);     { <TITLE EDiv0>

                                            This exception is raised, if a division by zero occurs. Note
                                            that this exception differs from the C++ standard exception
                                            for a division by zero. It is RPN specific and allows you to
                                            handle divisions by zero in the parsed equation.             }

  TJmBaseExpression = class(TObject)
  protected
    ExprTree    : pnode;
    function NewConst(value:TJmFloat):pnode;
    function NewiConst(value:TJmInteger):pnode;
    function NewCalc(op:calcop;left,right:pnode):pnode;
    function CopyTree(p :pnode):pnode;
    function NewFunc(fun:funcop;son:pnode):pnode; overload;
    function NewFunc(fun:funcop;son,son2:pnode):pnode; overload;
    function NewVar(variable:string):pnode;
    procedure DisposeExpr(p:pnode);
  end;

  EParserStack = class(ERPNStack);    { There was an parser stack under- or overflow while processing
                                        your expression. Make sure there are no errors in it.         }
  EParserIE    = class(EIError);              { An internal error occured while processing your expression.
                                                This should normally not happen.                            }

  TJmBaseExprParser= class(TJmBaseExpression)
  public
    function InFixToParseTree(Expr : String;VAR RPNexpr: String):pnode; virtual;
    function ParseTreeToRPN  (expr:pnode):string; virtual;
    function ParseTreeToInfix(expr:pnode):string; virtual;
  end;


  EFaculNotInt = Class(exception);    { This exception is raised if the calculated faculty on a real
                                        value deviates from an integer value by more than 0.01. This
                                        should normally not happen.                                  }
  EExprIE      = Class(EIerror);
  { An internal error occured while processing your expression.
    This should normally not happen.                            }

  ENotInt      = Class(exception);
  { The parsers raises this exception if an integer value is
    expected, but none is found. (e.g. the value is of the wrong
    type)                                                        }

  ENotFloat    = Class(Exception);
  { The parsers raises this exception if a float value is
    expected, but none is found. (e.g. the value is of the wrong
    type)                                                        }


Const
  ConstIncrement = 20;
  VLIWIncr       = 20;

type
  TJmEvaluator = class;

  TJmExpression = class(TJmBaseExprParser)
  protected
   InfixClean     : Boolean;
   InfixCache     : String;
   Evaluator      : TJmEvaluator;
   EvaluatorUpToDate : Boolean;
   function    GetInfix:String;
   function    GetRPN:String;
   procedure Simpleop(expr:TJmExpression;oper:calcop);
   function  Simpleopwithresult(expr:TJmExpression;oper:calcop) : TJmExpression;
   Function  IntDerive(const derivvariable:String;theexpr:pnode) : pnode;
   Function  GetIntValue:LongInt;
   Procedure SetIntValue(val:Longint);
   Function  GetFloatValue:TJmFloat;
   Procedure SetFloatValue(val:TJmFloat);
  public
   SimplificationLevel : Longint;
   constructor Create(Infix:String);
   constructor EmptyCreate;
   destructor Destroy; override;

   Procedure   SetNewInfix(Infix:String);
   Function    Derive(derivvariable:String):TJmExpression;
   procedure   SymbolSubst(ToSubst,SubstWith:String);
   function    SymbolicValueNames:TStringList;
   function    Taylor(Degree:TJmInteger;const x,x0:String):TJmExpression;
   function    Newton(x:String):TJmExpression;

   procedure   SimplifyConstants;

   function add(Expr:TJmExpression):TJmExpression;
   function dvd(Expr:TJmExpression):TJmExpression;
   function mul(Expr:TJmExpression):TJmExpression;
   function power(Expr:TJmExpression):TJmExpression;
   function sub(Expr:TJmExpression):TJmExpression;

   procedure Addto(Expr:TJmExpression);
   procedure Divby(Expr:TJmExpression);
   procedure RaiseTo(Expr:TJmExpression);
   procedure SubFrom(Expr:TJmExpression);
   procedure Times(Expr:TJmExpression);
   property  InfixExpr: string read GetInfix write SetNewInfix;
   property  RpnExpr: string read GetRPN;
   property  ValueAsInteger:longint read GetIntValue write SetIntvalue; {Default?}
   property  ValueAsFloat:TJmFloat   read GetFloatValue write SetFloatValue;
  end;


  VLIWWordtype=  (avariable,anoperation, afunction,
                  afconstant, aiconstant,placeholder);

 { RPN operators or functions with two arguments are the same.}
 vliwop2=(addv,subv,mulv,dvdv,powv,arctan2v,stepv,hypotv,lognv);

 pTJmFloat       = ^TJmFloat;


 pVLIWEvalWord  = ^VLIWEvalWord;
 VLIWEvalword  = record
                 case VLIWEntity :  VLIWWordType OF
                  AVariable  : (IndexOfVar : TJmInteger);
                  AnOperation: (op:vliwop2);       {2 arguments}
                  AFunction  : (fun1:funcop);   {functions with one argument}
                  AiConstant : (ivalue:TJmInteger);
                  AfConstant : (value:TJmFloat);
                  placeholder: (IndexOfConstant:TJmInteger) ;
                 end;

  TVLIWArr       = array[0..1000] of VLiwEvalWord;
  pVliwArr       = ^TVliwArr;

 TJmEvaluatorNotEnoughVariables=class(Exception);    {Not enough variables passed to Evaluate}
 TJmEvaluatorStackException    =class(ERPNStack); {RPN Stack over/under flow}
 TJmEvaluatorBadConstant       =class(Exception);    {Constant value not specified}
 TJmEvaluatorIE                =class(Exception);    {Internal error. Probably something out of sync.}

 TJmEvaluator = Class(TObject) {Only needs the notion of a pnode }
 public
   VariableName     : TStringList;
   ConstantValue    : TList;
   ConstantNames    : TStringList;
   MaxStack,
   VLIWCount,
   VLIWAlloc        : TJmInteger;
   VLIWRPNExpr      : pVLIWArr;
   procedure   SetConstant(Name:String;Value:TJmFloat);
   constructor Create(VariableList:TStringList;Expression:pnode); overload;
   constructor Create(VariableList:TStringList;Expression:TJmExpression); overload;
   destructor  Destroy; override;
   procedure   TreeToVLIWRPN(expr:pnode);
   function    Evaluate(const variables:Array of TJmFloat):TJmFloat;

end;

{
 Structures used to index a pnode tree to identify terms.
}
     PTerms = ^TTerms;
     PtermNode=^TTermNode;
     TtermNode= record
                 NrTerms :TJmInteger;
                 Terms  : Array[0..499] of PNode;
                end;
     TTerms = record
                 NrTerms : TJmInteger;
                 Terms: Array[0..499] of PtermNode;
                end;

const
  InfixOperatorName   : array[addo..powo] of char= ('+','-','*','/','^');
  FunctionNames    : array[cosx..lognx] of string[8]=(
             'cos','sin','tan','sqr','sqrt','exp','ln','inv','-',
             'cotan','arcsin','arccos','arctan','sinh',
             'cosh','tanh','arcsinh','arccosh','arctanh',
             'log10','log2','lnxp1','!','arctan2',
             'step','power','hypot','logn');
  FunctionNamesUpper: array[cosx..lognx] of string[8]=(
             'COS','SIN','TAN','SQR','SQRT','EXP','LN','INV','-',
             'COTAN','ARCSIN','ARCCOS','ARCTAN','SINH',
             'COSH','TANH','ARCSINH','ARCCOSH','ARCTANH',
             'LOG10','LOG2','LNXP1','!','ARCTAN2',
             'STEP','POWER','HYPOT','LOGN');

resourcestring
   SEvalIE                  ='TEvaluator: Internal Error: ';
   SEvalStackDepthExceeded  ='TEvaluator: Stack depth Evaluate exceeded';
   SEvalBadNumberOfVars     ='TEvaluator: Invalid number of variables';
   SParsIE                  ='TBaseExprParser: Internal Error:';
   SParseRPNOverflow        ='TBaseExprParser: RPN stack overflow';
   SParseRPNUnderflow       ='TBaseExprParser: RPN stack underflow';
   SParsDiv0                ='TBaseExprParser: RPN Division by zero in parser';
   SParsOpOverflow          ='TBaseExprParser: Operator stack Overflow';
   SParsOpUnderflow         ='TBaseExprParser: Operator stack Underflow';
   SNILDeref                ='NIL dereference';
   SExprIE                  ='TJmExpression: Internal error: ';
   SExprNotInt              ='TJmExpression: This is not an integer';
   SExprNotFloat            ='TJmExpression: This is not a float';
   SExprInvmsg              ='TJmExpression: Inv(x) evaluates to 1/0';
   SExprInvSimp             ='TJmExpression: Division by 0 encountered while simplifying';

implementation
uses
  JmMathBasics; {We use our own routines here}

{newconst and newiconst are overloaded in FPC}

function TJmBaseExpression.NewConst(value:TJmFloat):pnode;
{Generate a new node for a floating point constant}

var
  t : pnode;
begin
 new(t);
 t^.nodetype:=constnode;
 t^.value:=value;
 NewConst:=T;
end;

function TJmBaseExpression.NewiConst(value:TJmInteger):pnode;
{Generate a new node for integer constant}

var
  t : pnode;
begin
  new(t);
  t^.nodetype:=iconstnode;
  t^.ivalue:=value;
  NewiConst:=T;
end;

procedure TJmBaseExpression.DisposeExpr(p:pnode);
{Recursively kill expression tree}
begin
 IF p<>NIL THEN
  begin
   case p^.nodetype of
    CalcNode : begin
                 DisposeExpr(p^.right);
                 DisposeExpr(p^.left);
               end;
    FuncNode : DisposeExpr(p^.son);
    end;
   Dispose(p);
  end;
end;

function TJmBaseExpression.NewCalc(op:calcop;left,right:pnode):pnode;
{Create NewCalc node. Left and Right may be nil because
to avoid introducing empty nodes, the deriv()
function may return NIL's, which are to be treated as newiconst(0);

Also one of the functions most likely to have memory leaks

}

    function isintegerone(testme:pnode) : boolean;
    begin
     Isintegerone:=(testme^.nodetype=iconstnode) and (testme^.ivalue=1);
    end;

var
  t : pnode;

begin
  if op=powo then
   begin
    if right=NIL then                   {x^0 =1 for every X}
     begin
      DisposeExpr(left);
      newcalc:=newiconst(1);
      exit;
     end;
    if left=NIL THEN                    { 0^y = 0 except for y=0, but that is}
     begin                              { covered above}
      DisposeExpr(right);
      NewCalc:=NIL;
      exit;
     end;
    if IsIntegerone(left) then           {x^1 =x}
     begin
      DisposeExpr(left);
      NewCalc:=right;
      exit;
     end;
    If IsIntegerone(right) then             { 1^y=1}
     begin
      DisposeExpr(left);
      NewCalc:=right;
      exit;
     end;
   end; {generate a plain power node for all other cases}
  if left=NIL then
   begin
    if (right=nil) or (op=mulo) or (op=dvdo) then     { 0*0, 0*t or  0/t =0}
     begin                              { We have no way to check T for nul}
      IF Right<>NIL then
       DisposeExpr(Right);
      NewCalc:=NIL;
      exit;
     end;
    if op=addo then   {  Don't generate a calc node for 0+x, but return x}
     begin
      NewCalc:=right;
      exit;
     end;
    new(t);
    t^.nodetype:=funcnode; { 0-x = minus(x) }
    t^.fun:=minus;
    t^.son:=right;
    NewCalc:=T;
    exit;
   end;
  if right=NIL then
   begin
    if (left=nil) or (op=mulo) or (op=dvdo) then     { 0*0, 0*t or  0/t =0}
     begin
      IF left<>NIL then
       disposeExpr(Left);
      NewCalc:=Nil;
      exit;
     end;
    NewCalc:=Left;      { for x-0 or x+0, simply return 0}
    exit;
   end;

 If ((op=mulo) or (op=dvdo)) and isintegerone(right) then  { simplify t*1 and t/1}
  begin
   DisposeExpr(right);
   NewCalc:=Left;
   exit;
  end;
 if (op=mulo) and isintegerone(left) then                 { simplify 1*t}
   begin
   DisposeExpr(left);
   NewCalc:=right;
   exit;
  end;
 new(t);
 t^.nodetype:=calcnode;
 t^.op:=op;
 t^.left:=left;
 t^.right:=right;
 newcalc:=t;
end;

function TJmBaseExpression.CopyTree(p :pnode):pnode;

var newtree : pnode;

begin
 new(newtree);
 move(p^,Newtree^,sizeof(treenode));
 if newtree^.nodetype=CalcNode then
  begin
   newtree^.left:=CopyTree(p^.left);
   newtree^.right:=CopyTree(p^.right);
  end
 else
  if newtree^.nodetype=FuncNode then
   newtree^.son:=CopyTree(p^.son);
 CopyTree:=NewTree;
end;

function TJmBaseExpression.NewFunc(fun:funcop;son:pnode):pnode;

var t : pnode;

begin
 IF son<>nil then
  begin
   new(t);
   t^.nodetype:=funcnode;
   t^.fun:=fun;
   t^.son:=son;
   NewFunc:=T;
  end
 else
  NewFunc:=NIL;
end;

function TJmBaseExpression.NewFunc(fun:funcop;son,son2:pnode):pnode;

var t : pnode;

begin
 new(t);
 t^.nodetype:=func2node;
 t^.fun:=fun;
 t^.son2Left:=son;
 t^.son2Right:=son2;
 NewFunc:=T;
end;

function TJmBaseExpression.NewVar(variable:string):pnode;

var p :pnode;

begin
 new(p);
 p^.nodetype:=varnode;
 p^.variable:=variable;
 newvar:=p;
end;
{
Problems:
- -x^12 is -(x^12) or (-x)^12 ? (FIXED: Chose to do it as in FPC)
- No errorhandling. (will be rewritten to use classes and exceptions first)

Original comments:

---------------------------------------------------------------------------
THAI TRAN

I've netmailed you the full-featured version (800 lines!) that will do
Functions, exponentiation, factorials, and has all the bells and whistles,
but I thought you might want to take a look at a simple version so you can
understand the algorithm.

This one only works With +, -, *, /, (, and ).  I wrote it quickly, so it
makes extensive use of global Variables and has no error checking; Use at
your own risk.

Algorithm to convert infix to postfix (RPN) notation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Parse through the entire expression getting each token (number, arithmetic
operation, left or right parenthesis).  For each token, if it is:
 1. an operand (number)        Send it to the RPN calculator
 2. a left parenthesis         Push it onto the operation stack
 3. a right parenthesis        Pop operators off stack and send to RPN
                               calculator Until the a left parenthesis is
                               on top of the stack.  Pop it also, but don't
                               send it to the calculator.
 4. an operator                While the stack is not empty, pop operators
                               off the stack and send them to the RPN
                               calculator Until you reach one With a higher
                               precedence than the current operator (Note:
                               a left parenthesis has the least precendence).
                               Then push the current operator onto the stack.

This will convert (4+5)*6/(2-3) to 4 5 + 6 * 2 3 - /

Algorithm For RPN calculator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note:  this Uses a different stack from the one described above.

In RPN, if an operand (a number) is entered, it is just pushed onto the
stack.  For binary arithmetic operators (+, -, *, /, and ^), the top two
operands are popped off the stack, operated on, and the result pushed back
onto the stack.  if everything has gone correctly, at the end, the answer
should be at the top of the stack.

Released to Public Domain by Thai Tran (if that matters).
---------------------------------------------------------------------------
MvdV: It does for me. My routines might end up in either FPC or Jedi, and
      anything except LGPL and PD is unacceptable. :-)

Modifications: (starting to get so big that the original is hardly
               recognisable)
- OOP. Mainly to allow symbolic TJmExpression class to have custom parsers.
- Working with pnode stack instead of reals. Pnodes can be any expression,
   see inteface unit symbolic. (creating a parsetree)
- Support for functions(one or two parameter arguments), which weren't in the
   short Swag version. Most MATH functions are supported.
- Can make a difference between the minus of (-x) and the one in (x-y).
   The first is converted to function minus(x);
- power operator
- Faculty operator
- Conversions back to RPN and infix.
- Removing of excess parentheses.
}

type
  {Tokens generated by the parser. Anything else is a constant or variable}
  ParseOperation=(padd,psub,pmul,pdvd,ppow,pfacul,pleft,pright,
                  pcos,psin,ptan,psqr,psqrt,pexp,pln,pinv,
                  pminus, pcotan,parcsin,parccos,parctan,psinh,pcosh,ptanh,
                  parcsinh,parccosh,parctanh,plog10,
                  plog2,plnxpi,parctan2,pstep,ppower,phypot,
                  plogn,pnothing);

CONST
 ParserFunctionNamesUpper   : array[padd..pnothing] of string[7]=
                 ('+','-','*','/','^','!','(',')','COS','SIN',
                 'TAN','SQR','SQRT','EXP','LN','INV','-',
                 'COTAN','ARCSIN','ARCCOS','ARCTAN',
                 'SINH','COSH','TANH','ARCSINH',
                 'ARCCOSH','ARCTANH','LOG10',
                 'LOG2','LNXP1','ARCTAN2','STEP',
                 'POWER','HYPOT','LOGN','NOTHING');

 {Operator or function-}
 Priority          : array[padd..pnothing] of TJmInteger=
                 (1,1,2,2,3,0,0,0,
                  4,4,4,4,4,4,4,4,
                  4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5);

 OppsXlat='+-*/^!()'; {Must match the first entries of ParseOperation.
                         Pos(OppsXlat,c)-1+ord(Padd) is typecast!}

Const
  RPNMax = 20;              { I think you only need 4-8, but just to be safe }
  OpMax  = 25;
  AllowedInToken = ['0'..'9','.','E','e'];

type
  String15 = String[15];

procedure ParserInternalError(const Msg:String;A,B:TJmInteger);

var
  S,S2 : String;
begin
  Str(A,S);      {Usually a identification number for the occurance}
  Str(B,S2);     {Usually the value that tripped the IE}
  raise EParserIE.Create(SParsIE+Msg+S+' '+S2);
end;

function TJmBaseExprParser.InFixToParseTree(Expr : String;VAR RPNexpr: String):pnode;
var
  RPNStack   : Array[1..RPNMax] of PNode;        { Stack For RPN calculator }
  RPNTop,
  OpTop      : TJmInteger;
  OpStack    : Array[1..OpMax] of ParseOperation;    { Operator stack For conversion }

  Procedure RPNPush(Num : PNode); { Add an operand to the top of the RPN stack }
  begin
    if RPNTop < RPNMax then
     begin
      Inc(RPNTop);
      RPNStack[RPNTop] := Num;
     end
    else
    RAISE EParserStack.Create(SParseRPNOverflow);
  end;

  Function RPNPop : pnode;       { Get the operand at the top of the RPN stack }
  begin
    if RPNTop > 0 then
     begin
      RPNPop := RPNStack[RPNTop];
      Dec(RPNTop);
     end
    else
      RAISE EParserStack.Create(SParseRPNUnderflow);
  end;

  Procedure RPNCalc(Token : String15);                       { RPN Calculator }
  Var
    treal : TJmFloat;
    tint  : TJmInteger;
    Error : TJmInteger;
  begin
     RPNExpr:=RPNExpr+token+' ';
     Val(Token, treal, Error);
     IF (error=0) then
      begin
        if (Pos('.',token)=0) and (Pos('E',token)=0) Then
         begin
          Val(Token,tint,Error);
          RpnPush(Newiconst(tint));
         end
        else
         RPNPush(NewConst(Treal));
      end
     else  { Handle error }
       RPNPush(NewVar(Token));
  end;

  Procedure RPNOperation(Operation:ParseOperation);
  {The workhorse. Creates the tree, and associates a parseoperation with
   the TJmExpression enumerations. Avoids some ugly (and shaky) typecasts
   between operations like in earlier versions.}

  var Temp: pnode;

  begin
    RPNExpr:=RPNExpr+ParserFunctionNamesUpper[Operation]+' ';
    Case Operation of                                   { Handle operators }
      padd : RPNPush(newcalc(addo,RPNPop,RPNPop));
      psub : begin
              Temp:=RPNPOP;
              RPNPush(NewCalc(subo,RPNPOP,Temp));
             end;
      pmul : RPNPush(newcalc(mulo,RPNPOP,RPNPop));
      pdvd : begin
              Temp := RPNPop;
              if Temp <> NIL then
               RPNPush(newcalc(dvdo,RPNPop,Temp))
              else
               Raise EDiv0.Create(SParsDiv0);  { Handle divide by 0 error }
             end;
      ppow,ppower : {are only different in parsing x^y and power(x,y)}
             begin
              Temp:=RpnPop;
              RpnPush(NewCalc(powo,RpnPop,Temp));
             end;
    pfacul : RPNPush(NewFunc(faculx,RPNPOP));
      psin : RPNPush(NewFunc(sinx,RPNPop));
      pcos : RPNPush(NewFunc(cosx,RPNPop));
      ptan : RPNPush(NewFunc(tanx,RPNPop));
      psqr : RPNPush(NewFunc(sqrx,RPNPop));
      pexp : RPNPush(NewFunc(expx,RPNPop));
      pln  : RPNPush(NewFunc(lnx,RPNPop));
      pinv : RPNPush(NewFunc(invx,RPNPop));
    Pminus : RPNPush(newFunc(minus,RPNPop));
    pcotan : RPNPush(NewFunc(cotanx,rpnpop));
   parcsin : RPNPush(NewFunc(arcsinx,rpnpop));
   parccos : RPNPush(NewFunc(arccosx,rpnpop));
   parctan : RPNPush(NewFunc(arctanx,rpnpop));
     psinh : RPNPush(NewFunc(sinhx,rpnpop));
     pcosh : RPNPush(NewFunc(coshx,rpnpop));
     ptanh : RPNPush(NewFunc(tanhx,rpnpop));
  parcsinh : RPNPush(NewFunc(arcsinhx,rpnpop));
  parccosh : RPNPush(NewFunc(arccoshx,rpnpop));
  parctanh : RPNPush(NewFunc(arctanhx,rpnpop));
    plog10 : RPNPush(NewFunc(log10x,rpnpop));
     plog2 : RPNPush(NewFunc(log2x,rpnpop));
    plnxpi : RPNPush(NewFunc(lnxpix,rpnpop));
  parctan2 : begin
              Temp:=RpnPop;
              RpnPush(Newfunc(arctan2x,RpnPop,temp));
             end;
     pstep : begin
              Temp:=RpnPop;
              RpnPush(Newfunc(stepx,RpnPop,temp));
             end;
     phypot: begin
              Temp:=RpnPop;
              RpnPush(Newfunc(hypotx,RpnPop,temp));
             end;
     plogn : begin
              Temp:=RpnPop;
              RpnPush(Newfunc(lognx,RpnPop,Temp));
            end;
     else
      ParserInternalError('Unknown function',1,ORD(Operation));
     end;
  end;

  Function IsFunction(S:String):ParseOperation;
  var
    Count : ParseOperation;

  begin
   IsFunction:=pnothing;
   for Count:=pCos to pInv do {Minus is a pseudo function, and in this category
                              because it has only 1 argument}
    begin
     If Copy(S,1,3)=ParserFunctionNamesUpper[Count] then
      IsFunction:=Count;
    end;
  end;

  Procedure OpPush(operation : ParseOperation);  { Add an operation onto top of the stack }
  begin
    if OpTop < OpMax then
    begin
      Inc(OpTop);
      OpStack[OpTop] := operation;
    end
    else
     RAISE EParserStack.Create(SParsOpOverflow);
  end;

  Function OpPop : ParseOperation;        { Get operation at the top of the stack }
  begin
    if OpTop > 0 then
    begin
      OpPop := OpStack[OpTop];
      Dec(OpTop);
    end
    else
     RAISE EParserStack.Create(SParsOpUnderflow);
  end;

Var
  I,len       : TJmInteger;
  Token       : String15;
  OperationNr : ParseOperation;
  FunctionNr  : TJmInteger;
  isminus     : boolean;

begin
  RPNExpr:='';
  OpTop  := 0;                                              { Reset stacks }
  RPNTop := 0;
  Token  := '';
  Expr:=UpperCase(Expr);
  i:=1; len:=Length(Expr);
  while I<=Len do
  begin
    {Flush token, if we feel a infix operator coming}
     FunctionNr:=Pos(expr[I],OppsXlat);
     If (FunctionNr<>0) and (Token<>'') THEN
       begin        { Send last built number to calc. }
        RPNCalc(Token);
        Token := '';
       end;
    If (FunctionNr>0) and (FunctionNr<7) then
    begin
      OperationNr:=ParseOperation(FunctionNr-1+ORD(padd));
      If (OperationNr=psub) then  {Minus(x) or x-y?}
       begin
        IsMinus:=False;
        if I=1 then
         IsMinus:=true
        else
         If Expr[I-1] IN ['+','(','*','/','-','^'] then
          IsMinus:=true;
        If IsMinus then
         OperationNr:=PMinus;
       end;
      While (OpTop > 0) AND
                (Priority[OperationNr] <= Priority[OpStack[OpTop]]) DO
                 RPNOperation(OpPop);
      OpPush(OperationNr);
     end
    else
    case Expr[I] of
      '0'..'9' : begin
                  While (Expr[I] in AllowedInToken) and (I<=len) do
                   begin
                    Token:=Token+Expr[I];
                    inc(i);
                   end;
                  dec(i);
                 end;
      ','      :  if Token <> '' then              {Two parameter functions}
                   begin        { Send last built number to calc. }
                    RPNCalc(Token);
                    Token := '';
                   end;
      '('      : OpPush(pleft);
      ')'      : begin
                  While OpStack[OpTop] <> pleft DO
                   RPNOperation(OpPop);
                  OpPop;                    { Pop off and ignore the '(' }
                 end;
      'A'..'Z' : begin
                  if Token <> '' then
                   begin        { Send last built number to calc. }
                    RPNCalc(Token);
                    Token := '';
                   end;
                 While (Expr[I] IN ['0'..'9','A'..'Z']) AND (I<=Len) DO
                  begin
                   Token:=Token+Expr[I];
                   Inc(I);
                  end;
                 Dec(i);
                 OperationNr:=IsFunction(Token);
                 if OperationNr<>pnothing then
                  begin
                   Token:='';
                   While (OpTop > 0) AND
                   (Priority[OperationNr] <= Priority[OpStack[OpTop]]) DO
                   RPNOperation(OpPop);
                  OpPush(OperationNr);
                  end
                 else
                  begin
                   RpnCalc(Token);
                   Token:='';
                  end;
                end;
     end; { Case }
     inc(i);
   end;
  If Token<>'' Then
   RpnCalc(Token);

  While OpTop > 0 do                     { Pop off the remaining operations }
    RPNOperation(OpPop);
 InFixToParseTree:=RpnPop;
end;

function TJmBaseExprParser.ParseTreeToInfix(expr:pnode):string;

var S,right,left : string;
    IsSimpleExpr : boolean;

begin
 IF expr=nil then
  ParserInternalError(SNILDeref,5,0);
 case expr^.nodetype of
   VarNode  : S:=expr^.variable;
  iconstnode: str(expr^.ivalue,S);
   ConstNode: str(expr^.value,s);
   CalcNode : begin
                right:=ParseTreeToInfix(expr^.right);
                left:=ParseTreeToInfix(expr^.left);
                S:=left+InfixOperatorName[Expr^.op]+right;
                if (expr^.op=addo) or (expr^.op=subo) then
                 S:='('+S+')';
              end;
   FuncNode : begin
               left:=functionnames[expr^.fun];
               right:=ParseTreeToInfix(expr^.son);
               issimpleExpr:=false;
               If ((Expr^.fun=minus) or (Expr^.fun=faculx)) and
                  (expr^.son^.nodetype in [varnode,iconstnode,constnode]) then
                issimpleExpr:=true;
               if expr^.fun<>faculx then
                begin
                 If IsSimpleExpr then
                  S:=Left+Right
                 else
                  S:=Left+'('+Right+')';
                 end
                else
                 If IsSimpleExpr then
                  S:=Right+Left
                 else
                  S:='('+Right+')'+Left;
              end;
  Func2Node : begin
               S:=functionnames[expr^.fun];
               Left:=ParseTreeToInfix(Expr^.son2right);
               right:=ParseTreeToInfix(expr^.son2left);
               S:=S+'('+Left+','+Right+')';
              end;
  end;
  ParseTreeToInfix:=S;
end;

function TJmBaseExprParser.ParseTreeToRPN(expr:pnode):string;
{not fast because of the prepending. Creating an array of pnode would maybe
be faster}

procedure SearchTree(Tree:pnode;var s:string);

var temp:string;

begin
 if tree<>nil then
 case Tree^.nodetype of
  VarNode  : s:=Tree^.Variable +' '+s;
  ConstNode: begin
              str(Tree^.value:5:9,temp);              {should be configurable}
              s:=temp+' '+s;
             end;
 iconstnode: begin
              str(Tree^.ivalue,temp);
              s:=temp+' '+s;
             end;
 CalcNode  : begin
               s:=InfixOperatorName[Tree^.op]+' '+s;
               SearchTree(tree^.right,s);
               SearchTree(tree^.left,s);
             end;
 FuncNode:    begin
               s:=functionnames[tree^.fun]+' '+s;
               SearchTree(tree^.son,s);
             end;
  Func2Node:   begin
               s:=functionnames[tree^.fun]+' '+s;
               SearchTree(tree^.son2right,s);
               SearchTree(Tree^.son2left,s);
              end;
    end;
end;

var s : String;

begin
 s:='';
 searchTree(expr,s);
 ParseTreeToRPN:=S;
end;

 {Parser categories}
type
  Float10Arb =ARRAY[0..9] OF BYTE;

const

   TC1 : Float10Arb = (0,0,$00,$00,$00,$00,0,128,192,63);         {Eps}
   TC3 : Float10Arb = (1,0,0,0,0,0,0,0,0,0);                      {3.64519953188247460E-4951}


var {Looks ugly, but is quite handy.}
    macheps  : TJmFloat absolute TC1;  { macheps = r - 1,  with r
                                        the smallest TJmFloat > 1}
    midget   : TJmFloat absolute TC3;  { the smallest positive TJmFloat}

{$ifdef ToDo}
function spepol(x: TJmFloat; var a: TJmFloat; n: TJmInteger): TJmFloat;
var
  pa : ^TJmFloat; {FPC extension. Uses ^ some array of TJmFloat in TP}
  i : TJmInteger;
  polx : TJmFloat;
begin
  pa:=@a;
  polx:=0;
  raise Exception.Create('spepol not yet implemented');
  { TODO -oCJE -cFIXIT : Uncomment the following }
//  for i:=n downto 0 do
//    polx:=polx*x+pa[i]; {and pa^[i] here}
  spepol:=polx
end {spepol};
{$endif ToDo}

{$ifdef ToDo}
function spegam(x: TJmFloat): TJmFloat;
const

    tmax = 170;
    a: array[0..23] of TJmFloat =
    ( 8.86226925452758013e-1,  1.61691987244425092e-2,
      1.03703363422075456e-1, -1.34118505705967765e-2,
      9.04033494028101968e-3, -2.42259538436268176e-3,
      9.15785997288933120e-4, -2.96890121633200000e-4,
      1.00928148823365120e-4, -3.36375833240268800e-5,
      1.12524642975590400e-5, -3.75499034136576000e-6,
      1.25281466396672000e-6, -4.17808776355840000e-7,
      1.39383522590720000e-7, -4.64774927155200000e-8,
      1.53835215257600000e-8, -5.11961333760000000e-9,
      1.82243164160000000e-9, -6.13513953280000000e-10,
      1.27679856640000000e-10,-4.01499750400000000e-11,
      4.26560716800000000e-11,-1.46381209600000000e-11);

var
  tvsmall, t, g: TJmFloat;
  m, i: TJmInteger;
  af : TJmFloat;
begin
  if sizeof(TJmFloat) = 6
  then
    tvsmall:=2*midget
  else
    tvsmall:=midget;
  t:=abs(x);
  if t > tmax
  then
    raise Exception.Create('RunError(407)');
  if t < macheps
  then
    begin
      if t < tvsmall
      then
        raise Exception.Create('RunError(407)');
      spegam:=1/x
    end
  else  { abs(x) >= macheps }
    begin
      m:=trunc(x);
      if x > 0
      then
        begin
          t:=x-m; m:=m-1; g:=1;
          if m<0
          then
            g:=g/x
          else
            if m>0
            then
              for i:=1 to m do
                g:=(x-i)*g
        end
      else { x < 0 }
        begin
          t:=x-m+1;
          if t=1
          then
            raise Exception.Create('RunError(407)');
          m:=1-m;
          g:=x;
          for i:=1 to m do
            g:=(i+x)*g;
          g:=1/g
        end;
      af := a[0];
      spegam:=spepol(2*t-1, af, sizeof(a) div sizeof(TJmFloat) - 1)*g
    end { abs(x) >= macheps }
end; {spegam}
{$endif ToDo}

Procedure ExprInternalError(A,B:TJmInteger);

VAR S,S2 : String;

begin
 Str(ORD(A),S);
 Str(ORD(B),S2);
 Raise EExprIE.Create(SExprIE+S+' '+S2);
end;

CONSTRUCTOR TJmExpression.Create(Infix:String);

var dummy:String;

begin
 ExprTree:=NIL;

 if (Infix<>'') then
  ExprTree:=InfixToParseTree(Infix,Dummy);
 InfixCache:=Infix;
 InfixClean:=True;                      {Current pnode status' infix
                                            representation is in infixcache}
end;

constructor TJmExpression.EmptyCreate;
begin
 ExprTree:=Nil;
 InfixClean:=false;
end;

Procedure TJmExpression.SetNewInfix(Infix:String);

var dummy:String;

begin
 if Assigned(ExprTree) Then
  Dispose(ExprTree);
 if infix<>'' then
  ExprTree:=InFixToParseTree(Infix,Dummy)
 else
  ExprTree:=NIL;
 InfixClean:=True;
 InfixCache:=Infix;
end;

Destructor TJmExpression.Destroy;

begin
 If assigned(ExprTree) then
  DisposeExpr(ExprTree);
 inherited Destroy;
end;

function TJmExpression.GetRPN :String;

begin
 if ExprTree=NIL Then
  Result:='0'
 else
  Result:=ParseTreeToRpn(ExprTree);
end;

function TJmExpression.GetInfix:String;

begin
 if Not InfixClean then
  begin
   If ExprTree=NIL THEN
    InfixCache:='0'
   else
    InfixCache:=ParseTreeToInfix(ExprTree);
   InfixClean:=True;
  end;
 Result:=InfixCache;
end;

Function  TJmExpression.GetIntValue:LongInt;

begin
 SimplifyConstants;
 If ExprTree^.NodeType<>Iconstnode then
  Raise ENotInt.Create(SExprNotInt);
 result:=ExprTree^.ivalue;
end;

Procedure TJmExpression.SetIntValue(val:Longint);

begin
 if ExprTree<> NIL then
  DisposeExpr(ExprTree);
 New(ExprTree);
 ExprTree^.NodeType:=iconstnode;
 ExprTree^.Ivalue:=Val;
end;

Function  TJmExpression.GetFloatValue:TJmFloat;

begin
 If ExprTree^.NodeType<>constnode then
  Raise ENotFloat.Create(SExprNotFloat);
 result:=ExprTree^.value;
end;

Procedure TJmExpression.SetFloatValue(val:TJmFloat);

begin
 if ExprTree<> NIL then
  DisposeExpr(ExprTree);
 New(ExprTree);
 ExprTree^.NodeType:=constnode;
 ExprTree^.value:=Val;
end;

procedure TJmExpression.Simpleop(expr:TJmExpression;oper:calcop);

var
  tmp : pnode;
begin
  new(tmp);
  tmp^.nodetype:=calcnode;
  tmp^.op:=oper;
  tmp^.left:=exprtree;
  tmp^.right:=CopyTree(expr.exprtree);
  exprtree:=tmp;
  InFixCache:='garbadge';
  InfixClean:=False;
end;

function TJmExpression.Simpleopwithresult(expr:TJmExpression;oper:calcop):TJmExpression;
var
  tmp:pnode;
begin
  result := nil;
  new(tmp);
  tmp^.nodetype:=calcnode;
  tmp^.op:=oper;
  tmp^.left:=copytree(exprtree);
  tmp^.right:=CopyTree(expr.exprtree);
  result.EmptyCreate;
  result.SimplificationLevel:=simplificationlevel;
  result.exprtree:=tmp;
end;

procedure TJmExpression.Addto(Expr:TJmExpression);

begin
 simpleop(expr,addo);
end;


procedure TJmExpression.SubFrom(Expr:TJmExpression);

begin
 simpleop(expr,subo);
end;

procedure TJmExpression.Times(Expr:TJmExpression);

begin
 simpleop(expr,mulo);
end;

procedure TJmExpression.Divby(Expr:TJmExpression);

begin
 simpleop(expr,dvdo);
end;


procedure TJmExpression.RaiseTo(Expr:TJmExpression);

begin
 simpleop(expr,powo);
end;


function TJmExpression.add(Expr:TJmExpression):TJmExpression;


begin
 result:=Simpleopwithresult(expr,addo);
end;

function TJmExpression.sub(Expr:TJmExpression):TJmExpression;


begin
 result:=Simpleopwithresult(expr,subo);
end;

function TJmExpression.dvd(Expr:TJmExpression):TJmExpression;


begin
 result:=Simpleopwithresult(expr,dvdo);
end;


function TJmExpression.mul(Expr:TJmExpression):TJmExpression;


begin
 result:=Simpleopwithresult(expr,mulo);
end;

Function  TJmExpression.IntDerive(const derivvariable:String;theexpr:pnode):pnode;

function Deriv(t:pnode):pnode;
{Derive subexpression T. Returns NIL if subexpression derives to 0, to avoid
unnecessary (de)allocations. This is the reason why NewCalc is so big.}

var x     : TJmFloat;
    p1,p2 : pnode;

begin
  Deriv:=nil;
  if (t=nil) then               {Early out}
   exit;
  with t^ do begin
   case nodetype of
     VarNode:    if UpperCase(variable)=derivvariable then
                  Deriv:=NewiConst(TJmInteger(1))
                 else
                  Deriv:=NIL;
     ConstNode : Deriv:=NIL;
     IConstNode: Deriv:=NIL;
     CalcNode:  begin
                  case op of
                   addo,
                   subo:  Deriv:=NewCalc(op,Deriv(left),Deriv(right));
                   mulo:  Deriv:=NewCalc(addo,
                                 NewCalc(mulo,Deriv(left),copyTree(right)),
                                 NewCalc(mulo,Deriv(right),copytree(left)));

                   dvdo:  Deriv:=NewCalc(dvdo,
                                  NewCalc(subo,
                                   NewCalc(mulo,Deriv(left),copyTree(right)),
                                   NewCalc(mulo,Deriv(right),copytree(left))),
                                   NewFunc(sqrx,CopyTree(right)));
                  powo: begin
                         p1:=Deriv(Right);
                         if P1<>NIL then
                          p1:=NewCalc(mulo,p1,NewFunc(Lnx,CopyTree(Left))); { ln(l)*r'}
                         p2:=Deriv(Left);
                         if P2<>NIL then
                          p2:=Newcalc(Mulo,CopyTree(Right),newcalc(mulo,p2,
                                newfunc(invx,CopyTree(left))));
                         IF (P1<>nil) and (p2<>nil) then
                          deriv:=newcalc(mulo,CopyTree(t),newcalc(addo,p1,p2))
                         else
                          if (P1=NIL) and (P2=NIL) then {Simplify first to avoid this!}
                           deriv:=NIL
                          else
                           begin
                            if P1=NIL THEN { one of both is constant}
                             P1:=P2;       {The appopriate term is now in P1}
                            deriv:=newcalc(mulo,CopyTree(t),p1);
                           end;
                        end;
                     end;
                  end;
     FuncNode: begin
                 case fun of
                   invx:  Deriv:=NewCalc(dvdo,
                                  NewFunc(Minus,NewCalc(mulo,Deriv(right),copytree(left))),
                                   NewFunc(sqrx,CopyTree(right)));
                   minus: Deriv:=NewFunc(minus,Deriv(son));
                   sinx:  Deriv:=NewCalc(Mulo,
                            NewFunc(cosx,Copytree(son)),
                            Deriv(son));
                   cosx:  deriv:=NewCalc(mulo,
                            NewFunc(minus,NewFunc(sinx,copytree(son))),
                            Deriv(son));
                   tanx:  deriv:=Newcalc(dvdo,deriv(son),
                            newfunc(sqrx,newfunc(cosx,copytree(son))));
                   sqrx:  deriv:=newcalc(mulo, newiconst(2),
                                  newcalc(mulo,copytree(son),deriv(son)));
                                  { dx*1 /(2*sqrt(x)) }
                   sqrtx: deriv:=newcalc(mulo, deriv(son),newcalc(dvdo,newiconst(1),
                                       newcalc(mulo,newiconst(2),newfunc(sqrtx,copytree(son)))));
                   lnx :  deriv:=newcalc(mulo,newcalc(dvdo,newiconst(1),CopyTree(son)),
                                             deriv(son)); { dln(x)=x' * 1/x}
                   expx:  deriv:=newcalc(mulo,newfunc(expx,copytree(son)),deriv(son));
                 cotanx:  deriv:=newfunc(minus,Newcalc(dvdo,deriv(son),  { -dx/sqr(sin(x))}
                            newfunc(sqrx,newfunc(sinx,copytree(son)))));
                  coshx:  deriv:=newcalc(mulo,newfunc(sinhx,copytree(son)),deriv(son));
                  sinhx:  deriv:=newcalc(mulo,newfunc(coshx,copytree(son)),deriv(son));
               arcsinhx, {according to HP48?}
                arcsinx:  deriv:=newcalc(dvdo,deriv(son),newfunc(sqrtx,newcalc(subo,newiconst(1),
                                newfunc(sqrx,copytree(son)))));
                arccosx:  deriv:=newfunc(minus,newcalc(dvdo,deriv(son),
                                        newfunc(sqrtx,newcalc(subo,newiconst(1),newfunc(sqrx,copytree(son))))));
                arctanx:  deriv:=newcalc(dvdo,deriv(son),newcalc(addo,newiconst(1),newfunc(sqrx,copytree(son))));
                 log10x:  deriv:=newcalc(mulo,newcalc(dvdo,newconst(0.434294481902),CopyTree(son)),
                                             deriv(son)); { dlog10(x)=x' * log10(e)/x}
                  log2x:  deriv:=newcalc(mulo,newcalc(dvdo,newconst(1.44269504089),CopyTree(son)),
                                             deriv(son)); { dlog2(x)=x' * log2(e)/x}
                  stepx:   ;  {Should raise exception, not easily derivatable}
                  tanhx:  deriv:=newcalc(dvdo,deriv(son),newfunc(sqrx,newfunc(coshx,copytree(son))));
               arctanhx:  deriv:=newcalc(dvdo,deriv(son),newfunc(sqrtx,newcalc(addo,newiconst(1),
                                           newfunc(sqrx,copytree(son)))));
               arccoshx:  deriv:=NewCalc(dvdo,deriv(son),newcalc(mulo,newcalc(subo,newfunc(sqrtx,copytree(son)),newiconst(1)),
                                                                    newcalc(addo,newfunc(sqrtx,copytree(son)),newiconst(1))));
     lnxpix,arctan2x,
     hypotx,lognx : ; {Should also raise exceptions, not implemented yet}
                    end;
                  end;
     Func2Node: begin
                  if son2left^.nodetype=constnode then
                   x:=son2left^.value
                  else
                   x:=son2left^.ivalue;
                  case fun of
                 lognx:  deriv:=newcalc(mulo,newcalc(dvdo,newconst(logn(x,2.71828182846)),
                                 CopyTree(son2right)),deriv(son2right));
                                 { dlogn(x)=x' * log(n,e)/x}
                 Powerx: begin
                         p1:=Deriv(Son2Right);
                         if P1<>NIL then
                          p1:=NewCalc(mulo,p1,NewFunc(Lnx,CopyTree(Son2Left))); { ln(l)*r'}
                         p2:=Deriv(Son2Left);
                         if P2<>NIL then
                          p2:=Newcalc(Mulo,CopyTree(Son2Right),newcalc(mulo,p2,
                                newfunc(invx,CopyTree(Son2Left))));
                         IF (P1<>nil) and (p2<>nil) then
                          deriv:=newcalc(mulo,CopyTree(t),newcalc(addo,p1,p2))
                         else
                          if (P1=NIL) and (P2=NIL) then {Simplify first to avoid this!}
                           deriv:=NIL
                          else
                           begin
                            if P1=NIL THEN { one of both is constant}
                             P1:=P2;       {The appopriate term is now in P1}
                            deriv:=newcalc(mulo,CopyTree(t),p1);
                           end;
                        end;
                     end;
                end;
           end;
    end; {WITH}
end;

begin
 Result:=Deriv(theexpr);
end;

function TJmExpression.power(Expr:TJmExpression):TJmExpression;


begin
 result:=Simpleopwithresult(expr,powo);
end;


Function  TJmExpression.Derive(derivvariable:String) : TJmExpression;
var
  tmpvar : Pnode;
  DerivObj: TJmExpression;

begin
 derivvariable:=UpperCase(derivvariable);
 Tmpvar:=intDerive(derivvariable,exprtree);

 DerivObj:=TJmExpression.emptycreate;
 If tmpvar=NIL then
  derivobj.ExprTree:=NewIconst(0)
 else
  derivobj.exprtree:=tmpvar;
 derivobj.simplificationlevel:=simplificationlevel;
 DerivObj.InfixClean:=False;
 result:=derivobj;
end;

function ipower(x,y:TJmInteger):TJmInteger;

var tmpval : TJmInteger;

begin
 if y<0 then
  ; {exception}
 if y=0 then
   result:=1
 else
  begin
   result:=x;
   if y<>1 then
    for tmpval:=2 to y do
     result:=result*x;
  end;
end;

function ifacul(x:TJmInteger):TJmInteger;

var tmpval : TJmInteger;

begin
 if x<0 then
  ; {exception}
 if x=0 then
   result:=1
 else
  begin
   result:=1;
   if x<>1 then
    for tmpval:=2 to x do
     result:=result*tmpval;
  end;
end;

function EvaluateFunction(funcname:funcop;param:TJmFloat):TJmFloat;

//var Intermed : integer;

begin
  result := 0;
      case funcname of
       cosx : result:=Cos(param);
       sinx : result:=sin(param);
       tanx : result:=tan(param);
       sqrx : result:=sqr(param);
      sqrtx : result:=sqrt(param);
       expx : result:=exp(param);
        lnx : result:=ln(param);
     cotanx : result:=cotan(param);
    arcsinx : result:=arcsin(param);
    arccosx : result:=arccos(param);
    arctanx : result:=arctan(param);
      sinhx : result:=sinh(param);
      coshx : result:=cosh(param);
      tanhx : result:=tanh(param);
   arcsinhx : result:=arcsinh(param);
   arccoshx : result:=arccosh(param);
   arctanhx : result:=arctanh(param);
     log10x : result:=log10(param);
      log2x : result:=log2(param);
     lnxpix : result:=lnxp1(param);
{$ifdef ToDo}
     faculx : result:=spegam(param+1.0);
{$endif ToDo}
         else
          ExprInternalError(2,ord(funcname));
      end;
   If Result<1E-4900 then {Uncertainty in sinus(0.0)}
    Result:=0;
end;


procedure TJmExpression.SimplifyConstants;

procedure internalsimplify (expr:pnode);

function isconst(p:pnode):boolean;

begin
 isconst:=(p^.nodetype=iconstnode) or (p^.nodetype=constnode);
end;

function isconstnil(p:pnode):boolean;
begin
 IsConstNil:=false;
 if (p^.nodetype=iconstnode) and (P^.ivalue=0) then
  IsConstNil:=True;
 If (p^.nodetype=constnode) and (P^.value=0) then
  IsConstNil:=True
end;

var val1,val2 : TJmFloat;
    ival1,
    ival2 : TJmInteger;

function setupoperation(operat:calcop;simlevel:longint;Postprocess:boolean;param2func:boolean):longint;

function dosimple(mode:longint;theleft,theright:pnode):longint;

begin
 If Mode >3 then
  ;
 result:=0;
 if mode=0 then
  exit;
        if (theright^.nodetype=iconstnode) and (theleft^.nodetype=iconstnode) then
         begin
          if mode=3 then
           begin
            result:=2;
             val2:=theright^.value;
             val1:=theleft^.value;
           end
          else
           begin
            result:=1;
            ival2:=theright^.ivalue;
            ival1:=theleft^.Ivalue;
           end;
         end;
        if (theright^.nodetype=constnode) and (theleft^.nodetype=constnode) then
         begin
          result:=2;
          val2:=theright^.value;
          val1:=theleft^.value;
         end;
    if mode>1 then
     begin
       if result=0 then
        begin
         if (theright^.nodetype=constnode) and (theleft^.nodetype=iconstnode) then
         begin
          result:=3;
          val2:=theright^.value;
          val1:=theleft^.ivalue;
         end;
        if (theright^.nodetype=iconstnode) and (theleft^.nodetype=constnode) then
         begin
          result:=4;
          val2:=theright^.ivalue;
          val1:=theleft^.value;
         end;
      end;
    end;
end;

begin
 Result:=0;
 if SimplificationLevel<>0 then
  if param2func then
   result:=DoSimple(SimLevel,expr^.son2left,expr^.son2right)
  else
   result:=DoSimple(SimLevel,expr^.left,expr^.right);

 with expr^ do
  begin
 IF (result>0) and PostProcess then
  begin
   if (operat<>dvdo) then { Divide is special case. If
                                           integer x/y produces a fraction
                                           we want to be able to roll back}
    begin
     if Param2func then
      begin
       dispose(son2right);
       dispose(son2left);
      end
     else
      begin
       dispose(right);
       dispose(left);
      end;
     if result=1 then
      nodetype:=iconstnode
     else
      nodetype:=constnode;
    end;
   end;
  end;
end;

procedure Checkvarnode(p:pnode);

var treal:TJmFloat;
    error:integer;
    tint :Integer;

begin
   TrimLeft(P^.variable);
   TrimRight(p^.variable);
   Val(p^.variable, treal, Error);
   IF (error=0) then {Conversion to real succeeds. Numeric}
    begin
      if (Pos('.',p^.variable)=0) and (Pos('E',p^.variable)=0) Then
       begin
        Val(p^.variable,tint,Error);
        If error=0 then
         begin
          p^.nodetype:=iconstnode;
          p^.ivalue:=tint;
         end
        else
         begin
          p^.nodetype:=constnode;
          p^.value:=treal;
         end;
       end
      else
       begin
        p^.nodetype:=constnode;
        p^.value:=treal;
       end;
    end;
end;

var tmpval : TJmInteger;
    invdummy: pnode;

begin
 case expr^.nodetype of
  VarNode  : CheckVarnode(expr);   {sometimes a numeric value can slip in as constant.
                        (e.g. as people pass it as symbol to taylor or
                        "subst" methods}

  calcnode : begin
            internalsimplify(expr^.left);
            internalsimplify(expr^.right);
            if isconst(expr^.left) and isconst(expr^.right) then
             begin
              TmpVal:=Setupoperation(expr^.op,SimplificationLevel,true,false);
              if tmpval>0 then
               with expr^ do
               case op of
                addo :
                       if tmpval=1 then
                        ivalue:=ival1+ival2
                       else
                        value:=val1+val2;
               subo  : if tmpval=1 then
                        ivalue:=ival1-ival2
                       else
                        value:=val1-val2;
               mulo  : if tmpval=1 then
                        ivalue:=ival1*ival2
                       else
                        value:=val1*val2;

               dvdo  : if tmpval=1 then
                        begin
                         tmpval:=ival1 div ival2;
                         if (tmpval*ival2)=ival1 then {no rounding, OK!}
                          begin
                           Dispose(expr^.right);
                           Dispose(Expr^.left);
                           nodetype:=iconstnode;
                           ivalue:=tmpval;
                          end; {ELSE do nothing}
                        end
                        else
                         begin
                          dispose(expr^.right);
                          dispose(expr^.left);
                          nodetype:=constnode;
                          value:=val1 / val2;
                        end;
               powo :   If tmpval=1 then
                        begin
                         if ival2<0 then {integer x^-y -> inv (x^y)}
                          begin
                           new(invdummy);
                           invdummy^.nodetype:=iconstnode;
                           invdummy^.ivalue:=ipower(ival1,-ival2);
                           expr^.nodetype:=funcnode;
                           expr^.son:=invdummy;
                          end
                         else
                            ivalue:=ipower(ival1,ival2);
                        end
                      else
                        value:=exp(val2*ln(val1));
                 else
                  ExprInternalError(1,ord(Expr^.op));
                end; {case}
            end {if}
           else {At least one node is symbolic, or both types are wrong}
            begin
             With Expr^ do
              if IsConstNil(Left) then
               begin
                Dispose(Left);
               case op of
                addo : begin
                        InvDummy:=Right;
                        Expr^:=Right^;
                        Dispose(InvDummy);
                       end;
                subo: begin
                        invdummy:=right;
                        NodeType:=funcNode;
                        Fun:=Minus;
                        son:=invdummy;
                      end;
           mulo,powo,dvdo : begin
                        Dispose(Right);
                        nodetype:=IconstNode;
                        ivalue:=0;
                       end;
                   end;
                 end
               else
                if IsConstNil(Right) then
                 begin
                  if expr^.op<>dvdo then {Leave tree for DVD intact because of exception}
                   Dispose(Right);
                 case expr^.op of
       addo,subo : begin
                    InvDummy:=left;
                    Expr^:=left^;
                    Dispose(InvDummy);
                   end;
           mulo  : begin
                    Dispose(Left);
                    nodetype:=IconstNode;
                    ivalue:=0;
                   end;
           powo  : begin
                    Dispose(Left);
                    nodetype:=IconstNode;
                    ivalue:=1;
                   end;
           dvdo  : Raise EDiv0.Create(SExprInvSimp);
             else
                 ExprInternalError(6,ord(Expr^.op));
              end;
            end;
          end;
          end; {case calcnode}

  funcnode: begin
             internalSimplify(expr^.son);
             Case Expr^.fun of
              Minus : if IsConst(expr^.son) then
                       begin
                        InvDummy:=Expr^.Son;
                        expr^:=InvDummy^;
                        if InvDummy^.Nodetype=IconstNode then
                         expr^.ivalue:=-expr^.ivalue
                        else
                         expr^.value:=-expr^.value;
                        dispose(InvDummy);
                       end;
             invx   : begin
                       InvDummy:=Expr^.son;
                       If InvDummy^.nodeType=ConstNode Then
                        begin
                         if InvDummy^.Value=0.0 then
                          Raise EDiv0.Create(SExprInvMsg);
                         Expr^.NodeType:=ConstNode;
                         Expr^.Value:=1/InvDummy^.Value;
                         Dispose(InvDummy);
                        end
                       else
                        if InvDummy^.nodetype=iconstnode then
                         begin
                          if InvDummy^.iValue=0 then
                           Raise EDiv0.Create(SExprinvmsg);
                          If (InvDummy^.iValue=1) or (InvDummy^.iValue=-1) then
                           begin
                            expr^.NodeType:=Iconstnode;
                            Expr^.iValue:=InvDummy^.iValue;
                            Dispose(InvDummy);
                           end;
                         end;
                      end;
                 else {IE check in EvaluateFunction}
                  if (expr^.son^.nodetype=constnode) and (Expr^.fun<>faculx) then {Other functions, only func(real) is simplified}
                   begin
                    val1:=EvaluateFunction(expr^.fun,Expr^.son^.value);
                    dispose(expr^.son);
                    expr^.nodetype:=constnode;
                    expr^.value:=val1;
                   end;
                end; {Case 2}
           end;

  Func2Node : begin
               internalSimplify(expr^.son2left);
               internalSimplify(expr^.son2right);
               case expr^.fun2 of
                powerx : begin
                        TmpVal:=Setupoperation(powo,SimplificationLevel,true,true);
                        if TmpVal>1 then
                         begin
                          If tmpval=1 then
                           begin
                            if ival2<0 then {integer x^-y -> inv (x^y)}
                             begin
                              new(invdummy);
                              invdummy^.nodetype:=iconstnode;
                              invdummy^.ivalue:=ipower(ival1,-ival2);
                              expr^.nodetype:=funcnode;
                              expr^.son:=invdummy;
                             end
                           else
                            expr^.ivalue:=ipower(ival1,ival2);
                           end;
                          end;
                       end;
               stepx : begin
                       {N/I yet}
                       end;
             arctan2x : begin
                         TmpVal:=Setupoperation(powo,SimplificationLevel,false,true);
                         if tmpval>1 then {1 is integer, which we don't do}
                          begin
                           dispose(expr^.right);
                           dispose(expr^.left);
                           expr^.nodetype:=constnode;
                           expr^.value:=arctan2(ival2,ival1);
                          end;
                        end;
             hypotx   :begin
                         TmpVal:=Setupoperation(powo,SimplificationLevel,false,true);
                         if tmpval>1 then {1 is integer, which we don't do}
                          begin
                           dispose(expr^.right);
                           dispose(expr^.left);
                           expr^.nodetype:=constnode;
                           expr^.value:=hypot(ival2,ival1);
                          end;
                        end;
           lognx:      begin
                         TmpVal:=Setupoperation(powo,SimplificationLevel,false,true);
                         if tmpval>1 then {1 is integer, which we don't do}
                          begin
                           dispose(expr^.right);
                           dispose(expr^.left);
                           expr^.nodetype:=constnode;
                           expr^.value:=hypot(ival2,ival1);
                          end;
                        end;
               else
                ExprInternalError(3,ORD(expr^.fun2));
             end;
            end;
{         else
           ExprInternalError(4,ORD(expr^.nodetype));}
       end; {Case 1}
end;

begin
 internalsimplify(exprtree);
 InfixClean:=False; {Maybe changed}
end;

procedure TJmExpression.SymbolSubst(ToSubst,SubstWith:String);

procedure InternalSubst(expr:Pnode);

begin
 if Expr<>NIL THEN
  case Expr^.NodeType of
   VarNode:   if Expr^.Variable=ToSubst then
               Expr^.Variable:=SubstWith;
   calcnode:  begin
               InternalSubst(Expr^.left);
               InternalSubst(Expr^.right);
              end;
   funcnode:  InternalSubst(Expr^.son);
   func2node: begin
               InternalSubst(Expr^.son2left);
               InternalSubst(Expr^.son2right);
              end;
            end;
end;

begin
 InternalSubst(ExprTree);
end;

function TJmExpression.SymbolicValueNames:TStringList;

var TheList:TStringList;

procedure InternalSearch(expr:Pnode);

begin
 if Expr<>NIL THEN                      {NIL shouldn't be allowed, and signals corruption. IE? Let it die?}
  case Expr^.NodeType of
   VarNode:  begin
              Expr^.Variable:=UpperCase(Expr^.Variable);
              TheList.Add(Expr^.Variable);
             end;
   calcnode:  begin
               InternalSearch(Expr^.left);
               InternalSearch(Expr^.right);
              end;
   funcnode:  InternalSearch(Expr^.son);
   func2node: begin
               InternalSearch(Expr^.son2left);
               InternalSearch(Expr^.son2right);
              end;
            end;
end;

begin
 TheList:=TStringList.Create;
 TheList.Sorted:=TRUE;
 Thelist.Duplicates:=DupIgnore;
 InternalSearch(ExprTree);
 Result:=TheList;
end;

function TJmExpression.Taylor(Degree:TJmInteger;const x,x0:String):TJmExpression;
{Taylor(x,x0)=sum(m,0,degree, d(f)/d(x))(x0)/ m! * (x-x0)^m)

=   f(x0)+ (x-x0)/1! * df/d(x) + (x-x0)^2  /  2! * d^2(f)/d^2(x) +
       (x-x0)^3  /  3! * d^3(f)/d^3(x) + ....
}

Var
  TaylorPol    : TJmExpression;   { The result expression}
  Root,
  Tmp,Tmp2,
  tmp3,tmp4,tmp5 : pnode;       { Always have a nice storage of pointers.
                                  Used to hold all intermediate results}
  I,facul        : TJmInteger;      { Loop counters and faculty term}

begin
 TaylorPol:=TJmExpression.EmptyCreate;      {New expression}
 TaylorPol.ExprTree:=CopyTree(ExprTree);  {make a copy of the parsetree}
 TaylorPol.SymbolSubst(X,X0);             {subst x by x0. All occurances
                                          of  f() refer to x0, not x}
 if Degree>0 then                         {First term only? Or nonsense (negative?)}
                                          {Then ready. 0th term only.}
  begin                                   {Start preparing easy creation of higher terms}
   tmp2:=newcalc(subo,newvar(x),
                      newvar(x0));        {tmp2= x-x0 needed separately for first term}
   tmp4:=Newiconst(5);                    {exponent for x-x0, "a" need to keep a reference}
   tmp3:=newcalc(powo,tmp2,tmp4);         {tmp3= (x-x0)^a}
   tmp5:=newiconst(5);                    {faculty value, "b"=m! also keep a reference for later modification }
   tmp3:=Newcalc(dvdo,tmp3,tmp5);         {tmp3=  (x-x0)^a / b    a and b can be changed}
   facul:=1;                              {Calculate faculty as we go along. Start with 1!=1}
   root:=TaylorPol.ExprTree;              {0th term}
   tmp:=root;                             {term that needs to be differentiated per iteration}
   for i:=1 to Degree do
    begin
     facul:=Facul*i;                      {Next faculty value 1!*1 =1 btw :_)}
     tmp:=intderive(x0,tmp);              {Differentiate f^n(x0) to f^(n+1)(x0)}
     If I=1 then                          {first term is special case. No power }
       tmp2:=NewCalc(mulo,CopyTree(tmp2),tmp) {or faculty needed (^1 and 1!)}
      else
       begin
        tmp5^.Ivalue:=facul;              {Higher terms. Set faculty}
        tmp4^.ivalue:=i;                  {Set exponent (=a) of (x-x0)^a}
        tmp2:=NewCalc(mulo,CopyTree(tmp3),tmp); {multiplicate derivative with (x-x0)^a/b term}
       end;
      root:=NewCalc(addo,root,tmp2);      {String all terms together}
     end;
   DisposeExpr(tmp3);                     {Is only CopyTree()'d, not in new expression}
   TaylorPol.Exprtree:=root;              {Set result}
  end;
 Result:=TaylorPol;
end;

function TJmExpression.Newton(x:String):TJmExpression;
{
             f(x)
Newton(x)=x- ----
             f'(x);
Taylor(x,x0
}

Var
  NewtonExpr     : TJmExpression;   { The result expression}
//  Root,
  Tmp,Tmp2,
  tmp3 {,tmp4,tmp5} : pnode;       { Always have a nice storage of pointers.
                                  Used to hold all intermediate results}
  // {I,} facul        : TJmInteger;      { Loop counters and faculty term}

begin
 NewtonExpr:=TJmExpression.EmptyCreate;      {New expression}

 {Should I test for constant expr here?}

 Tmp:=CopyTree(ExprTree);                 {make a copy of the parsetree
                                            for the f(x) term}
 Tmp2:=intDerive(x,tmp);                  { calc f'(x)}
 Tmp3:=NewVar(x);                         { Create (x)}
 Tmp:=Newcalc(dvdo,tmp,tmp2);             { f(x)/f'(x)}
 tmp:=Newcalc(subo,tmp3,tmp);             { x- f(x)/f'(x)}

 {We built the above expression using a copy of the tree.
     So no pointers into self.exprtree exist. We can now safely assign it
     to exprtree}
 NewtonExpr.ExprTree:=tmp;
 NewtonExpr.SimplifyConstants;           {Simplify if f'(x)=constant, and
                                          kill 0*y(x) terms}
 Result:=NewtonExpr;
end;

Procedure TEvalInternalError(A,B:TJmInteger);
var
  S,S2 : String;
begin
  Str(ORD(A),S);
  Str(ORD(B),S2);
  Raise TJmEvaluatorIE.Create(SEvalIE+S+' '+S2);
end;


CONSTRUCTOR TJmEvaluator.Create(VariableList:TStringList;Expression:pnode);
{Constructor. Stringlist to set the order of variables in the function while
xconverting the pnode tree to a TEvaluator structure. This avoids any string
parsing during a real evaluation, and moves all stringparsing to the setup.

So for Func(x,y,z) Variablelist contains ('x','y','z') in that order.
}

begin
 VariableName:=VariableList;
 ConstantNames:=TStringList.Create;
 ConstantValue:=TList.Create;
 Getmem(VLIWRPnExpr,SIZEOF(VLIWEvalWord)*VLIWIncr);
 VLIWCount:=0;
 VLIWAlloc:=VLIWIncr;
 MaxStack :=0;
 TreeToVLIWRPN(Expression);
end;

CONSTRUCTOR TJmEvaluator.Create(VariableList:TStringList;Expression:TJmExpression);
{Overloaded, same as other constructor. (which it even calls), except that
it has a TJmExpression as argument.

Besides that it gets the pnode from the TJmExpression, it sets the
TJmExpression.Evaluator to self, and a flag to set in the TJmExpression that its
assiociated TEvaluator is up to date with the TJmExpression.
}

begin
 Self.Create(VariableList,Expression.ExprTree);
 Expression.Evaluator:=Self;
 Expression.EvaluatorUpToDate:=TRUE;
end;

DESTRUCTOR TJmEvaluator.Destroy;

VAR I       : LONGINT;
    TmpList : Tlist;

begin
 VariableName.Free;
 ConstantNames.Free;
 IF ConstantValue.Count>0 THEN
  FOR I:=0 to ConstantValue.Count -1 DO
   begin
    TmpList:=TList(ConstantValue[I]);
    TmpList.Free;
   end;
 ConstantValue.Free;
 If VLIWAlloc>0 THEN
  FreeMem(VLIWRPNExpr,VLIWAlloc*SIZEOF(VLIWEvalWord));
 inherited Destroy;
end;

PROCEDURE   TJmEvaluator.SetConstant(Name:String;Value:TJmFloat);

Var Ind,I    : Longint;
    TmpList  : TList;

begin
 Ind:=ConstantNames.IndexOf(Name);
 If Ind<>-1 THEN
  begin
   TmpList:=TList(ConstantValue[Ind]);
   I:=TmpList.Count;
   If I>0 Then
    For I:=0 TO TmpList.Count-1 DO
     begin
      PVLIWEvalWord(TmpList[I])^.VLIWEntity:=AfConstant;
      PVLIWEvalWord(TmpList[I])^.Value:=Value;
     end;
   end;
end;

procedure TJmEvaluator.TreeToVLIWRPN(expr:pnode);

procedure CheckVLIWArr;

begin
 if VLIWCount=VLIWAlloc then
  begin
   ReAllocMem(VLIWRPNExpr,(VLIWAlloc+VLIWIncr)*SIZEOF(VLIWEvalWord));
   Inc(VLIWAlloc,VLIWIncr);
  end;
end;

procedure searchTree(Tree:pnode);

var Ind : TJmInteger;
    TmpList : TList;

begin
 if tree<>nil then
 case Tree^.nodetype of
  VarNode  : begin
              {some variable or constant. First: Variable?}
               Ind:=VariableName.IndexOf(Tree^.Variable);
               If Ind<>-1 then
                begin {We have identified a variable}
                 CheckVLIWArr; {Make sure there is at least room for one variable}
                 WITH VLIWRPNExpr[VLIWCount] do
                  begin
                   VLIWEntity:=AVariable;
                   IndexOfVar:=Ind;
                  end;
                  {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}
                  inc(VLIWCount);
                end
               else
                begin {We have a constant}
                 ind:=ConstantNames.IndexOf(Tree^.Variable);
                 if Ind=-1 then
                  begin {That doesn't exist. Make sure it exists}
                   ConstantNames.Add(Tree^.Variable);
                   TmpList:=TList.Create;
                   ConstantValue.Add(TmpList);
                  end
                 else
                  begin
                   TmpList:=tlist(ConstantValue[Ind]);
                  end;

                 {Create the VLIW record}
                 CheckVLIWArr;

                 WITH VLIWRPNExpr[VLIWCount] do
                  begin
                   VLIWEntity:=Placeholder;
                   IndexOfConstant:=255;
                  end;
                  {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

                 {Store a pointer to the VLIW record to be able to change the
                  constant}
                 TmpList.Add(pointer(VLIWCount)); {Can't pick pointer here, due to realloc}
                 inc(VLIWCount);
                end; {Ind<>-1}
              end;
  ConstNode: begin

              CheckVLIWArr;
              WITH VLIWRPNExpr[VLIWCount] do
               begin
                VLIWEntity:=AfConstant;
                Value:=tree^.value;
               end;
               {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

              inc(VLIWCount);
             end;
 iconstnode: begin
               CheckVLIWArr;
              WITH VLIWRPNExpr[VLIWCount] do
               begin
                VLIWEntity:=AiConstant;
                IValue:=tree^.ivalue;
               end;
               {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

              inc(VLIWCount);
             end;
 CalcNode  : begin

              CheckVLIWArr;
              WITH VLIWRPNExpr[VLIWCount] do
               begin
                VLIWEntity:=AnOperation;
                op:=vliwop2(ord(Tree^.op));
               end;
               {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

              inc(VLIWCount);
              SearchTree(tree^.left);
              SearchTree(tree^.right);
             end;
 FuncNode:    begin

               CheckVLIWArr;
               WITH VLIWRPNExpr[VLIWCount] do
                begin
                 VLIWEntity:=AFunction;
                 fun1:=Tree^.fun;
                end;
                {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

               inc(VLIWCount);
               SearchTree(tree^.son);

              end;
 Func2Node:   begin

               CheckVLIWArr;
               WITH VLIWRPNExpr[VLIWCount] do
                begin
                 VLIWEntity:=AnOperation;
                 if tree^.fun2=powerx then
                  op:=VLIWOp2(powo)
                 else
                  if tree^.fun2 >powerx then
                   op:=vliwop2(ord(powv)+ord(tree^.fun2)-ord(arctan2x))
                  else
                   op:=vliwop2(1+ord(powv)+ord(tree^.fun2)-ord(arctan2x))
                end;
                {$IFDEF DebugDump}
                  WriteVliw(VLIWRPNExpr[VLIWCOUNT]);
                  {$ENDIF}

               inc(VLIWCount);
               SearchTree(tree^.son2left);
               SearchTree(tree^.son2right);
              end
           else
            TEvalInternalError(4,ORD(Tree^.nodetype ));
    end;
end;

Procedure FixLists;
{We added constants as VLIWCount indexes. To speed up we convert them to
pointers. We couldn't do that directly as a consequence of the ReAlloc.}

VAR I,J : Longint;
    TmpList : TList;

begin
 I:=ConstantValue.Count;
 IF I>0 THEN
  FOR J:=0 TO I-1 DO
   begin
    TmpList:=TList(ConstantValue[J]);
    IF (Tmplist<>NIL) and (TmpList.Count>0) then
      for I:=0 TO TmpList.Count-1 DO
       TmpList[I]:=@VLIWRPNExpr[longint(TmpList[I])];
    end;
end;

begin
 VLIWCount:=0;
 SearchTree(expr);
 FixLists;
end;

function TJmEvaluator.Evaluate(const variables:Array of TJmFloat):TJmFloat;
{The one that does the work}

CONST StackDepth=50;

 var TheArray   : pVLIWEvalWord;
     VLIWRecs   : Longint;
     RPNStack   : ARRAY[0..StackDepth] OF TJmFloat;
//     I,
     RPNPointer : Longint;
//     S          : String;

  procedure push(Val:TJmFloat);
  begin
    if RPNPointer=StackDepth then
      raise TJmEvaluatorStackException.Create(SEvalStackDepthExceeded);
    RPNStack[RpnPointer]:=Val;
    inc(RPNPointer);
  end;

begin
  VLIWRecs:=VariableName.Count;
  if (High(Variables)+1)<>VLIWRecs then
    Raise TJmEvaluatorNotEnoughVariables.Create(SeValBadNumberOfVars);
  RPNPointer:=0;
  VliwRecs:=VliwCount-1;
//  TheArray:=@VLIWRPNExpr[VLIWRecs];
  repeat
    TheArray:=@VLIWRPNExpr[VLIWRecs];
    case TheArray^.VLIWEntity of
      AVariable :  begin
                     Push(Variables[TheArray^.IndexOfVar]);
                   end;

      AfConstant : begin
                     Push(TheArray^.Value);
                   end;
      AiConstant : begin
                     Push(TheArray^.iValue);
                   end;
      Placeholder: begin
                    writeln('placeholder');
                     RAISE TJmEvaluatorBadConstant.Create(ConstantNames[TheArray^.IndexOfConstant]);
                   end;
      AnOperation: begin
                Case TheArray^.Op of
                     addv : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=RPNStack[RPNPointer]+RPNStack[RPNPointer-1];
                            end;
                     subv : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=RPNStack[RPNPointer]-RPNStack[RPNPointer-1];
                            end;
                     mulv : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=RPNStack[RPNPointer]*RPNStack[RPNPointer-1];
                            end;
                     dvdv : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=RPNStack[RPNPointer]/RPNStack[RPNPointer-1];
                            end;
                     powv : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=Power(RPNStack[RPNPointer],RPNStack[RPNPointer-1]);
                            end;
                 arctan2v : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=ArcTan2(RPNStack[RPNPointer],RPNStack[RPNPointer-1]);
                            end;
                   stepv  : begin
                             Dec(RPNPointer);
                             If RPNStack[RPNPointer-1]>RPNStack[RPNPOINTER] THEN
                              RPNStack[RPNPointer-1]:=1.0
                             else
                              RPNStack[RPNPointer-1]:=0.0;
                            end;
                  hypotv  : begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=hypot(RPNStack[RPNPointer],RPNStack[RPNPointer-1]);
                            end;
                   lognv  :  begin
                             Dec(RPNPointer);
                             RPNStack[RPNPointer-1]:=logn(RPNStack[RPNPointer],RPNStack[RPNPointer-1]);
                            end;
                     else
                      TEvalInternalError(1,ORD(TheArray^.op));
                     end;
                   end;
        AFunction : begin

                    Case TheArray^.Fun1 of
                      cosx: RPNStack[RPNPointer-1]:=cos(RPNStack[RPNPointer-1]);
                      sinx: RPNStack[RPNPointer-1]:=sin(RPNStack[RPNPointer-1]);
                      tanx: RPNStack[RPNPointer-1]:=tan(RPNStack[RPNPointer-1]);
                      sqrx: RPNStack[RPNPointer-1]:=sqr(RPNStack[RPNPointer-1]);
                     sqrtx: RPNStack[RPNPointer-1]:=sqrt(RPNStack[RPNPointer-1]);
                      expx: RPNStack[RPNPointer-1]:=exp(RPNStack[RPNPointer-1]);
                       lnx: RPNStack[RPNPointer-1]:=ln(RPNStack[RPNPointer-1]);
                      invx: RPNStack[RPNPointer-1]:=1/RPNStack[RPNPointer-1];
                     minus: RPNStack[RPNPointer-1]:=-RPNStack[RPNPointer-1];
                    cotanx: RPNStack[RPNPointer-1]:=cotan(RPNStack[RPNPointer-1]);
                   arcsinx: RPNStack[RPNPointer-1]:=arcsin(RPNStack[RPNPointer-1]);
                   arccosx: RPNStack[RPNPointer-1]:=arccos(RPNStack[RPNPointer-1]);
                   arctanx: RPNStack[RPNPointer-1]:=arctan(RPNStack[RPNPointer-1]);
                     sinhx: RPNStack[RPNPointer-1]:=sinh(RPNStack[RPNPointer-1]);
                     coshx: RPNStack[RPNPointer-1]:=cosh(RPNStack[RPNPointer-1]);
                     tanhx: RPNStack[RPNPointer-1]:=tanh(RPNStack[RPNPointer-1]);
                  arcsinhx: RPNStack[RPNPointer-1]:=ArcSinh(RPNStack[RPNPointer-1]);
                  arccoshx: RPNStack[RPNPointer-1]:=ArcCosh(RPNStack[RPNPointer-1]);
                  arctanhx: RPNStack[RPNPointer-1]:=ArcTanh(RPNStack[RPNPointer-1]);
                    log10x: RPNStack[RPNPointer-1]:=Log10(RPNStack[RPNPointer-1]);
                     log2x: RPNStack[RPNPointer-1]:=Log2(RPNStack[RPNPointer-1]);
                    lnxpix: RPNStack[RPNPointer-1]:=lnxp1(RPNStack[RPNPointer-1]);
                    else
                     TEvalInternalError(2,ORD(TheArray^.fun1));
                   end;
                  end;
      else
       TEvalInternalError(3,ORD(TheArray^.VLIWEntity));
    end;
    //dec(TheArray);
    dec(VliwRecs);
  UNTIL VliwRecs<0;
  Result:=RPNStack[0];
end;

end.




