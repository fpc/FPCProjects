{
    $ id:                                                       $
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
     member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    Implementation of Infix to parsetree/RPN converter based on principles
    copied from a RPN constant expression evaluator by Trai Tran
    (PD, from SWAG.)

    Parsetree to infix and parsetree to RPN/infix conversion
     by Marco v/d Voort
    OOP interface and vast improvements by Marco v/d Voort

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Problems:
- -x^12 is -(x^12) or (-x)^12 ? (FIXED: Chose to do it as in FPC)
- No errorhandling. (will be rewritten to use classes and exceptions first)
  (this is partially done now)

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
 1. an operand (number)        Send it to the RPN calculator (argument stack)
 2. a left parenthesis         Push it onto the operation stack
 3. a right parenthesis        Pop operators off stack and send to RPN
                               calculator. Until the a left parenthesis is
                               on top of the stack.  Pop it also, but don't
                               send it to the calculator.
 4. an operator                While the stack is not empty, pop operators
                               off the stack and send them to the RPN
                               calculator Until you reach one With a higher
                               precedence than the current operator (Note:
                               a left parenthesis has the least precendence).
                               Then push the current operator onto the stack.
( added by MvdV:
 5. A comma                    Push to operator stack, since it is needed
                                to estimate the number of arguments to pop
                                when a ')' comes. Highest precedence, since
                                it can't be evaluated except for special code.
 6. Identifier
          and nextchar<>'('    Push to argument stack.
 7. Identifier
          and nextchar='('     Call callback for token for function <indentifier>,
                                push token to operator stack.
)

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
- OOP. Mainly to allow symbolic TExpression class to have custom parsers.
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

(second public version)
- Partial rewrite of the parser body function. Now tokenizes everything first,
   which cleans up the code, and probably doesn't matter for performance much.
- The above will make implementing custom functions easier.


}

type {Tokens generated by the parser. Anything else is a constant or variable}
 ParseOperation=(padd,psub,pmul,pdvd,ppow,pfacul,pleft,pright,pcomma,
                 pcos,psin,ptan,psqr,psqrt,pexp,pln,pinv,pminus,
                 pcotan,parcsin,parccos,parctan,
                 psinh,pcosh,ptanh,parcsinh,
                 parccosh,parctanh,plog10,
                 plog2,plnxpi,parctan2,pstep,
                 ppower,phypot,plogn,pnothing,pidentifier,pconstant);

CONST
 ParserFunctionNamesUpper   : array[padd..pnothing] of string[7]=
                 ('+','-','*','/','^','!','(',')',',','COS','SIN',
                 'TAN','SQR','SQRT','EXP','LN','INV','-',
                 'COTAN','ARCSIN','ARCCOS','ARCTAN',
                 'SINH','COSH','TANH','ARCSINH',
                 'ARCCOSH','ARCTANH','LOG10',
                 'LOG2','LNXP1','ARCTAN2','STEP',
                 'POWER','HYPOT','LOGN','NOTHING');

 {Operator or function-}
 Priority          : array[padd..pnothing] of ArbInt=
                 (1,1,2,2,3,0,0,0,0,
                  4,4,4,4,4,4,4,4,
                  4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5);

 OppsXlat='+-*/^!(),'; {Must match the first entries of ParseOperation.
                         Pos(OppsXlat,c)-1+ord(Padd) is typecast!}

Const
  RPNMax = 20;              { I think you only need 4-8, but just to be safe }
  OpMax  = 25;
  AllowedInConstant= ['0'..'9','.','E','e'];
  AllowedInIdentifier= ['0'..'9','A'..'Z','_'];

Type
  String15 = String[15];

Procedure ParserInternalError(const Msg:String;A,B:ArbInt);

VAR S,S2 : String;

begin
 Str(A,S);      {Usually a identification number for the occurance}
 Str(B,S2);     {Usually the value that tripped the IE}
 Raise EParserIE.Create(SParsIE+Msg+S+' '+S2);
end;

function TBaseExprParser.InFixToParseTree(Expr : String;VAR RPNexpr: String):pnode;

Var
  RPNStack   : Array[1..RPNMax] of PNode;        { Stack For RPN calculator }
  RPNTop,
  OpTop      : ArbInt;
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
  treal : ArbFloat;
  tint  : ArbInt;
  Error : ArbInt;
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
 the TExpression enumerations. Avoids some ugly (and shaky) typecasts
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
   unknown0:
   unknown1:
   unknown2:
   unknown3:
   unknown4:
   else
    ParserInternalError('Unknown function',1,ORD(Operation));
   end;
end;

Function IsFunction(S:String):ParseOperation;

var Count:ParseOperation;

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
  I,len       : ArbInt;
  Identifier  : String15;
  LastOperation,
  OperationNr : ParseOperation;
  FunctionNr  : ArbInt;
  isminus     : boolean;

begin
  RPNExpr:='';
  OpTop  := 0;                                              { Reset stacks }
  RPNTop := 0;
  Token  := '';
  {$ifdef fpc}
   Expr:=Upcase(Expr);
  {$endif}
  OperationNr:=pnothing;
  i:=1; len:=Length(Expr);
  while I<=Len do
   begin
    LastOperation:=Operationnr;
    OperationNr:=pnothing;

    {one char token?}

    FunctionNr:=pos(expr[I],OppsXlat);
    If FunctionNr=0 THEN
     begin

      {Yes -> convert to token}

      OperationNr:=ParseOperation(FunctionNr-1+ORD(padd));

      {Unary minus handling}

      IF OperationNr=psub then
       IF (I=1) OR (LastOperation IN [padd,pleft,pmul,pdvd,psub,ppow]) THEN
         OperationNr:=PMinus;
     end
    else
     begin
      CASE Expr[I] OF
       '0'..'9' : While (Expr[I] in AllowedInConstant) and (I<=len) do
                   begin
                    identifier:=Identifier+Expr[I];
                    inc(i);
                   end;
                  dec(i);
                  OperationNr:=Pconstant;
                 end;
      'A'..'Z' :begin
                 While (Expr[I] IN AllowedInIdentifier) AND (I<=Len) DO
                  begin
                   Token:=Token+Expr[I];
                   Inc(I);
                  end;
                 Dec(i);
                 OperationNr:=pidentifier;
                end;
       else
        begin
          {Something's rotten in the State of Denmark}
        end;

    CASE OperationNr OF
     padd..pfacul :   begin
                        {Currently processed token a simple or infix operation? ->
                           evaluate precedence}
                        While (OpTop > 0) AND
                                  (Priority[OperationNr] <= Priority[OpStack[OpTop]]) DO
                                   RPNOperation(OpPop);
                        OpPush(OperationNr);
                      end;
    pcomma        : Begin
                     {We end a "term" here, so try to reduce the last term}

                     While (OpStack[OpTop-1] <> [pleft,pcomma]) OR (Optop<2) DO
                      RPNOperation(OpPop);

                     {There must be exactly one parameter now after the last comma!}
                     IF (OpStack[OpTop-1] IN [pleft, pcomma]) OR NOT (OpStack[Optop-2] IN [pleft,pcomma])  THEN
                     OpPush(OperationNr);
                    end;
    pleft         : OpPush(OperationNr);
    pright        : Begin {More difficult, because of custom functions}
                     {First, reduce last term}
                     While NOT(OpStack[OpTop] IN [pleft,pcomma]) DO
                      RPNOperation(OpPop);

                     {Then count comma's, and find "(" }
                     J:=OpTop;
                     Commas:=0;
                     WHILE (I>0) AND (OpStack[J]<>pLeft) DO
                      BEGIN
                       IF OpStack[J]=pComma THEN
                        INC(Commas);
                      END;
                    IF J<>0 THEN {Found?}
                     begin
                      {function name before it?}
                      IF OpStack[J-1]=pIdentifier THEN
                       begin {Get a functionnr for the identifier}
                         FunctionNr:=GetToken(PIntentifier,commas);
                         IF FunctionNr>=0 THEN { -1=unknown, -2 is wrong param}
                          RPNOperation(UnknownTokens[Commas],commas,functionnr);
                       end
                      else
                        OpPop         { Pop off and ignore the '(' }
                     end;
                   end;
            end;



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

function TBaseExprParser.ParseTreeToInfix(expr:pnode):string;

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

function TBaseExprParser.ParseTreeToRPN(expr:pnode):string;
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

{
  $Log$
  Revision 1.1  2002/12/15 21:01:23  marco
  Initial revision

}
