{******************************************************************************}
{                                                                              }
{ JmComplex.pas for Jedi Math Alpha 1.02                                       }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The Original Code is jmComplex.pas.                                          }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains                                                           }
{                                                                              }
{ Unit owner:                                                                  }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

//MyComplex - implements basic complex functions and operations in a procedural way
//Beside rectangular and polar type definitions, it implements
//copy
//change type
//conjugate
//add
//accumulate
//subtract
//negative accumulate
//multiply
//divide
//square root
//square
//exp
//log
//sin
//cos
//tan
//sin hyperbolic
//cos hyperbolic
//tan hyperbolic
//secans
//cosecans
//secans hyperbolic
//cosecans hyperbolic
//product over sum
//
//function and procedure parameters can be rectangular or polar or any combination
//thereof.
//
//result types are rectangular or polar, distinguished as postfix.
//
//--
//1.st draft      1.sept.02       written by R.Tschaggelar, based on previous work
//                                from M.Vaughan(mvComplex) the existing Jedi unit
//                                (JclComplex) and various input from JEDI-Math
//
//
//

unit JmComplex;
interface

{$i jedimath.inc}
uses
  JmTypes;

type
 PJmComplexRect=^TJmComplexRect;
 TJmComplexRect=Record
  Re,Im:TjmFloat;
  end;

 PJmComplexPolar=^TJmComplexPolar;
 TJmComplexPolar=Record
  Mag,Ang:TjmFloat;
  end;

 procedure PolarAdjust(u:TJmComplexPolar); // -pi <= Angle <= pi

 function CCreateRect(re,im:TJmFloat):TJmComplexRect;
 function CCreatePolar(mag,ang:TJmFloat):TJmComplexPolar;

 function CCopy(const u:TJmComplexRect):TJmComplexRect; overload;   // result:=u;
 function CCopy(const u:TJmComplexPolar):TJmComplexPolar;overload;  // result:=u;

 function CRect2Polar(const u:TJmComplexRect):TJmComplexpolar;   // result:=u; incl. conversion
 function CPolar2Rect(const u:TJmComplexPolar):TJmComplexRect;   // result:=u; incl. conversion

 procedure CConjugate(var u:TJmComplexRect); overload;               // u:=conjugate(u);
 procedure CConjugate(var u:TJmComplexPolar);overload;              // u:=conjugate(u);

 function CConjugateRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=conjugate(u);
 function CConjugateRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=conjugate(u);
 function CConjugatePolar(const u:TJmComplexPolar):TJmComplexPolar;overload;  // result:=conjugate(u);
 function CConjugatePolar(const u:TJmComplexRect):TJmComplexPolar;overload;   // result:=conjugate(u);

 function CAddRect(const a,b:TJmComplexRect):TJmComplexRect;overload;                         // result:=a+b
 function CAddRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;overload;   // result:=a+b
 function CAddRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;overload;  // result:=a+b
 function CAddRect(const a,b:TJmComplexPolar):TJmComplexRect;overload;                        // result:=a+b

 function CAddPolar(const a,b:TJmComplexRect):TJmComplexPolar;overload;                         // result:=a+b
 function CAddPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;overload;   // result:=a+b
 function CAddPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;overload;   // result:=a+b
 function CAddPolar(const a,b:TJmComplexPolar):TJmComplexPolar;overload;                        // result:=a+b

 procedure CAccumulate(var a:TJmComplexRect;const b:TJmComplexRect);overload;   // a:=a+b
 procedure CAccumulate(var a:TJmComplexRect;const b:TJmComplexPolar);overload;  // a:=a+b
 procedure CAccumulate(var a:TJmComplexPolar;const b:TJmComplexRect);overload;  // a:=a+b
 procedure CAccumulate(var a:TJmComplexPolar;const b:TJmComplexPolar);overload; // a:=a+b

 function CSubRect(const a,b:TJmComplexRect):TJmComplexRect;overload;                         // result:=a-b
 function CSubRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;overload;   // result:=a-b
 function CSubRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;overload;   // result:=a-b
 function CSubRect(const a,b:TJmComplexPolar):TJmComplexRect;overload;                        // result:=a-b

 function CSubPolar(const a,b:TJmComplexRect):TJmComplexPolar;overload;                       // result:=a-b
 function CSubPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;overload; // result:=a-b
 function CSubPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;overload; // result:=a-b
 function CSubPolar(const a,b:TJmComplexPolar):TJmComplexPolar;overload;                      // result:=a-b

 procedure CNegAccumulate(var a:TJmComplexRect;const b:TJmComplexRect);overload;   // a:=a-b
 procedure CNegAccumulate(var a:TJmComplexRect;const b:TJmComplexPolar);overload;  // a:=a-b
 procedure CNegAccumulate(var a:TJmComplexPolar;const b:TJmComplexRect);overload;  // a:=a-b
 procedure CNegAccumulate(var a:TJmComplexPolar;const b:TJmComplexPolar);overload; // a:=a-b

 function CMultRect(const a,b:TJmComplexRect):TJmComplexRect;overload;                         // result:=a*b
 function CMultRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;overload;   // result:=a*b
 function CMultRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;overload;   // result:=a*b
 function CMultRect(const a,b:TJmComplexPolar):TJmComplexRect;overload;                        // result:=a*b

 function CMultRect(const a:TJmComplexRect; const b:TJmFloat):TJmComplexRect;overload;        // result:=a*b
 function CMultRect(const a:TJmComplexPolar; const b:TJmFloat):TJmComplexRect;overload;       // result:=a*b

 function CMultPolar(const a,b:TJmComplexRect):TJmComplexPolar;overload;                        // result:=a*b
 function CMultPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;overload;  // result:=a*b
 function CMultPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;overload;  // result:=a*b
 function CMultPolar(const a,b:TJmComplexPolar):TJmComplexPolar;overload;                       // result:=a*b

 function CMultPolar(const a:TJmComplexRect; const b:TJmFloat):TJmComplexPolar;overload;        // result:=a*b
 function CMultPolar(const a:TJmComplexPolar; const b:TJmFloat):TJmComplexPolar;overload;       // result:=a*b

 function CDivRect(const a,b:TJmComplexRect):TJmComplexRect;overload;                         // result:=a/b
 function CDivRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;overload;   // result:=a/b
 function CDivRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;overload;   // result:=a/b
 function CDivRect(const a,b:TJmComplexPolar):TJmComplexRect;overload;                        // result:=a/b

 function CDivPolar(const a,b:TJmComplexRect):TJmComplexPolar;overload;   // result:=a/b
 function CDivPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;overload;   // result:=a/b
 function CDivPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;overload;   // result:=a/b
 function CDivPolar(const a,b:TJmComplexPolar):TJmComplexPolar;overload;   // result:=a/b

 function CSqrtRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=sqrt(u);
 function CSqrtRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=sqrt(u);
 function CSqrtPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=sqrt(u);
 function CSqrtPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=sqrt(u);

 function CSqrRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=sqr(u);
 function CSqrRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=sqr(u);
 function CSqrPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=sqr(u);
 function CSqrPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=sqr(u);

 function CExpRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=exp(u);
 function CExpRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=exp(u);
 function CExpPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=exp(u);
 function CExpPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=exp(u);

 function CLogRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=log(u);
 function CLogRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=log(u);
 function CLogPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=log(u);
 function CLogPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=log(u);

 function CSinRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=sin(u);
 function CSinRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=sin(u);
 function CSinPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=sin(u);
 function CSinPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=sin(u);

 function CCosRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=cos(u);
 function CCosRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=cos(u);
 function CCosPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=cos(u);
 function CCosPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=cos(u);

 function CTanRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=tan(u);
 function CTanRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=tan(u);
 function CTanPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;  // result:=tan(u);
 function CTanPolar(const u:TJmComplexRect):TJmComplexPolar;overload;   // result:=tan(u);

 function CSinhRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=sinh(u);
 function CSinhRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=sinh(u);
 function CSinhPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;  // result:=sinh(u);
 function CSinhPolar(const u:TJmComplexRect):TJmComplexPolar;overload;   // result:=sinh(u);

 function CCoshRect(const u:TJmComplexRect):TJmComplexRect;overload;     // result:=cosh(u);
 function CCoshRect(const u:TJmComplexPolar):TJmComplexRect;overload;    // result:=cosh(u);
 function CCoshPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;  // result:=cosh(u);
 function CCoshPolar(const u:TJmComplexRect):TJmComplexPolar;overload;   // result:=cosh(u);

 function CTanhRect(const u:TJmComplexRect):TJmComplexRect;overload;      // result:=tanh(u);
 function CTanhRect(const u:TJmComplexPolar):TJmComplexRect;overload;     // result:=tanh(u);
 function CTanhPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=tanh(u);
 function CTanhPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=tanh(u);

 function CSecRect(const u:TJmComplexRect):TJmComplexRect;overload;      // result:=sec(u);
 function CSecRect(const u:TJmComplexPolar):TJmComplexRect;overload;     // result:=sec(u);
 function CSecPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=sec(u);
 function CSecPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=sec(u);

 function CCscRect(const u:TJmComplexRect):TJmComplexRect;overload;      // result:=csc(u);
 function CCscRect(const u:TJmComplexPolar):TJmComplexRect;overload;     // result:=csc(u);
 function CCscPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=csc(u);
 function CCscPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=csc(u);

 function CSechRect(const u:TJmComplexRect):TJmComplexRect;overload;      // result:=sech(u);
 function CSechRect(const u:TJmComplexPolar):TJmComplexRect;overload;     // result:=sech(u);
 function CSechPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=sech(u);
 function CSechPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=sech(u);

 function CCschRect(const u:TJmComplexRect):TJmComplexRect;overload;      // result:=csch(u);
 function CCschRect(const u:TJmComplexPolar):TJmComplexRect;overload;     // result:=csch(u);
 function CCschPolar(const u:TJmComplexRect):TJmComplexPolar;overload;    // result:=csch(u);
 function CCschPolar(const u:TJmComplexPolar):TJmComplexPolar;overload;   // result:=csch(u);

 function CPosRect(const a,b:TJmComplexRect):TJmComplexRect;overload;                   // result:=(a*b)/(a+b);
 function CPosRect(const a:TJmComplexRect;b:TJmComplexPolar):TJmComplexRect;overload;   // result:=(a*b)/(a+b);
 function CPosRect(const a:TJmComplexPolar;b:TJmComplexRect):TJmComplexRect;overload;   // result:=(a*b)/(a+b);
 function CPosRect(const a,b:TJmComplexPolar):TJmComplexRect;overload;                  // result:=(a*b)/(a+b);

 function CPosPolar(const a,b:TJmComplexRect):TJmComplexPolar;overload;                 // result:=(a*b)/(a+b);
 function CPosPolar(const a:TJmComplexRect;b:TJmComplexPolar):TJmComplexPolar;overload; // result:=(a*b)/(a+b);
 function CPosPolar(const a:TJmComplexPolar;b:TJmComplexRect):TJmComplexPolar;overload; // result:=(a*b)/(a+b);
 function CPosPolar(const a,b:TJmComplexPolar):TJmComplexPolar;overload;                // result:=(a*b)/(a+b);

implementation
const
 AlmostZero = 1e-30;  // Rect2Polar

procedure PolarAdjust(u:TJmComplexPolar);
var
  t:extended;
begin
 if (u.Ang>pi)or(u.Ang<-pi) then begin
  t:=u.Ang/Pi;
  u.Ang:=pi*frac(t);
 end;
end;

function CCreateRect(re,im:TJmFloat):TJmComplexRect;
begin
 result.Re:=re; result.Im:=Im;
end;

function CCreatePolar(mag,ang:TJmFloat):TJmComplexPolar;
begin
 result.Mag:=mag; result.ang:=ang;
end;
 
function CCopy(const u:TJmComplexRect):TJmComplexRect;    // result:=u;
begin
 result.Re:=u.Re; result.Im:=u.im;
end;

function CCopy(const u:TJmComplexPolar):TJmComplexPolar;  // result:=u;
begin
 result.Mag:=u.Mag; result.Ang:=u.Ang;
end;



function CRect2Polar(const u:TJmComplexRect):TJmComplexPolar;
begin
 result.Mag:=sqrt(sqr(u.Re)+sqr(u.Im));
 if (abs(u.Re)<AlmostZero) then
 begin
  if (u.Im>0) then
    result.Ang:=Pi/2
  else
    result.Ang:=-Pi/2;
  end
 else
 begin // real part bigger than zero
  result.Ang:=arctan(u.Im/u.Re);
  if (u.re<0) then
    result.Ang:=result.Ang+Pi;
 end;
end;

// taken from the Borland math library
procedure SinCos(const Theta: Extended; var Sin, Cos: Extended);
asm
        FLD     Theta
        FSINCOS
        FSTP    tbyte ptr [edx]    // Cos
        FSTP    tbyte ptr [eax]    // Sin
        FWAIT
end;

function CPolar2Rect(const u:TJmComplexPolar):TJmComplexRect;   
var a,b:Extended;
begin
 sincos(u.Ang,a,b);
 result.Re:=u.Mag*b;
 result.Im:=u.Mag*a;
end;

// Conjugate  ***************************************************************

procedure CConjugate(var u:TJmComplexRect);               // u:=conjugate(u);
begin
 u.im:=-u.im;
end;

procedure CConjugate(var u:TJmComplexPolar);              // u:=conjugate(u);
begin
 u.Ang:=-u.Ang;
end;

function CConjugateRect(const u:TJmComplexRect):TJmComplexRect;     // result:=conjugate(u);
begin
 result.Re:=u.Re; result.Im:=-u.im;
end;

function CConjugateRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=conjugate(u);
begin
 result:=CPolar2Rect(u);
 result.Im:=-result.Im;
end;

function CConjugatePolar(const u:TJmComplexPolar):TJmComplexPolar;  // result:=conjugate(u);
begin
 result.Mag:=u.Mag; result.Ang:=-u.Ang;
end;

function CConjugatePolar(const u:TJmComplexRect):TJmComplexPolar;   // result:=conjugate(u);
begin
 result:=CRect2Polar(u);
 result.Ang:=-result.Ang;
end;

// Add  ***************************************************************************

function CAddRect(const a,b:TJmComplexRect):TJmComplexRect;                         // result:=a+b
begin
 result.Re:=a.Re+b.Re; result.Im:=a.Im+b.Im;
end;

function CAddRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;   // result:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 result.Re:=a.Re+u.Re;
 result.Im:=a.Im+u.Im;
end;

function CAddRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;   // result:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 result.Re:=b.Re+u.Re;
 result.Im:=b.Im+u.Im;
end;

function CAddRect(const a,b:TJmComplexPolar):TJmComplexRect;                        // result:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 result:=CPolar2Rect(b);
 result.Re:=result.Re+u.Re;
 result.Im:=result.Im+u.Im;
end;

function CAddPolar(const a,b:TJmComplexRect):TJmComplexPolar;                         // result:=a+b
var u:TJmComplexRect;
begin
 u.Re:=a.Re+b.Re;
 u.Im:=a.Im+b.Im;
 result:=CRect2Polar(u);
end;

function CAddPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;   // result:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 u.Re:=u.Re+a.Re;
 u.Im:=u.Im+a.Im;
 result:=CRect2Polar(u);
end;

function CAddPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;   // result:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 u.Re:=u.Re+b.Re;
 u.Im:=u.Im+b.Im;
 result:=CRect2Polar(u);
end;

function CAddPolar(const a,b:TJmComplexPolar):TJmComplexPolar;                        // result:=a+b
var u,v:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 v:=CPolar2Rect(b);
 u.Re:=u.Re+v.Re;
 u.Im:=u.Im+v.Im;
 result:=CRect2Polar(u);
end;

// Accumulate  ****************************************************************

procedure CAccumulate(var a:TJmComplexRect;const b:TJmComplexRect);   // a:=a+b
begin
 a.Re:=a.Re+b.Re; 
 a.im:=a.Im+b.Im;
end;

procedure CAccumulate(var a:TJmComplexRect;const b:TJmComplexPolar);  // a:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 a.Re:=a.Re+u.Re; 
 a.im:=a.Im+u.Im;
end;

procedure CAccumulate(var a:TJmComplexPolar;const b:TJmComplexRect);  // a:=a+b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 u.Re:=u.Re+b.Re; 
 u.im:=u.Im+b.Im;
 a:=CRect2Polar(u);
end;

procedure CAccumulate(var a:TJmComplexPolar;const b:TJmComplexPolar); // a:=a+b
var u,v:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 v:=CPolar2Rect(b);
 u.Re:=u.Re+v.Re; 
 u.im:=u.Im+v.Im;
 a:=CRect2Polar(u);
end;

// Subtract  ***********************************************************************

function CSubRect(const a,b:TJmComplexRect):TJmComplexRect;                         // result:=a-b
begin
 result.Re:=a.Re-b.re; result.im:=a.Im-b.Im;
end;

function CSubRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;   // result:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 result.Re:=a.Re-u.Re;
 result.Im:=a.Im-u.Im;
end;

function CSubRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;   // result:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 result.Re:=-b.Re+u.Re;
 result.Im:=-b.Im+u.Im;
end;

function CSubRect(const a,b:TJmComplexPolar):TJmComplexRect;                        // result:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 result:=CPolar2Rect(b);
 result.Re:=-result.Re+u.Re;
 result.Im:=-result.Im+u.Im;
end;

function CSubPolar(const a,b:TJmComplexRect):TJmComplexPolar;                       // result:=a-b
var u:TJmComplexRect;
begin
 u.Re:=a.Re-b.Re;
 u.Im:=a.Im-b.Im;
 result:=CRect2Polar(u);
end;

function CSubPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar; // result:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 u.Re:=-u.Re+a.Re;
 u.Im:=-u.Im+a.Im;
 result:=CRect2Polar(u);
end;

function CSubPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar; // result:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 u.Re:=u.Re-b.Re;
 u.Im:=u.Im+b.Im;
 result:=CRect2Polar(u);
end;

function CSubPolar(const a,b:TJmComplexPolar):TJmComplexPolar;                      // result:=a-b
var u,v:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 v:=CPolar2Rect(b);
 u.Re:=u.Re-v.Re;
 u.Im:=u.Im-v.Im;
 result:=CRect2Polar(u);
end;

// Negative Acumulate  ************************************************************

procedure CNegAccumulate(var a:TJmComplexRect;const b:TJmComplexRect);   // a:=a-b
begin
 a.Re:=a.re-b.re; a.Im:=a.Im-b.Im;
end;

procedure CNegAccumulate(var a:TJmComplexRect;const b:TJmComplexPolar);  // a:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 a.Re:=a.Re-u.Re; 
 a.im:=a.Im-u.Im;
end;

procedure CNegAccumulate(var a:TJmComplexPolar;const b:TJmComplexRect);  // a:=a-b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 u.Re:=u.Re-b.Re; 
 u.im:=u.Im-b.Im;
 a:=CRect2Polar(u);
end;

procedure CNegAccumulate(var a:TJmComplexPolar;const b:TJmComplexPolar); // a:=a-b
var u,v:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 v:=CPolar2Rect(b);
 u.Re:=u.Re-v.Re; 
 u.im:=u.Im-v.Im;
 a:=CRect2Polar(u);
end;

// Multiply  *************************************************************************************

function CMultRect(const a,b:TJmComplexRect):TJmComplexRect;                         // result:=a*b
begin
 result.Re:=a.Re*b.Re-a.Im*b.Im;
 result.Im:=a.Re*b.Im+a.Im*b.Re;
end;

function CMultRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;   // result:=a*b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 result.Re:=a.Re*u.Re-a.Im*u.Im;
 result.Im:=a.Re*u.Im+a.Im*u.Re;
end;

function CMultRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;   // result:=a*b
var u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 result.Re:=u.Re*b.Re-u.Im*b.Im;
 result.Im:=u.Re*b.Im+u.Im*b.Re;
end;

function CMultRect(const a,b:TJmComplexPolar):TJmComplexRect;                        // result:=a*b
var u:TJmComplexPolar;
begin
 u:=CMultPolar(a,b);
 result:=CPolar2Rect(u);
end;

function CMultRect(const a:TJmComplexRect; const b:TJmFloat):TJmComplexRect;        // result:=a*b
begin
 result.Re:=a.Re*b;
 result.Im:=a.Im*b;
end;

function CMultRect(const a:TJmComplexPolar; const b:TJmFloat):TJmComplexRect;       // result:=a*b
var u:TJmComplexPolar;
begin
 u.Mag:=a.Mag*b;
 u.Ang:=a.Ang;
 result:=CPolar2Rect(u);
end;

function CMultPolar(const a,b:TJmComplexRect):TJmComplexPolar;                        // result:=a*b
var u:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 result:=CRect2Polar(u);
end;

function CMultPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;  // result:=a*b
var u:TJmComplexPolar;
begin
 u:=CRect2Polar(a);
 result.Mag:=u.Mag*b.Mag; 
 result.Ang:=u.Ang+b.Ang;
end;

function CMultPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;  // result:=a*b
var u:TJmComplexPolar;
begin
 u:=CRect2Polar(b);
 result.Mag:=u.Mag*a.Mag; 
 Result.Ang:=u.Ang+a.Ang;
end;

function CMultPolar(const a,b:TJmComplexPolar):TJmComplexPolar;                       // result:=a*b
begin
 result.Mag:=a.Mag*b.Mag; 
 result.Ang:=a.Ang+b.Ang;
end;

function CMultPolar(const a:TJmComplexRect; const b:TJmFloat):TJmComplexPolar;        // result:=a*b
var u:TJmComplexPolar;
begin
 u:=CRect2Polar(a);
 result.Mag:=u.Mag*b; 
 result.Ang:=u.Ang;
end;

function CMultPolar(const a:TJmComplexPolar; const b:TJmFloat):TJmComplexPolar;       // result:=a*b
begin
 result.Mag:=a.Mag*b; 
 result.Ang:=a.Ang;
end;

// Div  ******************************************************************************************

function CDivRect(const a,b:TJmComplexRect):TJmComplexRect;                         // result:=a/b
var r2:TJmFloat;
begin
 { consider Error trapping ... }
 r2:=sqr(b.Re)+sqr(b.Im);
 result.Re:=(a.Re*b.Re+a.Im*b.Im)/r2;
 result.Im:=(a.im*b.Re-a.Re*b.Im)/r2;
end;

function CDivRect(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexRect;   // result:=a/b
var r2:TJmFloat;
    u:TJmComplexRect;
begin
 u:=CPolar2Rect(b);
 { consider Error trapping ... }
 r2:=sqr(b.Mag);
 result.Re:=(a.Re*u.Re+a.Im*u.Im)/r2;
 result.Im:=(a.im*u.Re-a.Re*u.Im)/r2;
end;

function CDivRect(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexRect;   // result:=a/b
var r2:Extended;   // extended against overflows
    u:TJmComplexRect;
begin
 u:=CPolar2Rect(a);
 { consider Error trapping ... }
 r2:=sqr(b.Re)+sqr(b.Im);
 result.Re:=(u.Re*b.Re+u.Im*b.Im)/r2;
 result.Im:=(u.im*b.Re-u.Re*b.Im)/r2;
end;

function CDivRect(const a,b:TJmComplexPolar):TJmComplexRect;                        // result:=a/b
var v:TJmComplexPolar;
begin
 { consider Error trapping ... }
 v.Mag:=a.Mag/b.Mag;
 v.Ang:=a.Ang+b.Ang;
 result:=CPolar2Rect(v);
end;

function CDivPolar(const a,b:TJmComplexRect):TJmComplexPolar;   // result:=a/b
var u:TJmComplexRect;
    r2:TJmFloat;
begin
{ consider Error trapping ... }
 r2:=sqr(b.Re)+sqr(b.Im);
 u.Re:=(a.Re*b.Re+a.Im*b.Im)/r2;
 u.Im:=(a.im*b.Re-a.Re*b.Im)/r2;
 result:=CRect2Polar(u);
end;

function CDivPolar(const a:TJmComplexRect;const b:TJmComplexPolar):TJmComplexPolar;   // result:=a/b
var u:TJmComplexPolar;
begin
 u:=CRect2Polar(a);
{ consider Error trapping ... }
 result.Mag:=u.Mag/b.Mag;
 result.Ang:=u.Ang-b.Ang;
end;

function CDivPolar(const a:TJmComplexPolar;const b:TJmComplexRect):TJmComplexPolar;   // result:=a/b
var u:TJmComplexPolar;
begin
 u:=CRect2Polar(b);
{ consider Error trapping ... }
 result.Mag:=a.Mag/u.Mag; result.Ang:=a.Ang-u.Ang;
end;

function CDivPolar(const a,b:TJmComplexPolar):TJmComplexPolar;   // result:=a/b
begin
 { consider Error trapping ... }
 result.Mag:=a.Mag/b.Mag; result.Ang:=a.Ang-b.Ang;
end;

// Sqrt  *************************************************************************

function CSqrtRect(const u:TJmComplexRect):TJmComplexRect;     // result:=sqrt(u);
var v:TJmComplexPolar;
begin
 v:=CRect2Polar(u);
 v.Mag:=sqrt(v.mag); 
 v.Ang:=v.Ang/2;
 result:=CPolar2Rect(v);
end;

function CSqrtRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=sqrt(u);
var v:TJmComplexPolar;
begin
 v.Mag:=sqrt(u.mag); 
 v.Ang:=u.Ang/2;
 result:=CPolar2Rect(v);
end;

function CSqrtPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=sqrt(u);
begin
 result.Mag:=sqrt(u.mag); 
 result.Ang:=u.Ang/2;
end;

function CSqrtPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=sqrt(u);
var v:TJmComplexPolar;
begin
 v:=CRect2Polar(u);
 result.Mag:=sqrt(v.mag); 
 result.Ang:=v.Ang/2;
end;

// Sqr  ************************************************************************

function CSqrRect(const u:TJmComplexRect):TJmComplexRect;     // result:=sqr(u);
begin
 result.Re:=sqr(u.Re)-sqr(u.Im);
 result.Im:=2*u.Re*u.Im;
end;

function CSqrRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=sqr(u);
var v:TJmComplexPolar;
begin
 v.Mag:=sqr(u.Mag); 
 v.Ang:=2*u.Ang;
 result:=CPolar2Rect(v);
end;

function CSqrPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=sqr(u);
begin
 result.Mag:=sqr(u.Mag); result.Ang:=2*u.Ang;
end;

function CSqrPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=sqr(u);
var v:TJmComplexPolar;
begin
 v:=CRect2Polar(u);
 result.Mag:=sqr(v.Mag);
 result.Ang:=2*v.Ang;
end;

// Exp  ***********************************************************************

function CExpRect(const u:TJmComplexRect):TJmComplexRect;     // result:=exp(u);
var v:TJmComplexPolar;
begin
 v.Mag:=exp(u.Re); 
 v.Ang:=u.Im;
 result:=CPolar2Rect(v);
end;

function CExpRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=exp(u);
var v:TJmComplexPolar;
    w:TJmComplexRect;
begin
 w:=CPolar2Rect(u);
 v.Mag:=exp(w.Re); 
 v.Ang:=w.Im;
 result:=CPolar2Rect(v);
end;

function CExpPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=exp(u);
var w:TJmComplexRect;
begin
 w:=CPolar2Rect(u);
 result.Mag:=exp(w.Re); 
 result.Ang:=w.Im;
end;

function CExpPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=exp(u);
begin
 result.Mag:=exp(u.Re); result.Ang:=u.Im;
end;

// Log  **********************************************************

function CLogRect(const u:TJmComplexRect):TJmComplexRect;     // result:=log(u);
var v:TJmComplexPolar;
begin
 v:=CRect2Polar(u);
 { consider Errortrapping here ... }
 result.Re:=ln(v.Mag); 
 result.Im:=v.Ang;
end;

function CLogRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=log(u);
begin
 { consider Errortrapping here ... }
 PolarAdjust(u);
 result.Re:=ln(u.Mag); 
 result.Im:=u.Ang;
end;

function CLogPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=log(u);
var v:TJmComplexRect;
begin
 { consider Errortrapping here ... }
 PolarAdjust(u);
 v.Re:=ln(u.Mag); 
 v.Im:=u.Ang;
 result:=CRect2Polar(v);
end;

function CLogPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=log(u);
var v:TJmComplexRect;
    w:TJmComplexPolar;
begin
 w:=CRect2Polar(u);
 { consider Errortrapping here ... }
 v.Re:=ln(w.Mag);
 v.Im:=w.Ang;
 result:=CRect2Polar(v);
end;

// sin(z)=sin(x+iy)=-i*(exp(iz)-exp(-iz))/2  ***********************************

function CSinRect(const u:TJmComplexRect):TJmComplexRect;     // result:=sin(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h.re:=-u.Im; h.Im:=u.Re;
 pe:=CExpPolar(h);
 h.re:=u.Im; h.Im:=-u.Re;
 ne:=CExpPolar(h);
 result:=CSubRect(pe,ne);
 t:=result.Re;
 result.Re:=result.Im/2;
 result.Im:=-t/2;
end;

function CSinRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=sin(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h:=CPolar2Rect(u);
 t:=h.Re;
 h.Re:=-h.Im; h.Im:=t;
 pe:=CExpPolar(h);
 h.Re:=-h.Re; h.Im:=-h.Im;
 ne:=CExpPolar(h);
 result:=CSubRect(pe,ne);
 t:=result.Re;
 result.Re:=result.Im/2;
 result.Im:=-t/2;
end;

function CSinPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=sin(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h:=CPolar2Rect(u);
 t:=h.Re;
 h.Re:=-h.Im; h.Im:=t;
 pe:=CExpPolar(h);
 h.re:=-h.re; h.Im:=-h.im;
 ne:=CExpPolar(h);
 h:=CSubRect(pe,ne);
 t:=h.Re;
 h.Re:=h.Im/2;
 h.Im:=-t/2;
 result:=CRect2Polar(h);
end;

function CSinPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=sin(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h.re:=-u.Im; h.Im:=u.Re;
 pe:=CExpPolar(h);
 h.re:=u.Im; h.Im:=-u.Re;
 ne:=CExpPolar(h);
 h:=CSubRect(pe,ne);
 t:=h.Re;
 h.Re:=h.Im/2;
 h.Im:=-t/2;
 result:=CRect2Polar(h);
end;

// cos(z)=cos(x+iy)=(exp(iz)+exp(-iz))/2  ***************************************

function CCosRect(const u:TJmComplexRect):TJmComplexRect;     // result:=cos(u);
var
  pe,ne:TJmComplexPolar;
  h:TJmComplexRect;
begin
 h.re:=-u.Im; h.Im:=u.Re;
 pe:=CExpPolar(h);
 h.re:=u.Im; h.Im:=-u.Re;
 ne:=CExpPolar(h);
 result:=CAddRect(pe,ne);
 result.Re:=result.Re/2;
 result.Im:=result.Im/2;
end;

function CCosRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=cos(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h:=CPolar2Rect(u);
 t:=h.Re;
 h.Re:=-h.Im; h.Im:=t;
 pe:=CExpPolar(h);
 h.Re:=-h.Re; h.Im:=-h.Im;
 ne:=CExpPolar(h);
 result:=CAddRect(pe,ne);
 result.Re:=result.Re/2;
 result.Im:=result.Im/2;
end;

function CCosPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=cos(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
    t:TJmFloat;
begin
 h:=CPolar2Rect(u);
 t:=h.Re;
 h.Re:=-h.Im; h.Im:=t;
 pe:=CExpPolar(h);
 h.re:=-h.Re; h.Im:=-h.Re;
 ne:=CExpPolar(h);
 result:=CAddPolar(pe,ne);
 result.Mag:=Result.Mag/2;
end;

function CCosPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=cos(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
begin
 h.re:=-u.Im; h.Im:=u.Re;
 pe:=CExpPolar(h);
 h.re:=u.Im; h.Im:=-u.Re;
 ne:=CExpPolar(h);
 result:=CAddPolar(pe,ne);
 result.Mag:=result.Mag/2;
end;

// tan(z)=sin(z)/cos(z)  **********************************************************

function CTanRect(const u:TJmComplexRect):TJmComplexRect;     // result:=tan(u);
var s,c:TJmComplexRect;
begin
 s:=CSinRect(u);
 c:=CCosRect(u);
 result:=CDivRect(s,c);
end;

function CTanRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=tan(u);
var s,c:TJmComplexRect;
begin
 s:=CSinRect(u);
 c:=CCosRect(u);
 result:=CDivRect(s,c);
end;

function CTanPolar(const u:TJmComplexPolar):TJmComplexPolar;  // result:=tan(u);
var s,c:TJmComplexRect;
begin
 s:=CSinRect(u);
 c:=CCosRect(u);
 result:=CDivPolar(s,c);
end;

function CTanPolar(const u:TJmComplexRect):TJmComplexPolar;   // result:=tan(u);
var s,c:TJmComplexRect;
begin
 s:=CSinRect(u);
 c:=CCosRect(u);
 result:=CDivPolar(s,c);
end;

// sinh(z)=(exp(z)-exp(-z))/2  **************************************************

function CSinhRect(const u:TJmComplexRect):TJmComplexRect;     // result:=sinh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
begin
 pe:=CExpPolar(u);
 h.Re:=-u.Re; h.Im:=-u.Im;
 ne:=CExpPolar(h);
 result:=CSubRect(pe,ne);
 result.Re:=Result.Re/2;
 result.Im:=result.Im/2;
end;

function CSinhRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=sinh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexPolar;
begin
 pe:=CExpPolar(u);
 h.Mag:=-u.Mag; h.Ang:=u.Ang;
 ne:=CExpPolar(h);
 result:=CSubRect(pe,ne);
 result.Re:=Result.Re/2;
 result.Im:=result.Im/2;
end;

function CSinhPolar(const u:TJmComplexPolar):TJmComplexPolar;  // result:=sinh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexPolar;
begin
 pe:=CExpPolar(u);
 h.Mag:=-u.Mag; h.Ang:=u.Ang;
 ne:=CExpPolar(h);
 result:=CSubPolar(pe,ne);
 result.Mag:=result.Mag/2;
end;

function CSinhPolar(const u:TJmComplexRect):TJmComplexPolar;   // result:=sinh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
begin
 pe:=CExpPolar(u);
 h.Re:=-u.Re; h.Im:=-u.Im;
 ne:=CExpPolar(h);
 result:=CSubPolar(pe,ne);
 result.Mag:=result.Mag/2;
end;

// cosh(z)=(exp(z)+exp(-z))/2  **************************************************

function CCoshRect(const u:TJmComplexRect):TJmComplexRect;     // result:=cosh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
begin
 pe:=CExpPolar(u);
 h.Re:=-u.Re; h.Im:=-u.Im;
 ne:=CExpPolar(h);
 result:=CAddRect(pe,ne);
 result.Re:=Result.Re/2;
 result.Im:=result.Im/2;
end;

function CCoshRect(const u:TJmComplexPolar):TJmComplexRect;    // result:=cosh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexPolar;
begin
 pe:=CExpPolar(u);
 h.Mag:=-u.Mag; h.Ang:=u.Ang;
 ne:=CExpPolar(h);
 result:=CAddRect(pe,ne);
 result.Re:=Result.Re/2;
 result.Im:=result.Im/2;
end;

function CCoshPolar(const u:TJmComplexPolar):TJmComplexPolar;  // result:=cosh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexPolar;
begin
 pe:=CExpPolar(u);
 h.Mag:=-u.Mag; h.Ang:=u.Ang;
 ne:=CExpPolar(h);
 result:=CAddPolar(pe,ne);
 result.Mag:=result.Mag/2;
end;

function CCoshPolar(const u:TJmComplexRect):TJmComplexPolar;   // result:=cosh(u);
var pe,ne:TJmComplexPolar;
    h:TJmComplexRect;
begin
 pe:=CExpPolar(u);
 h.Re:=-u.Re; h.Im:=-u.Im;
 ne:=CExpPolar(h);
 result:=CAddPolar(pe,ne);
 result.Mag:=result.Mag/2;
end;

// tanh(z)=sinh(z)/cosh(z)  **********************************************************

function CTanhRect(const u:TJmComplexRect):TJmComplexRect;      // result:=tanh(u);
var s,c:TJmComplexPolar;
begin
 s:=CSinhPolar(u);
 c:=CCoshPolar(u);
 result:=CDivRect(s,c);
end;

function CTanhRect(const u:TJmComplexPolar):TJmComplexRect;     // result:=tanh(u);
var s,c:TJmComplexPolar;
begin
 s:=CSinhPolar(u);
 c:=CCoshPolar(u);
 result:=CDivRect(s,c);
end;

function CTanhPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=tanh(u);
var s,c:TJmComplexPolar;
begin
 s:=CSinhPolar(u);
 c:=CCoshPolar(u);
 result:=CDivPolar(s,c);
end;

function CTanhPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=tanh(u);
var s,c:TJmComplexPolar;
begin
 s:=CSinhPolar(u);
 c:=CCoshPolar(u);
 result:=CDivPolar(s,c);
end;

// sec(z)= 1/cos(z)  **********************************************************

function CSecRect(const u:TJmComplexRect):TJmComplexRect;      // result:=sec(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCosRect(u);
 result:=CDivRect(h,u);
end;

function CSecRect(const u:TJmComplexPolar):TJmComplexRect;     // result:=sec(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCosRect(u);
 result:=CDivRect(h,u);
end;

function CSecPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=sec(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCosRect(u);
 result:=CDivPolar(h,u);
end;

function CSecPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=sec(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCosRect(u);
 result:=CDivPolar(h,u);
end;

// csc(z)= 1/sin(z)  **********************************************************

function CCscRect(const u:TJmComplexRect):TJmComplexRect;      // result:=csc(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinRect(u);
 result:=CDivRect(h,u);
end;

function CCscRect(const u:TJmComplexPolar):TJmComplexRect;     // result:=csc(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinRect(u);
 result:=CDivRect(h,u);
end;

function CCscPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=csc(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinRect(u);
 result:=CDivPolar(h,u);
end;

function CCscPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=csc(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinRect(u);
 result:=CDivPolar(h,u);
end;

// sech(z)= 1/cosh(z)  **********************************************************

function CSechRect(const u:TJmComplexRect):TJmComplexRect;      // result:=sech(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCoshRect(u);
 result:=CDivRect(h,u);
end;

function CSechRect(const u:TJmComplexPolar):TJmComplexRect;     // result:=sech(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCoshRect(u);
 result:=CDivRect(h,u);
end;

function CSechPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=sech(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCoshRect(u);
 result:=CDivPolar(h,u);
end;

function CSechPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=sech(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CCoshRect(u);
 result:=CDivPolar(h,u);
end;

// csch(z)= 1/sinh(z)  **********************************************************

function CCschRect(const u:TJmComplexRect):TJmComplexRect;      // result:=csch(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinhRect(u);
 result:=CDivRect(h,u);
end;

function CCschRect(const u:TJmComplexPolar):TJmComplexRect;     // result:=csch(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinhRect(u);
 result:=CDivRect(h,u);
end;

function CCschPolar(const u:TJmComplexRect):TJmComplexPolar;    // result:=csch(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinhRect(u);
 result:=CDivPolar(h,u);
end;

function CCschPolar(const u:TJmComplexPolar):TJmComplexPolar;   // result:=csch(u);
var h,t:TJmComplexRect;
begin
 h.Re:=1; h.Im:=0;
 t:=CSinhRect(u);
 result:=CDivPolar(h,u);
end;

// Product over Sum ********************************************************************************

function CPosRect(const a,b:TJmComplexRect):TJmComplexRect;                   // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivRect(u,v);
end;

function CPosRect(const a:TJmComplexRect;b:TJmComplexPolar):TJmComplexRect;   // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivRect(u,v);
end;

function CPosRect(const a:TJmComplexPolar;b:TJmComplexRect):TJmComplexRect;   // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivRect(u,v);
end;

function CPosRect(const a,b:TJmComplexPolar):TJmComplexRect;                  // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivRect(u,v);
end;

function CPosPolar(const a,b:TJmComplexRect):TJmComplexPolar;                 // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivPolar(u,v);
end;

function CPosPolar(const a:TJmComplexRect;b:TJmComplexPolar):TJmComplexPolar; // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivPolar(u,v);
end;

function CPosPolar(const a:TJmComplexPolar;b:TJmComplexRect):TJmComplexPolar; // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivPolar(u,v);
end;

function CPosPolar(const a,b:TJmComplexPolar):TJmComplexPolar;                // result:=(a*b)/(a+b);
var u,v:TJmComplexRect;
begin
 u:=CMultRect(a,b);
 v:=CAddRect(a,b);
 result:=CDivPolar(u,v);
end;

end.


