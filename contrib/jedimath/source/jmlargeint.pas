{******************************************************************************}
{                                                                              }
{ JmLargint.pas for Jedi Math Alpha 1.02                                       }
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
{ The Original Code is mint.pas.                                               }
{                                                                              }
{ Origonal comment.                                                            }
{                                                                              }
{  MyInteger (mint) Library is copyright (c) 2001-2002 by Carlo Kok            }
{                                                                              }
{  This software is provided 'as-is', without any expressed or implied         }
{  warranty. In no event will the author be held liable for any damages        }
{  arising from the use of this software.                                      }
{  Permission is granted to anyone to use this software for any kind of        }
{  application, and to alter it and redistribute it freely, subject to         }
{  the following restrictions:                                                 }
{  1. The origin of this software must not be misrepresented, you must         }
{     not claim that you wrote the original software.                          }
{  2. Altered source versions must be plainly marked as such, and must         }
{     not be misrepresented as being the original software.                    }
{  3. You may not create a library that uses this library as a main part       }
{     of the program and sell that library.                                    }
{  4. You must have a visible line in your programs aboutbox or                }
{     documentation that it is made using MINT and where MINT can be found.    }
{  5. This notice may not be removed or altered from any source                }
{     distribution.                                                            }
{                                                                              }
{  If you have any questions concerning this license write me (Carlo Kok):     }
{    ck@carlo-kok.com or try our newsserver:                                   }
{    news://news.carlo-kok.com/                                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains large integer routines                                    }
{                                                                              }
{ Unit owner:                                                                  }
{ Last modified:                                                               }
{      March 27, 2004 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)  }
{      for the Jedi Math Alpha 1.02 release                                    }
{                                                                              }
{******************************************************************************}

unit JmLargeInt;
{$R-}{$Q-}
interface

{$i jedimath.inc}

type
  TJmLargeInteger = record
    Sign: Byte;
    Data: array of Word;
  end;

procedure TJmLargeInteger_Init(var I: TJmLargeInteger);
{ Set the TJmLargeInteger to zero }
procedure TJmLargeInteger_Free(var I: TJmLargeInteger);
{ Set the TJmLargeInteger to zero }

procedure TJmLargeInteger_Set(var Val1: TJmLargeInteger; Const Val2: TJmLargeInteger);
{ Set the Val1 to Val2 }

procedure TJmLargeInteger_Add(var Output: TJmLargeInteger; const Val1, Val2: TJmLargeInteger); overload;
{ Output := Val1 + Val2 }
procedure TJmLargeInteger_Add(var Val1: TJmLargeInteger; const Val2: TJmLargeInteger); overload;
{ Val1 := Val1 + Val2 }
procedure TJmLargeInteger_Sub(var Output: TJmLargeInteger; const Val1,Val2: TJmLargeInteger); overload;
{ Output := Val1 - Val2 }
procedure TJmLargeInteger_Sub(var Val1: TJmLargeInteger; const Val2: TJmLargeInteger); overload;
{ Val1 := Val1 - Val2 }
procedure TJmLargeInteger_DivByInt(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; const val: Word); overload;
{ Output := Val1 div Val }
procedure TJmLargeInteger_DivByInt(var Val1: TJmLargeInteger; const val: Word); overload;
{ Val1 := Val1 div val }
procedure TJmLargeInteger_DivByIntMod(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; const val: Word; Var ModRes: Longword); overload;
{ Output + Mod = Val1 div Val }
procedure TJmLargeInteger_DivByIntMod(Var Val1: TJmLargeInteger; const val: Word; Var ModRes: Longword); overload;
{ Val1 + Mod := Val1 div Val }
procedure TJmLargeInteger_MulByInt(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; Val: Word); overload;
{ Output := Val1 * Val }
procedure TJmLargeInteger_MulByInt(Var Val1: TJmLargeInteger; Val: Word); overload;
{ Val1 := Val1 * Val }
procedure TJmLargeInteger_Mul(Var Output: TJmLargeInteger; const Val1, Val2: TJmLargeInteger); overload;
{ Output := Val1 * Val2 }
procedure TJmLargeInteger_Mul(Var val1: TJmLargeInteger; const Val2: TJmLargeInteger); overload;
{ Val1 := Val1 * Val2 }
function TJmLargeInteger_Mod(const Dividend, Divisor: TJmLargeInteger; var Remainder: TJmLargeInteger): Boolean;
{ Remainder := Dividend mod Divisor }
function TJmLargeInteger_Div(const Dividend, Divisor: TJmLargeInteger; var Quotient, Remainder: TJmLargeInteger): Boolean; overload;
{ Quotient * Divisor + Remainder = Dividend }
function TJmLargeInteger_Div(const Dividend, Divisor: TJmLargeInteger; var Quotient: TJmLargeInteger): Boolean; overload;
{ Quotient * Divisor = Dividend }
procedure TJmLargeInteger_BlockShiftLeft(var Value: TJmLargeInteger; Count: Cardinal); overload;
{ Block shift a value to to the left (shl 16) }
procedure TJmLargeInteger_BlockShiftLeft(var Output: TJmLargeInteger; const Value: TJmLargeInteger; Count: Cardinal); overload;
{ Block shift a value to to the left (shl 16) }

procedure TJmLargeInteger_GCD(var Output: TJmLargeInteger; const e1, e2: TJmLargeInteger);
{ Return the Greatest Common Divisor (e1, e2); Euclidean }

procedure TJmLargeInteger_Exp(var Output: TJmLargeInteger; const Org, Exp: TJmLargeInteger);
{ Output := Org ^ Exp }
procedure TJmLargeInteger_ExpMod(var Output: TJmLargeInteger; const Org, Exp, _Mod: TJmLargeInteger);
{ Output := Org ^ Exp mod _Mod }

function TJmLargeInteger_Compare(const V1, V2: TJmLargeInteger): SmallInt; { -1, 0 or 1 }
{ Returns -1 if v1 < v2; 0 if v1=v2 and 1 if v1 > v2 }
function TJmLargeInteger_CompareAbs(const V1, V2: TJmLargeInteger): SmallInt; { -1, 0 or 1 }
{ Returns -1 if v1 < v2; 0 if v1=v2 and 1 if v1 > v2 }

function TJmLargeInteger_Value(I: Longint): TJmLargeInteger; overload;
function TJmLargeInteger_Value(I: Int64): TJmLargeInteger; overload;
function TJmLargeInteger_Value0: TJmLargeInteger;
function TJmLargeInteger_Value1: TJmLargeInteger;

function TJmLargeInteger_TJmLargeIntegerToB2(const Val: TJmLargeInteger): string;
{ Convert to bits }
function TJmLargeInteger_TJmLargeIntegerToB10(const Val: TJmLargeInteger): string;
{ Convert to decimal string }
function TJmLargeInteger_TJmLargeIntegerToB16(const Val: TJmLargeInteger): string;
{ Convert to hexadecimal string }
function TJmLargeInteger_TJmLargeIntegerToB255(const Val: TJmLargeInteger): string;
{ Convert to bytes }


function TJmLargeInteger_TrialDivide(const Val: TJmLargeInteger): Boolean;
{ Check if something is a prime. Returns false when Val is a prime. Should work till 62710561. }

procedure TJmLargeInteger_RSASign(var Output: TJmLargeInteger; const Msg, PrivateKey, N: TJmLargeInteger);
procedure TJmLargeInteger_RSAEncode(var Output: TJmLargeInteger; const Msg, PubKey, N: TJmLargeInteger);
procedure TJmLargeInteger_RSADecode(var Output: TJmLargeInteger; const Msg, PrivateKey, N: TJmLargeInteger);

implementation
const
 PrimeTable: array[0..999] of Word =
 ( 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
   73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
   157, 163, 167, 173,179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
   239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
   331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
   421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
   509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607,
   613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
   709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811,
   821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911,
   919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019,
   1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097,
   1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201,
   1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291,
   1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409,
   1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487,
   1489, 1493, 1499, 1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579,
   1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667,
   1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777,
   1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877,
   1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993,
   1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083,
   2087, 2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, 2153, 2161, 2179,
   2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287,
   2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381,
   2383, 2389, 2393, 2399, 2411, 2417, 2423, 2437, 2441, 2447, 2459, 2467, 2473,
   2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609,
   2617, 2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693,
   2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741, 2749, 2753, 2767, 2777, 2789,
   2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887,
   2897, 2903, 2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001,
   3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, 3083, 3089, 3109, 3119,
   3121, 3137, 3163, 3167, 3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229,
   3251, 3253, 3257, 3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331,
   3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, 3433, 3449, 3457,
   3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 3541,
   3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637,
   3643, 3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, 3733, 3739,
   3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821, 3823, 3833, 3847, 3851, 3853,
   3863, 3877, 3881, 3889, 3907, 3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947,
   3967, 3989, 4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057, 4073,
   4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139, 4153, 4157, 4159, 4177,
   4201, 4211, 4217, 4219, 4229, 4231, 4241, 4243, 4253, 4259, 4261, 4271, 4273,
   4283, 4289, 4297, 4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409,
   4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, 4513, 4517,
   4519, 4523, 4547, 4549, 4561, 4567, 4583, 4591, 4597, 4603, 4621, 4637, 4639,
   4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733,
   4751, 4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, 4861, 4871,
   4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937, 4943, 4951, 4957, 4967, 4969,
   4973, 4987, 4993, 4999, 5003, 5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077,
   5081, 5087, 5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, 5189,
   5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279, 5281, 5297, 5303, 5309,
   5323, 5333, 5347, 5351, 5381, 5387, 5393, 5399, 5407, 5413, 5417, 5419, 5431,
   5437, 5441, 5443, 5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521,
   5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639, 5641, 5647, 5651,
   5653, 5657, 5659, 5669, 5683, 5689, 5693, 5701, 5711, 5717, 5737, 5741, 5743,
   5749, 5779, 5783, 5791, 5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851,
   5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, 5953, 5981,
   5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091,
   6101, 6113, 6121, 6131, 6133, 6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211,
   6217, 6221, 6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, 6311,
   6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, 6373, 6379, 6389, 6397,
   6421, 6427, 6449, 6451, 6469, 6473, 6481, 6491, 6521, 6529, 6547, 6551, 6553,
   6563, 6569, 6571, 6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673,
   6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761, 6763, 6779, 6781,
   6791, 6793, 6803, 6823, 6827, 6829, 6833, 6841, 6857, 6863, 6869, 6871, 6883,
   6899, 6907, 6911, 6917, 6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991,
   6997, 7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, 7121,
   7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237,
   7243, 7247, 7253, 7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369,
   7393, 7411, 7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499, 7507,
   7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, 7573, 7577, 7583, 7589,
   7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687, 7691, 7699,
   7703, 7717, 7723, 7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829,
   7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919);


var
  TJmLargeInteger_1: TJmLargeInteger;


function Max(const i1, i2: Longint): Longint;
begin
  if i1 > i2 then
    Result := i1
  else
    Result := i2;
end;
function Min(const i1, i2: Longint): Longint;
begin
  if i1 < i2 then
    Result := i1
  else
    Result := i2;
end;

const
  HTAB: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

function WordToStr_16(L: Word): string;
var
  i: Integer;
begin
  SetLength(Result, 4);
  i := 4;
  while i > 0 do
  begin
    Result[i] := HTab[L and 15];
    dec(i);
    L := l shr 4;
  end;
end;


function WordToStr_2(L: Word): string;
var
  i: Integer;
begin
  SetLength(Result, 16);
  i := 16;
  while i > 0 do
  begin
    Result[i] := Chr(Ord('0') + (L and 1));
    dec(i);
    L := l shr 1;
  end;
end;

function WordToStr_10(L, NO: word): string;
var
  i: Integer;
begin
  SetLength(Result, NO);
  i := NO;
  while i > 0 do
  begin
    Result[i] := Chr(Ord('0') + L mod 10);
    dec(i);
    L := L div 10;
  end;
end;

procedure TJmLargeInteger_Init(var I: TJmLargeInteger);
begin
  I.Sign := 0;
  SetLength(I.Data, 0);
end;

procedure TJmLargeInteger_Free(var I: TJmLargeInteger);
begin
  I.Sign := 0;
  SetLength(I.Data, 0);
end;

procedure RemoveLeadingZeros(var Val1: TJmLargeInteger);
var
  i, newL: Longint;
begin
  newl := Length(Val1.Data);
  for i := Length(Val1.Data) -1 downto 0 do
  begin
    if Val1.Data[i] = 0 then
      NewL := i
    else
      Break;
  end;
  if Length(Val1.Data) <> Newl then
    SetLength(Val1.Data, Newl);
end;


procedure TJmLargeInteger_Add(Var Output: TJmLargeInteger; const Val1, Val2: TJmLargeInteger);
var
  I: Longint;
  Res, L, L2: LongWord;
  Temp: TJmLargeInteger;
begin
  Res := 0;
  TJmLargeInteger_Set(Output, Val1);
  if Val1.Sign <> Val2.Sign then
  begin
    if TJmLargeInteger_CompareAbs(Output, Val2) < 0 then
    begin
      TJmLargeInteger_Add(Temp, Val2, Output);
      TJmLargeInteger_Set(Output, Temp);
      Exit;
    end;
    for i := 0 to Min(Length(Output.Data), Length(Val2.Data)) -1 do
    begin
      L := LongWord(Output.Data[i] - Val2.Data[i]) - Res;
      res := L shr 31;
      Output.Data[i] := L;
    end;
    if Length(Output.Data) > Length(Val2.Data) then
    begin
      for i := Length(Val2.Data) to Length(Output.Data) -1 do
      begin
        L := Output.Data[i] - Res;
        res := L shr 31;
        Output.Data[i] := L;
      end;
    end else begin
      L2 := Length(Output.Data);
      if Length(Val2.Data) > Longint(L2) then
      begin
        SetLength(Output.Data, Length(Val2.Data));
        for i := L2 to Length(Val2.Data) -1 do
        begin
          L := Val2.Data[i] + Res;
          res := L shr 31;
          Output.Data[i] := L;
        end;
        if Res = 0 then
          Output.Sign := 1 - Output.Sign;
      end;
    end;
    RemoveLeadingZeros(Output);
  end else begin
    for i := 0 to Min(Length(Output.Data), Length(Val2.Data)) -1 do
    begin
      L := Output.Data[i] + Val2.Data[i] + Res;
      res := L shr 16;
      Output.Data[i] := L;
    end;
    if Length(Output.Data) > Length(Val2.Data) then
    begin
      for i := Length(Val2.Data) to Length(Output.Data) -1 do
      begin
        L := Output.Data[i] + Res;
        res := L shr 16;
        Output.Data[i] := L;
      end;
    end else begin
      L2 := Length(Output.Data);
      SetLength(Output.Data, Length(Val2.Data));
      for i := L2 to Length(Val2.Data) -1 do
      begin
        L := Val2.Data[i] + Res;
        res := L shr 16;
        Output.Data[i] := L;
      end;
    end;
    if res > 0 then
    begin
      L := Length(Output.Data);
      SetLength(Output.Data, L + 1);
      Output.Data[L] := Res;
    end;
  end;
  RemoveLeadingZeros(Output);
end;

procedure TJmLargeInteger_Sub(Var Output: TJmLargeInteger; const Val1, Val2: TJmLargeInteger);
var
  V: TJmLargeInteger;
begin
  V.Data := Val2.Data;
  V.Sign := 1 - Val2.Sign;
  TJmLargeInteger_Add(Output, Val1, V);
end;

function TJmLargeInteger_Value(I: Longint): TJmLargeInteger;
var
  b: Boolean;
begin
  b := i < 0;
  i := abs(i);
  if I > 65535 then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 2);
    Result.Data[0] := I;
    Result.Data[1] := I shr 16;
  end else if I > 0 then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 1);
    Result.Data[0] := I;
  end else
  begin
    Result.Sign := 0;
    Result.Data := nil;
  end;
end;
function TJmLargeInteger_Value(I: Int64): TJmLargeInteger;
var
  b: Boolean;
begin
  b := i < 0;
  i := abs(i);
  if I > $FFFFFFFFFFFF then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 4);
    Result.Data[0] := I;
    Result.Data[1] := I shr 16;
    Result.Data[2] := I shr 32;
    Result.Data[3] := I shr 48;
  end else if I > $FFFFFFFF then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 3);
    Result.Data[0] := I;
    Result.Data[1] := I shr 16;
    Result.Data[2] := I shr 32;
  end else if I > $FFFF then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 2);
    Result.Data[0] := I;
    Result.Data[1] := I shr 16;
  end else if I > 0 then
  begin
    if b then
      Result.Sign := 1
    else
      Result.Sign := 0;
    SetLength(Result.Data, 1);
    Result.Data[0] := I;
  end else begin
    Result.Sign := 0;
    Result.Data := nil;
  end;
end;

function TJmLargeInteger_Value0: TJmLargeInteger;
begin
  Result.Sign := 0;
  Result.Data := nil;
end;

function TJmLargeInteger_Value1: TJmLargeInteger;
begin
  if TJmLargeInteger_1.Data = nil then
  begin
    SetLength(TJmLargeInteger_1.Data, 1);
    TJmLargeInteger_1.Data[0] := 1;
  end;
  Result.Data := TJmLargeInteger_1.Data;
  Result.Sign := 0;
end;

procedure TJmLargeInteger_Set(var Val1: TJmLargeInteger; Const Val2: TJmLargeInteger);
var
  i: Longint;
begin
  if length(Val1.Data) <> length(Val2.Data) then  // FPC doesn't support this
  begin
    SetLength(Val1.Data, length(Val2.Data));
    for i := 0 to Length(Val2.Data)-1 do
    begin
      Val1.Data[i] := Val2.Data[i];
    end;
  end else
    SetLength(Val1.Data, Length(Val1.Data));
  Val1.Sign := Val2.Sign;
end;


procedure TJmLargeInteger_DivByInt(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; const val: Word);
var
  I: Longint;
  D, Res: LongWord;
begin
  Res := 0;
  TJmLargeInteger_Set(Output, Val1);
  for i := length(Output.Data)-1 downto 0 do
  begin
    D := Res or Output.Data[i];
    Res := (d mod Val) shl 16;
    Output.Data[i] := D div Val;
  end;
  RemoveLeadingZeros(Output);
end;

procedure TJmLargeInteger_DivByIntMod(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; const val: Word; Var ModRes: Longword);
var
  I: Longint;
  D: LongWord;
begin
  ModRes := 0;
  if length(Output.Data) <> length(Val1.Data) then  // FPC doesn't support this
//  if Output.Data <> Val1.Data then
    TJmLargeInteger_Set(Output, Val1);
  for i := length(Output.Data)-1 downto 0 do
  begin
    D := (ModRes shl 16) or Output.Data[i];
    ModRes := (d mod Val);
    Output.Data[i] := D div Val;
  end;
  RemoveLeadingZeros(Output);
end;

procedure TJmLargeInteger_MulByInt(Var Output: TJmLargeInteger; const Val1: TJmLargeInteger; Val: Word);
var
  I: Longint;
  Res: Longword;
begin
  if Length(Val1.Data) = 0 then
  begin
    SetLength(Output.Data, 0);
  end else begin
    Res := 0;
    TJmLargeInteger_Set(Output, Val1);
    for i := 0 to Length(Output.Data) -1 do
    begin
      Res := (Output.Data[i] * Val) + Res;
      Output.Data[i] := Res;
      Res := res shr 16;
    end;
    if Res <> 0 then
    begin
      I := Length(Output.Data);
      SetLength(Output.Data, I + 1);
      Output.Data[i] := Res;
    end;
    RemoveLeadingZeros(Output);
  end; {if}
end;

procedure TJmLargeInteger_Mul(Var Output: TJmLargeInteger; const Val1, Val2: TJmLargeInteger);
var
  i1, i2: Longint;
  Res: Longword;
begin
  TJmLargeInteger_Init(Output);
  SetLength(Output.Data, Length(Val1.Data) + Length(Val2.Data) + 1);
  Output.Sign := Val1.Sign - Val2.Sign;
  for i1 := 0 to Length(Output.Data) - 1 do
  begin
    Output.Data[i1] := 0;
  end;
  for i2 := 0 to Length(Val2.Data) -1 do
  begin
    res := 0;
    for i1 := 0 to Length(Val1.Data) -1 do
    begin
      Res := Val1.Data[i1] * Val2.Data[i2] + Res +Output.Data[i1+i2];
      Output.Data[i1+i2] := Res;
      Res := Res shr 16;
    end;
    if Res <> 0 then
    begin
      Output.Data[i2+Length(Val1.Data)] := Res;
    end;
  end; {for}
  RemoveLeadingZeros(Output);
end;

function TJmLargeInteger_CompareAbs(const V1, V2: TJmLargeInteger): SmallInt; { -1, 0 or 1 }
var
  i: Longint;
begin
  if (Length(V1.Data) = 0) and (Length(v2.Data) = 0) then
    Result := 0
  else if Length(v1.Data) > Length(v2.Data) then
    Result := 1
  else if Length(V1.Data) < Length(v2.Data) then
    Result := -1
  else
  begin
    for i := Length(V1.Data)-1 downto 0 do
    begin
      if v1.Data[i] > v2.Data[i] then
      begin
        Result := 1;
        exit;
      end else if v1.Data[i] < V2.Data[i] then
      begin
        Result := -1;
        exit;
      end;
    end;
    Result := 0;
  end;
end;

function TJmLargeInteger_Compare(const V1, V2: TJmLargeInteger): SmallInt;
var
  i: Longint;
begin
  if (Length(V1.Data) = 0) and (Length(v2.Data) = 0) then
    Result := 0
  else if V1.Sign < V2.Sign then
    Result := 1
  else if V1.Sign > V2.Sign then
    Result := -1
  else if Length(v1.Data) > Length(v2.Data) then
    Result := 1
  else if Length(V1.Data) < Length(v2.Data) then
    Result := -1
  else
  begin
    for i := Length(V1.Data)-1 downto 0 do
    begin
      if v1.Data[i] > v2.Data[i] then
      begin
        if V1.Sign = 0 then
          Result := 1
        else
          Result := -1;
        exit;
      end else if v1.Data[i] < V2.Data[i] then
      begin
        if V1.Sign = 0 then
          Result := -1
        else
          Result := 1;
        exit;
      end;
    end;
    Result := 0;
  end;
end;

function TJmLargeInteger_TJmLargeIntegerToB2(const Val: TJmLargeInteger): string;
var
  i: Longint;
begin
  if Val.Sign = 1 then
    Result := '-'
  else
    Result := '';
  for i := 0 to Length(Val.data)-1 do
  begin
    Result := WordToStr_2(Val.Data[i]) + Result;
  end;
  i := 0;
  while Result[i + 1] = '0' do
    inc(i);
  delete(Result, 1, i);
end;

function TJmLargeInteger_TJmLargeIntegerToB16(const Val: TJmLargeInteger): string;
var
  i: Longint;
begin
  if Val.Sign = 1 then
    Result := '-'
  else
    Result := '';
  for i := 0 to Length(Val.data)-1 do
  begin
    Result := WordToStr_16(Val.Data[i]) + Result;
  end;
  i := 0;
  while Result[i + 1] = '0' do
    inc(i);
  delete(Result, 1, i);
end;

function TJmLargeInteger_TJmLargeIntegerToB255(const Val: TJmLargeInteger): string;
begin
  SetLength(Result, Length(Val.Data) * 2);
  Move(Val.Data[0], Result[1], Length(Result));
end;

function TJmLargeInteger_TJmLargeIntegerToB10(const Val: TJmLargeInteger): string;
var
  Res: LongWord;
  tmp: TJmLargeInteger;
begin
  if Length(Val.Data) = 0 then
    Result := '0'
  else begin
    Result := '';
    TJmLargeInteger_Set(tmp, Val);
    while Length(Tmp.Data) > 0 do
    begin
      TJmLargeInteger_DivByIntMod(Tmp, Tmp, 10000, Res);
      Result := WordToStr_10(Res, 4) + Result;
    end;
    res := 0;
    while Result[res + 1] = '0' do
      inc(res);
    delete(Result, 1, Res);
    if Val.Sign = 1 then
      Result := '-' + Result;
  end;
end;

procedure TJmLargeInteger_Add(var Val1: TJmLargeInteger; const Val2: TJmLargeInteger);
begin
  TJmLargeInteger_Add(Val1, Val1, Val2);
end;

procedure TJmLargeInteger_Sub(var Val1: TJmLargeInteger; const Val2: TJmLargeInteger);
begin
  TJmLargeInteger_Sub(Val1, Val1, Val2);
end;

procedure TJmLargeInteger_DivByInt(var Val1: TJmLargeInteger; const val: Word);
begin
  TJmLargeInteger_DivByInt(Val1, Val1, Val);
end;

procedure TJmLargeInteger_DivByIntMod(Var Val1: TJmLargeInteger; const val: Word; Var ModRes: Longword);
begin
  TJmLargeInteger_DivByIntMod(Val1, Val1, Val, Modres);
end;

procedure TJmLargeInteger_MulByInt(Var Val1: TJmLargeInteger; Val: Word);
begin
  TJmLargeInteger_MulByInt(Val1, Val1, Val);
end;

procedure TJmLargeInteger_Mul(Var val1: TJmLargeInteger; const Val2: TJmLargeInteger);
var
  Tmp: TJmLargeInteger;
begin
  TJmLargeInteger_Mul(Tmp, Val1, Val2);
  TJmLargeInteger_Set(Val1, Tmp);
end;


procedure TJmLargeInteger_Exp(var Output: TJmLargeInteger; const Org, Exp: TJmLargeInteger);
var
  i1, i2: Longint;
  Skip: Boolean;
begin
  Skip := True;
  TJmLargeInteger_Set(Output, Org);
  for i1 := Length(Exp.Data) -1 downto 0 do
  begin
    for i2 := 15 downto 0 do
    begin
      if not skip then
      begin
        TJmLargeInteger_Mul(Output, Output); {square}
      end;
      if (Exp.Data[i1] shr i2 and 1) <> 0 then
      begin
        if Skip then
        begin
          Skip := False;
        end else begin
          TJmLargeInteger_Mul(Output, Org);
        end;
      end;
    end;
  end;
end;
procedure TJmLargeInteger_BlockShiftLeft(var Output: TJmLargeInteger; const Value: TJmLargeInteger; Count: Cardinal); overload;
var
  C, I: Longint;
begin
  C := Length(Value.Data);
  SetLength(Output.Data, C + Longint(Count));
  Output.Sign := Value.Sign;
  for i := C -1 downto 0 do
  begin
    Output.Data[i+Longint(Count)] := Value.Data[i];
  end;
  for i := 0 to Count -1 do
  begin
    Output.Data[i] := 0;
  end;
end;
procedure TJmLargeInteger_BlockShiftLeft(var Value: TJmLargeInteger; Count: Cardinal); overload;
begin
  TJmLargeInteger_BlockShiftLeft(Value, Value, Count);
end;
function TJmLargeInteger_DivideShiftCount(const Value1, Value2: TJmLargeInteger): Longint;
var
  i: Longint;
begin
  Result := Length(Value1.Data) - Length(Value2.Data);
  for i := Length(Value2.Data)-1 downto 0 do
  begin
    if Value1.Data[i+Result] > Value2.Data[i] then
    begin
      Exit;
    end else if Value1.Data[i+Result] < Value2.Data[i] then
    begin
      Dec(Result);
      exit;
    end;
  end;
end;

function TJmLargeInteger_Div(const Dividend, Divisor: TJmLargeInteger; var Quotient, Remainder: TJmLargeInteger): Boolean;
var
  Temp: TJmLargeInteger;
  TempDiv: TJmLargeInteger;
  L: Cardinal;
  ShiftCount: Longint;
begin
  if Length(Divisor.Data) = 0 then
  begin
    Result := False;
    exit;
  end;
  case TJmLargeInteger_CompareAbs(Dividend, Divisor) of
    0:
      begin
        TJmLargeInteger_Set(Quotient, TJmLargeInteger_Value1);
        TJmLargeInteger_Set(Remainder, TJmLargeInteger_Value0);
        if Dividend.Sign <> Divisor.Sign then
          Quotient.Sign := 1;
        Result := True;
        exit;
      end;
    -1:
      begin
        TJmLargeInteger_Set(Quotient, TJmLargeInteger_Value0);
        TJmLargeInteger_Set(Remainder, Dividend);
        if Dividend.Sign <> Divisor.Sign then
          Remainder.Sign := 1
        else
          Remainder.Sign := 0;
        Result := False;
        exit;
      end;
  end;
  SetLength(Quotient.Data, 0);
  TJmLargeInteger_Set(Remainder, Dividend);
  while TJmLargeInteger_Compare(Remainder, Divisor) > 0 do
  begin
    TJmLargeInteger_Set(TempDiv, Divisor);
    ShiftCount := TJmLargeInteger_DivideShiftCount(Remainder, TempDiv);
    TJmLargeInteger_BlockShiftLeft(TempDiv, ShiftCount);
    if Length(Remainder.Data) <> Length(TempDiv.Data) then
    begin
      L := Remainder.Data[Length(Remainder.Data)-1] shl 16 + Remainder.Data[Length(Remainder.Data)-2];
      L := L div (TempDiv.Data[Length(TempDiv.Data)-1]+1);
    end else
    begin
      L :=
        Remainder.Data[Length(Remainder.Data)-1] div
        (TempDiv.Data[Length(TempDiv.Data)-1]+1);
    end;
    TJmLargeInteger_Set(Temp, TempDiv);
    if L <> 0 then
    begin
      TJmLargeInteger_MulByInt(Temp, L);
      TJmLargeInteger_Sub(Remainder, Temp);
    end;
    if TJmLargeInteger_Compare(Remainder, TempDiv) >= 0 then
    begin
      TJmLargeInteger_Sub(Remainder, TempDiv);
      Inc(L);
    end;
    if Length(Quotient.Data) <= ShiftCount then
    begin
      SetLength(Quotient.Data, ShiftCount+1);
    end;
    Quotient.Data[ShiftCount] := Quotient.Data[ShiftCount] + L;
  end;
  Result := True;
  if Dividend.Sign <> Divisor.Sign then
  begin
    Quotient.Sign := 1;
    Remainder.Sign := 1;
  end else begin
    Quotient.Sign := 0;
    Remainder.Sign := 0;
  end;
end;

function TJmLargeInteger_Mod(const Dividend, Divisor: TJmLargeInteger; var Remainder: TJmLargeInteger): Boolean;
var
  temp: TJmLargeInteger;
begin
  Result := TJmLargeInteger_Div(Dividend, Divisor, Temp, Remainder);
end;

function TJmLargeInteger_Div(const Dividend, Divisor: TJmLargeInteger; var Quotient: TJmLargeInteger): Boolean;
var
  Temp: TJmLargeInteger;
begin
  Result := TJmLargeInteger_Div(Dividend, Divisor, Quotient, Temp);
end;

procedure TJmLargeInteger_GCD(var Output: TJmLargeInteger; const e1, e2: TJmLargeInteger);
var
  t1, re1: TJmLargeInteger;
begin
  if TJmLargeInteger_Compare(e1,e2) = -1 then
  begin
    TJmLargeInteger_Set(re1, e2);
    TJmLargeInteger_Set(Output, e1);
  end else
  begin
    TJmLargeInteger_Set(re1, e1);
    TJmLargeInteger_Set(Output, e2);
  end;
  TJmLargeInteger_Mod(re1, Output, t1);
  while length(t1.Data) > 0 do
  begin
    TJmLargeInteger_Set(re1, Output);
    TJmLargeInteger_Set(Output, t1);
    TJmLargeInteger_Mod(re1, Output, t1);
  end;
end;

procedure TJmLargeInteger_ExpMod(var Output: TJmLargeInteger; const Org, Exp, _Mod: TJmLargeInteger);
var
  i1, i2: Longint;
  Skip: Boolean;
begin
  Skip := True;
  TJmLargeInteger_Set(Output, Org);
  for i1 := Length(Exp.Data) -1 downto 0 do
  begin
    for i2 := 15 downto 0 do
    begin
      if not skip then
      begin
        TJmLargeInteger_Mul(Output, Output); {square}
      end;
      if (Exp.Data[i1] shr i2 and 1) <> 0 then
      begin
        if Skip then
        begin
          Skip := False;
        end else begin
          TJmLargeInteger_Mul(Output, Org);
        end;
      end;
      TJmLargeInteger_Mod(Output, _Mod, Output);
    end;
  end;
end;



procedure TJmLargeInteger_RSAEncode(var Output: TJmLargeInteger; const Msg, PubKey, N: TJmLargeInteger);
begin
  TJmLargeInteger_ExpMod(Output, Msg, PubKey, N);
end;

procedure TJmLargeInteger_RSADecode(var Output: TJmLargeInteger; const Msg, PrivateKey, N: TJmLargeInteger);
begin
  TJmLargeInteger_ExpMod(Output, Msg, PrivateKey, N);
end;

procedure TJmLargeInteger_RSASign(var Output: TJmLargeInteger; const Msg, PrivateKey, N: TJmLargeInteger);
begin
  TJmLargeInteger_ExpMod(Output, Msg, PrivateKey, N);
end;

function TJmLargeInteger_TrialDivide(const Val: TJmLargeInteger): Boolean;
var
  Temp: TJmLargeInteger;
  e: LongWord;
  i: Longint;
begin
  if (Length(Val.Data) = 0) or ((Length(Val.Data) = 1) and (Val.Data[0] = 1)) then
  begin
    Result := True;
    exit;
  end;
  for i := 0 to 999 do
  begin
    if (Length(Val.Data) = 1)and(PrimeTable[i] >= Val.Data[0]) then Break;
    TJmLargeInteger_DivByIntMod(Temp, Val, PrimeTable[i], E);
    if E = 0 then begin

      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

end.


