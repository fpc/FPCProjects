program testsystools;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, unit1, StBase, StCRC, StDate, StDateSt, StDict, StList, StRegEx,
  StStrms, StToHTML, StUtils, StBits, StVArr, StPtrns, StTree, StBCD, StMath,
  StText, StSort, StHASH, StDQue, StFIN, StNVBits, StNVCont, StNVList, StNVDQ,
  StLArr, StNVLMat, StNVTree, StPQueue, StStat, StOStr, StNVSCol, StNVColl,
  StNVLAry, StNVDict, StExpr, StAstro, StMerc, StVenus, StMars, StJup, StSaturn,
  StNeptun, StUranus, StPluto, StEclpse, StJupsat, StRegLin;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

