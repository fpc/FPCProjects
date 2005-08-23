Unit MarkovTable;

{$mode objfpc}{$h+}

Interface

Const
	ccStartToken  = '<<';
	ccEndToken    = '>>';
	ccImpulse     = 10;

Type
	TTransition = Record
		W1, W2, Hits : Integer;
	End;

	TArrayOfTransition = Array Of TTransition;

	TMarkovTable = Class
	Private
		fName   : String;
		fNumber : Integer;
		fBuffer : TArrayOfTransition;
		Function FindPair(W1, W2 : Integer): Integer;
		Function AppendPair(W1, W2 : Integer): Integer;
		Procedure SetPair(W1, W2, Hits : Integer);
		Function GetPair(W1, W2 : Integer): Integer;
	Public
		Constructor Create(Arq : String);
		Destructor Destroy; Override;
		Procedure Load;
		Procedure Flush;
		Procedure ImpulsePair(W1, W2 : Integer);
		Property Transition[I1, I2 : Integer]: Integer Read GetPair Write SetPair;
		Property Count : Integer Read fNumber;
	End;

Implementation
Uses Sysutils;

Constructor TMarkovTable.Create(Arq : String);
Begin
	Inherited Create;
	fName := Arq;
	fNumber := 0;
	SetLength(fBuffer, 0);
End;

Destructor TMarkovTable.Destroy;
Begin
	fName := '';
	fNumber := 0;
	SetLength(fBuffer, 0);
	Inherited Destroy;
End;

Procedure TMarkovTable.Load;
Var 
	Handler : TextFile;
	Temp1,
	Temp2,
	Temp3   : AnsiString;
Begin
	AssignFile(Handler, fName);
	Reset(Handler);
	While Not(Eof(Handler)) Do
	Begin
		ReadLn(Handler, Temp1);
		ReadLn(Handler, Temp2);
		ReadLn(Handler, Temp3);
		SetPair(StrToInt(Temp1), StrToInt(Temp2), StrToInt(Temp3));
	End;
	CloseFile(Handler);
	fNumber := Length(fBuffer);
End;

Procedure TMarkovTable.Flush;
Var 
	Handler : TextFile;
	Ctrl    : Cardinal;
Begin
	fNumber := Length(fBuffer);
	If fNumber <= 0 Then
		Exit;
	AssignFile(Handler, fName);
	Rewrite(Handler);
	For Ctrl := 0 To (fNumber - 1) Do
	Begin
		WriteLn(Handler, fBuffer[Ctrl].W1);
		WriteLn(Handler, fBuffer[Ctrl].W2);
		WriteLn(Handler, fBuffer[Ctrl].Hits);
	End;
	CloseFile(Handler);
End;

Function TMarkovTable.FindPair(W1, W2 : Integer): Integer;
Var
	Ctrl : Integer;
Begin
	FindPair := -1;
	fNumber := Length(fBuffer);
	For Ctrl := 0 To fNumber - 1 Do
		If (fBuffer[Ctrl].W1 = W1) And (fBuffer[Ctrl].W2 = W2) Then
		Begin
			FindPair := Ctrl;
			Exit;
		End;
End;

Function TMarkovTable.AppendPair(W1, W2 : Integer): Integer;
Begin
	If FindPair(W1, W2) = -1 Then 
	Begin
		fNumber := Length(fBuffer) + 1;
		SetLength(fBuffer, fNumber);
		fBuffer[fNumber - 1].W1 := W1;
		fBuffer[fNumber - 1].W2 := W2;
		fBuffer[fNumber - 1].Hits := 0; // Start empty, will receive impulse afterwards
		AppendPair := fNumber - 1;
	End
	Else
		AppendPair := FindPair(W1, W2);
End;

Procedure TMarkovTable.ImpulsePair(W1, W2 : Integer);
Var
	PairPos : Integer;
Begin
	PairPos := FindPair(W1, W2);
	If PairPos < 0 Then
		PairPos := AppendPair(W1, W2);
	fBuffer[PairPos].Hits := fBuffer[PairPos].Hits + ccImpulse;
End;

Procedure TMarkovTable.SetPair(W1, W2, Hits : Integer);
Var
	PairPos : Integer;
Begin
	PairPos := FindPair(W1, W2);
	If PairPos < 0 Then
		PairPos := AppendPair(W1, W2); 
	fBuffer[PairPos].Hits := Hits;
End;

Function TMarkovTable.GetPair(W1, W2 : Integer): Integer;
Var
	PairPos : Integer;
Begin
	PairPos := FindPair(W1, W2);
	If PairPos = -1 Then
		GetPair := 0
	Else
		GetPair := fBuffer[PairPos].Hits;
End;

End.
