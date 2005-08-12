Unit MarkovTable;

{$mode objfpc}{$h+}

Interface

Type
	TArrayOfInteger = Array Of Integer;
	
	TMarkovTable = Class
	Private
		fName   : String;
		fNumber : Cardinal;
		fBuffer : TArrayOfInteger;
	Public
		Constructor Create(Arq : String);
		Destructor Destroy; Override;
		Procedure Load;
		Procedure Flush;
		Procedure AppendWord(W : Integer);
		Function Hits(W1, W2 : Integer): Cardinal;
		Property Words : TArrayOfInteger Read fBuffer Write fBuffer;
		Property Count : Cardinal Read fNumber;
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
	Handler : Text;
	Temp    : String;
Begin
	AssignFile(Handler, fName);
	Reset(Handler);
	While Not(Eof(Handler)) Do
	Begin
		ReadLn(Handler, Temp);
		AppendWord(StrToInt(Temp));
	End;
	CloseFile(Handler);
	fNumber := Length(fBuffer);
End;

Procedure TMarkovTable.Flush;
Var 
	Handler : Text;
	Ctrl    : Cardinal;
Begin
	If fNumber <= 0 Then
		Exit;
	AssignFile(Handler, fName);
	Rewrite(Handler);
	For Ctrl := 0 To (fNumber - 1) Do
		WriteLn(Handler, fBuffer[Ctrl]);
	CloseFile(Handler);
	fNumber := Length(fBuffer);
End;

Procedure TMarkovTable.AppendWord(W : Integer);
Begin
	fNumber := Length(fBuffer) + 1;
	SetLength(fBuffer, fNumber);
	fBuffer[fNumber - 1] := W;
End;

Function TMarkovTable.Hits(W1, W2 : Integer): Cardinal;
Var
	Ctrl,
	Temp : Cardinal;
Begin
	Temp := 0;
	For Ctrl := 0 To fNumber - 2 Do
		If (fBuffer[Ctrl] = W1) And (fBuffer[Ctrl + 1] = W2) Then
			Inc(Temp);
	Hits := Temp;
End;

End.
