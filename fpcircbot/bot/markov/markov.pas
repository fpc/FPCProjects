Unit Markov;

{$mode objfpc}{$h+}

Interface

Uses MarkovDict, MarkovTable, Classes;

Type
	// Holds the last words used by the engine in order to 
	// avoid recursions
	TLastWordsUsed = Class
	Private
		fBuffer : Array Of Integer;
		fPos    : Integer;
	Public
		Constructor Create(Size : Cardinal);
		Destructor Destroy; Override;
		Procedure WordSaid(W : Integer);
		Function WordUsed(W : Integer): Integer;
		Procedure Show(Dict : TMarkovDict);
	End;

	// A generic markov text generator
	TMarkov = Class
	Private
		fErrorMargin  : Byte; // Deviation
		fThreshold    : Byte; // Limiar level for a construction to be considered "right"
		fDict         : TMarkovDict;
		fTable        : TMarkovTable;
		fUsed         : TLastWordsUsed;
		fUserUsed     : TLastWordsUsed;
		fStartCode    : Integer;
		fEndCode      : Integer;
		// Number of words in the dictionary
		Function WordsCount : Integer;
		// Number of entries in the markov chain
		Function MarkovCount : Integer;
		// A random word to follow K, with at least E% usage
		Function MostProbable(K, E : Integer): Integer;
	Public
		// D = Dict file
		// P = Phrase file 
		// M = Deviation 
		// T = Threshold
		Constructor Create(D, P : String; M, T : Byte);
		Destructor Destroy; Override;
		// Use talkto when you need to write a new phrase into the database
		// without generating a response (I.E. Write-Only)
		Procedure TalkTo(W : TStringList);
		// Use talkfrom when you want to generate a new phrase from the
		// current database
		Function TalkFrom: AnsiString;
		// Use talk to write to and generate a response at the same time
		Function Talk(W : TStringList): AnsiString;
		// Change a word in the dictionary, correcting a mispelling ?
		// S1 is the current word, S2 is the word to wich it should be changed
		Procedure Correct(S1, S2 : AnsiString);
		// Error margin is the max deviation from a central threshold level
		Property ErrorMargin : Byte Read fErrorMargin Write fErrorMargin;
		// Threshold is the level where a construction is marked as good
		// language one
		Property Threshold : Byte Read fThreshold Write fThreshold;
		// The ammount of words in the dictionary
		Property DictionaryWords : Integer Read WordsCount;
		// The ammount of words in the markov chain
		Property EntriesMarkov : Integer Read MarkovCount;
	End;

Implementation

// Generates (guaranteed) a random number wich value in the [X, Y] Range
Function RandomFromTo(X, Y : Integer): Integer;
Var 
	Tmp : Integer;
Begin
	Repeat
		Tmp := X + Random(Y - X);
	Until ((Tmp >= X) and (Tmp <= Y));
	RandomFromTo := Tmp;
End;

// TMarkov

Constructor TMarkov.Create(D, P : String; M, T : Byte);
Begin
	Inherited Create;
	fDict := TMarkovDict.Create(D);
	fDict.Load;
	fTable := TMarkovTable.Create(P);
	fTable.Load;
	fUsed := TLastWordsUsed.Create(16);
	fUserUsed := TLastWordsUsed.Create(32);
	fErrorMargin := M;
	fThreshold   := T;
	fStartCode   := fDict.InsertWord(ccStartToken);
	fEndCode     := fDict.InsertWord(ccEndToken);
	WriteLn('Phrase start code : ', fStartCode);
	WriteLn('Phrase end code   : ', fEndCode);
End;

Destructor TMarkov.Destroy;
Begin
	fDict.Flush;
	fDict.Free;
	fTable.Flush;
	fTable.Free;
	fUsed.Free;
	fUserUsed.Free;
	Inherited Destroy;
End;

Procedure TMarkov.TalkTo(W : TStringList);
Var
	Ctrl : Integer;
Begin
	If Assigned(W) And (W.Count > 0) Then 
	Begin
		// Puts every word in the dictionary if not already
		// and mark as context input
		For Ctrl := 0 To W.Count - 1 Do
			fUserUsed.WordSaid(fDict.InsertWord(W[Ctrl]));
		// Reinforce start word behaviour
		fTable.ImpulsePair(fStartCode, fDict.FindWord(W[0]));
		If W.Count > 1 Then
			// Insert/reinforce word pairs in the transition table
			For Ctrl := 0 To W.Count - 2 Do
				fTable.ImpulsePair(fDict.FindWord(W[Ctrl]), fDict.FindWord(W[Ctrl + 1]));
		// Reinforce end word behaviour
		fTable.ImpulsePair(fDict.FindWord(W[W.Count - 1]), fEndCode);
	End;
End;

// Number of words in the dictionary
Function TMarkov.WordsCount : Integer;
Begin
	WordsCount := fDict.Count;
End;

// Number of entries in the markov chain
Function TMarkov.MarkovCount : Integer;
Begin
	MarkovCount := fTable.Count;
End;

Function TMarkov.MostProbable(K, E : Integer): Integer;
Var
	Ctrl      : Integer;
	Highest   : Integer;
	NextKey   : Integer;
	Partial   : Array Of Integer;
	Possible  : Array Of Boolean;
	Found     : Boolean;
	Coeff     : Double;
	Possibles : Integer;

Begin
	Highest := 0;
	Possibles := 0;
	SetLength(Partial, fDict.Count);
	SetLength(Possible, fDict.Count);
	Write('   Transition matrix with coefficients : ');
	// Generates the transition matrix for K
	// from K to each possible word
	For Ctrl := fEndCode To (fDict.Count - 1) Do
	Begin
		// Coefficient : four words in the context cancels out a word being too much used
		Coeff := (fUserUsed.WordUsed(Ctrl) + 1) / ((fUsed.WordUsed(Ctrl) + 1) * 4);
		Partial[Ctrl] := Round(fTable.Transition[K, Ctrl] * Coeff);
		If Partial[Ctrl] > Highest Then
			Highest := Partial[Ctrl];
	End;
	WriteLn(Highest, ' highest.');
	// Transforms from "hits" into percentual (%) values
	// relative to the highest scoring word in the markov transition table
	// If a value is higher than the threshold (E) then this is a "possible"
	// word to follow K in a normal text
	Write('   Apply thresholds : ');
	For Ctrl := fEndCode To (fDict.Count - 1) Do
	Begin
		If Partial[Ctrl] > 0 Then
			Possible[Ctrl] := Round((Partial[Ctrl]) / (Highest / 100)) > E
		Else
			Possible[Ctrl] := False;
		If Possible[Ctrl] Then
			Inc(Possibles);
	End;
	WriteLn(Possibles, ' words can follow.');
	// next value for K (nextkey) is a random word, but
	// it will only be accepted when its a "possible" value
	Ctrl := 0; // Ctrl Will work as a sort of timeout;
	Found := False;
	NextKey := fEndCode;
	// loop until find a possible word or tried too much...
	// (try at least twice the ammount of words in the dictionary)
	Write('   Select keyword : ');
	While Not(Possible[NextKey]) And (Ctrl <= fDict.Count * Possibles) Do 
	Begin
		NextKey := RandomFromTo(fEndCode, (fDict.Count - 1));
		Inc(Ctrl);
		If Possible[NextKey] Then
			Found := True;
	End;
	// most probable value
	If Found Then
	Begin
		MostProbable := NextKey;
		WriteLn(fDict.Words[NextKey], ' selected.');
	End
	Else
	Begin
		MostProbable := fEndCode;
		WriteLn(' End of phrase reached.');
	End;
End;

Function TMarkov.TalkFrom: AnsiString;
Var 
	Temp  : AnsiString;
	Key   : Integer;
Begin
	// Start a fresh phrase
	WriteLn('  TalkFrom');
	WriteLn('  {');
	Key := fStartCode;
	Temp := '';
	Repeat
		Key := MostProbable(Key, RandomFromTo(fThreshold - fErrorMargin, fThreshold + fErrorMargin));
		If Key > fEndCode Then
		Begin
			// Append the next word in the phrase
			Temp := Temp + fDict.Words[Key] + ' ';
			fUsed.WordSaid(Key);
		End;
		// The next word will be the most probable from the current one
	Until Key = fEndCode;
	WriteLn('  }');
	TalkFrom := Temp;
End;

Function TMarkov.Talk(W : TStringList): AnsiString;
Var
	Temp  : Ansistring;
	Ctrl  : Integer;
	Ctrl2 : Integer;

Begin
	Temp := '';
	Ctrl2 := 0;
	If Assigned(W) Then
	Begin
		TalkTo(W);
		WriteLn;
		WriteLn('Generating text');
		WriteLn('{');
		WriteLn(' Input words : ');
		WriteLn(' {');
		Write(' ');
		For Ctrl := 0 To W.Count - 1 Do
			Write(fDict.FindWord(W[Ctrl]), ' ');
		WriteLn;
		WriteLn(' }');
		WriteLn(' Context box');
		WriteLn(' {');
		Write(' ');
		fUserUsed.Show(fDict);
		WriteLn(' }');
		WriteLn(' Used words');
		WriteLn(' {');
		Write(' ');
		fUsed.Show(fDict);
		WriteLn(' }');
		WriteLn(' Iterating');
		WriteLn(' {');
		While (Temp = '') And (Ctrl2 <= 1024) Do
		Begin
			Temp := TalkFrom;
			Inc(Ctrl2);
			WriteLn('  Try : ', Ctrl2);
		End;
		WriteLn(' }');
		WriteLn(' Result = ', Temp);
		WriteLn('}');
		WriteLn;
	End;
	Talk := Temp;
End;

// Change a word in the dictionary, correcting a mispelling ?
// S1 is the current word, S2 is the word to wich it should be changed
Procedure TMarkov.Correct(S1, S2 : AnsiString);
Var
	S1Pos : Integer;
Begin
	WriteLn('Searching for mispelled words');
	WriteLn('{');
	S1Pos := fDict.FindWord(S1);
	While S1Pos > fEndCode Do
	Begin
		WriteLn(' Correcting mispelled word : ', S1, ' to ', S2);
		fDict.Words[S1Pos] := S2;
		S1Pos := fDict.FindWord(S1);
	End;
	WriteLn('}');
End;

// TLastWordsUsed

Constructor TLastWordsUsed.Create(Size : Cardinal);
Var
	Ctrl : Cardinal;
Begin
	Inherited Create;
	SetLength(fBuffer, Size);
	For Ctrl := 0 To (Size - 1) Do
		fBuffer[Ctrl] := -1;
	fPos := 0;
End;

Destructor TLastWordsUsed.Destroy; 
Begin
	SetLength(fBuffer, 0);
	fPos := -1;
	Inherited Destroy;
End;

Procedure TLastWordsUsed.WordSaid(W : Integer);
Begin
	If fPos >= Length(fBuffer) Then
		fPos := 0;
	fBuffer[fPos] := W;
	Inc(fPos);
End;

Function TLastWordsUsed.WordUsed(W : Integer): Integer;
Var
	Ctrl : Cardinal;
	Temp : Integer;
Begin
	Temp := 0;
	For Ctrl := 0 To (Length(fBuffer) - 1) Do
		If fBuffer[Ctrl] = W Then
			Inc(Temp);
	WordUsed := Temp;
End;

Procedure TLastWordsUsed.Show(Dict : TMarkovDict);
Var
	Ctrl : Cardinal;
Begin
	For Ctrl := 0 To (Length(fBuffer) - 1) Do
		If fBuffer[Ctrl] > 0 Then
			Write(Dict.Words[fBuffer[Ctrl]], ' ');
	WriteLn;
End;

End.
