Unit Markov;

{$mode objfpc}{$h+}

Interface

Uses MarkovDict, Classes, SysUtils;

Type
	// Holds the last words used by the engine in order to 
	// avoid recursions
	TLastWordsUsed = Class
	Private
		fBuffer : Array Of String;
		fPos    : Integer;
	Public
		Constructor Create(Size : Cardinal);
		Destructor Destroy; Override;
		Procedure WordSaid(W : String);
		Function WordUsed(W : String): Integer;
		Procedure Show;
	End;

	// A generic markov text generator

{ TMarkov }

TMarkov = Class
	Private
		fErrorMargin  : Byte; // Deviation
		fThreshold    : Byte; // Limiar level for a construction to be considered "right"
		fDict         : TMarkovDict;
		fUsed         : TLastWordsUsed;
		fUserUsed     : TLastWordsUsed;
		Function StartCode: Integer;
		Function EndCode: Integer;
		// Number of words in the dictionary
		Function WordsCount : Integer;
		// Number of entries in the markov chain
		Function MarkovCount : Integer;
		// A random word to follow K, with at least E% usage
		Function RandomWord(const S1: string; E : Integer): string;
	Public
		// D = Markov data file
		// M = Deviation
		// T = Threshold
		Constructor Create(D : String; M, T : Byte);
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

Constructor TMarkov.Create(D : String; M, T : Byte);
Begin
	Inherited Create;
	fDict := TMarkovDict.Create(D);
	fDict.Load;
//	fTable := TMarkovTable.Create(P);
//	fTable.Load;
	fUsed := TLastWordsUsed.Create(16);
	fUserUsed := TLastWordsUsed.Create(32);
	fErrorMargin := M;
	fThreshold   := T;
        // make sure the startToken and EndToken are part of it.
	fDict.InsertWord(ccStartToken);
	fDict.InsertWord(ccEndToken);
	WriteLn('Phrase start code : ', StartCode);
	WriteLn('Phrase end code   : ', EndCode);
End;

Destructor TMarkov.Destroy;
Begin
	fDict.Flush;
	fDict.Free;
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
		For Ctrl := 0 To W.Count - 1 Do begin
                   fDict.InsertWord(W[Ctrl]);
		   fUserUsed.WordSaid(W[Ctrl]);
                end;
		// Reinforce start word behaviour
		fDict.ImpulsePair(ccStartToken, W[0]);
		If W.Count > 1 Then
			// Insert/reinforce word pairs in the transition table
			For Ctrl := 0 To W.Count - 2 Do
				fDict.ImpulsePair(W[Ctrl], W[Ctrl + 1]);
		// Reinforce end word behaviour
		fDict.ImpulsePair(W[W.Count - 1], ccEndToken);
	End;
End;

function TMarkov.StartCode: Integer;
begin
  Result := fDict.FindWord(ccStartToken);
end;

function TMarkov.EndCode: Integer;
begin
  Result := fDict.FindWord(ccEndToken);
end;

// Number of words in the dictionary
Function TMarkov.WordsCount : Integer;
Begin
	WordsCount := fDict.Count;
End;

// Number of entries in the markov chain
Function TMarkov.MarkovCount : Integer;
Begin
	MarkovCount := -1; // Not yet implemented
End;

Function TMarkov.RandomWord(const S1: string; E : Integer): string;
Var
	Ctrl      : Integer;
	Highest   : Integer;
        Partial   : Array Of Integer;
	Possible  : Array Of Boolean;
	Coeff     : Double;
	Possibles : Integer;
        Chosen: Int64;
        Transitions: TStringList;
Begin
        Result := ccEndToken;
        Transitions := FDict.Transitions[FDict.FindWord(S1)];
        if (Transitions=nil) or (Transitions.Count=0) then exit;

        Highest:= 0;
	Possibles := 0;
	SetLength(Partial, Transitions.Count);
	SetLength(Possible, Transitions.Count);
	Write('   Transition matrix with ', Transitions.Count ,' coefficients : ');
	// Generates the transition matrix for K
	// from K to each possible word
	For Ctrl := 0 to Transitions.Count - 1 Do
	Begin
		// Coefficient : four words in the context cancels out a word being too much used
		Coeff := (fUserUsed.WordUsed(Transitions[Ctrl]) + 1) / ((fUsed.WordUsed(Transitions[Ctrl]) + 1) * 4);
		Partial[Ctrl] := Round(Ptrint(Transitions.Objects[Ctrl]) * Coeff);
		If Partial[Ctrl] > Highest Then
			Highest := Partial[Ctrl];
	End;
	WriteLn(Highest, ' highest.');

	// Transforms from "hits" into percentual (%) values
	// relative to the highest scoring word in the markov transition table
	// If a value is higher than the threshold (E) then this is a "possible"
	// word to follow K in a normal text
	Write('   Apply thresholds : ');
	For Ctrl := 0 to Transitions.Count - 1 Do
	Begin
		If Partial[Ctrl] > 0 Then
			Possible[Ctrl] := Round((Partial[Ctrl]) / (Highest / 100)) > E
		Else
			Possible[Ctrl] := False;
		If Possible[Ctrl] Then
			Inc(Possibles);
	End;
	WriteLn(Possibles, ' words can follow.');
        if Possibles=0 then exit;

        //Choose a random from the possible words
        Chosen := Random(Possibles); // number to skip
	Write('   Select keyword : ');
	Ctrl := 0;
        while (Chosen>=Ord(Possible[Ctrl])) do begin
          if Possible[Ctrl] then
            dec(Chosen);
          inc(Ctrl);
        end;
        Result := Transitions[Ctrl];
        WriteLn(ctrl, ' ', Result, ' selected.');
End;

Function TMarkov.TalkFrom: AnsiString;
Var 
	Temp  : AnsiString;
	Key   : Integer;
        AWord : AnsiString;
Begin
	// Start a fresh phrase
	WriteLn('  TalkFrom');
	WriteLn('  {');
	AWord := ccStartToken;
	Temp := '';
	Repeat
		AWord := RandomWord(AWord, RandomFromTo(fThreshold - fErrorMargin, fThreshold + fErrorMargin));
		If AWord <> ccEndToken Then
		Begin
			// Append the next word in the phrase
			Temp := Temp + AWord + ' ';
			fUsed.WordSaid(AWord);
		End;
		// The next word will be the most probable from the current one
	Until AWord = ccEndToken;
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
			Write(W[Ctrl], ' ');
		WriteLn;
		WriteLn(' }');
		WriteLn(' Context box');
		WriteLn(' {');
		Write(' ');
		fUserUsed.Show;
		WriteLn(' }');
		WriteLn(' Used words');
		WriteLn(' {');
		Write(' ');
		fUsed.Show;
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
(*
	WriteLn('Searching for mispelled words');
	WriteLn('{');
	S1Pos := fDict.FindWord(S1);
        if (S1Pos = StartCode) or (S1Pos=EndCode) then exit;
        
	While S1Pos >= 0 Do
	Begin
		WriteLn(' Correcting mispelled word : ', S1, ' to ', S2);
		fDict.Words[S1Pos] := S2;
		S1Pos := fDict.FindWord(S1);
	End;
	WriteLn('}');
*)
  raise Exception.Create('Not yet implemented');
End;

// TLastWordsUsed

Constructor TLastWordsUsed.Create(Size : Cardinal);
Var
	Ctrl : Cardinal;
Begin
	Inherited Create;
	SetLength(fBuffer, Size);
	For Ctrl := 0 To (Size - 1) Do
		fBuffer[Ctrl] := '';
	fPos := 0;
End;

Destructor TLastWordsUsed.Destroy; 
Begin
	SetLength(fBuffer, 0);
	fPos := -1;
	Inherited Destroy;
End;

Procedure TLastWordsUsed.WordSaid(W : String);
Begin
	If fPos >= Length(fBuffer) Then
		fPos := 0;
	fBuffer[fPos] := W;
	Inc(fPos);
End;

Function TLastWordsUsed.WordUsed(W : String): Integer;
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

Procedure TLastWordsUsed.Show();
Var
	Ctrl : Cardinal;
Begin
	For Ctrl := 0 To (Length(fBuffer) - 1) Do
		If fBuffer[Ctrl] <> '' Then
			Write(fBuffer[Ctrl], ' ');
	WriteLn;
End;

End.
