Unit MarkovDict;

{$mode objfpc}{$h+}

Interface

Uses
  Classes;

Const
	ccStartToken  = '<<';
	ccEndToken    = '>>';
	ccImpulse     = 10;

Type

        { TMarkovDict }

        TMarkovDict = Class
	Private
		fName   : String;
		fWords  : TStringList;
                function GetCount: Integer;
                function GetMarkovCount: Integer;
                function GetTransitions(index: integer): TStringList;
	Public
		Constructor Create(Arq : String);
		Destructor Destroy; Override;
		Procedure Load;
		Procedure Flush;
		Function InsertWord(St : AnsiString): Integer;
		Function FindWord(St : AnsiString): Integer;
		Procedure ImpulsePair(S1, S2 : AnsiString);
		Property Words : TStringList Read fWords Write fWords;
                Property Transitions[index: integer]: TStringList read GetTransitions;
		Property Count : Integer Read GetCount;
                Property MarkovCount: Integer read GetMarkovCount;
	End;

Implementation

function TMarkovDict.GetCount: Integer;
begin
  Result:=fWords.Count;
end;

function TMarkovDict.GetMarkovCount: Integer;
var
  i: integer;
  TransitionList: TStringList;
begin
  Result:=0;
  for i := 0 to fWords.Count-1 do begin
    TransitionList := Transitions[i];
    if assigned(TransitionList) then
      Result := Result + TransitionList.Count;
  end;
end;

function TMarkovDict.GetTransitions(index: integer): TStringList;
begin
  Result := TStringList(fWords.Objects[index]);
end;

Constructor TMarkovDict.Create(Arq : String);
Begin
	Inherited Create;
	fName := Arq;
        fWords := TStringList.Create;
        fWords.Sorted := true;
        fWords.Duplicates := dupIgnore;
End;

Destructor TMarkovDict.Destroy;
var
  i: Integer;
Begin
	fName := '';
        for i := 0 to fWords.Count -1 do
          if Assigned(Transitions[i]) then
            Transitions[i].Free;
        fWords.Free;
	Inherited Destroy;
End;

// Loads the list from a file
Procedure TMarkovDict.Load;
Var 
  Handler : TextFile;
  Word: string;
  Line    : String;
  NewTransitions: TStringList;
  HitCount: ptrint;
Begin
  AssignFile(Handler, fName);
  Reset(Handler);
  While Not(Eof(Handler)) Do
  Begin
    ReadLn(Handler, Word);
    readln(Handler, Line);
    if length(Line)>0 then begin
      NewTransitions := TStringList.Create;
      repeat
        readln(Handler, HitCount);
        NewTransitions.AddObject(Line, TObject(HitCount));
        readln(Handler, line);
      until length(line)=0;
    end
    else
      NewTransitions := nil;
    FWords.AddObject(Word, NewTransitions);
  End;
  CloseFile(Handler);
End;

// Saves the list to a file
Procedure TMarkovDict.Flush;
var
  w1, w2: integer;
  MarkovText: TextFile;
  Transition: TStringList;
begin
  assign(MarkovText, fName);
  rewrite(MarkovText);
  for w1 := 0 to fWords.Count-1 do begin
    writeln(MarkovText, fWords[w1]);
    Transition:=Transitions[w1];
    if assigned(Transition) then begin
      for w2 := 0 to Transition.Count -1 do begin
        writeln(MarkovText, Transition[w2]);
        writeln(MarkovText, ptrint(Transition.Objects[w2]));
      end;
    end;
    writeln(MarkovText);
  end;
  closefile(MarkovText);
End;

// adds a new word to the list returning his value or 
// returns the value of a word if it is already in the list
Function TMarkovDict.InsertWord(St : AnsiString): Integer;
Begin
  Result := FWords.Add(st);
End;

// Finds a word in the list, returning -1 if the word
// isn't already there
Function TMarkovDict.FindWord(St : AnsiString): Integer;
Begin
  FindWord := fWords.IndexOf(St);
End;

procedure TMarkovDict.ImpulsePair(S1, S2: AnsiString);
var
  W1, W2: integer;
  TransitionList: TStringList;
begin
  W1 := FWords.IndexOf(S1);
  if W1<0 then
    W1 := FWords.Add(S1);
  TransitionList := Transitions[W1];
  if not assigned(TransitionList) then begin
    TransitionList := TStringList.Create;
    FWords.Objects[W1] := TransitionList;
  end;
  W2 := TransitionList.IndexOf(S2);
  if W2<0 then
    W2 := TransitionList.Add(S2);
  TransitionList.Objects[w2] :=
    TObject(ptrint(TransitionList.Objects[w2])+ccImpulse);
end;

End.
