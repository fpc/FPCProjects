{$mode objfpc}{$H+}

uses SysUtils, Classes, PParser, PasTree;

type
  { We have to override abstract TPasTreeContainer methods.
    See utils/fpdoc/dglobals.pp for an implementation of TFPDocEngine,
    a "real" engine. }
  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

var
  M: TPasModule;
  E: TPasTreeContainer;
  I: Integer;
  Decls: TList;
  cmdl : string;
begin
  if Paramcount<1 then
    begin
     // remember to put the whole cmdline in quotes, and
     // to always add some path options. Even if only -Fu. -Fi.
      writeln('usage: test_parser <commandline>');
      halt;
    end;
  cmdl:=paramstr(1);
  if paramcount>1 then
    for i:=2 to paramcount do
      cmdl:=cmdl+' '+paramstr(i);
  E := TSimpleEngine.Create;
  try
    try
      M := ParseSource(E, cmdl , 'linux', 'i386');
    except
      on excep:EParserError do
        begin
          writeln(excep.message,' line:',excep.row,' column:',excep.column,' file:',excep.filename); 
          raise;
       end;  
      end;      
    { Cool, we successfully parsed the unit.
      Now output some info about it. }
    Decls := M.InterfaceSection.Declarations;
    for I := 0 to Decls.Count - 1 do
      Writeln('Interface item ', I, ': ', (TObject(Decls[I]) as TPasElement).Name);

    FreeAndNil(M);
  finally 
    FreeAndNil(E) 
    end;
end.
