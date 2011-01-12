{
    This file is part of the Free Component Library

    Pascal parse tree classes
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

unit PasTree;

interface

uses Classes;

resourcestring
  // Parse tree node type names
  SPasTreeElement = 'generic element';
  SPasTreeSection = 'unit section';
  SPasTreeModule = 'module';
  SPasTreePackage = 'package';
  SPasTreeResString = 'resource string';
  SPasTreeType = 'generic type';
  SPasTreePointerType = 'pointer type';
  SPasTreeAliasType = 'alias type';
  SPasTreeTypeAliasType = '"type" alias type';
  SPasTreeClassOfType = '"class of" type';
  SPasTreeRangeType = 'range type';
  SPasTreeArrayType = 'array type';
  SPasTreeFileType = 'file type';
  SPasTreeEnumValue = 'enumeration value';
  SPasTreeEnumType = 'enumeration type';
  SPasTreeSetType = 'set type';
  SPasTreeRecordType = 'record type';
  SPasTreeObjectType = 'object';
  SPasTreeClassType = 'class';
  SPasTreeInterfaceType = 'interface';
  SPasTreeArgument = 'argument';
  SPasTreeProcedureType = 'procedure type';
  SPasTreeResultElement = 'function result';
  SPasTreeConstructorType = 'constructor type';
  SPasTreeDestructorType = 'destructor type';
  SPasTreeFunctionType = 'function type';
  SPasTreeUnresolvedTypeRef = 'unresolved type reference';
  SPasTreeVariable = 'variable';
  SPasTreeConst = 'constant';
  SPasTreeProperty = 'property';
  SPasTreeOverloadedProcedure = 'overloaded procedure';
  SPasTreeProcedure = 'procedure';
  SPasTreeFunction = 'function';
  SPasTreeClassProcedure = 'class procedure';
  SPasTreeClassFunction = 'class function';
  SPasTreeConstructor = 'constructor';
  SPasTreeDestructor = 'destructor';
  SPasTreeProcedureImpl = 'procedure/function implementation';
  SPasTreeConstructorImpl = 'constructor implementation';
  SPasTreeDestructorImpl = 'destructor implementation';

type

  // Visitor pattern.
  TPassTreeVisitor = class;

  TPasElementBase = class
    procedure Accept(Visitor: TPassTreeVisitor); virtual; abstract;
  end;


  TPasModule = class;

  TPasMemberVisibility = (visDefault, visPrivate, visProtected, visPublic,
    visPublished, visAutomated,
    visStrictPrivate, visStrictProtected);

  TCallingConvention = (ccDefault,ccRegister,ccPascal,ccCDecl,ccStdCall,ccOldFPCCall,ccSafeCall);

  TPasMemberVisibilities = set of TPasMemberVisibility;
  TPasMemberHint = (hDeprecated,hLibrary,hPlatform,hExperimental,hUnimplemented);
  TPasMemberHints = set of TPasMemberHint; 

  TPTreeElement = class of TPasElement;

  { TPasElement }

  TPasElement = class(TPasElementBase)
  private
    FRefCount: LongWord;
    FName: string;
    FParent: TPasElement;
    FHints : TPasMemberHints;
  protected
    procedure ProcessHints(const ASemiColonPrefix: boolean; var AResult: string); virtual;
  public
    SourceFilename: string;
    SourceLinenumber: Integer;
    Visibility: TPasMemberVisibility;
  public
    constructor Create(const AName: string; AParent: TPasElement); virtual;
    procedure AddRef;
    procedure Release;
    function FullName: string;          // Name including parent's names
    function PathName: string;          // = Module.Name + FullName
    function GetModule: TPasModule;
    function ElementTypeName: string; virtual;
    function GetDeclaration(full : Boolean) : string; virtual;
    procedure Accept(Visitor: TPassTreeVisitor); override;
    property RefCount: LongWord read FRefCount;
    property Name: string read FName write FName;
    property Parent: TPasElement read FParent;
    Property Hints : TPasMemberHints Read FHints Write FHints;
  end;

  TPasExprKind = (pekIdent, pekNumber, pekString, pekSet, pekNil, pekBoolConst, pekRange,
     pekUnary, pekBinary, pekFuncParams, pekArrayParams, pekListOfExp);

  TExprOpCode = (eopNone,
                 eopAdd,eopSubtract,eopMultiply,eopDivide, eopDiv,eopMod, eopPower,// arithmetic
                 eopShr,eopSHl, // bit operations
                 eopNot,eopAnd,eopOr,eopXor, // logical/bit
                 eopEqual, eopNotEqual,  // Logical
                 eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual, // ordering
                 eopIn,eopIs,eopAs, eopSymmetricaldifference, // Specials
                 eopAddress, eopDeref, // Pointers
                 eopSubIdent); // SomeRec.A, A is subIdent of SomeRec

  { TPasExpr }

  TPasExpr = class(TPasElement)
    Kind      : TPasExprKind;
    OpCode    : TexprOpcode;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; AOpCode: TexprOpcode); virtual; overload;
  end;

  TUnaryExpr = class(TPasExpr)
    Operand   : TPasExpr;
    constructor Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TPasExpr)
    left      : TPasExpr;
    right     : TPasExpr;
    constructor Create(AParent : TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode); overload;
    constructor CreateRange(AParent : TPasElement; xleft, xright: TPasExpr); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
  end;

  TPrimitiveExpr = class(TPasExpr)
    Value     : AnsiString;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : Ansistring); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;
  
  TBoolConstExpr = class(TPasExpr)
    Value     : Boolean;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TNilExpr }

  TNilExpr = class(TPasExpr)
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TParamsExpr }

  TParamsExpr = class(TPasExpr)
    Value     : TPasExpr;
    Params    : array of TPasExpr;
    {pekArray, pekFuncCall, pekSet}
    constructor Create(AParent : TPasElement; AKind: TPasExprKind); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
    procedure AddParam(xp: TPasExpr);
  end;

  { TRecordValues }

  TRecordValuesItem = record
    Name      : AnsiString;
    ValueExp  : TPasExpr;
  end;

  TRecordValues = class(TPasExpr)
    Fields    : array of TRecordValuesItem;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure AddField(const AName: AnsiString; Value: TPasExpr);
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TArrayValues }

  TArrayValues = class(TPasExpr)
    Values    : array of TPasExpr;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure AddValues(AValue: TPasExpr);
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TPasDeclarations }

  TPasDeclarations = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
  public
    Declarations, ResStrings, Types, Consts, Classes,
    Functions, Variables, Properties: TList;
  end;

  { TPasSection }

  TPasSection = class(TPasDeclarations)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddUnitToUsesList(const AUnitName: string);
  public
    UsesList: TList;            // TPasUnresolvedTypeRef or TPasModule elements
  end;

  { TInterfaceSection }

  TInterfaceSection = class(TPasSection)
  end;

  { TImplementationSection }

  TImplementationSection = class(TPasSection)
  end;

  TProgramSection = class(TPasSection)
  end;

  TInitializationSection = class;
  TFinalizationSection = class;

  { TPasModule }

  TPasModule = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    InterfaceSection: TInterfaceSection;
    ImplementationSection: TImplementationSection;
    InitializationSection: TInitializationSection;
    FinalizationSection: TFinalizationSection;
    PackageName: string;
    Filename   : String;  // the IN filename, only written when not empty.
  end;

  { TPasProgram }

  TPasProgram = class(TPasModule);

  { TPasPackage }

  TPasPackage = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
  public
    Modules: TList;     // List of TPasModule objects
  end;

  { TPasResString }

  TPasResString = class(TPasElement)
  public
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean) : string; Override;
  public
    Value: string;
  end;

  { TPasType }

  TPasType = class(TPasElement)
  public
    function ElementTypeName: string; override;
  end;

  { TPasPointerType }

  TPasPointerType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean): string; override;
  public
    DestType: TPasType;
  end;

  { TPasAliasType }

  TPasAliasType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean): string; override;
  public
    DestType: TPasType;
  end;

  { TPasTypeAliasType }

  TPasTypeAliasType = class(TPasAliasType)
  public
    function ElementTypeName: string; override;
  end;

  { TPasClassOfType }

  TPasClassOfType = class(TPasAliasType)
  public
    function ElementTypeName: string; override;
    function GetDeclaration(full: boolean) : string; override;
  end;


  { TPasRangeType }

  TPasRangeType = class(TPasType)
  public
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    RangeStart, RangeEnd: string;
  end;

  { TPasArrayType }

  TPasArrayType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    IndexRange : string;
    IsPacked : Boolean;          // 12/04/04 - Dave - Added
    ElType: TPasType;
  end;

  { TPasFileType }

  TPasFileType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    ElType: TPasType;
  end;

  { TPasEnumValue }

  TPasEnumValue = class(TPasElement)
  public
    function ElementTypeName: string; override;
  public
    IsValueUsed: Boolean;
    Value: Integer;
    AssignedValue : string;
  end;

  { TPasEnumType }

  TPasEnumType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
     function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    Procedure GetEnumNames(Names : TStrings);
  public
    Values: TList;      // List of TPasEnumValue objects
  end;

  { TPasSetType }

  TPasSetType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    EnumType: TPasType;
  end;

  TPasRecordType = class;

  { TPasVariant }

  TPasVariant = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  public
    Values: TStringList;
    Members: TPasRecordType;
  end;

  { TPasRecordType }

  TPasRecordType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    IsPacked: Boolean;
    IsBitPacked : Boolean;
    Members: TList;     // array of TPasVariable elements
    VariantName: string;
    VariantType: TPasType;
    Variants: TList;	// array of TPasVariant elements, may be nil!
  end;


  TPasObjKind = (okObject, okClass, okInterface);

  { TPasClassType }

  TPasClassType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
  public
    ObjKind: TPasObjKind;
    AncestorType: TPasType;     // TPasClassType or TPasUnresolvedTypeRef
    IsPacked: Boolean;        // 12/04/04 - Dave - Added
    IsForward : Boolean;
    Members: TList;     // array of TPasElement objects
    InterfaceGUID : string; // 15/06/07 - Inoussa

    ClassVars: TList;   // class vars
    Modifiers: TStringList;
    Interfaces : TList;
  end;

  TArgumentAccess = (argDefault, argConst, argVar, argOut);

  { TPasArgument }

  TPasArgument = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    Access: TArgumentAccess;
    ArgType: TPasType;
    Value: string;
  end;

  { TPasProcedureType }

  TPasProcedureType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    class function TypeName: string; virtual;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure GetArguments(List : TStrings);
    function CreateArgument(const AName, AUnresolvedTypeName: string):TPasArgument;
  public
    IsOfObject: Boolean;
    Args: TList;        // List of TPasArgument objects
  end;

  { TPasResultElement }

  TPasResultElement = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName : string; override;
  public
    ResultType: TPasType;
  end;

  { TPasFunctionType }

  TPasFunctionType = class(TPasProcedureType)
  public
    destructor Destroy; override;
    class function TypeName: string; override;
    function ElementTypeName: string; override;
    function GetDeclaration(Full : boolean) : string; override;
  public
    ResultEl: TPasResultElement;
  end;

  TPasUnresolvedTypeRef = class(TPasType)
  public
    // Typerefs cannot be parented! -> AParent _must_ be NIL
    constructor Create(const AName: string; AParent: TPasElement); override;
    function ElementTypeName: string; override;
  end;

  { TPasTypeRef }

  TPasTypeRef = class(TPasUnresolvedTypeRef)
  public
  public
    // function GetDeclaration(full : Boolean): string; override;
    RefType: TPasType;
  end;

  { TPasVariable }

  TPasVariable = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    VarType: TPasType;
    Value: string;
    Modifiers : string;
    AbsoluteLocation : String;
    Expr: TPasExpr;
  end;

  { TPasConst }

  TPasConst = class(TPasVariable)
  public
  public
    function ElementTypeName: string; override;
  end;

  { TPasProperty }

  TPasProperty = class(TPasVariable)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
  public
    Args: TList;        // List of TPasArgument objects
    IndexValue, ReadAccessorName, WriteAccessorName,ImplementsName,
      StoredAccessorName, DefaultValue: string;
    IsDefault, IsNodefault: Boolean;
  end;

  { TPasProcedureBase }

  TPasProcedureBase = class(TPasElement)
  public
    function TypeName: string; virtual; abstract;
  end;

  { TPasOverloadedProc }

  TPasOverloadedProc = class(TPasProcedureBase)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; override;
  public
    Overloads: TList;           // List of TPasProcedure nodes
  end;

  TProcedureModifier = (pmVirtual, pmDynamic, pmAbstract, pmOverride,
                        pmExported, pmOverload, pmMessage, pmReintroduce,
                        pmStatic,pmInline,pmAssembler,pmVarargs,
                        pmCompilerProc,pmExternal,pmExtdecl,pmForward);
  TProcedureModifiers = Set of TProcedureModifier;
  TProcedureMessageType = (pmtInteger,pmtString);
                        
  TProcedureBody = class;

  TPasProcedure = class(TPasProcedureBase)
  Private
    FCallingConvention : TCallingConvention;
    FModifiers : TProcedureModifiers;
    FMessageName : String;
    FMessageType : TProcedureMessageType;
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration(full: Boolean): string; override;
    procedure GetModifiers(List: TStrings);
  public
    ProcType : TPasProcedureType;
    Body : TProcedureBody;
    Procedure AddModifier(AModifier : TProcedureModifier);
    Function IsVirtual : Boolean;
    Function IsDynamic : Boolean;
    Function IsAbstract : Boolean;
    Function IsOverride : Boolean;
    Function IsExported : Boolean;
    Function IsExternal : Boolean;
    Function IsOverload : Boolean;
    Function IsMessage: Boolean;
    Function IsReintroduced : Boolean;
    Function IsStatic : Boolean;
    Function IsForward: Boolean;
    Property Modifiers : TProcedureModifiers Read FModifiers Write FModifiers;
    Property CallingConvention : TCallingConvention Read FCallingConvention Write FCallingConvention;
    Property MessageName : String Read FMessageName Write FMessageName;
    property MessageType : TProcedureMessageType Read FMessageType Write FMessageType;
  end;

  TPasFunction = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration (full : boolean) : string; override;
  end;

  { TPasOperator }

  TPasOperator = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration (full : boolean) : string; override;
  end;

  { TPasConstructor }

  TPasConstructor = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasDestructor }

  TPasDestructor = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassProcedure }

  TPasClassProcedure = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassFunction }

  TPasClassFunction = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  TPasImplBlock = class;

  { TProcedureBody - the var+type+const+begin, without the header, child of TPasProcedure }

  TProcedureBody = class(TPasDeclarations)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  public
    Labels: TList;
    Body: TPasImplBlock;
  end;

  { TPasProcedureImpl - used by mkxmlrpc, not by pparser }

  TPasProcedureImpl = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; virtual;
  public
    ProcType: TPasProcedureType;
    Locals: TList;
    Body: TPasImplBlock;
  end;

  { TPasConstructorImpl - used by mkxmlrpc, not by pparser }

  TPasConstructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasDestructorImpl - used by mkxmlrpc, not by pparser }

  TPasDestructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasImplElement - implementation element }

  TPasImplElement = class(TPasElement)
  end;

  { TPasImplCommand }

  TPasImplCommand = class(TPasImplElement)
  public
    Command: string;
  end;

  { TPasImplCommands - used by mkxmlrpc, not used by pparser }

  TPasImplCommands = class(TPasImplElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  public
    Commands: TStrings;
  end;

  { TPasLabels }

  TPasLabels = class(TPasImplElement)
  public
    Labels  : TStrings;
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  end;

  TPasImplBeginBlock = class;
  TPasImplRepeatUntil = class;
  TPasImplIfElse = class;
  TPasImplWhileDo = class;
  TPasImplWithDo = class;
  TPasImplCaseOf = class;
  TPasImplForLoop = class;
  TPasImplTry = class;
  TPasImplExceptOn = class;
  TPasImplRaise = class;
  TPasImplAssign = class;
  TPasImplSimple = class;
  TPasImplLabelMark = class;

  { TPasImplBlock }

  TPasImplBlock = class(TPasImplElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); virtual;
    function AddCommand(const ACommand: string): TPasImplCommand;
    function AddCommands: TPasImplCommands; // used by mkxmlrpc, not by pparser
    function AddBeginBlock: TPasImplBeginBlock;
    function AddRepeatUntil: TPasImplRepeatUntil;
    function AddIfElse(const ACondition: string): TPasImplIfElse;
    function AddWhileDo(const ACondition: string): TPasImplWhileDo;
    function AddWithDo(const Expression: string): TPasImplWithDo;
    function AddCaseOf(const Expression: string): TPasImplCaseOf;
    function AddForLoop(AVar: TPasVariable;
      const AStartValue, AEndValue: string): TPasImplForLoop;
    function AddForLoop(const AVarName, AStartValue, AEndValue: string;
      ADownTo: Boolean = false): TPasImplForLoop;
    function AddTry: TPasImplTry;
    function AddExceptOn(const VarName, TypeName: string): TPasImplExceptOn;
    function AddRaise: TPasImplRaise;
    function AddLabelMark(const Id: string): TPasImplLabelMark;
    function AddAssign(left, right: TPasExpr): TPasImplAssign;
    function AddSimple(exp: TPasExpr): TPasImplSimple;
    function CloseOnSemicolon: boolean; virtual;
  public
    Elements: TList;    // TPasImplElement objects
  end;

  { TPasImplStatement }

  TPasImplStatement = class(TPasImplBlock)
  public
    function CloseOnSemicolon: boolean; override;
  end;

  { TPasImplBeginBlock }

  TPasImplBeginBlock = class(TPasImplBlock)
  end;

  { TInitializationSection }

  TInitializationSection = class(TPasImplBlock)
  end;

  { TFinalizationSection }

  TFinalizationSection = class(TPasImplBlock)
  end;

  { TPasImplRepeatUntil }

  TPasImplRepeatUntil = class(TPasImplBlock)
  public
    Condition: string;
  end;

  { TPasImplIfElse }

  TPasImplIfElse = class(TPasImplBlock)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    function CloseOnSemicolon: boolean; override;
  public
    Condition: string;
    IfBranch: TPasImplElement;
    ElseBranch: TPasImplElement; // can be nil
  end;

  { TPasImplWhileDo }

  TPasImplWhileDo = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
  public
    Condition: string;
    Body: TPasImplElement;
  end;

  { TPasImplWithDo }

  TPasImplWithDo = class(TPasImplStatement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expression: string);
  public
    Expressions: TStrings;
    Body: TPasImplElement;
  end;

  TPasImplCaseStatement = class;
  TPasImplCaseElse = class;

  { TPasImplCaseOf }

  TPasImplCaseOf = class(TPasImplBlock)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    function AddCase(const Expression: string): TPasImplCaseStatement;
    function AddElse: TPasImplCaseElse;
  public
    Expression: string;
    ElseBranch: TPasImplCaseElse;
  end;

  { TPasImplCaseStatement }

  TPasImplCaseStatement = class(TPasImplStatement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expr: string);
  public
    Expressions: TStrings;
    Body: TPasImplElement;
  end;

  { TPasImplCaseElse }

  TPasImplCaseElse = class(TPasImplBlock)
  end;

  { TPasImplForLoop }

  TPasImplForLoop = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
  public
    Variable: TPasVariable;
    VariableName, StartValue, EndValue: string;
    Down: boolean; // downto
    Body: TPasImplElement;
  end;

  { TPasImplAssign }

  TPasImplAssign = class (TPasImplStatement)
  public
    left  : TPasExpr;
    right : TPasExpr;
  end;

  { TPasImplSimple }

  TPasImplSimple = class (TPasImplStatement)
  public
    expr  : TPasExpr;
  end;

  TPasImplTryHandler = class;
  TPasImplTryFinally = class;
  TPasImplTryExcept = class;
  TPasImplTryExceptElse = class;

  { TPasImplTry }

  TPasImplTry = class(TPasImplBlock)
  public
    destructor Destroy; override;
    function AddFinally: TPasImplTryFinally;
    function AddExcept: TPasImplTryExcept;
    function AddExceptElse: TPasImplTryExceptElse;
  public
    FinallyExcept: TPasImplTryHandler;
    ElseBranch: TPasImplTryExceptElse;
  end;

  TPasImplTryHandler = class(TPasImplBlock)
  end;

  { TPasImplTryFinally }

  TPasImplTryFinally = class(TPasImplTryHandler)
  end;

  { TPasImplTryExcept }

  TPasImplTryExcept = class(TPasImplTryHandler)
  end;

  { TPasImplTryExceptElse }

  TPasImplTryExceptElse = class(TPasImplTryHandler)
  end;

  { TPasImplExceptOn }

  TPasImplExceptOn = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
  public
    VariableName, TypeName: string;
    Body: TPasImplElement;
  end;

  { TPasImplRaise }

  TPasImplRaise = class(TPasImplStatement)
  end;

  { TPassTreeVisitor }

  TPassTreeVisitor = class
    procedure Visit(obj: TPasElement); virtual;
  end;

  TPasImplLabelMark = class(TPasImplElement)
  public
    LabelId:  AnsiString;
  end;

const
  AccessNames: array[TArgumentAccess] of string[6] = ('', 'const ', 'var ', 'out ');
  AllVisibilities: TPasMemberVisibilities =
     [visDefault, visPrivate, visProtected, visPublic,
      visPublished, visAutomated];

  VisibilityNames: array[TPasMemberVisibility] of string = (
    'default', 'private', 'protected', 'public', 'published', 'automated','strict private', 'strict protected');

  ObjKindNames: array[TPasObjKind] of string = (
    'object', 'class', 'interface');
  
  OpcodeStrings : Array[TExprOpCode] of string = 
       ('','+','-','*','/','div','mod','**',
        'shr','shl',
        'not','and','or','xor',
        '=','<>',
        '<','>','<=','>=',
        'in','is','as','><',
        '@','^',
        '.');

  cPasMemberHint : array[TPasMemberHint] of string =
      ( 'deprecated', 'library', 'platform', 'experimental', 'unimplemented' );

implementation

uses SysUtils;

{$IFNDEF FPC}
  const
    LineEnding = sLineBreak;
{$ENDIF}

{ Parse tree element type name functions }

function TPasElement.ElementTypeName: string; begin Result := SPasTreeElement end;
function TPasDeclarations.ElementTypeName: string; begin Result := SPasTreeSection end;
function TPasModule.ElementTypeName: string; begin Result := SPasTreeModule end;
function TPasPackage.ElementTypeName: string; begin Result := SPasTreePackage end;
function TPasResString.ElementTypeName: string; begin Result := SPasTreeResString end;
function TPasType.ElementTypeName: string; begin Result := SPasTreeType end;
function TPasPointerType.ElementTypeName: string; begin Result := SPasTreePointerType end;
function TPasAliasType.ElementTypeName: string; begin Result := SPasTreeAliasType end;
function TPasTypeAliasType.ElementTypeName: string; begin Result := SPasTreeTypeAliasType end;
function TPasClassOfType.ElementTypeName: string; begin Result := SPasTreeClassOfType end;
function TPasRangeType.ElementTypeName: string; begin Result := SPasTreeRangeType end;
function TPasArrayType.ElementTypeName: string; begin Result := SPasTreeArrayType end;
function TPasFileType.ElementTypeName: string; begin Result := SPasTreeFileType end;
function TPasEnumValue.ElementTypeName: string; begin Result := SPasTreeEnumValue end;
function TPasEnumType.ElementTypeName: string; begin Result := SPasTreeEnumType end;
function TPasSetType.ElementTypeName: string; begin Result := SPasTreeSetType end;
function TPasRecordType.ElementTypeName: string; begin Result := SPasTreeRecordType end;
function TPasArgument.ElementTypeName: string; begin Result := SPasTreeArgument end;
function TPasProcedureType.ElementTypeName: string; begin Result := SPasTreeProcedureType end;
function TPasResultElement.ElementTypeName: string; begin Result := SPasTreeResultElement end;
function TPasFunctionType.ElementTypeName: string; begin Result := SPasTreeFunctionType end;
function TPasUnresolvedTypeRef.ElementTypeName: string; begin Result := SPasTreeUnresolvedTypeRef end;
function TPasVariable.ElementTypeName: string; begin Result := SPasTreeVariable end;
function TPasConst.ElementTypeName: string; begin Result := SPasTreeConst end;
function TPasProperty.ElementTypeName: string; begin Result := SPasTreeProperty end;
function TPasOverloadedProc.ElementTypeName: string; begin Result := SPasTreeOverloadedProcedure end;
function TPasProcedure.ElementTypeName: string; begin Result := SPasTreeProcedure end;
function TPasFunction.ElementTypeName: string; begin Result := SPasTreeFunction end;
function TPasClassProcedure.ElementTypeName: string; begin Result := SPasTreeClassProcedure; end;
function TPasClassFunction.ElementTypeName: string; begin Result := SPasTreeClassFunction; end;
function TPasOperator.ElementTypeName: string; begin Result := SPasTreeFunction end;
function TPasConstructor.ElementTypeName: string; begin Result := SPasTreeConstructor end;
function TPasDestructor.ElementTypeName: string; begin Result := SPasTreeDestructor end;
function TPasProcedureImpl.ElementTypeName: string; begin Result := SPasTreeProcedureImpl end;
function TPasConstructorImpl.ElementTypeName: string; begin Result := SPasTreeConstructorImpl end;
function TPasDestructorImpl.ElementTypeName: string; begin Result := SPasTreeDestructorImpl end;

function TPasClassType.ElementTypeName: string;
begin
  case ObjKind of
    okObject: Result := SPasTreeObjectType;
    okClass: Result := SPasTreeClassType;
    okInterface: Result := SPasTreeInterfaceType;
  end;
end;



{ All other stuff: }

procedure TPasElement.ProcessHints(const ASemiColonPrefix: boolean; var AResult: string);
var
  h: TPasMemberHint;
begin
  if Hints <> [] then
  begin
    if ASemiColonPrefix then
      AResult := AResult + ';';
    for h := Low(TPasMemberHint) to High(TPasMemberHint) do
    begin
      if h in Hints then
        AResult := AResult + ' ' + cPasMemberHint[h] + ';'
    end;
  end;
end;

constructor TPasElement.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
end;

procedure TPasElement.AddRef;
begin
  Inc(FRefCount);
end;

procedure TPasElement.Release;
begin
  if FRefCount = 0 then
    Free
  else
    Dec(FRefCount);
end;

function TPasElement.FullName: string;
var
  p: TPasElement;
begin
  Result := Name;
  p := Parent;
  while Assigned(p) and not p.InheritsFrom(TPasDeclarations) do
  begin
    if (p.ClassType <> TPasOverloadedProc) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.PathName: string;
var
  p: TPasElement;
begin
  Result := Name;
  p := Parent;
  while Assigned(p) do
  begin
    if (p.ClassType <> TPasOverloadedProc) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.GetModule: TPasModule;
begin
  if ClassType = TPasPackage then
    Result := nil
  else
  begin
    Result := TPasModule(Self);
    while Assigned(Result) and not (Result.ClassType = TPasModule) do
      Result := TPasModule(Result.Parent);
  end;
end;

function TPasElement.GetDeclaration (full : boolean): string;

begin
  if Full then
    Result := Name
  else
    Result := '';
end;

procedure TPasElement.Accept(Visitor: TPassTreeVisitor);
begin
  Visitor.Visit(Self);
end;

constructor TPasDeclarations.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Declarations := TList.Create;
  ResStrings := TList.Create;
  Types := TList.Create;
  Consts := TList.Create;
  Classes := TList.Create;
  Functions := TList.Create;
  Variables := TList.Create;
  Properties := TList.Create;
end;

destructor TPasDeclarations.Destroy;
var
  i: Integer;
begin
  Variables.Free;
  Functions.Free;
  Classes.Free;
  Consts.Free;
  Types.Free;
  ResStrings.Free;
  Properties.Free;
  for i := 0 to Declarations.Count - 1 do
    TPasElement(Declarations[i]).Release;
  Declarations.Free;

  inherited Destroy;
end;

destructor TPasModule.Destroy;
begin
  if Assigned(InterfaceSection) then
    InterfaceSection.Release;
  if Assigned(ImplementationSection) then
    ImplementationSection.Release;
  inherited Destroy;
end;


constructor TPasPackage.Create(const AName: string; AParent: TPasElement);
begin
  if (Length(AName) > 0) and (AName[1] <> '#') then
    inherited Create('#' + AName, AParent)
  else
    inherited Create(AName, AParent);
  Modules := TList.Create;
end;

destructor TPasPackage.Destroy;
var
  i: Integer;
begin
  for i := 0 to Modules.Count - 1 do
    TPasModule(Modules[i]).Release;
  Modules.Free;
  inherited Destroy;
end;


destructor TPasPointerType.Destroy;
begin
  if Assigned(DestType) then
    DestType.Release;
  inherited Destroy;
end;


destructor TPasAliasType.Destroy;
begin
  if Assigned(DestType) then
    DestType.Release;
  inherited Destroy;
end;


destructor TPasArrayType.Destroy;
begin
  if Assigned(ElType) then
    ElType.Release;
  inherited Destroy;
end;

destructor TPasFileType.Destroy;
begin
  if Assigned(ElType) then
    ElType.Release;
  inherited Destroy;
end;


constructor TPasEnumType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TList.Create;
end;

destructor TPasEnumType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Values.Count - 1 do
    TPasEnumValue(Values[i]).Release;
  Values.Free;
  inherited Destroy;
end;

procedure TPasEnumType.GetEnumNames(Names: TStrings);
var
  i: Integer;
begin
  with Values do
  begin
    for i := 0 to Count - 2 do
      Names.Add(TPasEnumValue(Items[i]).Name + ',');
    if Count > 0 then
      Names.Add(TPasEnumValue(Items[Count - 1]).Name);
  end;
end;


destructor TPasSetType.Destroy;
begin
  if Assigned(EnumType) then
    EnumType.Release;
  inherited Destroy;
end;


constructor TPasVariant.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TStringList.Create;
end;

destructor TPasVariant.Destroy;
begin
  Values.Free;
  if Assigned(Members) then
    Members.Release;
  inherited Destroy;
end;


constructor TPasRecordType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Members := TList.Create;
end;

destructor TPasRecordType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Members.Count - 1 do
    TPasVariable(Members[i]).Release;
  Members.Free;

  if Assigned(VariantType) then
    VariantType.Release;

  if Assigned(Variants) then
  begin
    for i := 0 to Variants.Count - 1 do
      TPasVariant(Variants[i]).Release;
    Variants.Free;
  end;

  inherited Destroy;
end;


constructor TPasClassType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  IsPacked := False;                     // 12/04/04 - Dave - Added
  Members := TList.Create;
  Modifiers := TStringList.Create;
  ClassVars := TList.Create;
  Interfaces:= TList.Create;
end;

destructor TPasClassType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Members.Count - 1 do
    TPasElement(Members[i]).Release;
  Members.Free;
  if Assigned(AncestorType) then
    AncestorType.Release;
  Modifiers.Free;
  ClassVars.Free;
  Interfaces.Free;
  inherited Destroy;
end;


destructor TPasArgument.Destroy;
begin
  if Assigned(ArgType) then
    ArgType.Release;
  inherited Destroy;
end;


constructor TPasProcedureType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TList.Create;
end;

destructor TPasProcedureType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  Args.Free;
  inherited Destroy;
end;

class function TPasProcedureType.TypeName: string;
begin
  Result := 'procedure';
end;

function TPasProcedureType.CreateArgument(const AName,
  AUnresolvedTypeName: string): TPasArgument;
begin
  Result := TPasArgument.Create(AName, Self);
  Args.Add(Result);
  Result.ArgType := TPasUnresolvedTypeRef.Create(AUnresolvedTypeName, Result);
end;


destructor TPasResultElement.Destroy;
begin
  if Assigned(ResultType) then
    ResultType.Release;
  inherited Destroy;
end;


destructor TPasFunctionType.Destroy;
begin
  if Assigned(ResultEl) then
    ResultEl.Release;
  inherited Destroy;
end;


class function TPasFunctionType.TypeName: string;
begin
  Result := 'function';
end;


constructor TPasUnresolvedTypeRef.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, nil);
end;


destructor TPasVariable.Destroy;
begin
  { Attention, in derived classes, VarType isn't necessarily set!
    (e.g. in Constants) }
  if Assigned(VarType) then
    VarType.Release;
  inherited Destroy;
end;


constructor TPasProperty.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TList.Create;
end;

destructor TPasProperty.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  Args.Free;
  inherited Destroy;
end;


constructor TPasOverloadedProc.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Overloads := TList.Create;
end;

destructor TPasOverloadedProc.Destroy;
var
  i: Integer;
begin
  for i := 0 to Overloads.Count - 1 do
    TPasProcedure(Overloads[i]).Release;
  Overloads.Free;
  inherited Destroy;
end;

function TPasOverloadedProc.TypeName: string;
begin
  if Assigned(TPasProcedure(Overloads[0]).ProcType) then
    Result := TPasProcedure(Overloads[0]).ProcType.TypeName
  else
    SetLength(Result, 0);
end;


destructor TPasProcedure.Destroy;
begin
  if Assigned(ProcType) then
    ProcType.Release;
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

function TPasProcedure.TypeName: string;
begin
  Result := 'procedure';
end;

constructor TPasProcedureImpl.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Locals := TList.Create;
end;

destructor TPasProcedureImpl.Destroy;
var
  i: Integer;
begin
  if Assigned(Body) then
    Body.Release;

  for i := 0 to Locals.Count - 1 do
    TPasElement(Locals[i]).Release;
  Locals.Free;

  if Assigned(ProcType) then
    ProcType.Release;

  inherited Destroy;
end;

function TPasProcedureImpl.TypeName: string;
begin
  Result := ProcType.TypeName;
end;


function TPasConstructorImpl.TypeName: string;
begin
  Result := 'constructor';
end;

function TPasDestructorImpl.TypeName: string;
begin
  Result := 'destructor';
end;


constructor TPasImplCommands.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Commands := TStringList.Create;
end;

destructor TPasImplCommands.Destroy;
begin
  Commands.Free;
  inherited Destroy;
end;


destructor TPasImplIfElse.Destroy;
begin
  if Assigned(IfBranch) then
    IfBranch.Release;
  if Assigned(ElseBranch) then
    ElseBranch.Release;
  inherited Destroy;
end;

procedure TPasImplIfElse.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if IfBranch=nil then
    IfBranch:=Element
  else if ElseBranch=nil then
    ElseBranch:=Element
  else
    raise Exception.Create('TPasImplIfElse.AddElement if and else already set - please report this bug');
end;

function TPasImplIfElse.CloseOnSemicolon: boolean;
begin
  Result:=ElseBranch<>nil;
end;

destructor TPasImplForLoop.Destroy;
begin
  if Assigned(Variable) then
    Variable.Release;
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

procedure TPasImplForLoop.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element
  else
    raise Exception.Create('TPasImplForLoop.AddElement body already set - please report this bug');
end;

constructor TPasImplBlock.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Elements := TList.Create;
end;

destructor TPasImplBlock.Destroy;
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
    TPasImplElement(Elements[i]).Release;
  Elements.Free;
  inherited Destroy;
end;

procedure TPasImplBlock.AddElement(Element: TPasImplElement);
begin
  Elements.Add(Element);
end;

function TPasImplBlock.AddCommand(const ACommand: string): TPasImplCommand;
begin
  Result := TPasImplCommand.Create('', Self);
  Result.Command := ACommand;
  AddElement(Result);
end;

function TPasImplBlock.AddCommands: TPasImplCommands;
begin
  Result := TPasImplCommands.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddBeginBlock: TPasImplBeginBlock;
begin
  Result := TPasImplBeginBlock.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddRepeatUntil: TPasImplRepeatUntil;
begin
  Result := TPasImplRepeatUntil.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddIfElse(const ACondition: string): TPasImplIfElse;
begin
  Result := TPasImplIfElse.Create('', Self);
  Result.Condition := ACondition;
  AddElement(Result);
end;

function TPasImplBlock.AddWhileDo(const ACondition: string): TPasImplWhileDo;
begin
  Result := TPasImplWhileDo.Create('', Self);
  Result.Condition := ACondition;
  AddElement(Result);
end;

function TPasImplBlock.AddWithDo(const Expression: string): TPasImplWithDo;
begin
  Result := TPasImplWithDo.Create('', Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplBlock.AddCaseOf(const Expression: string): TPasImplCaseOf;
begin
  Result := TPasImplCaseOf.Create('', Self);
  Result.Expression := Expression;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(AVar: TPasVariable; const AStartValue,
  AEndValue: string): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.Variable := AVar;
  Result.StartValue := AStartValue;
  Result.EndValue := AEndValue;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(const AVarName, AStartValue,
  AEndValue: string; ADownTo: Boolean): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.VariableName := AVarName;
  Result.StartValue := AStartValue;
  Result.EndValue := AEndValue;
  Result.Down := ADownTo;
  AddElement(Result);
end;

function TPasImplBlock.AddTry: TPasImplTry;
begin
  Result := TPasImplTry.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddExceptOn(const VarName, TypeName: string
  ): TPasImplExceptOn;
begin
  Result:=TPasImplExceptOn.Create('',Self);
  Result.VariableName:=VarName;
  Result.TypeName:=TypeName;
  AddElement(Result);
end;

function TPasImplBlock.AddRaise: TPasImplRaise;
begin
  Result:=TPasImplRaise.Create('',Self);
  AddElement(Result);
end;

function TPasImplBlock.AddLabelMark(const Id: string): TPasImplLabelMark;
begin
  Result:=TPasImplLabelMark.Create('', Self);
  Result.LabelId:=Id;
  AddElement(Result);
end;

function TPasImplBlock.AddAssign(left,right:TPasExpr):TPasImplAssign;
begin
  Result:=TPasImplAssign.Create('', Self);
  Result.left:=left;
  Result.right:=right;
end;

function TPasImplBlock.AddSimple(exp:TPasExpr):TPasImplSimple;
begin
  Result:=TPasImplSimple.Create('', Self);
  Result.expr:=exp;
end;

function TPasImplBlock.CloseOnSemicolon: boolean;
begin
  Result:=false;
end;



{ ---------------------------------------------------------------------

  ---------------------------------------------------------------------}

function TPasModule.GetDeclaration(full : boolean): string;
begin
  Result := 'Unit ' + Name;
end;

{
function TPas.GetDeclaration : string;
begin
  Result:=Name;
end;
}

function TPasResString.GetDeclaration (full : boolean) : string;
begin
  Result:=Value;
  If Full Then
    Result:=Name+' = '+Result;
end;

function TPasPointerType.GetDeclaration (full : boolean) : string;
begin
  Result:='^'+DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasAliasType.GetDeclaration (full : boolean) : string;
begin
  Result:=DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasClassOfType.GetDeclaration (full : boolean) : string;
begin
  Result:='Class of '+DestType.Name;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasRangeType.GetDeclaration (full : boolean) : string;
begin
  Result:=RangeStart+'..'+RangeEnd;
  If Full then
    Result:=Name+' = '+Result;
end;

function TPasArrayType.GetDeclaration (full : boolean) : string;
begin
  Result:='Array';
  If (IndexRange<>'') then
    Result:=Result+'['+IndexRange+']';
  Result:=Result+' of ';
  If IsPacked then
     Result := 'packed '+Result;      // 12/04/04 Dave - Added
  If Assigned(Eltype) then
    Result:=Result+ElType.Name
  else
    Result:=Result+'const';
  If Full Then
    Result:=Name+' = '+Result;
end;

function TPasFileType.GetDeclaration (full : boolean) : string;
begin
  Result:='File';
  If Assigned(Eltype) then
    Result:=Result+' of '+ElType.Name;
  If Full Then
    Result:=Name+' = '+Result;
end;

Function IndentStrings(S : TStrings; indent : Integer) : string;

Var
  I,CurrLen,CurrPos : Integer;


begin
  Result:='';
  CurrLen:=0;
  CurrPos:=0;
  For I:=0 to S.Count-1 do
    begin
    CurrLen:=Length(S[i]);
    If (CurrLen+CurrPos)>72 then
      begin
      Result:=Result+LineEnding+StringOfChar(' ',Indent);
      CurrPos:=Indent;
      end;
    Result:=Result+S[i];
    CurrPos:=CurrPos+CurrLen;
    end;
end;

function TPasEnumType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Name+' = (')
    else
      S.Add('(');
    GetEnumNames(S);
    S[S.Count-1]:=S[S.Count-1]+')';
    If Full then
      Result:=IndentStrings(S,Length(Name)+4)
    else
      Result:=IndentStrings(S,1);
  finally
    S.Free;
  end;
end;

function TPasSetType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  i : Integer;

begin
  If EnumType is TPasEnumType then
    begin
    S:=TStringList.Create;
    Try
      If Full then
        S.Add(Name+'= Set of (')
      else
        S.Add('Set of (');
      TPasEnumType(EnumType).GetEnumNames(S);
      S[S.Count-1]:=S[S.Count-1]+')';
      I:=Pos('(',S[0]);
      Result:=IndentStrings(S,i);
    finally
      S.Free;
    end;
    end
  else
    begin
    Result:='Set of '+EnumType.Name;
    If Full then
      Result:=Name+' = '+Result;
    end;
end;

function TPasRecordType.GetDeclaration (full : boolean) : string;

Var
  S,T : TStringList;
  temp : string;
  I,J : integer;

begin
  S:=TStringList.Create;
  T:=TStringList.Create;
  Try
    Temp:='record';
    If IsPacked then
      if IsBitPacked then
        Temp:='bitpacked '+Temp
      else
        Temp:='packed '+Temp;
    If Full then
      Temp:=Name+' = '+Temp;
    S.Add(Temp);
    For I:=0 to Members.Count-1 do
      begin
      Temp:=TPasVariable(Members[i]).GetDeclaration(True);
      If Pos(LineEnding,Temp)>0 then
        begin
        T.Text:=Temp;
        For J:=0 to T.Count-1 do
          if J=T.Count-1 then
            S.Add('  '+T[J]+';')
          else
            S.Add('  '+T[J])
        end
      else
        S.Add('  '+Temp+';');
      end;
    S.Add('end');
    Result:=S.Text;
    ProcessHints(False, Result);
  finally
    S.free;
    T.free;
  end;
end;

procedure TPasProcedureType.GetArguments(List : TStrings);

Var
  T : string;
  I : Integer;

begin
  For I:=0 to Args.Count-1 do
    begin
    T:=AccessNames[TPasArgument(Args[i]).Access];
    T:=T+TPasArgument(Args[i]).GetDeclaration(True);
    If I=0 then
      T:='('+T;
    If I<Args.Count-1 then
      List.Add(T+';')
    else
      List.Add(T+')');
    end;
end;

function TPasProcedureType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If IsOfObject then
      S.Add(' of object');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

function TPasFunctionType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If Assigned(ResultEl) then
      begin
      T:=' : ';
      If (ResultEl.ResultType.Name<>'') then
        T:=T+ResultEl.ResultType.Name
      else
        T:=T+ResultEl.ResultType.GetDeclaration(False);
      S.Add(T);
      end;
    If IsOfObject then
      S.Add(' of object');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

function TPasVariable.GetDeclaration (full : boolean) : string;

Const
 Seps : Array[Boolean] of Char = ('=',':');

begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    Result:=Result+Modifiers;
    if (Value<>'') then
      Result:=Result+' = '+Value;
    end
  else
    Result:=Value;
  If Full then
    Result:=Name+' '+Seps[Assigned(VarType)]+' '+Result;
end;

function TPasProperty.GetDeclaration (full : boolean) : string;

Var
  S : string;
  I : Integer;

begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    end
  else
    Result:=Value;
  S:='';
  If Assigned(Args) and (Args.Count>0) then
    begin
    For I:=0 to Args.Count-1 do
      begin
      If (S<>'') then
        S:=S+';';
      S:=S+TPasElement(Args[i]).GetDeclaration(true);
      end;
    end;
  If S<>'' then
    S:='['+S+']'
  else
    S:=' ';
  If Full then
    begin
    Result:=Name+S+': '+Result;
    If (ImplementsName<>'') then
       Result:=Result+' implements '+ImplementsName;
    end;   
  If IsDefault then
    Result:=Result+'; default';
  ProcessHints(True, Result);
end;

Procedure TPasProcedure.GetModifiers(List : TStrings);

  Procedure DoAdd(B : Boolean; S : string);

  begin
    if B then
      List.add('; '+S);
  end;

begin
  Doadd(IsVirtual,' Virtual');
  DoAdd(IsDynamic,' Dynamic');
  DoAdd(IsOverride,' Override');
  DoAdd(IsAbstract,' Abstract');
  DoAdd(IsOverload,' Overload');
  DoAdd(IsReintroduced,' Reintroduce');
  DoAdd(IsStatic,' Static');
  DoAdd(IsMessage,' Message');
end;

Procedure TPasProcedure.AddModifier(AModifier : TProcedureModifier);

begin
  Include(FModifiers,AModifier);
end;

Function TPasProcedure.IsVirtual : Boolean;
begin
  Result:=pmVirtual in FModifiers;
end;

Function TPasProcedure.IsDynamic : Boolean;
begin
  Result:=pmDynamic in FModifiers;
end;

Function TPasProcedure.IsAbstract : Boolean;
begin
  Result:=pmAbstract in FModifiers;
end;

Function TPasProcedure.IsOverride : Boolean;
begin
  Result:=pmOverride in FModifiers;
end;

Function TPasProcedure.IsExported : Boolean;
begin
  Result:=pmExported in FModifiers;
end;

function TPasProcedure.IsExternal: Boolean;
begin
  Result:=pmExternal in FModifiers;
end;

Function TPasProcedure.IsOverload : Boolean;
begin
  Result:=pmOverload in FModifiers;
end;

Function TPasProcedure.IsMessage: Boolean;
begin
  Result:=pmMessage in FModifiers;
end;

Function TPasProcedure.IsReintroduced : Boolean;
begin
  Result:=pmReintroduce in FModifiers;
end;

Function TPasProcedure.IsStatic : Boolean;

begin
  Result:=pmStatic in FModifiers;
end;

function TPasProcedure.IsForward: Boolean;
begin
  Result:=pmForward in FModifiers;
end;

function TPasProcedure.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasFunction.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+Name
        else
          T:=T+GetDeclaration(False);
        S.Add(T);
        end;
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasFunction.TypeName: string;
begin
  Result:='function';
end;

function TPasOperator.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+Name
        else
          T:=T+GetDeclaration(False);
        S.Add(T);
        end;
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasOperator.TypeName: string;
begin
  Result:='operator';
end;

function TPasClassProcedure.TypeName: string;
begin
  Result:='class procedure';
end;

function TPasClassFunction.TypeName: string;
begin
  Result:='class function';
end;

function TPasConstructor.TypeName: string;
begin
  Result:='constructor';
end;

function TPasDestructor.TypeName: string;
begin
  Result:='destructor';
end;

function TPasArgument.GetDeclaration (full : boolean) : string;
begin
  If Assigned(ArgType) then
    begin
    If ArgType.Name<>'' then
      Result:=ArgType.Name
    else
      Result:=ArgType.GetDeclaration(False);
    If Full then
      Result:=Name+': '+Result;
    end
  else If Full then
    Result:=Name
  else
    Result:='';
end;



{ TPassTreeVisitor }

procedure TPassTreeVisitor.Visit(obj: TPasElement);
begin
  // Needs to be implemented by descendents.
end;

{ TPasSection }

constructor TPasSection.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  UsesList := TList.Create;
end;

destructor TPasSection.Destroy;
var
  i: Integer;
begin
  for i := 0 to UsesList.Count - 1 do
    TPasType(UsesList[i]).Release;
  UsesList.Free;

  inherited Destroy;
end;

procedure TPasSection.AddUnitToUsesList(const AUnitName: string);
begin
  UsesList.Add(TPasUnresolvedTypeRef.Create(AUnitName, Self));
end;

{ TProcedureBody }

constructor TProcedureBody.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Labels:=TList.Create;
end;

destructor TProcedureBody.Destroy;
begin
  FreeAndNil(Labels);
  inherited Destroy;
end;

{ TPasImplWhileDo }

destructor TPasImplWhileDo.Destroy;
begin
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

procedure TPasImplWhileDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element
  else
    raise Exception.Create('TPasImplWhileDo.AddElement body already set - please report this bug');
end;

{ TPasImplCaseOf }

destructor TPasImplCaseOf.Destroy;
begin
  if Assigned(ElseBranch) then
    ElseBranch.Release;
  inherited Destroy;
end;

procedure TPasImplCaseOf.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
end;

function TPasImplCaseOf.AddCase(const Expression: string
  ): TPasImplCaseStatement;
begin
  Result:=TPasImplCaseStatement.Create('',Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplCaseOf.AddElse: TPasImplCaseElse;
begin
  Result:=TPasImplCaseElse.Create('',Self);
  ElseBranch:=Result;
  AddElement(Result);
end;

{ TPasImplCaseStatement }

constructor TPasImplCaseStatement.Create(const AName: string;
  AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TStringList.Create;
end;

destructor TPasImplCaseStatement.Destroy;
begin
  FreeAndNil(Expressions);
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

procedure TPasImplCaseStatement.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element;
end;

procedure TPasImplCaseStatement.AddExpression(const Expr: string);
begin
  Expressions.Add(Expr);
end;

{ TPasImplWithDo }

constructor TPasImplWithDo.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TStringList.Create;
end;

destructor TPasImplWithDo.Destroy;
begin
  if Assigned(Body) then
    Body.Release;
  FreeAndNil(Expressions);
  inherited Destroy;
end;

procedure TPasImplWithDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element;
end;

procedure TPasImplWithDo.AddExpression(const Expression: string);
begin
  Expressions.Add(Expression);
end;

{ TPasImplTry }

destructor TPasImplTry.Destroy;
begin
  if Assigned(FinallyExcept) then
    FinallyExcept.Release;
  if Assigned(ElseBranch) then
    ElseBranch.Release;
  inherited Destroy;
end;

function TPasImplTry.AddFinally: TPasImplTryFinally;
begin
  Result:=TPasImplTryFinally.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExcept: TPasImplTryExcept;
begin
  Result:=TPasImplTryExcept.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExceptElse: TPasImplTryExceptElse;
begin
  Result:=TPasImplTryExceptElse.Create('',Self);
  ElseBranch:=Result;
end;

{ TPasImplExceptOn }

destructor TPasImplExceptOn.Destroy;
begin
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

procedure TPasImplExceptOn.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    Body:=Element;
end;

{ TPasImplStatement }

function TPasImplStatement.CloseOnSemicolon: boolean;
begin
  Result:=true;
end;

{ TPasExpr }

constructor TPasExpr.Create(AParent : TPasElement; AKind: TPasExprKind; AOpCode: TexprOpcode);
begin
  Create(ClassName, AParent);
  Kind:=AKind;
  OpCode:=AOpCode;
end;

{ TPrimitiveExpr }

function TPrimitiveExpr.GetDeclaration(Full : Boolean):AnsiString;
begin
  Result:=Value;
end;

constructor TPrimitiveExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : Ansistring);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=AValue;
end;

{ TBoolConstExpr }

constructor TBoolConstExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=ABoolValue;
end;

Function TBoolConstExpr.GetDeclaration(Full: Boolean):AnsiString;

begin
  If Value then
    Result:='True'
  else
    Result:='False';  
end;



{ TUnaryExpr }

Function TUnaryExpr.GetDeclaration(Full : Boolean):AnsiString;

begin
  Result:=OpCodeStrings[Opcode];
  If Assigned(Operand) then
    Result:=Result+Operand.GetDeclaration(Full);
end;

constructor TUnaryExpr.Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode);
begin
  inherited Create(AParent,pekUnary, AOpCode);
  Operand:=AOperand;
end;

destructor TUnaryExpr.Destroy;
begin
  Operand.Free;
end;

{ TBinaryExpr }

function TBinaryExpr.GetDeclaration(Full : Boolean):AnsiString;

begin
  If Kind=pekRange then
    Result:='..'
  else
    Result:=' '+OpcodeStrings[Opcode]+' ';
  If Assigned(Left) then
    Result:=Left.GetDeclaration(Full)+Result;
  If Assigned(Right) then
    Result:=Result +Right.GetDeclaration(Full);
end;


constructor TBinaryExpr.Create(AParent : TPasElement; xleft,xright:TPasExpr; AOpCode:TExprOpCode);
begin
  inherited Create(AParent,pekBinary, AOpCode);
  left:=xleft;
  right:=xright;
end;

constructor TBinaryExpr.CreateRange(AParent : TPasElement; xleft,xright:TPasExpr);
begin
  inherited Create(AParent,pekRange, eopNone);
  left:=xleft;
  right:=xright;
end;

destructor TBinaryExpr.Destroy;
begin
  left.Free;
  right.Free;
  inherited Destroy;
end;

{ TParamsExpr }

Function TParamsExpr.GetDeclaration(Full: Boolean) : Ansistring;

Var
  I : Integer;

begin
  For I:=0 to Length(Params) do
    begin
    If (Result<>'')  then
      Result:=Result+', ';
    Result:=Result+Params[I].GetDeclaration(Full);  
    end;  
  Result:='('+Result+')';
end;

procedure TParamsExpr.AddParam(xp:TPasExpr);
var
  i : Integer;
begin
  i:=Length(Params);
  SetLength(Params, i+1);
  Params[i]:=xp;
end;

constructor TParamsExpr.Create(AParent : TPasElement; AKind: TPasExprKind);
begin
  inherited Create(AParent,AKind, eopNone)
end;

destructor TParamsExpr.Destroy;
var
  i : Integer;
begin
  for i:=0 to length(Params)-1 do Params[i].Free;
  inherited Destroy;
end;

{ TRecordValues }

Function TRecordValues.GetDeclaration(Full : Boolean):AnsiString;

Var
  I : Integer;
begin
  For I:=0 to Length(Fields) do
    begin
    If Result='' then
      Result:=Result+'; ';
    Result:=Result+Fields[I].Name+': '+Fields[i].ValueExp.getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

constructor TRecordValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone);
end;

destructor TRecordValues.Destroy;
var
  i : Integer;
begin
  for i:=0 to length(Fields)-1 do Fields[i].ValueExp.Free;
  inherited Destroy;
end;

procedure TRecordValues.AddField(const AName:AnsiString;Value:TPasExpr);
var
  i : Integer;
begin
  i:=length(Fields);
  SetLength(Fields, i+1);
  Fields[i].Name:=AName;
  Fields[i].ValueExp:=Value;
end;

{ TArrayValues }

Function TNilExpr.GetDeclaration(Full :Boolean):AnsiString;
begin
  Result:='Nil';
end;

Function TArrayValues.GetDeclaration(Full: Boolean):AnsiString;

Var
  I : Integer;

begin
  For I:=0 to Length(Values) do
    begin
    If Result='' then
      Result:=Result+', ';
    Result:=Result+Values[i].getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

constructor TArrayValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone)
end;

destructor TArrayValues.Destroy;
var
  i : Integer;
begin
  for i:=0 to length(Values)-1 do Values[i].Free;
  inherited Destroy;
end;

procedure TArrayValues.AddValues(AValue:TPasExpr);
var
  i : Integer;
begin
  i:=length(Values);
  SetLength(Values, i+1);
  Values[i]:=AValue;
end;

{ TNilExpr }

constructor TNilExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekNil, eopNone);
end;

{ TPasLabels }

constructor TPasLabels.Create(const AName:string;AParent:TPasElement);
begin
  inherited Create(AName,AParent);
  Labels := TStringList.Create;
end;

destructor TPasLabels.Destroy;
begin
  Labels.Free;
  inherited Destroy;
end;

end.
