{
  $Id$
}

{$mode objfpc}
{$H+}

unit coptions;

interface

uses kcl,classes;

ResourceString
  // Caption
  SProjectOptions = 'Project Options';
  
  // Buttons
  SOK = 'OK';
  SCancel = 'Cancel';
  
  // names of tabs.
  STabLinker = '&Linker';
  STabDirectories = '&Directories';
  STabCompilerMessages = '&Messages';
  STabSyntax = '&Syntax';
  STabCode = 'Generated Code'  ;
  STabGeneral = 'General';
  
  // Directory items
  SSearchPaths = 'Search paths';
  SUnitSearchPath = 'Units';
  SIncludePath = 'Include files';
  SObjectSearchPath = 'Object files';
  SLibrarySearchPath = 'Libraries';  
  SToolsSearchDir = 'External binaries';
  SoutputPaths = 'Output paths';
  SOutputDir = 'Units/Executables';
  SUnitOutputDir = 'Units';
  // Verbosity
  SShowWarnings = 'Warnings';
  SShowNotes = 'Notes';
  SShowHints = 'Hints';
  SShowInfo = 'General info';
  SShowLines = 'Linenumbers';
  SShowAll = 'All';
  SShowNothing = 'Nothing';
  SShowProcedureBacktrace = 'Procedure Declarations';
  SShowUnit = 'Unit information';
  SShowDebug = 'Debug information';
  SShowTried = 'Tried files';
  SShowMacros = 'Defined macros';
  SShowProcedures = 'Procedures';
  SShowConditionals = 'Conditionals';
  SShowGCC = 'GCC/Rhide compatible output';
  SShowLogo = 'Show logo';
  SSLevel = 'Messages to show';
  SSOnOff = 'Show information';
  // Syntax options
  SSYntaxMode = 'Pascal mode';
  SSyntaxFPC = 'Normal Mode';
  SSyntaxDelphi = 'Delphi Mode';
  SSyntaxTP = 'Turbo Pascal mode';
  SSyntaxObjfpc = 'Object pascal mode';
  SSyntaxOther = 'Other syntax options'; 
  SSyntaxCOperators = 'Allow C operators';
  SSyntaxAllowGoto = 'Allow goto/label';
  SSyntaxAnsiStrings = 'Use ansistrings';
  SSyntaxInline = 'Allow inline code';
  SSyntaxMacros = 'Allow macros';
  SSyntaxConst = 'Constructor name must be init';
  SSyntaxStatic = 'Allow static keyword in objects';
  SSYntaxAsmStyle = 'Assembler reading style';
  SSyntaxAsmAtt    = 'AT&T';
  SSyntaxAsmIntel  = 'Intel';
  SSyntaxAsmDirect = 'Direct';
  // Code options
  // general code options.
  SCodeOptions = 'Code options';
  SMemSizes = 'Memory Sizes';
  SCodeHeapSize = 'Heap size';
  SCodeIOCheck = 'IO checking';
  SCodeOverFlow = 'Overflow checking';
  SCodeRangeCheck = 'Range checking';
  SCodeStackSize = 'Stack Size';
  SCodeStackCheck = 'Stack checking';
  SEnableOptimizations = 'Enable Optimizations';
  SOptimizations = 'Optimizations';
  SCodeSmaller = 'Smaller code';
  SCodeFaster = 'Faster code';
  SCodeRegister = 'Use register variables';
  SCodeUncertain = 'Uncertain optimizations';
  SCodeLevel = 'Optimization level';
  SCodeLevel1 = 'Level 1 optimizations';
  SCodeLevel2 = 'Level 2 optimizations';
  SCodeLevel3 = 'Level 3 optimizations';
  SCodeProcessor = 'Set target processor';
  SCode386 = '386/486';
  SCodePentium = 'Pentium(MMX)';
  SCodePentiumPro = 'Pentium Pro/II/c6x86/K6';
  SCodeProcType = 'Processor type';
  // Linker options
  SLinkMakeDynlib = 'Create dynamic library';
  SLinkMakeSmartLink = 'Create Smartlinked units';
  SLinkLinkSmart = 'smartlinked';
  SLinkLinkStatic = 'statically linked';
  SLinkLinkDynamic = 'dynamically linked';
  SLinkLinkerOptions = 'Extra options';
  SLinkOutputName = 'Output name';
  SLinkOmitLinking = 'Omit linking stage';
  SLinkUseClib = 'Link to C library';
  SLinkStripSymbols = 'Strip unused symbols';
  SLinkType = 'Units to use';
  SlinkOptions = 'Linker options';
    
  // general options
  SGeneralCheckUnitName = 'Omit unit name check';
  SGeneralSystemUNit = 'Compile system unit';
  SGeneralBuild = 'Build all units';
  SGeneralNoConfig = 'Skip general config file';
  SGeneralPipes = 'Use pipes';
  SGeneralBrowserInfo = 'Generate browser info';
  SGeneralLocalBrowserINfo = 'Generate local symbol info';
  SGeneralScript = 'Generate script';
  SGeneralDefines = 'Define symbols';
  SGeneralUnDefines = 'UndefineSymbols';
  SGeneralDebugInfo = 'Include Debug info';
  SGeneralDebugOptions = 'Debug options';
  SGeneralProfile = 'Include profile code';
  SGeneralgsym = 'Use gsym';
  SGeneraldbx = 'use dbx';
  SGeneralHeaptrace = 'Include heap tracing';
  SGeneralLineInfo = 'Extended backtrace info';
  SGeneralCheckPointers = 'USe pinter checks';
  SGeneralKeepAsm = 'Keep assembler file';
  SGeneralAsmInfo = 'Extra assembler info';
  SGeneralAsmListSource = 'Source lines';
  SGeneralAsmListRegAlloc = 'Register allocation';
  SGeneralAsmListTempAlloc = 'Temporary allocations';
  
Type
    TFileEdit = TEdit; // change to TCombobox when implementing histories
 
    TCompilerOptionsForm = Class(TForm)
       // Visual elements.
//       FLabel : Tlabel;
       // Directories page
       FOptionPanes : TNotebook;
       FSearchPathsGroup,
       FoutputPathsGroup : TGroupBox;
       FUnitOutputDir,
       FOutputDir,
       FToolsSearchDir,
       FIncludePath,
       FObjectSearchPath,
       FLibrarySearchPath,
       FUnitSearchPath : TFileEdit;
       // Compiler messages page
       CBShowWarnings,
       CBShowNotes,
       CBShowHints,
       CBShowInfo,
       CBShowLines,
       CBShowAll,
       CBShowNothing,
       CBShowProcedureBacktrace,
       CBShowUnit,
       CBShowDebug,
       CBShowTried,
       CBShowMacros,
       CBShowProcedures,
       CBShowConditionals,
       CBShowLogo,
       CBShowGCC : TCheckBox;
       FGeneralGroup,
       FOnOffGroup : TGroupBox;
       // Syntax options
       RBSyntaxAsmAtt, 
       RBSyntaxAsmIntel,
       RBSyntaxAsmDirect,
       RBSyntaxFPC ,
       RBSyntaxDelphi ,
       RBSyntaxTP ,
       RBSyntaxObjfpc : TRadioButton;
       GBSyntaxMode,
       GBSyntaxAsmStyle,
       GBSyntaxOther : TgroupBox;
       CBSyntaxCOperators ,
       CBSyntaxAllowGoto ,
       CBSyntaxAnsistrings, 
       CBSyntaxInline, 
       CBSyntaxMacros ,
       CBSyntaxConst ,
       CBSyntaxStatic : TCheckBox;
       // Code page
       GBCodeOptions : TGroupBox; 
       GBMemSizes : TGroupBox;
       ECodeStackSize ,
       ECodeHeapSize  : TEdit;
       CBCodeIOCheck ,
       CBCodeOverFlow ,
       CBCodeRangeCheck ,
       CBCodeStackCheck  : TCheckBox;
       GBOptimizations : TGroupBox;
       CBEnableOptimizations,
       CBCodeSmaller ,
       CBCodeFaster ,
       CBCodeRegister ,
       CBCodeUncertain : TCheckBox;
       RBCodeLevel1 ,
       RBCodeLevel2 ,
       RBCodeLevel3 : TRadioButton;
       CBCodeProcessor : TCheckBox;
       RBCode386 ,
       RBCodePentium ,
       RBCodePentiumPro : TRadioButton;
       GBProcType : TGroupBox;   
       // Linker options
       CBLinkMakeDynlib,
       CBLinkMakeSmartLink, 
       CBLinkOmitLinking ,
       CBLinkUseClib ,
       CBLinkStripSymbols : TCheckBox;
       RBLinkLinkSmart ,
       RBLinkLinkStatic ,
       RBLinkLinkDynamic : TRadioButton;
       ELinkLinkerOptions,
       ELinkOutputName : TEdit;
       GBLinkOptions : TGroupBox;
       // general page.
       CBGeneralCheckUnitName ,
       CBGeneralSystemUNit ,
       CBGeneralBuild ,
       CBGeneralNoConfig ,
       CBGeneralPipes ,
       CBGeneralBrowserInfo ,
       CBGeneralLocalBrowserINfo ,
       CBGeneralScript ,
       CBGeneralDebugInfo ,
       CBGeneralProfile ,
       CBGeneralgsym ,
       CBGeneraldbx ,
       CBGeneralHeaptrace ,
       CBGeneralLineInfo ,
       CBGeneralCheckPointers ,
       CBGeneralKeepAsm ,
       CBGeneralAsmListSource ,
       CBGeneralAsmListRegAlloc ,
       CBGeneralAsmListTempAlloc : TCheckBox;
       EGeneralDefines ,
       EGeneralUnDefines : TEdit;
       GBGeneralAsmInfo ,
       GBGeneralDebugOptions : TGroupBox;
       
       // Buttons
       OKButton,
       CancelButton : TButton;
       
       // Non-visual elements
       FCompilerOptions : TStrings;
       Function CreateDirectoryPage : TWidget;
       Function CreateVerbosityPage : TWidget;
       Function CreateSyntaxPage : TWidget;
       Function CreateCodePAge : TWidget;
       Function CreateLinkerPage : TWidget;
       Function CreateGeneralPage : TWidget;
       Function CreateButtons : TWidget;
       Procedure OnOkCancelClick (Sender : TObject);
       Procedure OnEnableOptimizationsClick (Sender : TObject);
       Procedure OnVerbosityOffClick (Sender : TObject);
       Procedure OnVerbosityOnClick (Sender : TObject);
       Procedure OnOmitLinkingClick (Sender : TObject);
       Procedure OnSetProcessorOnClick (Sender : TObject);
       Procedure SetVerbosities (OnOff : Boolean);
       Procedure FormToOptions;
       Procedure OptionsToForm;
       Procedure SetCompilerOptions (Value : TStrings);
     Public
       Constructor Create (AOwner : TComponent); Override;
       Destructor Destroy; override;  
       Property CompilerOptions : TStrings Read FCompilerOptions 
                                           Write SetCompilerOptions;
     end;

Var 
  CompilerOptionsForm : TCompilerOptionsForm;
     
Implementation

uses sysutils;

Constructor TCompilerOptionsForm.Create (AOwner : TComponent);

 Var MainLayout : TBoxLayout;
     Page : Longint;
             
begin
  Inherited Create(AOwner);
  Visible:=False;
  FCompilerOptions:=TStringList.create;
  Text := SProjectOptions;
  Name:='ProjectOptions';
//  Content:=CreateDirectoryPage;
  BorderWidth:=8;
  MainLayout:=TBoxLayout.Create(Self);
  MainLayout.Orientation:=BoxVert;
  MainLayout.Spacing:=8;
  FOptionPanes:=TNoteBook.Create(Self);
  Page:=FOptionPanes.AddPage(STabGeneral,CreateGeneralPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabDirectories,CreateDirectoryPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabCompilerMessages,CreateVerbosityPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabSyntax,CreateSyntaxPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabCode,CreateCodePage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabLinker,CreateLinkerPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
//  FoptionPanes.FinishCreation;
//  Content:=FoptionPanes;
  MainLayout.AddWidget(FoptionPanes); // ,dmClient);
  MainLayout.AddWidget(TSeparator.Create(Self));
  MainLayout.AddWidget(CreateButtons); //,dmbottom);
  Content:=MainLayout;
  SetPosition(100,100);
  SetDefaultSize(500,300);
end;

Destructor TCompilerOptionsForm.Destroy;  

begin
  FCompilerOptions.Free;
  Inherited Destroy;
end;

Function TCompilerOptionsForm.CreateDirectoryPage : TWidget;

Var Layout : TLayout;
    ALabel : Tlabel;
    
begin
  Layout:=TGridLayout.Create(Self);
  With Layout as TGridLayout do 
    begin
    Name := 'SearchLayout';
    Rows := 5;
    Columns := 2;
    HorzSpacing := 8;
    VertSpacing := 4;
    SameSizeCells:=False;
    // Unit Search path
    FUnitSearchPath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SUnitSearchPath;
    ALabel.Alignment:=taRightJustify;
    ALabel.CanExpandWidth := False;
    AddWidget(ALabel,0,0,1,1);
    AddWidget(FUnitSearchPath,1,0,1,1);
    // include search path
    FIncludePath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SIncludePath;
    ALabel.Alignment:=taRightJustify;
    AddWidget(ALabel,0,1,1,1);
    AddWidget(FIncludePath,1,1,1,1);
    // Object file search path
    FObjectSearchPath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SObjectSearchPath;
    ALabel.Alignment:=taRightJustify;
    AddWidget(ALabel,0,2,1,1);
    AddWidget(FObjectSearchPath,1,2,1,1);
    // Library search parth
    FLibrarySearchPath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SLibrarySearchPath;
    ALabel.Alignment:=taRightJustify;
    AddWidget(ALabel,0,3,1,1);
    AddWidget(FLibrarySearchPath,1,3,1,1);
    // Tools search path
    FToolsSearchDir:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SToolsSearchDir;
    ALabel.Alignment:=taRightJustify;
    AddWidget(ALabel,0,4,1,1);
    AddWidget(FToolsSearchDir,1,4,1,1);
    FinishCreation;
    end;
  FSearchPathsGroup:=TGroupBox.Create(Self);
  With FSearchPathsGroup Do
    begin
    Name:='FSearchPathsGroup';
    Text:=SSearchPaths;
    Content:=Layout;
    end;
  Layout:=TGridLayout.Create(Self);
  With Layout as TGridLAyout do
    begin 
    Name:='OutputLayout';
    Rows := 2;
    Columns := 2;
    HorzSpacing := 4;
    VertSpacing := 4;
    FOutputDir:=TFileEdit.Create(Self);
    FoutputDir.Name:='FOutputDir';
    AddWidget(FOutPutDir,1,0,1,1);
    FUnitOutputDir:=TFileEdit.Create(Self);
    FUnitOutputDir.Name:='FUnitOutputDir';
    AddWidget(FUnitOutputDir,1,1,1,1);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SOutputDir;
    ALabel.Alignment:=taRightJustify;
    Alabel.CanExpandWidth:=FAlse;
    AddWidget(ALabel,0,0,1,1);
    Alabel:=TLabel.Create(Self);
    ALabel.Alignment:=taRightJustify;
    ALabel.Text:=SUnitOutputDir;
    AddWidget(ALabel,0,1,1,1);
    end;
  FOutPutPathsGroup:=TGroupBox.Create(Self);
  With FOutputPathsGroup do
    begin
    Name:='FOutputPathsGroup';
    Text:=SOutputPaths;
    Content:=Layout;
    end;
  Result:=TGridLayout.Create(Self);
  With Result as TGridLAyout do
    begin
    Name:='DirectoriesLayout';
    Rows := 2;
    Columns := 1;
    AddWidget(FSearchPathsGroup,0,0,1,1);
    AddWidget(FoutputPathsGroup,0,1,1,1);
    end;
end;

Function TCompilerOptionsForm.CreateButtons : TWidget;

begin
  Result:=TBoxLayout.Create(Self);
  With Result as TBoxLAyout do 
    begin
    Orientation := BoxHorz;
    HorzAlign:=HorzRight;
    VertAlign:=VertCenter;
    CanExpandHeight:=False;
    Spacing:=8;
    // OKbutton;
    OKButton:=TButton.Create(Self);
    OKButton.Text:=SOK;
    OKButton.OnClick:=@OnOKCancelClick;
    AddWidget(OKButton);
    // Cancel button;
    CancelButton:=TButton.Create(Self);
    CancelButton.Text:=SCancel;
    CancelButton.OnClick:=@OnOKCancelClick;
    AddWidget(CancelButton);
    end;
end;

Function TCompilerOptionsForm.CreateVerbosityPage : TWidget;

begin
  // Create everything and set texts..
  CBShowWarnings := TCheckBox.Create(Self);
  CBShowNotes := TCheckBox.Create(Self);
  CBShowHints := TCheckBox.Create(Self);
  CBShowInfo := TCheckBox.Create(Self);
  CBShowLines := TCheckBox.Create(Self);
  CBShowAll := TCheckBox.Create(Self);
  CBShowNothing := TCheckBox.Create(Self);
  CBShowProcedureBacktrace := TCheckBox.Create(Self);
  CBShowUnit := TCheckBox.Create(Self);
  CBShowLogo := TCheckBox.Create(Self);
  CBShowDebug := TCheckBox.Create(Self);
  CBShowTried := TCheckBox.Create(Self);
  CBShowMacros := TCheckBox.Create(Self);
  CBShowProcedures := TCheckBox.Create(Self);
  CBShowConditionals := TCheckBox.Create(Self);
  CBShowGCC := TCheckBox.Create(Self);
  
  // Texts
  
  CBShowWarnings.Text := SShowWarnings;
  CBShowNotes.Text := SShowNotes;
  CBShowHints.Text := SShowHints;
  CBShowInfo.Text := SShowInfo;
  CBShowLines.Text := SShowLines;
  CBShowAll.Text := SShowAll;
  CBShowNothing.Text := SShowNothing;
  CBShowProcedureBacktrace.Text := SShowProcedureBacktrace;
  CBShowUnit.Text := SShowUnit;
  CBShowLogo.Text := SShowLogo;
  CBShowDebug.Text := SShowDebug;
  CBShowTried.Text := SShowTried;
  CBShowMacros.Text := SShowMacros;
  CBShowProcedures.Text := SShowProcedures;
  CBShowConditionals.Text := SShowConditionals;
  CBShowGCC.Text := SShowGCC;
  
  // Names
  
  CBShowWarnings.Name := 'SShowWarnings';
  CBShowNotes.Name := 'SShowNotes';
  CBShowHints.Name := 'SShowHints';
  CBShowInfo.Name := 'SShowInfo';
  CBShowLines.Name := 'SShowLines';
  CBShowAll.Name := 'SShowAll';
  CBShowNothing.Name := 'SShowNothing';
  CBShowProcedureBacktrace.Name := 'SShowProcedureBacktrace';
  CBShowUnit.Name := 'SShowUnit';
  CBShowLogo.Name := 'SShowLogo';
  CBShowDebug.Name := 'SShowDebug';
  CBShowTried.Name := 'SShowTried';
  CBShowMacros.Name := 'SShowMacros';
  CBShowProcedures.Name := 'SShowProcedures';
  CBShowConditionals.Name := 'SShowConditionals';
  CBShowGCC.Name := 'SShowGCC';
  
  // CallBacks
  
  CBShowNothing.Onclick:=@OnVerbosityOffClick;
  CBShowAll.OnClick:=@OnVerbosityOnClick;
  
  // Layout.
  
  FGeneralGroup :=TGroupBox.Create(Self);
  With FGeneralGroup Do
    begin
    Name:='GeneralGroup';
    Text:=SSLevel;
    Content:=TGridLayout.Create(Self);
    With Content as TGridLayout do
      begin
      Name:='GeneralContent';
      Rows:=7;
      Columns:=2;
      AddWidget(CBShowWarnings,0,0,1,1);
      AddWidget(CBShowNotes,0,1,1,1);
      AddWidget(CBShowHints,0,2,1,1);
      AddWidget(CBShowInfo,0,3,1,1);
      AddWidget(CBShowLines,0,4,1,1);
      AddWidget(CBShowDebug,0,5,1,1);
      AddWidget(CBShowLogo,0,6,1,1);
      AddWidget(CBShowUnit,1,0,1,1);
      AddWidget(CBShowTried,1,1,1,1);
      AddWidget(CBShowMacros,1,2,1,1);
      AddWidget(CBShowProcedures,1,3,1,1);
      AddWidget(CBShowConditionals,1,4,1,1);
      AddWidget(CBShowGcc,1,5,1,1);
      AddWidget(CBShowProcedureBacktrace,1,6,1,1);
      end;
    end;
  FOnOffGroup := TGroupBox.Create(Self);
  With FOnOffGroup Do
    begin
    Name:='OnOffGroup';
    Text:=SSOnOff;
    Content:=TGridLayout.Create(Self);
    With Content as TGridLayout do
      begin
      Rows:=1;
      Columns:=2;
      AddWidget(CBShowAll,0,0,1,1);
      AddWidget(CBShowNothing,1,0,1,1);
      end;
    end;
  Result:=TGridLayout.Create(Self);
  With Result as TGridLayout do
    begin
    Name:='VerbosityLayout';
    Rows:=2;
    Columns:=1;
    VertSpacing:=8;
    AddWidget(FOnOffGroup,0,0,1,1);
    AddWidget(FGeneralGroup,0,1,1,1);
    end; 
end;

Procedure TCompilerOptionsForm.SetVerbosities (OnOff : Boolean);

Var TheState : TCheckBoxState;

begin
  If OnOff Then 
    TheState:=cbChecked
  else
    TheState:=cbUnchecked;
  CBShowWarnings.State :=TheState;
  CBShowNotes.State :=TheState;
  CBShowHints.State :=TheState;
  CBShowInfo.State :=TheState;
  CBShowLines.State :=TheState;
  CBShowProcedureBacktrace.State :=TheState;
  CBShowUnit.State :=TheState;
  CBShowLogo.State :=TheState;
  CBShowDebug.State :=TheState;
  CBShowTried.State :=TheState;
  CBShowMacros.State :=TheState;
  CBShowProcedures.State :=TheState;
  CBShowConditionals.State :=TheState;
  CBShowGCC.State :=TheState;
end;

Procedure TCompilerOptionsForm.OnVerbosityOffClick (Sender : TObject);

Var
  OnOff : Boolean;

begin
  OnOff:=Not ((Sender as TCheckBox).State=cbChecked);
  CBShowAll.State:=cbUnchecked;
  SetVerbosities(False);
  FGeneralGroup.Enabled:=OnOff;
//  Writeln('Verbosity off');
end;

Procedure TCompilerOptionsForm.OnVerbosityOnClick (Sender : TObject);

Var
  OnOff : Boolean;

begin
  OnOff:=(Sender as TCheckBox).State=cbChecked;
  CBShowNothing.State:=cbUnchecked;
  SetVerbosities(OnOff);
  FGeneralGroup.Enabled:=Not OnOff;
//  Writeln('Verbosity On');
end;

Function TCompilerOptionsForm.CreateSyntaxPage : TWidget;

var Layout1,Layout2 : TBoxLayout;

begin
  // Syntax options
  RBSyntaxFPC := TRadioButton.Create(Self);
  RBSyntaxFPC.Text:=SSyntaxFPC;
  RBSyntaxDelphi := TRadioButton.Create(Self);
  RBSyntaxDelphi.Text:=SSyntaxDelphi;
  RBSyntaxTP := TRadioButton.Create(Self);
  RBSyntaxTP.Text:=SSyntaxTP;
  RBSyntaxObjfpc := TRadioButton.Create(Self);
  RBSyntaxObjFPC.Text:=SSyntaxObjFPC;
  GBSYntaxMode := TGroupBox.Create(Self);
  RBSyntaxFPC.Name := 'SSyntaxFPC';
  RBSyntaxDelphi.Name := 'SSyntaxDelphi';
  RBSyntaxTP.Name := 'SSyntaxTP';
  RBSyntaxObjFPC.Name := 'SSyntaxObjFPC';
  RBSyntaxAsmAtt := TRadioButton.Create(Self); 
  RBSyntaxAsmIntel := TRadioButton.Create(Self);
  RBSyntaxAsmDirect := TRadioButton.Create(Self);
  GBSyntaxAsmStyle := TGroupBox.Create(Self);
  RBSyntaxAsmAtt.Name := 'RBSyntaxAsmAtt'; 
  RBSyntaxAsmIntel.Name := 'RBSyntaxAsmIntel';
  RBSyntaxAsmDirect.Name := 'RBSyntaxAsmDirect';
  GBSyntaxAsmStyle.Name := 'GBSyntaxAsmStyle';
  RBSyntaxAsmAtt.Text := SSyntaxAsmAtt; 
  RBSyntaxAsmIntel.Text := SSyntaxAsmIntel;
  RBSyntaxAsmDirect.Text := SSyntaxAsmDirect;
  GBSyntaxAsmStyle.Text := SSyntaxAsmStyle;
  with GBSyntaxMode do 
    begin
    Name:='SyntaxMode';
    Text:=SSyntaxMode;
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLAyout do
      begin
      Name:='ModeLayout';
      Orientation:=boxVert;
      VertALign:=VertFill;
//      Spacing:=6;
      AddWidget(RBSyntaxFPC);
      AddWidget(RBSyntaxTP);
      AddWidget(RBSyntaxObjFPC);
      AddWidget(RBSyntaxDelphi);
      end;
    end;
  CBSyntaxCOperators := TCheckBox.Create(Self);
  CBSyntaxAllowGoto := TCheckBox.Create(Self);
  CBSyntaxAnsiStrings := TCheckBox.Create(Self);
  CBSyntaxInline := TCheckBox.Create(Self);
  CBSyntaxMacros := TCheckBox.Create(Self);
  CBSyntaxConst := TCheckBox.Create(Self);
  CBSyntaxStatic := TCheckBox.Create(Self);
  CBSyntaxCOperators.Text:=  SSyntaxCOperators;
  CBSyntaxAllowGoto.Text:=  SSyntaxAllowGoto;
  CBSyntaxMacros.Text:=  SSyntaxMacros;
  CBSyntaxConst.Text:=  SSyntaxConst;
  CBSyntaxStatic.Text:=  SSyntaxStatic;
  CBSyntaxCOperators.Name := 'SSyntaxCOperators';
  CBSyntaxAllowGoto.Name := 'SSyntaxAllowGoto';
  CBSyntaxAnsistrings.Name := 'SSyntaxAnsistrings';
  CBSyntaxInline.Name := 'SSyntaxInline';
  CBSyntaxMacros.Name := 'SSyntaxMacros';
  CBSyntaxConst.Name := 'SSyntaxConst';
  CBSyntaxStatic.Name := 'SSyntaxStatic';
  GBSyntaxOther := TgroupBox.CReate(Self);
  With GBSyntaxOther do 
    begin
    Name:='SyntaxOther';
    Text:=  SSyntaxOther;
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLayout do
      begin
      Name:='SyntaxOtherContent';
      Orientation:=boxVert;
      AddWidget(CBSyntaxCOperators);
      AddWidget(CBSyntaxAllowGoto);
      AddWidget(CBSyntaxAnsistrings);
      AddWidget(CBSyntaxInline);
      AddWidget(CBSyntaxMacros);
      AddWidget(CBSyntaxConst);
      AddWidget(CBSyntaxStatic);
      end;
    end;  
  With GBSyntaxAsmStyle do 
    begin
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLayout Do
      begin
      Orientation:=BoxVert;
      Spacing:=8;
      HorzAlign:=HorzLeft;
      AddWidget(RBSyntaxAsmAtt);
      AddWidget(RBSyntaxAsmIntel);
      AddWidget(RBSyntaxAsmDirect);
      end;
    end;  
  Layout1:=TBoxLayout.Create(Self);
  With Layout1 do
    begin
    Name:='SyntaxLayoutTop';
    Orientation:=boxHorz;
    VertAlign:=VertFill;
    HorzAlign:=HorzFill;
    Spacing:=8;
    AddWidget(GBSyntaxMode);
    AddWidget(GBSyntaxOther);
    end;
  Layout2:=TBoxLayout.Create(Self);
  With Layout2 do
    begin
    Name:='SyntaxLayoutBottom';
    Orientation:=BoxHorz;
    HorzAlign:=horzLeft;
    Spacing:=8;
    AddWidget(GBSyntaxAsmStyle);
    end;
  Result:=TBoxLAyout.Create(Self);
  With Result as TBoxLAyout do
    begin
    NAme:='SyntaxLAyout';
    Orientation:=boxVert;
    VertAlign:=vertTop;
    Spacing:=8;
    AddWidget(Layout1);
    AddWidget(Layout2);
    end;
end;

Function TCompilerOptionsForm.CreateCodePage : TWidget;

Var Grid : TGridLayout;
    Layout1,Layout2 : TBoxlayout;
    GB : TGroupBox;
    ALabel : TLabel;
    
begin
   // Code page
   GBCodeOptions := TGroupBox.Create(Self);
   GBCodeOptions.Text:=SCodeOptions; 
   GBCodeOptions.Name:='SCodeOptions'; 
   GBMemSizes := TGroupBox.Create(Self);
   GBMemSizes.Text:=SMemSizes; 
   GBMemSizes.Name:='SMemSizes'; 
   ECodeStackSize := TEdit.Create(Self);
   ECodeStackSize.Name := 'SCodeStackSize';
   ECodeHeapSize := TEdit.Create(Self);
   ECodeHeapSize.Name := 'SCodeHeapSize';
   CBCodeIOCheck := TCheckBox.Create(Self);
   CBCodeIOCheck.Text := SCodeIOCheck;
   CBCodeIOCheck.Name := 'SCodeIOCheck';
   CBCodeOverFlow := TCheckBox.Create(Self);
   CBCodeOverflow.Text := SCodeOverflow;
   CBCodeOverflow.Name := 'SCodeOverflow';
   CBCodeRangeCheck := TCheckBox.Create(Self);
   CBCodeRangeCheck.Text := SCodeRangeCheck;
   CBCodeRangeCheck.Name := 'SCodeRangeCheck';
   CBCodeStackCheck := TCheckBox.Create(Self);
   CBCodeStackCheck.Text := SCodeStackCheck;
   CBCodeStackCheck.Name := 'SCodeStackCheck';
   GBOptimizations := TGroupBox.Create(Self);
   GBOptimizations.Text := SOPtimizations;
   GBOptimizations.Name := 'SOptimizations';
   CBEnableOptimizations := TCheckBox.Create(Self);
   CBEnableOptimizations.Text := SEnableOptimizations;
   CBEnableOptimizations.Name := 'SEnableOptimizations';
   CBENableOptimizations.OnCLick:=@OnEnableOptimizationsClick;
   CBCodeSmaller := TCheckBox.Create(Self);
   CBCodeSmaller.Text := SCodeSmaller;
   CBCodeSmaller.Name := 'SCodeSmaller';
   CBCodeFaster := TCheckBox.Create(Self);
   CBCodefaster.Text := SCodefaster;
   CBCodefaster.Name := 'SCodefaster';
   CBCodeRegister := TCheckBox.Create(Self);
   CBCoderegister.Text := SCoderegister;
   CBCoderegister.Name := 'SCoderegister';
   CBCodeUncertain := TCheckBox.Create(Self);
   CBCodeUncertain.Text := SCodeUncertain;
   CBCodeUncertain.Name := 'SCodeUncertain';
   RBCodeLevel1 := TRadioButton.Create(Self);
   RBCodeLevel1.Text := SCodeLevel1;
   RBCodeLevel1.Name := 'SCodeLevel1';
   RBCodeLevel2 := TRadioButton.Create(Self);
   RBCodeLevel2.Text := SCodeLevel2;
   RBCodeLevel2.Name := 'SCodeLevel2';
   RBCodeLevel3 := TRadioButton.Create(Self);
   RBCodeLevel3.Text := SCodeLevel3;
   RBCodeLevel3.Name := 'SCodeLevel3';
   CBCodeProcessor := TCheckBox.Create(Self);
   CBCodeProcessor.Text := SCodeProcessor;
   CBCodeProcessor.Name := 'SCodeProcessor';
   CBCodeProcessor.OnClick := @OnSetProcessorOnClick;
   RBCode386 := TRadioButton.Create(Self);
   RBCode386.Text := SCode386;
   RBCode386.Name := 'SCode386';
   RBCodePentium := TRadioButton.Create(Self);
   RBCodePentium.Text := SCodePentium;
   RBCodePentium.Name := 'SCodePentium';
   RBCodePentiumPro := TRadioButton.Create(Self);
   RBCodePentiumPro.Text := SCodePentiumPro;
   RBCodePentiumPro.Name := 'SCodePentiumPro';
   // build layout
   // First, Code options and memory sizes.
   With GBCodeOptions do
     begin
     Content:=TBoxLayout.Create(Self);
     With Content as TBoxLAyout do
       begin
       Orientation:=boxVert;
       AddWidget(CBCodeIOCheck);
       AddWidget(CBCodeRangeCheck);
       AddWidget(CBCodeOverFlow);
       AddWidget(CBCodeStackCheck);
       end;
     end;
  With GBMemSizes do
    begin
    Content:=TGridLayout.Create(Self);
    With Content as TGridLayout do
      begin
      Rows:=2;
      Columns:=2;
      VertSpacing:=8;
      HorzSpacing:=8;
      Alabel:=TLabel.Create(Self);
      ALabel.Alignment:=taRightJustify;
      ALabel.Text:=SCodeHeapSize;
      AddWidget(Alabel,0,0,1,1);
      AddWidget(ECodeHeapSize,1,0,1,1);
      ALabel:=TLabel.Create(Self);
      ALabel.Text:=SCodestackSize;
      ALabel.Alignment:=taRightJustify;
      ALAbel.CanExpandWidth:=False;
      AddWidget(ALabel,0,1,1,1);
      AddWidget(ECodeStackSize,1,1,1,1);
      end
    end;   
  // Now optimizations.  
  GB:=TGroupBox.Create(Self);
  With GB do
    begin
    Text:=SCodeLevel;
    Content:=TBoxLayout.Create(Self);
    With Content As TBoxLayout do
      begin
      Orientation:=BoxVert;
      AddWidget(RBCodeLevel1);
      AddWidget(RBCodeLevel2);
      AddWidget(RBCodeLevel3);
      end;
    end;
  GBProcType:=TGroupBox.Create(Self);
  With GBProctype do
    begin
    Text:=SCodeProcType;
    Enabled:=False;
    Content:=TBoxLayout.Create(Self);
    With Content As TBoxLayout do
      begin
      Orientation:=boxVert;
      AddWidget(RBCode386);
      AddWidget(RBCodePentium);
      AddWidget(RBCodePentiumPro);
      end;
    end;
  {
     Now the real layouting. The 2 columns must be displayed
     Left-aligned in a grid. For that it is necessary to
     Create 2 vertical,left aligned boxes, and stick them
     into a (2,1) grid. (just sticking everything in the grid would
     stretch everything to the grid cell size, which we do not want)
  }
  Layout1:=TBoxLayout.Create(Self);
  With Layout1 do 
    begin
    Orientation:=BoxVert;
    HorzAlign:=horzLeft;
    AddWidget(GB);
    AddWidget(CBCodeSmaller);
    AddWidget(CBCodeFaster);
    end; 
  Layout2:=TBoxLayout.Create(Self);
  With Layout2 do  
    begin
    Orientation:=boxVert;
    HorzAlign:=HorzLeft;
    AddWidget(CBCodeRegister);
    AddWidget(CBCodeUnCertain);
    AddWidget(CBCodeProcessor);
    AddWidget(GBProcType);
    end;
  Grid:=TgridLayout.Create(Self);
  with Grid do 
    begin
    Rows:=1;
    Columns:=2;
    AddWidget(Layout1,0,0,1,1);
    AddWidget(Layout2,1,0,1,1);
    end;
  { 
    Stack the Optimizations check box and group box on top
    of each other, and expand horizontally to full width.
    Also, disable optimizations, since it is off by default,
    and this way in concordance with 'enable optimizations'
  }
  With GBOptimizations do
    begin
    Enabled:=False;
    Content:=TDockingLayout.Create(Self);
    With Content As TDockingLayout do 
      AddWidget(Grid,dmClient); // ,2,0,1,1);
    end;
  {
    Prepare final layout: Codeoptions on top,
    Optimizations on the bottom.  
  }
  // Bottom half:
  Layout1:=TBoxLayout.Create(Self);
  With Layout1 do  
    begin
    Orientation:=boxVert;
    vertAlign:=vertTop;
    horzAlign:=horzfill;
    AddWidget(CBEnableOptimizations);
    AddWidget(GBOPtimizations);
    end;
  {
    Top half: We need to put mem sizes and code options
    group boxes in a grid
  } 
  Grid:=TGridLayout.Create(Self);
  With Grid do
    begin
    Rows:=1;
    Columns:=2;
    HorzSpacing:=20;
    AddWidget(GBCodeOptions,0,0,1,1);
    AddWidget(GBMemSizes,1,0,1,1);
    end;  
  {
    Finally, merge top and bottom in final result.
  }  
  Result:=TBoxLAyout.Create(Self);
  With Result as TBoxLayout do
    begin
    Orientation:=BoxVert;
    HorzAlign:=horzFill;
    VertAlign:=VertTop;
    AddWidget(Grid);
    AddWidget(Layout1);
    end;    
end;

Procedure TCompilerOptionsForm.OnEnableOptimizationsClick (Sender : TObject);

begin
  GBOPtimizations.Enabled:=((Sender as TCheckBox).State=cbChecked)
end;

Procedure TCompilerOptionsForm.OnSetProcessorOnClick (Sender : TObject);

begin
  GBProcType.Enabled:=((Sender as TCheckBox).State=cbChecked)
end;

Function TCompilerOptionsForm.CreateLinkerPage : TWidget;

Var 
  ALabel  : TLabel;
  Layout : TBoxLayout;
  Grid : TGridLayout;
  GB : TGroupBox;
  
begin
  CBLinkMakeDynlib := TCheckBox.Create(Self);
  CBLinkMakeSmartLink := TCheckBox.Create(Self); 
  CBLinkOmitLinking  := TCheckBox.Create(Self);
  CBLinkUseClib  := TCheckBox.Create(Self);
  CBLinkStripSymbols := TCheckBox.Create(Self);
  RBLinkLinkDynamic := TRadioButton.Create(Self);
  RBLinkLinkSmart  := TRadioButton.Create(Self);
  RBLinkLinkStatic  := TRadioButton.Create(Self);
  ELinkLinkerOptions := TEdit.Create(Self);
  ELinkOutputname := TEdit.Create(Self);
  GBLinkOptions := TGroupBox.Create(Self);
  // text
  CBLinkMakeDynlib.Text := SLinkMakeDynlib;
  CBLinkMakeSmartLink.Text := SLinkMakeSmartLink; 
  CBLinkOmitLinking.Text := SLinkOmitLinking ;
  CBLinkUseClib.Text := SLinkUseClib ;
  CBLinkStripSymbols.Text := SLinkStripSymbols;
  RBLinkLinkSmart.Text := SLinkLinkSmart ;
  RBLinkLinkStatic.Text := SLinkLinkStatic ;
  RBLinkLinkDynamic.Text := SLinkLinkDynamic;
  GBLinkOptions.Text := SLinkOptions;
  // Names  
  CBLinkMakeDynlib.Name := 'CBLinkMakeDynlib';
  CBLinkMakeSmartLink.Name := 'CBLinkMakeSmartLink'; 
  CBLinkOmitLinking.Name := 'CBLinkOmitLinking';
  CBLinkUseClib.Name := 'CBLinkUseClib';
  CBLinkStripSymbols.Name := 'CBLinkStripSymbols';
  RBLinkLinkSmart.Name := 'RBLinkLinkSmart';
  RBLinkLinkStatic.Name := 'RBLinkLinkStatic';
  RBLinkLinkDynamic.Name := 'RBLinkLinkDynamic';
  ELinkLinkerOptions.Name := 'ELinkLinkerOptions';
  GBLinkOptions.Name := 'GBLinkOptions';

  // CallBAcks
  CBLinkOmitLinking.OnClick:=@OnOmitLinkingClick;
  
  // Start layout.
  Grid:=TGridLayout.Create(Self);
  With Grid do
    begin
    Rows:=2;
    Columns:=2;
    HorzSpacing := 4;
    VertSpacing := 4;
    ALabel:=TLabel.Create(Self);
    ALabel.Alignment:=taRightJustify;
    ALabel.Text:=SLinkLinkerOptions;
    Alabel.CanExpandWidth:=False;
    AddWidget(Alabel,0,0,1,1);
    AddWidget(ELinkLinkerOptions,1,0,1,1);
    ALabel:=TLabel.Create(Self);
    ALabel.Alignment:=taRightJustify;
    ALabel.Text:=SLinkOutputname;
    Alabel.CanExpandWidth:=False;
    AddWidget(Alabel,0,1,1,1);
    AddWidget(ELinkOutputName,1,1,1,1);
    end;
  GB :=TGroupBox.Create(Self);
  With GB Do
    begin
    Text:=SLinkType;
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLAyout do
      begin
      Orientation:=BoxVert;
      HorzAlign:=HorzLeft;
      AddWidget(RBLinkLinkStatic);
      AddWidget(RBLinkLinkSmart);
      AddWidget(RBLinkLinkDynamic);
      end; 
    end;  
  Layout:=TBoxLAyout.Create(Self);
  with Layout do
    begin
    Orientation:=BoxHorz;
    HorzAlign:=HorzLeft;
    AddWidget(GB);
    end;  
  With GBLinkOptions do
    begin
    Content:=TBoxLayout.Create(Self);
    With (Content As TBoxLAyout) do
      begin
      Orientation:=BoxVert;
      HorzAlign:=HorzFill;
      VertAlign:=VertTop;
      Spacing:=8;
      AddWidget(CBLinkUseCLib);
      AddWidget(CBLinkStripSymbols);
      AddWidget(CBLinkMakeDynlib);
      AddWidget(CBLinkMakeSmartLink);
      AddWidget(Grid);
      AddWidget(Layout)
      end;
    end;
  Result:=TBoxLayout.Create(Self);  
  With Result as TBoxLayout do 
    begin
    Orientation:=boxVert;
    VertAlign:=VertTop;
    HorzAlign:=HorzFill;
    Spacing:=8;
    AddWidget(CBLinkOmitLinking);
    AddWidget(GBLinkOptions);
    end;
end;

Procedure TCompilerOptionsForm.OnOmitLinkingClick (Sender : TObject);

begin
  GBLinkOptions.Enabled:=Not((Sender as TCheckBox).State=CbChecked);
end;

Function TCompilerOptionsForm.CreateGeneralPage : TWidget;

Var Layout1,Layout2,Layout3 : TBoxLayout;
    Layout : TgridLayout;
    ALAbel : TLabel;
    
begin
  // general page.
  CBGeneralCheckUnitName := TCheckBox.Create(Self);
  CBGeneralSystemUNit := TCheckBox.Create(Self);
  CBGeneralBuild := TCheckBox.Create(Self);
  CBGeneralNoConfig := TCheckBox.Create(Self);
  CBGeneralPipes := TCheckBox.Create(Self);
  CBGeneralBrowserInfo := TCheckBox.Create(Self);
  CBGeneralLocalBrowserINfo := TCheckBox.Create(Self);
  CBGeneralScript := TCheckBox.Create(Self);
  CBGeneralDebugInfo := TCheckBox.Create(Self);
  CBGeneralProfile := TCheckBox.Create(Self);
  CBGeneralgsym := TCheckBox.Create(Self);
  CBGeneraldbx := TCheckBox.Create(Self);
  CBGeneralHeaptrace := TCheckBox.Create(Self);
  CBGeneralLineInfo := TCheckBox.Create(Self);
  CBGeneralCheckPointers := TCheckBox.Create(Self);
  CBGeneralKeepAsm := TCheckBox.Create(Self);
  CBGeneralAsmListSource := TCheckBox.Create(Self);
  CBGeneralAsmListRegAlloc := TCheckBox.Create(Self);
  CBGeneralAsmListTempAlloc := TCheckBox.Create(Self);
  EGeneralDefines := TEdit.Create(Self);
  EGeneralUnDefines := TEdit.Create(Self);
  GBGeneralAsmInfo := TGroupBox.Create(Self);
  GBGeneralDebugOptions := TGroupBox.Create(Self);

  // Names
  // Writeln('Starting names');
  
  CBGeneralCheckUnitName.Name := 'SGeneralCheckUnitName';
  CBGeneralSystemUNit.Name := 'SGeneralSystemUNit';
  CBGeneralBuild.Name := 'SGeneralBuild';
  CBGeneralNoConfig.Name := 'SGeneralNoConfig';
  CBGeneralPipes.Name := 'SGeneralPipes';
  CBGeneralBrowserInfo.Name := 'SGeneralBrowserInfo';
  CBGeneralLocalBrowserINfo.Name := 'SGeneralLocalBrowserINfo';
  CBGeneralScript.Name := 'SGeneralScript';
  CBGeneralDebugInfo.Name := 'SGeneralDebugInfo';
  CBGeneralProfile.Name := 'SGeneralProfile';
  CBGeneralgsym.Name := 'SGeneralgsym';
  CBGeneraldbx.Name := 'SGeneraldbx';
  CBGeneralHeaptrace.Name := 'SGeneralHeaptrace';
  CBGeneralLineInfo.Name := 'SGeneralLineInfo';
  CBGeneralCheckPointers.Name := 'SGeneralCheckPointers';
  CBGeneralKeepAsm.Name := 'SGeneralKeepAsm';
  CBGeneralAsmListSource.Name := 'SGeneralAsmListSource';
  CBGeneralAsmListRegAlloc.Name := 'SGeneralAsmListRegAlloc';
  CBGeneralAsmListTempAlloc.Name := 'SGeneralAsmListTempAlloc';
  EGeneralDefines.Name := 'SGeneralDefines';
  EGeneralUnDefines.Name := 'SGeneralUnDefines'; 
  GBGeneralAsmInfo.Name := 'SGeneralAsmInfo';
  GBGeneralDebugOptions.Name := 'SGeneralDebugOptions';

  // Texts
  // Writeln('Starting text');

  CBGeneralCheckUnitName.text := SGeneralCheckUnitName;
  CBGeneralSystemUNit.text := SGeneralSystemUNit;
  CBGeneralBuild.text := SGeneralBuild;
  CBGeneralNoConfig.text := SGeneralNoConfig;
  CBGeneralPipes.text := SGeneralPipes;
  CBGeneralBrowserInfo.text := SGeneralBrowserInfo;
  CBGeneralLocalBrowserINfo.text := SGeneralLocalBrowserINfo;
  CBGeneralScript.text := SGeneralScript;
  CBGeneralDebugInfo.text := SGeneralDebugInfo;
  CBGeneralProfile.text := SGeneralProfile;
  CBGeneralgsym.text := SGeneralgsym;
  CBGeneraldbx.text := SGeneraldbx;
  CBGeneralHeaptrace.text := SGeneralHeaptrace;
  CBGeneralLineInfo.text := SGeneralLineInfo;
  CBGeneralCheckPointers.text := SGeneralCheckPointers;
  CBGeneralKeepAsm.text := SGeneralKeepAsm;
  CBGeneralAsmListSource.text := SGeneralAsmListSource;
  CBGeneralAsmListRegAlloc.text := SGeneralAsmListRegAlloc;
  CBGeneralAsmListTempAlloc.text := SGeneralAsmListTempAlloc;
  GBGeneralAsmInfo.text := SGeneralAsmInfo;
  GBGeneralDebugOptions.text := SGeneralDebugOptions;

  // Writeln('Starting layout');
  
  // Layout.
  Result:=TBoxLAyout.Create(Self);
  With Result as TBoxLayout do
    begin
    Orientation:=BoxVert;
    HorzAlign:=HorzFill;
    Spacing:=8;
    end;
  Layout1:=TBoxLayout.Create(Self);
  With Layout1 do
    begin
    Orientation:=boxVert;
    AddWidget(CBGeneralBuild);
    AddWidget(CBGeneralPipes);
    AddWidget(CBGeneralNoConfig);
    AddWidget(CBGeneralScript);
    end;
  Layout2:=TBoxLayout.Create(Self);
  With Layout2 do
    begin
    Orientation:=boxVert;
    AddWidget(CBGeneralSystemUnit);
    AddWidget(CBGeneralCheckUnitName);
    AddWidget(CBgeneralBrowserInfo);
    AddWidget(CBgeneralLocalBrowserInfo);
    end;
  Layout3:=TBoxLayout.Create(Self);
  With Layout3 do
    begin
    Orientation:=BoxHorz;
    HorzAlign:=HorzFill;
    Spacing:=8;
    AddWidget(Layout1);
    AddWidget(Layout2);
    end;
  (Result as TBoxLayout).AddWidget(Layout3);
  // Writeln('Adding edits');
  // Add 2 edits.  
  Layout:=TGridLayout.Create(Self);
  With Layout do
    begin
    Rows:=2;
    Columns:=2;
    HorzSpacing:=4;
    VertSpacing:=4;
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SGeneralDefines;
    ALabel.Alignment:=taRightJustify;
    Alabel.CanExpandWidth:=False;
    AddWidget(Alabel,0,0,1,1);
    AddWidget(EGeneralDefines,1,0,1,1);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SGeneralUnDefines;
    ALabel.Alignment:=taRightJustify;
    Alabel.CanExpandWidth:=False;
    AddWidget(Alabel,0,1,1,1);
    AddWidget(EGeneralUnDefines,1,1,1,1);
    end;
  (Result as TBoxLayout).AddWidget(Layout);
  // Debug info
  // Writeln('Adding debug info');
  With GBGeneralDebugOptions do
    begin
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLAyout do
      begin
      Orientation:=BoxVert;
      HorzAlign:=HorzLeft;
      AddWidget(CBGeneralHeapTrace);
      AddWidget(CBGeneralLineInfo);
      AddWidget(CBGeneralCheckPointers);
      AddWidget(CBGeneralgsym);
      AddWidget(CBGeneraldbx);
      end;
    end;
  Layout1:=TBoxLayout.Create(Self);
  With Layout1 do
    begin
    Orientation:=boxVert;
    AddWidget(CBGeneralDebugInfo);
    AddWidget(GBGeneralDebugoptions);
    end;
  // Profile and asm info
//  // Writeln('Adding profie and asm  info');
  With GBGeneralAsmInfo do
    begin
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLAyout do
      begin
      Orientation:=BoxVert;
      HorzAlign:=HorzLeft;
      AddWidget(CBGeneralAsmListSource);
      AddWidget(CBGeneralAsmListRegAlloc);
      AddWidget(CBGeneralAsmListTempAlloc);
      end;
    end;  
  Layout2:=TBoxLayout.Create(Self);
  With Layout2 do
    begin
    Orientation:=boxVert;
    HorzAlign:=HorzLeft;
    AddWidget(CBGeneralProfile);
    AddWidget(CBGeneralKeepAsm);
    AddWidget(GBGeneralAsmInfo);
    end;
  Layout3:=TBoxLayout.Create(Self);
  With Layout3 do
    begin
    Orientation:=BoxHorz;
    HorzAlign:=HorzFill;
    Spacing:=8;
    AddWidget(Layout1);
    AddWidget(Layout2);
    end;
  (Result as TBoxLayout).AddWidget(Layout3);
  // Writeln('Finished');
end;

Procedure TCompilerOptionsForm.OnOkCancelClick (Sender : TObject);

begin
  If (Sender=OKButton) then
    FormToOptions;
  Close;
end;

Procedure TCompilerOptionsForm.FormToOptions;

 Function AddOption(Const Value : String) : longint;
 
 begin
   Result:=FCompilerOptions.Add(VAlue);
 end;
 
 procedure SplitOption (Value : String; Const Prefix : String);
 
 Var l : longint;
 
 begin
   // Convert spaces to ;
   For L:=1 to length(Value) do
     if Value[l]=' ' then
       Value[l]:=';';
   L:=Pos(';',Value);
   While (L>0) and (Length(Value)>0) do
     begin
     If L>1 then
       AddOption(Prefix+Copy(Value,1,L-1));
     Delete(Value,1,L);
     L:=Pos(';',Value);
     end;
   If Length(Value)>0 then  
     AddOption(Prefix+Value);
 end; 

Var Temp : String;
    L : Longint;
    
begin
  FCompilerOptions.Clear;
  // Directory page.
  If Length(FUnitOutputDir.Text)>0 then
    AddOption('-FU'+FUnitOutputDir.Text);
  If Length(FOutputDir.Text)>0 then
    AddOption('-FE'+FOutputDir.Text);
  If Length(FToolsSearchDir.Text)>0 then
    AddOption('-FD'+FToolsSearchDir.Text);
  If Length(FIncludePath.Text)>0 then
    AddOption('-Fi'+FIncludePath.Text);
  If Length(FObjectSearchPath.Text)>0 then
    AddOption('-Fo'+FObjectSearchPath.Text);
  If Length(FLibrarySearchPath.Text)>0 then
    AddOption('-Fl'+FLibrarySearchPath.Text);
  If Length(FUnitSearchPath.Text)>0 then
    AddOption('-Fu'+FUnitSearchPath.Text);
  // Compiler messages page
  If (CBShowAll.State=cbChecked) then
    begin
    AddOption('-va');
    AddOption('-l');
    end
  else if (CBShowNothing.State=cbChecked) then
    AddOption('-v0')
  else 
    begin
    Temp:='';
    If (CBShowWarnings.State=cbChecked) then
      Temp := Temp+'w';
    If (CBShowNotes.State=cbChecked) then
      Temp := Temp+'n';
    If (CBShowHints.State=cbChecked) then
      Temp := Temp+'h';
    If (CBShowInfo.State=cbChecked) then
      Temp := Temp+'i';
    If (CBShowLines.State=cbChecked) then
      Temp := Temp+'l';
    If (CBShowProcedureBacktrace.State=cbChecked) then
      Temp := Temp+'b';
    If (CBShowUnit.State=cbChecked) then
      Temp := Temp+'u';
    If (CBShowDebug.State=cbChecked) then
      Temp := Temp+'d';
    If (CBShowTried.State=cbChecked) then
      Temp := Temp+'t';
    If (CBShowMacros.State=cbChecked) then
      Temp := Temp+'m';
    If (CBShowProcedures.State=cbChecked) then
      Temp := Temp+'p';
    If (CBShowConditionals.State=cbChecked) then
      Temp := Temp+'c';
    If (CBShowGCC.State=cbChecked) then
      Temp := Temp+'r';
    If Length(Temp)>0 then
      AddOption('-v'+temp);  
    If (CBShowLogo.State=cbChecked) then
      AddOption('-l');
    end;
  // Syntax options
  If Not (RBSyntaxAsmAtt.Checked) then
    begin
    If (RBSyntaxAsmIntel.Checked) then
      AddOption('-Rintel');
    If (RBSyntaxAsmDirect.Checked) then
      AddOption('-Rdirect');
    end;   
  If not (RBSyntaxFPC.Checked) then
    begin
    If (RBSyntaxDelphi.Checked) then
      AddOption('-Sd');
    If (RBSyntaxTP.Checked) then
      AddOption('-So');
    If (RBSyntaxObjfpc.Checked) then
      AddOption('-S2');
    end;
  Temp:='';    
  If (CBSyntaxCOperators.State=cbChecked) then
    Temp := Temp+'c';
  If (CBSyntaxAllowGoto.State=cbChecked) then
    Temp := Temp+'g';
  If (CBSyntaxAnsistrings.State=cbChecked) then
    Temp := Temp+'h';
  If (CBSyntaxInline.State=cbChecked) then
    Temp := Temp+'i';
  If (CBSyntaxMacros.State=cbChecked) then
    Temp := Temp+'m';
  If (CBSyntaxConst.State=cbChecked) then
    Temp := Temp+'s';
  If (CBSyntaxStatic.State=cbChecked) then
    Temp := Temp+'t';
  If Length(Temp)>0 then
    AddOption('-S'+Temp);  
  // Code page
  Temp:=ECodeStackSize.Text;
  If Length(Temp)>0 then
    begin
    L:=StrToIntDef(Temp,-1);
    if L>0 then
      AddOption('-Cs'+Temp);
    end;
  Temp:=ECodeHeapSize.Text;  
  If Length(Temp)>0 then
    begin
    L:=StrToIntDef(Temp,-1);
    if L>0 then
      AddOption('-Ch'+Temp);
    end;
  // rest of -C options  
  //  Writeln(stderr,'Code options');
  Temp:='';  
  If (CBCodeIOCheck.State=cbChecked) then
    Temp:=Temp+'i';
  If (CBCodeOverFlow.State=cbChecked) then
    Temp:=Temp+'o';
  If (CBCodeRangeCheck.State=cbChecked) then
    Temp:=Temp+'r';
  If (CBCodeStackCheck.State=cbChecked) then
    Temp:=Temp+'t';
  if Length(Temp)>0 then
    AddOption('-C'+temp);
  // Optimizations  
  // Writeln(stderr,'Code optmizations');
  If (CBEnableOptimizations.State=cbChecked) then
    begin
    Temp:='-O';
    If (RBCodeLevel1.Checked) then
      Temp := Temp+'1'
    Else If (RBCodeLevel2.Checked) then
      Temp := Temp+'2'
    Else If (RBCodeLevel3.Checked) then
      Temp := Temp+'3';
    If (CBCodeSmaller.State=cbChecked) then
      Temp := Temp+'g';
    If (CBCodeFaster.State=cbChecked) then
      Temp := Temp+'G';
    If (CBCodeRegister.State=cbChecked) then
      Temp := Temp+'r';
    If (CBCodeUncertain.State=cbChecked) then
      Temp := Temp+'u';
    If (CBCodeProcessor.State=cbChecked) then
      begin
      temp:=temp+'p';
      If (RBCode386.Checked) then
        Temp := Temp+'1'
      else If (RBCodePentium.Checked) then
        Temp := Temp+'2'
      else If (RBCodePentiumPro.Checked) then
        Temp := Temp+'3';
      end;  
    AddOption(Temp);
    end;  
  // Linker options
  // Writeln(stderr,'Linker options');
  If (CBLinkOmitLinking.State=cbChecked) then
    AddOption('-Cn')
  else
    begin
    If (CBLinkMakeDynlib.State=cbChecked) then
      AddOption('-CD');
    If (CBLinkMakeSmartLink.State=cbChecked) then
      AddOption('-CX'); 
    If (CBLinkUseClib.State=cbChecked) then
      AddOption('-Xc');
    If (CBLinkStripSymbols.State=cbChecked) then
      AddOption('-Xs');
    If (RBLinkLinkSmart.Checked) then
      AddOption('-XX')
    {
     // This is on by default.
    Else If (RBLinkLinkStatic.Checked) then
      AddOption('-XS') 
    }
    else If (RBLinkLinkDynamic.Checked) then
      AddOption('-XD');
    If Length(ELinkLinkerOptions.Text)>0 then
      AddOption('-k'+ELinkLinkerOptions.Text);
    If Length(ELinkOutputName.Text)>0 then
      AddOption('-o'+ELinkOutputName.Text);
    end;  
  // Writeln(stderr,'General options');
  // general page.
  If (CBGeneralCheckUnitName.State=cbChecked) then
    AddOption('-Un');
  If (CBGeneralSystemUNit.State=cbChecked) then
    AddOption('-Us');
  If (CBGeneralBuild.State=cbChecked) then
    AddOption('-B');
  If (CBGeneralNoConfig.State=cbChecked) then
    AddOption('-n');
  If (CBGeneralPipes.State=cbChecked) then
    AddOption('-P');
  If (CBGeneralBrowserInfo.State=cbChecked) then
    begin
    Temp:='-b';
    If (CBGeneralLocalBrowserINfo.State=cbChecked) then
      Temp:=temp+'l';
    AddOption(Temp);
    end;  
  If (CBGeneralScript.State=cbChecked) then
    AddOption('-s');
  // Debug info.  
  // Writeln(stderr,'Debug options');
  If (CBGeneralDebugInfo.State=cbChecked) then
    begin
    temp:='-g';
    If (CBGeneralgsym.State=cbChecked) then
       Temp := Temp+'g';
    If (CBGeneraldbx.State=cbChecked) then
      Temp := Temp+'d';
    If (CBGeneralHeaptrace.State=cbChecked) then
      Temp := Temp+'h';
    If (CBGeneralLineInfo.State=cbChecked) then
      Temp := Temp+'l';
    If (CBGeneralCheckPointers.State=cbChecked) then
      Temp := Temp+'c';
    AddOption(Temp);  
    end;
  If (CBGeneralProfile.State=cbChecked) then
    AddOption('-pg');
  If (CBGeneralKeepAsm.State=cbChecked) then
    begin
    Temp:='-a';
    If (CBGeneralAsmListSource.State=cbChecked) then
       Temp := Temp+'l';
    If (CBGeneralAsmListRegAlloc.State=cbChecked) then
       Temp := Temp+'r';
    If (CBGeneralAsmListTempAlloc.State=cbChecked) then
       Temp := Temp+'t';
    AddOption(Temp);
    end;  
  If Length(EGeneralDefines.Text)>0 then
    SplitOption(EGeneralDefines.Text,'-d');
  If Length(EGeneralUnDefines.Text)>0 then
    SplitOption(EGeneralUnDefines.Text,'-u');
end;

Procedure TCompilerOptionsForm.OptionsToForm;

   Function Switch(Const S : String) : Boolean;
    
   begin
     result:=Not(S='-');
   end; 
   
   function Setbool(const opts:string; var pos: Longint):boolean;
   
   // return False if next char after pos is '-'
     
   begin
     Result := Not((Length(Opts) > Pos) And (Opts[Succ(Pos)] = '-'));
     if not result then
       inc(pos);
   end;

   Procedure InterpretOption (Option : String);

   Var Opt,Opt2 : Char;
       i,j : longint;
          
   begin
     Opt:=Option[2];
     Delete(Option,1,2);
     Case Opt of
       'a' : begin
             CBGeneralKeepAsm.Checked:=True;
             For I:=1 to Length(Option) do 
               Case Option[j] of
                 'l' : CBGeneralAsmListSource.Checked:=True;
                 'r' : CBGeneralAsmListRegAlloc.Checked:=True;
                 't' : CBgeneralAsmListTempAlloc.Checked:=True;
                 '-' : begin
                       CBGeneralAsmListSource.Checked:=False;
                       CBGeneralAsmListRegAlloc.Checked:=False;
                       CBgeneralAsmListTempAlloc.Checked:=False;
                       CBGeneralKeepAsm.Checked:=False;
                       end;
               end;
             end;
       'A' : Begin
             // To be implemented.
             end;      
       'b' : begin
             CBGeneralBrowserInfo.Checked:=True;
             if Option='l' then
               CBGeneralLocalBrowserInfo.Checked:=True
             else if OPtion='-' then
               begin
               CBGeneralLocalBrowserInfo.Checked:=False;
               CBGeneralBrowserInfo.Checked:=False;
               end;  
             end;
       'B' : CBGeneralBuild.Checked:=Switch(Option);
       'C' : begin
             j:=1;
             While j<=Length(Option) do
               begin
               Case Option[j] of
                 'h' : begin
                       ECodeheapSize.Text:=Copy(Option,2,255);
                       Break;
                       end; 
                 's' : begin
                       ECodeStackSize.Text:=Copy(Option,2,255);
                       Break;
                       end; 
                 't' : CBCodeStackCheck.Checked:=SetBool(Option,j);
                 'i' : CBCodeIOCheck.Checked:=SetBool(Option,j);
                 'n' : begin
                       CBLinkOmitLinking.Checked:=Not SetBool(OPtion,j);
                       GBLinkOptions.Enabled:=Not CBLinkOmitLinking.Checked;
                       end;
                 'o' : CBCodeOverflow.Checked:=SetBool(Option,j);
                 'r' : CBCodeRangeCheck.Checked:=SetBool(Option,j);
                 'D' : CBLinkMakeDynLib.Checked:=SetBool(Option,j);
                 'X' : CBLinkMakeSmartLink.Checked:=SetBool(Option,j);
                 
                 end;
               Inc(j);  
               end;  
             end;
       'd' : EGeneralDefines.Text:=EGeneralDefines.Text+';'+Option;
       'e' : FToolsSearchDir.Text:=Option;
       'E' : CBLinkOmitLinking.Checked:=Switch(Option);
       'F' : begin
             Opt2:=Option[1];
             Delete(Option,1,1);
             Case Opt2 of
               'D' : FToolsSearchDir.Text:=Option;
               'i' : FIncludePath.Text:=FIncludePath.text+';'+Option;
               'l' : FLibrarySearchPath.Text:=FLibrarySearchPath.text+';'+Option;
               'o' : FObjectSearchPath.Text:=FObjectSearchPath.text+';'+Option;
               'u' : FUnitSearchPath.Text:=FUnitSearchPath.text+';'+Option;
               'U' : FUnitOutputDir.Text:=Option;
               'E' : FOutputDir.Text:=Option;
             end;
             end;
       'g' : begin
             j:=0;
             CBGeneralDebugInfo.Checked:=SetBool(Option,J);
             GBGeneralDebugOptions.Enabled:=CBGeneralDebugInfo.Checked;
             if CBGeneralDebugInfo.Checked then
               begin
               While j<=Length(Option) do
                 begin
                 Case Option[j] of
                   'd' : CBGeneraldbx.Checked:=True;
                   'g' : CBGeneralgsym.Checked:=True; 
                   'h' : CBGeneralHeaptrace.Checked:=True;
                   'l' : CBGeneralLineInfo.Checked:=true; 
                   'c' : CBGeneralCheckPointers.Checked:=true;
                   end;
                 inc(j);
                 end;
               end;
             end;
       'I' : FIncludePath.Text:=FIncludePath.text+';'+Option;
       'k' : ELinkLinkerOptions.text:=ELinkLinkerOptions.Text+option;
       'l' : CBShowLogo.Checked:=Switch(Option); 
       'n' : CBGeneralNoConfig.Checked:=Switch(Option);
       'o' : ELinkOutputName.Text:=Option;
       'p' : begin
             j:=1;
             CBgeneralProfile.Checked:=(length(Option)>0) and (option[1]='g')
                                        and SetBool(Option,J);
             end;                           
       's' : CBGeneralScript.Checked:=Switch(Option);
       'S' : begin
             for i:=1 to length(Option) do
               case Option[i] of
                 '2' : RBSyntaxObjfpc.Checked:=true;
                 'd' : RBSyntaxDelphi.Checked:=True;
                 'o' : RBSyntaxTP.Checked:=true;
                 'c' : CBSyntaxCOperators.Checked:=True;
                 'g' : CBSyntaxAllowGoto.Checked:=True;
                 'h' : CBSyntaxAnsiStrings.Checked:=True;
                 'i' : CBSyntaxInline.Checked:=True;
                 'm' : CBSyntaxMacros.Checked:=true;
                 's' : CBSyntaxConst.Checked:=true;
                 't' : CBSyntaxStatic.Checked:=true;
               end;  
             end;
       'u' : EGeneralUndefines.Text:=EGeneralUndefines.Text+';'+Option;
       'U' : for i:=1 to length(Option) do
               Case OPtion[i] of
                 'n' : CBGeneralCheckUnitName.Checked:=true;
                 'g' : CBGeneralSystemUnit.Checked:=True;
               end;
       'v' : for i:=1 to length(option) do
               if (Option[i] in ['0','a']) then
                 begin
                 cbShowAll.Checked:=Option[i]='a';
                 cbShowNothing.Checked:=option[i]='0';
                 SetVerbosities(Option[i]='a');
                 FGeneralgroup.Enabled:=False;
                 end
               else
                  begin
                  CBShowAll.Checked:=False;
                  CBShowNothing.Checked:=False;
                  FGeneralGroup.Enabled:=True;
                  case Option[i] of
                    'w' : CBShowWarnings.Checked:=True;
                    'n' : CBShowNotes.Checked:=True;
                    'h' : CBShowHints.Checked:=true;
                    'i' : CBShowInfo.Checked:=true;
                    'l' : CBShowLines.Checked:=true;
                    'b' : CBShowProcedureBacktrace.Checked:=True;
                    'u' : CBShowUnit.Checked:=True;
                    'd' : CBShowDebug.Checked:=true;
                    't' : CBShowTried.Checked:=True;
                    'm' : CBShowMacros.Checked:=true;
                    'p' : CBShowProcedures.Checked:=True;
                    'c' : CBShowConditionals.Checked:=true;
                    'r' : CBShowGCC.Checked:=True;
                    end;
                  end;  
       'X' : For i:=1 to Length(Option) do
               Case Option[i] of
                 'c' : CBLinkUseCLib.Checked:=True;
                 's' : CBLinkStripSymbols.Checked:=True;
                 'X' : RBLinkLinkSmart.Checked:=true;
                 'S' : RBLinkLinkStatic.Checked:=True;
                 'D' : RBLinkLinkDynamic.Checked:=true;
               end;           
       'R' : begin
             RBSyntaxAsmAtt.Checked:=option='att';
             RBSyntaxAsmIntel.Checked:=Option='intel';
             RBSyntaxAsmDirect.Checked:=Option='direct';
             end;
       'O' : begin
             j:=1;
             While j<=Length(OPtion) do
               begin
               case Option[j] of
                 'g' : CBCodeSmaller.Checked:=true;
                 'G' : CBCodeFaster.Checked:=true;
                 'r' : CBCoderegister.Checked:=True;
                 'u' : CBCodeuncertain.Checked:=True;
                 'p' : if j<Length(Option) then
                         begin
                         inc(j);
                         CBCodeProcessor.Checked:=True;
                         GBProcType.Enabled:=True;
                         Case Option[j] of
                          '1' : RBCode386.Checked:=True;
                          '2' : RBCodePentium.Checked:=True;
                          '3' : RBCodePentiumPro.Checked:=True;
                         end;
                         end;
                 '1' : RBCodeLevel1.Checked:=True;
                 '2' : RBCodeLevel2.Checked:=True;
                 '3' : RBCodeLevel3.Checked:=True;
               end;  { Case }
               inc(j);
               end;
             end;  
     end; {Case Opt}      
   end;

Var i : longint;
    Temp : string;
  
begin
  If CompilerOptions.Count=0 then
    exit;
  For I:=0 to CompilerOptions.Count-1 do
    begin
    Temp:=Trim(CompilerOptions[i]);
    If (Length(Temp)>1) and (Temp[1]='-') then
      InterPretOption(Temp);
    end;
end;

Procedure TCompilerOptionsForm.SetCompilerOptions (Value : TStrings);

begin
  FCompilerOptions.Text:=Value.Text;
end;

end.

{
  $Log$
  Revision 1.7  2000/03/01 22:41:29  michael
  + Implemented Options2Form

  Revision 1.6  2000/02/25 14:51:17  michael
  Some cosmetics and output name added

  Revision 1.5  2000/02/25 13:55:47  michael
  Fixed some minor bugs. All options now passed correctly.

  Revision 1.4  2000/02/25 11:41:07  michael
  + Options can be read after dialog closes.


  revision 1.3
  date: 2000/02/24 22:11:55;  author: sg;  state: Exp;  lines: +3 -2
  * Fixed OK/Cancel button layout

  revision 1.2
  date: 2000/02/24 21:11:04;  author: michael;  state: Exp;  lines: +447 -18
  + All options present now.

  revision 1.1
  date: 2000/02/24 13:00:46;  author: michael;  state: Exp;
  + Initial implementation

}