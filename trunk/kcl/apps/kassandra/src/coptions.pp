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
  STabCompilerMessages = 'Compiler &Messages';
  STabSyntax = '&Pascal Syntax';
  STabCode = 'Generated Code'  ;
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
  SSyntaxMacros = 'Allow macros';
  SSyntaxConst = 'Constructor name must be init';
  SSyntaxStatic = 'Allow static keyword in objects';
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
  SLinkDynlib = 'Create dynamic library';
  SLinkSmartLink = 'Create Smartlinked units';
  
Type
    TFileEdit = TEdit; // change to TCombobox when implementing histories
 
    TCompilerOptionsForm = Class(TForm)
       FLabel : Tlabel;
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
       GBSYntaxMode : TGroupBox;
       RBSyntaxFPC ,
       RBSyntaxDelphi ,
       RBSyntaxTP ,
       RBSyntaxObjfpc : TRadioButton;
       GBSyntaxOther : TgroupBox;
       CBSyntaxCOperators ,
       CBSyntaxAllowGoto ,
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
       
       // Buttons
       OKButton,
       CancelButton : TButton;
       Function CreateDirectoryPage : TWidget;
       Function CreateVerbosityPage : TWidget;
       Function CreateSyntaxPage : TWidget;
       Function CreateCodePAge : TWidget;
       Function CreateButtons : TWidget;
       Procedure OnOkClick (Sender : TObject);
       Procedure OnEnableOptimizationsClick (Sender : TObject);
       Procedure OnVerbosityOffClick (Sender : TObject);
       Procedure OnVerbosityOnClick (Sender : TObject);
       Procedure OnSetProcessorOnClick (Sender : TObject);
       Procedure SetVerbosities (OnOff : Boolean);
     Public
       Constructor Create (AOwner : TComponent); Override;
       Destructor Destroy; override;  
     end;

Var 
  CompilerOptionsForm : TCompilerOptionsForm;
     
Implementation

Constructor TCompilerOptionsForm.Create (AOwner : TComponent);

 Var MainLayout : TBoxLayout;
     Page : Longint;
             
begin
  Inherited Create(AOwner);
  Text := SProjectOptions;
  Name:='ProjectOptions';
//  Content:=CreateDirectoryPage;
  BorderWidth:=8;
  MainLayout:=TBoxLayout.Create(Self);
  MainLayout.Orientation:=BoxVert;
  MainLayout.Spacing:=8;
  FOptionPanes:=TNoteBook.Create(Self);
  Page:=FOptionPanes.AddPage(STabDirectories,CreateDirectoryPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabCompilerMessages,CreateVerbosityPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabSyntax,CreateSyntaxPage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  Page:=FOptionPanes.AddPage(STabCode,CreateCodePage);
  FOptionPanes.Pages[page].BorderWidth := 8;
  FoptionPanes.FinishCreation;
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
    HorzSpacing := 4;
    VertSpacing := 4;
    SameSizeCells:=False;
    // Unit Search path
    FUnitSearchPath:=TFileEdit.Create(Self);
    FunitSearchPath.SetSize(200,25);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SUnitSearchPath;
    ALabel.CanExpandWidth := False;
    AddWidget(ALabel,0,0,1,1);
    AddWidget(FUnitSearchPath,1,0,1,1);
    // include search path
    FIncludePath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SIncludePath;
    AddWidget(ALabel,0,1,1,1);
    AddWidget(FIncludePath,1,1,1,1);
    // Object file search path
    FObjectSearchPath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SObjectSearchPath;
    AddWidget(ALabel,0,2,1,1);
    AddWidget(FObjectSearchPath,1,2,1,1);
    // Library search parth
    FLibrarySearchPath:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SLibrarySearchPath;
    AddWidget(ALabel,0,3,1,1);
    AddWidget(FLibrarySearchPath,1,3,1,1);
    // Tools search path
    FToolsSearchDir:=TFileEdit.Create(Self);
    ALabel:=TLabel.Create(Self);
    ALabel.Text:=SToolsSearchDir;
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
    Alabel.CanExpandWidth:=FAlse;
    AddWidget(ALabel,0,0,1,1);
    Alabel:=TLabel.Create(Self);
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
    VertAlign:=VertFill;
    Spacing:=8;
    // OKbutton;
    OKButton:=TButton.Create(Self);
    OKButton.Text:=SOK;
    OKButton.OnClick:=@OnOKClick;
    AddWidget(OKButton);
    // Cancel button;
    CancelButton:=TButton.Create(Self);
    CancelButton.Text:=SCancel;
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
  SetVerbosities(OnOff);
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

var Layout : TGridLayout;

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
  with GBSyntaxMode do 
    begin
    Name:='SyntaxMode';
    Text:=SSyntaxMode;
    Content:=TBoxLayout.Create(Self);
    With Content as TBoxLAyout do
      begin
      Name:='ModeLayout';
      Orientation:=boxVert;
      AddWidget(RBSyntaxFPC);
      AddWidget(RBSyntaxTP);
      AddWidget(RBSyntaxObjFPC);
      AddWidget(RBSyntaxDelphi);
      end;
    end;
  CBSyntaxCOperators := TCheckBox.Create(Self);
  CBSyntaxAllowGoto := TCheckBox.Create(Self);
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
      AddWidget(CBSyntaxMacros);
      AddWidget(CBSyntaxConst);
      AddWidget(CBSyntaxStatic);
      end;
    end;  
  Layout:=TGridLayout.Create(Self);
  With Layout do
    begin
    Name:='SyntaxLayout1';
    Rows:=1;
    Columns:=2;
    HorzSpacing:=8;
    AddWidget(GBSyntaxMode,0,0,1,1);
    AddWidget(GBSyntaxOther,1,0,1,1);
    end;
  Result:=TBoxLAyout.Create(Self);
  With Result as TBoxLAyout do
    begin
    NAme:='SyntaxLAyout';
    Orientation:=boxVert;
    VertAlign:=vertTop;
    AddWidget(Layout);
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
      ALabel.Text:=SCodeHeapSize;
      AddWidget(Alabel,0,0,1,1);
      AddWidget(ECodeHeapSize,1,0,1,1);
      ALabel:=TLabel.Create(Self);
      ALabel.Text:=SCodestackSize;
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

Procedure TCompilerOptionsForm.OnOkClick (Sender : TObject);

begin
  Close;
end;

end.
