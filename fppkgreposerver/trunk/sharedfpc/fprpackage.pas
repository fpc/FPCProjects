{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fprPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  fprWebModule, fprPackageIDEIntf, fprErrorHandling, fprPackageUtils, fprBuildTask, fprGCollection, 
  fprDeleteTree, fprBuildAgentResponse, fprCopyTree, fprJSONRTTI, fprInterfacedCollection, 
  fprFPCVersion, fprWebHandler, fprAuthenticationHandler, fprSetupLogging, fprJSONFileStreaming, 
  fprStackClient, fprModel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fprPackageIDEIntf', @fprPackageIDEIntf.Register);
end;

initialization
  RegisterPackage('fprPackage', @Register);
end.
