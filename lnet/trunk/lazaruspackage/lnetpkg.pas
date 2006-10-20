{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lnetpkg; 

interface

uses
  lNet, lEvents, lCommon, lFTP, lsmtp, lTelnet, lhttp, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('lnetpkg', @Register); 
end.
