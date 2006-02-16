{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lnetidepackage; 

interface

uses
  RegisterlNet, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterlNet', @RegisterlNet.Register); 
end; 

initialization
  RegisterPackage('lnetidepackage', @Register); 
end.
