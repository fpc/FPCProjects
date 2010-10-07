{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazProf; 

interface

uses
  LazProfView, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LazProfView', @LazProfView.Register); 
end; 

initialization
  RegisterPackage('LazProf', @Register); 
end.
