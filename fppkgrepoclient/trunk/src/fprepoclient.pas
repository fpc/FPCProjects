{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fprepoclient;

interface

uses
  authmantis, dcserverclient, mantis, mantis_proxy, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fprepoclient', @Register);
end.
