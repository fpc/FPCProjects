unit fprPackageIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fprWebModule;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents('fpWeb',[TfprWebModule]);
end;

end.

