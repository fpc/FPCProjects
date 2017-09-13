unit cnocOIDCIDToken;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjwt,
  fpoauth2;

type
  { TcnocOIDCIDTokenClaim }

  TcnocOIDCIDTokenClaim = class(TClaims)
  private
    Facr: string;
    Fauth_time: Int64;
    Fazp: string;
    Fclient_id: string;
    Fnonce: string;
    //Fscope: string;
  published
    property client_id: string read Fclient_id write Fclient_id;
    property auth_time: Int64 read Fauth_time write Fauth_time;
    property nonce: string read Fnonce write Fnonce;
    property acr: string read Facr write Facr;
    property azp: string read Fazp write Fazp;

  //    property scope: string read Fscope write Fscope;
  end;

  TcnocOIDCIDTokenJWT = class(TJWT)
  protected
    Function CreateClaims: TClaims; override;
  end;


implementation

{ TcnocOIDCIDTokenJWT }

Function TcnocOIDCIDTokenJWT.CreateClaims: TClaims;
begin
  Result := TcnocOIDCIDTokenClaim.Create;;
end;

end.

