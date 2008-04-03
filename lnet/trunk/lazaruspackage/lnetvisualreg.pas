{
 *****************************************************************************
 *                                                                           *
 *  See the file LICENSE and LICENSE.ADDON, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Ales Katona
  
  This unit registers the lnet components.
}
unit LNetVisualReg;

interface

uses
  Classes, LResources, LazarusPackageIntf,
  lNetComponents;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('lNet' , [TLTcpComponent, TLUdpComponent, TLTelnetClientComponent,
                               TLFTPClientComponent, TLSMTPClientComponent,
                               TLHTTPClientComponent, TLHTTPServerComponent,
                               TLSSLSessionComponent]);
end;

initialization
 {$i lnetvisualreg.lrs}

end.
 
