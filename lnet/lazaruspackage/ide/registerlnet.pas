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
unit RegisterlNet;

interface

uses
  Classes, LResources, LazarusPackageIntf,
  lNetComponents;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('lNet' , [TLTcpComponent, TLUdpComponent,
                               TLFTPClientComponent]);
end;

initialization
 {$i registerlnet.lrs}

end.
 
