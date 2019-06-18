{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RSQL_Package;

{$warn 5023 off : no warning about unused units}
interface

uses
  RSQL_Helper, RSQL_Crypto_HMAC, RSQL_Crypto_BASE64, RSQL_Crypto_ZStream, 
  RSQL_Crypto_JWT, RSQL_Server_Database, RSQL_Server_Transaction, 
  RSQL_Server_Router, RSQL_Server_Component, RSQL_Server_Application, 
  RSQL_Client_Connection, RSQL_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RSQL_Register', @RSQL_Register.Register);
end;

initialization
  RegisterPackage('RSQL_Package', @Register);
end.
