{
  MIT License

  Copyright (c) 2019 Anderson J. Gado da Silva and Hélio S. Ribeiro

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit RSQL_Crypto_JWT;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  RSQL_Crypto_HMAC,
  RSQL_Crypto_BASE64,
  RSQL_Helper;

function JWTParse(const ATOKEN, AKEY: string; out AOutput: string): boolean; overload;
function JWTSign(const AKEY, APAYLOAD: string): string;

implementation

uses
  strutils;

function JWTHeader: string;
begin
  with TJSONObject.Create() do
    try
      Add('alg', 'HS256');
      Add('typ', 'JWT');
      Result := Stringify();
    finally
      Free;
    end;
end;

function JWTParse(const ATOKEN, AKEY: string; out AOutput: string): boolean;
var
  VHeaderSeg: string;
  VPAYLOADSeg: string;
  VSignatureSeg: string;
  VHeader: TJSONObject;
  VPAYLOAD: TJSONObject;
  VSignature: string;
begin
  Result := False;
  try
    /// Segments
    VHeaderSeg := BASE64URLDecode(ExtractWord(1, ATOKEN, ['.']));
    VPAYLOADSeg := BASE64URLDecode(ExtractWord(2, ATOKEN, ['.']));
    VSignatureSeg := ExtractWord(3, AToken, ['.']);
    /// Check signature
    VSignature := BASE64URLEncode(HMACSHA256(AKEY, VHeaderSeg + '.' + VPAYLOADSeg));
    if (VSignature <> VSignatureSeg) then
    begin
      raise Exception.Create('signature verification failed');
    end;
    /// Check algorithm type
    if (TJSONData.Parse(VHeaderSeg, VHeader)) then
    begin
      try
        if (VHeader.Path('alg', '') <> 'HS256') then
        begin
          raise Exception.Create('algorithm not supported');
        end;
      finally
        FreeAndNil(VHeader);
      end;
    end;
    /// Check if token is expired
    if (TJSONData.Parse(VPAYLOADSeg, VPAYLOAD)) then
    begin
      try
        if (VPAYLOAD.Path('exp', 0) <= Now) then
        begin
          raise Exception.Create('TOKEN expired');
        end;
      finally
        FreeAndNil(VPAYLOAD);
      end;
    end;
    { TODO : Add new checks }
    AOutput := 'JWT signature verified';
    Result := True;
  except
    on E: Exception do
    begin
      AOutput := Format('JWT invalid. %s', [E.Message]);
    end;
  end;
end;

function JWTSign(const AKEY, APAYLOAD: string): string;
var
  VHeader: string;
  VPAYLOAD: string;
  VSignature: string;
begin
  VHeader := BASE64URLEncode(JWTHeader);
  VPAYLOAD := BASE64URLEncode(APAYLOAD);
  VSignature := BASE64URLEncode(HMACSHA256(AKEY, (VHeader + '.' + VPAYLOAD)));
  Result := VHeader + '.' + VPAYLOAD + '.' + VSignature;
end;

end.
