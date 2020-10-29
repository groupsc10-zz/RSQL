{
  MIT License

  Copyright (c) 2020 Anderson J. Gado da Silva and Hélio S. Ribeiro

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
unit RSQL_Crypto_BASE64;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils;

function BASE64Decode(const AValue: string): string;
function BASE64Encode(const AValue: string): string;

function BASE64URLDecode(const AValue: string): string;
function BASE64URLEncode(const AValue: string): string;

implementation

uses
  strutils,
  base64;

function BASE64Decode(const AValue: string): string;
begin
  if (AValue = EmptyStr) then
  begin
    Result := AValue;
  end
  else
  begin
    Result := DecodeStringBASE64(AValue);
  end;
end;

function BASE64Encode(const AValue: string): string;
begin
  if (AValue = EmptyStr) then
  begin
    Result := AValue;
  end
  else
  begin
    Result := EncodeStringBASE64(AValue);
  end;
end;

function BASE64URLDecode(const AValue: string): string;
var
  VLength: integer;
begin
  if (AValue = EmptyStr) then
  begin
    Result := AValue;
  end
  else
  begin
    Result := StringsReplace(AValue, ['-', '_'], ['+', '/'], [rfReplaceAll]);
    VLength := (Length(Result) mod 4);
    if (VLength > 0) then
    begin
      Result := Result + StringOfChar('=', 4 - VLength);
    end;
    Result := BASE64Decode(Result);
  end;
end;

function BASE64URLEncode(const AValue: string): string;
begin
  if (AValue = EmptyStr) then
  begin
    Result := AValue;
  end
  else
  begin
    Result := BASE64Encode(AValue);
    Result := StringsReplace(Result, ['+', '/'], ['-', '_'], [rfReplaceAll]);
    Result := TrimRightSet(Result, ['=']);
  end;
end;

end.
