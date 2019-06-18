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
unit RSQL_Crypto_HMAC;

{$I rsql.inc}
{$R-}

interface

uses
  Classes,
  SysUtils;

function HMACSHA256(const AKey, AData: string): string;

implementation

/// Ref: https://github.com/stijnsanders/TRethinkDB/blob/master/RethinkDBAuth.pas#L35
function SWAPEndian32(Value: cardinal): cardinal;
var
  x: array[0..3] of byte absolute Result;
  y: array[0..3] of byte absolute Value;
begin
  x[0] := y[3];
  x[1] := y[2];
  x[2] := y[1];
  x[3] := y[0];
end;
                               
/// Ref: https://github.com/stijnsanders/TRethinkDB/blob/master/RethinkDBAuth.pas#L46
function SHA256HASH(x: string): string;
const
  base: array[0..63] of cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5,
    $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3,
    $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc,
    $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7,
    $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
    $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3,
    $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5,
    $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208,
    $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);
var
  a, b: cardinal;
  dl, i, j: integer;
  d: array of cardinal;
  e: array[0..63] of cardinal;
  g, h: array[0..7] of cardinal;
begin
  Result := '';
  d := nil;
  a := Length(x);
  dl := a + 9;
  if (dl mod 64) <> 0 then
  begin
    dl := ((dl div 64) + 1) * 64;
  end;
  i := dl;
  dl := dl div 4;
  SetLength(d, dl);
  SetLength(x, i);
  j := a + 1;
  x[j] := #$80;
  while j < i do
  begin
    Inc(j);
    x[j] := #0;
  end;
  Move(x[1], d[0], i);
  d[dl - 1] := SWAPEndian32(a shl 3);
  h[0] := $6a09e667;
  h[1] := $bb67ae85;
  h[2] := $3c6ef372;
  h[3] := $a54ff53a;
  h[4] := $510e527f;
  h[5] := $9b05688c;
  h[6] := $1f83d9ab;
  h[7] := $5be0cd19;
  i := 0;
  while i < dl do
  begin
    j := 0;
    while j < 16 do
    begin
      e[j] := SWAPEndian32(d[i]);
      Inc(i);
      Inc(j);
    end;
    while j < 64 do
    begin
      a := e[j - 15];
      b := e[j - 2];
      e[j] := e[j - 16] + (((a shr 7) or (a shl 25)) xor ((a shr 18) or (a shl 14)) xor (a shr 3)) + e[j - 7] + (((b shr 17) or (b shl 15)) xor ((b shr 19) or (b shl 13)) xor (b shr 10));
      Inc(j);
    end;
    g := h;
    j := 0;
    while j < 64 do
    begin
      a := g[4];
      b := g[0];
      a := g[7] + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((g[4] and g[5]) or (not (g[4]) and g[6])) + base[j] + e[j];
      Inc(g[3], a);
      a := a + (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) or (b shl 10))) + ((g[0] and g[1]) or (g[1] and g[2]) or (g[2] and g[0]));
      g[7] := g[6];
      g[6] := g[5];
      g[5] := g[4];
      g[4] := g[3];
      g[3] := g[2];
      g[2] := g[1];
      g[1] := g[0];
      g[0] := a;
      Inc(j);
    end;
    for j := 0 to 7 do
    begin
      Inc(h[j], g[j]);
    end;
  end;
  SetLength(Result, 32);
  for j := 0 to 31 do
  begin
    Result[j + 1] := AnsiChar(h[j shr 2] shr ((3 - (j and 3)) * 8));
  end;
end;

function HMACSHA256(const AKey, AData: string): string;
const
  BlockSize = 64;
var
  k, h1, h2: string;
  i: integer;
begin
  Result := '';
  if (AKey <> '') then
  begin
    h1 := '';
    h2 := '';
    if Length(AKey) > BlockSize then
    begin
      k := SHA256HASH(AKey);
    end
    else
    begin
      k := AKey;
      i := Length(k);
      SetLength(k, BlockSize);
      while (i < BlockSize) do
      begin
        Inc(i);
        k[i] := #0;
      end;
    end;
    SetLength(h1, BlockSize);
    SetLength(h2, BlockSize);
    for i := 1 to BlockSize do
    begin
      byte(h1[i]) := byte(k[i]) xor $5C;
    end;
    for i := 1 to BlockSize do
    begin
      byte(h2[i]) := byte(k[i]) xor $36;
    end;
    Result := SHA256HASH(h1 + SHA256HASH(h2 + AData));
  end;
end;

end.

