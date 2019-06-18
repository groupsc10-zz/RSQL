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
unit RSQL_Crypto_ZStream;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  ZStream;

function ZCompressString(AString: string; const ALevel: TCompressionLevel = clmax): string;
function ZDecompressString(AString: string): string;

implementation

uses
  base64;

function ZCompressString(AString: string; const ALevel: TCompressionLevel): string;
var
  VCompressionStream: Tcompressionstream;
  VStream: TStringStream;
begin
  VStream := TStringStream.Create('');
  try
    VCompressionStream := Tcompressionstream.Create(ALevel, VStream);
    try
      VCompressionStream.WriteAnsiString(AString);
    finally
      FreeAndNil(VCompressionStream);
    end;
    Result := EncodeStringBase64( VStream.DataString );
  finally
    FreeAndNil(VStream);
  end;
end;

function ZDecompressString(AString: string): string;
var
  VDeCompressionStream: Tdecompressionstream;
  VStream: TStringStream;
begin
  VStream := TStringStream.Create(DecodeStringBase64(AString));
  try
    VDeCompressionStream := Tdecompressionstream.Create(VStream);
    try
      VDeCompressionStream.Position:= 0;
      Result := VDeCompressionStream.ReadAnsiString;
    finally
      FreeAndNil(VDeCompressionStream);
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

end.
