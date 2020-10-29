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
unit RSQL_Helper;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  jsonscanner,
  jsonparser,
  fpjson,
  DB,
  sqldb,
  httpdefs,
  httproute;

type

  { TJSONDataHelper }

  TJSONDataHelper = class helper for TJSONData
  public
    class function Parse(const ASource: TJSONStringType): TJSONData; static; overload;
    class function Parse(const ASource: TJSONStringType;
      out AJSONData: TJSONData): boolean; static; overload;
    class function Parse(const ASource: TJSONStringType;
      out AJSONArray: TJSONArray): boolean; static; overload;
    class function Parse(const ASource: TJSONStringType;
      out AJSONObject: TJSONObject): boolean; static; overload;
  public
    function Path(const AName: TJSONStringType): TJSONData; overload;
    function Path(const AName: TJSONStringType;
      const ADefault: TJSONStringType): TJSONStringType; overload;
    function Path(const AName: TJSONStringType; const ADefault: boolean): boolean;
      overload;
    function Path(const AName: TJSONStringType;
      const ADefault: TJSONFloat): TJSONFloat; overload;
    function Path(const AName: TJSONStringType; const ADefault: integer): integer;
      overload;
    function Path(const AName: TJSONStringType; const ADefault: int64): int64;
      overload;
    function Path(const AName: TJSONStringType;
      const ADefault: TJSONData): TJSONData; overload;
    function Path(const AName: TJSONStringType;
      const ADefault: TJSONArray): TJSONArray; overload;
    function Path(const AName: TJSONStringType;
      const ADefault: TJSONObject): TJSONObject; overload;
  public
    function Stringify(const AHumanReader: boolean = False): TJSONStringType;
  end;

  { TParamsHelper }

  TParamsHelper = class helper for TParams
  public
    procedure AssignJSON(const AJSONData: TJSONData);
  end;

  { TSQLQueryHelper }

  TSQLQueryHelper = class helper for TSQLQuery
  public
    {$IfNDef rsql_experimental}
    function SaveToJSON: TJSONObject;
    {$Else}
    function SaveToJSON(const ARecno, APacketRecords: Int64): TJSONObject;
    {$EndIf}
  end;

  { TSQLConnectionHelper }

  TSQLConnectionHelper = class helper for  TSQLConnection
  public
    procedure FillIndexDefs(AIndexDefs: TIndexDefs; const ATableName: string);
    function ExtractSchemaSQL(const ASchemaType: TSchemaType;
      const ASchemaObjectName, ASchemaPattern: string): string;
    function ExtractSequenceSQL(const ASequenceName: string;
      const AIncrementBy: integer): string;
  end;

  { EHTTP }

  EHTTP = class(Exception)
  private
    FStatusCode: integer;
  public
    constructor Create(const ACode: integer; const AMessage: string);
    constructor CreateFmt(const ACode: integer; const AMessage: string;
      const AArgs: array of const);
    property StatusCode: integer read FStatusCode write FStatusCode;
  end;

  { TResponseHelper }

  TResponseHelper = class helper for TResponse
  public
    procedure Envelop(const ACode: integer; const AContent: TJSONData); overload;
    procedure Envelop(const ACode: integer; const AContent: string); overload;
    procedure Envelop(const ACode: integer; const AContent: string;
      const AArgs: array of const); overload;
  end;

  { TCustomRoute }

  TCustomRoute = class(TObject)
  public
    procedure HandleRequest(AServer: TObject; ARequest: TRequest;
      AResponse: TResponse); virtual; abstract;
  end;
  TRouteClass = class of TCustomRoute;

  { THTTPRouterHelper }

  THTTPRouterHelper = class helper for THTTPRouter
  public
    function RegisterRoute(const APattern: string; const ARouteClass: TRouteClass;
      AIsDefault: boolean = False): THttpRoute; overload;
    function RegisterRoute(const APattern: string; AMethod: TRouteMethod;
      const ARouteClass: TRouteClass; AIsDefault: boolean = False): THttpRoute; overload;
    procedure RouteRequest(AServer: TObject; ARequest: TRequest;
      AResponse: TResponse); overload;
  end;

implementation

uses
  RSQL_Crypto_BASE64,
  RSQL_Crypto_ZStream;

{ TJSONDataHelper }

class function TJSONDataHelper.Parse(const ASource: TJSONStringType): TJSONData;
begin
  Result := nil;
  with TJSONParser.Create(ASource, [joUTF8]) do
  begin
    try
      try
        Result := Parse;
      except
        FreeAndNil(Result);
      end;
    finally
      Free;
    end;
  end;
end;

class function TJSONDataHelper.Parse(const ASource: TJSONStringType;
  out AJSONData: TJSONData): boolean;
begin
  AJSONData := Parse(ASource);
  Result := Assigned(AJSONData);
end;

class function TJSONDataHelper.Parse(const ASource: TJSONStringType;
  out AJSONArray: TJSONArray): boolean;
var
  VJSONData: TJSONData;
begin
  Result := False;
  AJSONArray := nil;
  if (Parse(ASource, VJSONData)) then
  begin
    if (VJSONData.JSONType = jtArray) then
    begin
      AJSONArray := TJSONArray(VJSONData);
      Result := True;
    end
    else
    begin
      FreeAndNil(VJSONData);
    end;
  end;
end;

class function TJSONDataHelper.Parse(const ASource: TJSONStringType;
  out AJSONObject: TJSONObject): boolean;
var
  VJSONData: TJSONData;
begin
  Result := False;
  AJSONObject := nil;
  if (Parse(ASource, VJSONData)) then
  begin
    if (VJSONData.JSONType = jtObject) then
    begin
      AJSONObject := TJSONObject(VJSONData);
      Result := True;
    end
    else
    begin
      FreeAndNil(VJSONData);
    end;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType): TJSONData;
begin
  Result := FindPath(AName);
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: TJSONStringType): TJSONStringType;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtString) then
  begin
    Result := VJSONData.AsString;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: boolean): boolean;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtBoolean) then
  begin
    Result := VJSONData.AsBoolean;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: TJSONFloat): TJSONFloat;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtNumber) then
  begin
    Result := VJSONData.AsInteger;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: int64): int64;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtNumber) then
  begin
    Result := VJSONData.AsInteger;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: integer): integer;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtNumber) then
  begin
    Result := VJSONData.AsInt64;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: TJSONData): TJSONData;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) then
  begin
    Result := VJSONData;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: TJSONArray): TJSONArray;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtArray) then
  begin
    Result := VJSONData as TJSONArray;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Path(const AName: TJSONStringType;
  const ADefault: TJSONObject): TJSONObject;
var
  VJSONData: TJSONData;
begin
  VJSONData := Path(AName);
  if (Assigned(VJSONData)) and (VJSONData.JSONType = jtObject) then
  begin
    Result := VJSONData as TJSONObject;
  end
  else
  begin
    Result := ADefault;
  end;
end;

function TJSONDataHelper.Stringify(const AHumanReader: boolean): TJSONStringType;
begin
  if (AHumanReader) then
  begin
    Result := FormatJSON();
  end
  else
  begin
    Result := FormatJSON(AsCompressedJSON);
  end;
end;

{ TParamsHelper }

procedure TParamsHelper.AssignJSON(const AJSONData: TJSONData);
var
  VParam: TParam;
  VIndex: integer;
  VName: string;
  VJSONData: TJSONData;
  VJSONObject: TJSONObject;
begin
  if (Assigned(AJSONData)) and (AJSONData.JSONType = jtObject) then
  begin
    VJSONObject := TJSONObject(AJSONData);
    for VIndex := 0 to (VJSONObject.Count - 1) do
    begin
      VName := VJSONObject.Names[VIndex];
      VJSONData := VJSONObject.Elements[VName];
      VParam := ParamByName(VName);
      if (Assigned(VJSONData)) and (Assigned(VParam)) then
      begin
        case VJSONData.JSONType of
          jtBoolean:
          begin
            VParam.AsBoolean := VJSONData.AsBoolean;
          end;
          jtNumber:
          begin
            VParam.AsFloat := VJSONData.AsFloat;
          end;
          jtString:
          begin
            VParam.AsString := VJSONData.AsString;
          end;
          jtObject:
          begin
            case (LowerCase(VJSONData.Path('type', 'string'))) of
              'boolean':
              begin
                VParam.AsBoolean := VJSONData.Path('value', False);
              end;
              'integer':
              begin
                VParam.AsLargeInt := VJSONData.Path('value', 0);
              end;
              'number':
              begin
                VParam.AsFloat := VJSONData.Path('value', 0);
                VParam.Precision:=VJSONData.Path('precision', 0);
              end;
              'currency':
              begin
                VParam.AsCurrency := VJSONData.Path('value', 0.0);
                VParam.Precision:=VJSONData.Path('precision', 0);
              end;
              'datetime':
              begin
                VParam.AsDateTime := VJSONData.Path('value', 0.0);
              end;
              'date':
              begin
                VParam.AsDate := VJSONData.Path('value', 0.0);
              end;
              'time':
              begin
                VParam.AsTime := VJSONData.Path('value', 0.0);
              end;
              else
              begin
                if (VJSONData.Path('b64', True)) and
                  (VJSONData.Path('value', EmptyStr) <> EmptyStr) then
                begin
                  VParam.AsString := BASE64Decode(VJSONData.Path('value', EmptyStr));
                end
                else
                begin
                  VParam.AsString := VJSONData.Path('value', EmptyStr);
                end;
                //VParam.Precision:=VJSONData.Path('precision', 0);
              end;
            end;
            //VParam.Size:=VJSONData.Path('size', 0);
          end;
          else
          begin
            VParam.Clear;
          end;
        end;
      end;
    end;
  end;
end;

{ TSQLQueryHelper }

{$IfNDef rsql_experimental}
function TSQLQueryHelper.SaveToJSON: TJSONObject;
{$Else}                                                                              
function TSQLQueryHelper.SaveToJSON(const ARecno, APacketRecords: Int64): TJSONObject;
{$EndIf}

  function ExtractMetadata: TJSONArray;
  var
    VIndex: integer;
    VField: TFieldDef;
    VColumn: TJSONObject;
  begin
    Result := TJSONArray.Create([]);
    for VIndex := 0 to (FieldDefs.Count - 1) do
    begin
      VField := FieldDefs[VIndex];
      if (Assigned(VField)) then
      begin
        VColumn := TJSONObject.Create([]);
        VColumn.Add('name', VField.Name);
        VColumn.Add('type', Fieldtypenames[VField.DataType]);
        VColumn.Add('length', VField.Size);
        VColumn.Add('precision', VField.Precision);  
        VColumn.Add('required', faRequired in VField.Attributes);
        VColumn.Add('readonly', faReadonly in VField.Attributes);
        VColumn.Add('hidden', faHiddenCol in VField.Attributes);
        VColumn.Add('fixed', faFixed in VField.Attributes);
        Result.Add(VColumn);
      end;
    end;
  end;

  function ExtractRow: TJSONArray;
  var
    VIndex: integer;
    VField: TField;
  begin
    Result := TJSONArray.Create();
    for VIndex := 0 to (FieldCount - 1) do
    begin
      VField := Fields[VIndex];
      if (Assigned(VField)) then
      begin
        if (VField.IsNull) then
        begin
          Result.Add;
        end
        else
        begin
          case VField.DataType of
            ftString,
            ftWideString,
            ftFixedChar,
            ftFixedWideChar,
            ftGuid:
            begin
              Result.Add(UTF8Decode(VField.AsString));
            end;
            ftBoolean:
            begin
              Result.Add(VField.AsBoolean);
            end;
            ftInteger,
            ftLargeint,
            ftSmallint,
            ftWord,
            ftAutoInc:
            begin
              Result.Add(VField.AsLargeInt);
            end;
            ftFloat,
            ftCurrency,
            ftBCD,
            ftFMTBcd:
            begin
              Result.Add(VField.AsFloat);
            end;
            ftDateTime,
            ftTimeStamp:
            begin
              Result.Add(VField.AsDateTime);
            end;
            ftDate:
            begin
              Result.Add(VField.AsDateTime);
            end;
            ftTime:
            begin
              Result.Add(VField.AsDateTime);
            end;
            ftBlob,
            ftBytes,
            ftGraphic,
            ftVarBytes,
            ftMemo,
            ftWideMemo,
            ftTypedBinary:
            begin
              Result.Add(BASE64Encode(VField.AsString));
            end;
            else
            begin
              Result.Add(VField.AsString);
            end;
          end;
        end;
      end;
    end;
  end;

  {$IfNDef rsql_experimental}
  function ExtractRows: TJSONArray;
  {$Else}                                                        
  function ExtractRows(ARecno, APacketRecords: Int64): TJSONArray;
  {$EndIf}
  begin
    Result := TJSONArray.Create();
    if IsEmpty then
      Exit;
    try
      DisableControls;
      {$IfNDef rsql_experimental}
      First;
      while not EOF do
      begin
        Result.Add(ExtractRow);
        Next;
      end;
      {$Else}
      if APacketRecords > 0 then
      begin
        ARecno:=ARecno+1;

        if (ARecno < 1) then
        begin
          ARecno := 1;
        end;

        //WriteLn('RECORDCOUNT ',RecordCount);
        //if (ARecno-1) > RecordCount then
          //Exit;

        //RecNo := ARecno;
        while not(EOF) and (RecNo < (ARecno + APacketRecords)) do
        begin
          if not (RecNo < ARecno) then
            Result.Add(ExtractRow);
          Next;
        end;
      end
      else
      begin
        First;
        while not EOF do
        begin
          Result.Add(ExtractRow);
          Next;
        end;
      end;

      {$EndIf}
    finally
      EnableControls;
    end;
  end;

begin
  Result := TJSONObject.Create();
  if (StatementType <> stSelect) then
  begin
    Result.Add('rowsaffected', RowsAffected);
  end
  else
  begin
    {$IfNDef rsql_experimental}
    Result.Add('rows', ExtractRows);
    {$Else}
    Result.Add('rows', ExtractRows(ARecno, APacketRecords));
    {$EndIf}
    Result.Add('rowsaffected', RowsAffected);
    Result.Add('metadata', ExtractMetadata);
  end;
end;

{ TSQLConnectionHelper }

procedure TSQLConnectionHelper.FillIndexDefs(AIndexDefs: TIndexDefs;
  const ATableName: string);
var
  VTransaction: TSQLTransaction;
begin
  if (not (Assigned(Transaction))) then
  begin
    try
      VTransaction := TSQLTransaction.Create(nil);
      VTransaction.DataBase := Self;
      UpdateIndexDefs(AIndexDefs, ATableName);
    finally
      FreeAndNil(VTransaction);
    end;
  end
  else
  begin
    UpdateIndexDefs(AIndexDefs, ATableName);
  end;
end;

function TSQLConnectionHelper.ExtractSchemaSQL(const ASchemaType: TSchemaType;
  const ASchemaObjectName, ASchemaPattern: string): string;
begin
  Result := GetSchemaInfoSQL(ASchemaType, ASchemaObjectName, ASchemaPattern);
end;

function TSQLConnectionHelper.ExtractSequenceSQL(const ASequenceName: string;
  const AIncrementBy: integer): string;
begin
  Result := GetNextValueSQL(ASequenceName, AIncrementBy);
end;

{ EHTTP }

constructor EHTTP.Create(const ACode: integer; const AMessage: string);
begin
  FStatusCode := ACode;
  inherited Create(AMessage);
end;

constructor EHTTP.CreateFmt(const ACode: integer; const AMessage: string;
  const AArgs: array of const);
begin
  FStatusCode := ACode;
  inherited CreateFmt(AMessage, AArgs);
end;

{ TResponseHelper }

procedure TResponseHelper.Envelop(const ACode: integer; const AContent: TJSONData);
begin
  Self.Code := ACode;
  with TJSONObject.Create() do
  begin
    try
      Add('success', (ACode >= 200) and (ACode <= 299));
      Add('content', AContent);
      if (Self.ContentEncoding = EmptyStr) then
      begin
        Self.Content := Stringify();
      end
      else
      begin
        // Encode response
        if (LowerCase(Self.ContentEncoding) = 'deflate') then
        begin
          Self.Content := ZCompressString(Stringify());
        end
        else
        begin
          Self.Content := Stringify();
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TResponseHelper.Envelop(const ACode: integer; const AContent: string);
begin
  Envelop(ACode, TJSONString.Create(AContent));
end;

procedure TResponseHelper.Envelop(const ACode: integer; const AContent: string;
  const AArgs: array of const);
begin
  Envelop(ACode, Format(AContent, AArgs));
end;

type

  { TCustomHttpRoute }

  TCustomHTTPRoute = class(THTTPRoute)
  private
    FRouteClass: TRouteClass;
  public
    procedure HandleRequest(AServer: TObject; ARequest: TRequest;
      AResponse: TResponse); overload;
    property RouteClass: TRouteClass read FRouteClass write FRouteClass;
  end;

procedure TCustomHTTPRoute.HandleRequest(AServer: TObject; ARequest: TRequest;
  AResponse: TResponse);
var
  VCustomRoute: TCustomRoute;
begin
  VCustomRoute := FRouteClass.Create;
  try
    VCustomRoute.HandleRequest(AServer, ARequest, AResponse);
  finally
    FreeAndNil(VCustomRoute);
  end;
end;

{ THTTPRouterHelper }

function THTTPRouterHelper.RegisterRoute(const APattern: string;
  const ARouteClass: TRouteClass; AIsDefault: boolean): THttpRoute;
begin
  Result := RegisterRoute(APattern, rmAll, ARouteClass, AIsDefault);
end;

function THTTPRouterHelper.RegisterRoute(const APattern: string;
  AMethod: TRouteMethod; const ARouteClass: TRouteClass;
  AIsDefault: boolean): THttpRoute;
begin
  Result := CreateHTTPRoute(TCustomHTTPRoute, APattern, AMethod, AIsDefault);
  TCustomHTTPRoute(Result).RouteClass := ARouteClass;
end;

procedure THTTPRouterHelper.RouteRequest(AServer: TObject; ARequest: TRequest;
  AResponse: TResponse);
var
  VPath: string;
  VMethod: TRouteMethod;
  VRoute: THTTPRoute;
  VParams: TStrings;
  VIndex: integer;
  VName: string;
  VValue: string;
  VMethodMisMatch: boolean;
begin
  VPath := GetRequestPath(ARequest);
  VMethod := StringToRouteMethod(ARequest.Method);
  VParams := TStringList.Create;
  try
    VRoute := FindHTTPRoute(VPath, VMethod, VParams, VMethodMisMatch);
    if (Assigned(VRoute)) then
    begin
      for VIndex := 0 to (VParams.Count - 1) do
      begin
        VParams.GetNameValue(VIndex, VName, VValue);
        if (VName <> '') then
        begin
          ARequest.RouteParams[VName] := VValue;
        end;
      end;
      if (VRoute.InheritsFrom(TCustomHTTPRoute)) then
      begin
        TCustomHTTPRoute(VRoute).HandleRequest(AServer, ARequest, AResponse);
      end
      else
      begin
        VRoute.HandleRequest(ARequest, AResponse);
      end;
    end
    else
    begin
      if (VMethodMisMatch) then
      begin
        AResponse.Envelop(405, 'the requested method %s is not allowed', [VMethod]);
      end
      else
      begin
        AResponse.Envelop(404, 'not found');
      end;
    end;
  finally
    FreeAndNil(VParams);
  end;
end;

end.
