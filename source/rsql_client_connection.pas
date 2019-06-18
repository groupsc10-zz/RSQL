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
unit RSQL_Client_Connection;

{$I rSQL.inc}

interface

uses
  Classes,
  SysUtils,
  DB,
  SQLdb,
  BufDataset,
  fphttpclient,
  fpjson;

type

  { TRSQLHTTPConnection }

  TRSQLHTTPConnection = class(TSQLConnection)
  private         
    FCompressed: boolean;
    FTOKEN: string;
    function POST(const ARoute: string; const ASource: string = '{}'): string; overload;
    function POST(const ARoute: string; const AArguments: array of const; const ASource: string = '{}'): string; overload;
  protected
    procedure DoInternalConnect; override;

    function AllocateCursorHandle: TSQLCursor; override;
    procedure DeAllocateCursorHandle(var ACursor: TSQLCursor); override;
    function AllocateTransactionHandle: TSQLHandle; override;

    procedure PrepareStatement(ACursor: TSQLCursor; {%H-}ATransaction: TSQLTransaction; ABuf: string; {%H-}AParams: TParams); override;
    procedure UnPrepareStatement(ACursor: TSQLCursor); override;
    function GetTransactionHandle(ATrans: TSQLHandle): pointer; override;
    function StartDBTransaction(ATrans: TSQLHandle; {%H-}AParams: string): boolean; override;
    function Commit(ATrans: TSQLHandle): boolean; override;
    function Rollback(ATrans: TSQLHandle): boolean; override;
    procedure CommitRetaining(ATrans: TSQLHandle); override;
    procedure RollbackRetaining(ATrans: TSQLHandle); override;

    procedure Execute(ACursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams); override;
    function RowsAffected(ACursor: TSQLCursor): TRowsCount; override;
    procedure AddFieldDefs(ACursor: TSQLCursor; AFieldDefs: TFieldDefs); override;
    function Fetch(ACursor: TSQLCursor): boolean; override;
    function LoadField(ACursor: TSQLCursor; AFieldDef: TFieldDef; ABuffer: pointer; out ACreateBlob: boolean): boolean; override;
    procedure LoadBlobIntoBuffer(AFieldDef: TFieldDef; ABlobBuf: PBufBlobField; ACursor: TSQLCursor; {%H-}ATransaction: TSQLTransaction); override;

    procedure UpdateIndexDefs(AIndexDefs: TIndexDefs; ATableName: string); override;
    function GetSchemaInfoSQL(ASchemaType: TSchemaType; ASchemaObjectName, ASchemaPattern: string): string; override;
    function GetNextValueSQL(const ASequenceName: string; AIncrementBy: integer): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetConnectionInfo(AInfoType: TConnInfoType): string; override;
  published
    property Port default 8091;
  end;

  { TRSQLHTTPConnectionDef }

  TRSQLHTTPConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;

  { ERSQLError }

  ERSQLError = class(ESQLDatabaseError);

implementation

uses
  FMTBcd,
  RSQL_Helper,
  RSQL_Crypto_ZStream,
  RSQL_Crypto_BASE64;

type

  { TRSQLTrans }

  TRSQLTrans = class(TSQLHandle)
  strict private
    FIdentifier: string;
  public
    property Identifier: string read FIdentifier write FIdentifier;
  end;

  { TRSQLCursor }

  TRSQLCursor = class(TSQLCursor)
  strict private
    FBuffer: TJSONData;
    FStatement: string;
    FColumns: TJSONObject;
    FIndexFetch: int64;
    FRowsAffected: int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PrepareData(const AData: TJSONData);
    function Fetch: boolean;
    function Metadata: TJSONArray;
    function Rows: TJSONArray;
    function BuildRequestStatement(const AParams: TParams): string;
    function Columns: TJSONObject;
  public
    property Buffer: TJSONData read FBuffer;
    property IndexFetch: int64 read FIndexFetch write FIndexFetch;
    property RowsAffected: int64 read FRowsAffected write FRowsAffected;
    property Statement: string read FStatement write FStatement;
    property Prepared: boolean read FPrepared write FPrepared;
  end;

constructor TRSQLCursor.Create;
begin
  inherited Create;
  FBuffer := nil;
  FColumns := nil;
  FStatement := '';
  FIndexFetch := 0;
  FRowsAffected := 0;
end;

destructor TRSQLCursor.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TRSQLCursor.PrepareData(const AData: TJSONData);
var
  VRows: TJSONData;
begin
  FStatement := '';
  FIndexFetch := 0;
  FRowsAffected := 0;
  FreeAndNil(FColumns);
  FreeAndNil(FBuffer);
  if (Assigned(AData)) then
  begin
    FBuffer := AData;
    VRows := FBuffer.Path('rows');
    if (Assigned(VRows)) then
    begin
      FRowsAffected := VRows.Count;
    end;
  end;
end;

function TRSQLCursor.Fetch: boolean;
begin
  Result := (FRowsAffected > FIndexFetch);
  if (Result) then
  begin
    Inc(FIndexFetch);
  end;
end;

function TRSQLCursor.Metadata: TJSONArray;
var
  VJSONData: TJSONData;
begin
  Result := nil;
  if (Assigned(FBuffer)) then
  begin
    VJSONData := FBuffer.Path('metadata');
    if (Assigned(VJSONData)) and (VJSONData.JSONType = jtArray) then
    begin
      Result := TJSONArray(VJSONData);
    end;
  end;
end;

function TRSQLCursor.Rows: TJSONArray;
var
  VJSONData: TJSONData;
begin
  Result := nil;
  if (Assigned(FBuffer)) then
  begin
    VJSONData := FBuffer.Path('rows');
    if (Assigned(VJSONData)) and (VJSONData.JSONType = jtArray) then
    begin
      Result := TJSONArray(VJSONData);
    end;
  end;
end;

function TRSQLCursor.BuildRequestStatement(const AParams: TParams): string;

  function BuildStatement: TJSONString;
  begin
    Result := TJSONString.Create(FStatement);
  end;

  function BuildParam(const AParam: TParam): TJSONData;
  begin
    if (AParam.IsNull) then
    begin
      Result := TJSONNull.Create;
    end
    else
    begin
      case AParam.DataType of
        ftAutoInc,
        ftLargeint,
        ftWord:
        begin
          Result := TJSONInt64Number.Create(AParam.AsLargeInt);
        end;
        ftInteger,
        ftSmallint:
        begin
          Result := TJSONIntegerNumber.Create(AParam.AsInteger);
        end;
        ftBCD,
        ftCurrency,
        ftFMTBcd:
        begin
          Result := TJSONObject.Create();
          with TJSONObject(Result) do
          begin
            Add('value', AParam.AsCurrency);
            Add('type', 'currency');
            Add('length', AParam.Size);
            Add('type', AParam.Precision);
          end;
        end;
        ftBoolean:
        begin
          Result := TJSONBoolean.Create(AParam.AsBoolean);
        end;
        ftDate:
        begin
          Result := TJSONObject.Create();
          with TJSONObject(Result) do
          begin
            Add('value', AParam.AsDate);
            Add('type', 'date');
          end;
        end;
        ftTime:
        begin
          Result := TJSONObject.Create();
          with TJSONObject(Result) do
          begin
            Add('value', AParam.AsTime);
            Add('type', 'time');
          end;
        end;
        ftDateTime,
        ftTimeStamp:
        begin
          Result := TJSONObject.Create();
          with TJSONObject(Result) do
          begin
            Add('value', AParam.AsDateTime);
            Add('type', 'datetime');
          end;
        end;
        ftFixedWideChar:
        begin
          Result := TJSONString.Create(AParam.AsWideString);
        end;
        ftBlob,
        ftMemo,
        ftGraphic,
        ftFmtMemo,
        ftWideMemo:
        begin
          Result := TJSONObject.Create();
          with TJSONObject(Result) do
          begin
            Add('value', BASE64Encode(AParam.AsString));
            Add('type', 'blob');
            Add('b64', True);
          end;
        end;
        else
        begin
          Result := TJSONString.Create(AParam.AsString);
        end;
      end;
    end;
  end;

  function BuildParams(const AParams: TParams): TJSONObject;
  var
    VIndex: integer;
    VParam: TParam;
  begin
    Result := TJSONObject.Create();
    if (Assigned(AParams)) and (AParams.Count > 0) then
    begin
      for VIndex := 0 to (AParams.Count - 1) do
      begin
        VParam := AParams[VIndex];
        if (Assigned(VParam)) then
        begin
          Result.Add(VParam.Name, BuildParam(VParam));
        end;
      end;
    end;
  end;

begin
  with TJSONObject.Create() do
    try
      Add('sql', BuildStatement);
      Add('params', BuildParams(AParams));
      Result := Stringify();
    finally
      Free;
    end;
end;

function TRSQLCursor.Columns: TJSONObject;
var
  VMetadata: TJSONArray;
  VData: TJSONData;
  VIndex: integer;
  VName: string;
begin
  if (not (Assigned(FColumns))) then
  begin
    FColumns := TJSONObject.Create();
    VMetadata := Metadata;
    if (Assigned(VMetadata)) then
    begin
      for VIndex := 0 to (VMetadata.Count - 1) do
      begin
        VData := VMetadata[VIndex];
        if (Assigned(VData)) then
        begin
          VName := VData.Path('name', '');
          if (VName <> '') then
          begin
            FColumns.Add(VName, VIndex);
          end;
        end;
      end;
    end;
  end;
  Result := FColumns;
end;

{ TRSQLHTTPConnection }

function TRSQLHTTPConnection.POST(const ARoute: string; const ASource: string): string;

  function URLBase: string;
  begin
    if (Port < 1) then
    begin
      Port := 8091;
    end;
    if (HostName = '') then
    begin
      HostName := 'localhost';
    end;
    Result := Format('http://%s:%d/', [HostName, Port]);
  end;

var
  VContentEncoding: string;
begin
  with TFpHttpClient.Create(nil) do
    try
      try                                          
        AddHeader('Authorization', Format('Bearer %s', [FTOKEN]));
        AddHeader('Content-Type', 'application/json');
        if (FCompressed) then
        begin
          /// Encode request
          AddHeader('Content-Encoding', 'deflate');
          RequestBody := TStringStream.Create(ZCompressString(ASource));  
          { TODO : add other forms of encode }
        end
        else
        begin
          RequestBody := TStringStream.Create(ASource);
        end;
        Result := Post(URLBase + ARoute); /// Send ==>>
        /// Decode response
        VContentEncoding := Trim(ResponseHeaders.Values['Content-Encoding']);
        if (VContentEncoding <> '') then
        begin
          case (LowerCase(VContentEncoding)) of
            'deflate':
            begin
              Result := ZDecompressString(Result);
            end;
            { TODO : add other forms of decode }
          end;
        end;
      except
        on E: Exception do
        begin
          Self.Close(True); /// Close connection
          raise;
        end;
      end;
    finally
      RequestBody.Free;
      RequestBody := nil;
      Free;
    end;
end;

function TRSQLHTTPConnection.POST(const ARoute: string; const AArguments: array of const; const ASource: string): string;
begin
  Result := POST(Format(ARoute, AArguments), ASource);
end;

procedure TRSQLHTTPConnection.DoInternalConnect;

  function AuthenticationSource: string;
  begin
    with TJSONObject.Create() do
      try
        Add('username', UserName);
        Add('password', Password);
        Result := Stringify();
      finally
        Free;
      end;
  end;

var
  VResponse: TJSONData;
  VRoute: string;
begin
  inherited DoInternalConnect;
  FCompressed := False;
  FTOKEN := '';
  VRoute := Format('authentication?database=%s', [DatabaseName]);
  if (TJSONData.Parse(POST(VRoute, AuthenticationSource), VResponse)) then
    try
      if (VResponse.Path('success', False)) then
      begin                 
        FCompressed := VResponse.Path('content.compressed', false);
        FTOKEN := VResponse.Path('content.token', '');
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', ''));
      end;
    finally
      FreeAndNil(VResponse);
    end;
end;

function TRSQLHTTPConnection.AllocateCursorHandle: TSQLCursor;
begin
  Result := TRSQLCursor.Create;
end;

procedure TRSQLHTTPConnection.DeAllocateCursorHandle(var ACursor: TSQLCursor);
begin
  FreeAndNil(ACursor);
end;

function TRSQLHTTPConnection.AllocateTransactionHandle: TSQLHandle;
begin
  Result := TRSQLTrans.Create;
end;

procedure TRSQLHTTPConnection.PrepareStatement(ACursor: TSQLCursor; ATransaction: TSQLTransaction; ABuf: string; AParams: TParams);
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  VCursor.Statement := ABuf;
end;

procedure TRSQLHTTPConnection.UnPrepareStatement(ACursor: TSQLCursor);
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  VCursor.Prepared := False;
end;

function TRSQLHTTPConnection.GetTransactionHandle(ATrans: TSQLHandle): pointer;
begin
  Result := ATrans;
end;

function TRSQLHTTPConnection.StartDBTransaction(ATrans: TSQLHandle; AParams: string): boolean;
var
  VTrans: TRSQLTrans absolute ATrans;
  VResponse: TJSONData;
  VRoute: string;
begin
  VRoute := Format('transaction?action=start&database=%s', [DatabaseName]);
  if (TJSONData.Parse(POST(VRoute), VResponse)) then
    try
      if (VResponse.Path('success', False)) then
      begin
        VTrans.Identifier := VResponse.Path('content', '');
        Result := True;
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', ''));
      end;
    finally
      FreeAndNil(VResponse);
    end;
end;

function TRSQLHTTPConnection.Commit(ATrans: TSQLHandle): boolean;
var
  VTrans: TRSQLTrans absolute ATrans;
  VResponse: TJSONData;
  VRoute: string;
begin
  VRoute := Format('transaction?action=commit&identifier=%s&database=%s', [VTrans.Identifier, DatabaseName]);
  if (TJSONData.Parse(POST(VRoute), VResponse)) then
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := True;
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', ''));
      end;
    finally
      FreeAndNil(VResponse);
    end;
end;

function TRSQLHTTPConnection.Rollback(ATrans: TSQLHandle): boolean;
var
  VTrans: TRSQLTrans absolute ATrans;
  VResponse: TJSONData;
  VRoute: string;
begin
  VRoute := Format('transaction?action=rollback&identifier=%s&database=%s', [VTrans.Identifier, DatabaseName]);
  if (TJSONData.Parse(POST(VRoute), VResponse)) then
    try
      Result := True;
    finally
      FreeAndNil(VResponse);
    end;
end;

procedure TRSQLHTTPConnection.CommitRetaining(ATrans: TSQLHandle);
begin
  Commit(ATrans);
  StartDBTransaction(ATrans, Params.CommaText);
end;

procedure TRSQLHTTPConnection.RollbackRetaining(ATrans: TSQLHandle);
begin
  Rollback(ATrans);
  StartDBTransaction(ATrans, Params.CommaText);
end;

procedure TRSQLHTTPConnection.Execute(ACursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
var
  VCursor: TRSQLCursor absolute ACursor;
  VTrans: TRSQLTrans;
  VQuerySource: string;
  VResponse: TJSONData;
  VRoute: string;
begin
  VQuerySource := VCursor.BuildRequestStatement(AParams);
  VTrans := TRSQLTrans(ATransaction.Handle);
  VRoute := Format('statement?action=query&identifier=%s&database=%s', [VTrans.Identifier, DatabaseName]);
  if (TJSONData.Parse(POST(VRoute, VQuerySource), VResponse)) then
    try
      if (VResponse.Path('success', False)) then
      begin
        VCursor.PrepareData(VResponse.Path('content').Clone);
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', ''));
      end;
    finally
      FreeAndNil(VResponse);
    end;
end;

function TRSQLHTTPConnection.RowsAffected(ACursor: TSQLCursor): TRowsCount;
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  Result := VCursor.RowsAffected;
end;

procedure TRSQLHTTPConnection.AddFieldDefs(ACursor: TSQLCursor; AFieldDefs: TFieldDefs);
var
  VCursor: TRSQLCursor absolute ACursor;
  VMetadata: TJSONArray;
  VData: TJSONData;
  VIndex: integer;
  VType: TFieldType;
  VName: string;
  VLength: integer;
  VPrecision: integer;
  VRequired: boolean;
  VReadonly: boolean;
  VHidden: boolean;
begin
  VMetadata := VCursor.Metadata;
  if (Assigned(VMetadata)) then
  begin
    for VIndex := 0 to (VMetadata.Count - 1) do
    begin
      VData := VMetadata[VIndex];
      if (Assigned(VData)) then
      begin
        case (LowerCase(VData.Path('type', ''))) of
          'string': VType := ftString;
          'widestring': VType := ftWideString;
          'boolean': VType := ftBoolean;
          'integer': VType := ftLargeint;
          'number': VType := ftFloat;
          'datetime': VType := ftDateTime;
          'date': VType := ftDate;
          'time': VType := ftTime;
          'blob': VType := ftBlob;
          'unknown': VType := ftUnknown;
        end;
        VName := VData.Path('name', '');
        VLength := VData.Path('length', 0);
        VPrecision := VData.Path('precision', 0);
        VRequired := VData.Path('required', False);
        VReadonly := VData.Path('readonly', False);
        VHidden := VData.Path('hidden', False);
        /// Add field
        with AFieldDefs.AddFieldDef do
        begin
          Name := VName;
          DataType := VType;
          Size := VLength;
          Required := VRequired;
          Precision := VPrecision;
          if (VReadonly) then
          begin
            Attributes := Attributes + [faReadonly];
          end;
          if (VHidden) then
          begin
            Attributes := Attributes + [faHiddenCol];
          end;
        end;
      end;
    end;
  end;
end;

function TRSQLHTTPConnection.Fetch(ACursor: TSQLCursor): boolean;
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  Result := VCursor.Fetch;
end;

function TRSQLHTTPConnection.LoadField(ACursor: TSQLCursor; AFieldDef: TFieldDef; ABuffer: pointer; out ACreateBlob: boolean): boolean;
var
  VCursor: TRSQLCursor absolute ACursor;
  VIndexCol: int64;
  VIndexRow: int64;
  VRows: TJSONArray;
  VColumns: TJSONObject;
  VData: TJSONData;
begin
  Result := False;
  ACreateBlob := False;
  VIndexRow := VCursor.IndexFetch - 1;
  VRows := VCursor.Rows;
  VColumns := VCursor.Columns;
  VData := nil;
  if (VIndexRow > -1) and (Assigned(VRows)) and
    (Assigned(VColumns)) and (VColumns.Count > 0) then
  begin
    VIndexCol := VColumns.Path(AFieldDef.Name, -1);
    VData := VRows.Path(Format('[%d][%d]', [VIndexRow, VIndexCol]));
    if (Assigned(VData)) and (not (VData.IsNull)) then
    begin
      Result := True;
      case AFieldDef.DataType of
        ftAutoInc,
        ftInteger:
        begin
          pinteger(ABuffer)^ := VData.AsInteger;
        end;
        ftSmallInt:
        begin
          psmallint(ABuffer)^ := VData.AsInt64;
        end;
        ftWord:
        begin
          pword(ABuffer)^ := VData.AsInteger;
        end;
        ftBoolean:
        begin
          pwordbool(ABuffer)^ := VData.AsBoolean;
        end;
        ftLargeInt:
        begin
          PInt64(ABuffer)^ := VData.AsInt64;
        end;
        ftBCD:
        begin
          PCurrency(ABuffer)^ := FloattoCurr(VData.AsFloat);
        end;
        ftFloat,
        ftCurrency:
        begin
          pdouble(ABuffer)^ := VData.AsFloat;
        end;
        ftDateTime:
        begin
          PDateTime(ABuffer)^ := FloatToDateTime(VData.AsFloat);
        end;
        ftDate:
        begin
          PDateTime(ABuffer)^ := TDate(FloatToDateTime(VData.AsFloat));
        end;
        ftTime:
        begin
          PDateTime(ABuffer)^ := TTime(FloatToDateTime(VData.AsFloat));
        end;
        ftFixedChar,
        ftString:
        begin
          if (Length(VData.AsString) > 0) then
          begin
            Move(PChar(VData.AsString)^, ABuffer^, Length(VData.AsString));
          end;
          PAnsiChar(ABuffer + Length(VData.AsString))^ := #0;
        end;
        ftFmtBCD:
        begin
          pBCD(ABuffer)^ := CurrToBCD(VData.AsFloat);
        end;
        ftFixedWideChar,
        ftWideString:
        begin
          PWideString(ABuffer)^ := WideString(VData.AsString);
        end;
        ftVarBytes,
        ftBytes:
        begin
          PWord(ABuffer)^ := VData.AsInteger;
        end;
        ftWideMemo,
        ftMemo,
        ftBlob:
        begin
          ACreateBlob := True;
        end;
        else
        begin
          Result := False;
        end;
      end;
    end;
  end;
end;

procedure TRSQLHTTPConnection.LoadBlobIntoBuffer(AFieldDef: TFieldDef; ABlobBuf: PBufBlobField; ACursor: TSQLCursor; ATransaction: TSQLTransaction);
var
  VCursor: TRSQLCursor absolute ACursor;
  VIndexCol: int64;
  VIndexRow: int64;
  VRows: TJSONArray;
  VColumns: TJSONObject;
  VData: TJSONData;
  VString: string;
  VLength: int64;
begin
  VIndexRow := VCursor.IndexFetch - 1;
  VRows := VCursor.Rows;
  VColumns := VCursor.Columns;
  VData := nil;
  VString := '';
  VLength := 0;
  if (VIndexRow > -1) and (Assigned(VRows)) and
    (Assigned(VColumns)) and (VColumns.Count > 0) then
  begin
    VIndexCol := VColumns.Path(AFieldDef.Name, -1);
    VData := VRows.Path(Format('[%d][%d]', [VIndexRow, VIndexCol]));
    if (Assigned(VData)) and (not (VData.IsNull)) then
    begin
      VString := BASE64Decode(VData.AsString);
      VLength := Length(VString);
      ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer, VLength);
      if (VLength > 0) then
      begin
        Move(PByte(VString)^, ABlobBuf^.BlobBuffer^.Buffer^, VLength);
      end;
      ABlobBuf^.BlobBuffer^.Size := VLength;
    end;
  end;
end;

procedure TRSQLHTTPConnection.UpdateIndexDefs(AIndexDefs: TIndexDefs; ATableName: string);

  function IndexSource: string;
  begin
    with TJSONObject.Create() do
      try
        Add('table', ATableName);
        Result := Stringify();
      finally
        Free;
      end;
  end;

  function JSONToIndexOptions(const AJSON: TJSONData): TIndexOptions;
  var
    VIndex: integer;
    VData: TJSONData;
  begin
    Result := [];
    if (Assigned(AJSON)) then
    begin
      for VIndex := 0 to (AJSON.Count - 1) do
      begin
        VData := AJSON.Items[VIndex];
        if (Assigned(VData)) and (VData.JSONType = jtString) then
        begin
          case LowerCase(VData.AsString) of
            'primary': Result := Result + [ixPrimary];
            'unique': Result := Result + [ixUnique];
            'descending': Result := Result + [ixDescending];
            'caseinsensitive': Result := Result + [ixCaseInsensitive];
            'expression': Result := Result + [ixExpression];
            'nonmaintained': Result := Result + [ixNonMaintained];
          end;
        end;
      end;
    end;
  end;

var
  VResponse: TJSONData;
  VContent: TJSONData;
  VContentItem: TJSONData;
  VIndex: integer;
  VRoute: string;
begin
  VRoute := Format('database?action=index&database=%s', [DatabaseName]);
  if (TJSONData.Parse(POST(VRoute, IndexSource), VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        VContent := VResponse.Path('content');
        if (Assigned(VContent)) then
        begin
          for VIndex := 0 to (VContent.Count - 1) do
          begin
            VContentItem := VContent.Items[VIndex];
            if (Assigned(VContentItem)) then
            begin
              with AIndexDefs.AddIndexDef do
              begin
                Name := VContentItem.Path('name', '');
                Expression := VContentItem.Path('expression', '');
                Fields := VContentItem.Path('fields', '');
                CaseInsFields := VContentItem.Path('caseInsFields', '');
                DescFields := VContentItem.Path('descFields', '');
                Options := JSONToIndexOptions(VContentItem.Path('options'));
                Source := VContentItem.Path('source', '');
              end;
            end;
          end;
        end;
      end
      else
      begin
        raise ERSQLError.CreateFmt('index update is not available for this type of database. %s', [VResponse.Path('content', '')]);
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end
  else
  begin
    inherited UpdateIndexDefs(AIndexDefs, ATableName);
  end;
end;

function TRSQLHTTPConnection.GetSchemaInfoSQL(ASchemaType: TSchemaType; ASchemaObjectName, ASchemaPattern: string): string;

  function SchemaSource: string;
  begin
    with TJSONObject.Create() do
      try
        Add('type', Ord(ASchemaType));
        Add('object', ASchemaObjectName);
        Add('pattern', ASchemaPattern);
        Result := Stringify();
      finally
        Free;
      end;
  end;

var
  VResponse: TJSONData;
  VRoute: string;
begin
  VRoute := Format('database?action=schema&database=%s', [DatabaseName]);
  if (TJSONData.Parse(POST(VRoute, SchemaSource), VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := VResponse.Path('content', '');
      end
      else
      begin
        raise ERSQLError.CreateFmt('schema is not available for this type of database. %s', [VResponse.Path('content', '')]);
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end
  else
  begin
    Result := inherited GetSchemaInfoSQL(ASchemaType, ASchemaObjectName, ASchemaPattern);
  end;
end;

function TRSQLHTTPConnection.GetNextValueSQL(const ASequenceName: string; AIncrementBy: integer): string;

  function SequenceSource: string;
  begin
    with TJSONObject.Create() do
      try
        Add('sequence', ASequenceName);
        Add('increment', AIncrementBy);
        Result := Stringify();
      finally
        Free;
      end;
  end;

var
  VResponse: TJSONData;
  VRoute: string;
begin
  VRoute := Format('database?action=sequence&database=%s', [DatabaseName]);
  if (TJSONData.Parse(POST(VRoute, SequenceSource), VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := VResponse.Path('content', '');
      end
      else
      begin
        raise ERSQLError.CreateFmt('sequence is not available for this type of database. %s', [VResponse.Path('content', '')]);
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end
  else
  begin
    Result := inherited GetNextValueSQL(ASequenceName, AIncrementBy);
  end;
end;

constructor TRSQLHTTPConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 8091;
  FConnOptions := FConnOptions + [sqSupportParams, sqEscapeRepeat, sqSupportReturning];
  FCompressed := False;
  FTOKEN := '';
end;

function TRSQLHTTPConnection.GetConnectionInfo(AInfoType: TConnInfoType): string;
begin
  case AInfoType of
    citServerType: Result := 'http';
    citServerVersion: Result := '0.9';
    citServerVersionString: Result := '09';
    citClientName: Result := 'rSQL';
    citClientVersion: Result := '0.9';
    else
    begin
      Result := inherited GetConnectionInfo(AInfoType);
    end;
  end;
end;


{ TRSQLHTTPConnectionDef }

class function TRSQLHTTPConnectionDef.TypeName: string;
begin
  Result := 'RSQL';
end;

class function TRSQLHTTPConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result := TRSQLHTTPConnection;
end;

class function TRSQLHTTPConnectionDef.Description: string;
begin
  Result := 'Connect to any database via an REST HTTP';
end;

initialization
  RegisterConnection(TRSQLHTTPConnectionDef);

finalization
  UnRegisterConnection(TRSQLHTTPConnectionDef);

end.
