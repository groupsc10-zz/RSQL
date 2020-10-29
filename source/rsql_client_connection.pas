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
  fpjson,
  opensslsockets;

type

  { TRSQLClient }

  TRSQLClient = class(TSQLConnection)
  private
    FCompressed: boolean;
    FTOKEN: string;
    FUseSSL: boolean;
  protected
    // - Connect/disconnect
    procedure DoInternalConnect; override;
    // - Handle (de)allocation
    function AllocateCursorHandle: TSQLCursor; override;
    procedure DeAllocateCursorHandle(var ACursor: TSQLCursor); override;
    function AllocateTransactionHandle: TSQLHandle; override;
    // - Statement handling
    procedure PrepareStatement(ACursor: TSQLCursor;
    {%H-}ATransaction: TSQLTransaction; ABuf: string;
    {%H-}AParams: TParams); override;
    procedure UnPrepareStatement(ACursor: TSQLCursor); override;
    // - Transaction handling
    function GetTransactionHandle(ATrans: TSQLHandle): pointer; override;
    function StartDBTransaction(ATrans: TSQLHandle;
    {%H-}AParams: string): boolean; override;
    function Commit(ATrans: TSQLHandle): boolean; override;
    function Rollback(ATrans: TSQLHandle): boolean; override;
    procedure CommitRetaining(ATrans: TSQLHandle); override;
    procedure RollbackRetaining(ATrans: TSQLHandle); override;
    // - Statement execution
    procedure Execute(ACursor: TSQLCursor; ATransaction: TSQLTransaction;
      AParams: TParams); override;
    function RowsAffected(ACursor: TSQLCursor): TRowsCount; override;
    // - Result retrieving
    procedure AddFieldDefs(ACursor: TSQLCursor; AFieldDefs: TFieldDefs); override;
    function Fetch(ACursor: TSQLCursor): boolean; override;
    function LoadField(ACursor: TSQLCursor; AFieldDef: TFieldDef;
      ABuffer: pointer; out ACreateBlob: boolean): boolean; override;
    procedure LoadBlobIntoBuffer(AFieldDef: TFieldDef; ABlobBuf: PBufBlobField;
      ACursor: TSQLCursor; {%H-}ATransaction: TSQLTransaction); override;
    // - UpdateIndexDefs
    procedure UpdateIndexDefs(AIndexDefs: TIndexDefs; ATableName: string); override;
    // - Schema info
    function GetSchemaInfoSQL(ASchemaType: TSchemaType;
      ASchemaObjectName, ASchemaPattern: string): string; override;
    function GetNextValueSQL(const ASequenceName: string;
      AIncrementBy: integer): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetConnectionInfo(AInfoType: TConnInfoType): string; override;
    function HttpRequest(const ARoute: string;
      const ASource: string = '{}'): TJSONData;
    property Compressed: boolean read FCompressed;
    property Token: string read FTOKEN;
  published
    property Port default 8091;
    property UseSSL: boolean read FUseSSL write FUseSSL;
  end;

  { TRSQLClientDef }

  TRSQLClientDef = class(TConnectionDef)
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
  private
    FConnection: TRSQLClient;
    FTransaction: TSQLTransaction;
    FStatement: string;
    FParams: TParams;
    FIndex: int64;
    FStatus: int64;
    FMax: int64;
    FMetadata: TJSONArray;
    FRows: TJSONArray;
    FRowsCount: int64;
    FRowsAffected: int64;
  protected
    function RequestStatement: string;
    procedure RequestNextPack;
  public
    constructor Create(const AConn: TRSQLClient);
    destructor Destroy; override;
    procedure Execute(const ATrans: TSQLTransaction; const AParams: TParams);
    function Fetch: boolean;
  public
    property Statement: string read FStatement write FStatement;
    property Prepared: boolean read FPrepared write FPrepared;
    property Index: int64 read FIndex write FIndex;
    property Status: int64 read FStatus write FStatus;
    property Metadata: TJSONArray read FMetadata;
    property Rows: TJSONArray read FRows;
    property RowsAffected: int64 read FRowsAffected;
  end;

function TRSQLCursor.RequestStatement: string;

  function BuildStatement(const AStatement: string): TJSONString;
  begin
    Result := TJSONString.Create(AStatement);
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
            Add('precision', AParam.Precision);
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

  function BuildOptions(const ARecno, APacketRecords: int64): TJSONObject;
  begin
    Result := TJSONObject.Create();
    Result.Add('recno', ARecno);
    Result.Add('packetrecords', APacketRecords);
  end;

begin
  with TJSONObject.Create() do
  begin
    try
      Add('sql', BuildStatement(FStatement));
      Add('params', BuildParams(FParams));
      {$IfDef rsql_experimental}
      Add('options', BuildOptions(FIndex, FMax));
      {$EndIf}
      Result := Stringify();
    finally
      Free;
    end;
  end;
end;

procedure TRSQLCursor.RequestNextPack;

  procedure Merge(const ATarget, AAdd: TJSONData);
  var
    VIndex: NativeInt;
  begin
    if (Assigned(ATarget)) and (Assigned(AAdd)) then
    begin
      for VIndex := 0 to (AAdd.Count - 1) do
      begin
        TJSONArray(ATarget).Add(AAdd.Items[VIndex].Clone);
      end;
    end;
  end;

var
  VQuerySource: string;
  VRoute: string;
  VResponse: TJSONData;
begin
  VQuerySource := RequestStatement;
  VRoute := Format('statement?action=query&identifier=%s&database=%s',
    [TRSQLTrans(FTransaction.Handle).Identifier, FConnection.DatabaseName]);
  VResponse := FConnection.HttpRequest(VRoute, VQuerySource);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        if  FStatementType = stSelect then
        begin
          if (not (Assigned(FRows))) then
          begin
            FMetadata := TJSONArray(VResponse.Path('content.metadata').Clone);
            FRows := TJSONArray(VResponse.Path('content.rows').Clone);
          end
          else
          begin
            {$IfDef rsql_experimental}
            Merge(FRows, VResponse.Path('content.rows'));
            {$EndIf}
          end;
          FRowsCount:=FRows.Count;
        end;
        FRowsAffected := VResponse.Path('content.rowsaffected', -1);
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', EmptyStr));
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end;
end;

constructor TRSQLCursor.Create(const AConn: TRSQLClient);
begin
  inherited Create;
  FConnection := AConn;
  FMax := 500;
end;

destructor TRSQLCursor.Destroy;
begin
  if Assigned(FMetadata) then
    FreeAndNil(FMetadata);
  if Assigned(FRows) then
    FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TRSQLCursor.Execute(const ATrans: TSQLTransaction; const AParams: TParams);
begin
  FreeAndNil(FMetadata);
  FreeAndNil(FRows);
  FIndex := 0;
  FStatus := 0;
  FTransaction := ATrans;
  FParams := AParams;
  RequestNextPack;
end;

function TRSQLCursor.Fetch: boolean;
begin
  {$IfDef rsql_experimental}
  if (FStatus >= FMax) then
  begin
    FStatus := 0;
    RequestNextPack;
  end;
  {$IfEnd}
  Result := FRowsCount > FIndex;//(Assigned(FRows)) and (FRows.Count > FIndex);
  if (Result) then
  begin
    Inc(FStatus);
    Inc(FIndex);
  end;
end;

{ TRSQLClient }

procedure TRSQLClient.DoInternalConnect;

  function AuthenticationSource: string;
  begin
    with TJSONObject.Create() do
    begin
      try
        Add('username', UserName);
        Add('password', Password);
        Result := Stringify();
      finally
        Free;
      end;
    end;
  end;

var
  VRoute: string;
  VResponse: TJSONData;
begin
  inherited DoInternalConnect;
  FCompressed := False;
  FTOKEN := EmptyStr;
  VRoute := Format('authentication?database=%s', [DatabaseName]);
  VResponse := HttpRequest(VRoute, AuthenticationSource);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        FCompressed := VResponse.Path('content.compressed', False);
        FTOKEN := VResponse.Path('content.token', EmptyStr);
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', EmptyStr));
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end;
end;

function TRSQLClient.AllocateCursorHandle: TSQLCursor;
begin
  Result := TRSQLCursor.Create(Self);
end;

procedure TRSQLClient.DeAllocateCursorHandle(var ACursor: TSQLCursor);
begin
  FreeAndNil(ACursor);
end;

function TRSQLClient.AllocateTransactionHandle: TSQLHandle;
begin
  Result := TRSQLTrans.Create;
end;

procedure TRSQLClient.PrepareStatement(ACursor: TSQLCursor;
  ATransaction: TSQLTransaction; ABuf: string; AParams: TParams);
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  VCursor.Statement := ABuf;
  VCursor.Prepared := True;
end;

procedure TRSQLClient.UnPrepareStatement(ACursor: TSQLCursor);
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  VCursor.Prepared := False;
end;

function TRSQLClient.GetTransactionHandle(ATrans: TSQLHandle): pointer;
begin
  Result := ATrans;
end;

function TRSQLClient.StartDBTransaction(ATrans: TSQLHandle; AParams: string): boolean;
var
  VRoute: string;
  VResponse: TJSONData;
  VTrans: TRSQLTrans absolute ATrans;
begin
  VRoute := Format('transaction?action=start&database=%s', [DatabaseName]);
  VResponse := HttpRequest(VRoute);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        VTrans.Identifier := VResponse.Path('content', EmptyStr);
        Result := True;
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', EmptyStr));
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end;
end;

function TRSQLClient.Commit(ATrans: TSQLHandle): boolean;
var
  VRoute: string;
  VResponse: TJSONData;
  VTrans: TRSQLTrans absolute ATrans;
begin
  VRoute := Format('transaction?action=commit&identifier=%s&database=%s',
    [VTrans.Identifier, DatabaseName]);
  VResponse := HttpRequest(VRoute);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := True;
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', EmptyStr));
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end;
end;

function TRSQLClient.Rollback(ATrans: TSQLHandle): boolean;
var
  VRoute: string;
  VResponse: TJSONData;
  VTrans: TRSQLTrans absolute ATrans;
begin
  VRoute := Format('transaction?action=rollback&identifier=%s&database=%s',
    [VTrans.Identifier, DatabaseName]);
  VResponse := HttpRequest(VRoute);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := True;
      end
      else
      begin
        raise ERSQLError.Create(VResponse.Path('content', EmptyStr));
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end;
end;

procedure TRSQLClient.CommitRetaining(ATrans: TSQLHandle);
begin
  Commit(ATrans);
  StartDBTransaction(ATrans, Params.CommaText);
end;

procedure TRSQLClient.RollbackRetaining(ATrans: TSQLHandle);
begin
  Rollback(ATrans);
  StartDBTransaction(ATrans, Params.CommaText);
end;

procedure TRSQLClient.Execute(ACursor: TSQLCursor; ATransaction: TSQLTransaction;
  AParams: TParams);
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  VCursor.Execute(ATransaction, AParams);
end;

function TRSQLClient.RowsAffected(ACursor: TSQLCursor): TRowsCount;
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  Result := VCursor.RowsAffected;
end;

procedure TRSQLClient.AddFieldDefs(ACursor: TSQLCursor; AFieldDefs: TFieldDefs);
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
  VFixed: boolean;
begin
  VMetadata := VCursor.Metadata;
  if (Assigned(VMetadata)) then
  begin
    for VIndex := 0 to (VMetadata.Count - 1) do
    begin
      VData := VMetadata[VIndex];
      if (Assigned(VData)) then
      begin
        VName := VData.Path('name', EmptyStr);
        VLength := VData.Path('length', 0);
        VPrecision := VData.Path('precision', 0);
        VRequired := VData.Path('required', False);
        VReadonly := VData.Path('readonly', False);
        VHidden := VData.Path('hidden', False);   
        VFixed := VData.Path('fixed', False);
        case (LowerCase(VData.Path('type', EmptyStr))) of
          'string': VType := ftString;
          'smallint': VType := ftSmallint;
          'integer': VType := ftInteger;
          'word': VType := ftWord;
          'boolean': VType := ftBoolean;
          'float': VType := ftFloat;
          'currency': VType := ftCurrency;
          'bcd': VType := ftBCD;
          'date': VType := ftDate;
          'time': VType := ftTime;
          'daterime': VType := ftDateTime;
          'bytes': VType := ftBytes;
          'varbytes': VType := ftVarBytes;
          'autoinc': VType := ftAutoInc;
          'blob': VType := ftBlob;
          'memo': VType := ftMemo;
          'graphic': VType := ftGraphic;
          'fmtmemo': VType := ftFmtMemo;
          'paradoxole': VType := ftParadoxOle;
          'dbaseole': VType := ftDBaseOle;
          'typedbinary': VType := ftTypedBinary;
          'cursor': VType := ftCursor;
          'fixedchar': VType := ftFixedChar;
          'widestring': VType := ftWideString;
          'largeint': VType := ftLargeint;
          'adt': VType := ftADT;
          'array': VType := ftArray;
          'reference': VType := ftReference;
          'dataSet': VType := ftDataSet;
          'orablob': VType := ftOraBlob;
          'oraclob': VType := ftOraClob;
          'variant': VType := ftVariant;
          'interface': VType := ftInterface;
          'idispatch': VType := ftIDispatch;
          'guid': VType := ftGuid;
          'timestamp': VType := ftTimeStamp;
          'fmtbcd': VType := ftFMTBcd;
          'fixedwidechar': VType := ftFixedWideChar;
          'widememo': VType := ftWideMemo;
          else VType := ftUnknown;
        end;

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
          if (VFixed) then
          begin
            Attributes := Attributes + [faFixed];
          end;
        end;
      end;
    end;
  end;
end;

function TRSQLClient.Fetch(ACursor: TSQLCursor): boolean;
var
  VCursor: TRSQLCursor absolute ACursor;
begin
  Result := VCursor.Fetch;
end;

function TRSQLClient.LoadField(ACursor: TSQLCursor; AFieldDef: TFieldDef;
  ABuffer: pointer; out ACreateBlob: boolean): boolean;
var
  VCursor: TRSQLCursor absolute ACursor;
  VIndexCol: Integer;
  VIndexRow: Int64;
  VRows: TJSONArray;
  VRow: TJSONArray;
  VData: TJSONData;
begin
  Result := False;
  ACreateBlob := False;
  VIndexCol := AFieldDef.FieldNo - 1;
  VIndexRow := VCursor.Index - 1;
  VRows := VCursor.Rows;
  if (VIndexCol > -1) and (VIndexRow > -1) and (Assigned(VRows)) and
    ({%H-}VRows.Count > 0) then
  begin
    VRow := VRows.Arrays[VIndexRow];
    VData := VRow[VIndexCol];
    if (not (VData.IsNull)) then
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
        ftBCD,
        ftFmtBCD:
        begin
          pBCD(ABuffer)^ := DoubleToBCD(VData.AsFloat);
        end;
        ftCurrency:
        begin
          PCurrency(ABuffer)^ := FloattoCurr(VData.AsFloat);
        end;
        ftFloat:
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

procedure TRSQLClient.LoadBlobIntoBuffer(AFieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; ACursor: TSQLCursor; ATransaction: TSQLTransaction);
var
  VCursor: TRSQLCursor absolute ACursor;
  VIndexCol: Integer;
  VIndexRow: int64;
  VRows: TJSONArray;
  VRow: TJSONArray;
  VData: TJSONData;
  VString: string;
  VLength: int64;
begin
  VIndexCol := AFieldDef.FieldNo - 1;
  VIndexRow := VCursor.Index - 1;
  VRows := VCursor.Rows;
  VString := '';
  VLength := 0;
  if (VIndexCol > -1) and (VIndexRow > -1) and (Assigned(VRows)) and
    ({%H-}VRows.Count > 0) then
  begin
    VRow := VRows.Arrays[VIndexRow];
    VData := VRow[VIndexCol];
    if (not (VData.IsNull)) then
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

procedure TRSQLClient.UpdateIndexDefs(AIndexDefs: TIndexDefs; ATableName: string);

  function IndexSource: string;
  begin
    with TJSONObject.Create() do
    begin
      try
        Add('table', ATableName);
        Result := Stringify();
      finally
        Free;
      end;
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
  VRoute: string;
  VResponse: TJSONData;
  VContent: TJSONData;
  VContentItem: TJSONData;
  VIndex: integer;
begin
  VRoute := Format('database?action=index&database=%s', [DatabaseName]);
  VResponse := HttpRequest(VRoute, IndexSource);
  if (Assigned(VResponse)) then
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
                Name := VContentItem.Path('name', EmptyStr);
                Expression := VContentItem.Path('expression', EmptyStr);
                Fields := VContentItem.Path('fields', EmptyStr);
                CaseInsFields := VContentItem.Path('caseInsFields', EmptyStr);
                DescFields := VContentItem.Path('descFields', EmptyStr);
                Options := JSONToIndexOptions(VContentItem.Path('options'));
                Source := VContentItem.Path('source', EmptyStr);
              end;
            end;
          end;
        end;
      end
      else
      begin
        raise ERSQLError.CreateFmt(
          'index update is not available for this type of database. %s',
          [VResponse.Path('content', EmptyStr)]);
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

function TRSQLClient.GetSchemaInfoSQL(ASchemaType: TSchemaType;
  ASchemaObjectName, ASchemaPattern: string): string;

  function SchemaSource: string;
  begin
    with TJSONObject.Create() do
    begin
      try
        Add('type', Ord(ASchemaType));
        Add('object', ASchemaObjectName);
        Add('pattern', ASchemaPattern);
        Result := Stringify();
      finally
        Free;
      end;
    end;
  end;

var
  VRoute: string;
  VResponse: TJSONData;
begin
  VRoute := Format('database?action=schema&database=%s', [DatabaseName]);
  VResponse := HttpRequest(VRoute, SchemaSource);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := VResponse.Path('content', EmptyStr);
      end
      else
      begin
        raise ERSQLError.CreateFmt(
          'schema is not available for this type of database. %s',
          [VResponse.Path('content', EmptyStr)]);
      end;
    finally
      FreeAndNil(VResponse);
    end;
  end
  else
  begin
    Result := inherited GetSchemaInfoSQL(ASchemaType, ASchemaObjectName,
      ASchemaPattern);
  end;
end;

function TRSQLClient.GetNextValueSQL(const ASequenceName: string;
  AIncrementBy: integer): string;

  function SequenceSource: string;
  begin
    with TJSONObject.Create() do
    begin
      try
        Add('sequence', ASequenceName);
        Add('increment', AIncrementBy);
        Result := Stringify();
      finally
        Free;
      end;
    end;
  end;

var
  VRoute: string;
  VResponse: TJSONData;
begin
  VRoute := Format('database?action=sequence&database=%s', [DatabaseName]);
  VResponse := HttpRequest(VRoute, SequenceSource);
  if (Assigned(VResponse)) then
  begin
    try
      if (VResponse.Path('success', False)) then
      begin
        Result := VResponse.Path('content', EmptyStr);
      end
      else
      begin
        raise ERSQLError.CreateFmt(
          'sequence is not available for this type of database. %s',
          [VResponse.Path('content', EmptyStr)]);
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

constructor TRSQLClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 8091;
  FCompressed := False;
  FTOKEN := '';
  FConnOptions := FConnOptions + [sqSupportParams, sqEscapeRepeat,
    sqSupportReturning];
end;

function TRSQLClient.GetConnectionInfo(AInfoType: TConnInfoType): string;
begin
  case AInfoType of
    citServerType:
    begin
      if (FUseSSL) then
      begin
        Result := 'https';
      end
      else
      begin
        Result := 'http';
      end;
    end;
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

function TRSQLClient.HttpRequest(const ARoute: string;
  const ASource: string): TJSONData;

  function UrlBase(const AConn: TRSQLClient): string;
  begin
    if (AConn.Port < 1) then
    begin
      AConn.Port := 8091;
    end;
    if (AConn.HostName = EmptyStr) then
    begin
      AConn.HostName := 'localhost';
    end;
    if (AConn.UseSSL) then
    begin
      Result := Format('https://%s:%d/', [AConn.HostName, AConn.Port]);
    end
    else
    begin
      Result := Format('http://%s:%d/', [AConn.HostName, AConn.Port]);
    end;
  end;

  function HttpPost(const AConn: TRSQLClient; const ARoute: string;
  const ASource: string): string;
  var
    VHttp: TFPHTTPClient;
    VEncoding: string;
  begin
    VHttp := TFPHTTPClient.Create(nil);
    try
      try
        VHttp.AddHeader('Authorization', Format('Bearer %s', [AConn.TOKEN]));
        VHttp.AddHeader('Content-Type', 'application/json');
        if (AConn.Compressed) then
        begin
          /// Encode request
          VHttp.AddHeader('Content-Encoding', 'deflate');
          VHttp.RequestBody := TStringStream.Create(ZCompressString(ASource));
        end
        else
        begin
          VHttp.RequestBody := TStringStream.Create(ASource);
        end;
        /// Send ==>
        Result := VHttp.Post(UrlBase(AConn) + ARoute);
        /// Decode response
        VEncoding := Trim(VHttp.ResponseHeaders.Values['Content-Encoding']);
        if (LowerCase(VEncoding) = 'deflate') then
        begin
          Result := ZDecompressString(Result);
        end;
      except
        on E: Exception do
        begin
          // Close connection
          AConn.Close(True);
          raise;
        end;
      end;
    finally
      VHttp.RequestBody.Free;
      VHttp.RequestBody := nil;
      VHttp.Free;
    end;
  end;

begin
  Result := TJSONData.Parse(HttpPost(Self, ARoute, ASource));
end;

{ TRSQLClientDef }

class function TRSQLClientDef.TypeName: string;
begin
  Result := 'RSQL';
end;

class function TRSQLClientDef.ConnectionClass: TSQLConnectionClass;
begin
  Result := TRSQLClient;
end;

class function TRSQLClientDef.Description: string;
begin
  Result := 'Connect to any database via an HTTP';
end;

initialization
  RegisterConnection(TRSQLClientDef);

finalization
  UnRegisterConnection(TRSQLClientDef);

end.
