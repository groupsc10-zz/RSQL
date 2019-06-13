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
unit RSQL_Server_Router;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  httpdefs,
  httproute,
  DB,
  sqldb,
  RSQL_Helper,
  RSQL_Crypto_JWT,
  RSQL_Server_Database;

type

  { TRoute }

  TRoute = class(TCustomRoute)
  protected
    FRequest: TRequest;
    FResponse: TResponse;
    FAuthorization: string;
    FCORS: boolean;
    FCredential: string;
    FMethod: string;
    FDatabase: TDatabase;
    FDatabaseList: TDatabaseList;
    FContent: TJSONObject;
  protected
    function AllowedMethods: string; virtual;
    procedure CheckMethod; virtual;
    procedure CheckContent; virtual;
    procedure CheckAutorization; virtual;
    procedure CheckDatabase; virtual;
  public
    destructor Destroy; override;
    procedure HandleRequest(AServer: TObject; ARequest: TRequest; AResponse: TResponse); override;
    /// Methods
    procedure POST; virtual; abstract;
    { TODO : Add other methods }
  end;

  { TRouteAuthentication }

  TRouteAuthentication = class(TRoute)
  protected
    procedure CheckAutorization; override;
    procedure CheckLogin; virtual;
  public
    procedure Post; override;
  end;

  { TRouteDatabase }

  TRouteDatabase = class(TRoute)
  protected
    FAction: string;
  protected
    procedure Index; virtual;
    procedure Schema; virtual;
    procedure Sequence; virtual;
  public
    procedure Post; override;
  end;

  { TRouteTransaction }

  TRouteTransaction = class(TRoute)
  protected
    FAction: string;
    FIdentifier: string;
  protected
    procedure Start; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;
  public
    procedure Post; override;
  end;

  { TRouteStatement }

  TRouteStatement = class(TRoute)
  protected
    FAction: string;
    FIdentifier: string;
  protected
    procedure Batch;
    procedure Query;
  public
    procedure Post; override;
  end;

procedure InitializeRoutes;
{ TODO : Finish Routes }

implementation

uses
  strutils,
  RSQL_Server_Component,
  RSQL_Server_Application,
  RSQL_Server_Transaction;

procedure InitializeRoutes;
begin
  with HTTPRouter do
  begin
    RegisterRoute('authentication', TRouteAuthentication);
    RegisterRoute('database', TRouteDatabase);
    RegisterRoute('transaction', TRouteTransaction);
    RegisterRoute('statement', TRouteStatement);
  end;
end;

{ TRoute }

function TRoute.AllowedMethods: string;
begin
  Result := 'POST';
end;

procedure TRoute.CheckMethod;
begin
  if (Pos(FMethod, AllowedMethods) < 1) then
  begin
    raise EHTTP.CreateFmt(405, 'the requested method %s is not allowed', [FMethod]);
  end;
end;

procedure TRoute.CheckContent;
begin
  if (FRequest.ContentType <> 'application/json') then
  begin
    raise EHTTP.Create(415, 'media type not supported on request');
  end;
  if (not (TJSONData.Parse(FRequest.Content, FContent))) then
  begin
    raise EHTTP.Create(400, 'request has invalid content type');
  end;
end;

procedure TRoute.CheckAutorization;
var
  VOutput: string;
begin
  if (FCredential <> '') then
  begin
    VOutput := '';
    FAuthorization := FRequest.Authorization;
    FAuthorization := StringsReplace(FAuthorization, ['Bearer '], [''], [rfIgnoreCase]);
    if (not (JWTParse(FCredential, FAuthorization, VOutput))) then
    begin
      raise EHTTP.CreateFmt(401, 'request unauthorized. %s', [VOutput]);
    end;
  end;
end;

procedure TRoute.CheckDatabase;
var
  VDatabaseItem: TDatabaseItem;
  VDatabaseName: string;
begin
  FDatabase := nil;
  if (Assigned(FDatabaseList)) then
  begin          
    VDatabaseName := FRequest.QueryFields.Values['database'];
    if (VDatabaseName <> '') then
    begin                     
      VDatabaseItem := FDatabaseList.Find(VDatabaseName);
    end
    else
    begin          
      VDatabaseItem := FDatabaseList.FindDefault;
    end;
    if (Assigned(VDatabaseItem)) then
    begin
      FDatabase := VDatabaseItem.Database;
    end;
  end;
  if (not (Assigned(FDatabase))) then
  begin
    raise EHTTP.Create(400, 'database not assigned!');
  end;
  if (not (FDatabase.Connected)) then
  begin
    raise EHTTP.Create(400, 'operation cannot be performed on an disconnected database');
  end;
  if (not (FDatabase.InheritsFrom(TSQLConnection))) then
  begin
    raise EHTTP.Create(400, 'operation is not supported by this type of database');
  end;
end;

destructor TRoute.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TRoute.HandleRequest(AServer: TObject; ARequest: TRequest; AResponse: TResponse);
var
  VServerComp: TRSQLHTTPServer;
  VServerApp: TRSQLApplication;
begin
  try
    /// Server
    if (Assigned(AServer)) and (AServer.InheritsFrom(TRSQLHTTPServer)) then
    begin
      VServerComp := TRSQLHTTPServer(AServer);
      FCORS := VServerComp.CORS;
      FCredential := VServerComp.Credential;
      FDatabaseList := VServerComp.DatabaseList;
    end
    else
    if (Assigned(AServer)) and (AServer.InheritsFrom(TRSQLApplication)) then
    begin
      VServerApp := TRSQLApplication(AServer);
      FCORS := VServerApp.CORS;
      FCredential := VServerApp.Credential;
      FDatabaseList := VServerApp.DatabaseList;
    end
    else
    begin
      FCORS := True;
      FCredential := '';
      FDatabaseList := nil;
    end;
    /// Request
    FRequest := ARequest;
    /// Response
    FResponse := AResponse;
    FResponse.ContentType := 'application/json; charset=utf-8';
    /// Cross-origin resource sharing
    if (FCORS) then
    begin
      FResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
      FResponse.SetCustomHeader('Access-Control-Allow-Credentials', 'true');
      FResponse.SetCustomHeader('Access-Control-Allow-Headers', 'X-Custom-Header, Cache-Control');
    end;
    /// Methods
    FMethod := UpperCase(FRequest.Method);
    /// Validations
    CheckDatabase;
    CheckMethod;
    CheckContent;
    CheckAutorization;
    /// Redirection
    case FMethod of
      'POST': POST;
      { TODO : Add other methods }
    end;
  except
    on E: EHTTP do
    begin
      FResponse.Envelop(E.StatusCode, E.Message);
    end;
    on E: Exception do
    begin
      FResponse.Envelop(500, 'internal server error, unexpected error :(');
      if (Assigned(VServerComp)) then
      begin
        { TODO : Add log }
      end;
      if (Assigned(VServerApp)) then
      begin
        VServerApp.Log(etError, E.Message);
      end;
    end;
  end;
end;

{ TRouteAuthentication }

procedure TRouteAuthentication.CheckAutorization;
begin
  /// empty of purpose
end;

procedure TRouteAuthentication.CheckLogin;
begin
  with TSQLConnection(FDatabase) do
  begin
    if (not (SameText(FContent.Path('username', ''), UserName))) or
      (not (SameText(FContent.Path('password', ''), Password))) then
    begin
      raise EHTTP.Create(401, 'username and/or password are invalid');
    end;
  end;
end;

procedure TRouteAuthentication.Post;

  function PAYLOAD: string;
  begin
    with TJSONObject.Create([]) do
      try
        Add('iss', 'rsql');
        Result := Stringify();
      finally
        Free;
      end;
  end;

  function AuthenticationInfo: TJSONObject;
  begin
    Result := TJSONObject.Create();
    Result.Add('token', JWTSign(FCredential, PAYLOAD));
  end;

begin
  CheckLogin;
  FResponse.Envelop(200, AuthenticationInfo);
end;

{ TRouteDatabase }

procedure TRouteDatabase.Index;

  function IndexOptionsToJSON(const AOpts: TIndexOptions): TJSONArray;
  begin
    Result := TJSONArray.Create();
    if (ixPrimary in AOpts) then
    begin
      Result.Add('primary');
    end;
    if (ixUnique in AOpts) then
    begin
      Result.Add('unique');
    end;
    if (ixDescending in AOpts) then
    begin
      Result.Add('descending');
    end;
    if (ixCaseInsensitive in AOpts) then
    begin
      Result.Add('caseinsensitive');
    end;
    if (ixExpression in AOpts) then
    begin
      Result.Add('expression');
    end;
    if (ixNonMaintained in AOpts) then
    begin
      Result.Add('nonmaintained');
    end;
  end;

var
  VTable: string;
  VIndex: integer;
  VIndexDef: TIndexDef;
  VIndexDefs: TIndexDefs;
  VJSONArray: TJSONArray;
  VJSONObject: TJSONObject;
  VConnection: TSQLConnection;
begin
  VTable := FContent.Path('table', '');
  try
    VIndexDefs := TIndexDefs.Create(nil);
    VConnection := TSQLConnection(FDatabase);
    VConnection.FillIndexDefs(VIndexDefs, VTable);
    VJSONArray := TJSONArray.Create();
    for VIndex := 0 to (VIndexDefs.Count - 1) do
    begin
      VIndexDef := VIndexDefs[VIndex];
      if (Assigned(VIndexDef)) then
      begin
        VJSONObject := TJSONObject.Create();
        with VJSONObject do
        begin
          Add('name', VIndexDef.Name);
          Add('expression', VIndexDef.Expression);
          Add('fields', VIndexDef.Fields);
          Add('caseinsfields', VIndexDef.CaseInsFields);
          Add('descfields', VIndexDef.DescFields);
          Add('options', IndexOptionsToJSON(VIndexDef.Options));
          Add('source', VIndexDef.Source);
        end;
        VJSONArray.Add(VJSONObject);
      end;
    end;
    FResponse.Envelop(200, VJSONArray);
  finally
    FreeAndNil(VIndexDefs);
  end;
end;

procedure TRouteDatabase.Schema;
var
  VObjectName: string;
  VPattern: string;
  VSchemaType: integer;  
  VConnection: TSQLConnection;
begin
  VObjectName := FContent.Path('object', '');
  VPattern := FContent.Path('pattern', '');
  VSchemaType := FContent.Path('type', 0);
  VConnection := TSQLConnection(FDatabase);
  FResponse.Envelop(200, VConnection.ExtractSchemaSQL(TSchemaType(VSchemaType), VObjectName, VPattern));
end;

procedure TRouteDatabase.Sequence;
var
  VSequence: string;
  VIncrement: integer;
  VConnection: TSQLConnection;
begin
  VSequence := FContent.Path('sequence', '');
  VIncrement := FContent.Path('increment', 0);
  VConnection := TSQLConnection(FDatabase);
  FResponse.Envelop(200, VConnection.ExtractSequenceSQL(VSequence, VIncrement));
end;

procedure TRouteDatabase.Post;
begin
  try
    FAction := LowerCase(FRequest.QueryFields.Values['action']);
    case FAction of
      'index':
      begin
        Index;
      end;
      'schema':
      begin
        Schema;
      end;
      'sequence':
      begin
        Sequence;
      end;
      else
      begin
        raise Exception.CreateFmt('invalid action[%s]', [FAction]);
      end;
    end;
  except
    on E: Exception do
    begin
      raise EHTTP.Create(400, E.Message);
    end;
  end;
end;

{ TRouteTransaction }

procedure TRouteTransaction.Start;
var
  VIndex: integer;
  VTransactionItem: TTransactionItem;
begin
  /// Find transaction
  if (TransactionList.Exists(FIdentifier, VIndex)) then
  begin
    VTransactionItem := TransactionList[VIndex];
    VTransactionItem.Start;
    /// Response
    FResponse.Envelop(200, FIdentifier);
  end
  else
  begin
    VTransactionItem := TransactionList.Add(FDatabase);
    VTransactionItem.Start;
    /// Response
    FResponse.Envelop(200, VTransactionItem.Identifier);
  end;
end;

procedure TRouteTransaction.Commit;
var
  VIndex: integer;
begin
  /// Find transaction
  if (TransactionList.Exists(FIdentifier, VIndex)) then
  begin
    TransactionList[VIndex].Commit;
    /// Response
    FResponse.Envelop(200, 'transaction(%s) successfully confirmed', [FIdentifier]);
  end
  else
  begin
    raise Exception.CreateFmt('transaction[%s] not found', [FIdentifier]);
  end;
end;

procedure TRouteTransaction.Rollback;
var
  VIndex: integer;
begin
  /// Find transaction
  if (TransactionList.Exists(FIdentifier, VIndex)) then
  begin
    TransactionList[VIndex].Rollback;
    /// Response
    FResponse.Envelop(200, 'transaction(%s) reverted successfully', [FIdentifier]);
  end
  else
  begin
    raise Exception.CreateFmt('transaction[%s] not found', [FIdentifier]);
  end;
end;

procedure TRouteTransaction.Post;
begin
  try
    FAction := LowerCase(FRequest.QueryFields.Values['action']);
    FIdentifier := FRequest.QueryFields.Values['identifier'];
    case FAction of
      'start':
      begin
        Start;
      end;
      'commit':
      begin
        Commit;
      end;
      'rollback':
      begin
        Rollback;
      end;
      else
      begin
        raise Exception.CreateFmt('invalid action[%s]', [FAction]);
      end;
    end;
  except
    on E: Exception do
    begin
      raise EHTTP.Create(400, E.Message);
    end;
  end;
end;

{ TRouteStatement }

procedure TRouteStatement.Batch;
var
  VBatch: TSQLScript;
  VTransactionItem: TTransactionItem;
begin
  /// Automatic transaction
  VTransactionItem := nil;
  VBatch := nil;
  try
    try
      /// Transaction
      VTransactionItem := TTransactionItem.Create(FDatabase);
      VTransactionItem.Start;
      /// Batch
      VBatch := TSQLScript.Create(nil);
      with VBatch do
      begin
        Transaction := VTransactionItem.Transaction;
        UseCommit := True;
        UseSetTerm := True;
        CommentsInSQL := False;
        Script.Text := FContent.Path('sql', '');
        Execute;
      end;
      /// Response
      FResponse.Envelop(200, 'succeeded');
      /// Transaction
      VTransactionItem.Commit;
    except
      /// Transaction
      if (Assigned(VTransactionItem)) and (VTransactionItem.InTransaction) then
      begin
        VTransactionItem.Rollback;
      end;
      raise
    end;
  finally
    FreeAndNil(VBatch);
    FreeAndNil(VTransactionItem);
  end;
end;

procedure TRouteStatement.Query;
var
  VQuery: TSQLQuery;
  VIndex: integer;
  VFast: boolean;
  VMetadata: boolean;
  VTransactionItem: TTransactionItem;
begin
  if (FIdentifier <> '') then
  begin
    /// Config
    VFast := FContent.Path('settings.fast', True);
    VMetadata := FContent.Path('settings.metadata', True);
    /// Find
    if (TransactionList.Exists(FIdentifier, VIndex)) then
    begin
      VTransactionItem := nil;
      VQuery := nil;
      try
        try
          /// Transaction
          VTransactionItem := TransactionList[VIndex];
          VTransactionItem.Start;
          /// Query
          VQuery := TSQLQuery.Create(nil);
          VQuery.Transaction := VTransactionItem.Transaction;
          VQuery.SQL.Text := FContent.Path('sql', '');
          VQuery.Params.AssignJSON(FContent.Path('params'));
          VQuery.Prepare;
          if (VQuery.StatementType = stSelect) then
          begin
            VQuery.Open;
            /// Response
            FResponse.Envelop(200, VQuery.SaveToJSON(VFast, VMetadata));
          end
          else
          begin
            VQuery.ExecSQL;
            /// Response
            FResponse.Envelop(200, TJSONObject.Create(['rowsAffected', VQuery.RowsAffected]));
          end;
        except
          raise
        end;
      finally
        FreeAndNil(VQuery);
      end;
    end
    else
    begin
      raise Exception.CreateFmt('transaction[%s] not found.', [FIdentifier]);
    end;
  end
  else
  begin
    /// Config
    VFast := FContent.Path('settings.fast', True);
    VMetadata := FContent.Path('settings.metadata', True);
    /// Automatic transaction
    VTransactionItem := nil;
    VQuery := nil;
    try
      try
        /// Transaction
        VTransactionItem := TTransactionItem.Create(FDatabase);
        VTransactionItem.Start;
        /// Query
        VQuery := TSQLQuery.Create(nil);
        VQuery.Transaction := VTransactionItem.Transaction;
        VQuery.SQL.Text := FContent.Path('sql', '');
        VQuery.Params.AssignJSON(FContent.Path('params'));
        VQuery.Prepare;
        if (VQuery.StatementType = stSelect) then
        begin
          VQuery.Open;
          /// Response
          FResponse.Envelop(200, VQuery.SaveToJSON(VFast, VMetadata));
          /// Transaction
          VTransactionItem.Commit;
        end
        else
        begin
          VQuery.ExecSQL;
          /// Response
          FResponse.Envelop(200, TJSONObject.Create(['rowsAffected', VQuery.RowsAffected]));
          /// Transaction
          VTransactionItem.Commit;
        end;
      except
        on E: Exception do
        begin
          /// Transaction
          if (Assigned(VTransactionItem)) and (VTransactionItem.InTransaction) then
          begin
            VTransactionItem.Rollback;
          end;
          raise;
        end;
      end;
    finally
      FreeAndNil(VQuery);
      FreeAndNil(VTransactionItem);
    end;
  end;
end;

procedure TRouteStatement.Post;
begin
  try
    FAction := LowerCase(FRequest.QueryFields.Values['action']);
    FIdentifier := FRequest.QueryFields.Values['identifier'];
    case FAction of
      'batch':
      begin
        Batch;
      end;
      'query':
      begin
        Query;
      end;
      else
      begin
        raise Exception.CreateFmt('invalid action[%s]', [FAction]);
      end;
    end;
  except
    on E: Exception do
    begin
      raise EHTTP.Create(400, E.Message);
    end;
  end;
end;

end.
