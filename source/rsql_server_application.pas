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
unit RSQL_Server_Application;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  httpdefs,
  httproute,
  fphttp,
  custweb,
  custhttpapp,
  RSQL_Helper,
  RSQL_Server_Database,
  {%H-}RSQL_Server_Router;

type

  { TRSQLServerHandler }

  TRSQLServerHandler = class(TFPHTTPServerHandler)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TRSQLApplication }

  TRSQLApplication = class(TCustomHTTPApplication)
  private
    FCORS: boolean;
    FCredential: string;
    FDatabaseList: TDatabaseList;
    FOnAfterRequest: TResponseEvent;
    FOnBeforeRequest: TRequestEvent;
    procedure SetDatabaseList(AValue: TDatabaseList);
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CORS: boolean read FCORS write FCORS default True;
    property Credential: string read FCredential write FCredential;
    property DatabaseList: TDatabaseList read FDatabaseList write SetDatabaseList;
    property Port default 8091;
    property Threaded default True;
    property OnAfterRequest: TResponseEvent read FOnAfterRequest write FOnAfterRequest;
    property OnBeforeRequest: TRequestEvent read FOnBeforeRequest write FOnBeforeRequest;
  end;

implementation

{ TRSQLServerHandler }

procedure TRSQLServerHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  VServer: TRSQLApplication;
begin
  try
    if (Assigned(Owner)) and (Owner.InheritsFrom(TRSQLApplication)) then
    begin
      VServer := TRSQLApplication(Owner);
    end
    else
    begin
      raise EFPWebError.Create('unattributed or incompatible SERVER');
    end;
    /// Before request
    if (Assigned(VServer.OnBeforeRequest)) then
    begin
      VServer.OnBeforeRequest(Self, ARequest);
    end;
    /// Without legacy
    HTTPRouter.RouteRequest(VServer, ARequest, AResponse);
    /// After request
    if (Assigned(VServer.OnAfterRequest)) then
    begin
      VServer.OnAfterRequest(Self, AResponse);
    end;
  except
    on E: Exception do
    begin
      ShowRequestException(AResponse, E);
    end;
  end;
end;

{ TRSQLApplication }

procedure TRSQLApplication.SetDatabaseList(AValue: TDatabaseList);
begin
  FDatabaseList.Assign(AValue);
end;

function TRSQLApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TRSQLServerHandler.Create(Self);
end;

constructor TRSQLApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 8091;
  Threaded := True;      
  FDatabaseList := TDatabaseList.Create;
  FCORS := True;
  FCredential := '';
  /// Routes
  InitializeRoutes;
end;

destructor TRSQLApplication.Destroy;
begin
  FreeAndNil(FDatabaseList);
  inherited Destroy;
end;

end.
