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
unit RSQL_Server_Component;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  httpDefs,
  httproute,
  fphttpserver,
  RSQL_Helper,
  RSQL_Server_Database,
  {%H-}RSQL_Server_Router;

type

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  strict private
    FOnExecute: TNotifyEvent;
  public
    constructor Create; reintroduce;
    procedure Execute; override;
    property Terminated;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  { THTTPServer }

  THTTPServer = class(TFPCustomHttpServer)
  private
    FCompressed: boolean;
    FCORS: boolean;
    FCredential: string;
    FDatabaseList: TDatabaseList;
    FCanExecute: boolean;
    FThread: THTTPServerThread;
    FOnAfterRequest: TResponseEvent;
    FOnBeforeRequest: TRequestEvent;
    procedure DoOnExecute({%H-}ASender: TObject);
    procedure DoOnTerminate({%H-}ASender: TObject);
    procedure SetDatabaseList(AValue: TDatabaseList);
  protected
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop;
    procedure Start;
  published
    property Compressed: boolean read FCompressed write FCompressed default False;
    property CORS: boolean read FCORS write FCORS default True;
    property Credential: string read FCredential write FCredential;
    property DatabaseList: TDatabaseList read FDatabaseList write SetDatabaseList;
    property Port default 8091;
    property Threaded default True;
    property OnAfterRequest: TResponseEvent read FOnAfterRequest write FOnAfterRequest;
    property OnBeforeRequest: TRequestEvent read FOnBeforeRequest write FOnBeforeRequest;
  end;

  { TRSQLServer }

  TRSQLServer = class(THTTPServer);

implementation

{ THTTPServerThread }

constructor THTTPServerThread.Create;
begin
  FOnExecute := nil;
  inherited Create(True);
end;

procedure THTTPServerThread.Execute;
begin
  if (Assigned(FOnExecute)) then
  begin
    FOnExecute(Self);
  end;
end;

{ THTTPServer }

procedure THTTPServer.DoOnExecute(ASender: TObject);
begin
  if (FCanExecute) then
  begin
    Active := True;
  end;
end;

procedure THTTPServer.DoOnTerminate(ASender: TObject);
begin
  FThread := nil;
end;

procedure THTTPServer.SetDatabaseList(AValue: TDatabaseList);
begin
  FDatabaseList.Assign(AValue);
end;

procedure THTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  // Before request
  if (Assigned(FOnBeforeRequest)) then
  begin
    FOnBeforeRequest(Self, ARequest);
  end;
  HTTPRouter.RouteRequest(Self, ARequest, AResponse);
  // After request
  if (Assigned(FOnAfterRequest)) then
  begin
    FOnAfterRequest(Self, AResponse);
  end;
end;

constructor THTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AcceptIdleTimeout := 1;
  Port := 8091;
  Threaded := True;
  FDatabaseList := TDatabaseList.Create;
  FCanExecute := False;
  FCompressed := False;
  FCORS := True;
  FCredential := EmptyStr;
  // Routes
  InitializeRoutes;
end;

destructor THTTPServer.Destroy;
begin
  FCanExecute := False;
  Stop;
  if Assigned(FThread) then
  begin
    FThread.FreeOnTerminate := False;
    FThread.Free;
  end;
  Threaded := False;
  FreeAndNil(FDatabaseList);
  inherited Destroy;
end;

procedure THTTPServer.Stop;
begin
  if (not (Active)) then
  begin
    Exit;
  end;
  try
    Active := False;
  except
    if (Active) then
    begin
      raise;
    end;
  end;
end;

procedure THTTPServer.Start;
begin
  if (Active) then
  begin
    Exit;
  end;
  FCanExecute := False;
  try
    if (not (Assigned(FThread))) then
    begin
      FThread := THTTPServerThread.Create;
      FThread.OnExecute := @DoOnExecute;
      FThread.FreeOnTerminate := True;
      FThread.OnTerminate := @DoOnTerminate;
    end;
  finally
    FCanExecute := True;
  end;
  // Start
  FThread.Start;
end;

end.
