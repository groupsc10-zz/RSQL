program rsqlproject1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  SQLdb,
  //IBConnection,
  //mysql40conn,
  //mysql41conn,
  //mysql50conn,
  //mysql51conn,
  //mysql55conn,
  //mysql56conn,
  //mysql57conn,
  //odbcconn,
  //oracleconnection,
  //pqconnection,
  sqlite3conn,
  HTTPDefs,
  RSQL_Server_Application;

type
  TRSQLApp = class(TRSQLApplication)
  protected
    FDatabase: TSQLConnector;
    procedure BeforeRequest({%H-}ASender: TObject; ARequest: TRequest);
    procedure AfterRequest({%H-}ASender: TObject; AResponse: TResponse);
  public
    constructor Create(AOwner: TComponent); overload; override;
  end;

  procedure TRSQLApp.BeforeRequest(ASender: TObject; ARequest: TRequest);
  begin
    WriteLn('<<== REQUEST');
    WriteLn(ARequest.Content);
  end;

  procedure TRSQLApp.AfterRequest(ASender: TObject; AResponse: TResponse);
  begin
    WriteLn('RESPONSE ==>>');
    WriteLn(AResponse.Content);
  end;

  constructor TRSQLApp.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    OnAfterRequest := @AfterRequest;
    OnBeforeRequest := @BeforeRequest;
    FDatabase := TSQLConnector.Create(Self);
    FDatabase.ConnectorType := 'SQLite3';
    FDatabase.HostName := 'localhost';
    FDatabase.DatabaseName := '../../db/rsql.sqlite';
    FDatabase.Open;
    with DatabaseList.Add do
    begin
      Database := FDatabase;
      Name := 'sqlitedb';
    end;
  end;

begin
  with TRSQLApp.Create(nil) do
    try
      Compressed := True;
      CORS := True;
      Initialize;
      Run;
    finally
      Free;
    end;
end.
