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
unit RSQL_Register;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  Controls,
  Dialogs,
  Forms,
  LazIDEIntf,
  ProjectIntf;

type

  { TRSQLApplicationDescriptor }

  TRSQLApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;

var
  ProjectDescriptorRSQLApplication: TRSQLApplicationDescriptor;

procedure Register;

implementation

uses
  LResources,
  PropEdits,
  sqldb,
  RSQL_Server_Component,
  RSQL_Client_Connection;

procedure Register;
begin
  {$I rsql.lrs}
  RegisterComponents('SQLdb', [TRSQLServer, TRSQLClient]);

  /// Hide properties TRSQLServer
  RegisterPropertyEditor(TypeInfo(string), TRSQLServer, 'AdminMail', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TRSQLServer, 'AdminName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TRSQLServer, 'ServerBanner', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(boolean), TRSQLServer, 'LookupHostNames', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(boolean), TRSQLServer, 'Threaded', THiddenPropertyEditor);

  /// Hide properties TRSQLClient
  RegisterPropertyEditor(TypeInfo(string), TRSQLClient, 'CharSet', THiddenPropertyEditor);
  //RegisterPropertyEditor(TypeInfo(string), TRSQLClient, 'DatabaseName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(boolean), TRSQLClient, 'KeepConnection', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(boolean), TRSQLClient, 'LoginPrompt', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TRSQLClient, 'Role', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TRSQLClient, 'Params', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDBEventTypes), TRSQLClient, 'LogEvents', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSQLConnectionOptions), TRSQLClient, 'Options', THiddenPropertyEditor);

  /// RSQL Application
  ProjectDescriptorRSQLApplication := TRSQLApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorRSQLApplication);
end;

{ TRSQLApplicationDescriptor }

constructor TRSQLApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'RSQLApplication';
end;

function TRSQLApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'RSQL Application';
end;

function TRSQLApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'RSQL Application';
end;

function TRSQLApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;

  function ProgramSource: string;
  begin
    Result := '';
    Result := Result + 'program rsqlhttpproject1;' + LineEnding;
    Result := Result + LineEnding;
    Result := Result + '{$mode objfpc}{$H+}' + LineEnding;
    Result := Result + LineEnding;
    Result := Result + 'uses' + LineEnding;
    Result := Result + '  {$IFDEF UNIX}' + LineEnding;
    Result := Result + '  cthreads,' + LineEnding;
    Result := Result + '  {$ENDIF}' + LineEnding;
    Result := Result + '  SysUtils,' + LineEnding;
    Result := Result + '  Classes,' + LineEnding;
    Result := Result + '  SQLdb,' + LineEnding;
    Result := Result + '  //IBConnection,' + LineEnding;
    Result := Result + '  //mysql40conn,' + LineEnding;
    Result := Result + '  //mysql41conn,' + LineEnding;
    Result := Result + '  //mysql50conn,' + LineEnding;
    Result := Result + '  //mysql51conn,' + LineEnding;
    Result := Result + '  //mysql55conn,' + LineEnding;
    Result := Result + '  //mysql56conn,' + LineEnding;
    Result := Result + '  //mysql57conn,' + LineEnding;
    Result := Result + '  //odbcconn,' + LineEnding;
    Result := Result + '  //oracleconnection,' + LineEnding;
    Result := Result + '  //pqconnection,' + LineEnding;  
    Result := Result + '  //sqlite3conn,' + LineEnding;  
    Result := Result + '  HTTPDefs,' + LineEnding;
    Result := Result + '  RSQL_Server_Application;' + LineEnding;   
    Result := Result + LineEnding;   
    Result := Result + 'type' + LineEnding;
    Result := Result + '  TRSQLApp = class(TRSQLApplication)' + LineEnding;
    Result := Result + '  protected' + LineEnding;
    Result := Result + '    FDatabase: TSQLConnector;' + LineEnding;
    Result := Result + '    procedure BeforeRequest(ASender: TObject; ARequest : TRequest);' + LineEnding;
    Result := Result + '    procedure AfterRequest(ASender: TObject; AResponse : TResponse);' + LineEnding;
    Result := Result + '  public' + LineEnding;
    Result := Result + '    constructor Create(AOwner: TComponent); overload; override;' + LineEnding;
    Result := Result + '  end;' + LineEnding;
    Result := Result + LineEnding;
    Result := Result + '  procedure TRSQLApp.BeforeRequest(ASender: TObject; ARequest : TRequest);' + LineEnding;
    Result := Result + '  begin' + LineEnding;
    Result := Result + '    WriteLn(''<<== REQUEST'');' + LineEnding; 
    Result := Result + '    WriteLn(ARequest.Content);' + LineEnding;
    Result := Result + '  end;' + LineEnding;
    Result := Result + LineEnding;
    Result := Result + '  procedure TRSQLApp.AfterRequest(ASender: TObject; AResponse : TResponse);' + LineEnding;
    Result := Result + '  begin' + LineEnding;  
    Result := Result + '    WriteLn(''RESPONSE ==>>'');' + LineEnding;
    Result := Result + '    WriteLn(AResponse.Content);' + LineEnding;
    Result := Result + '  end;' + LineEnding;    
    Result := Result + LineEnding;
    Result := Result + '  constructor TRSQLApp.Create(AOwner: TComponent);' + LineEnding;
    Result := Result + '  begin' + LineEnding;                  
    Result := Result + '    inherited Create(AOwner);' + LineEnding;     
    Result := Result + '    OnAfterRequest := @AfterRequest;' + LineEnding;
    Result := Result + '    OnBeforeRequest := @BeforeRequest;' + LineEnding;
    Result := Result + '    FDatabase :=  TSQLConnector.Create(Self);' + LineEnding;
    Result := Result + '    FDatabase.ConnectorType := ''?'';' + LineEnding;
    Result := Result + '    FDatabase.HostName := ''localhost'';' + LineEnding;
    Result := Result + '    FDatabase.DatabaseName :=''?'';' + LineEnding;
    Result := Result + '    FDatabase.UserName := ''?'';' + LineEnding;
    Result := Result + '    FDatabase.Password := ''?'';' + LineEnding;
    Result := Result + '    FDatabase.Open;' + LineEnding;
    Result := Result + '    with DatabaseList.Add do' + LineEnding;
    Result := Result + '    begin' + LineEnding;          
    Result := Result + '      Database := FDatabase;' + LineEnding;
    Result := Result + '      Name := ''database'';' + LineEnding;
    Result := Result + '    end;' + LineEnding;
    Result := Result + '  end;' + LineEnding; 
    Result := Result + LineEnding;            
    Result := Result + LineEnding;
    Result := Result + 'begin' + LineEnding; 
    Result := Result + '  with TRSQLApp.Create(nil) do' + LineEnding;
    Result := Result + '    try' + LineEnding;
    Result := Result + '      Initialize;' + LineEnding;
    Result := Result + '      Run;' + LineEnding;
    Result := Result + '    finally' + LineEnding;
    Result := Result + '      Free;' + LineEnding;
    Result := Result + '    end;' + LineEnding;
    Result := Result + 'end.' + LineEnding;
  end;

var
  VMainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);
  /// Main
  VMainFile := AProject.CreateProjectFile('rsqlproject1.lpr');
  VMainFile.IsPartOfProject := True;
  /// Project
  AProject.AddFile(VMainFile, False);
  AProject.MainFileID := 0;
  AProject.MainFile.SetSourceText(ProgramSource);
  AProject.AddPackageDependency('RSQL_Package');
  //AProject.LazCompilerOptions.Win32GraphicApp := False;
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  Result := mrOk;
end;

end.
