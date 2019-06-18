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
unit RSQL_Server_Transaction;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  fptimer,
  DB,
  sqldb;

type

  { TTransactionItem }

  TTransactionItem = class(TObject)
  protected
    FLOCK: TRTLCriticalSection;
    FDatabase: TDatabase;
    FTransaction: TSQLTransaction;
    FDurability: integer;
    FIdentifier: string;
    FStarted: TDateTime;
  protected
    procedure LOCK;
    procedure UNLOCK;
    procedure CheckDatabase;
  public
    constructor Create(const ADatabase: TDatabase; const AIdentifier: string = ''; const ADurability: integer = -1); reintroduce;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;
    function InTransaction: boolean; virtual;
    function Expired: boolean; virtual;
    function Identifier: string; virtual;
    function Transaction: TSQLTransaction; virtual;
  end;

  { TTransactionList }

  TTransactionList = class
  private
    FList: TFPList;
    FTimer: TFPTimer;
  protected
    function IndexOf(const AIdentifier: string): integer;
    procedure Manager({%H-}ASender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add(const ATransactionItem: TTransactionItem): TTransactionItem; overload;
    function Add(const ADatabase: TDatabase; const AIdentifier: string = ''; const ADurability: integer = -1): TTransactionItem; overload;
    procedure Clear;
    function Count: integer;
    function Exists(const AIdentifier: string): boolean; overload;
    function Exists(const AIdentifier: string; out AIndex: integer): boolean; overload;
    function Find(const AIndex: integer): TTransactionItem;
  public
    property Items[const AIndex: integer]: TTransactionItem read Find; default;
  end;

/// Singleton
function TransactionList: TTransactionList;

implementation

uses
  dateutils;

function CreateUUid: string;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := '';
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [longint(G.D1), G.D2, G.D3, G.D4[0], G.D4[1], G.D4[2], G.D4[3], G.D4[4], G.D4[5], G.D4[6], G.D4[7]]);
end;

{ TTransactionItem }

procedure TTransactionItem.LOCK;
begin
  EnterCriticalSection(FLOCK);
end;

procedure TTransactionItem.UNLOCK;
begin
  LeaveCriticalSection(FLOCK);
end;

procedure TTransactionItem.CheckDatabase;
begin
  if (not (Assigned(FDatabase))) or (not (FDatabase.Connected)) then
  begin
    raise Exception.Create('database connection is not active');
  end;
end;

constructor TTransactionItem.Create(const ADatabase: TDatabase; const AIdentifier: string; const ADurability: integer);
begin
  inherited Create;
  FStarted := 0;
  FDatabase := ADatabase;
  FTransaction := TSQLTransaction.Create(nil); /// SQLdb
  FTransaction.DataBase := FDatabase;
  FIdentifier := AIdentifier;
  if (FIdentifier = '') then
  begin
    FIdentifier := CreateUUid;
  end;
  FDurability := ADurability;
  if (FDurability <= 0) then
  begin
    FDurability := (1000 * 60 * 60); /// 1h
  end;
  InitCriticalSection(FLOCK);
end;

destructor TTransactionItem.Destroy;
begin
  LOCK;
  try
    FreeAndNil(FTransaction);
    inherited Destroy;
  finally
    UNLOCK;
    DoneCriticalSection(FLOCK);
  end;
end;

procedure TTransactionItem.Start;
begin
  try
    CheckDatabase;
    LOCK;
    try
      if (not (FTransaction.Active)) then
      begin
        FTransaction.StartTransaction;
        FStarted := Now; /// Cycle
      end;
    finally
      UNLOCK;
    end;
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt('it wasn''t possible to start the transaction. [%s]', [E.Message]);
    end;
  end;
end;

procedure TTransactionItem.Commit;
begin
  try
    CheckDatabase;
    LOCK;
    try
      if (FTransaction.Active) then
      begin
        FTransaction.Commit;
      end
      else
      begin
        raise Exception.Create('there is no transaction to commit');
      end;
    finally
      UNLOCK;
    end;
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt('it wasn''t possible to commit the transaction. [%s]', [E.Message]);
    end;
  end;
end;

procedure TTransactionItem.Rollback;
begin
  try
    CheckDatabase;
    Lock;
    try
      if (FTransaction.Active) then
      begin
        FTransaction.Rollback;
      end
      else
      begin
        raise Exception.Create('there are no transaction to revert');
      end;
    finally
      UnLock;
    end;
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt('it wasn''t possible to rollback the transaction. [%s]', [E.Message]);
    end;
  end;
end;

function TTransactionItem.InTransaction: boolean;
begin
  try
    CheckDatabase;
    Result := (FTransaction.Active);
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt('it wasn''t possible check transaction. [%s]', [E.Message]);
    end;
  end;
end;

function TTransactionItem.Expired: boolean;
begin
  Result := (FStarted > 0) and (IncMilliSecond(FStarted, FDurability) <= Now);
end;

function TTransactionItem.Identifier: string;
begin
  Result := FIdentifier;
end;

function TTransactionItem.Transaction: TSQLTransaction;
begin
  Result := FTransaction;
end;

{ TTransactionList }

function TTransactionList.IndexOf(const AIdentifier: string): integer;
var
  VIndex: integer;
  VTransactionItem: TTransactionItem;
begin
  Result := -1;
  for VIndex := 0 to (Count - 1) do
  begin
    VTransactionItem := Items[VIndex];
    if (Assigned(VTransactionItem)) and (VTransactionItem.Identifier = AIdentifier) then
    begin
      Result := VIndex;
      Break;
    end;
  end;
end;

procedure TTransactionList.Manager(ASender: TObject);
var
  VIndex: integer;
  VTransactionItem: TTransactionItem;
begin
  for VIndex := (Count - 1) downto 0 do
  begin
    VTransactionItem := Items[VIndex];
    if (Assigned(VTransactionItem)) and (VTransactionItem.Expired) then
    begin
      FList.Delete(VIndex);
      FreeAndNil(VTransactionItem);
    end;
  end;
end;

constructor TTransactionList.Create;
begin
  inherited Create;
  FList := TFPList.Create;
  FTimer := TFPTimer.Create(nil);
  FTimer.OnTimer := @Manager;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;
end;

destructor TTransactionList.Destroy;
begin
  Clear;
  FreeAndNil(FTimer);
  FreeAndNil(FList);
  inherited Destroy;
end;

function TTransactionList.Add(const ATransactionItem: TTransactionItem): TTransactionItem;
begin
  Result := ATransactionItem;
  FList.Add(ATransactionItem);
end;

function TTransactionList.Add(const ADatabase: TDatabase; const AIdentifier: string; const ADurability: integer): TTransactionItem;
begin
  Result := Add(TTransactionItem.Create(ADatabase, AIdentifier, ADurability));
end;

procedure TTransactionList.Clear;
var
  VIndex: integer;
  VTransactionItem: TTransactionItem;
begin
  for VIndex := (Count - 1) downto 0 do
  begin
    VTransactionItem := Items[VIndex];
    FList.Delete(VIndex);
    FreeAndNil(VTransactionItem);
  end;
end;

function TTransactionList.Count: integer;
begin
  Result := FList.Count;
end;

function TTransactionList.Exists(const AIdentifier: string): boolean;
begin
  Result := (IndexOf(AIdentifier) > -1);
end;

function TTransactionList.Exists(const AIdentifier: string; out AIndex: integer): boolean;
begin
  AIndex := IndexOf(AIdentifier);
  Result := (AIndex > -1);
end;

function TTransactionList.Find(const AIndex: integer): TTransactionItem;
begin
  Result := TTransactionItem(FList[AIndex]);
end;

/// Singleton
var
  VTransactionList: TTransactionList = nil;

function TransactionList: TTransactionList;
begin
  if (not (Assigned(VTransactionList))) then
  begin
    VTransactionList := TTransactionList.Create;
  end;
  Result := VTransactionList;
end;

initialization

finalization
  FreeAndNil(VTransactionList);

end.
