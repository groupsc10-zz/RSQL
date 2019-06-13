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
unit RSQL_Server_Database;

{$I rsql.inc}

interface

uses
  Classes,
  SysUtils,
  DB;

type
  /// Forward declaration
  TDatabaseList = class;

  { TDatabaseItem }

  TDatabaseItem = class(TCollectionItem)
  private
    FDatabase: TDatabase;
    FDatabaseList: TDatabaseList;
    FIsDefault: boolean;
    FName: string;
    procedure SetDatabase(AValue: TDatabase);
    procedure SetIsDefault(AValue: boolean);
    procedure SetName(AValue: string);
  protected
    function GetDisplayName: string; override;
    procedure SetCollection(AValue: TCollection); override;
  published
    property Database: TDatabase read FDatabase write SetDatabase;
    property IsDefault: boolean read FIsDefault write SetIsDefault;
    property Name: string read FName write SetName;
  end;

  { TDatabaseList }

  TDatabaseList = class(TCollection)
  private
    function GetItems(const AIndex: integer): TDatabaseItem;
  protected
    procedure Notify(AItem: TCollectionItem; AAction: TCollectionNotification); override;
    procedure UpdateDefault(const ADatabaseItem: TDatabaseItem);
  public
    constructor Create; reintroduce;
    function Add: TDatabaseItem;
    function Find(const AName: string): TDatabaseItem;
    function FindDefault: TDatabaseItem;
    property Items[const AIndex: integer]: TDatabaseItem read GetItems; default;
  end;

implementation

{ TDatabaseItem }

procedure TDatabaseItem.SetIsDefault(AValue: boolean);
begin
  if (FIsDefault <> AValue) then
  begin
    FIsDefault := AValue;
    if (Assigned(FDatabaseList)) then
    begin
      if (FIsDefault) then
      begin
        FDatabaseList.UpdateDefault(Self);
      end
      else
      begin
        /// Without default
        if (not (Assigned(FDatabaseList.FindDefault))) then
        begin
          FIsDefault := True;
        end;
      end;
    end;
  end;
end;

procedure TDatabaseItem.SetName(AValue: string);
begin
  if (FName <> AValue) then
  begin
    if (Assigned(FDatabaseList)) and (Assigned(FDatabaseList.Find(AValue))) then
    begin
      raise Exception.CreateFmt('duplicate name ''%s'' in %s', [AValue, FDatabaseList.ClassName]);
    end;
    FName := AValue;
  end;
end;

procedure TDatabaseItem.SetDatabase(AValue: TDatabase);
begin
  if (FDatabase <> AValue) then
  begin
    FDatabase := AValue;
    if (Assigned(FDatabase)) and (FName = '') then
    begin
      SetName(FDatabase.Name);
    end;
  end;
end;

function TDatabaseItem.GetDisplayName: string;
begin
  if (FName = '') then
  begin
    Result := inherited GetDisplayName;
  end
  else
  begin
    Result := FName;
  end;
end;

procedure TDatabaseItem.SetCollection(AValue: TCollection);
begin
  inherited SetCollection(AValue);
  FDatabaseList := nil;
  if (Assigned(AValue)) and (AValue.InheritsFrom(TDatabaseList)) then
  begin
    FDatabaseList := TDatabaseList(AValue);
  end;
end;

{ TDatabaseList }

function TDatabaseList.GetItems(const AIndex: integer): TDatabaseItem;
begin
  Result := TDatabaseItem(inherited Items[AIndex]);
end;

procedure TDatabaseList.UpdateDefault(const ADatabaseItem: TDatabaseItem);
var
  VIndex: integer;
  VDatabaseItem: TDatabaseItem;
begin
  for VIndex := 0 to (Count - 1) do
  begin
    VDatabaseItem := Items[VIndex];
    if (Assigned(VDatabaseItem)) then
    begin
      VDatabaseItem.IsDefault := (VDatabaseItem = ADatabaseItem);
    end;
  end;
end;

procedure TDatabaseList.Notify(AItem: TCollectionItem; AAction: TCollectionNotification);
begin
  inherited Notify(AItem, AAction);
  if (Assigned(AItem)) and (AItem.InheritsFrom(TDatabaseItem)) and
    (AAction = cnAdded) and (Count = 1) then
  begin
    UpdateDefault(TDatabaseItem(AItem));
  end;
end;

constructor TDatabaseList.Create;
begin
  inherited Create(TDatabaseItem);
end;

function TDatabaseList.Add: TDatabaseItem;
begin
  Result := TDatabaseItem(inherited Add);
end;

function TDatabaseList.Find(const AName: string): TDatabaseItem;
var
  VIndex: integer;
  VDatabaseItem: TDatabaseItem;
begin
  Result := nil;
  for VIndex := 0 to (Count - 1) do
  begin
    VDatabaseItem := Items[VIndex];
    if (Assigned(VDatabaseItem)) and (VDatabaseItem.Name = AName) then
    begin
      Result := VDatabaseItem;
      Break;
    end;
  end;
end;

function TDatabaseList.FindDefault: TDatabaseItem;
var
  VIndex: integer;
  VDatabaseItem: TDatabaseItem;
begin
  Result := nil;
  for VIndex := 0 to (Count - 1) do
  begin
    VDatabaseItem := Items[VIndex];
    if (Assigned(VDatabaseItem)) and (VDatabaseItem.IsDefault) then
    begin
      Result := VDatabaseItem;
      Break;
    end;
  end;
end;

end.
