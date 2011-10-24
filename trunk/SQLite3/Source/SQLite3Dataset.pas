unit SQLite3Dataset;

interface

uses
  DBClient, Classes, SQLiteTable3, DB, SysUtils;

type
   /// <summary>
  /// UpdateSQL component for specifying custom insert, update, delete or refresh statements
  ///  to TSQLiteDataset
  /// </summary>
  TSQLiteUpdateSQL = class(TComponent)
  private
    FInsertSQL: TStrings;
    FDeleteSQL: TStrings;
    FModifySQL: TStrings;
    FRefreshSQL: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DeleteSQL: TStrings read FDeleteSQL write FDeleteSQL;
    property InsertSQL: TStrings read FInsertSQL write FInsertSQL;
    property ModifySQL: TStrings read FModifySQL write FModifySQL;
    property RefreshSQL: TStrings read FRefreshSQL write FRefreshSQL;
  end;

  TSQLiteDataset = class(TClientDataset)
  private
    FUpdateSQL: TSQLiteUpdateSQL;
    FDB: TSQLiteDatabase;
    FStmtUpd: TSQLitePreparedStatement;
    FStmtIns: TSQLitePreparedStatement;
    FStmtDel: TSQLitePreparedStatement;
    FAutoIncField: TField;
    FAutoIncFieldName: string;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(MaxErrors: Integer; UseTrans: Boolean = True): Integer; reintroduce; overload;
    function ApplyUpdates(): Integer; reintroduce; overload;

    property AutoIncField: TField read FAutoIncField;
  published
    property AutoIncFieldName: string read FAutoIncFieldName write FAutoIncFieldName;
    property Database: TSQLiteDatabase read FDB write FDB;
    property UpdateSQL: TSQLiteUpdateSQL read FUpdateSQL write FUpdateSQL;

  end;


implementation

uses
  Generics.Collections,
  StrUtils;

type
  T3<TKey, T, TValue> = record
    Key: TKey;
    Second: T;
    Value: TValue;
    constructor Create(const AKey: TKey; const ASecond: T; const AValue: TValue);
  end;

{ TSQLiteDataset }

function TSQLiteDataset.ApplyUpdates(MaxErrors: Integer; UseTrans: Boolean): Integer;
var
  OrigFilter: TUpdateStatusSet;
  bkm: TBookmark;
  errCount: Integer;
  dicUpd, dicIns, dicDel: TList<T3<string, Boolean, TField>>;
  uStatus: TUpdateStatus;
  oldRecNo: Integer;
  iKey: Int64;
  {$REGION 'Doc'}
  /// <summary>
  /// Applies updates to Sqlite
  /// </summary>
  /// <param name="AUpdateStatus"></param>
  /// <returns></returns>
  {$ENDREGION}
  function DoApplyUpdate(AUpdateStatus: TUpdateStatus): Boolean;
  var
    A3: T3<string, Boolean, TField>;
    i: Integer;
    dic: TList<T3<string, Boolean, TField>>;
    stmt: TSQLitePreparedStatement;
  begin
    Result := False;
    
    //prepare values
    case AUpdateStatus of
      usModified:
      begin
        dic := dicUpd;
        stmt := FStmtUpd;
      end;
      usInserted:
      begin
        dic := dicIns;
        stmt := FStmtIns;
        {TODO -oLinas -cGeneral : resync autoincrement primary key}
        //FDB.GetLastInsertRowID
      end;
      usDeleted:
      begin
        dic := dicDel;
        stmt := FStmtDel;
      end
      else
      begin
        Exit;
      end;
    end;

    for i := 1 to dic.Count do
    begin
      A3 := dic[i-1];

      if A3.Second then
        stmt.SetParamVariant(i, dic[i-1].Value.OldValue)
      else
        stmt.SetParamVariant(i, dic[i-1].Value.Value);
    end;

    try
      Result := stmt.ExecSQL;
      if AUpdateStatus = usInserted then
      begin
        if Assigned(FAutoIncField) then
        begin
          iKey := FDB.GetLastInsertRowID;
          Edit;

          FAutoIncField.SetData(@iKey);
          {TODO -oLinas -cGeneral : exception: index is read only. dont know why...}
          Post;
        end;


      end;
    except
      Result := False;
    end;
    
  end;

  procedure FillParams();

    procedure GetPair(Astmt: TSQLitePreparedStatement; AList: TList<T3<string, Boolean, TField>>);
    var
      i, iPos: Integer;
      fld: TField;
      sField: string;
      A3: T3<string, Boolean, TField>;
    begin 
      for i := 1 to Astmt.BindParameterCount do
      begin
        A3.Key := Astmt.BindParameterName(i);
        A3.Second := False;
       // sField := A3.Key;

        if Length(A3.Key) > 0 then
        begin
          //trim first param letter
          sField := Copy(A3.Key, 2, Length(A3.Key)-1);
        end
        else
        begin
          raise ESQLiteException.Create('Incorrect parameter name: ' + A3.Key);
        end;

        fld := FindField(sField);
        if not Assigned(fld) then
        begin
          //parse param name
          iPos := PosEx('_', sField);
          if (iPos = 4) then   //correct format - OLD_ID
          begin
            A3.Second := SameText(Copy(sField, 1, 3), 'OLD');

            if A3.Second then
            begin
              fld := FieldByName('OLD_' + Copy(sField, 5, Length(sField)-4));
            end;
          end;
        end;

        if Assigned(fld) then
        begin
          A3.Value := fld;
          AList.Add(A3);
        end
        else
        begin
          raise ESQLiteException.Create(Format('Fieldname %S does not exist', []));
        end;
      end;
    end;

  begin   
    GetPair(FStmtUpd, dicUpd);
    GetPair(FStmtIns, dicIns);
    GetPair(FStmtDel, dicDel);  
  end;

begin
  Result := 0;
  errCount := 0;

  if Self.State in [dsEdit, dsInsert] then
    Post;

  if (ChangeCount < 1) and not (Assigned(FUpdateSQL)) and not (Assigned(FDB)) then
    Exit;

  FAutoIncField := FindField(FAutoIncFieldName);

  bkm := Bookmark;

  OrigFilter := StatusFilter;
  DisableControls;
  oldRecNo := RecNo;

  FStmtUpd := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FModifySQL.Text);
  FStmtIns := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FInsertSQL.Text);
  FStmtDel := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FDeleteSQL.Text);
  //dictionaries for storing prepared fields
  //key = (pair left = ParamName, pair right = Fieldname in the dataset)
  //value = TField in the dataset
  dicUpd := TList<T3<string, Boolean, TField>>.Create();
  dicIns := TList<T3<string, Boolean, TField>>.Create();
  dicDel := TList<T3<string, Boolean, TField>>.Create();

  //fill dictionaries

  if UseTrans then
    FDB.BeginTransaction;
  try
    FillParams;

    StatusFilter := [usInserted, usDeleted, usModified];

    First;

    while not Eof do
    begin
      uStatus := UpdateStatus;
      if uStatus = usUnmodified then
      begin
        Next;
        Continue;
      end;
    
      if not DoApplyUpdate(uStatus) then
        Inc(errCount)
      else
        Inc(Result);

      Next;
    end;

    MergeChangeLog;

  finally
    FStmtUpd.Free;
    FStmtIns.Free;
    FStmtDel.Free;
    dicUpd.Free;
    dicIns.Free;
    dicDel.Free;

    if (UseTrans) then
    begin
      if (errCount <= MaxErrors) or (MaxErrors = 0) then
        FDB.Commit
      else
        FDB.Rollback;
    end;

    StatusFilter := OrigFilter;

    try
      if BookmarkValid(bkm) then
        Bookmark := bkm
      else
      begin
        RecNo := oldRecNo;
      end;
    except
      //spawn
    end;
    EnableControls;
  end;
end;

function TSQLiteDataset.ApplyUpdates: Integer;
begin
  Result := ApplyUpdates(0, True);
end;

constructor TSQLiteDataset.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateSQL := nil;
  FDB := nil;
  FStmtUpd := nil;
  FStmtIns := nil;
  FStmtDel := nil;
  FAutoIncField := nil;
  FAutoIncFieldName := '';
end;

destructor TSQLiteDataset.Destroy;
begin
  FUpdateSQL := nil;
  FDB := nil;
  FAutoIncField := nil;
  FAutoIncFieldName := '';
  inherited;
end;

{ TSQLiteUpdateSQL }

constructor TSQLiteUpdateSQL.Create(AOwner: TComponent);
begin
  inherited;
  FDeleteSQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FModifySQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
end;

destructor TSQLiteUpdateSQL.Destroy;
begin
  FDeleteSQL.Free;
  FInsertSQL.Free;
  FModifySQL.Free;
  FRefreshSQL.Free;
  inherited;
end;

{ TPair<TKey, TKey, TValue> }

constructor T3<TKey, T, TValue>.Create(const AKey: TKey; const ASecond: T; const AValue: TValue);
begin
  Key := AKey;
  Second := ASecond;
  Value := AValue;
end;

end.
