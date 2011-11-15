﻿unit TestSQLiteTable3;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, Windows, SQLite3, Classes, SysUtils, SQLiteTable3, Generics.Collections;

{$HINTS OFF}

type
  // Test methods for class TSQLiteDatabase

  TestTSQLiteDatabase = class(TTestCase)
  strict private
    FSQLiteDatabase: TSQLiteDatabase;
    FWorks: Boolean;
    procedure TestAuthorize(Sender: TSQLiteDatabase; ActionCode: TSQLiteActionCode;
      const AArg1, AArg2, AArg3, AArg4: String; var AResult: Integer);
    procedure TestDoUpdateHook(Sender: TSQLiteDatabase; Operation: TSQLiteActionCode;
   const ADatabase, ATable: String; ARowID: Int64);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddNewSupportedColumnType;
    procedure TestGetTable;
    procedure TestExecSQL;
    procedure TestPrepareSQL;
    procedure TestGetTableValue;
    procedure TestGetTableString;
    procedure TestGetTableStrings;
    procedure TestGetPreparedStatement;
    procedure TestGetPreparedStatement1;
    procedure TestGetPreparedStatementIntf;
    procedure TestGetPreparedStatementIntfAndGetQuery;
    procedure TestUpdateBlob;
    procedure TestCommit;
    procedure TestRollback;
    procedure TestTableExists;
    procedure TestBackup;
    procedure TestAddCustomCollate;
    procedure TestAddSystemCollate;
    procedure TestParamsClear;
    procedure TestAttach;
    procedure TestMemoryUsed;
    procedure TestFunctions;
    procedure TestFunctions2;
    procedure TestSimpleDataLoad;
    procedure TestEncryptedDB;
    procedure TestChangePassword;
    procedure TestAuthorizer;
    procedure TestUpdateHook;
    procedure TestGetTableColumnMetadata;
    procedure TestSetExtEnabled;
    procedure TestGetVersion;
  end;
  // Test methods for class TSQLiteTable

  TestTSQLiteTable = class(TTestCase)
  strict private
    FSQLiteDatabase: TSQLiteDatabase;
    FSQLiteTable: TSQLiteTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFieldAsInteger;
    procedure TestFieldAsBlob;
    procedure TestFieldAsBlobText;
    procedure TestFieldIsNull;
    procedure TestFieldAsString;
    procedure TestFieldAsDouble;
    procedure TestNext;
    procedure TestPrevious;
    procedure TestMoveFirst;
    procedure TestMoveLast;
    procedure TestMoveTo;
  end;
  // Test methods for class TSQLitePreparedStatement

  TestTSQLitePreparedStatement = class(TTestCase)
  strict private
    FSQLiteDatabase: TSQLiteDatabase;
    FSQLitePreparedStatement: TSQLitePreparedStatement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClearParams;
    procedure TestSetParams;
    procedure TestSetParamNull;
    procedure TestExecSQLAndMapData;
    procedure TestExecQuery;
    procedure TestExecQuery1;
    procedure TestExecQuery2;
    procedure TestExecQueryIntf;
    procedure TestExecSQL;
    procedure TestExecSQL1;
    procedure TestExecSQL2;
    procedure TestExecSQL3;
    procedure TestExecSQL4;
    procedure TestUnicodeParams;
    procedure TestPreparedBlobs;
    procedure TestPreparedBlobs2;
    procedure TestPrepareStatement;
    procedure TestPrepareStatement1;
  end;
  // Test methods for class TSQLiteField

  TestTSQLiteField = class(TTestCase)
  strict private
    FSQLiteDatabase: TSQLiteDatabase;
    FSQLiteField: TSQLiteField;
    FDst: TSQLiteUniTable;
    stmt: TSQLitePreparedStatement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsNull;
    procedure TestAsBlob;
    procedure TestAsBlobPtr;
    procedure TestAsBlobText;
    procedure TestAsBlobTextDef;
    procedure TestAsDateTime;
    procedure TestAsDateTimeDef;
    procedure TestAsDouble;
    procedure TestAsDoubleDef;
    procedure TestAsInteger;
    procedure TestAsIntegerDef;
    procedure TestAsString;
    procedure TestAsStringDef;
    procedure TestValue;
    procedure TestValueDef;
  end;
  // Test methods for class TSQLiteUniTable

  TestTSQLiteUniTable = class(TTestCase)
  strict private
    FSQLiteDatabase: TSQLiteDatabase;
    FSQLiteUniTable: TSQLiteUniTable;
    stmt: TSQLitePreparedStatement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindField;
    procedure TestFieldAsInteger;
    procedure TestFieldAsBlob;
    procedure TestFieldAsBlobPtr;
    procedure TestFieldAsBlobText;
    procedure TestFieldIsNull;
    procedure TestFieldAsString;
    procedure TestFieldAsDouble;
    procedure TestNext;
  end;

implementation

uses
  Variants;

{$WARNINGS OFF}

procedure TestTSQLiteDatabase.SetUp;
begin
  FSQLiteDatabase := TSQLiteDatabase.Create('test.db');
end;

procedure TestTSQLiteDatabase.TearDown;
begin
  FSQLiteDatabase.Free;
  FSQLiteDatabase := nil;
end;

procedure TestTSQLiteDatabase.TestAddNewSupportedColumnType;
var
  ColType, iRes: Integer;
  ColTypeName: string;
begin
  ColTypeName := 'BIT';  //uppercase
  ColType := dtInt;

  FSQLiteDatabase.AddNewSupportedColumnType(ColTypeName, ColType);

  Check(TSQLiteDatabase.FColumnTypes.TryGetValue(ColTypeName, iRes));
end;

procedure TestTSQLiteDatabase.TestGetTable;
var
  ReturnValue: TSQLiteTable;
  SQL: string;
begin
  SQL := 'select * from testtable';

  ReturnValue := FSQLiteDatabase.GetTable(SQL);
  try
    Check((ReturnValue.ColCount > 0) and (ReturnValue.RowCount > 0));
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestGetTableColumnMetadata;
var
  DType, CSeq: AnsiString;
  isNotNull, isPrimKey, isAutoinc: Boolean;
begin
  FSQLiteDatabase.GetTableColumnMetadata('testtable', 'name', DType, CSeq, isNotNull, isPrimKey, isAutoinc);

  CheckTrue(SameText(DType, 'varchar (255)'));
  CheckFalse(isNotNull);
  CheckFalse(isAutoinc);
  CheckFalse(isPrimKey);

  FSQLiteDatabase.GetTableColumnMetadata('testtable', 'ID', DType, CSeq, isNotNull, isPrimKey, isAutoinc);
  CheckTrue(SameText(DType, 'integer'));
  CheckFalse(isNotNull);
  CheckFalse(isAutoinc);
  CheckTrue(isPrimKey);
end;

procedure TestTSQLiteDatabase.TestEncryptedDB;
var
  DB, DB2: TSQLiteDatabase;
  bOK: Boolean;
begin
  DeleteFile('encrypted.db');

  DB := TSQLiteDatabase.Create('encrypted.db', seUTF8, 'password');
  try
    CheckTrue(DB <> nil);

    DB.ExecSQL('CREATE TABLE IF NOT EXISTS testtable ([ID] INTEGER PRIMARY KEY,[OtherID] INTEGER NULL,[Name] VARCHAR (255),'+
      '[Number] FLOAT, [notes] BLOB, [picture] BLOB COLLATE NOCASE)');

    DB.ExecSQL('DELETE FROM testtable');

    DB.ExecSQL('INSERT INTO testtable ([OtherID], [Name], [Number]) VALUES (1, ''encrypted'', 100)');
    bOK := False;
    DB2 := TSQLiteDatabase.Create('encrypted.db');
    try
      try
        bOK := not DB2.TableExists('testtable');
      except
        bOK := True;
      end;

      CheckTrue(bOK);
    finally
      DB2.Free;
    end;


  finally
    DB.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestExecSQL;
var
  SQL: string;
begin

  SQL := 'update testtable set OtherId = 100 where ID = 10';
  FSQLiteDatabase.ExecSQL(SQL);

  Check(FSQLiteDatabase.GetLastChangedRows = 1);
end;

procedure TestTSQLiteDatabase.TestFunctions;
var
  stmt: TSQLitePreparedStatement;
  tbl: TSQLiteUniTable;
  iVal: Double;
  iInt: Integer;
begin
  FSQLiteDatabase.Functions.AddScalarFunction('TestAbs', 1,
    procedure(sqlite3_context: Psqlite3_context; ArgIndex: Integer; ArgValue: PPChar)
    begin
      case TSQLiteFunctions.GetValueType(ArgValue) of
        SQLITE_INTEGER:
        begin
          TSQLiteFunctions.ResultAsInteger(sqlite3_context,
            Abs(TSQLiteFunctions.ValueAsInteger(ArgValue)));
        end;
        SQLITE_NULL:
        begin
          TSQLiteFunctions.ResultAsNull(sqlite3_context);
        end;
        SQLITE_FLOAT:
        begin
          iVal := Abs(TSQLiteFunctions.ValueAsFloat(ArgValue));
          TSQLiteFunctions.ResultAsFloat(sqlite3_context, iVal);
        end;
        else
        begin
          iInt := Abs(TSQLiteFunctions.ValueAsInteger(ArgValue));
          TSQLiteFunctions.ResultAsInteger(sqlite3_context, iInt);
        end;
      end;
    end);

  stmt := FSQLiteDatabase.GetPreparedStatement('select TestAbs(-5) from testtable');
  try
    Check(Assigned(stmt));
    if Assigned(stmt) then
    begin
      tbl := stmt.ExecQuery;
      try
        iVal := tbl.Fields[0].AsDouble;
        Check(iVal > 0);
      finally
        tbl.Free;
      end;

    end;
  finally
    stmt.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestFunctions2;
var
  stmt, stmt2: TSQLitePreparedStatement;
  tbl, tbl2: TSQLiteUniTable;
  iVal, iVal2, iVal3: Double;
  iInt: Integer;
begin
  iVal := 0;
  FSQLiteDatabase.Functions.AddAggregateFunction('TestSum', 1,
    procedure(sqlite3_context: Psqlite3_context; ArgIndex: Integer; ArgValue: PPChar)
    begin
      case TSQLiteFunctions.GetValueType(ArgValue) of
        SQLITE_NULL:
        begin
          TSQLiteFunctions.ResultAsNull(sqlite3_context);
        end;
        SQLITE_FLOAT:
        begin
          iVal := iVal + TSQLiteFunctions.ValueAsFloat(ArgValue);
        end;
      end;
    end,
    procedure(sqlite3_context: Psqlite3_context)
    begin
      TSQLiteFunctions.ResultAsFloat(sqlite3_context, iVal);
    end);

  stmt := FSQLiteDatabase.GetPreparedStatement('select TestSum(Number) AS NUMBER1 from testtable');
  stmt2 := FSQLiteDatabase.GetPreparedStatement('select Sum(Number) AS NUMBER1 from testtable');
  try
    Check(Assigned(stmt));
    if Assigned(stmt) then
    begin
      tbl := stmt.ExecQuery;
      tbl2 := stmt2.ExecQuery;
      try
        iVal3 := tbl.FieldByName['NUMBER1'].AsDouble;
        iVal2 := tbl2.FieldByName['NUMBER1'].AsDouble;

        Check(iVal3 = iVal2);
      finally
        tbl.Free;
        tbl2.Free;
      end;

    end;
  finally
    stmt.Free;
    stmt2.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestPrepareSQL;
var
  ReturnValue: TSQLiteQuery;
  SQL: string;
begin
  FSQLiteDatabase.ParamsClear;
  SQL := 'update testtable set OtherId = ? where ID = ?';
  ReturnValue := FSQLiteDatabase.PrepareSQL(SQL);
  FSQLiteDatabase.BindSQL(ReturnValue, 1, 101);
  FSQLiteDatabase.BindSQL(ReturnValue, 2, 10);
  FSQLiteDatabase.ExecSQL(ReturnValue);

  Check(FSQLiteDatabase.GetLastChangedRows = 1);
end;

procedure TestTSQLiteDatabase.TestGetTableValue;
var
  ReturnValue: Int64;
  SQL: string;
begin
  SQL := 'select ID from testtable limit 1';
  ReturnValue := FSQLiteDatabase.GetTableValue(SQL);

  Check(ReturnValue = 1);
end;

procedure TestTSQLiteDatabase.TestGetVersion;
var
  ReturnValue: string;
begin
  ReturnValue := FSQLiteDatabase.Version;
  Check(Length(ReturnValue)>0);
end;

procedure TestTSQLiteDatabase.TestMemoryUsed;
var
  iMem: Int64;
begin
  iMem := 0;
  iMem := FSQLiteDatabase.GetMemoryUsed;
  Check(iMem > 0);
end;

procedure TestTSQLiteDatabase.TestGetTableString;
var
  ReturnValue: string;
  SQL: string;
begin
  sql := 'select Name from testtable where ID = 2';
  ReturnValue := FSQLiteDatabase.GetTableString(SQL);

  Check(ReturnValue = 'Some Name Русский 1');
end;

procedure TestTSQLiteDatabase.TestGetTableStrings;
var
  Value: TStrings;
  SQL: string;
begin
  SQL := 'select name from testtable limit 1,10';
  Value := TStringList.Create;
  try
    FSQLiteDatabase.GetTableStrings(SQL, Value);
    Check(Value.Count > 0);
  finally
    Value.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestGetPreparedStatement;
var
  ReturnValue: TSQLitePreparedStatement;
  SQL: string;
begin
  SQL := 'select * from testtable where ID > ? and Number > ?';
  ReturnValue := FSQLiteDatabase.GetPreparedStatement(SQL);

  try
    Check(ReturnValue.Stmt <> nil);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestGetPreparedStatement1;
var
  ReturnValue: TSQLitePreparedStatement;
  SQL: string;
begin
  SQL := 'select * from testtable where ID > ? and Number > ?';
  ReturnValue := FSQLiteDatabase.GetPreparedStatement(SQL, [5, 1.1]);

  try
    Check(ReturnValue.Stmt <> nil);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestGetPreparedStatementIntf;
var
  ReturnValue: ISQLitePreparedStatement;
  SQL: string;
begin
  SQL := 'select * from testtable where ID > ? and Number > ?';
  ReturnValue := FSQLiteDatabase.GetPreparedStatementIntf(SQL, [5, 1.1]);

  Check(ReturnValue.BindParameterCount = 2);
end;

procedure TestTSQLiteDatabase.TestGetPreparedStatementIntfAndGetQuery;
var
  ReturnValue: Int64;
  SQL: string;
begin
  SQL := 'select * from testtable where ID > ? and Number > ?';
  ReturnValue := FSQLiteDatabase.GetPreparedStatementIntf(SQL, [5, 1.1]).ExecQueryIntf.FieldByName['ID'].AsInteger;

  CheckTrue(ReturnValue > 5);
end;

procedure TestTSQLiteDatabase.TestUpdateBlob;
var
  BlobData: TFileStream;
  SQL: string;
begin
  SQL := 'update testtable set picture = ? where ID = 5';
  BlobData := TFileStream.Create(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Sunset.jpg',
   fmOpenRead or fmShareDenyNone);
  try
    FSQLiteDatabase.UpdateBlob(SQL, BlobData);

    Check(FSQLiteDatabase.GetLastChangedRows = 1);
  finally
    BlobData.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestUpdateHook;
begin
  FWorks := False;
  FSQLiteDatabase.OnUpdate := TestDoUpdateHook;

  FSQLiteDatabase.ExecSQL('update testtable set OtherID = 10 where ID = 100;');

  CheckTrue(FWorks);

  FWorks := False;

  FSQLiteDatabase.OnUpdate := nil;

  FSQLiteDatabase.ExecSQL('update testtable set OtherID = 10 where ID = 101;');
  CheckFalse(FWorks);
end;

procedure TestTSQLiteDatabase.TestChangePassword;
var
  DB, DB2: TSQLiteDatabase;
  stmt: TSQLitePreparedStatement;
  dst: TSQLiteUniTable;
begin
  DB := TSQLiteDatabase.Create('encrypted.db', seUTF8, 'password');
  try
    CheckTrue(DB <> nil);

    stmt := DB.GetPreparedStatement('select * from testtable where OtherId = ?', [1]);
    try
      dst := stmt.ExecQuery;

      try
        CheckFalse(dst.EOF);

        try
          DB.ChangePassword('master');
          CheckTrue(True);

          DB2 := TSQLiteDatabase.Create('encrypted.db', seUTF8, 'master');
          try
            CheckTrue(DB2.TableExists('testtable'));
          finally
            DB2.Free;
          end;


          DB.ChangePassword('password');
        except
          CheckTrue(False, 'Cannot change password');
        end;

      finally
        dst.Free;
      end;
    finally
      stmt.Free;
    end;

  finally
    DB.Free;
  end;

end;

procedure TestTSQLiteDatabase.TestCommit;
var
  iVal, iTest: Int64;
begin
  Randomize;
  iTest := Random(10000000);
  FSQLiteDatabase.BeginTransaction;

  FSQLiteDatabase.ExecSQL(Format('update testtable set OtherID = %D where ID = 10', [iTest]));

  FSQLiteDatabase.Commit;

  iVal := FSQLiteDatabase.GetTableValue('select OtherID from testtable where ID = 10');

  Check(iVal = iTest);
end;

procedure TestTSQLiteDatabase.TestDoUpdateHook(Sender: TSQLiteDatabase;
  Operation: TSQLiteActionCode; const ADatabase, ATable: String; ARowID: Int64);
begin
  FWorks := True;
end;

procedure TestTSQLiteDatabase.TestRollback;
var
  iVal, iTest: Int64;
begin
  Randomize;
  iTest := Random(10000000);
  FSQLiteDatabase.BeginTransaction;

  FSQLiteDatabase.ExecSQL(Format('update testtable set OtherID = %D where ID = 10', [iTest]));

  FSQLiteDatabase.Rollback;
  iVal := FSQLiteDatabase.GetTableValue('select OtherID from testtable where ID = 10');

  Check(iVal <> iTest);
end;

procedure TestTSQLiteDatabase.TestSetExtEnabled;
begin
  FSQLiteDatabase.ExtensionsEnabled := True;

  CheckTrue(FSQLiteDatabase.ExtensionsEnabled);
end;

procedure TestTSQLiteDatabase.TestSimpleDataLoad;
var
  DB: TSQLiteDatabase;
  stmt: TSQLitePreparedStatement;
  dst: TSQLiteUniTable;
  iVal: Integer;
begin
  DB := TSQLiteDatabase.Create('test.db');
  try
    //prepare statement with param value = 10
    stmt := DB.GetPreparedStatement('select * from testtable where ID > ?', [10]);
    try
      //execute query and return the resultset
      dst := stmt.ExecQuery;
      try
        while not dst.EOF do
        begin
          //get data from resultset
          iVal := dst['ID'].AsInteger;
          Check(iVal > 10);
          dst.Next;
        end;
      finally
        dst.Free;
      end;
    finally
      stmt.Free;
    end;
  finally
    DB.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestTableExists;
var
  ReturnValue: Boolean;
  TableName: string;
begin
  TableName := 'testtable';

  ReturnValue := FSQLiteDatabase.TableExists(TableName);

  Check(ReturnValue);
  TableName := 'notexisting';
  ReturnValue := FSQLiteDatabase.TableExists(TableName);
  CheckFalse(ReturnValue);
end;

procedure TestTSQLiteDatabase.TestBackup;
var
  ReturnValue: Integer;
  TargetDB: TSQLiteDatabase;
begin
  TargetDB := TSQLiteDatabase.Create(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'backup.db');
  try
    ReturnValue := FSQLiteDatabase.Backup(TargetDB);

    Check(ReturnValue = SQLITE_DONE);

    DeleteFile(TargetDB.Filename);
  finally
    TargetDB.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestAddCustomCollate;
var
  xCompare: TCollateXCompare;
  name: string;
begin
  // TODO: Setup method call parameters
 // FSQLiteDatabase.AddCustomCollate(name, xCompare);
  Check(True);
  // TODO: Validate method results
end;

procedure TestTSQLiteDatabase.TestAddSystemCollate;
begin
 // FSQLiteDatabase.AddSystemCollate;
  Check(True);
  // TODO: Validate method results
end;

procedure TestTSQLiteDatabase.TestAttach;
var
  sFile, sName: string;
  tbl: TSQLiteTable;
  SDB: TSQLiteDatabase;
begin
  sFile := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'attach16.db';
  SDB := TSQLiteDatabase.Create(sFile, seUTF16);
  SDB.ExecSQL('CREATE TABLE IF NOT EXISTS test ([ID] INTEGER PRIMARY KEY,[OtherID] INTEGER NULL,[Name] VARCHAR (255),'+
  '[Number] FLOAT, [notes] BLOB, [picture] BLOB COLLATE NOCASE);');
  try
    SDB.ExecSQL('INSERT INTO test (NAME) VALUES (''test'')');
    sName := 'ATTACHED';
    Check(FSQLiteDatabase.Attach(sFile, sName));
  //  Check(FSQLiteDatabase.TableExists('test'));
    tbl := FSQLiteDatabase.GetTable('select * from ATTACHED.test');

    Check(tbl.Count > 0);
  finally
    tbl.Free;
    SDB.Free;
  end;
end;

procedure TestTSQLiteDatabase.TestAuthorize(Sender: TSQLiteDatabase; ActionCode: TSQLiteActionCode;
  const AArg1, AArg2, AArg3, AArg4: String; var AResult: Integer);
begin
  AResult := SQLITE_OK;
  //AResult could be of: SQLITE_OK, SQLITE_DENY, SQLITE_IGNORE
  FWorks := True;
  case ActionCode of
    SQLITE_CREATE_INDEX: ;
    SQLITE_CREATE_TABLE: ;
    SQLITE_CREATE_TEMP_INDEX: ;
    SQLITE_CREATE_TEMP_TABLE: ;
    SQLITE_CREATE_TEMP_TRIGGER: ;
    SQLITE_CREATE_TEMP_VIEW: ;
    SQLITE_CREATE_TRIGGER: ;
    SQLITE_CREATE_VIEW: ;
    SQLITE_DELETE: ;
    SQLITE_DROP_INDEX: ;
    SQLITE_DROP_TABLE: ;
    SQLITE_DROP_TEMP_INDEX: ;
    SQLITE_DROP_TEMP_TABLE: ;
    SQLITE_DROP_TEMP_TRIGGER: ;
    SQLITE_DROP_TEMP_VIEW: ;
    SQLITE_DROP_TRIGGER: ;
    SQLITE_DROP_VIEW: ;
    SQLITE_INSERT: ;
    SQLITE_PRAGMA: ;
    SQLITE_READ: ;
    SQLITE_SELECT: AResult := SQLITE_OK;
    SQLITE_TRANSACTION: ;
    SQLITE_UPDATE: ;
    SQLITE_ATTACH: ;
    SQLITE_DETACH: ;
    SQLITE_ALTER_TABLE: ;
    SQLITE_REINDEX: ;
    SQLITE_ANALYZE: ;
    SQLITE_CREATE_VTABLE: ;
    SQLITE_DROP_VTABLE: ;
    SQLITE_FUNCTION: ;
    SQLITE_SAVEPOINT: ;
    SQLITE_COPY: ;
  end;
end;

procedure TestTSQLiteDatabase.TestAuthorizer;
begin
  FWorks := False;
  //add authorize
  FSQLiteDatabase.OnAuthorize := TestAuthorize;

  CheckTrue(FSQLiteDatabase.TableExists('testtable'));
  CheckTrue(FWorks);
  //remove authorize
  FWorks := False;
  FSQLiteDatabase.OnAuthorize := nil;

  CheckTrue(FSQLiteDatabase.TableExists('testtable'));
  CheckFalse(FWorks);
end;

procedure TestTSQLiteDatabase.TestParamsClear;
begin
  FSQLiteDatabase.ParamsClear;

  Check(True);
end;

procedure TestTSQLiteTable.SetUp;
begin
  FSQLiteDatabase := TSQLiteDatabase.Create('test.db');
  FSQLiteTable := TSQLiteTable.Create(FSQLiteDatabase, 'select * from testtable where ID = 1');
end;

procedure TestTSQLiteTable.TearDown;
begin
  FSQLiteDatabase.Free;
  FSQLiteTable.Free;
  FSQLiteTable := nil;
end;

procedure TestTSQLiteTable.TestFieldAsInteger;
var
  ReturnValue: Int64;
  I: Cardinal;
begin
  I := 0;
  ReturnValue := FSQLiteTable.FieldAsInteger(I);

  Check(ReturnValue = 1);
end;

procedure TestTSQLiteTable.TestFieldAsBlob;
var
  ReturnValue: TMemoryStream;
  I: Cardinal;
begin
  I := 5;
//  ReturnValue := TMemoryStream.Create;
  ReturnValue := FSQLiteTable.FieldAsBlob(I);
  try
    Check(ReturnValue.Size > 0);


  finally
    //ReturnValue.Free;
  end;
end;

procedure TestTSQLiteTable.TestFieldAsBlobText;
var
  ReturnValue: string;
  I: Cardinal;
begin
  I := 4;
  ReturnValue := FSQLiteTable.FieldAsBlobText(I);

  Check(ReturnValue <> '');
end;

procedure TestTSQLiteTable.TestFieldIsNull;
var
  ReturnValue: Boolean;
  I: Cardinal;
begin
  I := 3;
  ReturnValue := FSQLiteTable.FieldIsNull(I);

  Check(ReturnValue);
end;

procedure TestTSQLiteTable.TestFieldAsString;
var
  ReturnValue: string;
  I: Cardinal;
begin
  I := 2;
  ReturnValue := FSQLiteTable.FieldAsString(I);

  Check(ReturnValue <> '');
end;

procedure TestTSQLiteTable.TestFieldAsDouble;
var
  ReturnValue: Double;
  I: Cardinal;
begin
  I := 1;
  ReturnValue := FSQLiteTable.FieldAsDouble(I);

  Check(ReturnValue = 4);
end;

procedure TestTSQLiteTable.TestNext;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FSQLiteTable.Next;

  Check(FSQLiteTable.EOF);
end;

procedure TestTSQLiteTable.TestPrevious;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FSQLiteTable.Previous;

  Check(FSQLiteTable.BOF);
end;

procedure TestTSQLiteTable.TestMoveFirst;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FSQLiteTable.MoveFirst;

  Check(ReturnValue);
end;

procedure TestTSQLiteTable.TestMoveLast;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FSQLiteTable.MoveLast;

  Check(ReturnValue);
end;

procedure TestTSQLiteTable.TestMoveTo;
var
  ReturnValue: Boolean;
  position: Cardinal;
begin
  position := 2;
  ReturnValue := FSQLiteTable.MoveTo(position);

  CheckFalse(ReturnValue);
  position := 0;
  ReturnValue := FSQLiteTable.MoveTo(position);
  Check(ReturnValue);
end;

procedure TestTSQLitePreparedStatement.SetUp;
begin
  FSQLiteDatabase := TSQLiteDatabase.Create('test.db');
 // FSQLitePreparedStatement := TSQLitePreparedStatement.Create(FSQLiteDatabase,
 //   'select * from testtable where (OtherID = ?) and (Name = ?)  and (Number = ?)');
  FSQLitePreparedStatement := TSQLitePreparedStatement.Create(FSQLiteDatabase,
    'select * from testtable where (OtherID = ?) and (Name = ?)  and (Number = ?)', [1, 'Name 1', 1]);
end;

procedure TestTSQLitePreparedStatement.TearDown;
begin
  FSQLiteDatabase.Free;
  FSQLitePreparedStatement.Free;
  FSQLitePreparedStatement := nil;
end;

procedure TestTSQLitePreparedStatement.TestClearParams;
begin
  FSQLitePreparedStatement.ClearParams;

  CheckFalse(FSQLitePreparedStatement.ParamsBound);
end;

procedure TestTSQLitePreparedStatement.TestSetParams;
var
  value: Int64;
  aname: string;
  numb: Double;
begin
  value := 1;
  aname := 'Name 1';
  numb := 1;
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.SetParamInt(1, value);
  FSQLitePreparedStatement.SetParamText(2, aname);
  FSQLitePreparedStatement.SetParamFloat(3, numb);

  Check(FSQLitePreparedStatement.ParamCount = 3);
end;

procedure TestTSQLitePreparedStatement.TestUnicodeParams;
var
  ReturnValue: Boolean;
  SQL, sData, sDB: string;
  iId: Int64;
begin
  SQL := 'insert into testtable (Name) Values (?)';
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement(SQL);
  sData :=  'Lietuvių and Русский';
  ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL, [sData]);
  CheckTrue(ReturnValue);
  iId := FSQLiteDatabase.GetLastInsertRowID;
  SQL := 'select name from testtable where Rowid = ?';
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement(SQL, [iId]);
  sDB := FSQLitePreparedStatement.ExecQueryIntf.Fields[0].AsString;
  CheckTrue(AnsiSameStr(sDB, sData));

  SQL := 'delete from testtable where Rowid = ?';
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement(SQL, [iId]);
  FSQLitePreparedStatement.ExecSQL;
end;

procedure TestTSQLitePreparedStatement.TestSetParamNull;
var
  name: string;
begin
  // TODO: Setup method call parameters
 // FSQLitePreparedStatement.SetParamNull(name);
 // Check(True);
  // TODO: Validate method results
end;

type
  TMyData = class
  private
    FID: Integer;
    FOtherID: Integer;
    FName: string;
    FNumber: Double;
    FEmpty: Integer;

  public
    property ID: Integer read FID write FID;
    property OtherID: Integer read FOtherID write FOtherID;
    property Name: string read FName write FName;
    property Number: Double read FNumber write FNumber;
    property Empty: Integer read FEmpty write FEmpty;
  end;

procedure TestTSQLitePreparedStatement.TestExecSQLAndMapData;
var
  ReturnValue: Boolean;
  DataList: TObjectList<TMyData>;
begin
  DataList := TObjectList<TMyData>.Create;
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement('select * from testtable limit 10');
  try

    ReturnValue := FSQLitePreparedStatement.ExecSQLAndMapData<TMyData>(DataList);

    Check(ReturnValue);
    Check(DataList.Count > 0);
  finally
    DataList.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestExecQuery;
var
  ReturnValue: TSQLiteUniTable;
begin
  ReturnValue := FSQLitePreparedStatement.ExecQuery;

  try
    CheckFalse(ReturnValue.EOF);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestExecQuery1;
var
  ReturnValue: TSQLiteUniTable;
  SQL: string;
begin
  SQL := 'select * from testtable';

  ReturnValue := FSQLitePreparedStatement.ExecQuery(SQL);
  try
    CheckFalse(ReturnValue.EOF);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestExecQuery2;
var
  ReturnValue: TSQLiteUniTable;
  SQL: string;
begin
  SQL := 'select * from testtable where ID = ?';
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement(SQL);
  ReturnValue := FSQLitePreparedStatement.ExecQuery(SQL, [10]);
  try
    Check(ReturnValue.Fields[0].AsInteger = 10);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestExecQueryIntf;
var
  ReturnValue: ISQLiteTable;
  SQL: string;
begin
  SQL := 'select * from testtable where ID = ?';
  FSQLitePreparedStatement.ClearParams;
  FSQLitePreparedStatement.PrepareStatement(SQL);
  ReturnValue := FSQLitePreparedStatement.ExecQueryIntf(SQL, [10]);
  Check(ReturnValue.Fields[0].AsInteger = 10);
end;

procedure TestTSQLitePreparedStatement.TestExecSQL;
var
  ReturnValue: Boolean;
begin
//  ReturnValue := FSQLitePreparedStatement.ExecSQL;
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestExecSQL1;
var
  ReturnValue: Boolean;
  SQL: string;
begin
  // TODO: Setup method call parameters
//  ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL);
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestExecSQL2;
var
  ReturnValue: Boolean;
  RowsAffected: Integer;
begin
  // TODO: Setup method call parameters
//  ReturnValue := FSQLitePreparedStatement.ExecSQL(RowsAffected);
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestExecSQL3;
var
  ReturnValue: Boolean;
  RowsAffected: Integer;
  SQL: string;
begin
  // TODO: Setup method call parameters
 // ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL, RowsAffected);
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestExecSQL4;
var
  ReturnValue: Boolean;
  SQL: string;
begin
  // TODO: Setup method call parameters
 // ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL, []);
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestPreparedBlobs;
var
  ReturnValue: Boolean;
  SQL: string;
  ss: TStringStream;
  img: TFileStream;
begin
  SQL := 'insert into testtable (notes, picture) values (?,?)';
  FSQLitePreparedStatement.ClearParams;

  ss := TStringStream.Create('Testing strings');
  img := TFileStream.Create('Sunset.jpg', fmOpenRead or fmShareDenyNone);

  FSQLitePreparedStatement.PrepareStatement(SQL, [ss, img]);
  ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL);
  try
    Check(ReturnValue = true);
    Check(FSQLiteDatabase.GetLastChangedRows = 1);
  finally
    ss.Free;
    img.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestPreparedBlobs2;
var
  ReturnValue: Boolean;
  SQL: string;
  ss: TStringStream;
  img: TFileStream;
begin
  SQL := 'insert into testtable (notes, picture) values (?,?)';
  FSQLitePreparedStatement.ClearParams;

  ss := TStringStream.Create('Testing strings');
  img := TFileStream.Create('Sunset.jpg', fmOpenRead or fmShareDenyNone);

  FSQLitePreparedStatement.PrepareStatement(SQL);
  FSQLitePreparedStatement.SetParamBlob(1, ss);
  FSQLitePreparedStatement.SetParamBlob(2, img);
  ReturnValue := FSQLitePreparedStatement.ExecSQL(SQL);
  try
    Check(ReturnValue = true);
    Check(FSQLiteDatabase.GetLastChangedRows = 1);
  finally
    ss.Free;
    img.Free;
  end;
end;

procedure TestTSQLitePreparedStatement.TestPrepareStatement;
var
  SQL: string;
begin
  // TODO: Setup method call parameters
//  FSQLitePreparedStatement.PrepareStatement(SQL);
  // TODO: Validate method results
end;

procedure TestTSQLitePreparedStatement.TestPrepareStatement1;
var
  SQL: string;
begin
  // TODO: Setup method call parameters
//  FSQLitePreparedStatement.PrepareStatement(SQL, []);
  // TODO: Validate method results
end;

procedure TestTSQLiteField.SetUp;
begin
  FSQLiteDatabase := TSQLiteDatabase.Create('test.db');
  FSQLiteField := nil;
  FDst := FSQLiteDatabase.GetPreparedStatementIntf('select * from testtable where rowid < 100').ExecQuery;
  //stmt := FSQLiteDatabase.GetPreparedStatement('select * from testtable where rowid < 100');
 // FDst := stmt.ExecQuery;
end;

procedure TestTSQLiteField.TearDown;
begin
  FSQLiteField := nil;
  stmt.Free;
  FDst.Free;
  FSQLiteDatabase.Free;
end;

procedure TestTSQLiteField.TestIsNull;
var
  ReturnValue: Boolean;
begin
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.IsNull;
  CheckTrue(ReturnValue);
end;

procedure TestTSQLiteField.TestAsBlob;
var
  ReturnValue: TMemoryStream;
begin
  FSQLiteField := FDst.FieldByName['picture'];
  ReturnValue := FSQLiteField.AsBlob;
  try
    Check(ReturnValue.Size > 0);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLiteField.TestAsBlobPtr;
var
  ReturnValue: Pointer;
  iNumBytes: Integer;
begin
  FSQLiteField := FDst.FieldByName['picture'];
  ReturnValue := nil;
  ReturnValue := FSQLiteField.AsBlobPtr(iNumBytes);
  Check(ReturnValue <> nil);
  Check(iNumBytes > 0);
end;

procedure TestTSQLiteField.TestAsBlobText;
var
  ReturnValue: string;
begin
  FSQLiteField := FDst.FieldByName['notes'];
  ReturnValue := FSQLiteField.AsBlobText;
  Check(Length(ReturnValue)>0);
end;

procedure TestTSQLiteField.TestAsBlobTextDef;
var
  ReturnValue: string;
  Def: string;
begin
  FSQLiteField := FDst.FieldByName['notes'];
  ReturnValue := FSQLiteField.AsBlobTextDef;
  Check(Length(ReturnValue)>0);
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.AsBlobTextDef('');
  Check(ReturnValue = '');
end;

procedure TestTSQLiteField.TestAsDateTime;
var
  ReturnValue: TDateTime;
begin
 // ReturnValue := FSQLiteField.AsDateTime;
  // TODO: Validate method results
end;

procedure TestTSQLiteField.TestAsDateTimeDef;
var
  ReturnValue: TDateTime;
  Def: TDateTime;
begin
  // TODO: Setup method call parameters
 // ReturnValue := FSQLiteField.AsDateTimeDef(Def);
  // TODO: Validate method results
end;

procedure TestTSQLiteField.TestAsDouble;
var
  ReturnValue: Double;
begin
  FDst.Next;
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.AsDouble;


  Check(ReturnValue < -1);
end;

procedure TestTSQLiteField.TestAsDoubleDef;
var
  ReturnValue: Double;
  Def: Double;
begin
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.AsDoubleDef(-1);
  Check(ReturnValue = -1);
end;

procedure TestTSQLiteField.TestAsInteger;
var
  ReturnValue: Int64;
begin
  FSQLiteField := FDst.FieldByName['OtherID'];
  ReturnValue := FSQLiteField.AsInteger;
  Check(ReturnValue = 4);
end;

procedure TestTSQLiteField.TestAsIntegerDef;
var
  ReturnValue: Int64;
  Def: Int64;
begin
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.AsIntegerDef(-1);
  Check(ReturnValue = -1);
end;

procedure TestTSQLiteField.TestAsString;
var
  ReturnValue: string;
begin
  FSQLiteField := FDst.FieldByName['Name'];
  ReturnValue := FSQLiteField.AsString;
  Check(Length(ReturnValue)>0);
end;

procedure TestTSQLiteField.TestAsStringDef;
var
  ReturnValue: string;
  Def: string;
begin
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.AsStringDef('a');
  Check(ReturnValue = 'a');
end;

procedure TestTSQLiteField.TestValue;
var
  ReturnValue: Variant;
begin
  FSQLiteField := FDst.FieldByName['OtherID'];
  ReturnValue := FSQLiteField.Value;
  Check(ReturnValue = 4);
end;

procedure TestTSQLiteField.TestValueDef;
var
  ReturnValue: Variant;
  Def: Variant;
begin
  FSQLiteField := FDst.FieldByName['Number'];
  ReturnValue := FSQLiteField.ValueDef(-1);
  Check(ReturnValue = -1);
end;

procedure TestTSQLiteUniTable.SetUp;
begin
  FSQLiteDatabase := TSQLiteDatabase.Create('test.db');
  stmt := TSQLitePreparedStatement.Create(FSQLiteDatabase);
  FSQLiteUniTable := stmt.ExecQuery('select * from testtable');
end;

procedure TestTSQLiteUniTable.TearDown;
begin
  FSQLiteUniTable.Free;
  stmt.Free;
  FSQLiteDatabase.Free;
  FSQLiteUniTable := nil;
end;

procedure TestTSQLiteUniTable.TestFindField;
var
  ReturnValue: TSQLiteField;
  AFieldName: string;
begin
  AFieldName := 'Number';
  ReturnValue := FSQLiteUniTable.FindField(AFieldName);
  Check(ReturnValue <> nil);
end;

procedure TestTSQLiteUniTable.TestFieldAsInteger;
var
  ReturnValue: Int64;
  I: Cardinal;
begin
  ReturnValue := FSQLiteUniTable.FieldAsInteger(0);

  Check(ReturnValue > 0);
end;

procedure TestTSQLiteUniTable.TestFieldAsBlob;
var
  ReturnValue: TMemoryStream;
  I: Cardinal;
begin
  I := 5;
  ReturnValue := FSQLiteUniTable.FieldAsBlob(I);
  try
    Check(ReturnValue.Size > 0);
  finally
    ReturnValue.Free;
  end;
end;

procedure TestTSQLiteUniTable.TestFieldAsBlobPtr;
var
  ReturnValue: Pointer;
  iNumBytes: Integer;
  I: Cardinal;
begin
  I := 5;
  ReturnValue := nil;
  ReturnValue := FSQLiteUniTable.FieldAsBlobPtr(I, iNumBytes);

  Check(ReturnValue <> nil);
end;

procedure TestTSQLiteUniTable.TestFieldAsBlobText;
var
  ReturnValue: string;
  I: Cardinal;
begin
  I := 4;
  ReturnValue := FSQLiteUniTable.FieldAsBlobText(I);
  Check(Length(ReturnValue)>0);
end;

procedure TestTSQLiteUniTable.TestFieldIsNull;
var
  ReturnValue: Boolean;
  I: Cardinal;
begin
  ReturnValue := FSQLiteUniTable.FieldIsNull(3);

  Check(ReturnValue);
end;

procedure TestTSQLiteUniTable.TestFieldAsString;
var
  ReturnValue: string;
  I: Cardinal;
begin
  I := 2;
  ReturnValue := FSQLiteUniTable.FieldAsString(I);
  Check(Length(ReturnValue)>0);
end;

procedure TestTSQLiteUniTable.TestFieldAsDouble;
var
  ReturnValue: Double;
  I: Cardinal;
begin
  I := 3;
  ReturnValue := FSQLiteDatabase.GetPreparedStatementIntf('select * from testtable where Number = 1').ExecQueryIntf.FieldAsDouble(I);
  Check(ReturnValue = 1);
end;

procedure TestTSQLiteUniTable.TestNext;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FSQLiteUniTable.Next;

  Check(ReturnValue);
end;

{$HINTS ON}
{$WARNINGS ON}

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSQLiteDatabase.Suite);
  RegisterTest(TestTSQLiteTable.Suite);
  RegisterTest(TestTSQLitePreparedStatement.Suite);
  RegisterTest(TestTSQLiteField.Suite);
  RegisterTest(TestTSQLiteUniTable.Suite);
end.

