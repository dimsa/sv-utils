unit SQLiteTable3;

{
  Simple classes for using SQLite's exec and get_table.

  TSQLiteDatabase wraps the calls to open and close an SQLite database.
  It also wraps SQLite_exec for queries that do not return a result set

  TSQLiteTable wraps execution of SQL query.
  It run query and read all returned rows to internal buffer.
  It allows accessing fields by name as well as index and can move through a
  result set forward and backwards, or randomly to any row.

  TSQLiteUniTable wraps execution of SQL query.
  It run query as TSQLiteTable, but reading just first row only!
  You can step to next row (until not EOF) by 'Next' method.
  You cannot step backwards! (So, it is called as UniDirectional result set.)
  It not using any internal buffering, this class is very close to Sqlite API.
  It allows accessing fields by name as well as index on actual row only.
  Very good and fast for sequentional scanning of large result sets with minimal
    memory footprint.

  Warning! Do not close TSQLiteDatabase before any TSQLiteUniTable,
    because query is closed on TSQLiteUniTable destructor and database connection
    is used during TSQLiteUniTable live!

  SQL parameter usage:
    You can add named parameter values by call set of AddParam* methods.
    Parameters will be used for first next SQL statement only.
    Parameter name must be prefixed by ':', '$' or '@' and same prefix must be
    used in SQL statement!
    Sample:
      table.AddParamText(':str', 'some value');
      s := table.GetTableString('SELECT value FROM sometable WHERE id=:str');

   Notes from Andrew Retmanski on prepared queries
   The changes are as follows:

   SQLiteTable3.pas
   - Added new boolean property Synchronised (this controls the SYNCHRONOUS pragma as I found that turning this OFF increased the write performance in my application)
   - Added new type TSQLiteQuery (this is just a simple record wrapper around the SQL string and a TSQLiteStmt pointer)
   - Added PrepareSQL method to prepare SQL query - returns TSQLiteQuery
   - Added ReleaseSQL method to release previously prepared query
   - Added overloaded BindSQL methods for Integer and String types - these set new values for the prepared query parameters
   - Added overloaded ExecSQL method to execute a prepared TSQLiteQuery
   - Added TSQLitePreparedStatement class for managing prepared queries. It is recommended to use it together with TSQliteUniTable for best performance

   Usage of the new methods should be self explanatory but the process is in essence:

   1. Call PrepareSQL to return TSQLiteQuery 2. Call BindSQL for each parameter in the prepared query 3. Call ExecSQL to run the prepared query 4. Repeat steps 2 & 3 as required 5. Call ReleaseSQL to free SQLite resources

   One other point - the Synchronised property throws an error if used inside a transaction.

   Acknowledments
   Adapted by Tim Anderson (tim@itwriting.com)
   Originally created by Pablo Pissanetzky (pablo@myhtpc.net)
   Modified and enhanced by Lukas Gebauer
   Modified and enhanced by Tobias Gunkel
   Modified and enhanced by Linas Naginionis (lnaginionis@gmail.com)
}

interface

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

uses
  SQLite3,
  {$IFDEF DELPHI16_UP}
  {$IFDEF WIN32}
  Winapi.Windows,
  {$ENDIF}
  System.Classes, System.SysUtils, System.Generics.Collections;
  {$ELSE}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Generics.Collections;
  {$ENDIF}

const

  dtInt = 1;
  dtNumeric = 2;
  dtStr = 3;
  dtBlob = 4;
  dtNull = 5;
  //my types
  dtDate = 15;
  dtDateTime = 16;

type

  ESQLiteException = class(Exception)
  end;

  TSQliteParam = class
  public
    name: string;
    index: Integer;
    valuetype: integer;
    valueinteger: int64;
    valuefloat: double;
    valuedata: RawByteString;

    constructor Create(); virtual;
  end;

  THookQuery = procedure(Sender: TObject;const SQL: String) of object;

  TSQLiteQuery = record
    SQL: String;
    Statement: TSQLiteStmt;
  end;

  TSQLiteTable = class;
  TSQLiteUniTable = class;
  TSQLitePreparedStatement = class;

  TSQLiteDatabase = class
  class var
    FColumnTypes: TDictionary<string,Integer>;
  private
    fDB: TSQLiteDB;
    fInTrans: boolean;
    fSync: boolean;
    fParams: TList;
    FOnQuery: THookQuery;
    FFormatSett: TFormatSettings;
    FFilename: string;
    procedure RaiseError(const s: string; const SQL: string);
    procedure SetParams(Stmt: TSQLiteStmt);
    function GetRowsChanged: integer;
    class procedure InitDefaultColumnTypes;
  protected
    procedure SetSynchronised(Value: boolean);
    procedure DoQuery(const value: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
     /// <summary>
     /// Adds new supported column type
     /// </summary>
     /// <param name="ColTypeName">Column Type Name</param>
     /// <param name="ColType">Column Type constant, e.g. dtStr</param>
    class procedure AddNewSupportedColumnType(const ColTypeName: string; ColType: Integer = dtStr);

    function GetTable(const SQL: String): TSQLiteTable;
    procedure ExecSQL(const SQL: String); overload;
    procedure ExecSQL(Query: TSQLiteQuery); overload;
    function PrepareSQL(const SQL: String): TSQLiteQuery;
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer); overload;
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: String); overload;
    procedure ReleaseSQL(Query: TSQLiteQuery);
    function GetUniTable(const SQL: String): TSQLiteUniTable; deprecated;
    function GetTableValue(const SQL: String): int64;
    function GetTableString(const SQL: String): string;
    procedure GetTableStrings(const SQL: String; const Value: TStrings);
    function GetPreparedStatement(const SQL: string): TSQLitePreparedStatement; overload;
    function GetPreparedStatement(const SQL: string; const Params: array of TVarRec): TSQLitePreparedStatement; overload;
    procedure UpdateBlob(const SQL: String; BlobData: TStream);
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function TableExists(const TableName: string): boolean;
    function GetLastInsertRowID: int64;
    function GetLastChangedRows: int64;
    procedure SetTimeout(Value: integer);
    function Backup(TargetDB: TSQLiteDatabase): integer; Overload;
    function Backup(TargetDB: TSQLiteDatabase; const targetName: String; const sourceName: String): integer; Overload;
    function Version: string;
    procedure AddCustomCollate(const name: string; xCompare: TCollateXCompare);
    //adds collate named SYSTEM for correct data sorting by user's locale
    Procedure AddSystemCollate;
    procedure ParamsClear; deprecated;
    procedure AddParamInt(const name: string; value: int64); deprecated;
    procedure AddParamFloat(const name: string; value: double); deprecated;
    procedure AddParamText(const name: string; const value: string); deprecated;
    procedure AddParamNull(const name: string); deprecated;

    property Filename: string read FFilename;
    /// <summary>
    /// Format settings to use for DateTime fields
    /// </summary>
    property FmtSett: TFormatSettings read FFormatSett;
    property DB: TSQLiteDB read fDB;
  published
    property IsTransactionOpen: boolean read fInTrans;
    //database rows that were changed (or inserted or deleted) by the most recent SQL statement
    property RowsChanged : integer read getRowsChanged;
    property Synchronised: boolean read FSync write SetSynchronised;
    property OnQuery: THookQuery read FOnQuery write FOnQuery;

  end;

  TSQLiteTable = class
  private
    fResults: TList;
    fRowCount: cardinal;
    fColCount: cardinal;
    fCols: TStringList;
    fColTypes: TList;
    fRow: cardinal;
    function GetFields(I: cardinal): string;
    function GetEOF: boolean;
    function GetBOF: boolean;
    function GetColumns(I: integer): string;
    function GetFieldValByName(const FieldName: string): string;
    function GetFieldIndex(const FieldName: string): integer;
    function GetCount: integer;
    function GetCountResult: integer;
    function GetIsLastRow: boolean;
  public
    constructor Create(DB: TSQLiteDatabase; const SQL: String);
    destructor Destroy; override;

    function FieldAsInteger(I: cardinal): int64;
    function FieldAsBlob(I: cardinal): TMemoryStream;
    function FieldAsBlobText(I: cardinal): string;
    function FieldIsNull(I: cardinal): boolean;
    function FieldAsString(I: cardinal): string;
    function FieldAsDouble(I: cardinal): double;
    function Next: boolean;
    function Previous: boolean;
    property EOF: boolean read GetEOF;
    property BOF: boolean read GetBOF;
    property IsLastRow: boolean read GetIsLastRow;
    property Fields[I: cardinal]: string read GetFields;
    property FieldValByNameAsString[const FieldName: string]: string read GetFieldValByName;
    property FieldIndex[const FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: cardinal read fColCount;
    property RowCount: cardinal read fRowCount;
    property Row: cardinal read fRow;
    function MoveFirst: boolean;
    function MoveLast: boolean;
    function MoveTo(position: cardinal): boolean;
    property Count: integer read GetCount;
    // The property CountResult is used when you execute count(*) queries.
    // It returns 0 if the result set is empty or the value of the
    // first field as an integer.
    property CountResult: integer read GetCountResult;
  end;
  /// <summary>
  /// SQLite prepared statement - Linas Naginionis
  /// </summary>
  TSQLitePreparedStatement = class
  private
    FSQL: string;
    FDB: TSQLiteDatabase;
    fParams: TObjectList<TSQliteParam>;
    FStmt: TSQLiteStmt;
    FParamsBound: Boolean;
    procedure BindParams;
    procedure SetParams(const Params: array of TVarRec);
    function FindParam(const I: Integer): TSQliteParam; overload;
    function FindParam(const name: string): TSQliteParam; overload;
    function BindParameterCount: Integer;
    procedure SetSQL(const Value: string);
    function GetParamCount: Integer;
  public
    constructor Create(DB: TSQLiteDatabase); overload;
    constructor Create(DB: TSQLiteDatabase; const SQL: string); overload;
    constructor Create(DB: TSQLiteDatabase; const SQL: string; const Params: array of TVarRec); overload;
    destructor Destroy; override;

    procedure ClearParams;

    procedure SetParamInt(const name: string; value: int64); overload;
    procedure SetParamFloat(const name: string; value: double); overload;
    procedure SetParamText(const name: string; const value: string); overload;
    procedure SetParamNull(const name: string); overload;
    procedure SetParamInt(const I: Integer; value: int64); overload;
    procedure SetParamFloat(const I: Integer; value: double); overload;
    procedure SetParamText(const I: Integer; const value: string); overload;
    procedure SetParamNull(const I: Integer); overload;
    procedure SetParamDateTime(const I: Integer; const Value: TDateTime); overload;
    procedure SetParamDateTime(const name: string; const Value: TDateTime); overload;
    procedure SetParamDate(const I: Integer; const Value: TDate); overload;
    procedure SetParamDate(const name: string; const Value: TDate); overload;

    function ExecSQLAndMapData<T: constructor, class>(var DataList: TObjectList<T>): Boolean;
    function ExecQuery(): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string): TSQLiteUniTable; overload;
    function ExecQuery(const SQL: string; const Params: array of TVarRec): TSQLiteUniTable; overload;
    function ExecSQL(): Boolean; overload;
    function ExecSQL(const SQL: string): Boolean; overload;
    function ExecSQL(var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; var RowsAffected: Integer): Boolean; overload;
    function ExecSQL(const SQL: string; const Params: array of TVarRec): Boolean; overload;

    procedure PrepareStatement(const SQL: string = ''); overload;
    procedure PrepareStatement(const SQL: string; const Params: array of TVarRec); overload;

    property DB: TSQLiteDatabase read FDB;
    property Stmt: TSQLiteStmt read FStmt;
    property SQL: string read FSQL write SetSQL;
    property ParamCount: Integer read GetParamCount;
    property ParamsBound: Boolean read FParamsBound;
  end;

  TSQLiteField = class
  public
    Index: Integer;
    Name: string;
    FieldType: Integer;
    Table: TSQLiteUniTable;

    constructor Create(); virtual;
    destructor Destroy; override;

    function IsNull: Boolean;

    function AsBlob: TMemoryStream;
    function AsBlobPtr(out iNumBytes: integer): Pointer;
    function AsBlobText: string;
    function AsBlobTextDef(const Def: string = ''): string;
    function AsDateTime: TDateTime;  //datetime support slows down data retrieval
    function AsDateTimeDef(const Def: TDateTime = 0.0): TDateTime;
    function AsDouble: Double;
    function AsDoubleDef(const Def: Double = 0.0): Double;
    function AsInteger: Int64;
    function AsIntegerDef(const Def: Int64 = 0): Int64;
    function AsString: string;
    function AsStringDef(const Def: string = ''): string;
    function Value: Variant;
    function ValueDef(const Def: Variant): Variant;
  end;

  TSQLiteUniTable = class
  private
    fColCount: cardinal;
    fCols: TDictionary<string,Integer>;
    FColTypes: array of Integer;
    FColNames: array of string;
    FFields: TObjectList<TSQLiteField>;
    fRow: cardinal;
    fEOF: boolean;
    fStmt: TSQLiteStmt;
    fDB: TSQLiteDatabase;
    fSQL: string;
    function GetFieldsAsString(I: cardinal): string;
    function GetColumns(I: integer): string;
    function GetFieldByNameAsString(const FieldName: string): string;
    function GetFieldIndex(const FieldName: string): integer;
    function GetFieldByName(const FieldName: string): TSQLiteField;
    procedure SetFieldByName(const FieldName: string; const Value: TSQLiteField);
    procedure GetDataTypes;
    function GetField(I: Integer): TSQLiteField;

    procedure SetField(I: Integer; const Value: TSQLiteField);  protected
    procedure SetFields(I: Cardinal; const Value: Variant);
    function GetFieldsVal(I: Cardinal): Variant; virtual;

  public
    constructor Create(DB: TSQLiteDatabase); overload; //use with caution!
    constructor Create(DB: TSQLiteDatabase; hStmt: TSQLiteStmt); overload;
    constructor Create(DB: TSQLiteDatabase; const SQL: string); overload;
    destructor Destroy; override;
    function FindField(const AFieldName: string): TSQLiteField;
    function FieldAsInteger(I: cardinal): int64;
    function FieldAsBlob(I: cardinal): TMemoryStream;
    function FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): Pointer;
    function FieldAsBlobText(I: cardinal): string;
    function FieldIsNull(I: cardinal): boolean;
    function FieldAsString(I: cardinal): string;
    function FieldAsDouble(I: cardinal): double;
    function Next: boolean;

    property EOF: boolean read FEOF;
    property FieldCount: Cardinal read fColCount;
    property FieldsAsString[I: cardinal]: string read GetFieldsAsString;
    property Fields[I: Integer]: TSQLiteField read GetField write SetField;
    property FieldsVal[I: Cardinal]: Variant read GetFieldsVal write SetFields;
    property FieldByName[const FieldName: string]: TSQLiteField read GetFieldByName write SetFieldByName; default;
    property FieldByNameAsString[const FieldName: string]: string read GetFieldByNameAsString;
    property FieldIndex[const FieldName: string]: integer read GetFieldIndex;
    property Columns[I: integer]: string read GetColumns;
    property ColCount: cardinal read fColCount;
    property Row: cardinal read fRow;
    property SQL: string read fSQL write fSQL;
    property Stmt: TSQLiteStmt read FStmt;
  end;

procedure DisposePointer(ptr: pointer); cdecl;

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
{$ENDIF}

implementation

uses
  Variants,
  Math,
  Rtti,
  TypInfo;

const
  //default supported column types defined in sqlite db
  // each other type which isn't supported is bound as text column
  DEF_COLCOUNT = 9;
  DEF_COLTYPES_NAMES : array[0..DEF_COLCOUNT-1] of string =
    ('INTEGER', 'BOOLEAN', 'NUMERIC', 'FLOAT', 'DOUBLE', 'REAL',
     'DATETIME', 'DATE', 'BLOB'
    );
  DEF_COLTYPES : array[0..DEF_COLCOUNT-1] of Integer =
    (dtInt, dtInt, dtNumeric, dtNumeric, dtNumeric, dtNumeric,
     dtDateTime, dtDate, dtBlob
    );

procedure DisposePointer(ptr: pointer); cdecl;
begin
  if assigned(ptr) then
    freemem(ptr);
end;

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len,
    PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// TSQLiteDatabase
//------------------------------------------------------------------------------

constructor TSQLiteDatabase.Create(const FileName: string);
var
  Msg: PAnsiChar;
  iResult: integer;
begin
  inherited Create;
  fParams := TList.Create;
  FFilename := FileName;
  FFormatSett := TFormatSettings.Create();
  FFormatSett.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSett.DateSeparator := '-';
  FFormatSett.LongDateFormat := FFormatSett.ShortDateFormat + ' HH:MM:SS';

  self.fInTrans := False;

  Msg := nil;
  try
    iResult := SQLite3_Open16(PChar(FileName), Fdb);

//    iResult := SQLite3_Open(PAnsiChar(AnsiString(FileName)), Fdb);

    if iResult <> SQLITE_OK then
      if Assigned(Fdb) then
      begin
        Msg := Sqlite3_ErrMsg(Fdb);
        raise ESqliteException.CreateFmt('Failed to open database "%s" : %s',
          [FileName, Msg]);
      end
      else
        raise ESqliteException.CreateFmt('Failed to open database "%s" : unknown error',
          [FileName]);

//set a few configs
//L.G. Do not call it here. Because busy handler is not setted here,
// any share violation causing exception!

//    self.ExecSQL('PRAGMA SYNCHRONOUS=NORMAL;');
//    self.ExecSQL('PRAGMA temp_store = MEMORY;');

  finally
    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;

end;

//..............................................................................

destructor TSQLiteDatabase.Destroy;
begin
  if self.fInTrans then
    self.Rollback;  //assume rollback
  if Assigned(fDB) then
    SQLite3_Close(fDB);
  ParamsClear;
  fParams.Free;
  inherited;
end;

function TSQLiteDatabase.GetLastInsertRowID: int64;
begin
  Result := Sqlite3_LastInsertRowID(self.fDB);
end;

function TSQLiteDatabase.GetPreparedStatement(const SQL: string;
  const Params: array of TVarRec): TSQLitePreparedStatement;
begin
  Result := TSQLitePreparedStatement.Create(Self, SQL, Params);
end;

function TSQLiteDatabase.GetPreparedStatement(const SQL: string): TSQLitePreparedStatement;
begin
  Result := TSQLitePreparedStatement.Create(Self, SQL);

end;

function TSQLiteDatabase.GetLastChangedRows: int64;
begin
  Result := SQLite3_TotalChanges(self.fDB);
end;

//..............................................................................

procedure TSQLiteDatabase.RaiseError(const s: string; const SQL: string);
//look up last error and raise an exception with an appropriate message
var
  Msg: PAnsiChar;
  ret : integer;
begin

  Msg := nil;
  ret := sqlite3_errcode(self.fDB);

  if ret <> SQLITE_OK then
    Msg := sqlite3_errmsg(self.fDB);

  if Msg <> nil then
    raise ESqliteException.CreateFmt(s +'.'#13'Error [%d]: %s.'#13'"%s": %s',
    [ret, SQLiteErrorStr(ret),SQL, Msg])
  else
    raise ESqliteException.CreateFmt(s, [SQL, 'No message']);

end;

procedure TSQLiteDatabase.SetSynchronised(Value: boolean);
begin
  if Value <> fSync then
  begin
    if Value then
      ExecSQL('PRAGMA synchronous = ON;')
    else
      ExecSQL('PRAGMA synchronous = OFF;');
    fSync := Value;
  end;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
  iStepResult: integer;
begin
  try
    if Sqlite3_Prepare16_v2(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError('Error executing SQL', SQL);
    if (Stmt = nil) then
      RaiseError('Could not prepare SQL statement', SQL);
    DoQuery(SQL);
    SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    if (iStepResult <> SQLITE_DONE) then
      begin
      SQLite3_reset(stmt);
      RaiseError('Error executing SQL statement', SQL);
      end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

{$WARNINGS OFF}
procedure TSQLiteDatabase.ExecSQL(Query: TSQLiteQuery);
var
  iStepResult: integer;
begin
  if Assigned(Query.Statement) then
  begin
    iStepResult := Sqlite3_step(Query.Statement);

    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(Query.Statement);
      RaiseError('Error executing prepared SQL statement', Query.SQL);
    end;
    Sqlite3_Reset(Query.Statement);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSQLiteDatabase.PrepareSQL(const SQL: String): TSQLiteQuery;
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
begin
  Result.SQL := SQL;
  Result.Statement := nil;

  if Sqlite3_Prepare16(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>  SQLITE_OK then
    RaiseError('Error executing SQL', SQL)
  else
    Result.Statement := Stmt;

  if (Result.Statement = nil) then
    RaiseError('Could not prepare SQL statement', SQL);
  DoQuery(SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer);
begin
  if Assigned(Query.Statement) then
    sqlite3_Bind_Int(Query.Statement, Index, Value)
  else
    RaiseError('Could not bind integer to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: String);
begin
  if Assigned(Query.Statement) then
    Sqlite3_Bind_Text16(Query.Statement, Index, PChar(Value), Length(Value) * SizeOf(char), Pointer(SQLITE_STATIC))
  else
    RaiseError('Could not bind string to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.ReleaseSQL(Query: TSQLiteQuery);
begin
  if Assigned(Query.Statement) then
  begin
    Sqlite3_Finalize(Query.Statement);
    Query.Statement := nil;
  end
  else
    RaiseError('Could not release prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

procedure TSQLiteDatabase.UpdateBlob(const SQL: String; BlobData: TStream);
var
  iSize: integer;
  ptr: pointer;
  Stmt: TSQLiteStmt;
  Msg: PChar;
  NextSQLStatement: PChar;
  iStepResult: integer;
  iBindResult: integer;

begin
  //expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
  if pos('?', SQL) = 0 then
    RaiseError('SQL must include a ? parameter', SQL);

  Msg := nil;
  try

    if Sqlite3_Prepare16_v2(self.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError('Could not prepare SQL statement', SQL);

    if (Stmt = nil) then
      RaiseError('Could not prepare SQL statement', SQL);
    DoQuery(SQL);

    //now bind the blob data
    iSize := BlobData.size;

    GetMem(ptr, iSize);

    if (ptr = nil) then
      raise ESqliteException.CreateFmt('Error getting memory to save blob',
        [SQL, 'Error']);

    BlobData.position := 0;
    BlobData.Read(ptr^, iSize);

    iBindResult := SQLite3_Bind_Blob(stmt, 1, ptr, iSize, @DisposePointer);

    if iBindResult <> SQLITE_OK then
      RaiseError('Error binding blob to database', SQL);

    iStepResult := Sqlite3_step(Stmt);

    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(stmt);
      RaiseError('Error executing SQL statement', SQL);
    end;

  finally

    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);

    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;

end;

//..............................................................................

function TSQLiteDatabase.GetTable(const SQL: String): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL);
end;

function TSQLiteDatabase.GetUniTable(const SQL: string): TSQLiteUniTable;
var
  stmt: TSQLitePreparedStatement;
begin
  Result := nil;
  stmt := TSQLitePreparedStatement.Create(Self);
  try
    Result := stmt.ExecQuery(SQL);
   // Result := TSQLiteUniTable.Create(Self, SQL);
  finally
    stmt.Free;
  end;
end;

class procedure TSQLiteDatabase.InitDefaultColumnTypes;
var
  i: Integer;
begin
  TSQLiteDatabase.FColumnTypes.Clear;

  for i := Low(DEF_COLTYPES_NAMES) to High(DEF_COLTYPES_NAMES) do
  begin
    TSQLiteDatabase.FColumnTypes.Add(DEF_COLTYPES_NAMES[i], DEF_COLTYPES[i]);
  end;
end;

function TSQLiteDatabase.GetTableValue(const SQL: string): int64;
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Result := 0;
  stmt := TSQLitePreparedStatement.Create(Self);
  Table := stmt.ExecQuery(SQL);
  try
    if not Table.EOF then
      Result := Table.Fields[0].AsInteger;
  finally
    Table.Free;
    stmt.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: string): String;
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Result := '';
  stmt := TSQLitePreparedStatement.Create(Self);
  Table := stmt.ExecQuery(SQL);
  try
    if not Table.EOF then
      Result := Table.Fields[0].AsString;
  finally
    Table.Free;
    stmt.Free;
  end;
end;

procedure TSQLiteDatabase.GetTableStrings(const SQL: string;
  const Value: TStrings);
var
  Table: TSQLiteUniTable;
  stmt: TSQLitePreparedStatement;
begin
  Value.Clear;
  stmt := TSQLitePreparedStatement.Create(Self, SQL);
  Table := stmt.ExecQuery(SQL);
  Value.BeginUpdate;
  try
    while not table.EOF do
    begin
      Value.Add(Table.FieldAsString(0));
      table.Next;
    end;
  finally
    Value.EndUpdate;
    Table.Free;
    stmt.Free;
  end;
end;

procedure TSQLiteDatabase.BeginTransaction;
begin
  if not self.fInTrans then
  begin
    self.ExecSQL('BEGIN TRANSACTION');
    self.fInTrans := True;
  end
  else
    raise ESqliteException.Create('Transaction already open');
end;

procedure TSQLiteDatabase.Commit;
begin
  self.ExecSQL('COMMIT');
  self.fInTrans := False;
end;

procedure TSQLiteDatabase.Rollback;
begin
  self.ExecSQL('ROLLBACK');
  self.fInTrans := False;
end;

function TSQLiteDatabase.TableExists(const TableName: string): boolean;
var
  sql: string;
  ds: TSqliteTable;
begin
  //returns true if table exists in the database
  sql := 'select [sql] from sqlite_master where [type] = ''table'' and lower(name) = ''' +
    lowercase(TableName) + ''' ';
  ds := self.GetTable(sql);
  try
    Result := (ds.Count > 0);
  finally
    ds.Free;
  end;
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
  SQLite3_BusyTimeout(self.fDB, Value);
end;

function TSQLiteDatabase.Version: string;
begin
  Result := UTF8ToString(SQLite3_Version);
end;

procedure TSQLiteDatabase.AddCustomCollate(const name: string;
  xCompare: TCollateXCompare);
begin
  sqlite3_create_collation16(fdb, PChar(name), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
  {$IFDEF WIN32}
  sqlite3_create_collation16(fdb, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
  {$ENDIF}
end;

procedure TSQLiteDatabase.ParamsClear;
var
  n: integer;
begin
  for n := fParams.Count - 1 downto 0 do
    TSQliteParam(fparams[n]).free;
  fParams.Clear;
end;

procedure TSQLiteDatabase.AddParamInt(const name: string; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_INTEGER;
  par.valueinteger := value;
  fParams.Add(par);
end;

class procedure TSQLiteDatabase.AddNewSupportedColumnType(const ColTypeName: string; ColType: Integer);
var
  sColName: string;
begin
  if (ColTypeName <> '') and (ColType > 0) then
  begin
    sColName := UpperCase(ColTypeName);

    FColumnTypes.AddOrSetValue(sColName, ColType);
  end;
end;

procedure TSQLiteDatabase.AddParamFloat(const name: string; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_FLOAT;
  par.valuefloat := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamText(const name: string; const value: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_TEXT;
  par.valuedata := UTF8Encode(value);
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamNull(const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.SetParams(Stmt: TSQLiteStmt);
var
  n: integer;
  i: integer;
  par: TSQliteParam;
begin
  try
    for n := 0 to fParams.Count - 1 do
    begin
      par := TSQliteParam(fParams[n]);
      i := sqlite3_bind_parameter_index(Stmt, PAnsiChar(ansistring(par.name)));
      if i > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(Stmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(Stmt, i, par.valuefloat);
          SQLITE_TEXT:
            sqlite3_bind_text16(Stmt, i, PChar(UTF8ToUnicodeString(par.valuedata)),
              length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_NULL:
            sqlite3_bind_null(Stmt, i);
        end;
      end;
    end;
  finally
    ParamsClear;
  end;
end;

//database rows that were changed (or inserted or deleted) by the most recent SQL statement
function TSQLiteDatabase.GetRowsChanged: integer;
begin
  Result := SQLite3_Changes(self.fDB);
end;

procedure TSQLiteDatabase.DoQuery(const value: string);
begin
  if assigned(OnQuery) then
    OnQuery(Self, Value);
end;

//returns result of SQLITE3_Backup_Step
function TSQLiteDatabase.Backup(TargetDB: TSQLiteDatabase; const targetName: String; const sourceName: String): integer;
var
  pBackup: TSQLiteBackup;
begin
  pBackup := Sqlite3_backup_init(TargetDB.DB,PAnsiChar(ansistring(targetName)),self.DB,PAnsiChar(ansistring(sourceName)));

  if (pBackup = nil) then
    raise ESqliteException.Create('Could not initialize backup')
  else begin
      try

        result := SQLITE3_Backup_Step(pBackup,-1); //copies entire db
      finally
        SQLITE3_backup_finish(pBackup);
      end;
  end;
end;

function TSQliteDatabase.Backup(TargetDB: TSQLiteDatabase): integer;
begin
  result := self.Backup(TargetDB,'main','main');
end;

//------------------------------------------------------------------------------
// TSQLiteTable
//------------------------------------------------------------------------------

constructor TSQLiteTable.Create(DB: TSQLiteDatabase; const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PChar;
  iStepResult: integer;
  ptr: pointer;
  iNumBytes: integer;
  thisBlobValue: TMemoryStream;
  thisStringValue: pstring;
  thisDoubleValue: pDouble;
  thisIntValue: pInt64;
  thisColType: pInteger;
  i: integer;
  DeclaredColType: PChar;
  ActualColType: integer;
  ptrValue: PChar;
begin
  inherited create;
  try
    self.fRowCount := 0;
    self.fColCount := 0;
    //if there are several SQL statements in SQL, NextSQLStatment points to the
    //beginning of the next one. Prepare only prepares the first SQL statement.
    if Sqlite3_Prepare16_v2(DB.fDB, PChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      DB.RaiseError('Error executing SQL', SQL);
    if (Stmt = nil) then
      DB.RaiseError('Could not prepare SQL statement', SQL);
    DB.DoQuery(SQL);
    DB.SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    while (iStepResult <> SQLITE_DONE) do
    begin
      case iStepResult of
        SQLITE_ROW:
          begin
            Inc(fRowCount);
            if (fRowCount = 1) then
            begin
            //get data types
              fCols := TStringList.Create;
              fColTypes := TList.Create;
              fColCount := SQLite3_ColumnCount(stmt);
              for i := 0 to Pred(fColCount) do begin
                fCols.Add(UpperCase(Sqlite3_ColumnName16(stmt, i)));
              end;
              for i := 0 to Pred(fColCount) do
              begin
                new(thisColType);
                DeclaredColType := Sqlite3_ColumnDeclType16(stmt, i);
                if DeclaredColType = nil then
                  thisColType^ := Sqlite3_ColumnType(stmt, i) //use the actual column type instead
                //seems to be needed for last_insert_rowid
                else
                  if (DeclaredColType = 'INTEGER') or (DeclaredColType = 'BOOLEAN') then
                    thisColType^ := dtInt
                  else
                    if (DeclaredColType = 'NUMERIC') or
                      (DeclaredColType = 'FLOAT') or
                      (DeclaredColType = 'DOUBLE') or
                      (DeclaredColType = 'REAL') then
                      thisColType^ := dtNumeric
                    else
                      if DeclaredColType = 'BLOB' then
                        thisColType^ := dtBlob
                      else
                        thisColType^ := dtStr;
                fColTypes.Add(thiscoltype);
              end;
              fResults := TList.Create;
            end;

          //get column values
            for i := 0 to Pred(ColCount) do
            begin
              ActualColType := Sqlite3_ColumnType(stmt, i);
              if (ActualColType = SQLITE_NULL) then
                fResults.Add(nil)
              else
                if pInteger(fColTypes[i])^ = dtInt then
                begin
                  new(thisintvalue);
                  thisintvalue^ := Sqlite3_ColumnInt64(stmt, i);
                  fResults.Add(thisintvalue);
                end
                else
                  if pInteger(fColTypes[i])^ = dtNumeric then
                  begin
                    new(thisdoublevalue);
                    thisdoublevalue^ := Sqlite3_ColumnDouble(stmt, i);
                    fResults.Add(thisdoublevalue);
                  end
                  else
                    if pInteger(fColTypes[i])^ = dtBlob then
                    begin
                      iNumBytes := Sqlite3_ColumnBytes(stmt, i);
                      if iNumBytes = 0 then
                        thisblobvalue := nil
                      else
                      begin
                        thisblobvalue := TMemoryStream.Create;
                        thisblobvalue.position := 0;
                        ptr := Sqlite3_ColumnBlob(stmt, i);
                        //call again, see sqlite docs
                        iNumBytes := Sqlite3_ColumnBytes(stmt, i);
                        thisblobvalue.writebuffer(ptr^, iNumBytes);
                      end;
                      fResults.Add(thisblobvalue);
                    end
                    else
                    begin
                      new(thisstringvalue);
                      ptrValue := Sqlite3_ColumnText16(stmt, i);
                      setstring(thisstringvalue^, ptrvalue, strlen(ptrvalue));
                      fResults.Add(thisstringvalue);
                    end;
            end;
          end;
        SQLITE_BUSY:
          raise ESqliteException.CreateFmt('Could not prepare SQL statement',
            [SQL, 'SQLite is Busy']);
      else
        begin
        SQLite3_reset(stmt);
        DB.RaiseError('Could not retrieve data', SQL);
        end;
      end;
      iStepResult := Sqlite3_step(Stmt);
    end;
    fRow := 0;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

//..............................................................................

destructor TSQLiteTable.Destroy;
var
  i: cardinal;
  iColNo: integer;
begin
  if Assigned(fResults) then
  begin
    for i := 0 to fResults.Count - 1 do
    begin
      //check for blob type
      iColNo := (i mod fColCount);
      case pInteger(self.fColTypes[iColNo])^ of
        dtBlob:
          TMemoryStream(fResults[i]).Free;
        dtStr:
          if fResults[i] <> nil then
          begin
            setstring(string(fResults[i]^), nil, 0);
            dispose(fResults[i]);
          end;
      else
        dispose(fResults[i]);
      end;
    end;
    fResults.Free;
  end;
  if Assigned(fCols) then
    fCols.Free;
  if Assigned(fColTypes) then
    for i := 0 to fColTypes.Count - 1 do
      dispose(fColTypes[i]);
  fColTypes.Free;
  inherited;
end;

//..............................................................................

function TSQLiteTable.GetColumns(I: integer): string;
begin
  Result := fCols[I];
end;

//..............................................................................

function TSQLiteTable.GetCountResult: integer;
begin
  if not EOF then
    Result := StrToInt(Fields[0])
  else
    Result := 0;
end;

function TSQLiteTable.GetCount: integer;
begin
  Result := FRowCount;
end;

//..............................................................................

function TSQLiteTable.GetEOF: boolean;
begin
  Result := fRow >= fRowCount;
end;

function TSqliteTable.GetIsLastRow: boolean;
begin
Result := fRow = (fRowCount -1);
end;

function TSQLiteTable.GetBOF: boolean;
begin
  Result := fRow <= 0;
end;

//..............................................................................

function TSQLiteTable.GetFieldValByName(const FieldName: string): string;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(const FieldName: string): integer;
begin
  if (fCols = nil) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  Result := fCols.IndexOf(UpperCase(FieldName));

  if (result < 0) then
  begin
    raise ESqliteException.Create('Field not found in dataset: ' + fieldname)
  end;
end;

//..............................................................................

function TSQLiteTable.GetFields(I: cardinal): string;
var
  thisvalue: pstring;
  thistype: integer;
begin
  Result := '';
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
  thistype := pInteger(self.fColTypes[I])^;

  case thistype of
    dtStr:
      begin
        thisvalue := self.fResults[(self.frow * self.fColCount) + I];
        if (thisvalue <> nil) then
          Result := thisvalue^
        else
          Result := '';
      end;
    dtInt:
      Result := IntToStr(self.FieldAsInteger(I));
    dtNumeric:
      Result := FloatToStr(self.FieldAsDouble(I));
    dtBlob:
      Result := self.FieldAsBlobText(I);
  else
    Result := '';
  end;
end;

function TSqliteTable.FieldAsBlob(I: cardinal): TMemoryStream;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := nil
  else
    if pInteger(self.fColTypes[I])^ = dtBlob then
      Result := TMemoryStream(self.fResults[(self.frow * self.fColCount) + I])
    else
      raise ESqliteException.Create('Not a Blob field');
end;

function TSqliteTable.FieldAsBlobText(I: cardinal): string;
var
  MemStream: TMemoryStream;
  ts: TStringStream;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);

  if MemStream <> nil then
    if MemStream.Size > 0 then
      begin
         ts := TStringStream.Create('',TEncoding.UTF8);
         try
         ts.LoadFromStream(MemStream);
         ts.Position := 0;
         result := ts.ReadString(ts.size);
         finally
         ts.Free;
         end;
      end;
     //do not free the TMemoryStream here; it is freed when
     //TSqliteTable is destroyed

end;


function TSqliteTable.FieldAsInteger(I: cardinal): int64;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := trunc(strtofloat(pString(self.fResults[(self.frow * self.fColCount) + I])^))
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsDouble(I: cardinal): double;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := 0
  else
    if pInteger(self.fColTypes[I])^ = dtInt then
      Result := pInt64(self.fResults[(self.frow * self.fColCount) + I])^
    else
      if pInteger(self.fColTypes[I])^ = dtNumeric then
        Result := pDouble(self.fResults[(self.frow * self.fColCount) + I])^
      else
        raise ESqliteException.Create('Not an integer or numeric field');
end;

function TSqliteTable.FieldAsString(I: cardinal): string;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  if (self.fResults[(self.frow * self.fColCount) + I] = nil) then
    Result := ''
  else
    Result := self.GetFields(I);
end;

function TSqliteTable.FieldIsNull(I: cardinal): boolean;
var
  thisvalue: pointer;
begin
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  thisvalue := self.fResults[(self.frow * self.fColCount) + I];
  Result := (thisvalue = nil);
end;

//..............................................................................

function TSQLiteTable.Next: boolean;
begin
  Result := False;
  if not EOF then
  begin
    Inc(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.Previous: boolean;
begin
  Result := False;
  if not BOF then
  begin
    Dec(fRow);
    Result := True;
  end;
end;

function TSQLiteTable.MoveFirst: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := 0;
    Result := True;
  end;
end;

function TSQLiteTable.MoveLast: boolean;
begin
  Result := False;
  if self.fRowCount > 0 then
  begin
    fRow := fRowCount - 1;
    Result := True;
  end;
end;

{$WARNINGS OFF}
function TSQLiteTable.MoveTo(position: cardinal): boolean;
begin
  Result := False;
  if (self.fRowCount > 0) and (self.fRowCount > position) then
  begin
    fRow := position;
    Result := True;
  end;
end;
{$WARNINGS ON}

{ TSQLiteUniTable }

constructor TSQLiteUniTable.Create(DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB);

  DB.DoQuery(SQL);

  GetDataTypes;
end;

constructor TSQLiteUniTable.Create(DB: TSQLiteDatabase);
begin
  inherited Create;
  self.fDB := db;
  self.fEOF := false;
  self.fRow := 0;
  self.fColCount := 0;
  self.fSQL := SQL;
  fStmt := nil;
  FFields := TObjectList<TSQLiteField>.Create(True);
end;

constructor TSQLiteUniTable.Create(DB: TSQLiteDatabase; hStmt: TSQLiteStmt);
begin
  Create(DB);
  fStmt := hStmt;
  GetDataTypes;
end;

destructor TSQLiteUniTable.Destroy;
begin
  fStmt := nil;
  FFields.Free;
  if Assigned(fCols) then
  begin
    fCols.Free;
  end;
  inherited;
end;

procedure TSQLiteUniTable.GetDataTypes;
var
  i: Integer;
  sColName, sColType: string;
  aField: TSQLiteField;
  DeclaredColType: PChar;
  iColType: Integer;
begin
  //get data types
  FFields.Clear;
  fColCount := SQLite3_ColumnCount(fstmt);
  SetLength(FColTypes, fColCount);
  SetLength(FColNames, fColCount);
  fCols := TDictionary<string, Integer>.Create(fColCount);

  Next;

  for i := 0 to Pred(fColCount) do
  begin
    sColName := AnsiUpperCase(Sqlite3_ColumnName16(fstmt, i));
    DeclaredColType := Sqlite3_ColumnDeclType16(stmt, i);
    if DeclaredColType = nil then
      FColTypes[i] := SQLite3_ColumnType(fStmt, i)
    else
    begin
      sColType := UpperCase(DeclaredColType);

      if TSQLiteDatabase.FColumnTypes.TryGetValue(sColType, iColType) then
      begin
        FColTypes[i] := iColType;
      end
      else
      begin
        FColTypes[i] := dtStr;
      end;


     { if (sColType = 'INTEGER') or (sColType = 'BOOLEAN') then
        FColTypes[i] := dtInt
      else
        if (sColType = 'NUMERIC') or
          (sColType = 'FLOAT') or
          (sColType = 'DOUBLE') or
          (sColType = 'REAL') then
          FColTypes[i] := dtNumeric
        else
        if (sColType = 'DATETIME') then
          FColTypes[i] := dtDateTime
        else if (sColType = 'DATE') then
          FColTypes[i] := dtDate
        else
        if sColType = 'BLOB' then
          FColTypes[i] := dtBlob
          else
            FColTypes[i] := dtStr;  }
    end;
    aField := TSQLiteField.Create;
    aField.Table := Self;
    aField.Index := i;
    aField.Name := sColName;
    aField.FieldType := FColTypes[i];
    FFields.Add(aField);
    fCols.Add(sColName, i);
    FColNames[i] := sColName;
  end;

end;

function TSQLiteUniTable.FieldAsBlob(I: cardinal): TMemoryStream;
var
  iNumBytes: integer;
  ptr: pointer;
begin

  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
  if iNumBytes > 0 then
  begin
    Result := TMemoryStream.Create;
    ptr := Sqlite3_ColumnBlob(fstmt, i);
    //call it again - see sqlite docs
    iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
    Result.writebuffer(ptr^, iNumBytes);
    Result.Position := 0;
  end
  else
  Result := nil;

end;

function TSQLiteUniTable.FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): Pointer;
begin
  Result := Sqlite3_ColumnBlob(fstmt, i);
  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
end;

function TSQLiteUniTable.FieldAsBlobText(I: cardinal): string;
var
  MemStream: TMemoryStream;
  ts: TStringStream;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);
  try

  if MemStream <> nil then
    if MemStream.Size > 0 then
      begin
         ts := TStringStream.Create('',TEncoding.UTF8);
         try
         ts.LoadFromStream(MemStream);
         ts.Position := 0;
         result := ts.ReadString(ts.size);
         finally
         ts.Free;
         end;
      end;

  finally
  MemStream.Free;
  end;


end;

function TSQLiteUniTable.FieldAsDouble(I: cardinal): double;
begin
  Result := Sqlite3_ColumnDouble(fstmt, i);
end;

function TSQLiteUniTable.FieldAsInteger(I: cardinal): int64;
begin
  Result := Sqlite3_ColumnInt64(fstmt, i);
end;

function TSQLiteUniTable.FieldAsString(I: cardinal): string;
begin
  Result := self.GetFieldsAsString(I);
end;

function TSQLiteUniTable.FieldIsNull(I: cardinal): boolean;
begin
  Result := Sqlite3_ColumnText(fstmt, i) = nil;
end;

function TSQLiteUniTable.FindField(const AFieldName: string): TSQLiteField;
var
  ix: Integer;
begin
  Result := nil;
  if (fCols.TryGetValue(AnsiUpperCase(AFieldName), ix)) then
  begin
    Result := FFields[ix];
  end;
end;

function TSQLiteUniTable.GetColumns(I: integer): string;
begin
  Result := FColNames[I];
end;

function TSQLiteUniTable.GetField(I: Integer): TSQLiteField;
begin
  Result := FFields[I];
end;

function TSQLiteUniTable.GetFieldByName(const FieldName: string): TSQLiteField;
begin
  Result := GetField(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldByNameAsString(const FieldName: string): string;
begin
  Result := GetFieldsAsString(self.GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldIndex(const FieldName: string): integer;
begin
  if (fCols = nil) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create('Field ' + fieldname + ' Not found. Empty dataset');
    exit;
  end;

  Result := -1;

  if not (fCols.TryGetValue(AnsiUpperCase(FieldName), Result)) then
  begin
    raise ESqliteException.Create('Field not found in dataset: ' + fieldname)
  end;
end;

function TSQLiteUniTable.GetFieldsAsString(I: cardinal): string;
begin
  Result := Sqlite3_ColumnText16(fstmt, i);
end;

function TSQLiteUniTable.GetFieldsVal(I: Cardinal): Variant;
var
  //thisvalue: pstring;
  thistype: integer;
  dt1: TDateTime;
begin
  Result := Unassigned;
  if EOF then
    raise ESqliteException.Create('Table is at End of File');
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
  thistype := FColTypes[I];

  case thistype of
    dtStr:
      begin
        Result := FieldAsString(I);
      end;
    dtInt:
      Result := FieldAsInteger(I);
    dtNumeric:
      Result := FieldAsDouble(I);
    dtBlob:
      Result := FieldAsBlobText(I);
    dtDate:
    begin
      if not TryStrToDate(FieldAsString(I), dt1, fDB.FmtSett) then
        Result := Unassigned
      else
        Result := dt1;
    end;

    dtDateTime:
      if not TryStrToDateTime(FieldAsString(I), dt1, fDB.FmtSett) then
        Result := Unassigned
      else
        Result := dt1;
  else
    Result := Unassigned;
  end;
end;


function TSQLiteUniTable.Next: boolean;
var
  iStepResult: integer;
begin
  fEOF := true;
  iStepResult := Sqlite3_step(fStmt);
  case iStepResult of
    SQLITE_ROW:
      begin
        fEOF := false;
        inc(fRow);
      end;
    SQLITE_DONE:
      // we are on the end of dataset
      // return EOF=true only
      ;
  else
    begin
      SQLite3_reset(fStmt);
      fDB.RaiseError('Could not retrieve data', fSQL);
    end;
  end;
  Result := not fEOF;
end;

procedure TSQLiteUniTable.SetField(I: Integer; const Value: TSQLiteField);
begin
  //
end;

procedure TSQLiteUniTable.SetFieldByName(const FieldName: string; const Value: TSQLiteField);
begin
  {TODO -oLinas -cGeneral : implement update value}
end;


procedure TSQLiteUniTable.SetFields(I: Cardinal; const Value: Variant);
begin
  {TODO -oLinas -cGeneral : finish field update}
end;

{SQliteParam }

constructor TSQliteParam.Create;
begin
  inherited Create;

  index := -1;
end;



{ TSQLitePreparedStatement }

procedure TSQLitePreparedStatement.SetParamDateTime(const I: Integer; const Value: TDateTime);
begin
  SetParamText(I, DateTimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamDate(const I: Integer; const Value: TDate);
begin
  SetParamText(I, DateToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamDate(const name: string; const Value: TDate);
begin
  SetParamText(name, DateToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamDateTime(const name: string; const Value: TDateTime);
begin
  SetParamText(name, DateTimeToStr(Value, FDB.FFormatSett));
end;

procedure TSQLitePreparedStatement.SetParamFloat(const I: Integer; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.valuetype := SQLITE_FLOAT;
  fParams.Add(par);

  par.valuefloat := value;
end;

procedure TSQLitePreparedStatement.SetParamFloat(const name: string; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_FLOAT;
  fParams.Add(par);

  par.valuefloat := value;
end;

procedure TSQLitePreparedStatement.SetParamInt(const name: string; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_INTEGER;
  fParams.Add(par);

  par.valueinteger := value;
end;

procedure TSQLitePreparedStatement.SetParamInt(const I: Integer; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.valuetype := SQLITE_INTEGER;
  fParams.Add(par);

  par.valueinteger := value;
end;

procedure TSQLitePreparedStatement.SetParamNull(const I: Integer);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLitePreparedStatement.SetParams(const Params: array of TVarRec);
var
  i: Integer;
  ext: Extended;
  d: Double;
begin
  for i := Low(Params) to High(Params) do
  begin
    case Params[i].VType of
      vtAnsiString:
      begin
        SetParamText(i + 1, String(Params[i].VAnsiString));
      end;
      vtWideString:
      begin
        SetParamText(i + 1, String(Params[i].VWideString));
      end;
      vtString:
      begin
        SetParamText(i + 1, String(Params[i].VString));
      end;
      vtUnicodeString:
      begin
        SetParamText(i + 1, String(Params[i].VUnicodeString));
      end;
      vtInt64:
      begin
        SetParamInt(i+1, Int64(Params[i].VInt64^));
      end;
      vtInteger:
      begin
        SetParamInt(i+1, Params[i].VInteger);
      end;
      vtExtended:
      begin
        ext := Params[i].VExtended^;
        d := ext;
        SetParamFloat(i+1, d);
      end;
      vtCurrency:
      begin
        ext := Params[i].VCurrency^;
        d := ext;
        SetParamFloat(i+1, d);
      end;
      vtBoolean:
      begin
        SetParamInt(i+1, Integer(Params[i].VBoolean));
      end;
      vtPointer:
      begin
        if Params[i].VPointer = nil then
        begin
          SetParamNull(i+1);
        end;
      end;
      else
      begin
        SetParamNull(i+1);
      end;
    end;
  end;
  BindParams;
end;

procedure TSQLitePreparedStatement.SetParamNull(const name: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLitePreparedStatement.SetParamText(const name, value: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_TEXT;
  fParams.Add(par);
  par.valuedata := utf8Encode(value);
end;

procedure TSQLitePreparedStatement.SetParamText(const I: Integer; const value: string);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.index := I;
  par.valuetype := SQLITE_TEXT;
  fParams.Add(par);

  par.valuedata := utf8Encode(value);
end;

procedure TSQLitePreparedStatement.SetSQL(const Value: string);
begin
  if Value <> FSQL then
  begin
    if (FSQL <> '') and (Value = '') then
      Exit;

    FSQL := Value;
    if BindParameterCount > 0 then
    begin
      ClearParams;
    end;

    if Assigned(FStmt) then
    begin
      Sqlite3_Finalize(FStmt);
      FStmt := nil;
    end;
  end;
end;

procedure TSQLitePreparedStatement.ClearParams;
begin
  FParamsBound := False;
  if FStmt <> nil then
    sqlite3_clear_bindings(FStmt);
  fParams.Clear;
end;

constructor TSQLitePreparedStatement.Create(DB: TSQLiteDatabase; const SQL: string;
  const Params: array of TVarRec);
begin
  Create(DB);

  Assert(SQL <> '', 'SQL cannot be empty');

  FSQL := SQL;

  PrepareStatement(SQL);

  SetParams(Params);
end;

constructor TSQLitePreparedStatement.Create(DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB, SQL, []);
end;

constructor TSQLitePreparedStatement.Create(DB: TSQLiteDatabase);
begin
  inherited Create();
  FDB := DB;
  FSQL := '';
  FStmt := nil;
  FParamsBound := False;
  fParams := TObjectList<TSQliteParam>.Create(True);
end;

destructor TSQLitePreparedStatement.Destroy;
begin
  FDB := nil;
  ClearParams;
  fParams.Free;
  if Assigned(fStmt) then
    Sqlite3_Finalize(fstmt);
  inherited Destroy;
end;

function TSQLitePreparedStatement.ExecQuery(const SQL: string; const Params: array of TVarRec): TSQLiteUniTable;
begin
  Result := nil;

  if FStmt = nil then
  begin
    PrepareStatement(SQL);
  end;

  SetParams(Params);

  {TODO -oLinas -cGeneral : exec query and get resultset}
  Result := TSQLiteUniTable.Create(FDB, FStmt);
end;

function TSQLitePreparedStatement.ExecSQL(const SQL: string; const Params: array of TVarRec): Boolean;
var
  iStepResult: Integer;
begin
  Result := False;
  try
    Self.SQL := SQL;

    if FStmt = nil then
    begin
      PrepareStatement(SQL);
    end;

    SetParams(Params);

    iStepResult := Sqlite3_step(FStmt);
    Result := (iStepResult = SQLITE_DONE);
    if not Result then
    begin
      SQLite3_reset(fstmt);
      FDB.RaiseError('Error executing SQL statement', FSQL);
    end;


  finally
    if Assigned(FStmt) then
    begin
      Sqlite3_Finalize(FStmt);
      FStmt := nil;
      ClearParams;
    end;
  end;
end;

function TSQLitePreparedStatement.ExecSQL(var RowsAffected: Integer): Boolean;
begin
  Result := ExecSQL('', RowsAffected);
end;

function TSQLitePreparedStatement.ExecSQL(const SQL: string; var RowsAffected: Integer): Boolean;
begin
  RowsAffected := 0;
  Result := ExecSQL(SQL);
  if Result then
  begin
    RowsAffected := FDB.GetRowsChanged;
  end;
end;

function TSQLitePreparedStatement.ExecSQL(const SQL: string): Boolean;
begin
  Result := ExecSQL(SQL, []);
end;

function TSQLitePreparedStatement.ExecSQL: Boolean;
begin
  Result := ExecSQL('', []);
end;

function TSQLitePreparedStatement.ExecQuery: TSQLiteUniTable;
begin
  Result := ExecQuery('', []);
end;

function TSQLitePreparedStatement.ExecQuery(const SQL: string): TSQLiteUniTable;
begin
  Result := ExecQuery(SQL, []);
end;

function TSQLitePreparedStatement.ExecSQLAndMapData<T>(var DataList: TObjectList<T>): Boolean;
var
  dst: TSQLiteUniTable;
  obj: TObject;
  ctx: TRttiContext;
  rtype: TRttiType;
  mType: TRTTIMethod;
  props: TArray<TRttiProperty>;
  AProp: TRttiProperty;
  AVal: TValue;
  fld: TSQLiteField;
  metaClass: TClass;
begin
  Result := False;
  if Assigned(DataList) then
  begin
    dst := ExecQuery;
    if Assigned(dst) then
    begin
      try
        ctx := TRttiContext.Create;
        rtype := ctx.GetType(TypeInfo(T));

        props := rtype.GetProperties;
        if Length(props) > 0 then
        begin
          for mType in rType.GetMethods do
          begin
            if mType.HasExtendedInfo and mType.IsConstructor then
            begin
              if Length(mType.GetParameters) = 0 then
              begin
                // invoke
                metaClass := rType.AsInstance.MetaclassType;
                Break;
              end;
            end;
          end;

          while not dst.EOF do
          begin
            obj := mType.Invoke(metaClass, []).AsObject;


            for AProp in props do
            begin
              if (AProp.IsWritable) and
                ( (mvPublic = AProp.Visibility) or (mvPublished = AProp.Visibility)) then
              begin
                fld := dst.FindField(AProp.Name);
                if Assigned(fld) then
                begin
                  AVal := TValue.FromVariant(fld.Value);

                  AProp.SetValue(obj, AVal);
                end;
              end;

            end;

            DataList.Add(obj);

            dst.Next;
          end;
          Result := True;
        end;
      finally
        dst.Free;
      end;
    end;
  end;
end;

function TSQLitePreparedStatement.FindParam(const name: string): TSQliteParam;
var
  par: TSQliteParam;
begin
  Result := nil;
  {TODO -oLinas -cGeneral : don't know if params should be case sensitive}
  for par in fParams do
  begin
    if (par.name = name) then
    begin
      Result := par;
      Break;
    end;
  end;
end;

function TSQLitePreparedStatement.GetParamCount: Integer;
begin
  Result := fParams.Count;
end;

procedure TSQLitePreparedStatement.PrepareStatement(const SQL: string; const Params: array of TVarRec);
begin
  PrepareStatement(SQL);

  SetParams(Params);
end;

function TSQLitePreparedStatement.FindParam(const I: Integer): TSQliteParam;
begin
  Result := nil;

  if (I > 0) and (I <= fParams.Count) then
  begin
    Result := fParams[I-1];
  end;
end;

procedure TSQLitePreparedStatement.PrepareStatement(const SQL: string);
var
  NextSQLStatement: PChar;
begin
  if (SQL <> '') and (SQL <> FSQL) then
    Self.SQL := SQL;

 // if ParamCount > 0 then
 // begin
    if Sqlite3_Prepare16_v2(FDB.fDB, PChar(FSQL), -1, fStmt, NextSQLStatement) <> SQLITE_OK then
      FDB.RaiseError('Error executing SQL', FSQL);
    if (fStmt = nil) then
      FDB.RaiseError('Could not prepare SQL statement', FSQL);
 // end;
 // BindParams;
end;

function TSQLitePreparedStatement.BindParameterCount: Integer;
begin
  if FStmt <> nil then
    Result := sqlite3_bind_parameter_count(FStmt)
  else
    Result := 0;
end;

procedure TSQLitePreparedStatement.BindParams;
var
  n: integer;
  i: integer;
  par: TSQliteParam;
begin
  if (BindParameterCount = fParams.Count) and not (FParamsBound) and (FStmt <> nil) then
  begin
    for n := 0 to fParams.Count - 1 do
    begin
      par := fParams[n];
      if par.index < 1 then
      begin
        i := sqlite3_bind_parameter_index(fStmt, PAnsiChar(Ansistring(par.name)));
      end
      else
      begin
        i := par.index;
      end;


      if i > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(fStmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(fStmt, i, par.valuefloat);
          SQLITE_TEXT:
          begin
            sqlite3_bind_text(fStmt, i, PAnsiChar(par.valuedata),
              -1, SQLITE_TRANSIENT);
            // sqlite3_bind_text16 not working, don't know why
            //I guess it was incorrectly specified sqlite function name, corrected now
            //but our old method works ok so we don't change it now
           // sqlite3_bind_text16(fStmt, i, PChar(par.valuedata),
           //   -1, SQLITE_TRANSIENT);
          end;

          SQLITE_NULL:
            sqlite3_bind_null(fStmt, i);
        end;
      end;
    end;

    FParamsBound := True;
  end;
end;

{ TSQLiteField }

function TSQLiteField.AsBlob: TMemoryStream;
begin
  Result := Table.FieldAsBlob(Index);
end;

function TSQLiteField.AsBlobPtr(out iNumBytes: integer): Pointer;
begin
  Result := Table.FieldAsBlobPtr(Index, iNumBytes);
end;

function TSQLiteField.AsBlobText: string;
begin
  Result := Table.FieldAsBlobText(Index);
end;

function TSQLiteField.AsBlobTextDef(const Def: string): string;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsBlobText;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsDateTime: TDateTime;
begin
  Result := VarToDateTime(Value);
end;

function TSQLiteField.AsDateTimeDef(const Def: TDateTime): TDateTime;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsDateTime;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsDouble: Double;
begin
  Result := Table.FieldAsDouble(Index);
end;

function TSQLiteField.AsDoubleDef(const Def: Double): Double;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsDouble;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsInteger: Int64;
begin
  Result := Table.FieldAsInteger(Index);
end;

function TSQLiteField.AsIntegerDef(const Def: Int64): Int64;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsInteger;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

function TSQLiteField.AsString: string;
begin
  Result := Table.FieldsAsString[Index];
end;

function TSQLiteField.AsStringDef(const Def: string): string;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := AsString;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

constructor TSQLiteField.Create;
begin
  inherited Create();

end;

destructor TSQLiteField.Destroy;
begin
  Name := '';
  inherited Destroy;
end;

function TSQLiteField.IsNull: Boolean;
begin
  Result := Table.FieldIsNull(Index);
end;


function TSQLiteField.Value: Variant;
begin
  Result := Table.FieldsVal[Index];
end;

function TSQLiteField.ValueDef(const Def: Variant): Variant;
begin
  Result := Def;

  if not IsNull then
  begin
    try
      Result := Value;
    except
      //spawn, because we must always return default value
    end;
  end;
end;

initialization
  TSQLiteDatabase.FColumnTypes := TDictionary<string,Integer>.Create(DEF_COLCOUNT);
  TSQLiteDatabase.InitDefaultColumnTypes;

finalization
  TSQLiteDatabase.FColumnTypes.Free;

end.

