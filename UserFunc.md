# Implementation of SQLite3 User functions #
Suppose you have SQLite3 database opened. Examples:
## Adding simple scalar functions: ##

```
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
        //result is 5
        Check(iVal = 5);
      finally
        tbl.Free;
      end;

    end;
  finally
    stmt.Free;
  end;
end;
```

## Adding aggregate functions: ##
```
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
        //our aggregate function TestSum returns equal value just as internal Sum function. 
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
```