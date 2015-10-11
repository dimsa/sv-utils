No need to free sqlite prepared statements or tables when getting them as interfaces:

# Simple data loading using unidirectional table #

```
procedure TestTSQLiteDatabase.TestSimpleDataLoad;
var
  DB: TSQLiteDatabase;
  stmt: ISQLitePreparedStatement;
  dst: ISQLiteTable;
  iVal: Integer;
  ARec: Variant;
begin
  DB := TSQLiteDatabase.Create('test.db');
  try
    //prepare statement with param value = 10
    stmt := DB.GetPreparedStatementIntf('select * from testtable where ID > ?', [10]);
      //execute query and return the resultset
    dst := stmt.ExecQueryIntf;
    while not dst.EOF do
    begin
      //get data from resultset
      iVal := dst['ID'].AsInteger;

      dst.Next;
    end;
    //or you can use For each loop
    dst := stmt.ExecQueryIntf;
    for ARec in dst do
    begin
      iVal := ARec.ID; //get value from the field name ID
    end;
  finally
    DB.Free;
  end;
end;
```