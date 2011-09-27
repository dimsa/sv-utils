program PreparedStatementDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SQLiteTable3;


procedure Demo;
var
  slDBpath: string;
  sldb: TSQLiteDatabase;
  db: TSQLiteDatabase;
  pstm, pstm2: TSQLitePreparedStatement;
  ID: Int64;
begin
  //use db file from first demo
  //you must execute first demo for this file to be created!
  slDBpath := IncludeTrailingPathDelimiter(GetHomePath) + IncludeTrailingPathDelimiter('Test') + 'test.db';
  if not ForceDirectories(ExtractFileDir(slDBpath)) then
  begin
    WriteLn('Cannot create path: ' + slDBpath);
  end;

  db := TSQLiteDatabase.Create(slDBpath);
  try
    pstm := TSQLitePreparedStatement.Create(db);
    //test array of const
    pstm2 := TSQLitePreparedStatement.Create(db,
      'insert into testtable (name,number) values (?,?)',
      ['NewRec', 99.99]);

    pstm2.ExecSQL;

    ID := db.GetLastInsertRowID;
    Writeln(ID);

    //test reusing same prepared statement
    pstm2.PrepareStatement('update testtable set name = ?, number = ? where ID = ?',
      ['UpdatedRec', nil, ID]);

    pstm2.ExecSQL;

    Writeln('Updated table with prepared statement');

    pstm.PrepareStatement('update testtable set name = ?, number = ? where ID = ?');

    pstm.SetParamFloat(2, 1.111);
    pstm.SetParamInt(3, 2);
    pstm.SetParamText(1, 'Some Name Русский 1');

    pstm.ExecSQL;

    Writeln('Updated table with prepared statement');

    pstm.SetParamText(1, 'Some Name Русский 0');
    pstm.SetParamNull(2);
    pstm.SetParamInt(3, 1);

    pstm.ExecSQL;

    Writeln('Updated table with prepared statemnt');

  finally
    pstm.Free;
    pstm2.Free;
    db.Free;
  end;
end;

begin
  try
    Demo;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
