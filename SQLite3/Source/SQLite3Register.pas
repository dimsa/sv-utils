unit SQLite3Register;

interface

procedure Register;

implementation

uses
  Classes, SQLite3Dataset;

procedure Register;
begin
  RegisterComponents('SQLite3',
    [TSQLiteDataset, TSQLiteUpdateSQL]);
end;

end.
