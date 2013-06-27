program SQliteDataset;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  SQLite3 in '..\..\Source\SQLite3.pas',
  SQLite3Dataset in '..\..\Source\SQLite3Dataset.pas',
  SQLiteTable3 in '..\..\Source\SQLiteTable3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  ReportMemoryLeaksOnShutdown := True;
  Application.Run;
end.
