program SvSerializerTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSvSerializerJsonFactory in 'TestSvSerializerJsonFactory.pas',
  SvSerializerJsonFactory in '..\SvSerializerJsonFactory.pas',
  SvSerializer in '..\SvSerializer.pas',
  SvSerializerXMLFactory in '..\SvSerializerXMLFactory.pas',
  SvSerializer.Extensions.SQLite in '..\SvSerializer.Extensions.SQLite.pas',
  SQLiteTable3 in '..\..\SQLite3\Source\SQLiteTable3.pas',
  SQLite3 in '..\..\SQLite3\Source\SQLite3.pas';

{$R *.RES}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

