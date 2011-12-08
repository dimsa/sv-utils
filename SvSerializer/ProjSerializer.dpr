program ProjSerializer;

uses
  Forms,
  uGui in 'uGui.pas' {Form1},
  SvSerializer in 'SvSerializer.pas',
  SvSerializerJsonFactory in 'SvSerializerJsonFactory.pas',
  SvSerializerXMLFactory in 'SvSerializerXMLFactory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
