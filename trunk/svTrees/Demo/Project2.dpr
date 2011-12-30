// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Project2;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form1},
  svCollections.GenericTrees in 'svCollections.GenericTrees.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
