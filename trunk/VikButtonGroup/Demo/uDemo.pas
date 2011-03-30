unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ButtonGroup, uSVButtonGroup, ExtCtrls;

type
  TForm1 = class(TForm)
    bgrDemo: TSVButtonGroup;
    Splitter1: TSplitter;
    procedure DoClick(Sender: TObject);
    procedure bgrDemoButtonClicked(Sender: TObject; Index: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bgrDemoButtonClicked(Sender: TObject; Index: Integer);
begin
  DoClick(nil);
end;

procedure TForm1.DoClick(Sender: TObject);
begin
  //
end;

end.
