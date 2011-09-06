unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uSvTypes;

type
  TForm3 = class(TForm)
    ed1: TLabeledEdit;
    lblPath: TLabel;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FPathBuilder: TPathBuilder;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation


{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
begin
  FPathBuilder.Add(ed1.Text);
  lblPath.Caption := FPathBuilder.ToString;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FPathBuilder := TPathBuilder.InitCurrentDir;
  lblPath.Caption := FPathBuilder.ToString;
end;

end.
