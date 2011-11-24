unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSvStrings, StdCtrls, Diagnostics;

type
  TForm3 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    edtText: TEdit;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
    sw: TStopwatch;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
var
  s, s2: TSvString;
  str: string;
begin
  s := edtText.Text;
  s2 := 'zzz';

  str := 'aaa';

  s := s + s2;

  mmo1.Lines.Add(s)
end;

procedure TForm3.btn2Click(Sender: TObject);
var
  i: Integer;
  s: string;
  st: TSvString;
begin
  s := '';
  st := '';
  sw := TStopwatch.StartNew;
  //speed test
  for i := 0 to 100000 - 1 do
  begin
    s := s + 'a';
  end;

  sw.Stop;

  mmo1.Lines.Add(Format('Native string: %D ms',[sw.ElapsedMilliseconds]));

  sw := TStopwatch.StartNew;
  //speed test
  for i := 0 to 100000 - 1 do
  begin
    st := st + 'a';
  end;

  sw.Stop;

  mmo1.Lines.Add(Format('TString +: %D ms',[sw.ElapsedMilliseconds]));

  sw := TStopwatch.StartNew;

  st := '';
  //speed test
  for i := 0 to 100000 - 1 do
  begin
    st.Concat('a');
  end;

  sw.Stop;

  mmo1.Lines.Add(Format('TString.Concat: %D ms',[sw.ElapsedMilliseconds]));

end;

end.
