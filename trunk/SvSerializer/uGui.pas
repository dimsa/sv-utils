unit uGui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SvSerializer, StdCtrls, Generics.Collections;

type
  TDemoEnum = (deOne, deTwo);

  TMyRec = record
  private
    FString: string;
    FInt: Integer;
  public
    property AInt: Integer read FInt write FInt;
    property AString: string read FString write FString;
  end;

  IDemoObj = interface
  ['{045BBB1C-89AC-4FAF-9B60-14DC79CCB5FC}']
    function GetName: string;
    procedure SetName(const Value: string);
    [SvSerialize]
    property Name: string read GetName write SetName;
  end;

  TDemoObj = class(TInterfacedObject, IDemoObj)
  private
    FName: string;
    FTag: Integer;
    FDate: TDateTime;
    FEnumas: TDemoEnum;
    FMas: TArray<string>;
    FValue: Variant;
    FIsValid: Boolean;
    FList: TStrings;
    FList2: TList<Integer>;
    FMyRec: TMyRec;
    FColor: TColor;
    FFont: TFont;
    FIntf: IDemoObj;
    FMeth: TProc;
    function GetName: string;
    procedure SetName(const Value: string);
  public
    constructor Create();
    destructor Destroy; override;
    [SvSerialize]
    property Name: string read GetName write SetName;
    [SvSerialize]
    property Tag: Integer read FTag write FTag;
    [SvSerialize]
    property Date: TDateTime read FDate write FDate;
    [SvSerialize]
    property Enumas: TDemoEnum read FEnumas write FEnumas;
    [SvSerialize]
    property Mas: TArray<string> read FMas write FMas;
    [SvSerialize]
    property Value: Variant read FValue write FValue;
    [SvSerialize]
    property IsValid: Boolean read FIsValid write FIsValid;
    [SvSerialize]
    property List: TStrings read FList write FList;
    [SvSerialize]
    property List2: TList<Integer> read FList2 write FList2;
    [SvSerialize]
    property MyRec: TMyRec read FMyRec write FMyRec;
    [SvSerialize]
    property Color: TColor read FColor write FColor;
    [SvSerialize('MyFont')]
    property Font: TFont read FFont write FFont;
   // [SvSerialize]
    property Intf: IDemoObj read FIntf write FIntf;
    [SvSerialize]
    property Meth: TProc read FMeth write FMeth;
  end;

  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    mmo1: TMemo;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
  private
    { Private declarations }
    FSerial: TSvSerializer;
    FObj, FObj2: TDemoObj;
    Fintf: IDemoObj;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

{ TDemoObj }

constructor TDemoObj.Create;
begin
  FName := 'Demo';
  Tag := 100;
  Date := Now;
  Enumas := deTwo;

  FMas := TArray<string>.Create('Pirmas', 'Antras', 'Trecias');
  FValue := Unassigned;
  FIsValid := False;

  FList := TStringList.Create;
  FList.Add('First');
  FList.Add('Second');
  FList.Add('Third');

  FList2 := TList<Integer>.Create;

  FFont := TFont.Create;
  FFont.Name := 'Tahoma';

  FIntf := nil;
end;

destructor TDemoObj.Destroy;
begin
  FList.Free;
  FList2.Free;
  FFont.Free;
  inherited;
end;

function TDemoObj.GetName: string;
begin
  Result := FName;
end;

procedure TDemoObj.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  FObj.Mas[0] := 'Pakeistas';
  FObj.IsValid := True;

  FObj.List2.AddRange([1,2,3,4,5,6,7,8,9,10]);

  FObj.FMyRec.FInt := 555;
  FObj.FMyRec.FString := 'Record string';
  FObj.Color := clRed;
  FObj.Font.Name := 'Verdana';
  FObj.Intf.Name := 'Interface';
//  FObj.Value := 1568;
  FObj.Meth := procedure begin ShowMessage('Works') end;

  FSerial.Serialize('Config.json');
  Caption := 'Foobar';
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  i: Integer;
begin
  FObj.Name := '';
  FObj.Tag := 0;
  FObj.Date := MinDateTime;

  FObj2.Name := '';
  FObj2.Tag := 0;
  FObj2.Date := MinDateTime;

  FObj.FMyRec.AInt := -1;
  FObj.FMyRec.AString := '';


  FSerial.DeSerialize('Config.json');

  mmo1.Lines.Add(Format('FObj Name: %S',[FObj.Name]));
  mmo1.Lines.Add(Format('FObj2 Name: %S',[FObj2.Name]));

  for i := 0 to Length(FObj.Mas)-1 do
  begin
    mmo1.Lines.Add(Format('FObj Mas:[%D]  %S',[i, FObj.Mas[i]]));
  end;

  mmo1.Lines.Add(Format('FObj IsValid: %S',[BoolToStr(FObj.IsValid, True)]));

  for i := 0 to FObj.List.Count - 1 do
  begin
    mmo1.Lines.Add(Format('FObj List: [%D] %S',[i ,FObj.List[i]]));
  end;

  for i := 0 to FObj.List2.Count - 1 do
  begin
    mmo1.Lines.Add(Format('FObj List2: [%D] %D',[i ,FObj.List2[i]]));
  end;

  mmo1.Lines.Add(Format('FObj MyRec.Fint: %D',[FObj.MyRec.AInt]));
  mmo1.Lines.Add(Format('FObj MyRec.FString: %S',[FObj.MyRec.AString]));

  mmo1.Lines.Add(Format('FObj MyRec.Color: %S',[ColorToString(FObj.Color)]));

  mmo1.Lines.Add(Format('FObj MyRec.Font.Name: %S',[Fobj.Font.Name]));
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  FSerial.Marshall<TDemoObj>(FObj, 'Marshall.json');
end;

procedure TForm1.btn4Click(Sender: TObject);
var
  obj: TDemoObj;
begin
  obj := nil;
  obj := FSerial.UnMarshall<TDemoObj>('Marshall.json');
  if Assigned(obj) then
  begin
    mmo1.Lines.Add(Format('Obj Name: %S',[Obj.Name]));
    mmo1.Lines.Add(Format('Obj IsValid: %S',[BoolToStr(Obj.IsValid, True)]));

    obj.Free;
  end;
end;

procedure TForm1.btn5Click(Sender: TObject);
var
  obj: TDemoObj;
begin
  obj := TSvSerializer.CreateType<TDemoObj>;

  if Assigned(obj) then
  begin
    mmo1.Lines.Add(Format('Obj Name: %S',[Obj.Name]));
    mmo1.Lines.Add(Format('Obj IsValid: %S',[BoolToStr(Obj.IsValid, True)]));
    obj.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSerial := TSvSerializer.Create();
  FObj := TDemoObj.Create;
  FObj2 := TDemoObj.Create;
  FObj2.Name := '2 Demo[1]';
  FObj2.Tag := 200;
  FObj2.Enumas := deOne;
  FObj2.Date := MinDateTime;


  FSerial.AddObject('FSerial', FObj);
  FSerial.AddObject('FObj2', FObj2);

  Caption := 'New Text';
  FSerial.AddObjectCustomProperties('Forma', Self, ['Caption', 'WindowState']);

  Fintf := TDemoObj.Create;
  FObj.Intf := Fintf;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FObj.Free;
  FObj2.Free;
  FSerial.Free;
end;

end.
