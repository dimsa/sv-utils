(*
* Copyright (c) 2011, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit uFactory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SvDesignPatterns, ComCtrls, StdCtrls, ExtCtrls, uSvTypes;

type
  TFactoryEnum = (feOne, feTwo, feThree);

  TDemoEnum = TSvEnum<TFactoryEnum>;

  IDemo = interface
    function DemoType: TDemoEnum;
  end;

  TDemo = class(TInterfacedObject, IDemo)
  private
    FDemoType: TDemoEnum;
  public
    constructor Create(const ADemoType: TDemoEnum);

    function DemoType: TDemoEnum;
  end;

  TfrmUI = class(TForm)
    btn1: TButton;
    rgTypes: TRadioGroup;
    mmoLog: TMemo;
    btn2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DoCreateObjects(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
    Factory: TFactory<TDemoEnum,IDemo>;
    Multiton: TMultiton<TDemoEnum,IDemo>;
    ManagedMultiton: TMultiton<TDemoEnum,TDemo>;
    ButtonFactory: TFactory<string,TButton>;

    procedure FillItems();
  public
    { Public declarations }
  end;

var
  frmUI: TfrmUI;

implementation

uses
  TypInfo,
  Generics.Collections;

{$R *.dfm}

procedure TfrmUI.btn2Click(Sender: TObject);
begin
  ButtonFactory.GetInstance(btn2.Name);
end;

procedure TfrmUI.DoCreateObjects(Sender: TObject);
var
  obj: IDemo;
  fmethod: TPair<TDemoEnum,IDemo>;
  ix: Integer;
begin
  ix := 1;
  //for our enum type we can pass even integer values
  obj := Factory.GetInstance(rgTypes.ItemIndex);

  mmoLog.Lines.Add(Format('Got instance of type: %S',
    [obj.DemoType.ToString]));

  mmoLog.Lines.Add('Start enumerating registered TDemo factory methods:');
  //enumerate
  for fmethod in Factory do
  begin
    obj := fmethod.Value;
    if Assigned(obj) then
    begin
      mmoLog.Lines.Add(Format('%D. Instance of type: %S',
        [ix, obj.DemoType.ToString]));
    end;
    Inc(ix);
  end;
  mmoLog.Lines.Add('*********************************');
end;

procedure TfrmUI.FillItems;
var
  i: Integer;
begin
  for i := Ord(Low(TFactoryEnum)) to Ord(High(TFactoryEnum)) do
  begin
    rgTypes.Items.Add(GetEnumName(TypeInfo(TFactoryEnum), i));
  end;

  rgTypes.ItemIndex := 0;
end;

procedure TfrmUI.FormCreate(Sender: TObject);
var
  i: Integer;
  obj: IDemo;
  obj2: TDemo;
  pair: TPair<TDemoEnum,IDemo>;
  pair2: TPair<TDemoEnum,TDemo>;
begin
  Factory := TFactory<TDemoEnum,IDemo>.Create;

  Factory.RegisterFactoryMethod(feOne,
    function: IDemo
    begin
      Result := TDemo.Create(feOne);
    end);

  Factory.RegisterFactoryMethod(feTwo,
    function: IDemo
    begin
      Result := TDemo.Create(feTwo);
    end);

  Factory.RegisterFactoryMethod(feThree,
    function: IDemo
    begin
      Result := TDemo.Create(feThree);
    end);

  Multiton := TMultiton<TDemoEnum,IDemo>.Create;

  Multiton.RegisterFactoryMethod(feOne,
    function: IDemo
    begin
      Result := TDemo.Create(feOne);
    end);

  Multiton.RegisterFactoryMethod(feTwo,
    function: IDemo
    begin
      Result := TDemo.Create(feTwo);
    end);

  Multiton.RegisterFactoryMethod(feThree,
    function: IDemo
    begin
      Result := TDemo.Create(feThree);
    end);

  //get instance 10 times. Multiton will call factory method only first time
  for i := 1 to 10 do
  begin
    obj := Multiton.GetInstance(feOne);
  end;
  //test foreach loop
  for pair in Multiton do
  begin
    obj := pair.Value;
  end;

  ManagedMultiton := TMultiton<TDemoEnum,TDemo>.Create(True);
  ManagedMultiton.RegisterFactoryMethod(feOne,
    function: TDemo
    begin
      Result := TDemo.Create(feOne);
    end);

  ManagedMultiton.RegisterFactoryMethod(feTwo,
    function: TDemo
    begin
      Result := TDemo.Create(feTwo);
    end);
  ManagedMultiton.RegisterFactoryMethod(feThree,
    function: TDemo
    begin
      Result := TDemo.Create(feThree);
    end);

  //test foreach loop
  for pair2 in ManagedMultiton do
  begin
    obj2 := nil;
    obj2 := pair2.Value;
    Assert(Assigned(obj2));
  end;

  FillItems();

  ButtonFactory := TFactory<string,TButton>.Create;

  ButtonFactory.RegisterFactoryMethod(btn2.Name,
    function: TButton
    begin
      Result := btn2;
      Result.Caption := 'Random: ' + IntToStr(Random(1000));
    end);
end;

procedure TfrmUI.FormDestroy(Sender: TObject);
begin
  Factory.Free;
  ButtonFactory.Free;
  Multiton.Free;
  ManagedMultiton.Free;
end;

{ TDemo }

constructor TDemo.Create(const ADemoType: TDemoEnum);
begin
  inherited Create();
  FDemoType := ADemoType;
end;

function TDemo.DemoType: TDemoEnum;
begin
  Result := FDemoType;
end;

end.
