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
unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CategoryButtons, ExtCtrls, ComCtrls, Contnrs,
  VirtualTrees, svCollections.GenericTrees, Spin, uVSTSearchEdit;

type
  TTestas = class
  public
    Name: String;
    constructor Create(const AName: string = ''); virtual;
  end;


  TGenericTest = class
  private type
    TGenType<T> = class
    private
      FNode: TSVTreeNode<T>;
    end;

  end;

  TStringArray = array of string;

  TForm1 = class(TForm)
    Memo1: TMemo;
    CategoryButtons1: TCategoryButtons;
    Splitter1: TSplitter;
    pLeft: TPanel;
    vt1: TVirtualStringTree;
    pc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    TreeView1: TTreeView;
    p1: TPanel;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    seCount: TSpinEdit;
    lb1: TLabel;
    VSTSearchEdit1: TVSTSearchEdit;
    edFilter: TButtonedEdit;
    ProgressBar1: TProgressBar;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BuildDefault(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vt1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vt1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vt1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure BuildVirtual(Sender: TObject);
    procedure IterateTree(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure VSTSearchEdit1AfterSearch(ASender: TVSTSearchEdit; ASearchText: string);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    sRand: TStringArray;
    procedure BuildStructure(Count: Integer = 500);
    procedure DoSave(Sender: TSVTree<TTestas>; Node: TSVTreeNode<TTestas>; Stream: TStream);
    procedure DoLoad(Sender: TSVTree<TTestas>; Node: TSVTreeNode<TTestas>; Stream: TStream);
    procedure DoAddIndex(Node: TSVTreeNode<TTestas>);
  public
    { Public declarations }
    FData: TSVTree<TTestas>;
    procedure Log(const AText: string);
  end;

var
  Form1: TForm1;

implementation

uses
  StrUtils,
  usvHelpers,
  Collections.Base;

{$R *.dfm}

const
  ctViso = 1000000;
  ctLocates = 100000;


{build tree}
procedure TForm1.BuildDefault(Sender: TObject);
var
  svNode: TSVTreeNode<TTestas>;
  tc: Cardinal;
  function ProcessNode(Node: TSVTreeNode<TTestas>; Parent: TTreeNode): TTreeNode;
  var
    svNode1: TSVTreeNode<TTestas>;
  begin
    if not Assigned(Node) then
      Exit;

    if node.HasChildren then
    begin
      Result := TreeView1.Items.AddChild(Parent, Node.FValue.Name);

      Result := ProcessNode(Node.FFirstChild, Result);

      ProcessNode(Node.FNextSibling, Parent);
    end
    else
    begin
      Result := TreeView1.Items.AddChild(Parent, Node.FValue.Name);

      ProcessNode(Node.FNextSibling, Parent);
    end;
  end;
begin
  //build tree from structure
  TreeView1.Items.Clear;
  FData.VirtualTree := nil;

  tc := GetTickCount;

  if FData.RootNodeCount < 1 then
  begin
    BuildStructure(seCount.Value);
  end
  else
  begin
    FData.RebuildIndexes(nil);
  end;

  Log(Format('%S %D ms',
      ['Built default structure in', GetTickCount - tc]));


  tc := GetTickCount;
  //print
  TreeView1.Items.BeginUpdate;
  try
    svNode := FData.RootNode.FFirstChild;
    ProcessNode(svNode, nil);
  finally
    TreeView1.Items.EndUpdate;

    Log(Format('%S  - %D ms',
      [TCategoryButtons(Sender).SelectedItem.Caption, GetTickCount - tc]));
  end;
  FData.VirtualTree := vt1;
end;

procedure TForm1.BuildVirtual(Sender: TObject);
var
  tc: Cardinal;
begin
  vt1.Clear;
  FData.VirtualTree := nil;

  FData.OnAfterAddChild := DoAddIndex;

  tc := GetTickCount;
  //building virtual treeview
  if FData.RootNodeCount < 1 then
  begin
    BuildStructure(seCount.Value);
  end
  else
  begin
    FData.RebuildIndexes(nil);
  end;

  Log(Format('%S %D ms',
      ['Built structure in', GetTickCount - tc]));

  tc := GetTickCount;


  vt1.RootNodeCount := FData.RootNodeCount;

  Log(Format('%S  - %D ms. TotalCount: %D',
      [TCategoryButtons(Sender).SelectedItem.Caption, GetTickCount - tc,
      vt1.TotalCount]));

  FData.VirtualTree := vt1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  VSTSearchEdit1.RunSearch;
end;

procedure TForm1.DoAddIndex(Node: TSVTreeNode<TTestas>);
begin
// event on add new child
end;

procedure TForm1.DoLoad(Sender: TSVTree<TTestas>; Node: TSVTreeNode<TTestas>; Stream: TStream);
var
  obj: TTestas;
begin
  //
  if Assigned(Node) then
  begin
    //read from stream
    obj := TTestas.Create(Stream.AsString);
    Node.FValue := obj;
  end;
end;

procedure TForm1.DoSave(Sender: TSVTree<TTestas>; Node: TSVTreeNode<TTestas>; Stream: TStream);
var
  sVal: string;
begin
  //
  if Assigned(Node) then
  begin
    //read from stream
    if Assigned(Node.FValue) then
    begin
      sVal := TTestas(Node.FValue).Name;

      Stream.WriteString(sVal);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  FData := TSVTree<TTestas>.Create(True, 1000000);
  FData.VirtualTree := vt1;
  sRand := TStringArray.Create('VIienas', 'kažkas bėga per kelią', 'bla bla bla45',
  'belekas', 'jobtvojmat', 'nu zajac, nu pagady', 'jnebeprisikiškiakopūsteliadamasis',
  'kapiec bus ', '168979464 877846', 'Aefef sfkwoper ', 'asas ORWEUHWsksdmqopdj sdps');


  VSTSearchEdit1.Edit := edFilter;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

procedure TForm1.IterateTree(Sender: TObject);
var
  tc: Cardinal;
  ix: Integer;
begin
  tc := GetTickCount;
  ix := 0;
  FData.IterateSubtree(nil, procedure(Node: TSVTreeNode<TTestas>; Data: Pointer; var Abort: Boolean)
  begin
    Inc(ix);
  end);

  Log(Format('%S  - %D ms. %D items. Total: %D. VT Total: %D',
      [TCategoryButtons(Sender).SelectedItem.Caption, GetTickCount - tc, ix, FData.TotalCount, VT1.TotalCount]));
end;

procedure TForm1.Log(const AText: string);
begin
  Memo1.Lines.Add(AText);
end;

procedure TForm1.VSTSearchEdit1AfterSearch(ASender: TVSTSearchEdit; ASearchText: string);
begin
  Log(ASearchText);
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  fNode: PVirtualNode;
  sNode: TSVTreeNode<TTestas>;
begin
  fNode := Vt1.FocusedNode;
  if Assigned(fNode) then
  begin
    sNode := FData.GetNode(fNode);
    if Assigned(sNode) then
    begin
      FData.DeleteNode(sNode);
    end;
  end;
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  FData.Clear;
end;

procedure TForm1.btn4Click(Sender: TObject);
var
  fNode: PVirtualNode;
  sNode: TSVTreeNode<TTestas>;
begin
  fNode := Vt1.FocusedNode;
  if Assigned(fNode) then
  begin
    sNode := FData.GetNode(fNode);
    if Assigned(sNode) then
    begin
      FData.AddChild(sNode, TTestas.Create('New Node'));
    end;
  end;
end;

procedure TForm1.btn5Click(Sender: TObject);
var
  tc: Cardinal;
begin
  FData.OnSaveNode := DoSave;
  tc := GetTickCount;
  FData.SaveToFile('FData.dat');
  Log(Format('%S %D ms',
      ['Save to file in ', GetTickCount - tc]));
end;

procedure TForm1.btn6Click(Sender: TObject);
var
  tc: Cardinal;
begin
  //load
  FData.OnLoadNode := DoLoad;
  tc := GetTickCount;
  FData.LoadFromFile('FData.dat');
  Log(Format('%S %D ms',
    ['Load from file in ', GetTickCount - tc]));
end;

procedure TForm1.btn7Click(Sender: TObject);
begin
  Log('Not Implemented');
end;

procedure TForm1.BuildStructure(Count: Integer);
const
  ct1Level = 5000;
  ct2Level = 10;
  ct3Level = 5;
var
  i, j, iRootCount: Integer;
  svNode, svNode2: TSVTreeNode<TTestas>;
  Local_j: Integer;
  obj: TTestas;
begin
  iRootCount := Count;
  FData.BeginUpdate;
  try
    for i := 0 to iRootCount - 1 do
    begin
      obj := TTestas.Create;
      obj.Name := Format('Item %D', [i]);
      svNode := FData.AddChild(nil, obj);
      for Local_j := 0 to ct2Level - 1 do
      begin
        obj := TTestas.Create;
        obj.Name := Format('Child %D', [Local_j]);
        svNode2 := FData.AddChild(svNode, obj);
        for j := 0 to ct3Level - 1 do
        begin
          obj := TTestas.Create;
          obj.Name := Format('SubChild %D', [j]);
          FData.AddChild(svNode2, obj);
        end;
      end;
    end;
  finally
    FData.EndUpdate;
  end;
end;

procedure TForm1.vt1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  svNode: TSVTreeNode<TTestas>;
begin
  svNode := FData.GetNode(Node);
  if Assigned(svNode) then
  begin
    if Assigned(svNode.FValue) then
    begin
      CellText := svNode.FValue.Name + Format(' (%D) [%D] index: %D',
        [svNode.FTotalCount, Node.TotalCount, Node.Index]);
    end
    else
    begin
      CellText := Format('Total [%D] Index: %D',
        [Node.TotalCount, Node.Index]);
    end;
  end;
end;

procedure TForm1.vt1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  svNode: TSVTreeNode<TTestas>;
begin
  //  svNode := FData.GetNode(Node);
  svNode := FData.GetNode(Sender.GenerateIndex(Node));
  if Assigned(svNode) then
  begin
    ChildCount := svNode.FChildCount;
  end;
end;

procedure TForm1.vt1InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  svNode: TSVTreeNode<TTestas>;
begin
  // if our VirtualTree assigned to TSVTree<TTestas> structure, then we can use
  // GetNode(Node: PVirtualNode) function
  // otherwise we must use GetNode(Sender.GenerateIndex(Node)).
  //Note. GetNode(Sender.GenerateIndex(Node)) will work all the time

//  svNode := FData.GetNode(Node);
  svNode := FData.GetNode(Sender.GenerateIndex(Node));
  if Assigned(svNode) then
  begin
    //if TSVTree<TTestas> is synced with Virtual Treeview and we are building tree by
    // setting RootNodeCount, then we must set svNode.FVirtualNode := Node to
    // have correct node references
    svNode.FVirtualNode := Node;  // Don't Forget!!!!
    if svNode.HasChildren then
    begin
      Include(InitialStates, ivsHasChildren);
    end;
  end;
end;

{ TTestas }

constructor TTestas.Create(const AName: string);
begin
  Name := AName;
end;

end.
