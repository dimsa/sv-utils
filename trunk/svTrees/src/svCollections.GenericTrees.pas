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

unit svCollections.GenericTrees;

interface

uses
  SysUtils, Classes, Generics.Collections, VirtualTrees;

type
  TSVTree<T> = class;
  /// <summary>
  /// Main tree node class
  /// T - type of node's custom data object
  /// </summary>
  TSVTreeNode<T> = class
  public
    FValue: T; //user data pointer
    FParent: TSVTreeNode<T>;  // reference to the node's parent (for the root this contains the TSVTree)
    FVirtualNode: PVirtualNode; //if trees are synchronized, we keep virtual node reference here
    FNodeIndex: Integer;  //index of node with regard to its parent
    FTotalCount: Cardinal; // sum of this node, all of its child nodes and their child nodes etc.
    FChildCount: Cardinal; //number of child nodes
    FLastChild: TSVTreeNode<T>; // link to the node's last child...
    FFirstChild: TSVTreeNode<T>; // link to the node's first child...
    FNextSibling: TSVTreeNode<T>; // link to the node's next sibling or nil if it is the last node
    FPrevSibling: TSVTreeNode<T>; // link to the node's previous sibling or nil if it is the first node
    //functions
    function HasChildren: Boolean;
    function HasParent: Boolean;
    function GetOwner: TSVTree<T>;
    /// <summary>
    /// SVTreeFromNode
    ///  Get the tree given node belongs to.
    /// </summary>
    /// <param name="Node">Given node</param>
    /// <returns>TSVTree object or nil if doesn't exist</returns>
    class function SVTreeFromNode(Node: TSVTreeNode<T>): TSVTree<T>;
  end;


    // base information about a node
  TSVBaseChunkBody = packed record
    ChildCount: Cardinal;
  //  States: TVirtualNodeStates;
   // Align: Byte;
   // CheckState: TCheckState;
   // CheckType: TCheckType;
  //  Reserved: Cardinal;
  end;

  TSVChunkHeader = record
    ChunkType,
    ChunkSize: Integer;      // contains the size of the chunk excluding the header
  end;



  TSVBaseChunk = packed record
    Header: TSVChunkHeader;
    Body: TSVBaseChunkBody;
  end;

 // TSVTreeNode = TSVTreeNode;

  TSVMagicID = array[0..5] of WideChar;

  ProcOnFreeNode<T> = procedure(Node: TSVTreeNode<T>) of object;
  ProcOnSaveNode<T> = procedure(Sender: TSVTree<T>; Node: TSVTreeNode<T>; Stream: TStream) of object;
  ProcSaveTree<T> = procedure(Sender: TSVTree<T>; Stream: TStream) of object;
  ProcAfterAddNode<T> = procedure(Node: TSVTreeNode<T>) of object;

  TIterCallBack<T> = reference to procedure(Node: TSVTreeNode<T>; Data: Pointer; var Abort: Boolean);

  /// <summary>
  /// Class helper for more OO VirtualTree index generation
  /// </summary>
  TSVBaseVirtualTreeHelper = class helper for TBaseVirtualTree
  public
    function GenerateIndex(Node: PVirtualNode): RawByteString; inline;
    function GetSVNode<T>(Node: PVirtualNode; SVTree: TSVTree<T>): TSVTreeNode<T>; inline;
  end;

  /// <summary>
  /// TSVTree Base Class
  /// T - type of node's custom data object
  /// </summary>
  TSVTree<T> = class
  private
    FFreeValues: Boolean;
    FRoot: TSVTreeNode<T>;
    FMainIndex: TDictionary<RawByteString, TSVTreeNode<T>>;  //main index
    FOnFreeNode: ProcOnFreeNode<T>;
    FOnSaveNode: ProcOnSaveNode<T>;
    FVirtualTree: TVirtualStringTree;
    FUpdateCount: Integer;
    FOnLoadNode: ProcOnSaveNode<T>;
    FOnSaveTree: ProcSaveTree<T>;
    FOnLoadTree: ProcSaveTree<T>;
    FCapacity: Integer;
    FAfterAddChild: ProcAfterAddNode<T>;
    function GetRootNodeCount: Cardinal; inline;
    procedure AdjustTotalCount(Node: TSVTreeNode<T>; Value: Integer; Relative: Boolean = False); inline;
    function GetTotalCount: Cardinal; inline;
    procedure FreeRoot;
    procedure InitRootNode;
    procedure FixupTotalCount(Node: TSVTreeNode<T>); deprecated;
    function GetIsEmpty: Boolean;
    procedure SetIsEmpty(const Value: Boolean);
  protected
    procedure InternalDeleteNode(Node: TSVTreeNode<T>; DoReindex: Boolean = True);
    function InitNode(ParentNode: TSVTreeNode<T>; Data: T): TSVTreeNode<T>; inline;
    procedure WriteNode(Stream: TStream; Node: TSVTreeNode<T>); virtual;
    procedure WriteChunks(Stream: TStream; Node: TSVTreeNode<T>); virtual;
    procedure FinishChunkHeader(Stream: TStream; StartPos, EndPos: Integer); virtual;
    procedure DoSaveUserData(Node: TSVTreeNode<T>; Stream: TStream); virtual;
    procedure DoLoadUserData(Node: TSVTreeNode<T>; Stream: TStream); virtual;
    procedure ReadNode(Stream: TStream; Version: Integer; Node: TSVTreeNode<T>); virtual;
    procedure InternalAddFromStream(Stream: TStream; Version: Integer; Node: TSVTreeNode<T>); virtual;
    function ReadChunk(Stream: TStream; Version: Integer; Node: TSVTreeNode<T>; ChunkType,
      ChunkSize: Integer): Boolean; virtual;
    procedure ProcessNode(ANode: TSVTreeNode<T>; Regenerate: Boolean);
    procedure ProcessNodeAddIndex(ANode: TSVTreeNode<T>);
  public
    /// <summary>
    /// Constructor
    ///  Set ACapacity to number of items count you probably will have. It greatly increases performance.
    ///  Higher value = better performance = higher initial memory usage
    /// </summary>
    constructor Create(OwnsObjects: Boolean = True; ACapacity: Integer = 0); overload;
    destructor Destroy; override;
        //functions
    /// <summary>
    /// Adds a new node to the given parent node
    /// </summary>
    /// <param name="Parent">Parent node. If nil, node will be added to the root</param>
    /// <param name="NodeData">Node data pointer to store additional data in the node</param>
    /// <returns>Newly added node</returns>
    function AddChild(Parent: TSVTreeNode<T>; NodeData: T): TSVTreeNode<T>; inline;
    /// <summary>
    /// Removes given node from the tree
    /// </summary>
    /// <param name="Node">Node to remove</param>
    /// <param name="DoReindex">True: adjusts index, refreshes totalcounts.
    ///  False: useful only when entire tree is being cleared</param>
    /// <returns>True: succeded</returns>
    procedure DeleteNode(Node: TSVTreeNode<T>; DoReindex: Boolean = True);
    /// <summary>
    /// Removes given node's all children except the given node
    /// </summary>
    /// <param name="Node">Given node</param>
    /// <param name="DoReindex">Default: true. Use false only when clearing tree</param>
    procedure DeleteChildren(Node: TSVTreeNode<T>; DoReindex: Boolean);
    /// <summary>
    /// Checks if node has at least one single child
    /// </summary>
    /// <param name="Node">Node to check</param>
    /// <returns>True if success</returns>
    function HasChildren(Node: TSVTreeNode<T>): Boolean; inline;
    /// <summary>
    /// Checks if node has a valid parent
    /// </summary>
    /// <param name="Node">Node to check</param>
    /// <returns>True if valid parent node. False: Parent is RootNode</returns>
    function HasParent(Node: TSVTreeNode<T>): Boolean; inline;
    /// <summary>
    /// Gets node data from the given index
    /// </summary>
    /// <param name="sNodeIndex">Index string. Structure: CurrentIndex/HigherLevelIndex/HigherLevelIndex..., etc.</param>
    /// <returns>nil = not found, else returns founded node</returns>
    function GetNode(const sNodeIndex: RawByteString): TSVTreeNode<T>; overload; inline;
    function GetNode(Node: PVirtualNode): TSVTreeNode<T>; overload; inline;
    function AbsoluteIndex(Node: TSVTreeNode<T>): Integer;
    function IsRootNode(Node: TSVTreeNode<T>): Boolean; inline;
    /// <summary>
    /// Generates unique index for using with GetNode.
    /// </summary>
    /// <param name="Node">Node to use</param>
    /// <returns>index string, empty string if node is unassigned</returns>
    function GenerateIndex(Node: TSVTreeNode<T>): RawByteString; inline;
    /// <summary>
    /// Iterates recursively through all the children of the given node
    /// </summary>
    /// <param name="Node">Node to start</param>
    /// <param name="ACallbackProc">Anonymous callback procedure which is called while travelling on each node </param>
    /// <param name="Data">Custom data to be used in callback</param>
    /// <returns>Last used node</returns>
    function IterateSubtree(Node: TSVTreeNode<T>; const ACallbackProc: TIterCallback<T>; Data: Pointer = nil): TSVTreeNode<T>;

    //node get'ers
    function GetFirst: TSVTreeNode<T>; inline;
    function GetFirstLevel(NodeLevel: Integer): TSVTreeNode<T>;
    function GetNext(Node: TSVTreeNode<T>): TSVTreeNode<T>;
    function GetNextLevel(Node: TSVTreeNode<T>; NodeLevel: Integer): TSVTreeNode<T>;
    function GetPrevious(Node: TSVTreeNode<T>): TSVTreeNode<T>;
    function GetPreviousLevel(Node: TSVTreeNode<T>; NodeLevel: Integer): TSVTreeNode<T>;
    function GetLast(Node: TSVTreeNode<T>): TSVTreeNode<T>;
    function GetLastChild(Node: TSVTreeNode<T>): TSVTreeNode<T>;

    function GetNodeLevel(Node: TSVTreeNode<T>): Integer;
    //procedures
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure SaveToStream(Stream: TStream; Node: TSVTreeNode<T>);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: TFileName);
    procedure LoadFromFile(const FileName: TFileName);


    /// <summary>
    /// If virtual tree was cleared, we must rebuild our indexes
    ///  to reopen tree properly
    /// </summary>
    /// <param name="Node">Node to start. If Node = nil, then rebuilds whole tree</param>
    procedure RebuildIndexes(Node: TSVTreeNode<T>);
    /// <summary>
    /// This method rehashes the internal hash table to save space.
    ///This is only useful after a lot of items have been deleted from the tree.
    /// </summary>
    procedure CompactIndex; inline;
    //Virtual Tree procedure
    procedure RebuildVirtualTree(Node: PVirtualNode = nil);
    //properties
    property RootNode: TSVTreeNode<T> read FRoot;
    property RootNodeCount: Cardinal read GetRootNodeCount;
    property TotalCount: Cardinal read GetTotalCount;
    /// <summary>
    /// If VirtualTree assigned, our structure will be synchronized with it
    /// </summary>
    property VirtualTree: TVirtualStringTree read FVirtualTree write FVirtualTree;
    /// <summary>
    /// Owns Objects
    /// </summary>
    /// <param name="Owns Objects">If True, Tree will free associated object to the node
    ///   automatically </param>
    property OwnsObjects: Boolean read FFreeValues write FFreeValues;
    property IsEmpty: Boolean read GetIsEmpty write SetIsEmpty;
    //events
    property OnAfterAddChild: ProcAfterAddNode<T> read FAfterAddChild write FAfterAddChild;
    property OnFreeNode: ProcOnFreeNode<T> read FOnFreeNode write FOnFreeNode;
    property OnLoadNode: ProcOnSaveNode<T> read FOnLoadNode write FOnLoadNode;
    property OnSaveNode: ProcOnSaveNode<T> read FOnSaveNode write FOnSaveNode;
    property OnSaveTree: ProcSaveTree<T> read FOnSaveTree write FOnSaveTree;
    property OnLoadTree: ProcSaveTree<T> read FOnLoadTree write FOnLoadTree;

  end;

const
  SVTreeStreamVersion = 2;
  SVMagicID: TSVMagicID = (#$2045, 'S', 'V', WideChar(SVTreeStreamVersion), ' ', #$2046);

implementation

const

  NodeChunk = 1;
  BaseChunk = 2;        // chunk containing node state, check state, child node count etc.
                        // this chunk is immediately followed by all child nodes
  CaptionChunk = 3;     // used by the string tree to store a node's caption
  UserChunk = 4;        // used for data supplied by the application

{ TSVTree }
//TODO: fix absolute index counting
function TSVTree<T>.AbsoluteIndex(Node: TSVTreeNode<T>): Integer;
begin
  Result := 0;
  while Assigned(Node) and (Node <> FRoot) do
  begin
    if Assigned(Node.FPrevSibling) then
    begin
      // if there's a previous sibling then add its total count to the result
      Node := Node.FPrevSibling;
      Inc(Result, Node.FTotalCount);
    end
    else
    begin
      Node := Node.FParent;
      if Node <> FRoot then
        Inc(Result);
    end;
  end;
end;

function TSVTree<T>.AddChild(Parent: TSVTreeNode<T>; NodeData: T): TSVTreeNode<T>;
var
  tempRoot: TSVTreeNode<T>;
  pNode: PVirtualNode;
begin
  Result := nil;
  if not Assigned(Parent) then
  begin
    //root is present
    tempRoot := FRoot;
    pNode := nil;
  end
  else
  begin
    tempRoot := Parent;
    pNode := tempRoot.FVirtualNode;
  end;

  Result := InitNode(tempRoot, NodeData);

  if Assigned(FVirtualTree) then
  begin
    Result.FVirtualNode := FVirtualTree.AddChild(pNode)
  end
  else
  begin
    Result.FVirtualNode := nil;
  end;

  if Assigned(FAfterAddChild) then
  begin
    FAfterAddChild(Result);
  end;
end;

procedure TSVTree<T>.AdjustTotalCount(Node: TSVTreeNode<T>; Value: Integer;
  Relative: Boolean);
var
  Difference: Integer;
  Run: TSVTreeNode<T>;
begin
  if relative then
    Difference := Value
  else
    Difference := Integer(Value) - Integer(Node.FTotalCount);
  if Difference <> 0 then
  begin
    Run := Node;

    while Assigned(Run) and (Run <> Pointer(Self)) do
    begin
      Inc(Integer(Run.FTotalCount), Difference);
      Run := Run.FParent;
    end;
  end;
end;

procedure TSVTree<T>.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    if Assigned(FVirtualTree) then
    begin
      FVirtualTree.BeginUpdate;
    end;
  end;

  Inc(FUpdateCount);
end;

procedure TSVTree<T>.Clear;
begin
  BeginUpdate;
  try
    DeleteChildren(FRoot, False);
  
    FMainIndex.Clear;

  finally
    EndUpdate;
  end;

  if Assigned(FVirtualTree) then
    FVirtualTree.Clear;
end;

procedure TSVTree<T>.CompactIndex;
begin
  FMainIndex.TrimExcess;
end;

constructor TSVTree<T>.Create(OwnsObjects: Boolean = True; ACapacity: Integer = 0);
begin
  inherited Create();
  FCapacity := ACapacity;
  FFreeValues := OwnsObjects;
  FMainIndex := TDictionary<RawByteString,TSVTreeNode<T>>.Create(ACapacity);
  FOnFreeNode := nil;
  FOnLoadNode := nil;
  FOnSaveNode := nil;
  FOnLoadTree := nil;
  FOnSaveTree := nil;
  FAfterAddChild := nil;
  FVirtualTree := nil;
  FUpdateCount := 0;

  FRoot := nil;
  //init root node
  InitRootNode;
end;

function TSVTree<T>.ReadChunk(Stream: TStream; Version: Integer; Node: TSVTreeNode<T>;
  ChunkType, ChunkSize: Integer): Boolean;
var
  ChunkBody: TSVBaseChunkBody;
  Run: TSVTreeNode<T>;
  LastPosition: Integer;
  Foo: T;
begin
  case ChunkType of
    BaseChunk:
      begin
        // Load base chunk's body (chunk header has already been consumed).
        if Version > 1 then
          Stream.Read(ChunkBody, SizeOf(ChunkBody))
        else
        begin
         // with ChunkBody do
        //  begin
            // In version prior to 2 there was a smaller chunk body. Hence we have to read it entry by entry now.
            Stream.Read(ChunkBody.ChildCount, SizeOf(ChunkBody.ChildCount));
         //   Stream.Read(NodeHeight, SizeOf(NodeHeight));
            // TVirtualNodeStates was a byte sized type in version 1.
        //    States := [];
        //    Stream.Read(States, SizeOf(Byte));
            // vsVisible is now in the place where vsSelected was before, but every node was visible in the old version
            // so we need to fix this too.
        {    if vsVisible in States then
              Include(States, vsSelected)
            else
              Include(States, vsVisible);
            Stream.Read(Align, SizeOf(Align));
            Stream.Read(CheckState, SizeOf(CheckState));
            Stream.Read(CheckType, SizeOf(CheckType));  }
        //  end;
        end;

      //  with Node do
      //  begin
          // Set states first, in case the node is invisible.
          Node.FChildCount := ChunkBody.ChildCount;


          // Create and read child nodes.
          while ChunkBody.ChildCount > 0 do
          begin
            //Boolean
            Run := AddChild(Node,Foo);
            Dec(Node.FChildCount);  //addchild will increment it to parent

          {  Run.FPrevSibling := Node.FLastChild;
            if Assigned(Run.FPrevSibling) then
              Run.FNodeIndex := Run.FPrevSibling.FNodeIndex + 1;
            if Assigned(Node.FLastChild) then
              Node.FLastChild.FNextSibling := Run
            else
              Node.FFirstChild := Run;
            Node.FLastChild := Run;
            Run.FParent := Node;  }

            ReadNode(Stream, Version, Run);
            Dec(ChunkBody.ChildCount);
          end;
       // end;
        Result := True;
      end;
    UserChunk:
      if ChunkSize > 0 then
      begin
        // need to know whether the data was read
        LastPosition := Stream.Position;
        DoLoadUserData(Node, Stream);
        // compare stream position to learn whether the data was read
        Result := Stream.Position > LastPosition;
        // Improve stability by advancing the stream to the chunk's real end if
        // the application did not read what has been written.
        if not Result or (Stream.Position <> (LastPosition + ChunkSize)) then
          Stream.Position := LastPosition + ChunkSize;
      end
      else
        Result := True;
  else
    // unknown chunk, skip it
    Stream.Position := Stream.Position + ChunkSize;
    Result := False;
  end;
end;

procedure TSVTree<T>.ReadNode(Stream: TStream; Version: Integer;
  Node: TSVTreeNode<T>);
var
  Header: TSVChunkHeader;
  EndPosition: Integer;
begin
  with Stream do
  begin
    // Read anchor chunk of the node.
    Stream.Read(Header, SizeOf(Header));
    if Header.ChunkType = NodeChunk then
    begin
      EndPosition := Stream.Position + Header.ChunkSize;
      // Read all subchunks until the indicated chunk end position is reached in the stream.
      while Position < EndPosition do
      begin
        // Read new chunk header.
        Stream.Read(Header, SizeOf(Header));
        ReadChunk(Stream, Version, Node, Header.ChunkType, Header.ChunkSize);
      end;
      // If the last chunk does not end at the given end position then there is something wrong.
      if Position <> EndPosition then
        raise Exception.Create('Corrupted stream');
    end
    else
      raise Exception.Create('Corrupted stream');
  end;
end;

procedure TSVTree<T>.ProcessNode(ANode: TSVTreeNode<T>; Regenerate: Boolean);
var
  ix: Integer;
begin
  //index
  ix := 0;
  while Assigned(ANode) do
  begin
    if Assigned(ANode.FFirstChild) then
    begin
      ProcessNode(ANode.FFirstChild, Regenerate);
    end;

    if Regenerate then
      FMainIndex.Remove(GenerateIndex(ANode));

    ANode.FNodeIndex := ix;
    ANode.FVirtualNode := nil;

  //  FMainIndex.Add(GenerateIndex(ANode), ANode);

    ANode := ANode.FNextSibling;
    Inc(ix);
  end;
end;

procedure TSVTree<T>.ProcessNodeAddIndex(ANode: TSVTreeNode<T>);
var
  pKey: string;
begin
  //index
  while Assigned(ANode) do
  begin
    if ANode.HasChildren then
    begin
      ProcessNodeAddIndex(ANode.FFirstChild);
    end;

    FMainIndex.Add(GenerateIndex(ANode), ANode);

    ANode := ANode.FNextSibling;
  end;
end;

procedure TSVTree<T>.RebuildIndexes(Node: TSVTreeNode<T>);
var
  pRun, pNode: TSVTreeNode<T>;
  bRegenerate: Boolean;
begin
  bRegenerate := (Node <> nil);
  if not bRegenerate then
  begin
    pRun := FRoot.FFirstChild;
    FMainIndex.Clear;
  end
  else
    pRun := Node;

  if Assigned(pRun) then
  begin
    pNode := pRun;

    ProcessNode(pRun, bRegenerate);

    ProcessNodeAddIndex(pNode);
  end;

end;

procedure TSVTree<T>.RebuildVirtualTree(Node: PVirtualNode);
var
  sNode: TSVTreeNode<T>;
begin
  if Assigned(FVirtualTree) then
  begin
    if not Assigned(Node) then
    begin
      FVirtualTree.Clear;
      RebuildIndexes(nil);
      FVirtualTree.RootNodeCount := RootNodeCount;
    end
    else
    begin
      sNode := GetNode(FVirtualTree.GenerateIndex(Node));
      if Assigned(sNode) then
      begin
        RebuildIndexes(sNode);
        FVirtualTree.BeginUpdate;
        try
          FVirtualTree.ChildCount[Node] := sNode.FChildCount;
        finally
          FVirtualTree.EndUpdate;
        end;
      end;
     //
    end;
  end;
end;

procedure TSVTree<T>.SaveToFile(const FileName: TFileName);
var
  FileStream: TFileStream;

begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream, nil);
  finally
    FileStream.Free;
  end;
end;

procedure TSVTree<T>.SaveToStream(Stream: TStream; Node: TSVTreeNode<T>);
var
  Count: Cardinal;
begin
  Stream.Write(SVMagicID, SizeOf(SVMagicID));
  if Node = nil then
  begin
    // Keep number of top level nodes for easy restauration.
    Count := RootNodeCount;
    Stream.WriteBuffer(Count, SizeOf(Count));

    Node := FRoot.FFirstChild;
    while Assigned(Node) do
    begin
      WriteNode(Stream, Node);
      Node := Node.FNextSibling;
    end;
  end
  else
  begin
    Count := 1;
    Stream.WriteBuffer(Count, SizeOf(Count));
    WriteNode(Stream, Node);
  end;

  if Assigned(FOnSaveTree) then
    FOnSaveTree(Self, Stream);
end;

procedure TSVTree<T>.SetIsEmpty(const Value: Boolean);
begin
  if Value then
  begin
    Clear;
  end;
end;

procedure TSVTree<T>.WriteChunks(Stream: TStream; Node: TSVTreeNode<T>);
var
  Header: TSVChunkHeader;
  LastPosition,
  ChunkSize: Integer;
  Chunk: TSVBaseChunk;
  Run: TSVTreeNode<T>;
begin
  with Stream do
  begin
    // 1. The base chunk...
    LastPosition := Position;
    Chunk.Header.ChunkType := BaseChunk;
    Chunk.Body.ChildCount := Node.FChildCount;
    // write the base chunk
    Write(Chunk, SizeOf(Chunk));

    // 2. ... directly followed by the child node chunks (actually they are child chunks of
    //   the base chunk)

    Run := Node.FFirstChild;
    while Assigned(Run) do
    begin
      WriteNode(Stream, Run);
      Run := Run.FNextSibling;
    end;

    FinishChunkHeader(Stream, LastPosition, Position);

    // 3. write user data
    LastPosition := Position;
    Header.ChunkType := UserChunk;
    Write(Header, SizeOf(Header));
    //save custom object data
    DoSaveUserData(Node, Stream);
    // check if the application actually wrote data
    ChunkSize := Position - LastPosition - SizeOf(TSVChunkHeader);
    // seek back to start of chunk if nothing has been written
    if ChunkSize = 0 then
    begin
      Position := LastPosition;
      Size := Size - SizeOf(Header);
    end
    else
      FinishChunkHeader(Stream, LastPosition, Position);
  end;
end;

procedure TSVTree<T>.WriteNode(Stream: TStream; Node: TSVTreeNode<T>);
var
  LastPosition: Integer;
  Header: TSVChunkHeader;
begin
  with Stream do
  begin
    LastPosition := Position;
    // Emit the anchor chunk.
    Header.ChunkType := NodeChunk;
    Write(Header, SizeOf(Header));
    // Write other chunks to stream taking their size into this chunk's size.
    WriteChunks(Stream, Node);

    // Update chunk size.
    FinishChunkHeader(Stream, LastPosition, Position);
  end;
end;

function TSVTree<T>.InitNode(ParentNode: TSVTreeNode<T>; Data: T):TSVTreeNode<T>;
var
  sHash: RawByteString;
begin
  Result := TSVTreeNode<T>.Create;
 // New(Result);
  //init default node values
  Result.FValue := Data;
//  Result.FOwner := Self;
  Result.FChildCount := 0;
  //  Result.FLevel := ParentNode.FLevel + 1;
  if Assigned(ParentNode.FLastChild) then
  begin
    Result.FNodeIndex := ParentNode.FLastChild.FNodeIndex + 1;
    ParentNode.FLastChild.FNextSibling := Result;
  end
  else
    Result.FNodeIndex := 0;

  Result.FParent := ParentNode;
  Result.FPrevSibling := ParentNode.FLastChild;


  ParentNode.FLastChild := Result;

  if not Assigned(ParentNode.FFirstChild) then
    ParentNode.FFirstChild := Result;

  Result.FTotalCount := 1;
  Inc(ParentNode.FChildCount);

  AdjustTotalCount(ParentNode, 1, True);

  sHash := GenerateIndex(Result);
  FMainIndex.Add(sHash, Result);
  //ateityje atnaujinam  total count kitiems
 // AdjustTotalCount(Result, 1, True);

  Result.FLastChild := nil;
  Result.FFirstChild := nil;
  Result.FNextSibling := nil;
  //add index
end;

procedure TSVTree<T>.InitRootNode;
begin
  if FRoot = nil then
  begin
    FRoot := TSVTreeNode<T>.Create;
  //  FRoot.FValue := nil;
    FRoot.FParent := Pointer(Self);
    FRoot.FChildCount := 0;
    FRoot.FNodeIndex := -1;
    FRoot.FTotalCount := 1;
    FRoot.FLastChild := nil;
    FRoot.FFirstChild := nil;
    FRoot.FNextSibling := FRoot;
    FRoot.FPrevSibling := FRoot;
  end;
end;

procedure TSVTree<T>.InternalAddFromStream(Stream: TStream; Version: Integer;
  Node: TSVTreeNode<T>);
begin
  Assert(Node <> FRoot, 'The root node cannot be loaded from stream.');

  ReadNode(Stream, Version, Node);

 // FixupTotalCount(Node);
 // AdjustTotalCount(Node.FParent, Node.FTotalCount - 1, True); 
end;

procedure TSVTree<T>.InternalDeleteNode(Node: TSVTreeNode<T>; DoReindex: Boolean);
var
  obj: TObject;
  pTemp: TSVTreeNode<T>;
begin
  if Assigned(FOnFreeNode) then
    FOnFreeNode(Node)
  else
  begin
    if FFreeValues then
    begin
      obj := TObject(Node.FValue);
     // obj := @(Node.FValue)^;
      if Assigned(obj) then
        obj.Free;
    end;
  end;

  pTemp := Node.FParent;

  if Assigned(pTemp) then
  begin
    Dec(pTemp.FChildCount);

    if Node = pTemp.FLastChild then
    begin
      pTemp.FLastChild := Node.FPrevSibling;
    end;

    if Node = pTemp.FFirstChild then
    begin
      pTemp.FFirstChild := Node.FNextSibling;
    end;

    if Assigned(Node.FPrevSibling) then
    begin
      Node.FPrevSibling.FNextSibling := Node.FNextSibling;
    end;

    if Assigned(Node.FNextSibling) then
    begin
      Node.FNextSibling.FPrevSibling := Node.FPrevSibling;
    end;
  end;


  Node.FNextSibling := nil;
    

  if Assigned(FVirtualTree) then
  begin
    //make sure that node is inited
    if Assigned(Node.FVirtualNode) then
    begin
 //     FVirtualTree.ChildCount[Node.FVirtualNode] := 0;
    //  FVirtualTree.ValidateNode(Node.FVirtualNode, False);
      //check if virtual node are created
        
      FVirtualTree.DeleteNode(Node.FVirtualNode, False);
    end;
  end;
  

  if DoReindex then
  begin
    FMainIndex.Remove(GenerateIndex(Node));
    AdjustTotalCount(Node.FParent, 1, False);
  end;



//free resources
  Node.Free;
 // Dispose(Node);
  Node := nil;
end;


function TSVTree<T>.IsRootNode(Node: TSVTreeNode<T>): Boolean;
begin
  Result := (Assigned(Node)) and (FRoot = Node);
end;

function TSVTree<T>.IterateSubtree(Node: TSVTreeNode<T>;
  const ACallbackProc: TIterCallback<T>; Data: Pointer = nil): TSVTreeNode<T>;
var
  DoAbort: Boolean;
  pStop: TSVTreeNode<T>;
begin
  Result := nil;
  DoAbort := False;
  if Node = nil then
    pStop := nil
  else
  begin
    // The stopper does not need to be initialized since it is not taken into the enumeration.
    pStop := Node.FNextSibling;
    if pStop = nil then
    begin
      pStop := Node;
      repeat
        pStop := pStop.FParent;
      until (pStop = FRoot) or Assigned(pStop.FNextSibling);
      if pStop = FRoot then
        pStop := nil
      else
        pStop := pStop.FNextSibling;
    end;
  end;

  if Node = nil then
  begin
    Node := FRoot.FFirstChild;
  end;

  if Assigned(Node) then
  begin
    while Assigned(Node) and (Node <> pStop) do
    begin
      ACallbackProc(Node, Data, DoAbort);
      if DoAbort then
      begin
        Result := Node;
        Break;
      end;

      Node := GetNext(Node);
    end;
  end;


end;

procedure TSVTree<T>.LoadFromFile(const FileName: TFileName);
var
  FileStream: TFileStream;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TSVTree<T>.LoadFromStream(Stream: TStream);
var
  ThisID: TSVMagicID;
  Version,
  Count: Cardinal;
  Node: TSVTreeNode<T>;
  vt: TVirtualStringTree;
  Foo: T;
begin
  Clear;
  if Assigned(FVirtualTree) then
  begin
    vt := FVirtualTree;
    FVirtualTree := nil;
  end
  else
  begin
    vt := nil;
  end;

  try

  // Check first whether this is a stream we can read.
  if Stream.Read(ThisID, SizeOf(TSVMagicID)) < SizeOf(TSVMagicID) then
    raise Exception.Create('Uncompatible stream');

  if (ThisID[0] = SVMagicID[0]) and
       (ThisID[1] = SVMagicID[1]) and
       (ThisID[2] = SVMagicID[2]) and
       (ThisID[5] = SVMagicID[5]) then
  begin
    Version := Word(ThisID[3]);
    if Version <= SVTreeStreamVersion then
    begin
      BeginUpdate;
      try
        if Version < 2 then
          Count := MaxInt
        else
          Stream.ReadBuffer(Count, SizeOf(Count));

        while (Stream.Position < Stream.Size) and (Count > 0) do
        begin
          Dec(Count);
          //get node data
          Node := AddChild(nil, Foo);
          
        //  Node := AddChild();
        //  InternalConnectNode(Node, FRoot, Self, amAddChildLast);
          InternalAddFromStream(Stream, Version, Node);
        end;
       // DoNodeCopied(nil);
        if Assigned(FOnLoadTree) then
          FOnLoadTree(Self, Stream);
      finally
        EndUpdate;
      end;
    end
    else
      raise Exception.Create('Wrong stream version');
  end
  else
    raise Exception.Create('Wrong stream format');

  finally
    FVirtualTree := vt;   
  end;

  if Assigned(FVirtualTree) then
  begin
    FVirtualTree.Clear;
    FVirtualTree.RootNodeCount := RootNodeCount;
  end;
end;

procedure TSVTree<T>.DeleteChildren(Node: TSVTreeNode<T>; DoReindex: Boolean);
var
  Run, Mark: TSVTreeNode<T>;
begin
  if (Assigned(Node)) and (Node.HasChildren) then
  begin
    Run := Node.FLastChild;
    while Assigned(Run) do
    begin
      Mark := Run;
      Run := Run.FPrevSibling;
      if Assigned(Run) then
        Run.FNextSibling := nil;  

      DeleteNode(Mark); 
    end;  

    Node.FChildCount := 0;
    if IsRootNode(Node) then
    begin
      Node.FTotalCount := 1;
    end
    else
    begin
      if DoReindex then
      begin
        AdjustTotalCount(Node, 1);
      end;
    end;

    Node.FFirstChild := nil;
    Node.FLastChild := nil;
   end;
end;

procedure TSVTree<T>.DeleteNode(Node: TSVTreeNode<T>; DoReindex: Boolean);
begin
  if (Assigned(Node)) and (Node <> FRoot) then
  begin
    DeleteChildren(Node, DoReindex);
    InternalDeleteNode(Node, DoReindex);
  end;
end;

destructor TSVTree<T>.Destroy;
begin
  FVirtualTree := nil;

  Clear;

  FreeRoot;

  FMainIndex.Free;

  FOnFreeNode := nil;
  FOnLoadNode := nil;
  FOnSaveNode := nil;
  FOnLoadTree := nil;
  FOnSaveTree := nil;
  FAfterAddChild := nil;

  inherited Destroy;
end;

procedure TSVTree<T>.DoLoadUserData(Node: TSVTreeNode<T>; Stream: TStream);
begin
  if Assigned(FOnLoadNode) then
  begin
    if Node = FRoot then
      FOnLoadNode(Self, nil, Stream)
    else
      FOnLoadNode(Self, Node, Stream);
  end;
end;

procedure TSVTree<T>.DoSaveUserData(Node: TSVTreeNode<T>; Stream: TStream);
begin
  if Assigned(FOnSaveNode) then
  begin
    if Node = FRoot then
      FOnSaveNode(Self, nil, Stream)
    else
      FOnSaveNode(Self, Node, Stream);
  end;
end;

procedure TSVTree<T>.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);

  if FUpdateCount = 0 then
  begin
    if Assigned(FVirtualTree) then
    begin
      FVirtualTree.EndUpdate;
    end;
  end;
end;

procedure TSVTree<T>.FinishChunkHeader(Stream: TStream; StartPos, EndPos: Integer);
var
  Size: Integer;
begin
  // seek back to the second entry in the chunk header
  Stream.Position := StartPos + SizeOf(Integer);
  // determine size of chunk without the chunk header
  Size := EndPos - StartPos - SizeOf(TSVChunkHeader);
  // write the size...
  Stream.Write(Size, SizeOf(Size));
  // ... and seek to the last endposition
  Stream.Position := EndPos;
end;

procedure TSVTree<T>.FixupTotalCount(Node: TSVTreeNode<T>);
var
  Child: TSVTreeNode<T>;

begin
  // Initial total count is set to one on node creation.
  Child := Node.FFirstChild;
  while Assigned(Child) do
  begin
    FixupTotalCount(Child);
    Inc(Node.FTotalCount, Child.FTotalCount);
    Child := Child.FNextSibling;
  end;
end;

procedure TSVTree<T>.FreeRoot;
begin
  FRoot.Free;
//  Dispose(FRoot);
  FRoot := nil;
end;

function TSVTree<T>.GenerateIndex(Node: TSVTreeNode<T>): RawByteString;
var
  pNode: TSVTreeNode<T>;
begin
  Result := EmptyAnsiStr;
  if Assigned(Node) then
  begin
    Result := RawByteString(IntToStr(Node.FNodeIndex));
    pNode := Node.FParent;
    while Assigned(pNode) and (pNode <> FRoot) do
    begin
      Result := Result + RawByteString(Format('/%D',[pNode.FNodeIndex]));

      pNode := pNode.FParent;
    end;
  end;
end;

function TSVTree<T>.GetFirst: TSVTreeNode<T>;
begin
  Result := FRoot.FFirstChild;
end;

function TSVTree<T>.GetFirstLevel(NodeLevel: Integer): TSVTreeNode<T>;
begin
  Result := GetFirst;
  while Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) do
    Result := GetNext(Result);

  if Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) then // i.e. there is no node with the desired level in the tree
    Result := nil;
end;

function TSVTree<T>.GetIsEmpty: Boolean;
begin
  Result := (RootNodeCount < 1);
end;

function TSVTree<T>.GetLast(Node: TSVTreeNode<T>): TSVTreeNode<T>;
var
  pNext: TSVTreeNode<T>;
begin
  Result := GetLastChild(Node);
  while Assigned(Result) do
  begin
    // Test if there is a next last child. If not keep the node from the last run.
    // Otherwise use the next last child.
    pNext := GetLastChild(Result);
    if pNext = nil then
      Break;
    Result := pNext;
  end;
end;

function TSVTree<T>.GetLastChild(Node: TSVTreeNode<T>): TSVTreeNode<T>;
begin
  if (Node = nil) or (Node = FRoot) then
    Result := FRoot.FLastChild
  else
  begin
    if Node.HasChildren then
      Result := Node.FLastChild
    else
      Result := nil;
  end;
end;

function TSVTree<T>.GetNext(Node: TSVTreeNode<T>): TSVTreeNode<T>;
begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the root node.');
    // If there is no child node try siblings.
    if Assigned(Result.FFirstChild) then
      Result := Result.FFirstChild
    else
    begin
      repeat
        // Is there a next sibling?
        if Assigned(Result.FNextSibling) then
        begin
          Result := Result.FNextSibling;
          Break;
        end
        else
        begin
          // No sibling anymore, so use the parent's next sibling.
          if Result.FParent <> FRoot then
            Result := Result.FParent
          else
          begin
            // There are no further nodes to examine, hence there is no further visible node.
            Result := nil;
            Break;
          end;
        end;
      until False;
    end;
  end;
end;

function TSVTree<T>.GetNextLevel(Node: TSVTreeNode<T>; NodeLevel: Integer): TSVTreeNode<T>;
var
  StartNodeLevel: Integer;
begin
  Result := nil;

  if Assigned(Node) and (Node <> FRoot) then
  begin
    StartNodeLevel := GetNodeLevel(Node);

    if StartNodeLevel < NodeLevel then
    begin
      Result := GetNext(Node);
      if Assigned(Result) and (GetNodeLevel(Result) <> NodeLevel) then
        Result := GetNextLevel(Result, NodeLevel);
    end
    else if StartNodeLevel = NodeLevel then
    begin
      Result := Node.FNextSibling;
      if not Assigned(Result) then // i.e. start node was a last sibling
      begin
        Result := Node.FParent;
        if Assigned(Result) then
        begin
          // go to next anchestor of the start node which has a next sibling (if exists)
          while Assigned(Result) and not Assigned(Result.FNextSibling) do
            Result := Result.FParent;
          if Assigned(Result) then
            Result := GetNextLevel(Result.FNextSibling, NodeLevel);
        end;
      end;
    end
    else // i.e. StartNodeLevel > NodeLevel
      Result := GetNextLevel(Node.FParent, NodeLevel);
  end;
end;

function TSVTree<T>.GetNode(const sNodeIndex: RawByteString): TSVTreeNode<T>;
begin
  Result := nil;
  if sNodeIndex <> '' then
  begin
    if not FMainIndex.TryGetValue(sNodeIndex, Result) then
      Result := nil;
  end;
end;

function TSVTree<T>.GetNode(Node: PVirtualNode): TSVTreeNode<T>;
begin
  Result := nil;
  if Assigned(FVirtualTree) and Assigned(Node) then
  begin
    Result := GetNode(FVirtualTree.GenerateIndex(Node));
  end;
end;

function TSVTree<T>.GetNodeLevel(Node: TSVTreeNode<T>): Integer;
var
  pRun: TSVTreeNode<T>;
begin
  Result := 0;
  if Assigned(Node) and (Node <> FRoot) then
  begin
    pRun := Node.FParent;
    while pRun <> FRoot do
    begin
      pRun := pRun.FParent;
      Inc(Result);
    end;
  end;
end;

function TSVTree<T>.GetPrevious(Node: TSVTreeNode<T>): TSVTreeNode<T>;
begin
  Result := Node;
  if Assigned(Result) then
  begin
    Assert(Result <> FRoot, 'Node must not be the root node.');
    // Is there a previous sibling?
    if Assigned(Node.FPrevSibling) then
    begin
      // Go down and find the last child node.
      Result := GetLast(Node.FPrevSibling);
      if Result = nil then
        Result := Node.FPrevSibling;
    end
    else
      // No previous sibling so the parent of the node is the previous node.
      if Node.FParent <> FRoot then
        Result := Node.FParent
      else
        Result := nil
  end;
end;

function TSVTree<T>.GetPreviousLevel(Node: TSVTreeNode<T>;
  NodeLevel: Integer): TSVTreeNode<T>;
var
  StartNodeLevel: Integer;
  pRun: TSVTreeNode<T>;
begin
  Result := nil;

  if Assigned(Node) and (Node <> FRoot) then
  begin
    StartNodeLevel := GetNodeLevel(Node);

    if StartNodeLevel < NodeLevel then
    begin
      Result := Node.FPrevSibling;
      if Assigned(Result) then
      begin
        // go to last descendant of previous sibling with desired node level (if exists)
        pRun := Result;
        while Assigned(pRun) and (GetNodeLevel(pRun) < NodeLevel) do
        begin
          Result := pRun;
          pRun := GetLastChild(pRun);
        end;
        if Assigned(pRun) and (GetNodeLevel(pRun) = NodeLevel) then
          Result := pRun
        else
        begin
          if Assigned(Result.FPrevSibling) then
            Result := GetPreviousLevel(Result, NodeLevel)
          else if Assigned(Result) and (Result.FParent <> FRoot) then
            Result := GetPreviousLevel(Result.FParent, NodeLevel)
          else
            Result := nil;
        end;
      end
      else
        Result := GetPreviousLevel(Node.FParent, NodeLevel);
    end
    else if StartNodeLevel = NodeLevel then
    begin
      Result := Node.FPrevSibling;
      if not Assigned(Result) then // i.e. start node was a first sibling
      begin
        Result := Node.FParent;
        if Assigned(Result) then
          Result := GetPreviousLevel(Result, NodeLevel);
      end;
    end
    else // i.e. StartNodeLevel > NodeLevel
      Result := GetPreviousLevel(Node.FParent, NodeLevel);
  end;
end;

function TSVTree<T>.GetRootNodeCount: Cardinal;
begin
  if Assigned(FRoot) then
    Result := FRoot.FChildCount
  else
    Result := 0;
end;

function TSVTree<T>.GetTotalCount: Cardinal;
begin
  Result := FRoot.FTotalCount - 1;
end;

function TSVTree<T>.HasChildren(Node: TSVTreeNode<T>): Boolean;
begin
  Result := (Assigned(Node)) and (Node.FChildCount > 0);
end;

function TSVTree<T>.HasParent(Node: TSVTreeNode<T>): Boolean;
begin
  Result := (Node.FParent <> Pointer(Self));
end;



{ TSVTreeNode<T> }

function TSVTreeNode<T>.GetOwner: TSVTree<T>;
var
  pNode: TSVTreeNode<T>;
begin
  pNode := @Self;
  Result := SVTreeFromNode(pNode);
end;

function TSVTreeNode<T>.HasChildren: Boolean;
begin
  Result := (FChildCount > 0);
end;

function TSVTreeNode<T>.HasParent: Boolean;
var
  pNode: TSVTreeNode<T>;
begin
  pNode := @Self;
  Result := (Self.FParent <> Pointer(SVTreeFromNode(pNode)));
end;


class function TSVTreeNode<T>.SVTreeFromNode(Node: TSVTreeNode<T>): TSVTree<T>;
begin
  Assert(Assigned(Node), 'Node must not be nil.');

  // The root node is marked by having its NextSibling (and PrevSibling) pointing to itself.
  while Assigned(Node) and (Node.FNextSibling <> Node) do
    Node := Node.FParent;
  if Assigned(Node) then
  begin
    try
      Result := TSVTree<T>(Node.FParent);
    except
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TSVBaseVirtualTreeHelper }

function TSVBaseVirtualTreeHelper.GenerateIndex(
  Node: PVirtualNode): RawByteString;
var
  pNode: PVirtualNode;
begin
  Result := EmptyAnsiStr;
  if Assigned(Node) then
  begin
    Result := RawByteString(IntToStr(Node.Index));
    pNode := Node.Parent;
    while Assigned(pNode) and (pNode <> RootNode) do
    begin
      Result := Result + RawByteString(Format('/%D',[pNode.Index]));

      pNode := pNode.Parent;
    end;
  end;
end;

function TSVBaseVirtualTreeHelper.GetSVNode<T>(Node: PVirtualNode; SVTree: TSVTree<T>): TSVTreeNode<T>;
begin
  Result := nil;
  if Assigned(Node) then
  begin
    Result := SVTree.GetNode(Node);
  end;
end;


end.
