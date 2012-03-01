(*
* Copyright (c) 2012, Linas Naginionis
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
unit SvContainers;

interface

uses
  Classes, Generics.Collections;

type

  THashLinkedItem<T> = class(TObject)
  private
    FValue: Cardinal;
    FData: T;
    FNext: THashLinkedItem<T>;
  public
    constructor Create(Value: Cardinal; Data: T; Next: THashLinkedItem<T>);
    destructor Destroy; override;
  end;

 // THashTrie = class; //forward declaration

 // TTraverseProc = procedure<T>(UserData, UserProc: Pointer; Value: Cardinal; Data: T; var Done: Boolean) of object;

  THashTreeItem<T> = class(TObject)
  private
    FOwner: TObject;
    FLevel: Integer;
    FFilled: Integer;
    FItems: array of TObject; // This will be at most LeafSize entries.
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: T);
    procedure Delete(Value, Hash: Cardinal);
    function Find(Value, Hash: Cardinal; var Data: T): Boolean;
    function GetFilled: Integer;
    function Modify(Value, Hash: Cardinal; const Data: T): Boolean;
    function ROR(Value: Cardinal): Cardinal;
    function RORN(Value: Cardinal; Level: Integer): Cardinal;
   // function Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    procedure Clear;
  end;


  THashTrie<T> = class(TEnumerable<T>)
  private
    FRoot: THashTreeItem<T>;
    function GetCount: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure AddDown(Value, Hash: Cardinal; const Data: T);
    function CompareValue(Value1, Value2: Cardinal): Boolean; virtual; abstract;
    procedure Delete(Value, Hash: Cardinal);
    procedure DestroyItem(var Value: Cardinal; var Data: T); virtual; abstract;
    function HashValue(Value: Cardinal): Cardinal; virtual; abstract;


  //  procedure Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);
  public
    constructor Create; virtual;
    destructor Destroy; override;



    procedure Clear;
    function Find(Value, Hash: Cardinal; var Data: T): Boolean; overload;

    property Count: Integer read GetCount;

  type
    TEnumerator = class(TEnumerator<T>)
    private
      FTrie: THashTrie<T>;
      FIndex: Integer;
      FMax: Integer;
      FCurrIndex: Integer;
      FCurrObj: TObject;
      FRoot: THashTreeItem<T>;
      FLinkedItem: THashLinkedItem<T>;
      function GetCurrent: T;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(ATrie: THashTrie<T>);
      property Current: T read GetCurrent;
      function MoveNext: Boolean;
    end;

    function GetEnumerator: TEnumerator; reintroduce;
  end;


  TSvStringTrie<T> = class(THashTrie<T>)
  private
    FCaseSensitive: Boolean;
  //  FOnFreeItem: TSHTFreeItemEvent;
  protected
    function HashValue(Value: Cardinal): Cardinal; override;
    procedure DestroyItem(var Value: Cardinal; var Data: T); override;
    function CompareValue(Value1, Value2: Cardinal): Boolean; override;
    function HashStr(const S: string): Cardinal;
  //  procedure TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: T; var Done: Boolean);
  //  procedure TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: T; var Done: Boolean);
  public
    procedure Add(const S: string; const Data: T);
    procedure Delete(const S: string);
    function Find(const S: string; var Data: T): Boolean; overload;
  //  procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc); overload;
  //  procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth); overload;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;

  //  property OnFreeItem: TSHTFreeItemEvent read FOnFreeItem write FOnFreeItem;
  end;

  function CalcStrCRC32(const S: string): Cardinal;
  function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;
  //procedure TrieStatistics(Trie: THashTrie<T>; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  //  var LengthStatistics: TLengthStatistics);


implementation

uses
  SysUtils;

const
  BufferIncrement = 1024; // Size by which the buffer in the buffered string is incremented
                          // when the current space is exhausted.

  // LeafSize must be 256. No changes allowed.
  LeafSize = 256;
  // BucketSize determines max length of the list. Very big|small values decrease performance, while
  // the optimum value in range 4..16.
  BucketSize = 8;

  const
  CRC32_POLYNOMIAL = $EDB88320;

var
  // Dynamic crc32 table.
  CCITT32Table: array of Cardinal;

procedure BuildCRCTable;

var
  i, j: longint;
  value: Cardinal;
begin
  SetLength(CCITT32Table, 256);
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do
      if ((value and 1) <> 0) then
        value := (value shr 1) xor CRC32_POLYNOMIAL
      else
        value := value shr 1;
    Ccitt32Table[i] := value;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function CalcStrCRC32(const S: string): Cardinal;

var
  I: Integer;

begin
  // Create CRC table if not yet done.
  if CCITT32Table = nil then
    BuildCRCTable;

  Result := $FFFFFFFF;
  for I:=1 to Length(S) do
    Result:= (((Result shr 8) and $00FFFFFF) xor (CCITT32Table[(Result xor Byte(S[I])) and $FF]));
end;

//----------------------------------------------------------------------------------------------------------------------

// By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net
//
// If you are hashing n strings (ub1 **)k, do it like this:
//   for (i=0, h=0; i<n; ++i) h = jhash( k[i], len[i], h);

procedure Mix(var A, B, C: Cardinal);

begin
  Dec(A, B); Dec(A, C); A := A xor (C shr 13);
  Dec(B, C); Dec(B, A); B := A xor (A shl 8);
  Dec(C, A); Dec(C, B); C := C xor (B shr 13);
  Dec(A, B); Dec(A, C); A := A xor (C shr 12);
  Dec(B, C); Dec(B, A); B := B xor (A shl 16);
  Dec(C, A); Dec(C, B); C := C xor (B shr 5);
  Dec(A, B); Dec(A, C); A := A xor (C shr 3);
  Dec(B, C); Dec(B, A); B := B xor (A shl 10);
  Dec(C, A); Dec(C, B); C := C xor (B shr 15);
end;

//----------------------------------------------------------------------------------------------------------------------

function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;

// Length: the length of the key.
// InitVal: the previous hash, or an arbitrary value.

var
  A, B, C, Len: Cardinal;
  K: PByteArray;

begin
  // Set up the internal state.
  Len := Length;
  K := Key;
  A := $9E3779B9;  // The golden ratio; an arbitrary value.
  B := $9E3779B9;
  C := InitVal;    // The previous hash value.

  // Handle most of the key.
  while Len >= 12 do
  begin
    Inc(A, K[0] + (Cardinal(K[1]) shl 8) + (Cardinal(K[2]) shl 16) + (Cardinal(K[3]) shl 24));
    Inc(B, K[4] +(Cardinal(K[5]) shl 8) + (Cardinal(K[6]) shl 16) + (Cardinal(K[7]) shl 24));
    Inc(C, K[8] + (Cardinal(K[9]) shl 8) + (Cardinal(K[10]) shl 16) + (Cardinal(K[11]) shl 24));
    Mix(A, B, C);
    Inc(PByte(K), 12);
    Dec(Len, 12);
  end;

   // Handle the last 11 bytes.
  Inc(C, Length);
  if Len >= 11 then
    Inc(C, Cardinal(K[10]) shl 24);
  if Len >= 10 then
    Inc(C, Cardinal(K[9]) shl 16);
  if Len >= 9 then
    Inc(C, Cardinal(K[8]) shl 8);
  if Len >= 8 then
    Inc(B, Cardinal(K[7]) shl 24);
  if Len >= 7 then
    Inc(B, Cardinal(K[6]) shl 16);
  if Len >= 6 then
    Inc(B, Cardinal(K[5]) shl 8);
  if Len >= 5 then
    Inc(B, Cardinal(K[4]));
  if Len >= 4 then
    Inc(A, Cardinal(K[3]) shl 24);
  if Len >= 3 then
    Inc(A, Cardinal(K[2]) shl 16);
  if Len >= 2 then
    Inc(A, Cardinal(K[1]) shl 8);
  if Len >= 1 then
    Inc(A, Cardinal(K[0]));
  // Case 0: nothing left to add.

  Mix(A, B, C);
  Result := C;
end;

{
procedure TrieStatistics<T>(Trie: THashTrie<T>; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);

  //--------------- local function --------------------------------------------

  procedure TreeStat(Item: THashTreeItem);

  var
    I, J: Integer;
    LinkedItem: THashLinkedItem;

  begin
    Inc(PeakCount);
    if Item.FLevel + 1 > MaxLevel then
      MaxLevel := Item.FLevel + 1;

    for J := 0 to High(Item.FItems) do
      if Assigned(Item.FItems[J]) then
      begin
        Inc(FillCount);
        if Item.FItems[J] is THashTreeItem then
          TreeStat(THashTreeItem(Item.FItems[J]))
        else
        begin
          I := 0;
          LinkedItem := THashLinkedItem(Item.FItems[J]);
          while Assigned(LinkedItem) do
          begin
            Inc(I);
            LinkedItem := LinkedItem.FNext;
          end;
          Inc(LengthStatistics[I]);
        end;
      end
      else
        Inc(EmptyCount);
  end;

  //--------------- end local function ----------------------------------------

begin
  MaxLevel := 0;
  PeakCount := 0;
  FillCount := 0;
  EmptyCount := 0;

  if Assigned(Trie.FRoot) then
    TreeStat(Trie.FRoot);
end;  }

{ THashLinkedItem }

constructor THashLinkedItem<T>.Create(Value: Cardinal; Data: T; Next: THashLinkedItem<T>);
begin
  inherited Create;
  FValue := Value;
  FData := Data;
  FNext := Next;
end;

destructor THashLinkedItem<T>.Destroy;
begin
  FNext.Free;
  inherited Destroy;
end;

{ THashTreeItem<T> }

procedure THashTreeItem<T>.AddDown(Value, Hash: Cardinal; const Data: T);
var
  I, J: Integer;
  TreeItem: THashTreeItem<T>;
  LinkedItem: THashLinkedItem<T>;
begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if FItems[I] = nil then
  begin
    FItems[I] := THashLinkedItem<T>.Create(Value, Data, nil);
    Inc(FFilled);
  end
  else
    if FItems[I] is THashTreeItem<T> then
      THashTreeItem<T>(FItems[I]).AddDown(Value, ROR(Hash), Data)
    else
    begin
      J := 0;
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
        Inc(J)
      end;

      if J >= BucketSize then
      begin
        // full
        TreeItem := THashTreeItem<T>.Create(FOwner);
        TreeItem.FLevel := FLevel + 1;
        LinkedItem := THashLinkedItem<T>(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TreeItem.AddDown(LinkedItem.FValue, RORN(THashTrie<T>(FOwner).HashValue(LinkedItem.FValue), FLevel + 1), LinkedItem.FData);
          LinkedItem := LinkedItem.FNext;
        end;
        TreeItem.AddDown(Value, ROR(Hash), Data);
        THashLinkedItem<T>(FItems[I]).Free;
        FItems[I] := TreeItem;
      end
      else
        FItems[I] := THashLinkedItem<T>.Create(Value, Data, THashLinkedItem<T>(FItems[I]));
    end;
end;

procedure THashTreeItem<T>.Clear;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem<T> then
      THashTreeItem<T>(FItems[I]).Free
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        THashTrie<T>(FOwner).DestroyItem(LinkedItem.FValue, LinkedItem.FData);
        LinkedItem := LinkedItem.FNext;
      end;
      THashLinkedItem<T>(FItems[I]).Free;
    end;
  FItems := nil;
end;

constructor THashTreeItem<T>.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure THashTreeItem<T>.Delete(Value, Hash: Cardinal);
var
  I: Integer;
  PrevLinkedItem,
  LinkedItem: THashLinkedItem<T>;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[i] is THashTreeItem<T> then
    begin
      THashTreeItem<T>(FItems[I]).Delete(Value, ROR(Hash));
      if THashTreeItem<T>(FItems[I]).FFilled = 0 then
      begin
        THashTreeItem<T>(FItems[I]).Free;
        FItems[I] := nil;
      end;
    end
    else
    begin
      PrevLinkedItem := nil;
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          if PrevLinkedItem = nil then
          begin
            FItems[I] := LinkedItem.FNext;
            if FItems[I] = nil then
              Dec(FFilled);
          end
          else
            PrevLinkedItem.FNext := LinkedItem.FNext;
          LinkedItem.FNext := nil;
          THashTrie<T>(FOwner).DestroyItem(LinkedItem.FValue, LinkedItem.FData);
          LinkedItem.Free;
          Exit;
        end;
        PrevLinkedItem := LinkedItem;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

destructor THashTreeItem<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function THashTreeItem<T>.Find(Value, Hash: Cardinal; var Data: T): Boolean;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem<T> then
      Result := THashTreeItem<T>(FItems[I]).Find(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          Data := LinkedItem.FData;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

function THashTreeItem<T>.GetFilled: Integer;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem<T> then
      Inc(Result, THashTreeItem<T>(FItems[I]).GetFilled)
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        Inc(Result);
        LinkedItem := LinkedItem.FNext;
      end;
    end;
end;

function THashTreeItem<T>.Modify(Value, Hash: Cardinal; const Data: T): Boolean;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;

begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem<T> then
      Result := THashTreeItem<T>(FItems[I]).Modify(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;

end;

function THashTreeItem<T>.ROR(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
end;

function THashTreeItem<T>.RORN(Value: Cardinal; Level: Integer): Cardinal;
begin
  Result := Value;
  while Level > 0 do
  begin
    Result := ROR(Result);
    Dec(Level);
  end;
end;

{
function THashTreeItem<T>.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := False;
  for I := 0 to High(FItems) do
    if Assigned(FItems[I]) then
    begin
      if FItems[I] is THashTreeItem<T> then
        Result := THashTreeItem<T>(FItems[I]).Traverse(UserData, UserProc, TraverseProc)
      else
      begin
        LinkedItem := THashLinkedItem<T>(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TraverseProc(UserData, UserProc, LinkedItem.FValue, LinkedItem.FData, Result);
          LinkedItem := LinkedItem.FNext;
        end;
      end;
      if Result then
        Break;
    end;
end;   }

{ THashTrie<T> }

procedure THashTrie<T>.AddDown(Value, Hash: Cardinal; const Data: T);
begin
  FRoot.AddDown(Value, Hash, Data);
end;

procedure THashTrie<T>.Clear;
begin
  FRoot.Clear;
end;

constructor THashTrie<T>.Create;
begin
  inherited Create;
  FRoot := THashTreeItem<T>.Create(Self);
end;

procedure THashTrie<T>.Delete(Value, Hash: Cardinal);
begin
  FRoot.Delete(Value, Hash);
end;

destructor THashTrie<T>.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function THashTrie<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function THashTrie<T>.Find(Value, Hash: Cardinal; var Data: T): Boolean;
begin
  Result := FRoot.Find(Value, Hash, Data);
end;

function THashTrie<T>.GetCount: Integer;
begin
  Result := FRoot.GetFilled;
end;


function THashTrie<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{
procedure THashTrie<T>.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);
begin
  FRoot.Traverse(UserData, UserProc, TraverseProc);
end;   }

{ TStringHashTrie<T> }

procedure TSvStringTrie<T>.Add(const S: string; const Data: T);
var
  Value: PChar;
begin
  Value := StrNew(PChar(S));
  AddDown(Cardinal(Value), HashStr(S), Data);
end;

function TSvStringTrie<T>.CompareValue(Value1, Value2: Cardinal): Boolean;
begin
  if FCaseSensitive then
    Result := StrComp(PChar(Value1), PChar(Value2)) = 0
  else
    Result := StrIComp(PChar(Value1), PChar(Value2)) = 0;
end;

procedure TSvStringTrie<T>.Delete(const S: string);
begin
  inherited Delete(Cardinal(PWideChar(S)), HashStr(S));
end;

procedure TSvStringTrie<T>.DestroyItem(var Value: Cardinal; var Data: T);
begin
  StrDispose(PChar(Value));
  Value := 0;
  {TODO -oLinas -cGeneral : add event on free item}
end;

function TSvStringTrie<T>.Find(const S: string; var Data: T): Boolean;
begin
  Result := Find(Cardinal(PChar(S)), HashStr(S), Data);
end;

function TSvStringTrie<T>.HashStr(const S: string): Cardinal;
begin
  if FCaseSensitive then
    Result := CalcStrCRC32(S)
  else
    Result := CalcStrCRC32(ANSIUpperCase(S));
end;

function TSvStringTrie<T>.HashValue(Value: Cardinal): Cardinal;
begin
  Result := HashStr(PChar(Value));
end;
{
procedure TStringHashTrie<T>.Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc);
begin
  inherited Traverse(UserData, @UserProc, TraverseProc);
end;

procedure TStringHashTrie<T>.Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth);
begin
  inherited Traverse(UserData, @TMethod(UserProc), TraverseMeth);
end;

procedure TStringHashTrie<T>.TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: T; var Done: Boolean);
type
  PTStrHashTraverseMeth = ^TStrHashTraverseMeth;
begin
  PTStrHashTraverseMeth(UserProc)^(UserData, PChar(Value), Data, Done);
end;

procedure TStringHashTrie<T>.TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: T; var Done: Boolean);
begin
  TStrHashTraverseProc(UserProc)(UserData, PChar(Value), Data, Done);
end; }

{ THashTrie<T>.TEnumerator }

constructor THashTrie<T>.TEnumerator.Create(ATrie: THashTrie<T>);
begin
  inherited Create;
  FTrie := ATrie;
  FIndex := 0;
  FRoot := FTrie.FRoot;
  FCurrObj := FRoot.FItems[FIndex];
  FLinkedItem := nil;
  FMax := FTrie.Count;
  FCurrIndex := -1;
end;

function THashTrie<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;

end;

function THashTrie<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function THashTrie<T>.TEnumerator.GetCurrent: T;
begin
  Result := FLinkedItem.FData;
  Inc(FCurrIndex);
end;

function THashTrie<T>.TEnumerator.MoveNext: Boolean;
var
  ix: Integer;
  FOldObj: TObject;
  FOldRoot: THashTreeItem<T>;
 // FOldLinkedItem: THashLinkedItem<T>;
begin
  Result := (FCurrIndex < FMax - 1 ) ;
  if not Result then
    Exit;

  if Assigned(FLinkedItem) then
  begin
    FLinkedItem := FLinkedItem.FNext;
    Result := Assigned(FLinkedItem);
    if Result then
      Exit
    else
    begin
      FCurrObj := FRoot.FItems[FIndex];
    end;
  end;

  if Assigned(FCurrObj) then
  begin
    if FCurrObj is THashTreeItem<T> then
    begin
      FRoot := THashTreeItem<T>(FCurrObj);
      FCurrObj := FRoot.FItems[FIndex];
      FLinkedItem := nil;
      ix := FIndex;
      FOldObj := FCurrObj;
      FOldRoot := FRoot;
    //  FOldLinkedItem := FLinkedItem;
      FIndex := 0;
      Inc(FIndex);
      //do recursion
      Result := MoveNext;
      //restore index
      FIndex := ix;
     // Inc(FIndex);
      FRoot := FOldRoot;
  //    FCurrObj := FOldObj;
    //  FLinkedItem := FOldLinkedItem;
    end
    else
    begin
      FLinkedItem := THashLinkedItem<T>(FCurrObj);
      Inc(FIndex);
      Result := Assigned(FLinkedItem.FNext);

     // FCurrObj := FLinkedItem.FNext;
    end;
  end;
end;

end.
