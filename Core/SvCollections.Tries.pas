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
unit SvCollections.Tries;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Compact Generic Trie Dictionary
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$R-,O-,STRINGCHECKS OFF}
{$INLINE AUTO}

// uncomment to disable inline asm
{.$DEFINE PUREPASCAL}

// uncomment in order to use FastMM4 memory alignment
{.$Include FastMM4Options.inc}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections,
  SvCollections.BinaryAccess;

type
  TStateIndex = Cardinal;     { change this to Word only if you're certain that
                                the number of states will not exceed 65534. }

{ Note: generic classes do not permit use of locally declared constants. }
const
  MAX_KEY_LENGTH = 1024;      { used only by enumerator classes }

{$IFNDEF Align16Bytes}
  MM_ALIGNMENT   = 8;         { MM aligns to 8 byte boundaries }
{$ELSE}
  MM_ALIGNMENT   = 16;        { MM aligns to 16 byte boundaries }
{$ENDIF}
  MAX_VALUE_SIZE = MM_ALIGNMENT - 1;
  VALUE_MASK     = $ffffffff xor MAX_VALUE_SIZE;

  INVALID_STATE: TStateIndex = High(TStateIndex);

type
  TSvTrie<TKey,TValue> = class;

  PStateIndexArray = ^TStateIndexArray;
  TStateIndexArray = array [0..0] of TStateIndex;

  PSmallDictionaryEntry = ^TSmallDictionaryEntry;
  TSmallDictionaryEntry = packed record
    SetBits: Byte;
    MaskBits: Byte;
  end;
  PDictionaryEntry = ^TDictionaryEntry;
  TDictionaryEntry = packed record
    SetBits: Byte;
    MaskBits: Byte;
    Transitions: array [0..255] of TStateIndex;
  end;

  PDictionaryItems = ^TDictionaryItems;
  TDictionaryItems = array of PDictionaryEntry;

  { Enumerator }
  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { Enumerable }
  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

  { TEnumerable }
  TEnumerable<T> = class(TInterfacedObject, IEnumerable<T>)
  protected
    function GetEnumerator: IEnumerator<T>; virtual; abstract;
  end;

  { TItemEnumeratorBase }
  { Uses pre-order traversal for key enumeration }
  TItemEnumeratorBase = class(TInterfacedPersistent)
  private
    FItems: TDictionaryItems;
    FStack: TStack<TStateIndex>;
    FData: array [0..MAX_KEY_LENGTH-1] of Byte;
    FDataIndex: Integer;
    FCurrentState: TStateIndex;
    FLastState: TStateIndex;
    FFirstMove: Boolean;
    FAscending: Boolean;
  protected
    property CurrentState: TStateIndex read FCurrentState write FCurrentState;
    property LastState: TStateIndex read FLastState write FLastState;
    property Ascending: Boolean read FAscending write FAscending;
    procedure Seek(AData: PByteArray; DataSize: Integer);
  public
    constructor Create(const AItems: TDictionaryItems); virtual;
    destructor Destroy; override;
  end;

  { TItemEnumerator<T> }
  TItemEnumerator<T> = class(TItemEnumeratorBase, IEnumerator<T>)
  public
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean;
  end;

  { TDictionary<TKey,TValue> }
  { Dictionary class based on a trie data structure }
  TSvTrie<TKey,TValue> = class(TInterfacedPersistent, IEnumerable<TPair<TKey,TValue>>)
  private
    type
      PPValue = ^PValue;
      PValue = ^TValue;
  class var
    FBinaryAccess: IBinaryAccess<TKey>;
  private
    FItems: TDictionaryItems;
    FFreeState: TStateIndex;
    FCount: Integer;
    procedure InitializeStartState;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
  protected
    function AddState: TStateIndex;
    procedure RemoveState(Index: TStateIndex);
    procedure AddTransition(Symbol: Byte; S1, S2: TStateIndex);
    procedure RemoveTransition(Symbol: Byte; State: TStateIndex);
    function GetValue(State: TStateIndex): TValue; overload; inline;
    class function GetValue(P: PDictionaryEntry): TValue; overload; inline;
    class function GetKey(Data: Pointer; DataSize: Integer): TKey; inline;
    procedure SetValue(const Index: TStateIndex; const Value: TValue);
    procedure FinalizeState(Index: TStateIndex);
    function GetState(const Key: TKey): TStateIndex;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Pack;
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function Contains(const Key: TKey): Boolean; overload;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean; overload;
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read FCount;

    type
      TPairEnumerator = class(TItemEnumerator<TPair<TKey,TValue>>)
      public
        function GetCurrent: TPair<TKey, TValue>; override;
        property Current: TPair<TKey,TValue> read GetCurrent;
      end;

      TKeyEnumerator = class(TItemEnumerator<TKey>)
      public
        function GetCurrent: TKey; override;
        property Current: TKey read GetCurrent;
      end;

      TValueEnumerator = class(TItemEnumerator<TValue>)
      public
        function GetCurrent: TValue; override;
        property Current: TValue read GetCurrent;
      end;

      TValueCollection = class(TEnumerable<TValue>)
      private
        FDictionary: TSvTrie<TKey,TValue>;
      public
        constructor Create(ADictionary: TSvTrie<TKey,TValue>);
        function GetEnumerator: TValueEnumerator; reintroduce;
      end;

      TKeyCollection = class(TEnumerable<TKey>)
      private
        FDictionary: TSvTrie<TKey,TValue>;
      public
        constructor Create(ADictionary: TSvTrie<TKey,TValue>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
      end;

  private
    FFirst: TKey;
    FLast: TKey;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    procedure InitializeEnumerator(Enumerator: TItemEnumeratorBase);
    function DoGetEnumerator(): TPairEnumerator;
  public
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    function SetRange(const AFirst, ALast: TKey): TSvTrie<TKey,TValue>;
  end;

type
  TLUT256x256 = array [Byte,Byte] of Byte;

var
  PACK_BITS: TLUT256x256;
  UNPACK_BITS: TLUT256x256;

const
  STEP: array [Boolean] of Integer = (-1, 1);
  FILL: array [Boolean] of Integer = ($ff, $00);

function Advance(Item: Pointer; Symbol: Byte): TStateIndex;
procedure ReallocEntry(var P: Pointer; Size: Integer);

implementation

uses
  RTLConsts, Math;

function PackBits(const Mask, X: Integer): Integer;
var
  I, J, S: Integer;
begin
  Result := 0;
  J := 0;
  for I := 0 to 7 do
  begin
    S := 1 shl I;
    if Mask and S <> 0 then
    begin
      Result := Result or (1 shl J) * Ord((X and S) <> 0);
      Inc(J);
    end;
  end;
end;

function UnpackBits(const Mask, X: Integer): Integer;
var
  I, J, S: Integer;
begin
  Result := 0;
  J := 0;
  for I := 0 to 7 do
  begin
    S := 1 shl I;
    if Mask and S <> 0 then
    begin
      Result := Result or S * Ord(X and (1 shl J) <> 0);
      Inc(J);
    end;
  end;
end;

procedure InitializeTables;
var
  I, J: Integer;
begin
  for I := Low(Byte) to High(Byte) do
    for J := Low(Byte) to High(Byte) do
    begin
      PACK_BITS[I, J] := PackBits(I, J);
      UNPACK_BITS[I, J] := UnpackBits(I, J);
    end;
end;

//============================================================================//

function HasValue(P: Pointer): Boolean; inline;
begin
  Result := Integer(P) and MAX_VALUE_SIZE <> 0;
end;

function GetValuePtr(P: Pointer): Pointer; inline;
begin
  Result := Pointer(Integer(P) and VALUE_MASK);
end;

function LastIndex(MaskBits: Integer): Integer; inline;
begin
  Result := PACK_BITS[MaskBits, $ff];
end;

procedure ReallocEntry(var P: Pointer; Size: Integer); inline;
var
  Offset: Integer;
begin
  Offset := Integer(P) and MAX_VALUE_SIZE;
  Size := Size + Offset;
  Integer(P) := Integer(P) and VALUE_MASK;
  ReallocMem(P, Size);
  Integer(P) := Integer(P) + Offset;
end;

function Advance(Item: Pointer; Symbol: Byte): TStateIndex;
const
  IndexSize = SizeOf(TStateIndex);
  ByteOffset = SizeOf(TSmallDictionaryEntry);
asm
// eax = item
// edx = symbol
// ecx = state

// if Symbol or MaskBits = SetBits then
        movzx ecx,word ptr [eax]           // cl = setbits, ch = maskbits
        mov dh,ch                          // dh = maskbits
        or  dh,dl                          // symbol or maskbits
        xor dh,cl                          // symbol or maskbits = setbits?
        jnz @@exit
// Result := Transitions[PACK_BITS[MaskBits, Symbol]];
        mov cl,dl
        movzx edx,[ecx+PACK_BITS]          // edx = PACK_BITS[MaskBits, Symbol]
        mov eax,[eax+edx*IndexSize+ByteOffset]       // eax = Transitions[X]
        ret
@@exit:
        mov eax,Cardinal(INVALID_STATE)
end;


{ TDictionary }

procedure TSvTrie<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
var
  I, J: Integer;
  State, NewState: TStateIndex;
  Data: PByteArray;
  N: Integer;
begin
  State := 0;
  FBinaryAccess.DataFromValue(Key, Pointer(Data), N);

  for I := 0 to N - 1 do
  begin
    NewState := Advance(FItems[State], Data[I]);
    if NewState = INVALID_STATE then
    begin
      for J := I to N - 2 do
      begin
        NewState := AddState;
        AddTransition(Data[J], State, NewState);
        State := NewState;
      end;
      NewState := AddState;
      SetValue(NewState, Value);

      AddTransition(Data[N - 1], State, NewState);
      Exit;
    end;
    State := NewState;
  end;
  // already existing state? => replace value
  SetValue(State, Value);
end;

function TSvTrie<TKey,TValue>.AddState: TStateIndex;
var
  NewEntry: PSmallDictionaryEntry;
begin
  if FFreeState = INVALID_STATE then
  begin
    Result := Length(FItems);
    SetLength(FItems, Result + 1);
  end
  else
  begin
    Result := FFreeState;
    FFreeState := TStateIndex(FItems[FFreeState]);
  end;

  GetMem(NewEntry, SizeOf(TSmallDictionaryEntry));
  FItems[Result] := PDictionaryEntry(NewEntry);
  PWord(NewEntry)^ := $ff00;
end;

procedure TSvTrie<TKey,TValue>.AddTransition(Symbol: Byte; S1, S2: TStateIndex);
var
  E: PDictionaryEntry;
  M, X, I, J, Diff, OldMaskBits, OldSetBits, V: Integer;
begin
  E := FItems[S1];

  OldMaskBits := E.MaskBits;
  OldSetBits := E.SetBits;

  // first symbol?
  if PWord(E)^ = $ff00 then
  begin
    OldMaskBits := 0;
    E.MaskBits := 0;
    E.SetBits := Symbol;
    ReallocEntry(Pointer(E), SizeOf(TSmallDictionaryEntry) + SizeOf(TStateIndex));
    FItems[S1] := E;
  end
  else
  begin
    { find bits that can be both 0 and 1 }
    // alt. E.MaskBits := E.SetBits and E.UnsetBits;
    E.MaskBits := E.MaskBits or (E.SetBits xor Symbol);
    E.SetBits := E.SetBits or Symbol;
  end;

  M := PACK_BITS[E.MaskBits, $ff];     { M = High(E.Transitions) }
  X := PACK_BITS[E.MaskBits, Symbol];

  { resize transition table? }
  if E.MaskBits <> OldMaskBits then
  begin
    J := PACK_BITS[OldMaskBits, $ff];

    { reallocate + update entry positions }
    ReallocEntry(Pointer(E), SizeOf(TSmallDictionaryEntry) + (M + 1) * SizeOf(TStateIndex));
    FItems[S1] := E;

    Diff := PACK_BITS[E.MaskBits, E.MaskBits xor OldMaskBits];
    V := PACK_BITS[E.MaskBits, OldSetBits] and Diff;
    for I := M downto 0 do
    begin
      if I and Diff = V then
      begin
        E.Transitions[I] := E.Transitions[J];
        Dec(J);
      end
      else
        E.Transitions[I] := INVALID_STATE;
    end;
  end;
  E.Transitions[X] := S2;
end;

procedure TSvTrie<TKey,TValue>.FinalizeState(Index: TStateIndex);
var
  P: Pointer;
begin
  P := FItems[Index];
  if HasValue(P) then
  begin
    P := Pointer(Integer(P) and VALUE_MASK);
    if SizeOf(TValue) <= MAX_VALUE_SIZE then
    begin
      Finalize(PValue(P)^);
    end
    else
    begin
      Finalize(PPValue(P)^^);
      Dispose(PPValue(P)^);
    end;
  end;
  FreeMem(P);
end;

procedure TSvTrie<TKey,TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
  begin
    FinalizeState(I);
  end;
  FItems := nil;
  InitializeStartState;
end;

function TSvTrie<TKey,TValue>.Contains(const Key: TKey): Boolean;
var
  Value: TValue;
begin
  Result := TryGetValue(Key, Value);
end;

constructor TSvTrie<TKey,TValue>.Create;
begin
  InitializeStartState;
  FFreeState := INVALID_STATE;
  FBinaryAccess := TBinaryAccess<TKey>.Default;
  FFirst := Default(TKey);
  FLast := Default(TKey);
end;

destructor TSvTrie<TKey,TValue>.Destroy;
begin
  Clear;
  FKeyCollection.Free;
  FValueCollection.Free;
  inherited;
end;

function TSvTrie<TKey, TValue>.DoGetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(FItems);
  InitializeEnumerator(Result);
end;

function TSvTrie<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey,TValue>>;
begin
  Result := DoGetEnumerator;
end;

function TSvTrie<TKey,TValue>.GetItem(const Key: TKey): TValue;
begin
  if not TryGetValue(Key, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

class function TSvTrie<TKey, TValue>.GetKey(Data: Pointer; DataSize: Integer): TKey;
begin
  FBinaryAccess.ValueFromData(Data, DataSize, Result);
end;

function TSvTrie<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  Result := FKeyCollection;
end;

function TSvTrie<TKey, TValue>.GetState(const Key: TKey): TStateIndex;
var
  I, N: Integer;
  State, NewState: TStateIndex;
  Data: PByteArray;
begin
  State := 0;
  FBinaryAccess.DataFromValue(Key, Pointer(Data), N);

  for I := 0 to N - 1 do
  begin
    NewState := Advance(FItems[State], Data[I]);
    if NewState = INVALID_STATE then Break;
    State := NewState;
  end;
  Exit(State);
end;

class function TSvTrie<TKey, TValue>.GetValue(
  P: PDictionaryEntry): TValue;
begin
  if SizeOf(TValue) <= MAX_VALUE_SIZE then
    Result := PValue(Integer(P) and VALUE_MASK)^
  else
    Result := PPValue(Integer(P) and VALUE_MASK)^^;
end;

function TSvTrie<TKey, TValue>.GetValues: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  Result := FValueCollection;
end;

function TSvTrie<TKey,TValue>.GetValue(State: TStateIndex): TValue;
begin
  Result := GetValue(FItems[State]);
end;

function TSvTrie<TKey,TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  I, N: Integer;
  State: TStateIndex;
  Data: PByteArray;
begin
  Value := Default(TValue);
  State := 0;
  FBinaryAccess.DataFromValue(Key, Pointer(Data), N);
  for I := 0 to N - 1 do
  begin
    State := Advance(FItems[State], Data[I]);
    if State = INVALID_STATE then Exit(False);
  end;
  if not HasValue(FItems[State]) then Exit(False);
  Value := GetValue(State);
  Result := True;
end;

procedure TSvTrie<TKey, TValue>.InitializeEnumerator(
  Enumerator: TItemEnumeratorBase);
var
  LeftData, RightData: Pointer;
  LeftDataSize, RightDataSize: Integer;
  I: Integer;
begin
  FBinaryAccess.DataFromValue(FFirst, LeftData, LeftDataSize);
  FBinaryAccess.DataFromValue(FLast, RightData, RightDataSize);

  Enumerator.Seek(PByteArray(LeftData), LeftDataSize);
  Enumerator.LastState := GetState(FLast);
  Enumerator.Ascending := BinaryCompare(LeftData, RightData, Min(LeftDataSize, RightDataSize)) <= 0;
end;

procedure TSvTrie<TKey,TValue>.InitializeStartState;
var
  P: PSmallDictionaryEntry;
begin
  GetMem(P, SizeOf(TSmallDictionaryEntry));
  P.SetBits := 0;
  P.MaskBits := $ff;
  SetLength(FItems, 1);
  FItems[0] := PDictionaryEntry(P);
end;

procedure TSvTrie<TKey,TValue>.Pack;
begin
// TODO: remove unused states & reindex existing nodes
end;

procedure TSvTrie<TKey,TValue>.Remove(const Key: TKey);
var
  I, J, N, SavedI: Integer;
  State, SavedState: TStateIndex;
  Data: PByteArray;
begin
  State := 0;
  FBinaryAccess.DataFromValue(Key, Pointer(Data), N);
  for I := 0 to N - 2 do
  begin
    State := Advance(FItems[State], Data[I]);
    if State = INVALID_STATE then Exit;
    // MaskBits is zero only when there is exactly one transition
    if (FItems[State].MaskBits <> 0) or HasValue(FItems[State]) then
    begin
      SavedState := State;
      SavedI := I;
    end;
  end;
  if Advance(FItems[State], Data[N - 1]) = INVALID_STATE then Exit;

  // SavedState is the last state with multiple transitions -- delete remaining states/transitions
  RemoveTransition(Data[SavedI], SavedState);

  // no multiple transition for the remaining states
  for I := SavedI + 1 to N - 1 do
  begin
    State := Advance(FItems[State], Data[I]);
    RemoveState(State);
  end;
end;

procedure TSvTrie<TKey,TValue>.RemoveState(Index: TStateIndex);
begin
  FinalizeState(Index);
  FItems[Index] := Pointer(FFreeState);
  FFreeState := Index;
end;

procedure TSvTrie<TKey,TValue>.RemoveTransition(Symbol: Byte; State: TStateIndex);
var
  E: PDictionaryEntry;
  X, Y, I, J, V, M, OldMaskBits, OldSetBits, Diff: Integer;
begin
  E := FItems[State];
  OldMaskBits := E.MaskBits;
  OldSetBits := E.SetBits;
  X := PACK_BITS[OldMaskBits, Symbol];
  E.Transitions[X] := INVALID_STATE;
  M := PACK_BITS[OldMaskBits, $ff];
  X := 0;
  Y := 0;
  for I := 0 to M do
  begin
    if E.Transitions[I] <> INVALID_STATE then
    begin
      X := X or I;
      Y := Y or (I xor $ff);
    end;
  end;
  X := UNPACK_BITS[OldMaskBits, X];
  Y := UNPACK_BITS[OldMaskBits, Y];
  E.SetBits := X;
  E.MaskBits := X and Y;

  M := PACK_BITS[E.MaskBits, $ff];     { M = High(E.Transitions) }
  X := PACK_BITS[E.MaskBits, Symbol];

  if E.MaskBits <> OldMaskBits then
  begin
    Diff := PACK_BITS[E.MaskBits, E.MaskBits xor OldMaskBits];
    V := PACK_BITS[E.MaskBits, OldSetBits] and Diff;

    { update entry positions }
    J := 0;
    for I := 0 to M do
    begin
      if I and Diff = V then
      begin
        E.Transitions[J] := E.Transitions[I];
        Inc(J);
      end;
    end;

    { reallocate }
    ReallocMem(E, SizeOf(TSmallDictionaryEntry) + (M + 1) * SizeOf(Integer));
    FItems[State] := E;
  end;
end;

procedure TSvTrie<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Add(Key, Value);
end;

function TSvTrie<TKey, TValue>.SetRange(const AFirst,
  ALast: TKey): TSvTrie<TKey,TValue>;
begin
  FFirst := AFirst;
  FLast := ALast;
  Result := Self;
end;

procedure TSvTrie<TKey, TValue>.SetValue(const Index: TStateIndex;
  const Value: TValue);
var
  Src, Dst: PDictionaryEntry;
  Size: Integer;
  P: PValue;
begin
  Src := FItems[Index];
  if not HasValue(Src) then
  begin
    if PWord(Src)^ = $ff00 then
      Size := 0
    else
      Size := (PACK_BITS[Src.MaskBits,$ff] + 1) * SizeOf(TStateIndex);

    if SizeOf(TValue) <= MAX_VALUE_SIZE then
    begin
      GetMem(Dst, Size + SizeOf(TValue) + SizeOf(TSmallDictionaryEntry));
      Inc(PByte(Dst), SizeOf(TValue));
      FItems[Index] := Dst;

      Move(Src^, Dst^, SizeOf(TSmallDictionaryEntry) + Size);
      FreeMem(Src);
    end
    else
    begin
      GetMem(Dst, Size + SizeOf(PValue) + SizeOf(TSmallDictionaryEntry));
      New(P);
      PPValue(Dst)^ := P;

      Inc(PByte(Dst), SizeOf(PValue));
      FItems[Index] := Dst;

      Move(Src^, Dst^, SizeOf(TSmallDictionaryEntry) + Size);
      FreeMem(Src);
    end;

    Inc(FCount);
  end
  else
    Dst := Src;

  if SizeOf(TValue) <= MAX_VALUE_SIZE then
    PValue(GetValuePtr(Dst))^ := Value
  else
    PPValue(GetValuePtr(Dst))^^ := Value;
end;


{ TDictionary<TKey, TValue>.TPairEnumerator }

function TSvTrie<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result.Key := TSvTrie<TKey,TValue>.GetKey(@FData, FDataIndex);
  Result.Value := TSvTrie<TKey,TValue>.GetValue(FItems[FCurrentState]);
end;

{ TDictionary<TKey, TValue>.TValueCollection }

constructor TSvTrie<TKey, TValue>.TValueCollection.Create(
  ADictionary: TSvTrie<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TSvTrie<TKey, TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(FDictionary.FItems);
  FDictionary.InitializeEnumerator(Result);
end;

{ TDictionary<TKey, TValue>.TKeyCollection }

constructor TSvTrie<TKey, TValue>.TKeyCollection.Create(
  ADictionary: TSvTrie<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TSvTrie<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FDictionary.FItems);
  FDictionary.InitializeEnumerator(Result);
end;

{ TDictionary<TKey, TValue>.TKeyEnumerator }

function TSvTrie<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := TSvTrie<TKey,TValue>.GetKey(@FData, FDataIndex);
end;

{ TDictionary<TKey, TValue>.TValueEnumerator }

function TSvTrie<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := TSvTrie<TKey, TValue>.GetValue(FItems[FCurrentState]);
end;


{ TItemEnumerator }

constructor TItemEnumeratorBase.Create(const AItems: TDictionaryItems);
begin
  inherited Create;
  FItems := AItems;
  FStack := TStack<TStateIndex>.Create;
  FDataIndex := 0;
  FCurrentState := 0;
  FLastState := 0;
  FFirstMove := True;
  FAscending := True;
end;

destructor TItemEnumeratorBase.Destroy;
begin
  FStack.Free;
  inherited;
end;

function TItemEnumerator<T>.MoveNext: Boolean;
var
  State: TStateIndex;
  E: PDictionaryEntry;
  I, N: Integer;
{ uncomment below for F2084 Internal Error: AV08E7071A-R0000000C-0 }
//label
//  Next;
begin
  if FFirstMove then
  begin
    FFirstMove := False;
    E := FItems[FCurrentState];
    if HasValue(E) then Exit(True);
  end;

  State := FCurrentState;
  E := FItems[State];
  I := PACK_BITS[E.MaskBits, FData[FDataIndex]];

  repeat
    if PWord(E)^ = $ff00 then // state has no transitions --> step out
    begin
      // goto Next;
      N := 0;
      I := 1;
    end
    else
    begin
      // find valid transition
      N := PACK_BITS[E.MaskBits, $ff];
      while (I and N = I) and (E.Transitions[I] = INVALID_STATE) do
        Inc(I, STEP[FAscending]);
    end;

    if (I and N = I) then
    begin { step in (E.Transitions[I] = INVALID_STATE) }
      FStack.Push(State);

      FData[FDataIndex] := UNPACK_BITS[E.MaskBits, I] or (E.SetBits and (E.MaskBits xor $ff));
      Inc(FDataIndex);

      State := E.Transitions[I];
      E := FItems[State];
      FCurrentState := State;

      if HasValue(E) then Exit(True);

      I := PACK_BITS[E.MaskBits, FILL[FAscending]];
    end
    else { step out (I > N) }
    begin
//Next:
      FData[FDataIndex] := FILL[FAscending];
      Dec(FDataIndex);
      if (State = LastState) or (FDataIndex < 0) then
        Exit(False)
      else
      begin
        State := FStack.Pop;
        E := FItems[State];
        I := PACK_BITS[E.MaskBits, FData[FDataIndex]];
        Inc(I, STEP[FAscending]);    // advance symbol index
      end;
    end;

  until False;
end;

procedure TItemEnumeratorBase.Seek(AData: PByteArray; DataSize: Integer);
var
  I: Integer;
  State: TStateIndex;
begin
  State := 0;
  FDataIndex := DataSize;
  for I := 0 to DataSize - 1 do
  begin
    FStack.Push(State);
    State := Advance(FItems[State], AData[I]);
    if State = INVALID_STATE then
    begin
      State := FStack.Pop;
      FDataIndex := I;
      Break;
    end;
    FData[I] := AData[I];
  end;
  FCurrentState := State;
end;

initialization
  InitializeTables;

end.
