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
unit SvCollections.BinaryAccess;

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

interface

{$R-,O-,STRINGCHECKS OFF}

{.$DEFINE PUREPASCAL}

uses
  TypInfo, SysUtils;

type
  IBinaryAccess<T> = interface
  ['{90E4F17E-250C-4985-9D47-FC1F4D820044}']
    procedure DataFromValue(const Value: T; out Data: Pointer; out DataSize: Integer);
    procedure ValueFromData(Data: Pointer; DataSize: Integer; out Value: T);
  end;

  TBinaryAccess<T> = class(TInterfacedObject, IBinaryAccess<T>)
  public
    class function Default: IBinaryAccess<T>;
    procedure DataFromValue(const Value: T; out Data: Pointer; out DataSize: Integer); virtual; abstract;
    procedure ValueFromData(Data: Pointer; DataSize: Integer; out Value: T); virtual; abstract;
  end;

function _LookupVtableInfo(info: PTypeInfo; size: Integer): Pointer;

implementation

uses
  Windows;

type
  PSimpleInstance = ^TSimpleInstance;
  TSimpleInstance = record
    Vtable: Pointer;
    RefCount: Integer;
    Size: Integer;
  end;

function MakeInstance(vtable: Pointer; sizeField: Integer): Pointer;
var
  inst: PSimpleInstance;
begin
  GetMem(inst, SizeOf(inst^));
  inst^.Vtable := vtable;
  inst^.RefCount := 0;
  inst^.Size := sizeField;
  Result := inst;
end;

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function MemAddref(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := InterlockedIncrement(inst^.RefCount);
end;

function MemRelease(inst: PSimpleInstance): Integer; stdcall;
begin
  Result := InterlockedDecrement(inst^.RefCount);
  if Result = 0 then
    FreeMem(inst);
end;

// Binary

procedure DataFromValue_Binary(Inst: PSimpleInstance; const Value; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value;
  DataSize := Inst^.Size;
end;

procedure ValueFromData_Binary(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value);
begin
  Move(Data^, Value, DataSize);
end;

const
  BinaryAccess_Vtable_Binary: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @DataFromValue_Binary,
    @ValueFromData_Binary
  );

// I1, I2, I4, I8
procedure DataFromValue_I1(Inst: Pointer; const Value: Byte; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value;
  DataSize := 1;
end;

procedure DataFromValue_I2(Inst: Pointer; const Value: Byte; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value;
  DataSize := 2;
end;

procedure DataFromValue_I4(Inst: Pointer; const Value: Byte; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value;
  DataSize := 4;
end;

procedure DataFromValue_I8(Inst: Pointer; const Value: Byte; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value;
  DataSize := 8;
end;

procedure ValueFromData_I1(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: Byte);
begin
  Value := PByte(Data)^;
end;

procedure ValueFromData_I2(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: Word);
begin
  Value := PWord(Data)^;
end;

procedure ValueFromData_I4(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: Cardinal);
begin
  Value := PCardinal(Data)^;
end;

procedure ValueFromData_I8(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: Cardinal);
begin
  Value := PUInt64(Data)^;
end;

const
  BinaryAccess_Vtable_I1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_I1,
    @ValueFromData_I1
  );

  BinaryAccess_Vtable_I2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_I2,
    @ValueFromData_I2
  );

  BinaryAccess_Vtable_I4: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_I4,
    @ValueFromData_I4
  );

  BinaryAccess_Vtable_I8: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_I8,
    @ValueFromData_I8
  );

  BinaryAccess_Instance_I1: Pointer = @BinaryAccess_Vtable_I1;
  BinaryAccess_Instance_I2: Pointer = @BinaryAccess_Vtable_I2;
  BinaryAccess_Instance_I4: Pointer = @BinaryAccess_Vtable_I4;
  BinaryAccess_Instance_I8: Pointer = @BinaryAccess_Vtable_I8;

function BinaryAccess_Selector_Binary(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    1: Result := @BinaryAccess_Instance_I1;
    2: Result := @BinaryAccess_Instance_I2;
    4: Result := @BinaryAccess_Instance_I4;
    8: Result := @BinaryAccess_Instance_I8;
  else
    Result := MakeInstance(@BinaryAccess_Vtable_Binary, size);
  end;
end;


// PStrings

type
  TPS1 = string[1];
  TPS2 = string[2];
  TPS3 = string[3];

procedure DataFromValue_PS1(Inst: Pointer; const Value: TPS1; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := 1;
end;

procedure ValueFromData_PS1(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: TPS1);
begin
  Value[1] := PAnsiChar(Data)^;
end;

procedure DataFromValue_PS2(Inst: Pointer; const Value: TPS1; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := 2;
end;

procedure ValueFromData_PS2(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: TPS1);
begin
  PWord(@Value[1])^ := PWord(Data)^;
end;

procedure DataFromValue_PS3(Inst: Pointer; const Value: TPS1; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := 3;
end;

procedure ValueFromData_PS3(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: TPS1);
begin
  Move(Data^, Value[1], 3);
end;

procedure DataFromValue_PSn(Inst: Pointer; const Value: OpenString; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := Length(Value);
end;

procedure ValueFromData_PSn(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: OpenString);
begin
  SetLength(Value, DataSize);
  Move(Data^, Value[1], DataSize);
end;

const
  BinaryAccess_Vtable_PS1: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_PS1,
    @ValueFromData_PS1
  );
  BinaryAccess_Instance_PS1: Pointer = @BinaryAccess_Vtable_PS1;

  BinaryAccess_Vtable_PS2: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_PS2,
    @ValueFromData_PS2
  );
  BinaryAccess_Instance_PS2: Pointer = @BinaryAccess_Vtable_PS2;

  BinaryAccess_Vtable_PS3: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_PS3,
    @ValueFromData_PS3
  );
  BinaryAccess_Instance_PS3: Pointer = @BinaryAccess_Vtable_PS3;

  BinaryAccess_Vtable_PSn: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_PSn,
    @ValueFromData_PSn
  );
  BinaryAccess_Instance_PSn: Pointer = @BinaryAccess_Vtable_PSn;

function BinaryAccess_Selector_String(info: PTypeInfo; size: Integer): Pointer;
begin
  case size of
    2: Result := @BinaryAccess_Instance_PS1;
    3: Result := @BinaryAccess_Instance_PS2;
    4: Result := @BinaryAccess_Instance_PS3;
  else
    Result := @BinaryAccess_Instance_PSn;
  end;
end;


// LString

{$IFNDEF PUREPASCAL}
procedure DataFromValue_LString(Inst: Pointer; const Value: AnsiString; out Data: Pointer; out DataSize: Integer);
asm
    mov  [ecx],edx
    test edx,edx
    jz   @1
    mov  edx,[edx-4]
@1: mov  eax,[DataSize]
    mov  [eax],edx
end;
{$ELSE}
procedure DataFromValue_LString(Inst: Pointer; const Value: AnsiString; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := Length(Value);
end;
{$ENDIF}

procedure ValueFromData_LString(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: AnsiString);
begin
  SetLength(Value, DataSize);
  Move(Data^, Value[1], DataSize);
end;

const
  BinaryAccess_Vtable_LString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_LString,
    @ValueFromData_LString
  );
  BinaryAccess_Instance_LString: Pointer = @BinaryAccess_Vtable_LString;


// WString

procedure DataFromValue_WString(Inst: Pointer; const Value: WideString; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := Length(Value) * SizeOf(WideChar);
end;

procedure ValueFromData_WString(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: WideString);
var
  L: Integer;
begin
  L := DataSize div SizeOf(Char);
  SetLength(Value, L);
  Move(Data^, Value[1], DataSize);
end;

const
  BinaryAccess_Vtable_WString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_WString,
    @ValueFromData_WString
  );
  BinaryAccess_Instance_WString: Pointer = @BinaryAccess_Vtable_WString;


// UString

procedure DataFromValue_UString(Inst: Pointer; const Value: UnicodeString; out Data: Pointer; out DataSize: Integer);
begin
  Data := @Value[1];
  DataSize := Length(Value) * SizeOf(Char);
end;

procedure ValueFromData_UString(Inst: Pointer; const Data: Pointer; const DataSize: Integer; out Value: UnicodeString);
var
  L: Integer;
begin
  L := DataSize div SizeOf(Char);
  SetLength(Value, L);
  Move(Data^, Value[1], DataSize);
end;

const
  BinaryAccess_Vtable_UString: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @DataFromValue_UString,
    @ValueFromData_UString
  );
  BinaryAccess_Instance_UString: Pointer = @BinaryAccess_Vtable_UString;


// DynArray

function DynLen(Arr: Pointer): Longint; inline;
begin
  if Arr = nil then
    Result := 0
  else
    Result := PLongint(PByte(Arr) - SizeOf(Longint))^;
end;

procedure DataFromValue_DynArray(Inst: PSimpleInstance; const Value: Pointer; out Data: Pointer; out DataSize: Integer);
begin
  Data := Value;
  DataSize := DynLen(Value) * Inst^.Size;
end;

procedure ValueFromData_DynArray(Inst: PSimpleInstance; const Data: Pointer; const DataSize: Integer; out Value: Pointer);
var
  p: Pointer absolute Value;
  newLength, neededSize: Integer;
begin
  newLength := DataSize div Inst^.Size;
  if p <> nil then
    Dec(PLongint(p), 2);

  neededSize := DataSize;
  Inc(neededSize, Sizeof(Longint)*2);

  if (p = nil) or (PLongInt(p)^ = 1) then
  begin
    ReallocMem(p, neededSize);
  end
  else
  begin
    Dec(PLongint(p)^);
    GetMem(p, neededSize);
  end;
  PLongint(p)^ := 1;
  Inc(PLongint(p));
  PLongint(p)^ := newLength;
  Inc(PLongint(p));
  Move(Data^, P^, DataSize);
end;

const
  BinaryAccess_Vtable_DynArray: array[0..4] of Pointer =
  (
    @NopQueryInterface,
    @MemAddref,
    @MemRelease,
    @DataFromValue_DynArray,
    @ValueFromData_DynArray
  );

function BinaryAccess_Selector_DynArray(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := MakeInstance(@BinaryAccess_Vtable_DynArray, GetTypeData(info)^.elSize);
end;

function _LookupVtableInfo(info: PTypeInfo; size: Integer): Pointer;
begin
  Result := nil;
  if info <> nil then
  begin
    case info^.Kind of
      tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkClass,
      tkMethod, tkWChar, tkRecord, tkInterface, tkClassRef, tkPointer,
      tkProcedure, tkArray:
        Result := BinaryAccess_Selector_Binary(info, size);
      tkString: Result := BinaryAccess_Selector_String(info, size);
      tkLString: Result := @BinaryAccess_Instance_LString;
      tkWString: Result := @BinaryAccess_Instance_WString;
      tkUString: Result := @BinaryAccess_Instance_UString;
      tkDynArray: Result := BinaryAccess_Selector_DynArray(info, size);
    end;
  end
  else
  begin
    Result := BinaryAccess_Selector_Binary(info, size);
  end;
end;

{ TBinaryAccess<T> }

class function TBinaryAccess<T>.Default: IBinaryAccess<T>;
begin
  Result := IBinaryAccess<T>(_LookupVtableInfo(TypeInfo(T), SizeOf(T)));
end;

end.
