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
unit SvDesignPatterns;

interface

uses
  SysUtils, Generics.Collections;

type
  EFactoryMethodKeyAlreadyRegisteredException = class(Exception);
  EFactoryMethodKeyNotRegisteredException = class(Exception);

  TFactoryMethodKeyAlreadyRegisteredException = EFactoryMethodKeyAlreadyRegisteredException;
  TFactoryMethodKeyNotRegisteredException = EFactoryMethodKeyNotRegisteredException;

  TFactoryMethod<TBaseType> = reference to function: TBaseType;

  /// <summary>
  /// Factory method class
  /// </summary>
  TFactory<TKey, TBaseType> = class
  strict private
    type
      TFactoryEnumerator = class(TEnumerator<TPair<TKey,TBaseType>>)
      private
        FEnum: TEnumerator<TPair<TKey,TFactoryMethod<TBaseType>>>;
        function GetCurrent: TPair<TKey,TBaseType>;
      protected
        function DoGetCurrent: TPair<TKey,TBaseType>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AEnum: TEnumerator<TPair<TKey,TFactoryMethod<TBaseType>>>);
        destructor Destroy; override;

        property Current: TPair<TKey,TBaseType> read GetCurrent;
        function MoveNext: Boolean;
      end;

  private
    FFactoryMethods: TDictionary<TKey, TFactoryMethod<TBaseType>>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
    procedure RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>);
    procedure UnregisterFactoryMethod(const AKey: TKey);
    procedure UnregisterAll();
    function IsRegistered(const AKey: TKey): Boolean;
    function GetInstance(const AKey: TKey): TBaseType;

    function GetEnumerator: TFactoryEnumerator;
  end;


implementation


{ TFactory<TKey, TBaseType> }

constructor TFactory<TKey, TBaseType>.Create;
begin
  inherited Create();
  FFactoryMethods := TDictionary<TKey, TFactoryMethod<TBaseType>>.Create();
end;

destructor TFactory<TKey, TBaseType>.Destroy;
begin
  FFactoryMethods.Free;
  inherited Destroy;
end;

function TFactory<TKey, TBaseType>.GetCount: Integer;
begin
  Result := FFactoryMethods.Count;
end;

function TFactory<TKey, TBaseType>.GetEnumerator: TFactoryEnumerator;
begin
  Result := TFactoryEnumerator.Create(FFactoryMethods.GetEnumerator);
end;

function TFactory<TKey, TBaseType>.GetInstance(const AKey: TKey): TBaseType;
var
  factoryMethod: TFactoryMethod<TBaseType>;
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');
  factoryMethod := fFactoryMethods.Items[AKey];
  if Assigned(factoryMethod) then
    Result := factoryMethod;
end;

function TFactory<TKey, TBaseType>.IsRegistered(const AKey: TKey): Boolean;
begin
  Result := FFactoryMethods.ContainsKey(AKey);
end;

procedure TFactory<TKey, TBaseType>.RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>);
begin
  if IsRegistered(AKey) then
    raise TFactoryMethodKeyAlreadyRegisteredException.Create('Factory already registered');

  fFactoryMethods.Add(AKey, AFactoryMethod);
end;

procedure TFactory<TKey, TBaseType>.UnregisterAll;
begin
  FFactoryMethods.Clear;
end;

procedure TFactory<TKey, TBaseType>.UnregisterFactoryMethod(const AKey: TKey);
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  FFactoryMethods.Remove(AKey);
end;

{ TFactory<TKey, TBaseType>.TFactoryEnumerator }

constructor TFactory<TKey, TBaseType>.TFactoryEnumerator.Create(
  AEnum: TEnumerator<TPair<TKey, TFactoryMethod<TBaseType>>>);
begin
  inherited Create();
  FEnum := AEnum;
end;

destructor TFactory<TKey, TBaseType>.TFactoryEnumerator.Destroy;
begin
  FEnum.Free;
  inherited Destroy;
end;

function TFactory<TKey, TBaseType>.TFactoryEnumerator.DoGetCurrent: TPair<TKey,TBaseType>;
begin
  Result := GetCurrent;
end;

function TFactory<TKey, TBaseType>.TFactoryEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFactory<TKey, TBaseType>.TFactoryEnumerator.GetCurrent: TPair<TKey,TBaseType>;
var
  factoryMethod: TFactoryMethod<TBaseType>;
begin
  Result.Key := FEnum.Current.Key;
  factoryMethod := FEnum.Current.Value;
  if Assigned(factoryMethod) then
    Result.Value := factoryMethod;
end;

function TFactory<TKey, TBaseType>.TFactoryEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

end.
