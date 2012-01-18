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
  SysUtils, Generics.Collections, Rtti;

type
  EFactoryMethodKeyAlreadyRegisteredException = class(Exception);
  EFactoryMethodKeyNotRegisteredException = class(Exception);

  TFactoryMethodKeyAlreadyRegisteredException = EFactoryMethodKeyAlreadyRegisteredException;
  TFactoryMethodKeyNotRegisteredException = EFactoryMethodKeyNotRegisteredException;

  TFactoryMethod<TBaseType> = reference to function: TBaseType;

  /// <summary>
  /// Abstract factory class
  /// </summary>
  TAbstractFactory<TKey, TBaseType> = class abstract
  strict private
    type
      TFactoryEnumerator = class(TEnumerator<TPair<TKey,TBaseType>>)
      private
        FFactory: TAbstractFactory<TKey, TBaseType>;
        FEnum: TEnumerator<TPair<TKey,TFactoryMethod<TBaseType>>>;
        function GetCurrent: TPair<TKey,TBaseType>;
      protected
        function DoGetCurrent: TPair<TKey,TBaseType>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AFactory: TAbstractFactory<TKey, TBaseType>);
        destructor Destroy; override;

        property Current: TPair<TKey,TBaseType> read GetCurrent;
        function MoveNext: Boolean;
      end;
  private
    FFactoryMethods: TDictionary<TKey, TFactoryMethod<TBaseType>>;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  
    /// <summary>
    /// Registers new factory method. If AFactoryMethod is nil, factory will use 
    ///   parameterless constructor
    /// </summary>
    /// <param name="AKey">Key to identify the element</param>
    /// <param name="AFactoryMethod">Anonymous method which returns the correct element</param>
    procedure RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>); virtual;
    procedure UnregisterFactoryMethod(const AKey: TKey); virtual;
    procedure UnregisterAll(); virtual;
    function IsRegistered(const AKey: TKey): Boolean; virtual;

    class function CreateElement(): TBaseType;
    
    function GetEnumerator: TFactoryEnumerator;
  protected
    function GetInstance(const AKey: TKey): TBaseType; virtual; abstract;
  public
    /// <summary>
    /// Count of registered factory methods
    /// </summary>
    property Count: Integer read GetCount;
  end;
  
  /// <summary>
  /// Factory method class
  /// </summary>
  TFactory<TKey, TBaseType> = class(TAbstractFactory<TKey, TBaseType>)
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    /// Gets instance of TBaseType
    /// </summary>
    /// <remarks>
    /// AFactoryMethod will be invoked on each and every instance retrieval. 
    /// </remarks>
    /// <param name="AKey">Key of an element</param>
    /// <returns>Instance of TBaseType</returns>
    function GetInstance(const AKey: TKey): TBaseType; override;
  end;

  /// <summary>
  /// Multiton pattern. 
  /// </summary>
  TMultiton<TKey, TBaseType> = class(TAbstractFactory<TKey, TBaseType>)
  private
    FLifetimeWatcher: TDictionary<TKey,TValue>;
    FOwnsObjects: Boolean;

    procedure ClearObject(const AValue: TValue);
    procedure ClearObjects();
  public
    constructor Create(AOwnsObjects: Boolean = False); reintroduce; overload; 
    destructor Destroy; override;
    /// <summary>
    /// Registers new factory method. If AFactoryMethod is nil, factory will use an instance created 
    /// with parameterless constructor
    /// </summary>
    /// <param name="AKey">Key to identify the element</param>
    /// <param name="AFactoryMethod">Anonymous method which returns the correct element</param>
    procedure RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>); override;
    procedure UnregisterFactoryMethod(const AKey: TKey); override;
    procedure UnregisterAll(); override;
    /// <summary>
    /// Gets instance of TBaseType
    /// </summary>
    /// <remarks>
    /// AFactoryMethod will be invoked only on first instance retrieval. Further retrievals 
    ///  will return the same instance as it was returned in AFactoryMethod
    /// </remarks>
    /// <param name="AKey">Key of an element</param>
    /// <returns>Instance of TBaseType</returns>
    function GetInstance(const AKey: TKey): TBaseType; override;
    /// <summary>
    /// If multiton owns object , it will try to free them.
    /// </summary>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TAbstractFactory<TKey, TBaseType> }

constructor TAbstractFactory<TKey, TBaseType>.Create;
begin
  inherited Create();
  FFactoryMethods := TDictionary<TKey, TFactoryMethod<TBaseType>>.Create();
end;

class function TAbstractFactory<TKey, TBaseType>.CreateElement: TBaseType;
var
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  rType := TRttiContext.Create.GetType(TypeInfo(TBaseType));
  if rType.IsInstance then
  begin
    for AMethCreate in rType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 0) then
      begin
        instanceType := rType.AsInstance;

        Result := AMethCreate.Invoke(instanceType.MetaclassType, []).AsType<TBaseType>;

        Break;
      end;
    end;
  end;
end;

destructor TAbstractFactory<TKey, TBaseType>.Destroy;
begin
  FFactoryMethods.Free;
  inherited Destroy;
end;

function TAbstractFactory<TKey, TBaseType>.GetCount: Integer;
begin
  Result := FFactoryMethods.Count;
end;

function TAbstractFactory<TKey, TBaseType>.GetEnumerator: TFactoryEnumerator;
begin
  Result := TFactoryEnumerator.Create(Self);
end;

function TAbstractFactory<TKey, TBaseType>.IsRegistered(const AKey: TKey): Boolean;
begin
  Result := FFactoryMethods.ContainsKey(AKey);
end;

procedure TAbstractFactory<TKey, TBaseType>.RegisterFactoryMethod(const AKey: TKey;
  AFactoryMethod: TFactoryMethod<TBaseType>);
begin
  if IsRegistered(AKey) then
    raise TFactoryMethodKeyAlreadyRegisteredException.Create('Factory already registered');

  FFactoryMethods.Add(AKey, AFactoryMethod);
end;

procedure TAbstractFactory<TKey, TBaseType>.UnregisterAll;
begin
  FFactoryMethods.Clear;
end;

procedure TAbstractFactory<TKey, TBaseType>.UnregisterFactoryMethod(const AKey: TKey);
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  FFactoryMethods.Remove(AKey);
end;

{ TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator }

constructor TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.Create(
  AFactory: TAbstractFactory<TKey, TBaseType>);
begin
  inherited Create();
  FFactory := AFactory;
  FEnum := FFactory.FFactoryMethods.GetEnumerator;
end;

destructor TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.Destroy;
begin
  FEnum.Free;
  inherited Destroy;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.DoGetCurrent: TPair<TKey, TBaseType>;
begin
  Result := GetCurrent;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.GetCurrent: TPair<TKey, TBaseType>;
begin
  Result.Key := FEnum.Current.Key;
  Result.Value := FFactory.GetInstance(Result.Key);
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

{ TFactory<TKey, TBaseType> }

constructor TFactory<TKey, TBaseType>.Create;
begin
  inherited Create();
end;

destructor TFactory<TKey, TBaseType>.Destroy;
begin
  inherited Destroy;
end;

function TFactory<TKey, TBaseType>.GetInstance(const AKey: TKey): TBaseType;
var
  factoryMethod: TFactoryMethod<TBaseType>;
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');
  factoryMethod := fFactoryMethods.Items[AKey];
  if Assigned(factoryMethod) then
    Result := factoryMethod
  else
    Result := CreateElement;
end;

{ TMultiton<TKey, TBaseType> }

procedure TMultiton<TKey, TBaseType>.ClearObject(const AValue: TValue);
var
  obj: TObject;
begin
  if not (AValue.IsEmpty) and (AValue.IsObject) then
  begin
    obj := AValue.AsObject;
    if Assigned(obj) then
      obj.Free;
  end;
end;

procedure TMultiton<TKey, TBaseType>.ClearObjects;
var
  pair: TPair<TKey,TValue>;
begin
  if FOwnsObjects then
  begin
    for pair in FLifetimeWatcher do
    begin
      ClearObject(pair.Value);
    end;
  end;
end;

constructor TMultiton<TKey, TBaseType>.Create(AOwnsObjects: Boolean);
begin
  inherited Create();
  FLifetimeWatcher := TDictionary<TKey,Rtti.TValue>.Create; 
  FOwnsObjects := AOwnsObjects;
end;

destructor TMultiton<TKey, TBaseType>.Destroy;
begin
  ClearObjects;
  FLifetimeWatcher.Free;
  inherited;
end;

function TMultiton<TKey, TBaseType>.GetInstance(const AKey: TKey): TBaseType;
var
  factoryMethod: TFactoryMethod<TBaseType>;
  AValue: TValue;
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  AValue := FLifetimeWatcher.Items[AKey];
  if AValue.IsEmpty then
  begin
    factoryMethod := fFactoryMethods.Items[AKey];
    if Assigned(factoryMethod) then
      Result := factoryMethod     
    else
      Result := CreateElement;
    AValue := TValue.From<TBaseType>(Result);
    FLifetimeWatcher.AddOrSetValue(AKey, AValue);
  end
  else
  begin
    Result := AValue.AsType<TBaseType>;
  end;
end;

procedure TMultiton<TKey, TBaseType>.RegisterFactoryMethod(const AKey: TKey;
  AFactoryMethod: TFactoryMethod<TBaseType>);
begin
  inherited;
  FLifetimeWatcher.Add(AKey, TValue.Empty);
end;

procedure TMultiton<TKey, TBaseType>.UnregisterAll;
begin
  inherited;
  ClearObjects;
  FLifetimeWatcher.Clear;
end;

procedure TMultiton<TKey, TBaseType>.UnregisterFactoryMethod(const AKey: TKey);
var
  AValue: TValue;
begin
  inherited;
  if FOwnsObjects then
  begin
    AValue := FLifetimeWatcher.Items[AKey];
    ClearObject(AValue);                    
  end;
  FLifetimeWatcher.Remove(AKey);
end;

end.
