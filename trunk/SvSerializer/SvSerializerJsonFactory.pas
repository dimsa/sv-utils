{*******************************************************}
{                                                       }
{       SvSerializerJsonFactory                         }
{                                                       }
{       Copyright (C) 2011 "Linas Naginionis"           }
{                                                       }
{*******************************************************}

unit SvSerializerJsonFactory;

interface

uses
  Classes, SvSerializer, SysUtils, DBXJSON, Rtti, Types;

type
  TSvJsonString = class(TJSONString)
  private
    function EscapeValue(const AValue: string): string;
  public
    constructor Create(const AValue: string); overload;
  end;

  TSvJsonSerializerFactory = class(TSvSerializerFactory)
  private
    FMainObj: TJSONObject;
    ss: TStringStream;
    FStream: TStream;
    FFormatSettings: TFormatSettings;
    FOldNullStrConvert: Boolean;
  protected
    procedure BeginSerialization(); override;
    procedure EndSerialization(); override;
    procedure BeginDeSerialization(AStream: TStream); override;
    procedure EndDeSerialization(AStream: TStream); override;

    function FindRecordFieldName(const AFieldName: string; ARecord: TRttiRecordType): TRttiField;
  
    procedure SerializeObject(const AKey: string; const obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); override;
    procedure DeSerializeObject(const AKey: string; obj: TValue; AStream: TStream;
      ACustomProps: TStringDynArray); override;

    function GetValue(const AFrom: TValue; AProp: TRttiProperty; const ADef: string = ''): TJSONValue; virtual;
    function SetValue(const AFrom: TJSONValue; const AObj: TValue; AProp: TRttiProperty; AType: TRttiType; var Skip: Boolean): TValue; virtual;
  public
    constructor Create(AOwner: TSvSerializer); override;
    destructor Destroy; override;
    
  end;

implementation

uses
  TypInfo,
  Variants;

{ TSvJsonSerializerFactory }

procedure TSvJsonSerializerFactory.BeginDeSerialization(AStream: TStream);
var
  sBytes: TBytesStream;
  AJsonVal: TJSONValue;
begin
  inherited;
  FMainObj := nil;
  FOldNullStrConvert := NullStrictConvert;
  NullStrictConvert := False;
  if Assigned(AStream) then
  begin
    //parse json stream
    sBytes := TBytesStream.Create();
    try
      sBytes.CopyFrom(AStream, AStream.Size);
      sBytes.Position := 0;

      if sBytes.Size > 0 then
      begin
        
        AJsonVal := TJSONObject.ParseJSONValue(sBytes.Bytes, 0, sBytes.Size, True);

        if Assigned(AJsonVal) and (AJsonVal is TJSONObject) then
        begin
          FMainObj := TJSONObject(AJsonVal);          
        end;
      end;
      
    finally
      sBytes.Free;
    end;
    
  end
  else
  begin
    PostError('Cannot deserialize from nil stream');
    raise ESvSerializeException.Create('Cannot deserialize from nil stream');
  end;
end;

procedure TSvJsonSerializerFactory.EndDeSerialization(AStream: TStream);
begin
  inherited;
  NullStrictConvert := FOldNullStrConvert;
  if Assigned(FMainObj) then
    FMainObj.Free;
    
end;

procedure TSvJsonSerializerFactory.BeginSerialization;
begin
  inherited;
  FMainObj := TJSONObject.Create;
  ss := TStringStream.Create('', TEncoding.UTF8);
  FOldNullStrConvert := NullStrictConvert;
  NullStrictConvert := False;
end;

constructor TSvJsonSerializerFactory.Create(AOwner: TSvSerializer);
begin
  inherited Create(AOwner);
  FMainObj := nil;
  ss := nil;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := '.';
end;

procedure TSvJsonSerializerFactory.DeSerializeObject(const AKey: string; obj: TValue;
  AStream: TStream; ACustomProps: TStringDynArray);
var
  rtype: TRttiType;
  rprop: TRttiProperty;
  svAttr: SvSerialize;
  AValue: TValue;
  FObj: TJSONObject;
  FPair: TJSONPair;
  APropName: string;
  I: Integer;
  ASkip: Boolean;
  AField: TRttiField;
begin
  inherited;

  if not obj.IsEmpty and Assigned(FMainObj) then
  begin
    FPair := FMainObj.Get(GetObjectUniqueName(AKey, obj));
    if Assigned(FPair) then
    begin
      //enumerate properties and set values to object
      if FPair.JsonValue is TJSONObject then
      begin
        FObj := TJSONObject(FPair.JsonValue);

        rtype := TSvRttiInfo.GetType(obj);

        if Length(ACustomProps) > 0 then
        begin
          for I := Low(ACustomProps) to High(ACustomProps) do
          begin
            rprop := rtype.GetProperty(ACustomProps[I]);
            if Assigned(rprop) and (rprop.IsWritable) then
            begin
              APropName := rprop.Name;
              FPair := FObj.Get(APropName);
              if Assigned(FPair) then
              begin
                AValue := SetValue(FPair.JsonValue, obj, rprop, rprop.PropertyType, ASkip);
                if not ASkip then
                  TSvRttiInfo.SetValue(rprop, obj, AValue);
              end;
            end;

          end;
        end
        else
        begin
          if rtype.IsRecord then
          begin
            for AField in rtype.AsRecord.GetFields do
            begin
              APropName := AField.Name;
              FPair := FObj.Get(APropName);
              if Assigned(FPair) then
              begin
                AValue := SetValue(FPair.JsonValue, obj, TRttiProperty(AField), AField.FieldType, ASkip);
                if not ASkip then
                  TSvRttiInfo.SetValue(AField, obj, AValue);
              end;
            end;
          end
          else
          begin
            for rprop in rtype.GetProperties do
            begin
              if not rprop.IsWritable then
                Continue;

              svAttr := TSvSerializer.GetAttribute(rprop);
              if Assigned(svAttr) then
              begin
                if svAttr.Name = '' then
                    APropName := rprop.Name
                else
                  APropName := svAttr.Name;

                FPair := FObj.Get(APropName);
                if Assigned(FPair) then
                begin
                  AValue := SetValue(FPair.JsonValue, obj, rprop, rprop.PropertyType, ASkip);
                  if not ASkip then
                    TSvRttiInfo.SetValue(rprop, obj, AValue);
                end;
              end;
            end;
          end;
        end;
      end;
    end;    
  end;
end;

destructor TSvJsonSerializerFactory.Destroy;
begin
  inherited Destroy;
end;

procedure TSvJsonSerializerFactory.EndSerialization;
begin
  inherited;

  NullStrictConvert := FOldNullStrConvert;

  ss.WriteString(FMainObj.ToString);

  ss.Position := 0;

  FStream.CopyFrom(ss, ss.Size);

  FMainObj.Free;

  ss.Free;
end;

function TSvJsonSerializerFactory.FindRecordFieldName(const AFieldName: string; ARecord: TRttiRecordType): TRttiField;
var
  AField: TRttiField;
begin
  for AField in ARecord.GetFields do
  begin
    if SameText(AFieldName, AField.Name) then
      Exit(AField);
  end;
  Result := nil;
end;

function TSvJsonSerializerFactory.GetValue(const AFrom: TValue; AProp: TRttiProperty; const ADef: string): TJSONValue;
var
  i: Integer;
  jArr: TJSONArray;
  AVariant: Variant;
  rType, lEnumType: TRttiType;
  lEnumMethod, lMoveNextMethod: TRttiMethod;
  lEnumerator: TValue;
  lCurrentProp: TRttiProperty;
  FRecord: TRttiRecordType;
  FField: TRttiField;
  bVal: Boolean;
begin
  if AFrom.IsEmpty then
    Result := TJSONNull.Create
  else
  begin
    
    case AFrom.Kind of
      tkInteger: Result := TJSONNumber.Create(AFrom.AsInteger);
      tkInt64: Result := TJSONNumber.Create(AFrom.AsInt64);
      tkEnumeration:
      begin
        if AFrom.TryAsType<Boolean>(bVal) then
        begin
          if bVal then
            Result := TJSONTrue.Create
          else
            Result := TJSONFalse.Create;
        end
        else
        begin
          Result := TJSONString.Create(AFrom.ToString);
        end;
      end;
      tkSet:
      begin
        Result := TJSONString.Create(AFrom.ToString);
      end;
      tkFloat: Result := TJSONNumber.Create(AFrom.AsExtended);
      tkString, tkWChar, tkLString, tkWString, tkChar, tkUString:
        Result := TJSONString.Create(AFrom.AsString);
      tkArray, tkDynArray:
      begin
        Result := TJSONArray.Create();
        jArr := TJSONArray(Result);
        for i := 0 to AFrom.GetArrayLength - 1 do
        begin
          jArr.AddElement(GetValue(AFrom.GetArrayElement(i), nil));
        end;
      end;
      tkVariant:
      begin
        AVariant := AFrom.AsVariant;

        if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
          Result := TJSONNull.Create
        else
          Result := TJSONString.Create(VarToStr(AVariant));
      end;
      tkClass:
      begin
        //support enumerable types
        rType := TSvRttiInfo.GetType(AFrom.TypeInfo);
        if Assigned(rType) then
        begin
          lEnumMethod := rType.GetMethod('GetEnumerator');
          if Assigned(lEnumMethod) then
          begin
            //enumerator exists
            Result := TJSONArray.Create();
            jArr := TJSONArray(Result);
            lEnumerator := lEnumMethod.Invoke(AFrom,[]);
            lEnumType :=  TSvRttiInfo.GetType(lEnumerator.TypeInfo);
            lMoveNextMethod := lEnumType.GetMethod('MoveNext');
            lCurrentProp := lEnumType.GetProperty('Current');
            Assert(Assigned(LMoveNextMethod), 'MoveNext method not found');
            Assert(Assigned(lCurrentProp), 'Current property not found');
            while lMoveNextMethod.Invoke(lEnumerator.AsObject,[]).asBoolean do
            begin
              jArr.AddElement(GetValue(lCurrentProp.GetValue(lEnumerator.AsObject), lCurrentProp));
            end;

            if lEnumerator.IsObject then
            begin
              lEnumerator.AsObject.Free;
            end;

          end
          else
          begin
            if AFrom.IsObject then
            begin
              Result := TJSONObject.Create;
              //try to serialize
              for lCurrentProp in rType.GetProperties do
              begin
                if lCurrentProp.Visibility = mvPublished then
                begin
                  //try to serialize only published properties
                  TJSONObject(Result).AddPair(TJSONPair.Create(lCurrentProp.Name, 
                    GetValue(lCurrentProp.GetValue(AFrom.AsObject), lCurrentProp)));
                end;
              end;
            end;
          end;
        end;

      end;
      tkRecord:
      begin
        rType := TSvRttiInfo.GetType(AFrom.TypeInfo);
        FRecord := rType.AsRecord;
        Result := TJSONObject.Create();
        for FField in FRecord.GetFields do
        begin
          TJSONObject(Result).AddPair(TJSONPair.Create(FField.Name, 
            GetValue(FField.GetValue(AFrom.GetReferenceToRawData), nil)));
        end;

      end;
     { tkMethod: ;
      tkInterface: ;
      tkClassRef: ;
      tkPointer: ;
      tkProcedure: ; }
      else
      begin
        if ADef <> '' then
        begin
          Result := TJSONString.Create(ADef);
        end
        else
        begin
          PostError('Unsupported type: ' + AFrom.ToString);
          Result := TJSONString.Create('Unsupported type: ' + AFrom.ToString);
        //  raise ESvSerializeException.Create('Unsupported type: ' + AFrom.ToString);
        end;

      end;
    end;
  end;

end;

procedure TSvJsonSerializerFactory.SerializeObject(const AKey: string; const obj: TValue;
  AStream: TStream; ACustomProps: TStringDynArray);
var
  rtype: TRttiType;
  rprop: TRttiProperty;
  svAttr: SvSerialize;
  AValue: TValue;
  FObj: TJSONObject;
  FPair: TJSONPair;
  APropName: string;
  I: Integer;
  AField: TRttiField;
begin
  inherited;

  if not obj.IsEmpty and (Assigned(AStream)) then
  begin
    FStream := AStream;
    rtype := TSvRttiInfo.GetType(obj);
    //create main object
    FObj := TJSONObject.Create();
    FPair := TJSONPair.Create(GetObjectUniqueName(AKey, obj), FObj);
    FMainObj.AddPair(FPair); 
    if Length(ACustomProps) > 0 then
    begin
      for I := Low(ACustomProps) to High(ACustomProps) do
      begin
        rprop := rtype.GetProperty(ACustomProps[I]);
        if Assigned(rprop) then
        begin
          AValue := TSvRttiInfo.GetValue(rprop, obj);

          APropName := rprop.Name;
                    
          FPair := TJSONPair.Create(TJSONString.Create(APropName), GetValue(AValue, rprop));

          FObj.AddPair(FPair);
        end;        
      end;
    end
    else
    begin
      if rtype.IsRecord then
      begin
        for AField in rtype.AsRecord.GetFields do
        begin
          AValue := AField.GetValue(obj.GetReferenceToRawData);
          APropName := AField.Name;
          FPair := TJSONPair.Create(TJSONString.Create(APropName), GetValue(AValue, TRttiProperty(AField)));

          FObj.AddPair(FPair);
        end;
      end
      else
      begin
        for rprop in rtype.GetProperties do
        begin
          svAttr := TSvSerializer.GetAttribute(rprop);
          if Assigned(svAttr) then
          begin
            AValue := TSvRttiInfo.GetValue(rprop, obj);
            if Assigned(svAttr.GetData) then
            begin
              AValue := svAttr.GetData(AValue);
            end;

            if svAttr.Name = '' then
              APropName := rprop.Name
            else
              APropName := svAttr.Name;

            FPair := TJSONPair.Create(TJSONString.Create(APropName), GetValue(AValue, rprop, svAttr.DefValue));

            FObj.AddPair(FPair);
          end;
        end;
      end;


    end;

    
  end;
end;

function TSvJsonSerializerFactory.SetValue(const AFrom: TJSONValue; const AObj: TValue; AProp: TRttiProperty; AType: TRttiType; var Skip: Boolean): TValue;
var
  AInt, i: Integer;
  AInt64: Int64;
  sVal: string;
  arrVal: array of TValue;
  rType: TRttiType;
  lEnumMethod : TRttiMethod;
  lEnumerator, AValue : TValue;
  lClearMethod : TRttiMethod;
  FField: TRttiField ;
  FRecord: TRttiRecordType ;
  CurrProp: TRttiProperty;
  AParams: TArray<TRttiParameter>;
  AJsonValue: TJSONValue;
  x: Integer;
begin
  Skip := False;
  if Assigned(AFrom) then
  begin
    if AFrom is TJSONNumber then
    begin
      sVal := TJSONNumber(AFrom).ToString;

      if TryStrToInt(sVal, AInt) then
      begin
        Result := AInt;
      end
      else if TryStrToInt64(sVal, AInt64) then
      begin
        Result := AInt64;
      end
      else
      begin
        Result := TJSONNumber(AFrom).AsDouble;
      end;
    end
    else if AFrom is TJSONString then
    begin
      if Assigned(AType) then
      begin
        case AType.TypeKind of
          tkEnumeration:
          begin
            Result := TValue.FromOrdinal(AType.Handle,
              GetEnumValue(AType.Handle, TJSONString(AFrom).Value));
          end;
          tkSet:
          begin
            i := StringToSet(AType.Handle, TJSONString(AFrom).Value);
            TValue.Make(@i, AType.Handle, Result);
          end;
          tkVariant:
          begin
            Result := TValue.FromVariant(TJSONString(AFrom).Value);
          end;
          tkUString, tkWString, tkLString, tkWChar, tkChar, tkString:
          begin
            //avoid skip
            Result := TJSONString(AFrom).Value;
          end
          else
          begin
            //error msg value, skip
            PostError('Cannot set unknown type value: ' + AType.ToString);
            Skip := True;
          end;
        end;
      end
      else
      begin
        Result := TJSONString(AFrom).Value;
      end;
    end
    else if AFrom is TJSONTrue then
    begin
      Result := True;
    end
    else if AFrom is TJSONFalse then
    begin
      Result := False;
    end
    else if AFrom is TJSONNull then
    begin
      Result := TValue.Empty;
    end
    else if AFrom is TJSONArray then
    begin
      if Assigned(AType) then
      begin
        case AType.TypeKind of
          tkArray:
          begin
            SetLength(arrVal, TJSONArray(AFrom).Size);

            for i := 0 to Length(arrVal)-1 do
            begin
              arrVal[i] := SetValue(TJSONArray(AFrom).Get(i), AObj, AProp, TRttiArrayType(AType).ElementType, Skip);
            end;

            Result := TValue.FromArray(AType.Handle, arrVal);
          end;
          tkDynArray:
          begin
            SetLength(arrVal, TJSONArray(AFrom).Size);

            for i := 0 to Length(arrVal)-1 do
            begin
              arrVal[i] := SetValue(TJSONArray(AFrom).Get(i), AObj, AProp, TRttiDynamicArrayType(AType).ElementType, Skip);
            end;

            Result := TValue.FromArray(AType.Handle, arrVal);
          end;
          tkClass:
          begin
            rType := TSvRttiInfo.GetType(AType.Handle);
            if Assigned(rType) then
            begin
              lEnumMethod := TSvRttiInfo.GetBasicMethod('Add', rType);
              if Assigned(lEnumMethod) and Assigned(AProp) then
              begin
                AValue := TSvRttiInfo.GetValue(AProp, AObj);
               // AValue := AProp.GetValue(AObj);
                lClearMethod := TSvRttiInfo.GetBasicMethod('Clear', rType);
                if Assigned(lClearMethod) and (Length(lClearMethod.GetParameters) = 0) then
                begin
                  lClearMethod.Invoke(AValue, []);
                end;

                AParams := lEnumMethod.GetParameters;

                if Length(AParams) > 1 then
                begin
                  SetLength(arrVal, Length(AParams));
                  //probably we are dealing with key value pair class like TDictionary

                  for i := 0 to TJSONArray(AFrom).Size - 1 do
                  begin
                    AJsonValue := TJSONArray(AFrom).Get(i);


                    Assert(Length(AParams) = TJSONObject(AJsonValue).Size, 'Parameters count differ');
                    if AJsonValue is TJSONObject then
                    begin
                      for x := 0 to TJSONObject(AJsonValue).Size - 1 do
                      begin
                        arrVal[x] := SetValue(TJSONObject(AJsonValue).Get(x).JsonValue,
                          AObj, nil, AParams[x].ParamType, Skip);
                      end;
                    end
                    else if AJsonValue is TJSONArray then
                    begin
                      for x := 0 to TJSONArray(AJsonValue).Size - 1 do
                      begin
                        arrVal[x] :=
                          SetValue(TJSONArray(AJsonValue).Get(x), AObj, nil, AParams[x].ParamType, Skip);
                      end;
                    end
                    else
                    begin
                      // must not happen
                      //arrVal[0] := SetValue(TJSONArray(AFrom).Get(i), AObj, nil, AParams[j], Skip);
                    end;

                    lEnumerator := lEnumMethod.Invoke(AValue, arrVal);
                  end;
                end
                else
                begin
                  SetLength(arrVal, TJSONArray(AFrom).Size);

                  for i := 0 to Length(arrVal)-1 do
                  begin
                    {TODO -oLinas -cGeneral : fix arguments}
                    arrVal[i] := SetValue(TJSONArray(AFrom).Get(i), AObj, nil, nil, Skip);


                    lEnumerator := lEnumMethod.Invoke(AValue, [arrVal[i]]);
                  end;
                end;



                Skip := True;
              end;
            end;
          end
          else
          begin
            Skip := True;
            PostError('Cannot assign array data to non array type');
           // raise ESvSerializeException.Create('Cannot assign array data to non array type');
          end;
        end;
      end;
    end
    else if AFrom is TJSONObject then
    begin
      if Assigned(AType) then
      begin
        case AType.TypeKind of
          tkRecord:
          begin
            TValue.MakeWithoutCopy(nil, AType.Handle, Result);
            FRecord := TSvRttiInfo.GetType(AType.Handle).AsRecord;

            for i := 0 to TJSONObject(AFrom).Size - 1 do
            begin
              //search for property name
              FField := FindRecordFieldName(TJSONObject(AFrom).Get(i).JsonString.Value, FRecord);
              if Assigned(FField) then
              begin
                {DONE -oLinas -cGeneral : fix arguments}
                FField.SetValue(Result.GetReferenceToRawData,
                  SetValue(TJSONObject(AFrom).Get(i).JsonValue, AObj, nil, FField.FieldType, Skip));
              end;
            end;
          end;
          tkClass:
          begin
            if Assigned(AProp) then
            begin
              rType := TSvRttiInfo.GetType(AType.Handle);
              Result := TSvRttiInfo.GetValue(AProp, AObj);
            //  Result := AProp.GetValue(AObj);
              for i := 0 to TJSONObject(AFrom).Size - 1 do
              begin
                CurrProp := rType.GetProperty(TJSONObject(AFrom).Get(i).JsonString.Value);
                if Assigned(CurrProp) then
                begin
                  CurrProp.SetValue(Result.AsObject, SetValue(TJSONObject(AFrom).Get(i).JsonValue, AObj, CurrProp,
                    CurrProp.PropertyType, Skip));
                end;
              end;
            end;

          end
          else
          begin
            Skip := True;
          end;
        end;
      end;
    end
    else
    begin
      Skip := True;
      PostError('Unsupported value type: ' + AFrom.ToString);
       // raise ESvSerializeException.Create('Unsupported value type: ' + AFrom.ClassName)
    end;
  end;
end;

{ TSvJsonString }

constructor TSvJsonString.Create(const AValue: string);
begin
  inherited Create(EscapeValue(AValue));
end;

function TSvJsonString.EscapeValue(const AValue: string): string;
var
  AChar: Char;
begin
  Result := '';
  for AChar in AValue do
  begin
    if CharInSet(AChar, ['"','/','\']) then
    begin
      Result := '\' + AChar;
    end
    else
    begin
      Result := Result + AChar;
    end;
  end;
end;

end.
