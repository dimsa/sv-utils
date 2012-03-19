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
unit SvRttiUtils;

interface

uses
  Rtti, SysUtils;

type
  ESvRttiException = class(Exception);

  TSvRtti = class abstract
  public
    class function CreateNewClass<T>: T;
    class procedure DestroyClass<T>(var AObject: T);

    class procedure SetValue<T>(const APropertyName: string; const AObject: T; const AValue: TValue);
  end;

implementation

{ TSvRtti }

class function TSvRtti.CreateNewClass<T>: T;
var
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  rType := TRttiContext.Create.GetType(TypeInfo(T));
  if rType.IsInstance then
  begin
    for AMethCreate in rType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 0) then
      begin
        instanceType := rType.AsInstance;

        Result := AMethCreate.Invoke(instanceType.MetaclassType, []).AsType<T>;

        Break;
      end;
    end;
  end;
end;

class procedure TSvRtti.DestroyClass<T>(var AObject: T);
var
  rType: TRttiType;
  AMethDestroy: TRttiMethod;
begin
  rType := TRttiContext.Create.GetType(TypeInfo(T));
  if rType.IsInstance then
  begin
    for AMethDestroy in rType.GetMethods do
    begin
      if (AMethDestroy.IsDestructor) and (Length(AMethDestroy.GetParameters) = 0) then
      begin
        AMethDestroy.Invoke(TValue.From<T>(AObject), []);
        Break;
      end;
    end;
  end;
end;

class procedure TSvRtti.SetValue<T>(const APropertyName: string; const AObject: T; const AValue: TValue);
var
  rType: TRttiType;
  rField: TRttiField;
  rProp: TRttiProperty;
  obj: TValue;
begin
  rType := TRttiContext.Create.GetType(TypeInfo(T));
  rField := rType.GetField(APropertyName);
  obj := TValue.From<T>(AObject);
  if Assigned(rField) then
  begin
    if obj.IsObject then
      rField.SetValue(obj.AsObject, AValue)
    else
      rField.SetValue(obj.GetReferenceToRawData, AValue);
  end
  else
  begin
    rProp := rType.GetProperty(APropertyName);
    if Assigned(rProp) then
    begin
      if obj.IsObject then
        rProp.SetValue(obj.AsObject, AValue)
      else
        rProp.SetValue(obj.GetReferenceToRawData, AValue);
    end
    else
    begin
      raise ESvRttiException.Create(Format('Property or field %S does not exist for a value %S',
        [APropertyName, obj.ToString]));
    end;
  end;
end;

end.
