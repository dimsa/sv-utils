unit uSvTypes;

interface

uses
  SysUtils;

type

{$REGION 'Doc'}
  /// <summary>
  /// record for building file paths
  /// </summary>
  /// <example>
  /// <code>
  ///  function GetPath: string;
  ///  var
  ///  pBuilder: TPathBuilder;
  ///  begin
  ///    pBuilder := TPathBuilder.InitCurrentDir;
  ///    Result := pBuilder.Add('bin').Add('etc').Add('test').ToString;
  ///    //result is appdir\bin\etc\test\ in Windows
  ///    //result is appdir/bin/etc/test/ in Mac OSX
  ///  end;
  /// </code>
  /// </example>
{$ENDREGION}
  TPathBuilder = record
  private
    sPath: string;
  public
    function Add(const APath: string; IncludeTrailingDelimiter: Boolean = True): TPathBuilder;
    function AddFile(const AFilename: string): TPathBuilder;
    function ToString(): string;
    class function Init(): TPathBuilder; static;
    class function InitCurrentDir(): TPathBuilder; static;
    class function InitHomePath(): TPathBuilder; static;
    class function InitCustomPath(const APath: string): TPathBuilder; static;
  end;

  /// <summary>
  /// Generic enumeration type
  /// <remarks>
  /// Should be used only for enumerations!
  /// </remarks>
  /// </summary>
  TSvEnum<T> = record
  private
    FValue: NativeInt;
  private
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    class operator Implicit(const AEnum: TSvEnum<T>): T; inline;
    class operator Implicit(const AEnum: T): TSvEnum<T>; inline;
    class operator Implicit(const AEnum: TSvEnum<T>): Integer; inline;
    class operator Implicit(const AEnum: Integer): TSvEnum<T>; inline;

    function ToString(): string;

    property Value: T read GetValue write SetValue;
  end;

implementation

uses
  Rtti,
  TypInfo;

{ TPathBuilder }
function TPathBuilder.Add(const APath: string;
  IncludeTrailingDelimiter: Boolean): TPathBuilder;
begin
  if IncludeTrailingDelimiter then
  begin
    sPath := sPath + IncludeTrailingPathDelimiter(APath);
  end
  else
  begin
    sPath := sPath + APath;
  end;

  Result := Self;
end;

function TPathBuilder.AddFile(const AFilename: string): TPathBuilder;
begin
  sPath := sPath + AFilename;
  Result := Self;
end;

class function TPathBuilder.Init: TPathBuilder;
begin
  Result.sPath := '';
end;

class function TPathBuilder.InitCurrentDir: TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
end;

class function TPathBuilder.InitCustomPath(const APath: string): TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(APath);
end;

class function TPathBuilder.InitHomePath: TPathBuilder;
begin
  Result.sPath := IncludeTrailingPathDelimiter(GetHomePath);
end;

function TPathBuilder.ToString: string;
begin
  Result := sPath;
end;

{ TSvEnum<T> }

function TSvEnum<T>.GetValue: T;
begin
  Result := TValue.FromOrdinal(TypeInfo(T), FValue).AsType<T>;
end;

class operator TSvEnum<T>.Implicit(const AEnum: TSvEnum<T>): T;
begin
  Result := AEnum.Value;
end;

class operator TSvEnum<T>.Implicit(const AEnum: T): TSvEnum<T>;
begin
  Result.Value := AEnum;
end;

class operator TSvEnum<T>.Implicit(const AEnum: TSvEnum<T>): Integer;
begin
  Result := AEnum.FValue;
end;

class operator TSvEnum<T>.Implicit(const AEnum: Integer): TSvEnum<T>;
begin
  Result.FValue := AEnum;
end;

procedure TSvEnum<T>.SetValue(const Value: T);
var
  AValue: TValue;
begin
  AValue := TValue.From<T>(Value);
  if AValue.Kind = tkEnumeration then
    FValue := AValue.AsOrdinal;
end;

function TSvEnum<T>.ToString: string;
begin
  Result := GetEnumName(TypeInfo(T), FValue);
end;



end.
