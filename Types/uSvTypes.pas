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
  strict private
    sPath: string;
  public
    function Add(const APath: string; IncludeTrailingDelimiter: Boolean = True): TPathBuilder;
    function AddFile(const AFilename: string): TPathBuilder;
    function ToString(): string;
    class function Init(): TPathBuilder; static;
    class function InitCurrentDir(): TPathBuilder; static;
  end;

implementation

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

function TPathBuilder.ToString: string;
begin
  Result := sPath;
end;

end.
