unit uSvHelpers;

{*****************************************************************************
          Sound Vibe class helpers for extending class functionality
              without overriding



           Copyright 2010 Linas Naginionis
           support@soundvibe.net
******************************************************************************}

interface

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes;

type

  {** TStream Helper }
  TStreamHelper = class helper for TStream
  public
    function AsString: string;
    procedure WriteString(const Text: string); inline;
  end;


implementation


{ TStreamHelper }

function TStreamHelper.AsString: string;
var
  len: Integer;
  iString: UTF8String;  //UTF8String would be better
begin
  Result := '';
  Self.ReadBuffer(len, 4);
  if len > 0 then
  begin
    SetLength(iString, len);
  //  SetLength(iString, len div 2);  //edited to be compatible with old format
    Self.ReadBuffer(iString[1], len);
    Result := string(iString);
  end;
end;

procedure TStreamHelper.WriteString(const Text: string);
var
  len: cardinal;
  oString: UTF8String;
begin
  oString := UTF8String(Text);
  len := Length(oString);
  Self.WriteBuffer(len, 4);
  if len > 0 then
    Self.WriteBuffer(oString[1], len);
end;


end.
