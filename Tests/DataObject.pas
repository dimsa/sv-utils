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
unit DataObject;

interface

uses
  Classes, Graphics;

const
  CNAME = 'Foobar';
  CID = 58;
  CPOINTS = 5;

type
  TData = class
  private
    FName: string;
    FID: Integer;
    FDate: TDateTime;
    FPoints: Integer;
    FColor: TColor;
    FIsChecked: Boolean;
    FItems: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetDefaults();
    procedure Clear();

    property Name: string read FName write FName;
    property ID: Integer read FID write FID;
    property Date: TDateTime read FDate write FDate;
    property Points: Integer read FPoints write FPoints;
    property Color: TColor read FColor write FColor;
    property IsChecked: Boolean read FIsChecked write FIsChecked;
    property Items: TStrings read FItems write FItems;
  end;

implementation

uses
  DateUtils;

{ TData }

procedure TData.Clear;
begin
  SetDefaults;
  FItems.Clear;
end;

constructor TData.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TData.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TData.SetDefaults;
begin
  FName := CNAME;
  FID := CID;
  FDate := Today;
  FPoints := CPOINTS;
  FColor := clBlack;
  FIsChecked := True;
  FItems.AddStrings(TArray<string>.Create(CNAME, '2', '3'));
end;

end.
