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
unit TestSvContainers;

interface

uses
  TestFramework, SysUtils, Classes, SvContainers, Diagnostics;

type
  TestRec = record
    Name: string;
    ID: Integer;
  end;

  TestTSvStringTrie = class(TTestCase)
  private
    FTrie: TSvStringTrie<TestRec>;
    sw: TStopwatch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd();
    procedure TestDelete();
    procedure TestFind();
    procedure TestEnumerator();
  end;

implementation

{ TestTSvStringTrie }

procedure TestTSvStringTrie.SetUp;
begin
  inherited;
  FTrie := TSvStringTrie<TestRec>.Create;
end;

procedure TestTSvStringTrie.TearDown;
begin
  FTrie.Free;
  inherited;
end;

const
  ITER_SIZE = 100000;

procedure TestTSvStringTrie.TestAdd;
var
  rec: TestRec;
  i: Integer;
begin
  FTrie.Clear;
  sw := TStopwatch.StartNew;
  for i := 1 to ITER_SIZE do
  begin
    rec.ID := i;
    rec.Name := IntToStr(i);

    FTrie.Add(rec.Name, rec);
  end;
  sw.Stop;

  CheckEquals(ITER_SIZE, FTrie.Count);

  Status(Format('%D items added in %D ms', [FTrie.Count, sw.ElapsedMilliseconds]));
end;

procedure TestTSvStringTrie.TestDelete;
var
  i: Integer;
begin
  TestAdd;

  sw := TStopwatch.StartNew;
  for i := 1 to ITER_SIZE do
  begin
    FTrie.Delete(IntToStr(i));
  end;
  sw.Stop;

  CheckEquals(0, FTrie.Count);
  Status(Format('%D items deleted in %D ms', [FTrie.Count, sw.ElapsedMilliseconds]));
end;

procedure TestTSvStringTrie.TestEnumerator;
var
  rec: TestRec;
  ix, i: Integer;
begin
  TestAdd;
  ix := 0;
  for rec in FTrie do
  begin
   // CheckEquals(ix, rec.ID);
    i := rec.ID;
    Inc(ix);
  end;
  CheckEquals(ix, ITER_SIZE);
end;

procedure TestTSvStringTrie.TestFind;
var
  rec: TestRec;
  i: Integer;
begin
  TestAdd;
  sw := TStopwatch.StartNew;
  for i := 1 to ITER_SIZE do
  begin
    rec.ID := -1;
    rec.Name := '';
    CheckTrue( FTrie.Find(IntToStr(i), rec));
    CheckEquals(i, rec.ID);
    CheckEqualsString(IntToStr(i), rec.Name);
  end;
  sw.Stop;

  Status(Format('%D items found in %D ms', [FTrie.Count, sw.ElapsedMilliseconds]));

  CheckFalse(FTrie.Find('random valuesdsd', rec));
end;

initialization
  RegisterTest(TestTSvStringTrie.Suite);

end.
