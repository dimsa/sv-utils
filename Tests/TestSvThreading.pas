unit TestSvThreading;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, Classes, SysUtils, SvThreading, Diagnostics;

type
  TestTSvFuture = class(TTestCase)
  private
    FSvFuture: TSvFuture<Integer>;
    sw: TStopwatch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecute;
  end;

  TestTSvParallel = class(TTestCase)
  private
  //  sw: TStopwatch;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestForEach;
  end;

implementation

uses
  SyncObjs;

{ TestTSvFuture }

procedure TestTSvFuture.SetUp;
begin
  inherited;

end;

procedure TestTSvFuture.TearDown;
begin
  inherited;

end;

const
  INTVAL = 100;
  INTVAL2 = 111111;

procedure TestTSvFuture.TestExecute;
var
  func1, func2: TFunc<Integer>;
begin
  sw := TStopwatch.StartNew;
  func1 := function: Integer
    begin
      Sleep(1100);
      Result := INTVAL;
    end;

  func2 := function: Integer
    begin
      Sleep(600);
      Result := INTVAL2;
    end;

  FSvFuture.Assign(func1);
  Check(sw.ElapsedMilliseconds < 500, '1');
  CheckEquals(INTVAL, FSvFuture, '2');
  sw.Stop;
  Check(sw.ElapsedMilliseconds >= 1000, '3');

  sw := TStopwatch.StartNew;

  FSvFuture.Assign(func2);

  CheckEquals(INTVAL2, FSvFuture, '4');
  sw.Stop;
  Check(sw.ElapsedMilliseconds >= 500, '5');
end;

{ TestTSvParallel }

procedure TestTSvParallel.SetUp;
begin
  inherited;

end;

procedure TestTSvParallel.TearDown;
begin
  inherited;

end;

procedure TestTSvParallel.TestForEach;
var
  AFrom, ATo: NativeInt;
  iCounter: Integer;
  AFunc1: TParallelFunc1;
  AFunc2 : TParallelFunc2;
begin
  AFrom := 0;
  ATo := 1000 - 1;
  iCounter := 0;
  AFunc1 := procedure(const i: NativeInt; var Abort: Boolean)
    begin
      TInterlocked.Increment(iCounter);
      Sleep(5);
    end;
  AFunc2 := procedure(const i, AThreadIndex: NativeInt; var Abort: Boolean)
    begin
      Abort := iCounter = 500;
      if not Abort then
        TInterlocked.Increment(iCounter);

      Sleep(5);
    end;
  TSvParallel.ForEach(AFrom, ATo, AFunc1, nil);
  CheckEquals(ATo + 1, iCounter);
  iCounter := 0;
  TSvParallel.ForEach(AFrom, ATo, AFunc2, nil);
  CheckEquals(500, iCounter);
  iCounter := 0;
  TSvParallel.ForEachNonBlocking(AFrom, ATo, AFunc2,
    procedure
    begin
      CheckEquals(500, iCounter);
    end);
  Sleep(1000);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSvFuture.Suite);
  RegisterTest(TestTSvParallel.Suite);
end.
