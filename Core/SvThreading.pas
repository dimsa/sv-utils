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
//inspired from open source DSharp library
unit SvThreading;

interface

uses
  SysUtils, Classes;

type
  ISvFuture = interface
    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure WaitFor;
  end;

  ISvFuture<T> = interface(ISvFuture)
    function Value: T;
  end;


  TSvAbstractFutureThread = class(TThread)
  protected
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSvAbstractFuture = class(TInterfacedObject, ISvFuture)
  strict protected
    FCanceled: Boolean;
    FWorker: TSvAbstractFutureThread;
  public
    constructor Create;
    destructor Destroy; override;

    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure WaitFor;
  end;

  TSvFutureThread<T> = class(TSvAbstractFutureThread)
  strict private
    FAction: TFunc<T>;
    FResult: T;
  public
    constructor Create(const AAction: TFunc<T>);
    procedure Execute; override;
    property Result: T read FResult;
  end;

  TFutureType<T> = class(TSvAbstractFuture, ISvFuture<T>)
  public
    constructor Create(const AAction: TFunc<T>);

    function Value: T;
  end;

  TSvFuture<T> = record
  private
    FFuture: ISvFuture<T>;
  private
    function GetValue: T;
    function GetCanceled: Boolean;
    function GetFinished: Boolean;
  public
    constructor Create(const AFunc: TFunc<T>);

    procedure Cancel;
    procedure WaitFor;

    class operator Implicit(const AFunc: TFunc<T>): TSvFuture<T>; inline;
    class operator Implicit(const AFuture: TSvFuture<T>): T; inline;

    property Canceled: Boolean read GetCanceled;
    property Finished: Boolean read GetFinished;
    property Value: T read GetValue;
  end;

implementation

{ TSvFuture<T> }

procedure TSvFuture<T>.Cancel;
begin
  FFuture.Cancel;
end;

constructor TSvFuture<T>.Create(const AFunc: TFunc<T>);
begin
  FFuture := TFutureType<T>.Create(AFunc);
end;

function TSvFuture<T>.GetCanceled: Boolean;
begin
  Result := FFuture.Canceled;
end;

function TSvFuture<T>.GetFinished: Boolean;
begin
  Result := FFuture.Finished;
end;

function TSvFuture<T>.GetValue: T;
begin
  Result := FFuture.Value;
end;

class operator TSvFuture<T>.Implicit(const AFuture: TSvFuture<T>): T;
begin
  Result := AFuture.Value;
end;

class operator TSvFuture<T>.Implicit(const AFunc: TFunc<T>): TSvFuture<T>;
begin
  Result := TSvFuture<T>.Create(AFunc);
end;

procedure TSvFuture<T>.WaitFor;
begin
  FFuture.WaitFor;
end;

{ TSvAbstractFuture }

constructor TSvAbstractFuture.Create;
begin
  inherited Create();
end;

destructor TSvAbstractFuture.Destroy;
begin
  FreeAndNil(FWorker);
  inherited;
end;

procedure TSvAbstractFuture.Cancel;
begin
  if FCanceled then
    raise Exception.Create('Action already canceled');

  if not FWorker.Finished then
  begin
    FWorker.Terminate();
    FCanceled := True;
  end;
end;

function TSvAbstractFuture.Canceled: Boolean;
begin
  Result := FCanceled;
end;

function TSvAbstractFuture.Finished: Boolean;
begin
  Result := FWorker.Finished;
end;

procedure TSvAbstractFuture.WaitFor;
begin
  FWorker.WaitFor();
end;

{ TSvAbstractFutureThread }

constructor TSvAbstractFutureThread.Create;
begin
  inherited Create(True);
end;

destructor TSvAbstractFutureThread.Destroy;
begin
  if not Finished and not Terminated then
  begin
    Terminate;
    WaitFor;
  end;
  inherited;
end;

procedure TSvAbstractFutureThread.DoTerminate;
begin
  inherited;
end;

{ TSvFutureThread<T> }

constructor TSvFutureThread<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TSvFutureThread<T>.Execute;
begin
  inherited;
  FResult := FAction();
end;

{ TFutureType<T> }

constructor TFutureType<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create;
  FWorker := TSvFutureThread<T>.Create(AAction);
  FWorker.Start();
end;

function TFutureType<T>.Value: T;
begin
  if FCanceled then
    raise Exception.Create('Action was canceled');

  if not FWorker.Finished then
  begin
    FWorker.WaitFor();
  end;
  Result := TSvFutureThread<T>(FWorker).Result;
end;



end.
