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

unit ColorVisualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolsAPI, StdCtrls, ExtCtrls;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope);

  TColorViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater, IOTAThreadNotifier)
    lbColor: TLabel;
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FColor: TColor;
    sColor: string;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure DisplayColor(const Expression, TypeName, EvalResult: string);

    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
  end;

procedure Register;

implementation

uses
  DesignIntf, Actnlist, ImgList, Menus, IniFiles;

{$R *.dfm}

resourcestring
  sColorVisualizerName = 'TColor Visualizer for Delphi';
  sColorVisualizerDescription = 'Displays a color of TColor';
  sMenuText = 'Show Color';
  sFormCaption = 'TColor Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sOutOfScope = 'out of scope';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TColorVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TColorViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerColorVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

{ TDebuggerDateTimeVisualizer }

function TDebuggerColorVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerColorVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TColor';
  AllDescendants := True;
end;

function TDebuggerColorVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerColorVisualizer.GetVisualizerDescription: string;
begin
  Result := sColorVisualizerDescription;
end;

function TDebuggerColorVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerColorVisualizer.GetVisualizerName: string;
begin
  Result := sColorVisualizerName;
end;

function TDebuggerColorVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TColorViewerFrame;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TColorVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TColorViewerFrame;
  AFrame.DisplayColor(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TColorViewerFrame }

procedure TColorViewerFrame.DisplayColor(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  FExpression := Expression;

  try
    FColor := StringToColor(EvalResult);
    Self.Color := FColor;
    if not ColorToIdent(FColor, sColor) then
    begin
      sColor := EvalResult;
    end;

    lbColor.Caption := Format('%S Value: %S',[FExpression, sColor]);

  except
    on E: Exception do
    begin
      Self.Color := clBtnFace;
      lbColor.Caption :=  E.Message;
    end;
  end;

  lbColor.Invalidate;
  Self.Invalidate;
end;

procedure TColorViewerFrame.AfterSave;
begin

end;

procedure TColorViewerFrame.BeforeSave;
begin

end;

procedure TColorViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TColorViewerFrame.Destroyed;
begin

end;

function TColorViewerFrame.Evaluate(Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
          CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
        case EvalRes of
          erOK: Result := ResultStr;
          erDeferred:
            begin
              FCompleted := False;
              FDeferredResult := '';
              FDeferredError := False;
              FNotifierIndex := CurThread.AddNotifier(Self);
              while not FCompleted do
                DebugSvcs.ProcessDebugEvents;
              CurThread.RemoveNotifier(FNotifierIndex);
              FNotifierIndex := -1;
              if not FDeferredError then
              begin
                if FDeferredResult <> '' then
                  Result := FDeferredResult
                else
                  Result := ResultStr;
              end;
            end;
          erBusy:
            begin
              DebugSvcs.ProcessDebugEvents;
              Result := Evaluate(Expression);
            end;
        end;
      end;
    end;
  end;
end;

procedure TColorViewerFrame.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

procedure TColorViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;

end;

procedure TColorViewerFrame.Modified;
begin

end;

procedure TColorViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TColorViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  DisplayColor(Expression, TypeName, EvalResult);
end;

procedure TColorViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TColorViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TColorViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    sColor := '';
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TColorViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TColorVisualizerForm }

constructor TColorVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TColorVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TColorVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TColorVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TColorVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TColorViewerFrame(AFrame);
end;

function TColorVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TColorVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TColorVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TColorVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TColorVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TColorViewerFrame;
end;

function TColorVisualizerForm.GetIdentifier: string;
begin
  Result := 'ColorDebugVisualizer';
end;

function TColorVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TColorVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TColorVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TColorVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TColorVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TColorVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TColorVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TColorVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TColorViewerFrame(Frame);
end;

var
  ColorVis: IOTADebuggerVisualizer;

procedure Register;
begin
  ColorVis := TDebuggerColorVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(ColorVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(ColorVis);
    ColorVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

