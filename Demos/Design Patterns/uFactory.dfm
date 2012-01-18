object frmUI: TfrmUI
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Factory'
  ClientHeight = 167
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    502
    167)
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 16
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Create TDemo'
    TabOrder = 0
    OnClick = DoCreateObjects
  end
  object rgTypes: TRadioGroup
    Left = 16
    Top = 39
    Width = 145
    Height = 89
    Caption = 'TDemo types:'
    TabOrder = 1
  end
  object mmoLog: TMemo
    Left = 176
    Top = 8
    Width = 318
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btn2: TButton
    Left = 16
    Top = 134
    Width = 145
    Height = 25
    Caption = 'Get Button'
    TabOrder = 3
    OnClick = btn2Click
  end
end
