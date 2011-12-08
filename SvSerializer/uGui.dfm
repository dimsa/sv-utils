object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 348
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Serialize'
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'DeSerialize'
    TabOrder = 1
    OnClick = btn2Click
  end
  object mmo1: TMemo
    Left = 176
    Top = 8
    Width = 299
    Height = 225
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btn3: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Marshall'
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 8
    Top = 111
    Width = 75
    Height = 25
    Caption = 'unMarshall'
    TabOrder = 4
    OnClick = btn4Click
  end
  object btn5: TButton
    Left = 8
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 5
    OnClick = btn5Click
  end
end
