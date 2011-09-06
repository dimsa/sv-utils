object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 220
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblPath: TLabel
    Left = 16
    Top = 56
    Width = 409
    Height = 145
    AutoSize = False
    EllipsisPosition = epEndEllipsis
    ShowAccelChar = False
    WordWrap = True
  end
  object ed1: TLabeledEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Enter folder name:'
    TabOrder = 0
  end
  object btn1: TButton
    Left = 144
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Add To Path'
    TabOrder = 1
    OnClick = btn1Click
  end
end
