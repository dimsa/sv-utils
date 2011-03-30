object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 480
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object bgrDemo: TSVButtonGroup
    Left = 0
    Top = 0
    Width = 161
    Height = 480
    Align = alLeft
    BevelInner = bvNone
    BorderStyle = bsNone
    ButtonOptions = [gboAllowReorder, gboFullSize, gboGroupStyle, gboShowCaptions]
    Items = <
      item
        Caption = 'First <b>bold</b>'
        OnClick = DoClick
      end
      item
        Caption = 'Second <i>italic</i>'
      end
      item
        Caption = 'Third <u>underline</u>'
      end
      item
        Caption = 'Fourth <s>strikeout</s>'
      end>
    ItemIndex = 0
    TabOrder = 0
    ButtonGradient = True
    ButtonGradientDirection = gdVertical
    Colors.BackColor = clWindow
    Colors.ButtonColor = clWindow
    Colors.ButtonColorFrom = clWindow
    Colors.ButtonColorTo = clWindow
    Colors.ButtonDownColor = clBtnFace
    Colors.ButtonDownColorFrom = clBtnFace
    Colors.ButtonDownColorTo = clBtnFace
    Colors.HotButtonColor = clHotLight
    Colors.HotButtonColorFrom = clHotLight
    Colors.HotButtonColorTo = clHotLight
    DrawFocusRect = False
    HTMLCaptions = True
  end
end
