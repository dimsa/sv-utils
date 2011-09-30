object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Tests'
  ClientHeight = 622
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ScreenSnap = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 522
    ResizeStyle = rsUpdate
    ExplicitLeft = 128
    ExplicitTop = 104
    ExplicitHeight = 100
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 525
    Width = 524
    Height = 94
    Align = alBottom
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 522
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pLeft'
    ShowCaption = False
    TabOrder = 0
    object CategoryButtons1: TCategoryButtons
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 179
      Height = 519
      Cursor = crHandPoint
      Margins.Bottom = 0
      Align = alClient
      BackgroundGradientColor = clGradientActiveCaption
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 21
      ButtonOptions = [boAllowReorder, boFullSize, boGradientFill, boShowCaptions, boVerticalCategoryCaptions, boBoldCaptions]
      Categories = <
        item
          Caption = 'TreeView'
          Color = 15395839
          Collapsed = False
          Items = <
            item
              Caption = 'Build Default'
              ImageIndex = 39
              OnClick = BuildDefault
            end
            item
              Caption = 'Build Virtual'
              ImageIndex = 39
              OnClick = BuildVirtual
            end
            item
              Caption = 'Iterate Tree'
              OnClick = IterateTree
            end>
        end>
      DoubleBuffered = True
      ParentDoubleBuffered = False
      RegularButtonColor = clWhite
      SelectedButtonColor = 15132390
      ShowHint = True
      TabOrder = 0
    end
    object edFilter: TButtonedEdit
      Left = 24
      Top = 88
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object ProgressBar1: TProgressBar
      Left = 24
      Top = 120
      Width = 150
      Height = 17
      TabOrder = 2
    end
  end
  object pc1: TPageControl
    Left = 188
    Top = 0
    Width = 342
    Height = 522
    ActivePage = ts2
    Align = alClient
    TabOrder = 1
    object ts1: TTabSheet
      Caption = 'Default Treeview'
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 334
        Height = 494
        Align = alClient
        BorderStyle = bsNone
        Indent = 19
        TabOrder = 0
      end
    end
    object ts2: TTabSheet
      Caption = 'VirtualStringTree'
      ImageIndex = 1
      object vt1: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 334
        Height = 392
        Align = alClient
        BorderStyle = bsNone
        Header.AutoSizeIndex = -1
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        TabOrder = 0
        TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus]
        OnGetText = vt1GetText
        OnInitChildren = vt1InitChildren
        OnInitNode = vt1InitNode
        Columns = <>
      end
      object p1: TPanel
        Left = 0
        Top = 392
        Width = 334
        Height = 102
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lb1: TLabel
          Left = 189
          Top = 76
          Width = 55
          Height = 13
          Caption = 'Root Count'
        end
        object btn2: TButton
          Left = 248
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 4
          OnClick = btn2Click
        end
        object btn3: TButton
          Left = 167
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 0
          OnClick = btn3Click
        end
        object btn4: TButton
          Left = 168
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 3
          OnClick = btn4Click
        end
        object btn5: TButton
          Left = 8
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Save'
          TabOrder = 1
          OnClick = btn5Click
        end
        object btn6: TButton
          Left = 89
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Load'
          TabOrder = 2
          OnClick = btn6Click
        end
        object seCount: TSpinEdit
          Left = 249
          Top = 73
          Width = 73
          Height = 22
          Increment = 100
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 5000
        end
        object Button1: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 6
          OnClick = Button1Click
        end
      end
    end
  end
  object VSTSearchEdit1: TVSTSearchEdit
    OnAfterSearch = VSTSearchEdit1AfterSearch
    VST = vt1
    Left = 56
    Top = 216
  end
end
