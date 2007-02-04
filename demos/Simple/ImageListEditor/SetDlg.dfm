object Settings: TSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Image Settings'
  ClientHeight = 373
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 352
    Width = 409
    Height = 13
    Caption = 
      'TIP: Press CTRL to add this colour to the alpha channel. Otherwi' +
      'se it will be replaced.'
    Visible = False
  end
  object Button1: TButton
    Left = 537
    Top = 345
    Width = 75
    Height = 25
    Hint = 'Save settings and close this dialog'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object Button2: TButton
    Left = 456
    Top = 344
    Width = 75
    Height = 25
    Hint = 'Close this dialog'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 7
    Width = 604
    Height = 331
    Caption = 'Settings'
    TabOrder = 2
    object GroupBox2: TGroupBox
      Left = 3
      Top = 16
      Width = 185
      Height = 41
      Caption = 'Name:'
      TabOrder = 0
      object Edit1: TEdit
        Left = 3
        Top = 16
        Width = 179
        Height = 21
        TabOrder = 0
        Text = 'unnamed'
      end
    end
    object RadioGroup1: TRadioGroup
      Left = 3
      Top = 63
      Width = 185
      Height = 54
      Caption = 'Bit-Depth:'
      ItemIndex = 1
      Items.Strings = (
        '16-Bit'
        '32-Bit')
      TabOrder = 1
      OnClick = RadioGroup1Click
    end
    object GroupBox3: TGroupBox
      Left = 3
      Top = 123
      Width = 185
      Height = 118
      Caption = 'Compressor:'
      TabOrder = 2
      object ListBox1: TListBox
        Left = 3
        Top = 16
        Width = 179
        Height = 97
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object GroupBox4: TGroupBox
      Left = 399
      Top = 12
      Width = 202
      Height = 229
      Caption = 'Main layer:'
      TabOrder = 3
      object Image1: TImage
        Left = 12
        Top = 20
        Width = 182
        Height = 134
        Center = True
        Proportional = True
        Stretch = True
        OnMouseDown = Image1MouseDown
      end
    end
    object GroupBox5: TGroupBox
      Left = 194
      Top = 12
      Width = 199
      Height = 229
      Caption = 'Transparency layer:'
      TabOrder = 4
      object Image2: TImage
        Left = 8
        Top = 20
        Width = 182
        Height = 137
        Center = True
        Proportional = True
        Stretch = True
      end
      object SpeedButton1: TSpeedButton
        Left = 8
        Top = 163
        Width = 25
        Height = 25
        Hint = 'Add a color from the color dialog to the transparency layer'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA05027954B25
          944A24944A24944A24944A24944A24944A24944A24944A24944A24944A24944A
          24964C25A15127FF00FFAB4E21FEF4E9FEF0E0FEECD7FEE8CFFEE4C7FEE1C1FE
          DEBBFEDDB8FEDDB8FEDDB8FEDDB8FEDDB8FEDDB8914923FF00FFAB4E21FEF8F2
          4571FA4571FA4571FAFEE9D2A23F08A23F08A23F08FEDDB8059ACD059ACD059A
          CDFEDDB88F4823FF00FFAB4E21FEFCF94571FA4571FA4571FAFEEEDCA23F08A2
          3F08A23F08FEE0BE059ACD059ACD059ACDFEDDB88F4823FF00FFAB4E21FEFEFE
          4571FA4571FA4571FAFEF3E7A23F08A23F08A23F08FEE3C6059ACD059ACD059A
          CDFEDDB88F4823FF00FFAB4E21FEFEFEFEFEFEFEFDFCFEFBF7FEF7F0FEF4E8FE
          F0E1FEECD7FEE8D0FEE4C8FEE1C0FEDEBBFEDDB88F4823FF00FFAB4E21FEFEFE
          CC9A99CC9A99CC9A99FEFCF9E27E03E27E03E27E03FEEDDA029A03029A03029A
          03FEDFBD8F4823FF00FFAB4E21FEFEFECC9A99CC9A99CC9A99FEFEFEE27E03E2
          7E03E27E03FEF2E5029A03029A03029A03FEE2C48F4823FF00FFAB4E21FEFEFE
          CC9A99CC9A99CC9A99FEFEFEE27E03E27E03E27E03FEF7EE029A03029A03029A
          03FEE7CD8E4722FF00FFAB4E21E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4
          E4E4E4E4E4E4E4E4E4E2E1E4E0DCE4DED6E4DACF944D29FF00FFAE5C27AE6122
          AD5F20AD5F20AD5F20AD5F20AD5F20AD5F20AD5F21AF6225AE6122AF6225AC60
          24AA61288F4823FF00FFAE5C27EE9733EE9733EE9733EE9733EE9733EE9733EE
          9733EE9733EE9733EE9733EE9733EE9733EE9733B95D19FF00FFFF00FFCB731A
          CC731ACC731ACC731ACC731ACC731ACC731ACC741ACD751BCC7318CD751BCA72
          1AC8721EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton1Click
      end
      object SpeedButton2: TSpeedButton
        Left = 33
        Top = 163
        Width = 25
        Height = 25
        HelpType = htKeyword
        HelpKeyword = 'Choose the color from the main layer'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000880B0000880B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000
          0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FF000000FFFFFF000000FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FF000000FFFFFFFF
          FFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          000000FF00FFFF00FF000000FFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FFFFFF000000000000FFFFFFFFFFFF00
          0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          FFFFFFFFFFFF000000FFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFFFFFFFFFFFFFF00000000
          0000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          FFFFFFFFFFFFFFFFFFFFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFFFFFFFF000000FF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          FFFFFFFFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FFFFFF000000FF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
          000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        OnClick = Button4Click
      end
      object Button3: TButton
        Left = 115
        Top = 163
        Width = 75
        Height = 25
        Hint = 'Load the alphachannel from a bitmap'
        Caption = 'Load...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button5: TButton
        Left = 8
        Top = 194
        Width = 182
        Height = 25
        Hint = 'Clear the alphachannel and make the picture completly opac.'
        Caption = 'Clear'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 58
        Top = 163
        Width = 21
        Height = 25
        Hint = 'Setup the sensitivity of the "Use color" tool'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = Button6Click
      end
    end
    object GroupBox6: TGroupBox
      Left = 3
      Top = 247
      Width = 185
      Height = 72
      Caption = 'Pattern:'
      TabOrder = 5
      object Label1: TLabel
        Left = 3
        Top = 16
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label2: TLabel
        Left = 3
        Top = 43
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object Edit2: TEdit
        Left = 61
        Top = 13
        Width = 36
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object Edit3: TEdit
        Left = 61
        Top = 40
        Width = 36
        Height = 21
        TabOrder = 1
        Text = '0'
      end
    end
    object GroupBox7: TGroupBox
      Left = 194
      Top = 247
      Width = 185
      Height = 72
      Caption = 'Skip:'
      TabOrder = 6
      object Label3: TLabel
        Left = 3
        Top = 16
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label4: TLabel
        Left = 3
        Top = 43
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object Edit4: TEdit
        Left = 61
        Top = 13
        Width = 36
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object Edit5: TEdit
        Left = 61
        Top = 40
        Width = 36
        Height = 21
        TabOrder = 1
        Text = '0'
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp|All Files (*.*)|*.*'
    Left = 80
    Top = 88
  end
  object ColorDialog1: TColorDialog
    Left = 112
    Top = 88
  end
end
