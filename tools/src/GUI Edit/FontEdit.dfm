object FontEditDlg: TFontEditDlg
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Font editor'
  ClientHeight = 293
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 369
    Height = 41
    Caption = 'Name:'
    TabOrder = 0
    object Edit1: TEdit
      Left = 3
      Top = 17
      Width = 363
      Height = 21
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 55
    Width = 369
    Height = 202
    Caption = 'Properties:'
    TabOrder = 1
    object Label1: TLabel
      Left = 11
      Top = 21
      Width = 60
      Height = 13
      Caption = 'Fontname:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 11
      Top = 67
      Width = 26
      Height = 13
      Caption = 'Size:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 75
      Top = 67
      Width = 32
      Height = 13
      Caption = 'Style:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 226
      Top = 21
      Width = 47
      Height = 13
      Caption = 'Shadow:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 226
      Top = 48
      Width = 52
      Height = 13
      Caption = 'Shadow-X:'
    end
    object Label6: TLabel
      Left = 226
      Top = 80
      Width = 52
      Height = 13
      Caption = 'Shadow-Y:'
    end
    object Label7: TLabel
      Left = 226
      Top = 107
      Width = 22
      Height = 13
      Caption = 'Blur:'
    end
    object Label8: TLabel
      Left = 226
      Top = 139
      Width = 29
      Height = 13
      Caption = 'Color:'
    end
    object Label9: TLabel
      Left = 226
      Top = 170
      Width = 31
      Height = 13
      Caption = 'Alpha:'
    end
    object ComboBox1: TComboBox
      Left = 11
      Top = 40
      Width = 198
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 0
      OnDrawItem = ComboBox1DrawItem
    end
    object Edit2: TEdit
      Left = 11
      Top = 86
      Width = 52
      Height = 21
      MaxLength = 3
      TabOrder = 1
      Text = '12'
    end
    object CheckBox1: TCheckBox
      Left = 75
      Top = 86
      Width = 46
      Height = 17
      Caption = 'Italic'
      TabOrder = 2
    end
    object CheckBox2: TCheckBox
      Left = 75
      Top = 109
      Width = 46
      Height = 17
      Caption = 'Bold'
      TabOrder = 3
    end
    object CheckBox3: TCheckBox
      Left = 75
      Top = 132
      Width = 70
      Height = 17
      Caption = 'Underline'
      TabOrder = 4
    end
    object Edit3: TEdit
      Left = 284
      Top = 40
      Width = 45
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object Edit4: TEdit
      Left = 284
      Top = 72
      Width = 45
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object ComboBox2: TComboBox
      Left = 284
      Top = 99
      Width = 45
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 7
      Text = '0'
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4'
        '5')
    end
    object Panel1: TPanel
      Left = 284
      Top = 126
      Width = 45
      Height = 25
      BevelKind = bkSoft
      BevelOuter = bvLowered
      Color = clBlack
      ParentBackground = False
      TabOrder = 8
      OnDblClick = Panel1DblClick
    end
    object Edit5: TEdit
      Left = 284
      Top = 162
      Width = 45
      Height = 21
      MaxLength = 3
      TabOrder = 9
      Text = '0'
    end
  end
  object Button1: TButton
    Left = 221
    Top = 263
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 302
    Top = 263
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ColorDialog1: TColorDialog
    Left = 8
    Top = 264
  end
end
