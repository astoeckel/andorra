object ElemWzrd: TElemWzrd
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Skin element creation wizzard'
  ClientHeight = 228
  ClientWidth = 214
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 134
    Top = 199
    Width = 75
    Height = 25
    Caption = 'Generate'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 53
    Top = 199
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 114
    Caption = 'Borders:'
    TabOrder = 2
    object Label3: TLabel
      Left = 15
      Top = 16
      Width = 55
      Height = 13
      Caption = 'Border left:'
    end
    object Label4: TLabel
      Left = 15
      Top = 64
      Width = 61
      Height = 13
      Caption = 'Border right:'
    end
    object Label5: TLabel
      Left = 110
      Top = 16
      Width = 55
      Height = 13
      Caption = 'Border top:'
    end
    object Label6: TLabel
      Left = 110
      Top = 64
      Width = 73
      Height = 13
      Caption = 'Border bottom:'
    end
    object Edit1: TEdit
      Left = 15
      Top = 35
      Width = 75
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object Edit2: TEdit
      Left = 15
      Top = 83
      Width = 75
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object Edit3: TEdit
      Left = 110
      Top = 37
      Width = 75
      Height = 21
      TabOrder = 2
      Text = '0'
    end
    object Edit4: TEdit
      Left = 110
      Top = 83
      Width = 75
      Height = 21
      TabOrder = 3
      Text = '0'
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 128
    Width = 201
    Height = 65
    Caption = 'Size:'
    TabOrder = 3
    object Label1: TLabel
      Left = 15
      Top = 14
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label2: TLabel
      Left = 110
      Top = 14
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object Edit5: TEdit
      Left = 110
      Top = 33
      Width = 75
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object Edit6: TEdit
      Left = 15
      Top = 33
      Width = 75
      Height = 21
      TabOrder = 1
      Text = '0'
    end
  end
end
