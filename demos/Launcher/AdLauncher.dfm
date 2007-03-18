object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Launcher'
  ClientHeight = 366
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 198
    Top = 333
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 333
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 272
    Width = 233
    Height = 49
    Caption = 'Rendering Plugin:'
    TabOrder = 2
    object ComboBox1: TComboBox
      Left = 5
      Top = 16
      Width = 223
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 24
    Top = 192
    Width = 233
    Height = 74
    Caption = 'Anti-Alias:'
    TabOrder = 3
    object CheckBox1: TCheckBox
      Left = 5
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Enable Antialias'
      TabOrder = 0
    end
    object TrackBar1: TTrackBar
      Left = 3
      Top = 35
      Width = 227
      Height = 36
      Enabled = False
      Max = 2
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 24
    Top = 81
    Width = 233
    Height = 105
    Caption = 'Resolution:'
    TabOrder = 4
    object CheckBox2: TCheckBox
      Left = 5
      Top = 79
      Width = 97
      Height = 17
      Caption = 'Fullscreen'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 5
      Top = 56
      Width = 196
      Height = 17
      Caption = 'Use current Desktop Resolution'
      TabOrder = 1
    end
    object ComboBox2: TComboBox
      Left = 5
      Top = 24
      Width = 220
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 265
    Height = 67
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 5
    object Image1: TImage
      Left = 2
      Top = 2
      Width = 261
      Height = 63
      Align = alClient
      ExplicitLeft = 158
      ExplicitWidth = 63
      ExplicitHeight = 105
    end
  end
end
