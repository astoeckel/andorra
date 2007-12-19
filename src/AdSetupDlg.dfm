object AdSetupFrm: TAdSetupFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Andorra 2D Setup'
  ClientHeight = 459
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 418
    Width = 321
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 87
      Top = 18
      Width = 144
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = '<copyright>'
    end
    object Button1: TButton
      Left = 237
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button2: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object imgContainer: TPanel
      Left = 8
      Top = 8
      Width = 304
      Height = 129
      BevelOuter = bvLowered
      BevelWidth = 2
      TabOrder = 0
      object Image1: TImage
        Left = 2
        Top = 2
        Width = 300
        Height = 125
        Align = alClient
        Proportional = True
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 145
    Width = 321
    Height = 273
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object GroupBox1: TGroupBox
      Left = 24
      Top = 0
      Width = 273
      Height = 106
      Caption = 'Resolution'
      TabOrder = 0
      object ComboBox2: TComboBox
        Left = 16
        Top = 24
        Width = 241
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 51
        Width = 177
        Height = 17
        Caption = 'Use current screen resolution'
        TabOrder = 1
        OnClick = CheckBox3Click
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 74
        Width = 177
        Height = 17
        Caption = 'Fullscreen'
        TabOrder = 2
      end
    end
    object GroupBox2: TGroupBox
      Left = 24
      Top = 110
      Width = 273
      Height = 50
      Caption = 'Advanced Options'
      TabOrder = 1
      object CheckBox1: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Antialias'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 96
        Top = 24
        Width = 97
        Height = 17
        Caption = 'V-Sync'
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 24
      Top = 168
      Width = 273
      Height = 99
      Caption = 'Plugin'
      TabOrder = 2
      object ComboBox1: TComboBox
        Left = 16
        Top = 24
        Width = 241
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = ComboBox1Change
      end
      object RadioButton1: TRadioButton
        Left = 95
        Top = 64
        Width = 74
        Height = 17
        Caption = 'Software'
        TabOrder = 2
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 64
        Width = 73
        Height = 17
        Caption = 'Hardware'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
  end
  object XPManifest1: TXPManifest
    Left = 16
    Top = 16
  end
end
