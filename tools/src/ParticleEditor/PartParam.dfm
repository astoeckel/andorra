object ParamEdt: TParamEdt
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Particle parameter editor'
  ClientHeight = 225
  ClientWidth = 211
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 178
    Caption = 'Parameters:'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 66
      Height = 13
      Caption = 'Start value:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 63
      Height = 13
      Caption = 'Stop value:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 16
      Top = 118
      Width = 90
      Height = 13
      Caption = 'Variation (in %)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edt_start: TEdit
      Left = 16
      Top = 43
      Width = 161
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object edt_stop: TEdit
      Left = 16
      Top = 91
      Width = 161
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edt_var: TEdit
      Left = 16
      Top = 137
      Width = 161
      Height = 21
      TabOrder = 2
      Text = '0'
    end
  end
  object Button1: TButton
    Left = 126
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 45
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
