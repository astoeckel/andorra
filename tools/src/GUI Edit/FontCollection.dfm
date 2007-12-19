object FontCollectionDlg: TFontCollectionDlg
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Font editor'
  ClientHeight = 309
  ClientWidth = 361
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 265
    Caption = 'Available Fonts'
    TabOrder = 0
    object ListBox1: TListBox
      Left = 2
      Top = 15
      Width = 341
      Height = 248
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnKeyDown = ListBox1KeyDown
    end
  end
  object Button1: TButton
    Left = 280
    Top = 275
    Width = 73
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button3: TButton
    Left = 8
    Top = 277
    Width = 89
    Height = 25
    Caption = 'Add font...'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button6: TButton
    Left = 103
    Top = 277
    Width = 92
    Height = 25
    Caption = 'Delete font'
    TabOrder = 3
    OnClick = Button6Click
  end
end
