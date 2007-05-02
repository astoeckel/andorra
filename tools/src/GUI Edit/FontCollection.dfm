object FontColl: TFontColl
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Font Collection'
  ClientHeight = 362
  ClientWidth = 335
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
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 319
    Height = 317
    Style = lbOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 0
    OnClick = ListBox1Click
    OnDrawItem = ListBox1DrawItem
  end
  object Button1: TButton
    Left = 8
    Top = 331
    Width = 73
    Height = 25
    Caption = 'Add font...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 87
    Top = 331
    Width = 90
    Height = 25
    Caption = 'Delete font'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 264
    Top = 331
    Width = 63
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button4: TButton
    Left = 183
    Top = 331
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 16
    Top = 16
  end
end
