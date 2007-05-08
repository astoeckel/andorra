object Images: TImages
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Image Editor'
  ClientHeight = 252
  ClientWidth = 329
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
    Left = 247
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 247
    Top = 39
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button3: TButton
    Left = 8
    Top = 223
    Width = 57
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 184
    Top = 223
    Width = 57
    Height = 25
    Caption = 'Load...'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 121
    Top = 223
    Width = 57
    Height = 25
    Caption = 'Save...'
    TabOrder = 4
    OnClick = Button6Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 233
    Height = 209
    BevelOuter = bvLowered
    TabOrder = 5
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 231
      Height = 207
      Align = alClient
      Center = True
      ExplicitLeft = -1
      ExplicitWidth = 207
      ExplicitHeight = 233
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 8
    Top = 8
  end
  object SavePictureDialog1: TSavePictureDialog
    Left = 40
    Top = 8
  end
end
