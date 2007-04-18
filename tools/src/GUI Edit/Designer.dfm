object DesignerDlg: TDesignerDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'GUI Designer'
  ClientHeight = 576
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ScreenSnap = True
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 8
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Cut1: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
        OnClick = Cut1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        ShortCut = 46
        OnClick = Delete1Click
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Sendtoback1: TMenuItem
      Caption = 'Send to back'
      OnClick = Sendtoback1Click
    end
    object Bringtofront1: TMenuItem
      Caption = 'Bring to front'
      OnClick = Bringtofront1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object EditXML1: TMenuItem
      Caption = 'Edit XML'
    end
  end
end
