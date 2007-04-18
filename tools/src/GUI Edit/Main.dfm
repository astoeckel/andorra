object MainDlg: TMainDlg
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Andorra 2D - GUI Editor'
  ClientHeight = 84
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  ScreenSnap = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 664
    Height = 84
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 64
  end
  object MainMenu1: TMainMenu
    Left = 80
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
      object NewGUI1: TMenuItem
        Caption = 'New GUI'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Loadfile1: TMenuItem
        Caption = 'Load file'
      end
      object Loadrecent1: TMenuItem
        Caption = 'Load recent'
        object Norecentfilesfound1: TMenuItem
          Caption = 'No recent files found'
          Enabled = False
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SaveGUIas1: TMenuItem
        Caption = 'Save file'
      end
      object Savefileas1: TMenuItem
        Caption = 'Save file as...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Project1: TMenuItem
      Caption = 'Designer'
      OnClick = Project1Click
      object Designmode1: TMenuItem
        Caption = 'Designmode'
        OnClick = Designmode1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Grid1: TMenuItem
        Caption = 'Grid'
        OnClick = Grid1Click
        object Off1: TMenuItem
          Tag = 1
          Caption = 'Off'
          GroupIndex = 1
          RadioItem = True
          OnClick = N10x101Click
        end
        object N5: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object N3x31: TMenuItem
          Tag = 3
          Caption = '3x3'
          GroupIndex = 1
          RadioItem = True
          OnClick = N10x101Click
        end
        object N5x51: TMenuItem
          Tag = 5
          Caption = '5x5'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = N10x101Click
        end
        object N10x101: TMenuItem
          Tag = 10
          Caption = '10x10'
          GroupIndex = 1
          RadioItem = True
          OnClick = N10x101Click
        end
      end
    end
    object Windows1: TMenuItem
      Caption = 'Windows'
      OnClick = Windows1Click
      object Designer1: TMenuItem
        Caption = 'Designer'
        OnClick = Designer1Click
      end
      object Objectinspector1: TMenuItem
        Caption = 'Objectinspector'
        OnClick = Objectinspector1Click
      end
      object Structure1: TMenuItem
        Caption = 'Structure'
        OnClick = Structure1Click
      end
    end
  end
  object XPManifest1: TXPManifest
    Left = 112
    Top = 8
  end
  object ImageList1: TImageList
    Height = 24
    Width = 24
    Left = 144
    Top = 8
  end
end
