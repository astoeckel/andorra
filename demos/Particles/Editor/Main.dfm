object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Explosion'
  ClientHeight = 552
  ClientWidth = 838
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 233
    Top = 0
    Width = 605
    Height = 552
    Align = alClient
    BevelOuter = bvLowered
    BorderWidth = 2
    Color = clBlack
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseMove = FormMouseMove
    OnResize = Panel1Resize
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 552
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 233
      Height = 552
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Basics'
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 222
          Height = 49
          Caption = 'Name'
          TabOrder = 0
          object Edit1: TEdit
            Left = 3
            Top = 16
            Width = 216
            Height = 21
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            Text = 'My Particle'
          end
        end
        object GroupBox2: TGroupBox
          Left = 0
          Top = 55
          Width = 222
          Height = 314
          Caption = 'Image and Colors'
          TabOrder = 1
          object Image1: TImage
            Left = 3
            Top = 216
            Width = 216
            Height = 24
          end
          object Label1: TLabel
            Left = 143
            Top = 109
            Width = 11
            Height = 13
            Caption = 'R:'
          end
          object Label2: TLabel
            Left = 143
            Top = 138
            Width = 11
            Height = 13
            Caption = 'G:'
          end
          object Label3: TLabel
            Left = 143
            Top = 165
            Width = 10
            Height = 13
            Caption = 'B:'
          end
          object Label4: TLabel
            Left = 143
            Top = 197
            Width = 11
            Height = 13
            Caption = 'A:'
          end
          object Image2: TImage
            Left = 159
            Top = 17
            Width = 60
            Height = 47
            Center = True
          end
          object Label5: TLabel
            Left = 3
            Top = 268
            Width = 92
            Height = 13
            Caption = 'Image blend mode:'
          end
          object Button1: TButton
            Left = 3
            Top = 16
            Width = 150
            Height = 25
            Caption = 'Load new image file...'
            TabOrder = 0
            OnClick = Button1Click
          end
          object CheckBox1: TCheckBox
            Left = 3
            Top = 47
            Width = 150
            Height = 17
            Caption = 'Include image in particle file'
            TabOrder = 1
          end
          object ListBox1: TListBox
            Left = 3
            Top = 101
            Width = 134
            Height = 109
            Style = lbOwnerDrawVariable
            ItemHeight = 13
            TabOrder = 2
            OnClick = ListBox1Click
            OnDblClick = ListBox1DblClick
            OnDrawItem = ListBox1DrawItem
            OnKeyDown = ListBox1KeyDown
          end
          object Button2: TButton
            Left = 143
            Top = 70
            Width = 72
            Height = 25
            Caption = 'Add color'
            TabOrder = 3
            OnClick = Button2Click
          end
          object Edit2: TEdit
            Left = 160
            Top = 101
            Width = 55
            Height = 21
            TabOrder = 4
            Text = '255'
          end
          object Edit3: TEdit
            Left = 160
            Top = 128
            Width = 55
            Height = 21
            TabOrder = 5
            Text = '255'
          end
          object Edit4: TEdit
            Left = 160
            Top = 155
            Width = 55
            Height = 21
            TabOrder = 6
            Text = '255'
          end
          object Edit5: TEdit
            Left = 160
            Top = 189
            Width = 55
            Height = 21
            TabOrder = 7
            Text = '255'
          end
          object CheckBox2: TCheckBox
            Left = 3
            Top = 245
            Width = 134
            Height = 17
            Caption = 'Draw background mask'
            Checked = True
            State = cbChecked
            TabOrder = 8
            OnClick = CheckBox2Click
          end
          object ComboBox1: TComboBox
            Left = 9
            Top = 287
            Width = 84
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 1
            TabOrder = 9
            Text = 'bmAdd'
            Items.Strings = (
              'bmAlpha'
              'bmAdd'
              'bmMask')
          end
          object Button3: TButton
            Left = 3
            Top = 70
            Width = 62
            Height = 25
            Caption = 'Move up'
            Enabled = False
            TabOrder = 10
            OnClick = Button3Click
          end
          object Button4: TButton
            Left = 71
            Top = 70
            Width = 66
            Height = 25
            Caption = 'Move down'
            Enabled = False
            TabOrder = 11
            OnClick = Button4Click
          end
        end
        object GroupBox3: TGroupBox
          Left = 0
          Top = 375
          Width = 222
          Height = 66
          Caption = 'Lifetime'
          TabOrder = 2
          object Label6: TLabel
            Left = 3
            Top = 16
            Width = 101
            Height = 13
            Caption = 'Lifetime of a particle:'
          end
          object Label7: TLabel
            Left = 71
            Top = 43
            Width = 20
            Height = 13
            Caption = 'sec.'
          end
          object Label8: TLabel
            Left = 139
            Top = 16
            Width = 46
            Height = 13
            Caption = 'Variation:'
          end
          object Label9: TLabel
            Left = 204
            Top = 43
            Width = 11
            Height = 13
            Caption = '%'
          end
          object Edit6: TEdit
            Left = 3
            Top = 35
            Width = 62
            Height = 21
            TabOrder = 0
            Text = '1,00'
          end
          object Edit7: TEdit
            Left = 139
            Top = 35
            Width = 59
            Height = 21
            TabOrder = 1
            Text = '0'
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Movement'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 534
      end
    end
  end
  object Timer1: TTimer
    Interval = 1
    Left = 240
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 272
    Top = 16
    object Datei1: TMenuItem
      Caption = 'File'
      object PartikelLaden1: TMenuItem
        Caption = 'Load particles...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Saveparticles1: TMenuItem
        Caption = 'Save particles'
      end
      object Saveparticlesas1: TMenuItem
        Caption = 'Save particles as...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
      end
    end
    object Images1: TMenuItem
      Caption = 'Images'
      object Loadnewimagefile1: TMenuItem
        Caption = 'Load new image file...'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Includeimageinparticlefile1: TMenuItem
        Caption = 'Include image in particle file'
      end
    end
    object Environment1: TMenuItem
      Caption = 'Environment'
      object Addparticlesystem1: TMenuItem
        Caption = 'Add particle system'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Backgroundcolor1: TMenuItem
        Caption = 'Background color...'
        OnClick = Backgroundcolor1Click
      end
    end
  end
  object XPManifest1: TXPManifest
    Left = 304
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Left = 336
    Top = 16
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 368
    Top = 16
  end
end
