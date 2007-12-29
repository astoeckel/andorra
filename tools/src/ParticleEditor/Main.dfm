object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Particle Editor'
  ClientHeight = 628
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
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 233
    Top = 0
    Width = 605
    Height = 609
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
    Height = 609
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 233
      Height = 609
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Basics'
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 153
          Height = 49
          Caption = 'Name'
          TabOrder = 0
          object Edit1: TEdit
            Left = 3
            Top = 16
            Width = 147
            Height = 21
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            OnChange = Edit1Change
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
            Top = 200
            Width = 216
            Height = 39
          end
          object Image2: TImage
            Left = 159
            Top = 17
            Width = 60
            Height = 47
            Center = True
            Picture.Data = {
              07544269746D6170360C0000424D360C00000000000036000000280000002000
              0000200000000100180000000000000C0000880B0000880B0000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              1111111111111111111111111111111111111111111111111111111111111111
              1100000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000111111111111
              2121212121212121212121212121212121212121212121212121212121212121
              2111111111111100000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000111111111111212121212121
              3131313131313131313131313131313131313131313131313131313131313131
              3121212121212111111111111100000000000000000000000000000000000000
              0000000000000000000000000000000000111111111111212121313131313131
              3131314141414141414141414141414141414141414141414141414141413131
              3131313131313121212111111111111100000000000000000000000000000000
              0000000000000000000000000000111111212121212121313131313131414141
              4141415050505050505050505050505050505050505050505050505050504141
              4141414131313131313121212121212111111100000000000000000000000000
              0000000000000000000000111111212121212121313131414141414141505050
              5050506060606060606060606060606060606060606060606060606060605050
              5050505041414141414131313121212121212111111100000000000000000000
              0000000000000000111111111111212121313131414141414141505050606060
              6060607070707070707070707070707070707070707070707070707070706060
              6060606050505041414141414131313121212111111111111100000000000000
              0000000000000000111111212121313131414141414141505050606060606060
              7070708080808080808080808080808080808080808080808080808080807070
              7060606060606050505041414141414131313121212111111100000000000000
              0000000000111111212121313131313131414141505050606060707070707070
              8080808080809090909090909090909090909090909090909090908080808080
              8070707070707060606050505041414131313131313121212111111100000000
              0000000000111111212121313131414141505050606060606060707070808080
              909090909090A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A09090909090
              9080808070707060606060606050505041414131313121212111111100000000
              0000111111212121313131313131414141505050606060707070808080909090
              909090A0A0A0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0A0A0A09090
              9090909080808070707060606050505041414131313131313121212111111100
              0000111111212121313131414141505050606060707070808080808080909090
              A0A0A0B0B0B0B0B0B0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0B0B0B0B0B0B0A0A0
              A090909080808080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0B0B0B0C0C0C0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0C0C0C0B0B0B0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0C0C0C0D0D0D0E0E0E0E0E0E0E0E0E0E0E0E0E0E0E0D0D0D0C0C0C0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0C0C0C0D0D0D0E0E0E0F0F0F0F0F0F0F0F0F0E0E0E0D0D0D0C0C0C0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0C0C0C0D0D0D0E0E0E0F0F0F0FFFFFFF0F0F0E0E0E0D0D0D0C0C0C0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0C0C0C0D0D0D0E0E0E0F0F0F0F0F0F0F0F0F0E0E0E0D0D0D0C0C0C0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0C0C0C0D0D0D0E0E0E0E0E0E0E0E0E0E0E0E0E0E0E0D0D0D0C0C0C0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080909090A0A0A0
              B0B0B0B0B0B0C0C0C0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0C0C0C0B0B0B0B0B0
              B0A0A0A090909080808070707060606050505041414131313121212111111100
              0000111111212121313131414141505050606060707070808080808080909090
              A0A0A0B0B0B0B0B0B0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0B0B0B0B0B0B0A0A0
              A090909080808080808070707060606050505041414131313121212111111100
              0000111111212121313131313131414141505050606060707070808080909090
              909090A0A0A0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0A0A0A09090
              9090909080808070707060606050505041414131313131313121212111111100
              0000000000111111212121313131414141505050606060606060707070808080
              909090909090A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A09090909090
              9080808070707060606060606050505041414131313121212111111100000000
              0000000000111111212121313131313131414141505050606060707070707070
              8080808080809090909090909090909090909090909090909090908080808080
              8070707070707060606050505041414131313131313121212111111100000000
              0000000000000000111111212121313131414141414141505050606060606060
              7070708080808080808080808080808080808080808080808080808080807070
              7060606060606050505041414141414131313121212111111100000000000000
              0000000000000000111111111111212121313131414141414141505050606060
              6060607070707070707070707070707070707070707070707070707070706060
              6060606050505041414141414131313121212111111111111100000000000000
              0000000000000000000000111111212121212121313131414141414141505050
              5050506060606060606060606060606060606060606060606060606060605050
              5050505041414141414131313121212121212111111100000000000000000000
              0000000000000000000000000000111111212121212121313131313131414141
              4141415050505050505050505050505050505050505050505050505050504141
              4141414131313131313121212121212111111100000000000000000000000000
              0000000000000000000000000000000000111111111111212121313131313131
              3131314141414141414141414141414141414141414141414141414141413131
              3131313131313121212111111111111100000000000000000000000000000000
              0000000000000000000000000000000000000000111111111111212121212121
              3131313131313131313131313131313131313131313131313131313131313131
              3121212121212111111111111100000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000111111111111
              2121212121212121212121212121212121212121212121212121212121212121
              2111111111111100000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              1111111111111111111111111111111111111111111111111111111111111111
              1100000000000000000000000000000000000000000000000000000000000000
              0000}
          end
          object Label5: TLabel
            Left = 3
            Top = 268
            Width = 92
            Height = 13
            Caption = 'Image blend mode:'
          end
          object Label37: TLabel
            Left = 3
            Top = 13
            Width = 151
            Height = 31
            AutoSize = False
            Caption = 'Note: The Image is not included in the saved file.'
            Transparent = True
            WordWrap = True
          end
          object SpeedButton1: TSpeedButton
            Left = 61
            Top = 167
            Width = 23
            Height = 22
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
              3333333333777F33333333333309033333333333337F7F333333333333090333
              33333333337F7F33333333333309033333333333337F7F333333333333090333
              33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
              3333333777737777F333333099999990333333373F3333373333333309999903
              333333337F33337F33333333099999033333333373F333733333333330999033
              3333333337F337F3333333333099903333333333373F37333333333333090333
              33333333337F7F33333333333309033333333333337373333333333333303333
              333333333337F333333333333330333333333333333733333333}
            NumGlyphs = 2
            OnClick = SpeedButton1Click
          end
          object SpeedButton2: TSpeedButton
            Left = 90
            Top = 167
            Width = 23
            Height = 22
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
              333333333337F33333333333333033333333333333373F333333333333090333
              33333333337F7F33333333333309033333333333337373F33333333330999033
              3333333337F337F33333333330999033333333333733373F3333333309999903
              333333337F33337F33333333099999033333333373333373F333333099999990
              33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
              33333333337F7F33333333333309033333333333337F7F333333333333090333
              33333333337F7F33333333333309033333333333337F7F333333333333090333
              33333333337F7F33333333333300033333333333337773333333}
            NumGlyphs = 2
            OnClick = SpeedButton2Click
          end
          object SpeedButton3: TSpeedButton
            Left = 3
            Top = 167
            Width = 23
            Height = 22
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFF99FFF
              FFFFFFFFFF9999FFFFFFFFFFFF9999FFFFFFFFFFFF9999FFFFFFFFFFFF9999FF
              FFFFFFFFFF9999FFFFFFF99999999999999F9999999999999999999999999999
              9999F99999999999999FFFFFFF9999FFFFFFFFFFFF9999FFFFFFFFFFFF9999FF
              FFFFFFFFFF9999FFFFFFFFFFFF9999FFFFFFFFFFFFF99FFFFFFF}
            OnClick = SpeedButton3Click
          end
          object SpeedButton4: TSpeedButton
            Left = 32
            Top = 167
            Width = 23
            Height = 22
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFCCCCCCCCCCCCCCFCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              CCCCFCCCCCCCCCCCCCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            OnClick = SpeedButton4Click
          end
          object Button1: TButton
            Left = 3
            Top = 39
            Width = 150
            Height = 25
            Caption = 'Load new image file...'
            TabOrder = 0
            OnClick = Button1Click
          end
          object CheckBox2: TCheckBox
            Left = 3
            Top = 245
            Width = 134
            Height = 17
            Caption = 'Draw background mask'
            Checked = True
            State = cbChecked
            TabOrder = 1
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
            TabOrder = 2
            Text = 'bmAdd'
            OnChange = ComboBox1Change
            Items.Strings = (
              'bmAlpha'
              'bmAdd'
              'bmMask')
          end
          object ListBox1: TListBox
            Left = 3
            Top = 70
            Width = 216
            Height = 91
            Style = lbOwnerDrawFixed
            ItemHeight = 13
            TabOrder = 3
            OnDblClick = ListBox1DblClick
            OnDrawItem = ListBox1DrawItem
          end
          object Button2: TButton
            Left = 119
            Top = 167
            Width = 100
            Height = 22
            Caption = 'Set Alpha...'
            TabOrder = 4
            OnClick = Button2Click
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
            OnChange = Edit6Change
          end
          object Edit7: TEdit
            Left = 139
            Top = 35
            Width = 59
            Height = 21
            TabOrder = 1
            Text = '0'
            OnChange = Edit7Change
          end
        end
        object Button5: TButton
          Left = 165
          Top = 14
          Width = 57
          Height = 25
          Caption = 'Reset'
          TabOrder = 3
          OnClick = Button5Click
        end
        object GroupBox4: TGroupBox
          Left = 0
          Top = 447
          Width = 223
          Height = 112
          Caption = 'Particle creation (only for the editor)'
          TabOrder = 4
          object Label11: TLabel
            Left = 71
            Top = 46
            Width = 13
            Height = 13
            Caption = 'ms'
          end
          object Label10: TLabel
            Left = 71
            Top = 96
            Width = 40
            Height = 13
            Caption = 'Particles'
          end
          object Edit8: TEdit
            Left = 11
            Top = 38
            Width = 54
            Height = 21
            TabOrder = 0
            Text = '1,0'
            OnChange = Edit8Change
          end
          object RadioButton1: TRadioButton
            Left = 3
            Top = 19
            Width = 166
            Height = 17
            Caption = 'Create a particle every...'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = Edit8Change
          end
          object Edit17: TEdit
            Left = 11
            Top = 88
            Width = 54
            Height = 21
            TabOrder = 2
            Text = '200'
            OnChange = Edit17Change
          end
          object RadioButton2: TRadioButton
            Left = 3
            Top = 65
            Width = 212
            Height = 17
            Caption = 'If there are no particles, create'
            TabOrder = 3
            OnClick = Edit17Change
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Movement'
        ImageIndex = 1
        object GroupBox5: TGroupBox
          Left = 0
          Top = 0
          Width = 223
          Height = 209
          Caption = 'Size, Speed and Rotation'
          TabOrder = 0
          object Label16: TLabel
            Left = 3
            Top = 41
            Width = 23
            Height = 13
            Caption = 'Size:'
          end
          object Label19: TLabel
            Left = 3
            Top = 73
            Width = 45
            Height = 13
            Caption = 'Rotation:'
          end
          object Label18: TLabel
            Left = 111
            Top = 73
            Width = 5
            Height = 13
            Caption = #176
          end
          object Label15: TLabel
            Left = 111
            Top = 46
            Width = 27
            Height = 13
            Caption = 'in x/1'
          end
          object Label17: TLabel
            Left = 198
            Top = 73
            Width = 5
            Height = 13
            Caption = #176
          end
          object Label12: TLabel
            Left = 196
            Top = 46
            Width = 27
            Height = 13
            Caption = 'in x/1'
            Transparent = True
          end
          object Label14: TLabel
            Left = 135
            Top = 19
            Width = 51
            Height = 13
            Caption = 'End Value:'
          end
          object Label13: TLabel
            Left = 48
            Top = 19
            Width = 57
            Height = 13
            Caption = 'Start value:'
          end
          object Label20: TLabel
            Left = 198
            Top = 113
            Width = 21
            Height = 13
            Caption = 'px/s'
          end
          object Label21: TLabel
            Left = 111
            Top = 113
            Width = 21
            Height = 13
            Caption = 'px/s'
          end
          object Label22: TLabel
            Left = 3
            Top = 113
            Width = 43
            Height = 13
            Caption = 'Speed X:'
          end
          object Label23: TLabel
            Left = 3
            Top = 132
            Width = 43
            Height = 13
            Caption = 'Speed Y:'
          end
          object Label24: TLabel
            Left = 111
            Top = 140
            Width = 21
            Height = 13
            Caption = 'px/s'
          end
          object Label25: TLabel
            Left = 198
            Top = 135
            Width = 21
            Height = 13
            Caption = 'px/s'
          end
          object Label34: TLabel
            Left = 3
            Top = 159
            Width = 79
            Height = 13
            Caption = 'Speed variation:'
          end
          object Label35: TLabel
            Left = 111
            Top = 186
            Width = 11
            Height = 13
            Caption = '%'
          end
          object Label36: TLabel
            Left = 3
            Top = 186
            Width = 28
            Height = 13
            Caption = 'max.:'
          end
          object Edit11: TEdit
            Left = 54
            Top = 65
            Width = 51
            Height = 21
            TabOrder = 0
            OnChange = Edit11Change
          end
          object Edit9: TEdit
            Left = 54
            Top = 38
            Width = 51
            Height = 21
            TabOrder = 1
            OnChange = Edit9Change
          end
          object Edit10: TEdit
            Left = 137
            Top = 38
            Width = 53
            Height = 21
            TabOrder = 2
            OnChange = Edit9Change
          end
          object Edit12: TEdit
            Left = 139
            Top = 65
            Width = 53
            Height = 21
            TabOrder = 3
            OnChange = Edit11Change
          end
          object Edit13: TEdit
            Left = 54
            Top = 105
            Width = 51
            Height = 21
            TabOrder = 4
            OnChange = Edit13Change
          end
          object Edit14: TEdit
            Left = 139
            Top = 105
            Width = 53
            Height = 21
            TabOrder = 5
            OnChange = Edit13Change
          end
          object Edit15: TEdit
            Left = 54
            Top = 132
            Width = 51
            Height = 21
            TabOrder = 6
            OnChange = Edit13Change
          end
          object Edit16: TEdit
            Left = 139
            Top = 132
            Width = 53
            Height = 21
            TabOrder = 7
            OnChange = Edit13Change
          end
          object Edit18: TEdit
            Left = 54
            Top = 178
            Width = 51
            Height = 21
            TabOrder = 8
            Text = '0'
            OnChange = Edit18Change
          end
        end
        object GroupBox6: TGroupBox
          Left = 0
          Top = 215
          Width = 222
          Height = 298
          Caption = 'Forces and creation angle'
          TabOrder = 1
          object Label26: TLabel
            Left = 109
            Top = 104
            Width = 56
            Height = 13
            Caption = 'Open: 360'#176
          end
          object Label27: TLabel
            Left = 109
            Top = 61
            Width = 45
            Height = 13
            Caption = 'Angle: 0'#176
          end
          object Label28: TLabel
            Left = 3
            Top = 21
            Width = 74
            Height = 13
            Caption = 'Creation angle:'
          end
          object Label29: TLabel
            Left = 3
            Top = 157
            Width = 31
            Height = 13
            Caption = 'Force:'
          end
          object Label30: TLabel
            Left = 3
            Top = 176
            Width = 31
            Height = 13
            Caption = 'Angle:'
          end
          object Label31: TLabel
            Left = 159
            Top = 218
            Width = 43
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'#176
          end
          object Label32: TLabel
            Left = 24
            Top = 274
            Width = 178
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0 px/s'
          end
          object Label33: TLabel
            Left = 3
            Top = 232
            Width = 34
            Height = 13
            Caption = 'Power:'
          end
          object PaintBox1: TPaintBox
            Left = 3
            Top = 40
            Width = 100
            Height = 100
          end
          object ScrollBar1: TScrollBar
            Left = 109
            Top = 123
            Width = 110
            Height = 17
            Max = 360
            Min = 2
            PageSize = 0
            Position = 359
            TabOrder = 0
            OnChange = ScrollBar2Change
          end
          object ScrollBar2: TScrollBar
            Left = 109
            Top = 80
            Width = 110
            Height = 17
            Max = 359
            PageSize = 0
            TabOrder = 1
            OnChange = ScrollBar2Change
          end
          object ScrollBar3: TScrollBar
            Left = 16
            Top = 195
            Width = 186
            Height = 17
            Max = 360
            PageSize = 0
            TabOrder = 2
            OnChange = ScrollBar3Change
          end
          object ScrollBar4: TScrollBar
            Left = 16
            Top = 251
            Width = 186
            Height = 17
            Max = 500
            PageSize = 0
            TabOrder = 3
            OnChange = ScrollBar3Change
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 609
    Width = 838
    Height = 19
    Panels = <>
    SimplePanel = True
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
      object Save1: TMenuItem
        Caption = 'Save'
        Enabled = False
        ShortCut = 16467
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
        OnClick = Saveas1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object LoadFile1: TMenuItem
        Caption = 'Load file...'
        OnClick = LoadFile1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object Images1: TMenuItem
      Caption = 'Images'
      object Loadnewimagefile1: TMenuItem
        Caption = 'Load new image file...'
        OnClick = Button1Click
      end
    end
    object Environment1: TMenuItem
      Caption = 'Environment'
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
    OnClose = ColorDialog1Close
    OnShow = ColorDialog1Show
    Left = 336
    Top = 16
  end
  object OpenPictureDialog1: TOpenPictureDialog
    OnClose = ColorDialog1Close
    OnShow = ColorDialog1Show
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 368
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    OnClose = ColorDialog1Close
    OnShow = ColorDialog1Show
    Filter = 'Andorra Particle File (*.apf)|*.apf'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 240
    Top = 48
  end
  object OpenDialog1: TOpenDialog
    OnClose = ColorDialog1Close
    OnShow = ColorDialog1Show
    Filter = 'Andorra Particle File (*.apf)|*.apf|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 272
    Top = 48
  end
end
