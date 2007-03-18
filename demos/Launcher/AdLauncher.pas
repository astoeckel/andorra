unit AdLauncher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    TrackBar1: TTrackBar;
    GroupBox3: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox2: TComboBox;
    Panel1: TPanel;
    Image1: TImage;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
