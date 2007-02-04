unit VarDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TVariations = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure Values(var Var1: integer; var Var2:integer);
  end;

var
  Variations: TVariations;
  ld:boolean;

implementation

{$R *.dfm}

{ TVariations }

procedure TVariations.FormCreate(Sender: TObject);
begin
  SpeedButton1.Down := ld;
end;

procedure TVariations.ScrollBar1Change(Sender: TObject);
begin
  if (Scrollbar1.Position > Scrollbar2.Position) or (SpeedButton1.Down) then
  begin
    Scrollbar2.Position := Scrollbar1.Position;
  end;
  Label3.Caption := inttostr(Scrollbar1.Position);
end;

procedure TVariations.ScrollBar2Change(Sender: TObject);
begin
  if (Scrollbar2.Position < Scrollbar1.Position) or (SpeedButton1.Down)then
  begin
    Scrollbar1.Position := Scrollbar2.Position;
  end;
  Label4.Caption := inttostr(Scrollbar2.Position);
end;

procedure TVariations.SpeedButton1Click(Sender: TObject);
begin
  ld := SpeedButton1.Down;
  if ld then ScrollBar1Change(nil);
end;

procedure TVariations.Values(var Var1, Var2: integer);
begin
  ScrollBar1.Position := Var1;
  ScrollBar2.Position := Var2;
  ShowModal;
  Var1 := ScrollBar1.Position;
  Var2 := ScrollBar2.Position;
end;

initialization
  ld := true;

finalization

end.
