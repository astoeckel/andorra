unit Wizzard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TElemWzrd = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    GroupBox1: TGroupBox;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TElemWzrd.Button1Click(Sender: TObject);
begin
  if (StrToIntDef(Edit1.Text,0) > 0) and
     (StrToIntDef(Edit2.Text,0) > 0) and
     (StrToIntDef(Edit3.Text,0) > 0) and
     (StrToIntDef(Edit4.Text,0) > 0) and
     (StrToIntDef(Edit5.Text,0) > 0) and
     (StrToIntDef(Edit6.Text,0) > 0) then
  begin
    ModalResult := mrOk;
  end
  else
  begin
    beep;
  end;
end;

end.
