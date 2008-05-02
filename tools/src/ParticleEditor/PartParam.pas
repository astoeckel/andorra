unit PartParam;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdParticles;

type
  TParamEdt = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    edt_start: TEdit;
    edt_stop: TEdit;
    Label2: TLabel;
    edt_var: TEdit;
    Label3: TLabel;
  private
    { Private-Deklarationen }
  public
    procedure Edit(AParam: TAdParticleParameter);
  end;

var
  ParamEdt: TParamEdt;

implementation

{$R *.dfm}

{ TParamEdt }

procedure TParamEdt.Edit(AParam: TAdParticleParameter);
begin
  edt_start.Text := FloatToStr(AParam.Start);
  edt_stop.Text := FloatToStr(AParam.Stop);
  edt_var.Text := FloatToStr(AParam.Variation);

  if ShowModal = mrOk then
  begin
    AParam.Start := StrToFloatDef(edt_start.Text, 0);
    AParam.Stop := StrToFloatDef(edt_stop.Text, 0);
    AParam.Variation := StrToFloatDef(edt_var.Text, 0);
  end;
end;

end.
