program Physic;

uses
  Forms,
  Main in 'Main.pas'{$IFDEF FPC}, LazOpenGLContext{$ENDIF};

{$R *.res}

{$IFDEF FPC}
{$IFDEF WINDOWS}{$R Physic.rc}{$ENDIF}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
