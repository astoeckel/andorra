unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  AdDraws, AdClasses, AdSetupDlg, AdPng, AdCanvas, AdPerformanceCounter,
  AdTypes, AdFont, AdFontFactory, AdStandardFontGenerator;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  mx,my:integer;
  c:double;
  s:string;

const
  dir = '..\demos\Simple\TextDemo\';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
  StrList:TStringList;
  p:PAnsiChar;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);

  c := -500;

  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw1;
  AdSetupDlg.Form := self;

  if AdSetupDlg.Execute then
  begin
    if AdDraw1.Initialize then
    begin
      Application.OnIdle := Idle;

      AdDraw1.AdAppl.Setup3DScene(
        ClientWidth, ClientHeight, AdVector3(ClientWidth div 2, ClientHeight div 2, -1000),
        AdVector3(ClientWidth div 2, ClientHeight * 1.2, 0), AdVector3(0,-1,0));

      StrList := TStringList.Create;
      StrList.LoadFromFile(dir+'engine-wars.txt');
      p := strlist.GetText;
      s := p;
      StrDispose(p);
      StrList.Free;
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                  'mode or another video adapter.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
  AdSetupDlg.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
var
  i:integer;
  mat1,mat2:TAdMatrix;
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;

    c := c + AdPerCounter.TimeGap * 0.05;

    mat1 := AdMatrix_Translate(0, ClientHeight, c);
    mat2 := AdMatrix_RotationX(-Pi/2);
    mat1 := AdMatrix_Multiply(mat2,mat1);
    with AdDraw1.Fonts.GenerateFont('Arial', 72, []) do
    begin
      with TAdSimpleTypeSetter(TypeSetter) do
      begin
        DrawMode := [dtWordWrap, dtTop, dtCenter, dtDoLineFeeds];
        CharWidth := 0.5;
        CharHeight := 0.5;
      end;
      Color := Ad_ARGB(255,230,200,0);
      TransformationMatrix := mat1;
      TextOut(AdRect(ClientWidth div 2 - 400, 0, ClientWidth div 2 + 400, ClientHeight), s);
    end;

    AdDraw1.EndScene;
    AdDraw1.Flip;
  end;

  Done := false;
end;

end.
