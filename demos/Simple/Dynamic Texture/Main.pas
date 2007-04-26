unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdImageList1:TAdImageList;
    AdPerCounter:TPerformanceCounter;
    Bmp:TBitmap;
    Ico:TIcon;
    procedure Idle(Sender:TObject;var Done:boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';

  ReportMemoryLeaksOnShutDown := True;

  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;
    Bmp := TBitmap.Create;
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    Bmp.Transparent := true;
    Bmp.TransparentMode := tmFixed;
    Bmp.TransparentColor := clWhite;
    AdImageList1 := TAdImageList.Create(AdDraw1);
    with AdImageList1.Add('surface') do
    begin
      Texture.LoadFromGraphic(Bmp);
      Restore;
    end;
    Ico := TIcon.Create;
    Ico.LoadFromFile('icon32.ico');
    AdPerCounter := TPerformanceCounter.Create;
  end
  else
  begin
    ShowMessage('Unable to initialize Andorra 2D');
    Close;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Bmp.Free;
  Ico.Free;
  AdImageList1.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Bmp.Canvas.MoveTo(X,Y);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
  begin
    if not (ssRight in Shift) then
    begin
      Bmp.Canvas.Draw(X-16,Y-16,Ico);
    end
    else
    begin
      with Bmp.Canvas do
      begin
        Pen.Color := Rgb(254,254,254);
        LineTo(X,Y)
      end;
    end;
    AdImageList1[0].Texture.LoadFromGraphic(bmp);
  end;
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
  Bmp.Canvas.Rectangle(-1,-1,ClientWidth+1,ClientHeight+1);
  AdImageList1[0].Texture.LoadFromGraphic(bmp);
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS: '+inttostr(AdPerCounter.FPS);

    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    AdImageList1.Items[0].Draw(AdDraw1,0,0,0);
    AdDraw1.EndScene;
    AdDraw1.Flip;

    Done := false;
  end;
end;

end.
