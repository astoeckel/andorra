{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AdDraws, AdPNG, AdClasses, AdSetupDlg, AdPerformanceCounter, AdConsts;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure Idle(Sender:TObject;var Done:boolean);
  public
    AdDraw:TAdDraw;
    AdImage:TAdImage;
    AdPerCounter:TAdPerformanceCounter;
    Bmp:TBitmap;
    Ico:TIcon;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  //Show the plugin selection section only
  AdSetup.Sections := [dlgPlugin];

  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;
    
    //Try to initialize the TAdDraw
    if AdDraw.Initialize then
    begin
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;

      //Create a simple VCL bitmap
      Bmp := TBitmap.Create;
      Bmp.Width := ClientWidth;
      Bmp.Height := ClientHeight;

      //Create a single image to draw on
      AdImage := TAdImage.Create(AdDraw);
      with AdImage do
      begin
        Texture.LoadFromGraphic(Bmp);
        Restore;
      end;
      
      //Load a simple icon that will be drawn on the bitmap
      Ico := TIcon.Create;
      Ico.LoadFromFile('icon32.ico');
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end
  else
  begin
    AdSetup.Free;
    halt;
  end;                             
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //Free the created bitmaps
  Bmp.Free;
  Ico.Free;
  
  AdImage.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Bmp.Canvas.MoveTo(X, Y);
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
        Pen.Color := AdCol24_ForestGreen;
        LineTo(X,Y)
      end;
    end;

    //Store the bitmap in the texture
    AdImage.Texture.LoadFromGraphic(bmp);
  end;
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
  //Clear the bitmap
  Bmp.Canvas.Rectangle(-1,-1,ClientWidth+1,ClientHeight+1);
  //Store the bitmap in the texture
  AdImage.Texture.LoadFromGraphic(bmp);
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference. The information gained here is not used
    //in this demo - But you'll need this class in most cases
    AdPerCounter.Calculate;

    //Clear the surface of the TAdDraw with black color
    AdDraw.ClearSurface(AdCol24_Black);

    AdDraw.BeginScene;

    //Draw the image
    AdImage.Draw(AdDraw,0,0,0);
    
    AdDraw.EndScene;
    
    //Bring the drawn things on the screen
    AdDraw.Flip;

    Done := false;
  end;
end;

end.
