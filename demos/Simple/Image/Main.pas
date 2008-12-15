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
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms,
  AdDraws, AdClasses, AdSetupDlg, AdPerformanceCounter, AdPNG, AdConsts;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImageList:TAdImageList;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
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

      //Create a image list
      AdImageList := TAdImageList.Create(AdDraw);

      //Load the image from a file
      with AdImageList.Add('logo') do
        Texture.LoadGraphicFromFile('icon64.png');

      //Initialize all images in the list
      AdImageList.Restore;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end else
  begin
    AdSetup.Free;
    halt;
  end;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImageList.Free;
  AdPerCounter.Free;
  AdDraw.Free;
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
    AdDraw.ClearSurface(AdCol24_BlanchedAlmond);
    
    AdDraw.BeginScene;

    //Draw the image on the center of the screen
    with AdImageList.Find('logo') do
     Draw(AdDraw,
       (AdDraw.SurfaceRect.Right - Width) div 2,
       (AdDraw.SurfaceRect.Bottom - Height) div 2, 0);

    AdDraw.EndScene;

    //Bring the drawn things on the screen
    AdDraw.Flip;
  end;          

  Done := false;
end;

end.
