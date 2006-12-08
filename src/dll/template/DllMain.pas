{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DllMain.pas
* Comment: Only a DLL template
}

unit DX3DMain;

interface

uses AndorraUtils, Classes, Windows, Graphics, Math, SysUtils;

//Initialization
function CreateApplication:TAndorraApplication;stdcall;
procedure DestroyApplication(Appl:TAndorraApplication);stdcall;
function InitDisplay(Appl:TAndorraApplication; AWindow:hWnd; AOptions:TAdDrawModes; ADisplay:TAdDrawDisplay):boolean;stdcall;
procedure SetTextureQuality(Appl:TAndorraApplication;Quality:TAndorraTextureQuality);stdcall;

//Render Control
procedure BeginScene(Appl:TAndorraApplication);stdcall;
procedure EndScene(Appl:TAndorraApplication);stdcall;
procedure ClearScene(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
procedure SetupScene(Appl:TAndorraApplication;AWidth,AHeight:integer);stdcall;
procedure Flip(Appl:TAndorraApplication);stdcall;
procedure SetOptions(Appl:TAndorraApplication;AOptions:TAdDrawModes);stdcall;
procedure SetAmbientLight(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;

//SpriteControl
function CreateImage(Appl:TAndorraApplication):TAndorraImage;stdcall;
procedure DrawImage(DestApp:TAndorraApplication;Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);stdcall;
procedure DestroyImage(Img:TAndorraImage);stdcall;
procedure ImageLoadTexture(Img:TAndorraImage;ATexture:TAndorraTexture);stdcall;
procedure SetImageColor(Img:TAndorraImage;AColor:TAndorraColor);stdcall;
procedure SetTextureXMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
procedure SetTextureYMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
procedure SetImageDetail(Img:TAndorraImage;ADetail:integer);stdcall;

//Texture Creation
function LoadTextureFromFile(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
function LoadTextureFromFileEx(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
function LoadTextureFromBitmap(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;stdcall;
procedure FreeTexture(ATexture:TAndorraTexture);stdcall;
procedure AddTextureAlphaChannel(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
function GetTextureInfo(Tex:TAndorraTexture):TImageInfo;stdcall;
procedure SetTextureAlpha(Tex:TAndorraTexture;AValue:Byte);stdcall;
function CheckTextureMem(Appl:TAndorraApplication):integer;stdcall;

//Lights
function CreateLight(Appl:TAndorraApplication):TAndorraLight;stdcall;
procedure DestroyLight(ALight:TAndorraLight);stdcall;
procedure RestoreLight(ALight:TAndorraLight;Data:TLight);stdcall;
procedure EnableLight(ALight:TAndorraLight);stdcall;
procedure DisableLight(ALight:TAndorraLight);stdcall;

//LogSystem
procedure SetLogProc(Appl:TAndorraApplication;ALogProc:TAdLogProc;AAppl:Pointer);

implementation

//Initialization
function CreateApplication:TAndorraApplication;
begin
  //Create an application object. Returns the pointer to this application object.
end;

function InitDisplay(Appl:TAndorraApplication; AWindow:hWnd; AOptions:TAdDrawModes;
   ADisplay:TAdDrawDisplay):boolean;
begin
  //Initializes the display.
  //Appl --> Pointer to the application object
  //AWindows --> The window the application is displayed in.
  //AOptions --> Contains the options (see TAdDrawModes)
  //ADisplay --> Contains information how to display the scene in the fullscreen mode.
end;

procedure DestroyApplication(Appl:TAndorraApplication);
begin
  //Destroys an application object
end;

//Log System
procedure SetLogProc(Appl:TAndorraApplication;ALogProc:TAdLogProc;AAppl:Pointer);
begin
  //Set a call back function for an application object.
  //ALog --> Pointer to the function.
  //AAppl --> Pointer to the TAdDraw object.
end;

//Render Control
procedure BeginScene(Appl:TAndorraApplication);
begin
  //Begins the scene
end;

procedure EndScene(Appl:TAndorraApplication);
begin
  //Ends the scene
  //Disables all lights
end;

procedure Flip(Appl:TAndorraApplication);
begin
  //Flips Front- and Backbuffer.
end;

procedure ClearScene(Appl:TAndorraApplication;AColor:TAndorraColor);
begin
  //Fills the Scene with a specific color.
end;

procedure SetupScene(Appl:TAndorraApplication;AWidth,AHeight:integer);
var pos, dir, up : TD3DXVector3;
    matView, matProj: TD3DXMatrix;
begin
  //Creates the 2D Projection Matrix.
end;

procedure SetTextureQuality(Appl:TAndorraApplication;Quality:TAndorraTextureQuality);
begin
  //
end;

procedure SetOptions(Appl:TAndorraApplication;AOptions:TAdDrawModes);
begin
  //Takes the (new) options and applies them.
end;

procedure SetAmbientLight(Appl:TAndorraApplication;AColor:TAndorraColor);
begin
  //Sets the color of the ambient light.
end;

//Image Controls

function CreateImage(Appl:TAndorraApplication):TAndorraImage;
begin
  //Creates an Image
end;

procedure DestroyImage(Img:TAndorraImage);
begin
  //Destroys the Image
end;

procedure DrawImage(DestApp:TAndorraApplication;Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);
begin
  //Draws the Image
end;

procedure ImageLoadTexture(Img:TAndorraImage;ATexture:TAndorraTexture);
begin
  //Loads an texture for the Image
end;

procedure SetImageColor(Img:TAndorraImage;AColor:TAndorraColor);
begin
  //Sets the vertex color of the Image
end;

procedure SetTextureXMode(Img:TAndorraImage;AMode:TAndorraTextureMode);
begin
  //Sets how the Image's textures are drawn in X Direction. May be a little problem in OpenGl.
end;

procedure SetTextureYMode(Img:TAndorraImage;AMode:TAndorraTextureMode);
begin
  //Sets how the Image's textures are drawn in Y Direction. May be a little problem in OpenGl.
end;

function GetTextureInfo(Tex:TAndorraImage):TImageInfo;
begin
  //Returns informations about the texture (width, height etc.)
end;

procedure SetImageDetail(Img:TAndorraImage;ADetail:integer);
begin
  //Sets the vertex count.
end;

//Texture Creation

function CheckTextureMem(Appl:TAndorraApplication):integer;
begin
  //Returns the size of the aviable texture memory in byte.
end;

function LoadTextureFromFile(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;
begin
  //ATransparentColor is A=0 R=0 G=0 B=0 if transparenzy is turned off.
end;

function LoadTextureFromFileEx(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;
begin
  //ATransparentColor is A=0 R=0 G=0 B=0 if transparenzy is turned off.
end;

procedure FreeTexture(ATexture:TAndorraTexture);
begin
  //
end;


function LoadTextureFromBitmap(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;
begin
   //
end;

procedure AddTextureAlphaChannel(ATexture:TAndorraTexture;ABitmap:Pointer);
begin
  //
end;

procedure SetTextureAlpha(Tex:TAndorraTexture;AValue:Byte);
begin
  //The same as AddTextureAlphaChannel. The AlphaChannel has a regular value.
end;


function CreateLight(Appl:TAndorraApplication):TAndorraLight;
begin
  //Returns the pointer to a new light
end;

procedure DestroyLight(ALight:TAndorraLight);
begin
  //
end;

procedure RestoreLight(ALight:TAndorraLight;Data:TLight);
begin
  //Sets the light parameters. Data contains informations about the light.
end;

procedure EnableLight(ALight:TAndorraLight);
begin
  //Enables a light. Lights are automaticly disabled in "EndScene"
end;

procedure DisableLight(ALight:TAndorraLight);
begin
  //
end;   

end.
