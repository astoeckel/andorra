{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AndorraUtils.pas
* Comment: Unit used by the DLL and the application for data exchange 
}

unit AndorraUtils;

interface
uses SysUtils,Windows;

type TAndorraApplication = pointer;
type TAndorraImage = pointer;
type TAndorraTexture = pointer;

type TAndorraColor = packed record
  a,r,g,b:integer;
end;

type TImageInfo = packed record
  Width,Height:integer;
end;

type TAndorraTextureQuality = (tqNone,tqLinear,tqAnisotropic);
type TAndorraBlendMode = (bmAlpha,bmAdd);

const adnone = 0;

type TAdProcedure = procedure(Appl:TAndorraApplication);stdcall;
type TAdInitDisplay = function (Appl:TAndorraApplication; AWindow:hWnd; doHardware:boolean=true;
                     fullscreen:boolean=false; bitcount:byte=32;
                     resx:integer=0; resy:integer=0):boolean;stdcall;
type TAdGetLastError = function:Pchar;stdcall;
type TAdCreateApplication = function:TAndorraApplication;stdcall;
type TAdDestroyApplication = TAdProcedure;
type TAdSetTextureQuality = procedure (Appl:TAndorraApplication;Quality:TAndorraTextureQuality);stdcall;
type TAdClearScene = procedure (Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
type TAdImage = procedure(Img:TAndorraImage);stdcall;
type TAdImageDraw = procedure(Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);stdcall;
type TAdImageLoadTexture = procedure(Img:TAndorraImage;ATexture:TAndorraTexture);stdcall;
type TAdConstructor = function(Appl:TAndorraApplication):TAndorraImage;stdcall;
type TAdSetupScene = procedure(Appl:TAndorraApplication;AWidth,AHeight:integer);stdcall;
type TAdTextureFromBitmap = function(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;stdcall;
type TAdTextureFromFile = function(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
type TAdTextureFromFileEx = function(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
type TAdFreeTexture = procedure(ATexture:TAndorraTexture);stdcall;
type TAdAddAlpha = procedure (ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
type TAdSetImageColor = procedure(Img:TAndorraImage;AColor:TAndorraColor);stdcall;
type TAdGetImageInfo = function(Img:TAndorraImage):TImageInfo;stdcall;


function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
function Ad_RGB(r,g,b:byte):TAndorraColor;

implementation

function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
begin
  result.a := a;
  result.r := r;
  result.g := g;
  result.b := b;
end;

function Ad_RGB(r,g,b:byte):TAndorraColor;
begin
  result := Ad_ARGB(255,r,g,b);
end;

end.
