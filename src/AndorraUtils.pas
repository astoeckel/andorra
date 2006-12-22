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

{ This unit contains types used by the Dll and the Main Application }
unit AndorraUtils;

interface
uses SysUtils,Windows;

  {Specifies the dimensions of the display. }
type TAdDrawDisplay = record
    //The Width of the Display
    Width:integer;
    //The Height of the Display
    Height:integer;
    //The Bitcount of the Display (May be 16 or 32 (and normaly 24, but this is, whyever, very buggy...) )
    BitCount:byte;
    //The horizontal refresh rate
    Freq:integer;
  end;


type TAndorraApplication = pointer;
type TAndorraImage = pointer;
type TAndorraTexture = pointer;
type TAndorraLight = pointer;

{Specifies the options the application is created with.

If you change these settings while running, simply call the "restore" function of TAdDraw.}
TAdDrawMode = (
  doFullscreen,  //< Specifies weather the application should run in the fullscreen mode or not
  doVSync, //< If turned on, the frame rate is equal to the vertical frequenzy of the screen
  doStretch, //< Should the picture be stretched when the window resizes?
  doHardware,//< Run in hardware mode? (WARNING: Should be set!)
  doZBuffer, //< The ZBuffer has to be used if you are using 3D Objects in your scene
  doAntialias,//< should Antialiasing be used
  doSystemMemory,//< use system memory instead of video memory for textures?
  doLights//Turn lights off/on.
);
{Declares a set of TAdDrawMode. See above to learn what all these settings mean.}
TAdDrawModes = set of TAdDrawMode;


type TAndorraTextureMode = (amWrap,amMirror,amClamp);

type TAndorraColor = packed record
  a,r,g,b:byte;
end;

type TImageInfo = packed record
  Width,Height:integer;
  BaseRect:TRect;
end;

type TLight = packed record
  X1,Y1:integer;
  Color:TAndorraColor;
  Range:single;
  Falloff:single;
end;

type TAndorraTextureQuality = (tqNone,tqLinear,tqAnisotropic);
type TAndorraBlendMode = (bmAlpha,bmAdd,bmMask);

type TAdLogTyp = (ltInfo,ltWarning,ltError,ltFatalError,ltNone);

type TAdLogItem = record
  Text:PChar;
  Typ:TAdLogTyp;  
end;

const adnone = 0;

type TAdProcedure = procedure(Appl:TAndorraApplication);stdcall;
type TAdInitDisplay = function (Appl:TAndorraApplication; AWindow:hWnd; AOptions:TAdDrawModes;
                     ADisplay:TAdDrawDisplay):boolean;stdcall;
type TAdCreateApplication = function:TAndorraApplication;stdcall;
type TAdDestroyApplication = TAdProcedure;
type TAdSetTextureQuality = procedure (Appl:TAndorraApplication;Quality:TAndorraTextureQuality);stdcall;
type TAdClearScene = procedure (Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
type TAdImage = procedure(Img:TAndorraImage);stdcall;
type TAdImageDraw = procedure(DestApp:TAndorraApplication;Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);stdcall;
type TAdImageLoadTexture = procedure(Img:TAndorraImage;ATexture:TAndorraTexture);stdcall;
type TAdSetImageDetail = procedure(Img:TAndorraImage;ADetail:integer);stdcall;
type TAdConstructor = function(Appl:TAndorraApplication):TAndorraImage;stdcall;
type TAdSetupScene = procedure(Appl:TAndorraApplication;AWidth,AHeight:integer);stdcall;
type TAdTextureFromBitmap = function(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;stdcall;
type TAdTextureFromFile = function(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
type TAdTextureFromFileEx = function(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
type TAdFreeTexture = procedure(ATexture:TAndorraTexture);stdcall;
type TAdAddAlpha = procedure (ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
type TAdSetImageColor = procedure(Img:TAndorraImage;AColor:TAndorraColor);stdcall;
type TAdGetTextureInfo = function(Tex:TAndorraTexture):TImageInfo;stdcall;
type TAdSetTextureAlpha = procedure (Tex:TAndorraTexture;AValue:Byte);stdcall;
type TAdSetTextureMode = procedure(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
type TAdSetOptions = procedure(Appl:TAndorraApplication;AOptions:TAdDrawModes);stdcall;
type TAdSetAmbientLight = procedure(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
type TAdLightProc = procedure(ALight:TAndorraLight);stdcall;
type TAdCreateLight = function(AAppl:TAndorraApplication):TAndorraLight;stdcall;
type TAdRestoreLight = procedure(ALight:TAndorraLight;Data:TLight);stdcall;
type TAdLogProc = procedure(LogItem:TAdLogItem;AAppl:Pointer);stdcall;
type TAdSetLogProc = procedure(Appl:TAndorraApplication;ALogProc:TAdLogProc;AAppl:Pointer);
type TAdGetTexture = procedure(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;

function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
function Ad_RGB(r,g,b:byte):TAndorraColor;
function AdColorToString(AColor:TAndorraColor):string;
function StringToAdColor(AString:string):TAndorraColor;
function AdColorToColor(AAdColor:TAndorraColor):LongWord;

function GetRValue(AColor:LongWord):byte;
function GetGValue(AColor:LongWord):byte;
function GetBValue(AColor:LongWord):byte;

function RGB(r,g,b:byte):LongWord;

function CompareColors(col1,col2:TAndorraColor):boolean;

function Cut(AValue:integer):byte;

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

function CompareColors(col1,col2:TAndorraColor):boolean;
begin
  result := (col1.a = col2.a) and
            (col1.r = col2.r) and
            (col1.g = col2.g) and
            (col1.b = col2.b);
end;

function GetRValue(AColor:LongWord):byte;
begin
  result := AColor and 255;
end;

function GetGValue(AColor:LongWord):byte;
begin
  result := (AColor shr 8) and 255;
end;

function GetBValue(AColor:LongWord):byte;
begin
  result := (AColor shr 16) and 255;
end;

function AdColorToString(AColor:TAndorraColor):string;
begin
  result := FormatFloat('000',AColor.a)+FormatFloat('000',AColor.r)+
            FormatFloat('000',AColor.g)+FormatFloat('000',AColor.b);
end;

function StringToAdColor(AString:string):TAndorraColor;
begin
  result.a  := StrToInt(Copy(AString,1,3));
  result.r  := StrToInt(Copy(AString,4,3));
  result.g  := StrToInt(Copy(AString,7,3));
  result.b  := StrToInt(Copy(AString,10,3));
end;

function RGB(r,g,b:byte):LongWord;
begin
  result := R + G shl 8 + B shl 16; 
end;

function Cut(AValue:integer):byte;
begin
  if AValue < 255 then
  begin
    if AValue < 0 then
    begin
      result := 0;
    end
    else
    begin
      result := AValue;
    end;
  end
  else
  begin
    result := 255;
  end;
end;

function AdColorToColor(AAdColor:TAndorraColor):LongWord;
begin
  result := RGB(AAdColor.r,AAdColor.g,AAdColor.b);
end;


end.
