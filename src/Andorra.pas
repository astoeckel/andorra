{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: Andorra.pas
* Comment: Contains the DLL Loader 
}

unit Andorra;

interface

uses AndorraUtils,Windows,SysUtils;

type TAndorraDllLoader = class
  private
    DllHandle:THandle;
  public
    //Initialization
    InitDisplay:TAdInitDisplay;
    GetLastError:TAdGetLastError;
    CreateApplication:TAdCreateApplication;
    DestroyApplication:TAdDestroyApplication;

    //Render Control
    ClearScene:TAdClearScene;
    BeginScene:TAdProcedure;
    EndScene:TAdProcedure;
    Flip:TAdProcedure;
    SetupScene:TAdSetupScene;
    SetTextureQuality:TAdSetTextureQuality;

    //Image Control
    CreateImage:TAdConstructor;
    DestroyImage:TAdImage;
    DrawImage:TAdImageDraw;
    ImageLoadTexture:TAdImageLoadTexture;
    SetImageColor:TAdSetImageColor;
    GetImageInfo:TAdGetImageInfo;

    //Texture Creation
    LoadTextureFromBitmap:TAdTextureFromBitmap;
    LoadTextureFromFile:TAdTextureFromFile;
    LoadTextureFromFileEx:TAdTextureFromFileEx;
    FreeTexture:TAdFreeTexture;
    AddTextureAlphaChannel:TAdAddAlpha;

    procedure LoadLibrary(afile:string);
    procedure UnLoadLibrary;
    function LibraryLoaded:boolean;
    constructor Create;
    destructor Destroy;override;
end;

implementation

constructor TAndorraDllLoader.Create;
begin
  inherited Create;
end;

destructor TAndorraDllLoader.Destroy;
begin
  UnLoadLibrary;
  inherited Destroy;
end;

procedure TAndorraDllLoader.LoadLibrary(afile: string);
begin
  if fileExists(ExtractFilePath(ParamStr(0))+afile) then
  begin
    if LibraryLoaded then
    begin
      UnLoadLibrary;
    end;
    DllHandle := Windows.LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+afile));
    if LibraryLoaded then
    begin
      @InitDisplay := GetProcAddress(DllHandle, 'InitDisplay');
      @GetLastError := GetProcAddress(DllHandle, 'GetLastError');
      @CreateApplication := GetProcAddress(DllHandle, 'CreateApplication');
      @DestroyApplication := GetProcAddress(DllHandle, 'DestroyApplication');
      @BeginScene := GetProcAddress(DllHandle, 'BeginScene');
      @EndScene := GetProcAddress(DllHandle, 'EndScene');
      @ClearScene := GetProcAddress(DllHandle, 'ClearScene');
      @DrawImage := GetProcAddress(DllHandle, 'DrawImage');
      @DestroyImage := GetProcAddress(DllHandle, 'DestroyImage');
      @CreateImage := GetProcAddress(DllHandle, 'CreateImage');
      @SetupScene := GetProcAddress(DllHandle, 'SetupScene');
      @ImageLoadTexture := GetProcAddress(DllHandle, 'ImageLoadTexture');
      @LoadTextureFromFile := GetProcAddress(DllHandle, 'LoadTextureFromFile');
      @LoadTextureFromFileEx := GetProcAddress(DllHandle, 'LoadTextureFromFileEx');
      @LoadTextureFromBitmap := GetProcAddress(DllHandle, 'LoadTextureFromBitmap');
      @FreeTexture := GetProcAddress(DllHandle, 'FreeTexture');
      @AddTextureAlphaChannel := GetProcAddress(DllHandle, 'AddTextureAlphaChannel');
      @SetTextureQuality := GetProcAddress(DllHandle, 'SetTextureQuality');
      @SetImageColor := GetProcAddress(DllHandle, 'SetImageColor');
      @GetImageInfo := GetProcAddress(DllHandle, 'GetImageInfo');
      @Flip := GetProcAddress(DllHandle,'Flip');
    end;
  end;
end;

function TAndorraDllLoader.LibraryLoaded:boolean;
begin
  result := DllHandle <> 0;
end;

procedure TAndorraDllLoader.UnLoadLibrary;
begin
  if LibraryLoaded then
  begin
    FreeLibrary(DllHandle);
  end;  
end;

end.
