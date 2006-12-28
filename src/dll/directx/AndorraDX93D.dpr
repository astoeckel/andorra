{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AndorraDX93D.dpr
* Comment: The DLL Project File
}

library AndorraDX93D;

uses
  SysUtils,
  Classes,
  DX3DMain in 'DX3DMain.pas',
  AndorraUtils in '..\..\AndorraUtils.pas';

{$R *.res}

exports
  CreateApplication,
  DestroyApplication,
  InitDisplay,
  BeginScene,
  EndScene,
  Flip,
  ClearScene,
  CreateImage,
  DestroyImage,
  DrawImage,
  SetupScene,
  ImageLoadTexture,
  LoadTextureFromFile,
  LoadTextureFromFileEx,
  LoadTextureFromBitmap,
  FreeTexture,
  AddTextureAlphaChannel,
  SetTextureQuality,
  SetImageColor,
  GetTextureInfo,
  SetTextureAlpha,
  SetTextureXMode,
  SetTextureYMode,
  GetTextureAsBitmap,
  GetTextureAlphaChannelAsBitmap,
  SetOptions,
  SetAmbientLight,
  CreateLight,
  DestroyLight,
  RestoreLight,
  SetImageDetail,
  DisableLight,
  EnableLight,
  SetLogProc,
  RefreshTextureWithBitmap;

begin
end.
