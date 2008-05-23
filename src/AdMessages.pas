{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdMessages.pas
* Comment: Contains all messages used by the Andorra 2D framework so that it may be translated by simpley exchanging this unit
}

{Contains all messages used by the Andorra 2D framework so that it may be
 translated by simpley exchanging this unit}
unit AdMessages;

interface

resourcestring
  { Messages for the shader module }
  MsgShaderNotLoaded = 'Shader object is not created. Check return result of ' +
    'TAdShader.CompileProgram!';
  MsgShaderLanguageNotSupported = 'The shader language "%s"is not supported. Use ' +
    'TAdShader.SupportsShaderLanguage to check if a specific language is ' +
    'available.';

  { Texts for the setup dialog}
  MsgSetupDefaultFormCaption = 'Andorra 2D Setup Dialog';
  MsgSetupBtnOK = 'OK';
  MsgSetupBtnCancel = 'Cancel';
  MsgSetupResolution = 'Resolution';
  MsgSetupPlugin = 'Plugin';
  MsgSetupCurrentDesktopResolution = 'Use current desktop resolution';
  MsgSetupFullscreen = 'Fullscreen';

  MsgSetupNoPluginsFound = 'No compatible Andorra 2D plugin library found.';

implementation

end.
