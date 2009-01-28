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
 translated by simply exchanging this unit}
unit AdMessages; //< @exclude

interface

resourcestring
  { Messages used in the log system }

  MsgLogInfo = 'Info';
  MsgLogWarning = 'Warning';
  MsgLogError = 'Error';
  MsgLogFatalError = 'Fatal Error';

  { Messages used in the image module }

  MsgNoValidImage = 'Error while loading the image. Compatible Andorra TAdImage ' +
    'header not found. Not a valid Andorra Image.';

  MsgNoValidFileFormat = 'No loader found to load the specified file "%s".';

  MsgNoBitmapCompressor = 'No compressor not found to load the compressed bitmap data.';

  MsgNoBitmapClassHandler = 'No handler for the specified bitmap class "%s" found.';

  { Messages for the surface system }

  MsgSurfaceInterfaceNotAvailable = 'The Andorra surface interface is not ' +
    'available. Try to use another Andorra graphic plugin. Try to install the ' +
    'newest video driver for your graphic board.';

  { Messages used in TAdDraw }
  MsgLibraryNotLoaded = 'No graphic library has been loaded. This problem ' +
    'might have been caused, because the specified library has not been found ' +
    'or the library version is not compatible to the host Andorra 2D version. ' +
    'Use TAdSetupDlg or TAdDllExplorer to find all available graphic system ' +
    'plugins.';

  MsgAdDrawInterfaceNotSupported = 'The TAdApplication inteface is not supported ' +
    'by the graphic plugin. This error should never occur, because TAdApplication ' +
    'is the main Andorra interface and has to be supported by every Andorra graphic ' +
    'plugin.';

  MsgNoWindowframework = 'No window framework that is supported by both, the ' +
    'parent control and the graphic plugin had been found. Try to use another ' +
    'graphic plugin or include other window frameworks.';

  MsgInitializationFailed = 'Andorra 2D could not be initialized. This problem ' +
    'may have multiple reasons: Probably the graphic system used by the graphic ' +
    'plugin is not available on your computer. You may not have a 3D-Graphic ' +
    'card. Try to install the newest graphic drivers. View the log entries created ' +
    'by the graphic plugin to get more information.';

  { Messages used in the DLL loaded}
  MsgPluginInvalid = 'The specified graphic plugin is not a valid Andorra 2D ' +
    'plugin!';

  MsgPluginVersionIncompatible = 'The version of the library ("%s", "%s") is ' +
    'incompatible to the current version %s.';

  { Messages for the shader module }

  MsgShaderNotLoaded = 'Shader object is not created. Check return result of ' +
    'TAdShader.CompileProgram!';
  MsgShaderLanguageNotSupported = 'The shader language "%s"is not supported. Use ' +
    'TAdShader.SupportsShaderLanguage to check if a specific language is ' +
    'available.';

  { Texts for the setup dialog }

  MsgSetupDefaultFormCaption = 'Andorra 2D Setup Dialog';
  MsgSetupBtnOK = 'OK';
  MsgSetupBtnCancel = 'Cancel';
  MsgSetupResolution = 'Resolution';
  MsgSetupPlugin = 'Plugin';
  MsgSetupCurrentDesktopResolution = 'Use current desktop resolution';
  MsgSetupFullscreen = 'Fullscreen';
  MsgSetupNoPluginsFound = 'No compatible Andorra 2D plugin library found.';

  { Error messages used in the BMP loader}
  MsgInvalidHeaderSize = 'The header size is invalid.';
  MsgNoBMP = 'This is not a valid BMP file.';
  MsgInvalidBitDepth = 'Bit depth not supported.';
  MsgInvalidCompressor = 'Compressor mode not supported.';

  { Error messages for the bitmap filters }
  MsgCallbackIsNil = 'The dithering callback function is nil.';

  { Sprite engine messages }
  MsgSpriteListIsNil = 'The given sprite list is nil';

  { 3DS Loader}
  Msg3DSInvalidChunkSize = 'The read chunk has a invalid length. The file may be ' +
    'corrupted.';
  Msg3DSInvalidChunkOrder = 'The chunks of this 3DS file are misordered. The file may be ' +
    'corrupted.';

implementation

end.
