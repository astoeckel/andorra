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
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdConsts.pas
* Comment: Contains color and material declarations. The AdCol24 values are
  compatible with the standard Delphi/Lazarus colors.
}

{ Contains color and material declarations. The AdCol24 values are
  compatible with the standard Delphi/Lazarus colors. }
unit AdConsts;

interface

uses
  AdClasses, AdTypes;

//Color declarations
//Colors taken from the W3C color declarations for CSS3.
//See http://en.wikipedia.org/wiki/Web_colors
//The AdCol24 colors are compatible to the standard Delphi/Lazarus colors
const
  //Web color "AliceBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_AliceBlue : TAndorraColor = (r: 240; g: 248; b: 255; a: 255);
  //Web color "AliceBlue " as a 24 Bit color value. Opaque.
  AdCol24_AliceBlue : longint = 16775408;

  //Web color "AntiqueWhite " as a 32 Bit andorra color value. Opaque.
  AdCol32_AntiqueWhite : TAndorraColor = (r: 250; g: 235; b: 215; a: 255);
  //Web color "AntiqueWhite " as a 24 Bit color value. Opaque.
  AdCol24_AntiqueWhite : longint = 14150650;

  //Web color "Aqua " as a 32 Bit andorra color value. Opaque.
  AdCol32_Aqua : TAndorraColor = (r: 0; g: 255; b: 255; a: 255);
  //Web color "Aqua " as a 24 Bit color value. Opaque.
  AdCol24_Aqua : longint = 16776960;

  //Web color "Aquamarine " as a 32 Bit andorra color value. Opaque.
  AdCol32_Aquamarine : TAndorraColor = (r: 127; g: 255; b: 212; a: 255);
  //Web color "Aquamarine " as a 24 Bit color value. Opaque.
  AdCol24_Aquamarine : longint = 13959039;

  //Web color "Azure " as a 32 Bit andorra color value. Opaque.
  AdCol32_Azure : TAndorraColor = (r: 240; g: 255; b: 255; a: 255);
  //Web color "Azure " as a 24 Bit color value. Opaque.
  AdCol24_Azure : longint = 16777200;

  //Web color "Beige " as a 32 Bit andorra color value. Opaque.
  AdCol32_Beige : TAndorraColor = (r: 245; g: 245; b: 220; a: 255);
  //Web color "Beige " as a 24 Bit color value. Opaque.
  AdCol24_Beige : longint = 14480885;

  //Web color "Bisque " as a 32 Bit andorra color value. Opaque.
  AdCol32_Bisque : TAndorraColor = (r: 255; g: 228; b: 196; a: 255);
  //Web color "Bisque " as a 24 Bit color value. Opaque.
  AdCol24_Bisque : longint = 12903679;

  //Web color "Black " as a 32 Bit andorra color value. Opaque.
  AdCol32_Black : TAndorraColor = (r: 0; g: 0; b: 0; a: 255);
  //Web color "Black " as a 24 Bit color value. Opaque.
  AdCol24_Black : longint = 0;

  //Web color "BlanchedAlmond " as a 32 Bit andorra color value. Opaque.
  AdCol32_BlanchedAlmond : TAndorraColor = (r: 255; g: 235; b: 205; a: 255);
  //Web color "BlanchedAlmond " as a 24 Bit color value. Opaque.
  AdCol24_BlanchedAlmond : longint = 13495295;

  //Web color "Blue " as a 32 Bit andorra color value. Opaque.
  AdCol32_Blue : TAndorraColor = (r: 0; g: 0; b: 255; a: 255);
  //Web color "Blue " as a 24 Bit color value. Opaque.
  AdCol24_Blue : longint = 16711680;

  //Web color "BlueViolet " as a 32 Bit andorra color value. Opaque.
  AdCol32_BlueViolet : TAndorraColor = (r: 138; g: 43; b: 226; a: 255);
  //Web color "BlueViolet " as a 24 Bit color value. Opaque.
  AdCol24_BlueViolet : longint = 14822282;

  //Web color "Brown " as a 32 Bit andorra color value. Opaque.
  AdCol32_Brown : TAndorraColor = (r: 165; g: 42; b: 42; a: 255);
  //Web color "Brown " as a 24 Bit color value. Opaque.
  AdCol24_Brown : longint = 2763429;

  //Web color "BurlyWood " as a 32 Bit andorra color value. Opaque.
  AdCol32_BurlyWood : TAndorraColor = (r: 222; g: 184; b: 135; a: 255);
  //Web color "BurlyWood " as a 24 Bit color value. Opaque.
  AdCol24_BurlyWood : longint = 8894686;

  //Web color "CadetBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_CadetBlue : TAndorraColor = (r: 95; g: 158; b: 160; a: 255);
  //Web color "CadetBlue " as a 24 Bit color value. Opaque.
  AdCol24_CadetBlue : longint = 10526303;

  //Web color "Chartreuse " as a 32 Bit andorra color value. Opaque.
  AdCol32_Chartreuse : TAndorraColor = (r: 127; g: 255; b: 0; a: 255);
  //Web color "Chartreuse " as a 24 Bit color value. Opaque.
  AdCol24_Chartreuse : longint = 65407;

  //Web color "Chocolate " as a 32 Bit andorra color value. Opaque.
  AdCol32_Chocolate : TAndorraColor = (r: 210; g: 105; b: 30; a: 255);
  //Web color "Chocolate " as a 24 Bit color value. Opaque.
  AdCol24_Chocolate : longint = 1993170;

  //Web color "Coral " as a 32 Bit andorra color value. Opaque.
  AdCol32_Coral : TAndorraColor = (r: 255; g: 127; b: 80; a: 255);
  //Web color "Coral " as a 24 Bit color value. Opaque.
  AdCol24_Coral : longint = 5275647;

  //Web color "CornflowerBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_CornflowerBlue : TAndorraColor = (r: 100; g: 149; b: 237; a: 255);
  //Web color "CornflowerBlue " as a 24 Bit color value. Opaque.
  AdCol24_CornflowerBlue : longint = 15570276;

  //Web color "Cornsilk " as a 32 Bit andorra color value. Opaque.
  AdCol32_Cornsilk : TAndorraColor = (r: 255; g: 248; b: 220; a: 255);
  //Web color "Cornsilk " as a 24 Bit color value. Opaque.
  AdCol24_Cornsilk : longint = 14481663;

  //Web color "Crimson " as a 32 Bit andorra color value. Opaque.
  AdCol32_Crimson : TAndorraColor = (r: 220; g: 20; b: 60; a: 255);
  //Web color "Crimson " as a 24 Bit color value. Opaque.
  AdCol24_Crimson : longint = 3937500;

  //Web color "Cyan " as a 32 Bit andorra color value. Opaque.
  AdCol32_Cyan : TAndorraColor = (r: 0; g: 255; b: 255; a: 255);
  //Web color "Cyan " as a 24 Bit color value. Opaque.
  AdCol24_Cyan : longint = 16776960;

  //Web color "DarkBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkBlue : TAndorraColor = (r: 0; g: 0; b: 139; a: 255);
  //Web color "DarkBlue " as a 24 Bit color value. Opaque.
  AdCol24_DarkBlue : longint = 9109504;

  //Web color "DarkCyan " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkCyan : TAndorraColor = (r: 0; g: 139; b: 139; a: 255);
  //Web color "DarkCyan " as a 24 Bit color value. Opaque.
  AdCol24_DarkCyan : longint = 9145088;

  //Web color "DarkGoldenrod " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGoldenrod : TAndorraColor = (r: 184; g: 134; b: 11; a: 255);
  //Web color "DarkGoldenrod " as a 24 Bit color value. Opaque.
  AdCol24_DarkGoldenrod : longint = 755384;

  //Web color "DarkGray " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGray : TAndorraColor = (r: 169; g: 169; b: 169; a: 255);
  //Web color "DarkGray " as a 24 Bit color value. Opaque.
  AdCol24_DarkGray : longint = 11119017;

  //Web color "DarkGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGreen : TAndorraColor = (r: 0; g: 100; b: 0; a: 255);
  //Web color "DarkGreen " as a 24 Bit color value. Opaque.
  AdCol24_DarkGreen : longint = 25600;

  //Web color "DarkKhaki " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkKhaki : TAndorraColor = (r: 189; g: 183; b: 107; a: 255);
  //Web color "DarkKhaki " as a 24 Bit color value. Opaque.
  AdCol24_DarkKhaki : longint = 7059389;

  //Web color "DarkMagenta " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkMagenta : TAndorraColor = (r: 139; g: 0; b: 139; a: 255);
  //Web color "DarkMagenta " as a 24 Bit color value. Opaque.
  AdCol24_DarkMagenta : longint = 9109643;

  //Web color "DarkOliveGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOliveGreen : TAndorraColor = (r: 85; g: 107; b: 47; a: 255);
  //Web color "DarkOliveGreen " as a 24 Bit color value. Opaque.
  AdCol24_DarkOliveGreen : longint = 3107669;

  //Web color "DarkOrange " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOrange : TAndorraColor = (r: 255; g: 140; b: 0; a: 255);
  //Web color "DarkOrange " as a 24 Bit color value. Opaque.
  AdCol24_DarkOrange : longint = 36095;

  //Web color "DarkOrchid " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOrchid : TAndorraColor = (r: 153; g: 50; b: 204; a: 255);
  //Web color "DarkOrchid " as a 24 Bit color value. Opaque.
  AdCol24_DarkOrchid : longint = 13382297;

  //Web color "DarkRed " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkRed : TAndorraColor = (r: 139; g: 0; b: 0; a: 255);
  //Web color "DarkRed " as a 24 Bit color value. Opaque.
  AdCol24_DarkRed : longint = 139;

  //Web color "DarkSalmon " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSalmon : TAndorraColor = (r: 233; g: 150; b: 122; a: 255);
  //Web color "DarkSalmon " as a 24 Bit color value. Opaque.
  AdCol24_DarkSalmon : longint = 8034025;

  //Web color "DarkSeaGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSeaGreen : TAndorraColor = (r: 143; g: 188; b: 143; a: 255);
  //Web color "DarkSeaGreen " as a 24 Bit color value. Opaque.
  AdCol24_DarkSeaGreen : longint = 9419919;

  //Web color "DarkSlateBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSlateBlue : TAndorraColor = (r: 72; g: 61; b: 139; a: 255);
  //Web color "DarkSlateBlue " as a 24 Bit color value. Opaque.
  AdCol24_DarkSlateBlue : longint = 9125192;

  //Web color "DarkSlateGray " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSlateGray : TAndorraColor = (r: 47; g: 79; b: 79; a: 255);
  //Web color "DarkSlateGray " as a 24 Bit color value. Opaque.
  AdCol24_DarkSlateGray : longint = 5197615;

  //Web color "DarkTurquoise " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkTurquoise : TAndorraColor = (r: 0; g: 206; b: 209; a: 255);
  //Web color "DarkTurquoise " as a 24 Bit color value. Opaque.
  AdCol24_DarkTurquoise : longint = 13749760;

  //Web color "DarkViolet " as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkViolet : TAndorraColor = (r: 148; g: 0; b: 211; a: 255);
  //Web color "DarkViolet " as a 24 Bit color value. Opaque.
  AdCol24_DarkViolet : longint = 13828244;

  //Web color "DeepPink " as a 32 Bit andorra color value. Opaque.
  AdCol32_DeepPink : TAndorraColor = (r: 255; g: 20; b: 147; a: 255);
  //Web color "DeepPink " as a 24 Bit color value. Opaque.
  AdCol24_DeepPink : longint = 9639167;

  //Web color "DeepSkyBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_DeepSkyBlue : TAndorraColor = (r: 0; g: 191; b: 255; a: 255);
  //Web color "DeepSkyBlue " as a 24 Bit color value. Opaque.
  AdCol24_DeepSkyBlue : longint = 16760576;

  //Web color "DimGray " as a 32 Bit andorra color value. Opaque.
  AdCol32_DimGray : TAndorraColor = (r: 105; g: 105; b: 105; a: 255);
  //Web color "DimGray " as a 24 Bit color value. Opaque.
  AdCol24_DimGray : longint = 6908265;

  //Web color "DodgerBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_DodgerBlue : TAndorraColor = (r: 30; g: 144; b: 255; a: 255);
  //Web color "DodgerBlue " as a 24 Bit color value. Opaque.
  AdCol24_DodgerBlue : longint = 16748574;

  //Web color "FireBrick " as a 32 Bit andorra color value. Opaque.
  AdCol32_FireBrick : TAndorraColor = (r: 178; g: 34; b: 34; a: 255);
  //Web color "FireBrick " as a 24 Bit color value. Opaque.
  AdCol24_FireBrick : longint = 2237106;

  //Web color "FloralWhite " as a 32 Bit andorra color value. Opaque.
  AdCol32_FloralWhite : TAndorraColor = (r: 255; g: 250; b: 240; a: 255);
  //Web color "FloralWhite " as a 24 Bit color value. Opaque.
  AdCol24_FloralWhite : longint = 15792895;

  //Web color "ForestGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_ForestGreen : TAndorraColor = (r: 34; g: 139; b: 34; a: 255);
  //Web color "ForestGreen " as a 24 Bit color value. Opaque.
  AdCol24_ForestGreen : longint = 2263842;

  //Web color "Fuchsia " as a 32 Bit andorra color value. Opaque.
  AdCol32_Fuchsia : TAndorraColor = (r: 255; g: 0; b: 255; a: 255);
  //Web color "Fuchsia " as a 24 Bit color value. Opaque.
  AdCol24_Fuchsia : longint = 16711935;

  //Web color "Gainsboro " as a 32 Bit andorra color value. Opaque.
  AdCol32_Gainsboro : TAndorraColor = (r: 220; g: 220; b: 220; a: 255);
  //Web color "Gainsboro " as a 24 Bit color value. Opaque.
  AdCol24_Gainsboro : longint = 14474460;

  //Web color "GhostWhite " as a 32 Bit andorra color value. Opaque.
  AdCol32_GhostWhite : TAndorraColor = (r: 248; g: 248; b: 255; a: 255);
  //Web color "GhostWhite " as a 24 Bit color value. Opaque.
  AdCol24_GhostWhite : longint = 16775416;

  //Web color "Gold " as a 32 Bit andorra color value. Opaque.
  AdCol32_Gold : TAndorraColor = (r: 255; g: 215; b: 0; a: 255);
  //Web color "Gold " as a 24 Bit color value. Opaque.
  AdCol24_Gold : longint = 55295;

  //Web color "Goldenrod " as a 32 Bit andorra color value. Opaque.
  AdCol32_Goldenrod : TAndorraColor = (r: 218; g: 165; b: 32; a: 255);
  //Web color "Goldenrod " as a 24 Bit color value. Opaque.
  AdCol24_Goldenrod : longint = 2139610;

  //Web color "Gray " as a 32 Bit andorra color value. Opaque.
  AdCol32_Gray : TAndorraColor = (r: 128; g: 128; b: 128; a: 255);
  //Web color "Gray " as a 24 Bit color value. Opaque.
  AdCol24_Gray : longint = 8421504;

  //Web color "Green " as a 32 Bit andorra color value. Opaque.
  AdCol32_Green : TAndorraColor = (r: 0; g: 128; b: 0; a: 255);
  //Web color "Green " as a 24 Bit color value. Opaque.
  AdCol24_Green : longint = 32768;

  //Web color "GreenYellow " as a 32 Bit andorra color value. Opaque.
  AdCol32_GreenYellow : TAndorraColor = (r: 173; g: 255; b: 47; a: 255);
  //Web color "GreenYellow " as a 24 Bit color value. Opaque.
  AdCol24_GreenYellow : longint = 3145645;

  //Web color "Honeydew " as a 32 Bit andorra color value. Opaque.
  AdCol32_Honeydew : TAndorraColor = (r: 240; g: 255; b: 240; a: 255);
  //Web color "Honeydew " as a 24 Bit color value. Opaque.
  AdCol24_Honeydew : longint = 15794160;

  //Web color "HotPink " as a 32 Bit andorra color value. Opaque.
  AdCol32_HotPink : TAndorraColor = (r: 255; g: 105; b: 180; a: 255);
  //Web color "HotPink " as a 24 Bit color value. Opaque.
  AdCol24_HotPink : longint = 11823615;

  //Web color "IndianRed " as a 32 Bit andorra color value. Opaque.
  AdCol32_IndianRed : TAndorraColor = (r: 205; g: 92; b: 92; a: 255);
  //Web color "IndianRed " as a 24 Bit color value. Opaque.
  AdCol24_IndianRed : longint = 6053069;

  //Web color "Indigo " as a 32 Bit andorra color value. Opaque.
  AdCol32_Indigo : TAndorraColor = (r: 75; g: 0; b: 130; a: 255);
  //Web color "Indigo " as a 24 Bit color value. Opaque.
  AdCol24_Indigo : longint = 8519755;

  //Web color "Ivory " as a 32 Bit andorra color value. Opaque.
  AdCol32_Ivory : TAndorraColor = (r: 255; g: 255; b: 240; a: 255);
  //Web color "Ivory " as a 24 Bit color value. Opaque.
  AdCol24_Ivory : longint = 15794175;

  //Web color "Khaki " as a 32 Bit andorra color value. Opaque.
  AdCol32_Khaki : TAndorraColor = (r: 240; g: 230; b: 140; a: 255);
  //Web color "Khaki " as a 24 Bit color value. Opaque.
  AdCol24_Khaki : longint = 9234160;

  //Web color "Lavender " as a 32 Bit andorra color value. Opaque.
  AdCol32_Lavender : TAndorraColor = (r: 230; g: 230; b: 250; a: 255);
  //Web color "Lavender " as a 24 Bit color value. Opaque.
  AdCol24_Lavender : longint = 16443110;

  //Web color "LavenderBlush " as a 32 Bit andorra color value. Opaque.
  AdCol32_LavenderBlush : TAndorraColor = (r: 255; g: 240; b: 245; a: 255);
  //Web color "LavenderBlush " as a 24 Bit color value. Opaque.
  AdCol24_LavenderBlush : longint = 16118015;

  //Web color "LawnGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_LawnGreen : TAndorraColor = (r: 124; g: 252; b: 0; a: 255);
  //Web color "LawnGreen " as a 24 Bit color value. Opaque.
  AdCol24_LawnGreen : longint = 64636;

  //Web color "LemonChiffon " as a 32 Bit andorra color value. Opaque.
  AdCol32_LemonChiffon : TAndorraColor = (r: 255; g: 250; b: 205; a: 255);
  //Web color "LemonChiffon " as a 24 Bit color value. Opaque.
  AdCol24_LemonChiffon : longint = 13499135;

  //Web color "LightBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightBlue : TAndorraColor = (r: 173; g: 216; b: 230; a: 255);
  //Web color "LightBlue " as a 24 Bit color value. Opaque.
  AdCol24_LightBlue : longint = 15128749;

  //Web color "LightCoral " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightCoral : TAndorraColor = (r: 240; g: 128; b: 128; a: 255);
  //Web color "LightCoral " as a 24 Bit color value. Opaque.
  AdCol24_LightCoral : longint = 8421616;

  //Web color "LightCyan " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightCyan : TAndorraColor = (r: 224; g: 255; b: 255; a: 255);
  //Web color "LightCyan " as a 24 Bit color value. Opaque.
  AdCol24_LightCyan : longint = 16777184;

  //Web color "LightGoldenrodYellow " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGoldenrodYellow : TAndorraColor = (r: 250; g: 250; b: 210; a: 255);
  //Web color "LightGoldenrodYellow " as a 24 Bit color value. Opaque.
  AdCol24_LightGoldenrodYellow : longint = 13826810;

  //Web color "LightGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGreen : TAndorraColor = (r: 144; g: 238; b: 144; a: 255);
  //Web color "LightGreen " as a 24 Bit color value. Opaque.
  AdCol24_LightGreen : longint = 9498256;

  //Web color "LightGrey " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGrey : TAndorraColor = (r: 211; g: 211; b: 211; a: 255);
  //Web color "LightGrey " as a 24 Bit color value. Opaque.
  AdCol24_LightGrey : longint = 13882323;

  //Web color "LightPink " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightPink : TAndorraColor = (r: 255; g: 182; b: 193; a: 255);
  //Web color "LightPink " as a 24 Bit color value. Opaque.
  AdCol24_LightPink : longint = 12695295;

  //Web color "LightSalmon " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSalmon : TAndorraColor = (r: 255; g: 160; b: 122; a: 255);
  //Web color "LightSalmon " as a 24 Bit color value. Opaque.
  AdCol24_LightSalmon : longint = 8036607;

  //Web color "LightSeaGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSeaGreen : TAndorraColor = (r: 32; g: 178; b: 170; a: 255);
  //Web color "LightSeaGreen " as a 24 Bit color value. Opaque.
  AdCol24_LightSeaGreen : longint = 11186720;

  //Web color "LightSkyBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSkyBlue : TAndorraColor = (r: 135; g: 206; b: 250; a: 255);
  //Web color "LightSkyBlue " as a 24 Bit color value. Opaque.
  AdCol24_LightSkyBlue : longint = 16436871;

  //Web color "LightSlateGray " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSlateGray : TAndorraColor = (r: 119; g: 136; b: 153; a: 255);
  //Web color "LightSlateGray " as a 24 Bit color value. Opaque.
  AdCol24_LightSlateGray : longint = 10061943;

  //Web color "LightSteelBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSteelBlue : TAndorraColor = (r: 176; g: 196; b: 222; a: 255);
  //Web color "LightSteelBlue " as a 24 Bit color value. Opaque.
  AdCol24_LightSteelBlue : longint = 14599344;

  //Web color "LightYellow " as a 32 Bit andorra color value. Opaque.
  AdCol32_LightYellow : TAndorraColor = (r: 255; g: 255; b: 224; a: 255);
  //Web color "LightYellow " as a 24 Bit color value. Opaque.
  AdCol24_LightYellow : longint = 14745599;

  //Web color "Lime " as a 32 Bit andorra color value. Opaque.
  AdCol32_Lime : TAndorraColor = (r: 0; g: 255; b: 0; a: 255);
  //Web color "Lime " as a 24 Bit color value. Opaque.
  AdCol24_Lime : longint = 65280;

  //Web color "LimeGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_LimeGreen : TAndorraColor = (r: 50; g: 205; b: 50; a: 255);
  //Web color "LimeGreen " as a 24 Bit color value. Opaque.
  AdCol24_LimeGreen : longint = 3329330;

  //Web color "Linen " as a 32 Bit andorra color value. Opaque.
  AdCol32_Linen : TAndorraColor = (r: 250; g: 240; b: 230; a: 255);
  //Web color "Linen " as a 24 Bit color value. Opaque.
  AdCol24_Linen : longint = 15134970;

  //Web color "Magenta " as a 32 Bit andorra color value. Opaque.
  AdCol32_Magenta : TAndorraColor = (r: 255; g: 0; b: 255; a: 255);
  //Web color "Magenta " as a 24 Bit color value. Opaque.
  AdCol24_Magenta : longint = 16711935;

  //Web color "Maroon " as a 32 Bit andorra color value. Opaque.
  AdCol32_Maroon : TAndorraColor = (r: 128; g: 0; b: 0; a: 255);
  //Web color "Maroon " as a 24 Bit color value. Opaque.
  AdCol24_Maroon : longint = 128;

  //Web color "MediumAquamarine " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumAquamarine : TAndorraColor = (r: 102; g: 205; b: 170; a: 255);
  //Web color "MediumAquamarine " as a 24 Bit color value. Opaque.
  AdCol24_MediumAquamarine : longint = 11193702;

  //Web color "MediumBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumBlue : TAndorraColor = (r: 0; g: 0; b: 205; a: 255);
  //Web color "MediumBlue " as a 24 Bit color value. Opaque.
  AdCol24_MediumBlue : longint = 13434880;

  //Web color "MediumOrchid " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumOrchid : TAndorraColor = (r: 186; g: 85; b: 211; a: 255);
  //Web color "MediumOrchid " as a 24 Bit color value. Opaque.
  AdCol24_MediumOrchid : longint = 13850042;

  //Web color "MediumPurple " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumPurple : TAndorraColor = (r: 147; g: 112; b: 219; a: 255);
  //Web color "MediumPurple " as a 24 Bit color value. Opaque.
  AdCol24_MediumPurple : longint = 14381203;

  //Web color "MediumSeaGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSeaGreen : TAndorraColor = (r: 60; g: 179; b: 113; a: 255);
  //Web color "MediumSeaGreen " as a 24 Bit color value. Opaque.
  AdCol24_MediumSeaGreen : longint = 7451452;

  //Web color "MediumSlateBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSlateBlue : TAndorraColor = (r: 123; g: 104; b: 238; a: 255);
  //Web color "MediumSlateBlue " as a 24 Bit color value. Opaque.
  AdCol24_MediumSlateBlue : longint = 15624315;

  //Web color "MediumSpringGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSpringGreen : TAndorraColor = (r: 0; g: 250; b: 154; a: 255);
  //Web color "MediumSpringGreen " as a 24 Bit color value. Opaque.
  AdCol24_MediumSpringGreen : longint = 10156544;

  //Web color "MediumTurquoise " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumTurquoise : TAndorraColor = (r: 72; g: 209; b: 204; a: 255);
  //Web color "MediumTurquoise " as a 24 Bit color value. Opaque.
  AdCol24_MediumTurquoise : longint = 13422920;

  //Web color "MediumVioletRed " as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumVioletRed : TAndorraColor = (r: 199; g: 21; b: 133; a: 255);
  //Web color "MediumVioletRed " as a 24 Bit color value. Opaque.
  AdCol24_MediumVioletRed : longint = 8721863;

  //Web color "MidnightBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_MidnightBlue : TAndorraColor = (r: 25; g: 25; b: 112; a: 255);
  //Web color "MidnightBlue " as a 24 Bit color value. Opaque.
  AdCol24_MidnightBlue : longint = 7346457;

  //Web color "MintCream " as a 32 Bit andorra color value. Opaque.
  AdCol32_MintCream : TAndorraColor = (r: 245; g: 255; b: 250; a: 255);
  //Web color "MintCream " as a 24 Bit color value. Opaque.
  AdCol24_MintCream : longint = 16449525;

  //Web color "MistyRose " as a 32 Bit andorra color value. Opaque.
  AdCol32_MistyRose : TAndorraColor = (r: 255; g: 228; b: 225; a: 255);
  //Web color "MistyRose " as a 24 Bit color value. Opaque.
  AdCol24_MistyRose : longint = 14804223;

  //Web color "Moccasin " as a 32 Bit andorra color value. Opaque.
  AdCol32_Moccasin : TAndorraColor = (r: 255; g: 228; b: 181; a: 255);
  //Web color "Moccasin " as a 24 Bit color value. Opaque.
  AdCol24_Moccasin : longint = 11920639;

  //Web color "NavajoWhite " as a 32 Bit andorra color value. Opaque.
  AdCol32_NavajoWhite : TAndorraColor = (r: 255; g: 222; b: 173; a: 255);
  //Web color "NavajoWhite " as a 24 Bit color value. Opaque.
  AdCol24_NavajoWhite : longint = 11394815;

  //Web color "Navy " as a 32 Bit andorra color value. Opaque.
  AdCol32_Navy : TAndorraColor = (r: 0; g: 0; b: 128; a: 255);
  //Web color "Navy " as a 24 Bit color value. Opaque.
  AdCol24_Navy : longint = 8388608;

  //Web color "OldLace " as a 32 Bit andorra color value. Opaque.
  AdCol32_OldLace : TAndorraColor = (r: 253; g: 245; b: 230; a: 255);
  //Web color "OldLace " as a 24 Bit color value. Opaque.
  AdCol24_OldLace : longint = 15136253;

  //Web color "Olive " as a 32 Bit andorra color value. Opaque.
  AdCol32_Olive : TAndorraColor = (r: 128; g: 128; b: 0; a: 255);
  //Web color "Olive " as a 24 Bit color value. Opaque.
  AdCol24_Olive : longint = 32896;

  //Web color "OliveDrab " as a 32 Bit andorra color value. Opaque.
  AdCol32_OliveDrab : TAndorraColor = (r: 107; g: 142; b: 35; a: 255);
  //Web color "OliveDrab " as a 24 Bit color value. Opaque.
  AdCol24_OliveDrab : longint = 2330219;

  //Web color "Orange " as a 32 Bit andorra color value. Opaque.
  AdCol32_Orange : TAndorraColor = (r: 255; g: 165; b: 0; a: 255);
  //Web color "Orange " as a 24 Bit color value. Opaque.
  AdCol24_Orange : longint = 42495;

  //Web color "OrangeRed " as a 32 Bit andorra color value. Opaque.
  AdCol32_OrangeRed : TAndorraColor = (r: 255; g: 69; b: 0; a: 255);
  //Web color "OrangeRed " as a 24 Bit color value. Opaque.
  AdCol24_OrangeRed : longint = 17919;

  //Web color "Orchid " as a 32 Bit andorra color value. Opaque.
  AdCol32_Orchid : TAndorraColor = (r: 218; g: 112; b: 214; a: 255);
  //Web color "Orchid " as a 24 Bit color value. Opaque.
  AdCol24_Orchid : longint = 14053594;

  //Web color "PaleGoldenrod " as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleGoldenrod : TAndorraColor = (r: 238; g: 232; b: 170; a: 255);
  //Web color "PaleGoldenrod " as a 24 Bit color value. Opaque.
  AdCol24_PaleGoldenrod : longint = 11200750;

  //Web color "PaleGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleGreen : TAndorraColor = (r: 152; g: 251; b: 152; a: 255);
  //Web color "PaleGreen " as a 24 Bit color value. Opaque.
  AdCol24_PaleGreen : longint = 10025880;

  //Web color "PaleTurquoise " as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleTurquoise : TAndorraColor = (r: 175; g: 238; b: 238; a: 255);
  //Web color "PaleTurquoise " as a 24 Bit color value. Opaque.
  AdCol24_PaleTurquoise : longint = 15658671;

  //Web color "PaleVioletRed " as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleVioletRed : TAndorraColor = (r: 219; g: 112; b: 147; a: 255);
  //Web color "PaleVioletRed " as a 24 Bit color value. Opaque.
  AdCol24_PaleVioletRed : longint = 9662683;

  //Web color "PapayaWhip " as a 32 Bit andorra color value. Opaque.
  AdCol32_PapayaWhip : TAndorraColor = (r: 255; g: 239; b: 213; a: 255);
  //Web color "PapayaWhip " as a 24 Bit color value. Opaque.
  AdCol24_PapayaWhip : longint = 14020607;

  //Web color "PeachPuff " as a 32 Bit andorra color value. Opaque.
  AdCol32_PeachPuff : TAndorraColor = (r: 255; g: 218; b: 185; a: 255);
  //Web color "PeachPuff " as a 24 Bit color value. Opaque.
  AdCol24_PeachPuff : longint = 12180223;

  //Web color "Peru " as a 32 Bit andorra color value. Opaque.
  AdCol32_Peru : TAndorraColor = (r: 205; g: 133; b: 63; a: 255);
  //Web color "Peru " as a 24 Bit color value. Opaque.
  AdCol24_Peru : longint = 4163021;

  //Web color "Pink " as a 32 Bit andorra color value. Opaque.
  AdCol32_Pink : TAndorraColor = (r: 255; g: 192; b: 203; a: 255);
  //Web color "Pink " as a 24 Bit color value. Opaque.
  AdCol24_Pink : longint = 13353215;

  //Web color "Plum " as a 32 Bit andorra color value. Opaque.
  AdCol32_Plum : TAndorraColor = (r: 221; g: 160; b: 221; a: 255);
  //Web color "Plum " as a 24 Bit color value. Opaque.
  AdCol24_Plum : longint = 14524637;

  //Web color "PowderBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_PowderBlue : TAndorraColor = (r: 176; g: 224; b: 230; a: 255);
  //Web color "PowderBlue " as a 24 Bit color value. Opaque.
  AdCol24_PowderBlue : longint = 15130800;

  //Web color "Purple " as a 32 Bit andorra color value. Opaque.
  AdCol32_Purple : TAndorraColor = (r: 128; g: 0; b: 128; a: 255);
  //Web color "Purple " as a 24 Bit color value. Opaque.
  AdCol24_Purple : longint = 8388736;

  //Web color "Red " as a 32 Bit andorra color value. Opaque.
  AdCol32_Red : TAndorraColor = (r: 255; g: 0; b: 0; a: 255);
  //Web color "Red " as a 24 Bit color value. Opaque.
  AdCol24_Red : longint = 255;

  //Web color "RosyBrown " as a 32 Bit andorra color value. Opaque.
  AdCol32_RosyBrown : TAndorraColor = (r: 188; g: 143; b: 143; a: 255);
  //Web color "RosyBrown " as a 24 Bit color value. Opaque.
  AdCol24_RosyBrown : longint = 9408444;

  //Web color "RoyalBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_RoyalBlue : TAndorraColor = (r: 65; g: 105; b: 225; a: 255);
  //Web color "RoyalBlue " as a 24 Bit color value. Opaque.
  AdCol24_RoyalBlue : longint = 14772545;

  //Web color "SaddleBrown " as a 32 Bit andorra color value. Opaque.
  AdCol32_SaddleBrown : TAndorraColor = (r: 139; g: 69; b: 19; a: 255);
  //Web color "SaddleBrown " as a 24 Bit color value. Opaque.
  AdCol24_SaddleBrown : longint = 1262987;

  //Web color "Salmon " as a 32 Bit andorra color value. Opaque.
  AdCol32_Salmon : TAndorraColor = (r: 250; g: 128; b: 114; a: 255);
  //Web color "Salmon " as a 24 Bit color value. Opaque.
  AdCol24_Salmon : longint = 7504122;

  //Web color "SandyBrown " as a 32 Bit andorra color value. Opaque.
  AdCol32_SandyBrown : TAndorraColor = (r: 244; g: 164; b: 96; a: 255);
  //Web color "SandyBrown " as a 24 Bit color value. Opaque.
  AdCol24_SandyBrown : longint = 6333684;

  //Web color "SeaGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_SeaGreen : TAndorraColor = (r: 46; g: 139; b: 87; a: 255);
  //Web color "SeaGreen " as a 24 Bit color value. Opaque.
  AdCol24_SeaGreen : longint = 5737262;

  //Web color "Seashell " as a 32 Bit andorra color value. Opaque.
  AdCol32_Seashell : TAndorraColor = (r: 255; g: 245; b: 238; a: 255);
  //Web color "Seashell " as a 24 Bit color value. Opaque.
  AdCol24_Seashell : longint = 15660543;

  //Web color "Sienna " as a 32 Bit andorra color value. Opaque.
  AdCol32_Sienna : TAndorraColor = (r: 160; g: 82; b: 45; a: 255);
  //Web color "Sienna " as a 24 Bit color value. Opaque.
  AdCol24_Sienna : longint = 2970272;

  //Web color "Silver " as a 32 Bit andorra color value. Opaque.
  AdCol32_Silver : TAndorraColor = (r: 192; g: 192; b: 192; a: 255);
  //Web color "Silver " as a 24 Bit color value. Opaque.
  AdCol24_Silver : longint = 12632256;

  //Web color "SkyBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_SkyBlue : TAndorraColor = (r: 135; g: 206; b: 235; a: 255);
  //Web color "SkyBlue " as a 24 Bit color value. Opaque.
  AdCol24_SkyBlue : longint = 15453831;

  //Web color "SlateBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_SlateBlue : TAndorraColor = (r: 106; g: 90; b: 205; a: 255);
  //Web color "SlateBlue " as a 24 Bit color value. Opaque.
  AdCol24_SlateBlue : longint = 13458026;

  //Web color "SlateGray " as a 32 Bit andorra color value. Opaque.
  AdCol32_SlateGray : TAndorraColor = (r: 112; g: 128; b: 144; a: 255);
  //Web color "SlateGray " as a 24 Bit color value. Opaque.
  AdCol24_SlateGray : longint = 9470064;

  //Web color "Snow " as a 32 Bit andorra color value. Opaque.
  AdCol32_Snow : TAndorraColor = (r: 255; g: 250; b: 250; a: 255);
  //Web color "Snow " as a 24 Bit color value. Opaque.
  AdCol24_Snow : longint = 16448255;

  //Web color "SpringGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_SpringGreen : TAndorraColor = (r: 0; g: 255; b: 127; a: 255);
  //Web color "SpringGreen " as a 24 Bit color value. Opaque.
  AdCol24_SpringGreen : longint = 8388352;

  //Web color "SteelBlue " as a 32 Bit andorra color value. Opaque.
  AdCol32_SteelBlue : TAndorraColor = (r: 70; g: 130; b: 180; a: 255);
  //Web color "SteelBlue " as a 24 Bit color value. Opaque.
  AdCol24_SteelBlue : longint = 11829830;

  //Web color "Tan " as a 32 Bit andorra color value. Opaque.
  AdCol32_Tan : TAndorraColor = (r: 210; g: 180; b: 140; a: 255);
  //Web color "Tan " as a 24 Bit color value. Opaque.
  AdCol24_Tan : longint = 9221330;

  //Web color "Teal " as a 32 Bit andorra color value. Opaque.
  AdCol32_Teal : TAndorraColor = (r: 0; g: 128; b: 128; a: 255);
  //Web color "Teal " as a 24 Bit color value. Opaque.
  AdCol24_Teal : longint = 8421376;

  //Web color "Thistle " as a 32 Bit andorra color value. Opaque.
  AdCol32_Thistle : TAndorraColor = (r: 216; g: 191; b: 216; a: 255);
  //Web color "Thistle " as a 24 Bit color value. Opaque.
  AdCol24_Thistle : longint = 14204888;

  //Web color "Tomato " as a 32 Bit andorra color value. Opaque.
  AdCol32_Tomato : TAndorraColor = (r: 255; g: 99; b: 71; a: 255);
  //Web color "Tomato " as a 24 Bit color value. Opaque.
  AdCol24_Tomato : longint = 4678655;

  //Web color "Turquoise " as a 32 Bit andorra color value. Opaque.
  AdCol32_Turquoise : TAndorraColor = (r: 64; g: 224; b: 208; a: 255);
  //Web color "Turquoise " as a 24 Bit color value. Opaque.
  AdCol24_Turquoise : longint = 13688896;

  //Web color "Violet " as a 32 Bit andorra color value. Opaque.
  AdCol32_Violet : TAndorraColor = (r: 238; g: 130; b: 238; a: 255);
  //Web color "Violet " as a 24 Bit color value. Opaque.
  AdCol24_Violet : longint = 15631086;

  //Web color "Wheat " as a 32 Bit andorra color value. Opaque.
  AdCol32_Wheat : TAndorraColor = (r: 245; g: 222; b: 179; a: 255);
  //Web color "Wheat " as a 24 Bit color value. Opaque.
  AdCol24_Wheat : longint = 11788021;

  //Web color "White " as a 32 Bit andorra color value. Opaque.
  AdCol32_White : TAndorraColor = (r: 255; g: 255; b: 255; a: 255);
  //Web color "White " as a 24 Bit color value. Opaque.
  AdCol24_White : longint = 16777215;

  //Web color "WhiteSmoke " as a 32 Bit andorra color value. Opaque.
  AdCol32_WhiteSmoke : TAndorraColor = (r: 245; g: 245; b: 245; a: 255);
  //Web color "WhiteSmoke " as a 24 Bit color value. Opaque.
  AdCol24_WhiteSmoke : longint = 16119285;

  //Web color "Yellow " as a 32 Bit andorra color value. Opaque.
  AdCol32_Yellow : TAndorraColor = (r: 255; g: 255; b: 0; a: 255);
  //Web color "Yellow " as a 24 Bit color value. Opaque.
  AdCol24_Yellow : longint = 65535;

  //Web color "YellowGreen " as a 32 Bit andorra color value. Opaque.
  AdCol32_YellowGreen : TAndorraColor = (r: 154; g: 205; b: 50; a: 255);
  //Web color "YellowGreen " as a 24 Bit color value. Opaque.
  AdCol24_YellowGreen : longint = 3329434;


implementation

end.
