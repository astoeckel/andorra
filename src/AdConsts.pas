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
//http://www.w3.org/TR/css3-color/#svg-color
//The AdCol24 colors are compatible to the standard Delphi/Lazarus colors
const
  //Web color "AliceBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_AliceBlue: TAndorraColor = (r: 240; g: 248; b: 255; a: 255);
  //Web color "AliceBlue" as a 24 Bit color value.
  AdCol24_AliceBlue: Longint = 16775408;

  //Web color "AntiqueWhite" as a 32 Bit andorra color value. Opaque.
  AdCol32_AntiqueWhite: TAndorraColor = (r: 250; g: 235; b: 215; a: 255);
  //Web color "AntiqueWhite" as a 24 Bit color value.
  AdCol24_AntiqueWhite: Longint = 14150650;

  //Web color "Aqua" as a 32 Bit andorra color value. Opaque.
  AdCol32_Aqua: TAndorraColor = (r: 0; g: 255; b: 255; a: 255);
  //Web color "Aqua" as a 24 Bit color value.
  AdCol24_Aqua: Longint = 16776960;

  //Web color "Aquamarine" as a 32 Bit andorra color value. Opaque.
  AdCol32_Aquamarine: TAndorraColor = (r: 127; g: 255; b: 212; a: 255);
  //Web color "Aquamarine" as a 24 Bit color value.
  AdCol24_Aquamarine: Longint = 13959039;

  //Web color "Azure" as a 32 Bit andorra color value. Opaque.
  AdCol32_Azure: TAndorraColor = (r: 240; g: 255; b: 255; a: 255);
  //Web color "Azure" as a 24 Bit color value.
  AdCol24_Azure: Longint = 16777200;

  //Web color "Beige" as a 32 Bit andorra color value. Opaque.
  AdCol32_Beige: TAndorraColor = (r: 245; g: 245; b: 220; a: 255);
  //Web color "Beige" as a 24 Bit color value.
  AdCol24_Beige: Longint = 14480885;

  //Web color "Bisque" as a 32 Bit andorra color value. Opaque.
  AdCol32_Bisque: TAndorraColor = (r: 255; g: 228; b: 196; a: 255);
  //Web color "Bisque" as a 24 Bit color value.
  AdCol24_Bisque: Longint = 12903679;

  //Web color "Black" as a 32 Bit andorra color value. Opaque.
  AdCol32_Black: TAndorraColor = (r: 0; g: 0; b: 0; a: 255);
  //Web color "Black" as a 24 Bit color value.
  AdCol24_Black: Longint = 0;

  //Web color "BlanchedAlmond" as a 32 Bit andorra color value. Opaque.
  AdCol32_BlanchedAlmond: TAndorraColor = (r: 255; g: 235; b: 205; a: 255);
  //Web color "BlanchedAlmond" as a 24 Bit color value.
  AdCol24_BlanchedAlmond: Longint = 13495295;

  //Web color "Blue" as a 32 Bit andorra color value. Opaque.
  AdCol32_Blue: TAndorraColor = (r: 0; g: 0; b: 255; a: 255);
  //Web color "Blue" as a 24 Bit color value.
  AdCol24_Blue: Longint = 16711680;

  //Web color "BlueViolet" as a 32 Bit andorra color value. Opaque.
  AdCol32_BlueViolet: TAndorraColor = (r: 138; g: 43; b: 226; a: 255);
  //Web color "BlueViolet" as a 24 Bit color value.
  AdCol24_BlueViolet: Longint = 14822282;

  //Web color "Brown" as a 32 Bit andorra color value. Opaque.
  AdCol32_Brown: TAndorraColor = (r: 165; g: 42; b: 42; a: 255);
  //Web color "Brown" as a 24 Bit color value.
  AdCol24_Brown: Longint = 2763429;

  //Web color "BurlyWood" as a 32 Bit andorra color value. Opaque.
  AdCol32_BurlyWood: TAndorraColor = (r: 222; g: 184; b: 135; a: 255);
  //Web color "BurlyWood" as a 24 Bit color value.
  AdCol24_BurlyWood: Longint = 8894686;

  //Web color "CadetBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_CadetBlue: TAndorraColor = (r: 95; g: 158; b: 160; a: 255);
  //Web color "CadetBlue" as a 24 Bit color value.
  AdCol24_CadetBlue: Longint = 10526303;

  //Web color "Chartreuse" as a 32 Bit andorra color value. Opaque.
  AdCol32_Chartreuse: TAndorraColor = (r: 127; g: 255; b: 0; a: 255);
  //Web color "Chartreuse" as a 24 Bit color value.
  AdCol24_Chartreuse: Longint = 65407;

  //Web color "Chocolate" as a 32 Bit andorra color value. Opaque.
  AdCol32_Chocolate: TAndorraColor = (r: 210; g: 105; b: 30; a: 255);
  //Web color "Chocolate" as a 24 Bit color value.
  AdCol24_Chocolate: Longint = 1993170;

  //Web color "Coral" as a 32 Bit andorra color value. Opaque.
  AdCol32_Coral: TAndorraColor = (r: 255; g: 127; b: 80; a: 255);
  //Web color "Coral" as a 24 Bit color value.
  AdCol24_Coral: Longint = 5275647;

  //Web color "CornflowerBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_CornflowerBlue: TAndorraColor = (r: 100; g: 149; b: 237; a: 255);
  //Web color "CornflowerBlue" as a 24 Bit color value.
  AdCol24_CornflowerBlue: Longint = 15570276;

  //Web color "Cornsilk" as a 32 Bit andorra color value. Opaque.
  AdCol32_Cornsilk: TAndorraColor = (r: 255; g: 248; b: 220; a: 255);
  //Web color "Cornsilk" as a 24 Bit color value.
  AdCol24_Cornsilk: Longint = 14481663;

  //Web color "Crimson" as a 32 Bit andorra color value. Opaque.
  AdCol32_Crimson: TAndorraColor = (r: 220; g: 20; b: 60; a: 255);
  //Web color "Crimson" as a 24 Bit color value.
  AdCol24_Crimson: Longint = 3937500;

  //Web color "Cyan" as a 32 Bit andorra color value. Opaque.
  AdCol32_Cyan: TAndorraColor = (r: 0; g: 255; b: 255; a: 255);
  //Web color "Cyan" as a 24 Bit color value.
  AdCol24_Cyan: Longint = 16776960;

  //Web color "DarkBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkBlue: TAndorraColor = (r: 0; g: 0; b: 139; a: 255);
  //Web color "DarkBlue" as a 24 Bit color value.
  AdCol24_DarkBlue: Longint = 9109504;

  //Web color "DarkCyan" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkCyan: TAndorraColor = (r: 0; g: 139; b: 139; a: 255);
  //Web color "DarkCyan" as a 24 Bit color value.
  AdCol24_DarkCyan: Longint = 9145088;

  //Web color "DarkGoldenrod" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGoldenrod: TAndorraColor = (r: 184; g: 134; b: 11; a: 255);
  //Web color "DarkGoldenrod" as a 24 Bit color value.
  AdCol24_DarkGoldenrod: Longint = 755384;

  //Web color "DarkGray" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGray: TAndorraColor = (r: 169; g: 169; b: 169; a: 255);
  //Web color "DarkGray" as a 24 Bit color value.
  AdCol24_DarkGray: Longint = 11119017;

  //Web color "DarkGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkGreen: TAndorraColor = (r: 0; g: 100; b: 0; a: 255);
  //Web color "DarkGreen" as a 24 Bit color value.
  AdCol24_DarkGreen: Longint = 25600;

  //Web color "DarkKhaki" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkKhaki: TAndorraColor = (r: 189; g: 183; b: 107; a: 255);
  //Web color "DarkKhaki" as a 24 Bit color value.
  AdCol24_DarkKhaki: Longint = 7059389;

  //Web color "DarkMagenta" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkMagenta: TAndorraColor = (r: 139; g: 0; b: 139; a: 255);
  //Web color "DarkMagenta" as a 24 Bit color value.
  AdCol24_DarkMagenta: Longint = 9109643;

  //Web color "DarkOliveGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOliveGreen: TAndorraColor = (r: 85; g: 107; b: 47; a: 255);
  //Web color "DarkOliveGreen" as a 24 Bit color value.
  AdCol24_DarkOliveGreen: Longint = 3107669;

  //Web color "DarkOrange" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOrange: TAndorraColor = (r: 255; g: 140; b: 0; a: 255);
  //Web color "DarkOrange" as a 24 Bit color value.
  AdCol24_DarkOrange: Longint = 36095;

  //Web color "DarkOrchid" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkOrchid: TAndorraColor = (r: 153; g: 50; b: 204; a: 255);
  //Web color "DarkOrchid" as a 24 Bit color value.
  AdCol24_DarkOrchid: Longint = 13382297;

  //Web color "DarkRed" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkRed: TAndorraColor = (r: 139; g: 0; b: 0; a: 255);
  //Web color "DarkRed" as a 24 Bit color value.
  AdCol24_DarkRed: Longint = 139;

  //Web color "DarkSalmon" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSalmon: TAndorraColor = (r: 233; g: 150; b: 122; a: 255);
  //Web color "DarkSalmon" as a 24 Bit color value.
  AdCol24_DarkSalmon: Longint = 8034025;

  //Web color "DarkSeaGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSeaGreen: TAndorraColor = (r: 143; g: 188; b: 143; a: 255);
  //Web color "DarkSeaGreen" as a 24 Bit color value.
  AdCol24_DarkSeaGreen: Longint = 9419919;

  //Web color "DarkSlateBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSlateBlue: TAndorraColor = (r: 72; g: 61; b: 139; a: 255);
  //Web color "DarkSlateBlue" as a 24 Bit color value.
  AdCol24_DarkSlateBlue: Longint = 9125192;

  //Web color "DarkSlateGray" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkSlateGray: TAndorraColor = (r: 47; g: 79; b: 79; a: 255);
  //Web color "DarkSlateGray" as a 24 Bit color value.
  AdCol24_DarkSlateGray: Longint = 5197615;

  //Web color "DarkTurquoise" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkTurquoise: TAndorraColor = (r: 0; g: 206; b: 209; a: 255);
  //Web color "DarkTurquoise" as a 24 Bit color value.
  AdCol24_DarkTurquoise: Longint = 13749760;

  //Web color "DarkViolet" as a 32 Bit andorra color value. Opaque.
  AdCol32_DarkViolet: TAndorraColor = (r: 148; g: 0; b: 211; a: 255);
  //Web color "DarkViolet" as a 24 Bit color value.
  AdCol24_DarkViolet: Longint = 13828244;

  //Web color "DeepPink" as a 32 Bit andorra color value. Opaque.
  AdCol32_DeepPink: TAndorraColor = (r: 255; g: 20; b: 147; a: 255);
  //Web color "DeepPink" as a 24 Bit color value.
  AdCol24_DeepPink: Longint = 9639167;

  //Web color "DeepSkyBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_DeepSkyBlue: TAndorraColor = (r: 0; g: 191; b: 255; a: 255);
  //Web color "DeepSkyBlue" as a 24 Bit color value.
  AdCol24_DeepSkyBlue: Longint = 16760576;

  //Web color "DimGray" as a 32 Bit andorra color value. Opaque.
  AdCol32_DimGray: TAndorraColor = (r: 105; g: 105; b: 105; a: 255);
  //Web color "DimGray" as a 24 Bit color value.
  AdCol24_DimGray: Longint = 6908265;

  //Web color "DodgerBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_DodgerBlue: TAndorraColor = (r: 30; g: 144; b: 255; a: 255);
  //Web color "DodgerBlue" as a 24 Bit color value.
  AdCol24_DodgerBlue: Longint = 16748574;

  //Web color "FireBrick" as a 32 Bit andorra color value. Opaque.
  AdCol32_FireBrick: TAndorraColor = (r: 178; g: 34; b: 34; a: 255);
  //Web color "FireBrick" as a 24 Bit color value.
  AdCol24_FireBrick: Longint = 2237106;

  //Web color "FloralWhite" as a 32 Bit andorra color value. Opaque.
  AdCol32_FloralWhite: TAndorraColor = (r: 255; g: 250; b: 240; a: 255);
  //Web color "FloralWhite" as a 24 Bit color value.
  AdCol24_FloralWhite: Longint = 15792895;

  //Web color "ForestGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_ForestGreen: TAndorraColor = (r: 34; g: 139; b: 34; a: 255);
  //Web color "ForestGreen" as a 24 Bit color value.
  AdCol24_ForestGreen: Longint = 2263842;

  //Web color "Fuchsia" as a 32 Bit andorra color value. Opaque.
  AdCol32_Fuchsia: TAndorraColor = (r: 255; g: 0; b: 255; a: 255);
  //Web color "Fuchsia" as a 24 Bit color value.
  AdCol24_Fuchsia: Longint = 16711935;

  //Web color "Gainsboro" as a 32 Bit andorra color value. Opaque.
  AdCol32_Gainsboro: TAndorraColor = (r: 220; g: 220; b: 220; a: 255);
  //Web color "Gainsboro" as a 24 Bit color value.
  AdCol24_Gainsboro: Longint = 14474460;

  //Web color "GhostWhite" as a 32 Bit andorra color value. Opaque.
  AdCol32_GhostWhite: TAndorraColor = (r: 248; g: 248; b: 255; a: 255);
  //Web color "GhostWhite" as a 24 Bit color value.
  AdCol24_GhostWhite: Longint = 16775416;

  //Web color "Gold" as a 32 Bit andorra color value. Opaque.
  AdCol32_Gold: TAndorraColor = (r: 255; g: 215; b: 0; a: 255);
  //Web color "Gold" as a 24 Bit color value.
  AdCol24_Gold: Longint = 55295;

  //Web color "Goldenrod" as a 32 Bit andorra color value. Opaque.
  AdCol32_Goldenrod: TAndorraColor = (r: 218; g: 165; b: 32; a: 255);
  //Web color "Goldenrod" as a 24 Bit color value.
  AdCol24_Goldenrod: Longint = 2139610;

  //Web color "Gray" as a 32 Bit andorra color value. Opaque.
  AdCol32_Gray: TAndorraColor = (r: 128; g: 128; b: 128; a: 255);
  //Web color "Gray" as a 24 Bit color value.
  AdCol24_Gray: Longint = 8421504;

  //Web color "Green" as a 32 Bit andorra color value. Opaque.
  AdCol32_Green: TAndorraColor = (r: 0; g: 128; b: 0; a: 255);
  //Web color "Green" as a 24 Bit color value.
  AdCol24_Green: Longint = 32768;

  //Web color "GreenYellow" as a 32 Bit andorra color value. Opaque.
  AdCol32_GreenYellow: TAndorraColor = (r: 173; g: 255; b: 47; a: 255);
  //Web color "GreenYellow" as a 24 Bit color value.
  AdCol24_GreenYellow: Longint = 3145645;

  //Web color "Honeydew" as a 32 Bit andorra color value. Opaque.
  AdCol32_Honeydew: TAndorraColor = (r: 240; g: 255; b: 240; a: 255);
  //Web color "Honeydew" as a 24 Bit color value.
  AdCol24_Honeydew: Longint = 15794160;

  //Web color "HotPink" as a 32 Bit andorra color value. Opaque.
  AdCol32_HotPink: TAndorraColor = (r: 255; g: 105; b: 180; a: 255);
  //Web color "HotPink" as a 24 Bit color value.
  AdCol24_HotPink: Longint = 11823615;

  //Web color "IndianRed" as a 32 Bit andorra color value. Opaque.
  AdCol32_IndianRed: TAndorraColor = (r: 205; g: 92; b: 92; a: 255);
  //Web color "IndianRed" as a 24 Bit color value.
  AdCol24_IndianRed: Longint = 6053069;

  //Web color "Indigo" as a 32 Bit andorra color value. Opaque.
  AdCol32_Indigo: TAndorraColor = (r: 75; g: 0; b: 130; a: 255);
  //Web color "Indigo" as a 24 Bit color value.
  AdCol24_Indigo: Longint = 8519755;

  //Web color "Ivory" as a 32 Bit andorra color value. Opaque.
  AdCol32_Ivory: TAndorraColor = (r: 255; g: 255; b: 240; a: 255);
  //Web color "Ivory" as a 24 Bit color value.
  AdCol24_Ivory: Longint = 15794175;

  //Web color "Khaki" as a 32 Bit andorra color value. Opaque.
  AdCol32_Khaki: TAndorraColor = (r: 240; g: 230; b: 140; a: 255);
  //Web color "Khaki" as a 24 Bit color value.
  AdCol24_Khaki: Longint = 9234160;

  //Web color "Lavender" as a 32 Bit andorra color value. Opaque.
  AdCol32_Lavender: TAndorraColor = (r: 230; g: 230; b: 250; a: 255);
  //Web color "Lavender" as a 24 Bit color value.
  AdCol24_Lavender: Longint = 16443110;

  //Web color "LavenderBlush" as a 32 Bit andorra color value. Opaque.
  AdCol32_LavenderBlush: TAndorraColor = (r: 255; g: 240; b: 245; a: 255);
  //Web color "LavenderBlush" as a 24 Bit color value.
  AdCol24_LavenderBlush: Longint = 16118015;

  //Web color "LawnGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_LawnGreen: TAndorraColor = (r: 124; g: 252; b: 0; a: 255);
  //Web color "LawnGreen" as a 24 Bit color value.
  AdCol24_LawnGreen: Longint = 64636;

  //Web color "LemonChiffon" as a 32 Bit andorra color value. Opaque.
  AdCol32_LemonChiffon: TAndorraColor = (r: 255; g: 250; b: 205; a: 255);
  //Web color "LemonChiffon" as a 24 Bit color value.
  AdCol24_LemonChiffon: Longint = 13499135;

  //Web color "LightBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightBlue: TAndorraColor = (r: 173; g: 216; b: 230; a: 255);
  //Web color "LightBlue" as a 24 Bit color value.
  AdCol24_LightBlue: Longint = 15128749;

  //Web color "LightCoral" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightCoral: TAndorraColor = (r: 240; g: 128; b: 128; a: 255);
  //Web color "LightCoral" as a 24 Bit color value.
  AdCol24_LightCoral: Longint = 8421616;

  //Web color "LightCyan" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightCyan: TAndorraColor = (r: 224; g: 255; b: 255; a: 255);
  //Web color "LightCyan" as a 24 Bit color value.
  AdCol24_LightCyan: Longint = 16777184;

  //Web color "LightGoldenrodYellow" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGoldenrodYellow: TAndorraColor = (r: 250; g: 250; b: 210; a: 255);
  //Web color "LightGoldenrodYellow" as a 24 Bit color value.
  AdCol24_LightGoldenrodYellow: Longint = 13826810;

  //Web color "LightGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGreen: TAndorraColor = (r: 144; g: 238; b: 144; a: 255);
  //Web color "LightGreen" as a 24 Bit color value.
  AdCol24_LightGreen: Longint = 9498256;

  //Web color "LightGrey" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightGrey: TAndorraColor = (r: 211; g: 211; b: 211; a: 255);
  //Web color "LightGrey" as a 24 Bit color value.
  AdCol24_LightGrey: Longint = 13882323;

  //Web color "LightPink" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightPink: TAndorraColor = (r: 255; g: 182; b: 193; a: 255);
  //Web color "LightPink" as a 24 Bit color value.
  AdCol24_LightPink: Longint = 12695295;

  //Web color "LightSalmon" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSalmon: TAndorraColor = (r: 255; g: 160; b: 122; a: 255);
  //Web color "LightSalmon" as a 24 Bit color value.
  AdCol24_LightSalmon: Longint = 8036607;

  //Web color "LightSeaGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSeaGreen: TAndorraColor = (r: 32; g: 178; b: 170; a: 255);
  //Web color "LightSeaGreen" as a 24 Bit color value.
  AdCol24_LightSeaGreen: Longint = 11186720;

  //Web color "LightSkyBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSkyBlue: TAndorraColor = (r: 135; g: 206; b: 250; a: 255);
  //Web color "LightSkyBlue" as a 24 Bit color value.
  AdCol24_LightSkyBlue: Longint = 16436871;

  //Web color "LightSlateGray" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSlateGray: TAndorraColor = (r: 119; g: 136; b: 153; a: 255);
  //Web color "LightSlateGray" as a 24 Bit color value.
  AdCol24_LightSlateGray: Longint = 10061943;

  //Web color "LightSteelBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightSteelBlue: TAndorraColor = (r: 176; g: 196; b: 222; a: 255);
  //Web color "LightSteelBlue" as a 24 Bit color value.
  AdCol24_LightSteelBlue: Longint = 14599344;

  //Web color "LightYellow" as a 32 Bit andorra color value. Opaque.
  AdCol32_LightYellow: TAndorraColor = (r: 255; g: 255; b: 224; a: 255);
  //Web color "LightYellow" as a 24 Bit color value.
  AdCol24_LightYellow: Longint = 14745599;

  //Web color "Lime" as a 32 Bit andorra color value. Opaque.
  AdCol32_Lime: TAndorraColor = (r: 0; g: 255; b: 0; a: 255);
  //Web color "Lime" as a 24 Bit color value.
  AdCol24_Lime: Longint = 65280;

  //Web color "LimeGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_LimeGreen: TAndorraColor = (r: 50; g: 205; b: 50; a: 255);
  //Web color "LimeGreen" as a 24 Bit color value.
  AdCol24_LimeGreen: Longint = 3329330;

  //Web color "Linen" as a 32 Bit andorra color value. Opaque.
  AdCol32_Linen: TAndorraColor = (r: 250; g: 240; b: 230; a: 255);
  //Web color "Linen" as a 24 Bit color value.
  AdCol24_Linen: Longint = 15134970;

  //Web color "Magenta" as a 32 Bit andorra color value. Opaque.
  AdCol32_Magenta: TAndorraColor = (r: 255; g: 0; b: 255; a: 255);
  //Web color "Magenta" as a 24 Bit color value.
  AdCol24_Magenta: Longint = 16711935;

  //Web color "Maroon" as a 32 Bit andorra color value. Opaque.
  AdCol32_Maroon: TAndorraColor = (r: 128; g: 0; b: 0; a: 255);
  //Web color "Maroon" as a 24 Bit color value.
  AdCol24_Maroon: Longint = 128;

  //Web color "MediumAquamarine" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumAquamarine: TAndorraColor = (r: 102; g: 205; b: 170; a: 255);
  //Web color "MediumAquamarine" as a 24 Bit color value.
  AdCol24_MediumAquamarine: Longint = 11193702;

  //Web color "MediumBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumBlue: TAndorraColor = (r: 0; g: 0; b: 205; a: 255);
  //Web color "MediumBlue" as a 24 Bit color value.
  AdCol24_MediumBlue: Longint = 13434880;

  //Web color "MediumOrchid" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumOrchid: TAndorraColor = (r: 186; g: 85; b: 211; a: 255);
  //Web color "MediumOrchid" as a 24 Bit color value.
  AdCol24_MediumOrchid: Longint = 13850042;

  //Web color "MediumPurple" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumPurple: TAndorraColor = (r: 147; g: 112; b: 219; a: 255);
  //Web color "MediumPurple" as a 24 Bit color value.
  AdCol24_MediumPurple: Longint = 14381203;

  //Web color "MediumSeaGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSeaGreen: TAndorraColor = (r: 60; g: 179; b: 113; a: 255);
  //Web color "MediumSeaGreen" as a 24 Bit color value.
  AdCol24_MediumSeaGreen: Longint = 7451452;

  //Web color "MediumSlateBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSlateBlue: TAndorraColor = (r: 123; g: 104; b: 238; a: 255);
  //Web color "MediumSlateBlue" as a 24 Bit color value.
  AdCol24_MediumSlateBlue: Longint = 15624315;

  //Web color "MediumSpringGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumSpringGreen: TAndorraColor = (r: 0; g: 250; b: 154; a: 255);
  //Web color "MediumSpringGreen" as a 24 Bit color value.
  AdCol24_MediumSpringGreen: Longint = 10156544;

  //Web color "MediumTurquoise" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumTurquoise: TAndorraColor = (r: 72; g: 209; b: 204; a: 255);
  //Web color "MediumTurquoise" as a 24 Bit color value.
  AdCol24_MediumTurquoise: Longint = 13422920;

  //Web color "MediumVioletRed" as a 32 Bit andorra color value. Opaque.
  AdCol32_MediumVioletRed: TAndorraColor = (r: 199; g: 21; b: 133; a: 255);
  //Web color "MediumVioletRed" as a 24 Bit color value.
  AdCol24_MediumVioletRed: Longint = 8721863;

  //Web color "MidnightBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_MidnightBlue: TAndorraColor = (r: 25; g: 25; b: 112; a: 255);
  //Web color "MidnightBlue" as a 24 Bit color value.
  AdCol24_MidnightBlue: Longint = 7346457;

  //Web color "MintCream" as a 32 Bit andorra color value. Opaque.
  AdCol32_MintCream: TAndorraColor = (r: 245; g: 255; b: 250; a: 255);
  //Web color "MintCream" as a 24 Bit color value.
  AdCol24_MintCream: Longint = 16449525;

  //Web color "MistyRose" as a 32 Bit andorra color value. Opaque.
  AdCol32_MistyRose: TAndorraColor = (r: 255; g: 228; b: 225; a: 255);
  //Web color "MistyRose" as a 24 Bit color value.
  AdCol24_MistyRose: Longint = 14804223;

  //Web color "Moccasin" as a 32 Bit andorra color value. Opaque.
  AdCol32_Moccasin: TAndorraColor = (r: 255; g: 228; b: 181; a: 255);
  //Web color "Moccasin" as a 24 Bit color value.
  AdCol24_Moccasin: Longint = 11920639;

  //Web color "NavajoWhite" as a 32 Bit andorra color value. Opaque.
  AdCol32_NavajoWhite: TAndorraColor = (r: 255; g: 222; b: 173; a: 255);
  //Web color "NavajoWhite" as a 24 Bit color value.
  AdCol24_NavajoWhite: Longint = 11394815;

  //Web color "Navy" as a 32 Bit andorra color value. Opaque.
  AdCol32_Navy: TAndorraColor = (r: 0; g: 0; b: 128; a: 255);
  //Web color "Navy" as a 24 Bit color value.
  AdCol24_Navy: Longint = 8388608;

  //Web color "OldLace" as a 32 Bit andorra color value. Opaque.
  AdCol32_OldLace: TAndorraColor = (r: 253; g: 245; b: 230; a: 255);
  //Web color "OldLace" as a 24 Bit color value.
  AdCol24_OldLace: Longint = 15136253;

  //Web color "Olive" as a 32 Bit andorra color value. Opaque.
  AdCol32_Olive: TAndorraColor = (r: 128; g: 128; b: 0; a: 255);
  //Web color "Olive" as a 24 Bit color value.
  AdCol24_Olive: Longint = 32896;

  //Web color "OliveDrab" as a 32 Bit andorra color value. Opaque.
  AdCol32_OliveDrab: TAndorraColor = (r: 107; g: 142; b: 35; a: 255);
  //Web color "OliveDrab" as a 24 Bit color value.
  AdCol24_OliveDrab: Longint = 2330219;

  //Web color "Orange" as a 32 Bit andorra color value. Opaque.
  AdCol32_Orange: TAndorraColor = (r: 255; g: 165; b: 0; a: 255);
  //Web color "Orange" as a 24 Bit color value.
  AdCol24_Orange: Longint = 42495;

  //Web color "OrangeRed" as a 32 Bit andorra color value. Opaque.
  AdCol32_OrangeRed: TAndorraColor = (r: 255; g: 69; b: 0; a: 255);
  //Web color "OrangeRed" as a 24 Bit color value.
  AdCol24_OrangeRed: Longint = 17919;

  //Web color "Orchid" as a 32 Bit andorra color value. Opaque.
  AdCol32_Orchid: TAndorraColor = (r: 218; g: 112; b: 214; a: 255);
  //Web color "Orchid" as a 24 Bit color value.
  AdCol24_Orchid: Longint = 14053594;

  //Web color "PaleGoldenrod" as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleGoldenrod: TAndorraColor = (r: 238; g: 232; b: 170; a: 255);
  //Web color "PaleGoldenrod" as a 24 Bit color value.
  AdCol24_PaleGoldenrod: Longint = 11200750;

  //Web color "PaleGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleGreen: TAndorraColor = (r: 152; g: 251; b: 152; a: 255);
  //Web color "PaleGreen" as a 24 Bit color value.
  AdCol24_PaleGreen: Longint = 10025880;

  //Web color "PaleTurquoise" as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleTurquoise: TAndorraColor = (r: 175; g: 238; b: 238; a: 255);
  //Web color "PaleTurquoise" as a 24 Bit color value.
  AdCol24_PaleTurquoise: Longint = 15658671;

  //Web color "PaleVioletRed" as a 32 Bit andorra color value. Opaque.
  AdCol32_PaleVioletRed: TAndorraColor = (r: 219; g: 112; b: 147; a: 255);
  //Web color "PaleVioletRed" as a 24 Bit color value.
  AdCol24_PaleVioletRed: Longint = 9662683;

  //Web color "PapayaWhip" as a 32 Bit andorra color value. Opaque.
  AdCol32_PapayaWhip: TAndorraColor = (r: 255; g: 239; b: 213; a: 255);
  //Web color "PapayaWhip" as a 24 Bit color value.
  AdCol24_PapayaWhip: Longint = 14020607;

  //Web color "PeachPuff" as a 32 Bit andorra color value. Opaque.
  AdCol32_PeachPuff: TAndorraColor = (r: 255; g: 218; b: 185; a: 255);
  //Web color "PeachPuff" as a 24 Bit color value.
  AdCol24_PeachPuff: Longint = 12180223;

  //Web color "Peru" as a 32 Bit andorra color value. Opaque.
  AdCol32_Peru: TAndorraColor = (r: 205; g: 133; b: 63; a: 255);
  //Web color "Peru" as a 24 Bit color value.
  AdCol24_Peru: Longint = 4163021;

  //Web color "Pink" as a 32 Bit andorra color value. Opaque.
  AdCol32_Pink: TAndorraColor = (r: 255; g: 192; b: 203; a: 255);
  //Web color "Pink" as a 24 Bit color value.
  AdCol24_Pink: Longint = 13353215;

  //Web color "Plum" as a 32 Bit andorra color value. Opaque.
  AdCol32_Plum: TAndorraColor = (r: 221; g: 160; b: 221; a: 255);
  //Web color "Plum" as a 24 Bit color value.
  AdCol24_Plum: Longint = 14524637;

  //Web color "PowderBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_PowderBlue: TAndorraColor = (r: 176; g: 224; b: 230; a: 255);
  //Web color "PowderBlue" as a 24 Bit color value.
  AdCol24_PowderBlue: Longint = 15130800;

  //Web color "Purple" as a 32 Bit andorra color value. Opaque.
  AdCol32_Purple: TAndorraColor = (r: 128; g: 0; b: 128; a: 255);
  //Web color "Purple" as a 24 Bit color value.
  AdCol24_Purple: Longint = 8388736;

  //Web color "Red" as a 32 Bit andorra color value. Opaque.
  AdCol32_Red: TAndorraColor = (r: 255; g: 0; b: 0; a: 255);
  //Web color "Red" as a 24 Bit color value.
  AdCol24_Red: Longint = 255;

  //Web color "RosyBrown" as a 32 Bit andorra color value. Opaque.
  AdCol32_RosyBrown: TAndorraColor = (r: 188; g: 143; b: 143; a: 255);
  //Web color "RosyBrown" as a 24 Bit color value.
  AdCol24_RosyBrown: Longint = 9408444;

  //Web color "RoyalBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_RoyalBlue: TAndorraColor = (r: 65; g: 105; b: 225; a: 255);
  //Web color "RoyalBlue" as a 24 Bit color value.
  AdCol24_RoyalBlue: Longint = 14772545;

  //Web color "SaddleBrown" as a 32 Bit andorra color value. Opaque.
  AdCol32_SaddleBrown: TAndorraColor = (r: 139; g: 69; b: 19; a: 255);
  //Web color "SaddleBrown" as a 24 Bit color value.
  AdCol24_SaddleBrown: Longint = 1262987;

  //Web color "Salmon" as a 32 Bit andorra color value. Opaque.
  AdCol32_Salmon: TAndorraColor = (r: 250; g: 128; b: 114; a: 255);
  //Web color "Salmon" as a 24 Bit color value.
  AdCol24_Salmon: Longint = 7504122;

  //Web color "SandyBrown" as a 32 Bit andorra color value. Opaque.
  AdCol32_SandyBrown: TAndorraColor = (r: 244; g: 164; b: 96; a: 255);
  //Web color "SandyBrown" as a 24 Bit color value.
  AdCol24_SandyBrown: Longint = 6333684;

  //Web color "SeaGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_SeaGreen: TAndorraColor = (r: 46; g: 139; b: 87; a: 255);
  //Web color "SeaGreen" as a 24 Bit color value.
  AdCol24_SeaGreen: Longint = 5737262;

  //Web color "Seashell" as a 32 Bit andorra color value. Opaque.
  AdCol32_Seashell: TAndorraColor = (r: 255; g: 245; b: 238; a: 255);
  //Web color "Seashell" as a 24 Bit color value.
  AdCol24_Seashell: Longint = 15660543;

  //Web color "Sienna" as a 32 Bit andorra color value. Opaque.
  AdCol32_Sienna: TAndorraColor = (r: 160; g: 82; b: 45; a: 255);
  //Web color "Sienna" as a 24 Bit color value.
  AdCol24_Sienna: Longint = 2970272;

  //Web color "Silver" as a 32 Bit andorra color value. Opaque.
  AdCol32_Silver: TAndorraColor = (r: 192; g: 192; b: 192; a: 255);
  //Web color "Silver" as a 24 Bit color value.
  AdCol24_Silver: Longint = 12632256;

  //Web color "SkyBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_SkyBlue: TAndorraColor = (r: 135; g: 206; b: 235; a: 255);
  //Web color "SkyBlue" as a 24 Bit color value.
  AdCol24_SkyBlue: Longint = 15453831;

  //Web color "SlateBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_SlateBlue: TAndorraColor = (r: 106; g: 90; b: 205; a: 255);
  //Web color "SlateBlue" as a 24 Bit color value.
  AdCol24_SlateBlue: Longint = 13458026;

  //Web color "SlateGray" as a 32 Bit andorra color value. Opaque.
  AdCol32_SlateGray: TAndorraColor = (r: 112; g: 128; b: 144; a: 255);
  //Web color "SlateGray" as a 24 Bit color value.
  AdCol24_SlateGray: Longint = 9470064;

  //Web color "Snow" as a 32 Bit andorra color value. Opaque.
  AdCol32_Snow: TAndorraColor = (r: 255; g: 250; b: 250; a: 255);
  //Web color "Snow" as a 24 Bit color value.
  AdCol24_Snow: Longint = 16448255;

  //Web color "SpringGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_SpringGreen: TAndorraColor = (r: 0; g: 255; b: 127; a: 255);
  //Web color "SpringGreen" as a 24 Bit color value.
  AdCol24_SpringGreen: Longint = 8388352;

  //Web color "SteelBlue" as a 32 Bit andorra color value. Opaque.
  AdCol32_SteelBlue: TAndorraColor = (r: 70; g: 130; b: 180; a: 255);
  //Web color "SteelBlue" as a 24 Bit color value.
  AdCol24_SteelBlue: Longint = 11829830;

  //Web color "Tan" as a 32 Bit andorra color value. Opaque.
  AdCol32_Tan: TAndorraColor = (r: 210; g: 180; b: 140; a: 255);
  //Web color "Tan" as a 24 Bit color value.
  AdCol24_Tan: Longint = 9221330;

  //Web color "Teal" as a 32 Bit andorra color value. Opaque.
  AdCol32_Teal: TAndorraColor = (r: 0; g: 128; b: 128; a: 255);
  //Web color "Teal" as a 24 Bit color value.
  AdCol24_Teal: Longint = 8421376;

  //Web color "Thistle" as a 32 Bit andorra color value. Opaque.
  AdCol32_Thistle: TAndorraColor = (r: 216; g: 191; b: 216; a: 255);
  //Web color "Thistle" as a 24 Bit color value.
  AdCol24_Thistle: Longint = 14204888;

  //Web color "Tomato" as a 32 Bit andorra color value. Opaque.
  AdCol32_Tomato: TAndorraColor = (r: 255; g: 99; b: 71; a: 255);
  //Web color "Tomato" as a 24 Bit color value.
  AdCol24_Tomato: Longint = 4678655;

  //Web color "Turquoise" as a 32 Bit andorra color value. Opaque.
  AdCol32_Turquoise: TAndorraColor = (r: 64; g: 224; b: 208; a: 255);
  //Web color "Turquoise" as a 24 Bit color value.
  AdCol24_Turquoise: Longint = 13688896;

  //Web color "Violet" as a 32 Bit andorra color value. Opaque.
  AdCol32_Violet: TAndorraColor = (r: 238; g: 130; b: 238; a: 255);
  //Web color "Violet" as a 24 Bit color value.
  AdCol24_Violet: Longint = 15631086;

  //Web color "Wheat" as a 32 Bit andorra color value. Opaque.
  AdCol32_Wheat: TAndorraColor = (r: 245; g: 222; b: 179; a: 255);
  //Web color "Wheat" as a 24 Bit color value.
  AdCol24_Wheat: Longint = 11788021;

  //Web color "White" as a 32 Bit andorra color value. Opaque.
  AdCol32_White: TAndorraColor = (r: 255; g: 255; b: 255; a: 255);
  //Web color "White" as a 24 Bit color value.
  AdCol24_White: Longint = 16777215;

  //Web color "WhiteSmoke" as a 32 Bit andorra color value. Opaque.
  AdCol32_WhiteSmoke: TAndorraColor = (r: 245; g: 245; b: 245; a: 255);
  //Web color "WhiteSmoke" as a 24 Bit color value.
  AdCol24_WhiteSmoke: Longint = 16119285;

  //Web color "Yellow" as a 32 Bit andorra color value. Opaque.
  AdCol32_Yellow: TAndorraColor = (r: 255; g: 255; b: 0; a: 255);
  //Web color "Yellow" as a 24 Bit color value.
  AdCol24_Yellow: Longint = 65535;

  //Web color "YellowGreen" as a 32 Bit andorra color value. Opaque.
  AdCol32_YellowGreen: TAndorraColor = (r: 154; g: 205; b: 50; a: 255);
  //Web color "YellowGreen" as a 24 Bit color value.
  AdCol24_YellowGreen: Longint = 3329434;

//Material declarations taken from
//http://wiki.delphigl.com/index.php/Materialsammlung
const
  //Metal

  //Gold - polished
  AdMat_Gold_Polished: TAd2dMaterial = (
    Diffuse: (r: 90; g: 79; b: 23; a: 255);
    Ambient: (r: 63; g: 56; b: 15; a: 255);
    Specular: (r: 204; g: 186; b: 54; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 20;
  );

  //Chrome
  AdMat_Chrome: TAd2dMaterial = (
    Diffuse: (r: 100; g: 100; b: 100; a: 255);
    Ambient: (r: 64; g: 64; b: 64; a: 255);
    Specular: (r: 198; g: 198; b: 198; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 40;
  );

  //--------

  //Plastic

  //Black rubber (e.g. for the tires of cars)
  AdMat_Rubber_Black: TAd2dMaterial = (
    Diffuse: (r: 2; g: 2; b: 2; a: 255);
    Ambient: (r: 4; g: 4; b: 4; a: 255);
    Specular: (r: 100; g: 100; b: 100; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 10;
  );

  //Black plastic
  AdMat_Plastic_Black: TAd2dMaterial = (
    Diffuse: (r: 2; g: 2; b: 2; a: 255);
    Ambient: (r: 0; g: 0; b: 0; a: 255);
    Specular: (r: 128; g: 128; b: 128; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 40;
  );

  //Blue plastic
  AdMat_Plastic_Blue: TAd2dMaterial = (
    Diffuse: (r: 2; g: 2; b: 128; a: 255);
    Ambient: (r: 0; g: 0; b: 32; a: 255);
    Specular: (r: 128; g: 128; b: 255; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 20;
  );

  //Green plastic
  AdMat_Plastic_Green: TAd2dMaterial = (
    Diffuse: (r: 2; g: 128; b: 2; a: 255);
    Ambient: (r: 0; g: 32; b: 0; a: 255);
    Specular: (r: 128; g: 255; b: 128; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 20;
  );

  //Red plastic
  AdMat_Plastic_Red: TAd2dMaterial = (
    Diffuse: (r: 128; g: 2; b: 2; a: 255);
    Ambient: (r: 32; g: 0; b: 0; a: 255);
    Specular: (r: 255; g: 0; b: 128; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 40;
  );

  //White plastic
  AdMat_Plastic_White: TAd2dMaterial = (
    Diffuse: (r: 255; g: 255; b: 255; a: 255);
    Ambient: (r: 32; g: 32; b: 32; a: 255);
    Specular: (r: 255; g: 255; b: 255; a: 255);
    Emissive: (r: 0; g: 0; b: 0; a: 255);
    Power: 100;
  );


implementation

end.
