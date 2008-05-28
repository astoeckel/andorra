{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPNStandardFontGenerator.pas
* Comment: The standard font generator class
}

{Contains the standard font generator class, which uses the LCL/VCL}
unit AdStandardFontGenerator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  AdTypes, AdClasses, AdFontGenerator, Graphics, AdBitmap, AdBitmapEffects;

type
  {Properties that are passed as data to the font generator class}
  TAdStandardFontProperties = packed record
    FontName:ShortString; {< Stores the name of the font}
    FontSize:integer; {< The size of the font.}
    FontStyles:TAdFontStyles; {< The style of the font.}
    ShadowColor:longint; {< The shadow color of the font.}
    ShadowAlpha:byte; {< The alpha value of the shadow.}
    ShadowOffsetX:integer; {< The distance of the shadow from the text.}
    ShadowOffsetY:integer; {< The distance of the shadow from the text.}
    ShadowBlur:byte; {< The blur value of the text.}
  end;

  {Pointer on TAdStandardFontProperties}
  PAdStandardFontProperties = ^TAdStandardFontProperties;

  {The standard font generator class, which uses the LCL/VCL to produce a 
   font bitmap that is given to the type setter.}
  TAdStandardFontGenerator = class(TAdFontGenerator)
    public      
      procedure Generate(AData:Pointer;ASize:Cardinal;
        var ASizes:TAdCharSizes; var APatterns: TAdCharPatterns; ATexture:TAd2dBitmapTexture);override;
      function IsValidData(AData:Pointer;ASize:Cardinal):boolean;override;
  end;


implementation

{ TAdStandardFontGenerator }

procedure TAdStandardFontGenerator.Generate(AData: Pointer; ASize: Cardinal;
  var ASizes: TAdCharSizes; var APatterns: TAdCharPatterns; ATexture: TAd2dBitmapTexture);
var
  tmp:PByte;
  data:PAdStandardFontProperties;
  rgb, alpha:TBitmap;
  i,j:integer;
  c:char;
  maxw, maxh, ax, ay, sx, sy, cx, cy:integer;
  shadow:boolean;
  adbmp,adeffect:TAdBitmap;
  alphacolor:longint;
  blur:TAdBitmapBlur;
  p1,p2:PRGBARec;
  p3:PRGBRec;
begin
  tmp := AData;
  inc(tmp, 5);

  data := PAdStandardFontProperties(tmp);
  with data^ do
  begin
    rgb := TBitmap.Create;

    //Set font properties
    with rgb.Canvas.Font do
    begin
      Name := FontName;
      Size := FontSize;
      Color := clWhite;
      Style := [];
      if afItalic in FontStyles then Style := Style + [fsItalic];
      if afBold in FontStyles then Style := Style + [fsBold];
      if afUnderLine in FontStyles then Style := Style + [fsUnderline];
    end;

    //Calculate max char size
    maxw := 0;
    maxh := 0;

    for i := 0 to 255 do
    begin
      ax := rgb.Canvas.TextWidth(chr(i));
      ay := rgb.Canvas.TextHeight(chr(i));
      if ax > maxw then maxw := ax;
      if ay > maxh then maxh := ay;
    end;

    maxw := maxw + abs(ShadowOffsetX) + ShadowBlur + 1;
    maxh := maxh + abs(ShadowOffsetY) + ShadowBlur + 1;

    //Set bitmap size as calculated
    rgb.Width := (maxw) * 16;
    rgb.Height := (maxh) * 16;

    shadow := (ShadowOffsetX <> 0) or (ShadowOffSetY <> 0) or (ShadowBlur <> 0);

    //Prepare alphachannel
    alphacolor := AdTypes.RGB(ShadowAlpha,ShadowAlpha,ShadowAlpha);
    alpha := TBitmap.Create;
    alpha.Width := rgb.Width;
    alpha.Height := rgb.Height;

    alpha.Canvas.Font.Assign(rgb.Canvas.Font);
    alpha.Canvas.Font.Color := alphacolor;

    with rgb.Canvas do
    begin
      alpha.Canvas.Brush.Color := clBlack;
      alpha.Canvas.FillRect(ClipRect);
      alpha.Canvas.Brush.Style := bsClear;

      if ColorToRGB(ShadowColor) = AdTypes.RGB(255,255,255) then
        ShadowColor := AdTypes.RGB(254,254,254);
      Brush.Color := ShadowColor;
      FillRect(ClipRect);
      Brush.Style := bsClear;

      for i := 0 to 15 do
      begin
        for j := 0 to 15 do
        begin
          c := chr(i * 16 + j);
          ax := TextWidth(c) + ShadowBlur;
          ay := TextHeight(c) + ShadowBlur;

          sx := j * maxw;
          sy := i * maxh;

          cx := sx;
          cy := sy;

          if shadow then
          begin
            if ShadowOffsetX < 0 then
              cx := cx - ShadowOffsetX;
            if ShadowOffsetY < 0 then
              cy := cy - ShadowOffsetY;

            ax := ax + abs(ShadowOffsetX);
            ay := ay + abs(ShadowOffsetY);

            alpha.Canvas.Font.Color := alphacolor;
            alpha.Canvas.TextOut(cx + ShadowOffsetX, cy + ShadowOffsetY, c);
          end;

          ASizes[i * 16 + j] := AdPoint(TextWidth(c), TextHeight(c));
          APatterns[i * 16 + j] := AdRect(j*maxw, i*maxh, j*maxw + (maxw + ax) div 2, i*maxh + (maxh + ay) div 2);

          //Alphachannel
          alpha.Canvas.Font.Color := clWhite;
          alpha.Canvas.TextOut(cx, cy, c);
          
          Font.Color := clWhite;
          TextOut(cx, cy, c);
        end;
      end;
    end;

    adbmp := TAdBitmap.Create;
    adbmp.Assign(rgb);
    if ShadowBlur = 0 then
    begin
      adbmp.AssignAlphaChannel(alpha);
    end
    else
    begin
      //Blur alpha channel
      {$IFNDEF FPC}
      adeffect := TAdBitmap.Create;
      adeffect.Assign(alpha);

      blur := TAdBitmapBlur.Create;
      blur.Radius := data^.ShadowBlur;
      blur.AssignEffect(adeffect);
      blur.Free;

      //Copy blured bitmap into the alphachannel
      p1 := adbmp.ScanLine;
      p2 := adeffect.ScanLine;
      rgb.PixelFormat := pf24Bit;
      for i := 0 to adeffect.Height - 1 do
      begin
        p3 := rgb.ScanLine[i];
        for j := 0 to adeffect.Width - 1 do
        begin
          //Keep unblured text
          if (p3^.r = 255) and (p3^.g = 255) and (p3^.b = 255) then
            p1^.a := 255
          else
            p1^.a := (p2^.r + p2^.g + p2^.b) div 3;
          inc(p1); inc(p2); inc(p3);
        end;
      end;
      adeffect.Free;
      {$ENDIF}
    end;

    ATexture.LoadFromBitmap(adbmp, ad32Bit);

    adbmp.Free;

    alpha.Free;
    rgb.Free;
  end;
end;

function TAdStandardFontGenerator.IsValidData(AData: Pointer;
  ASize: Cardinal): boolean;
var
  pss:^TAdVeryShortString;
begin
  pss := AData;
  result := pss^ = 'STDF';
end;

initialization
  RegisterFontGeneratorClass(TAdStandardFontGenerator);

end.
