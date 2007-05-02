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
* File: OGLMain.pas
* Comment: The OpenGL Main unit for Windows and Linux
}

unit OGLMain;

interface

uses AdClasses, Classes, Math,
     {$IFDEF UseComponents}gl, Interfaces, Controls, openglboxcomponent;
     {$ELSE}dglOpenGL, Windows;{$ENDIF}

type
  TOGLApplication = class(TAd2DApplication)
    private
      {$IFDEF UseComponents}
      FBox : TOpenGlBox;
      {$ELSE}
      FDC : HDC;
      FRC : HGLRC;
      FWnd: LongWord;
      {$ENDIF}
      FLastTexture:TAd2dTexture;
    protected
      procedure SetAmbientLight(AValue:TAndorraColor);override;
      procedure SetOptions(AValue:TAdOptions);override;
    public
      function CreateLight:TAd2DLight;override;
      function CreateBitmapTexture:TAd2DBitmapTexture;override;
      //function CreateRenderTargetTexture:TAdRenderTargetTexture;override;
      function CreateMesh:TAd2DMesh;override;
      //procedure SetRenderTarget(ATarget:TAdRenderTargetTexture);override;
      {$IFDEF UseComponents}
      function Initialize(AWnd:TComponent; AOptions:TAdOptions; ADisplay:TAdDisplay):boolean;override;
      {$ELSE}
      function Initialize(AWnd:LongWord; AOptions:TAdOptions; ADisplay:TAdDisplay):boolean;override;
      {$ENDIF}
      procedure Finalize;override;

      procedure Setup2DScene(AWidth,AHeight:integer);override;
      procedure Setup3DScene(AWidth,AHeight:integer;APos,ADir,AUp:TAdVector3);override;
      procedure SetupManualScene(AMatView, AMatProj:TAdMatrix);override;
      procedure GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);override;

      procedure Resize(AWidth,AHeight:integer);override;

      procedure SetTextureFilter(AFilterMode:TAd2DFilterMode;AFilter:TAd2DTextureFilter);override;

      procedure ClearSurface(AColor: TAndorraColor);override;
      procedure BeginScene;override;
      procedure EndScene;override;
      procedure Flip;override;
  end;

  TOGLMesh = class(TAd2DMesh)
    private
      FMatrix:TAdMatrix;
      FParent:TOglApplication;      
    protected
      procedure SetVertices(AVertices:TAdVertexArray);override;
      procedure SetIndex(AIndex:TAdIndexArray);override;
      procedure SetTexture(ATexture:TAd2DTexture);override;
      function GetLoaded:boolean;override;
    public
      procedure SetMatrix(AMatrix:TAdMatrix);override;
      constructor Create(AParent:TOGLApplication);
      destructor Destroy;override;
      procedure Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);override;
      procedure Update;override;
  end;

  TOGLBitmapTexture = class(TAd2DBitmapTexture)
    private
    protected
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TOGLApplication);
      destructor Destroy;override;
      procedure FlushTexture;override;
      procedure LoadFromBitmap(ABmp:TAdBitmap;ABitDepth:byte=32);override;
      procedure SaveToBitmap(ABmp:TAdBitmap);override;
  end;
  
  TOGLLight = class(TAd2DLight)
    private
    public
      constructor Create(AParent:TOGLApplication);
      destructor Destroy;reintroduce;
      procedure Restore;override;
      procedure Enable;override;
      procedure Disable;override;
  end;

implementation

{ TOGLApplication }

function TOGLApplication.CreateMesh: TAd2DMesh;
begin
  result := TOGLMesh.Create(self);
end;

function TOGLApplication.CreateBitmapTexture: TAd2DBitmapTexture;
begin
  result := TOGLBitmapTexture.Create(self);
end;

function TOGLApplication.CreateLight: TAd2DLight;
begin
  result := TOGLLight.Create(self);
end;

{function TOGLApplication.CreateRenderTargetTexture: TAdRenderTargetTexture;
begin

end;    }

{$IFNDEF UseComponents}
function TOGLApplication.Initialize(AWnd: LongWord; AOptions: TAdOptions;
  ADisplay: TAdDisplay):boolean;
{$ELSE}
function TOGLApplication.Initialize(AWnd: TComponent; AOptions: TAdOptions;
  ADisplay: TAdDisplay):boolean;
{$ENDIF}
begin
  result := false;
  WriteLog(ltNone,'Try to init Andorra OpenGL Plugin.');
  if InitOpenGL then
  begin
    FOptions := AOptions;
    FWnd := AWnd;

    FDC := GetDC(AWnd);
    FRC := CreateRenderingContext(FDC,[opDoubleBuffered],32,24,0,0,0,0);
    ActivateRenderingContext(FDC,FRC);

    result := true;

    glEnable(GL_BLEND);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    WriteLog(ltFatalError,'Error while initializing OpenGL');
  end;  
end;

procedure TOGLApplication.Finalize;
begin
  WriteLog(ltNone,'Finalize Andorra OpenGL Plugin');
  {$IFNDEF UseComponents}
  DeactivateRenderingContext;
  DestroyRenderingContext(FRC);
  ReleaseDC(FWnd, FDC);
  {$ELSE}
  {$ENDIF}
end;

procedure TOGLApplication.SetOptions(AValue: TAdOptions);
begin
  //Sets the options (wether Antialiasing or Lights etc. are turned on or off.)
end;

procedure TOGLApplication.SetAmbientLight(AValue: TAndorraColor);
begin
  inherited;
  //Sets the ambient light color
end;

procedure TOGLApplication.Setup2DScene(AWidth, AHeight: integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glViewport(0,0,AWidth,AHeight);
  glOrtho(0,AWidth,AHeight,0,0,128);
  glMatrixMode(GL_MODELVIEW);
end;

procedure TOGLApplication.Setup3DScene(AWidth,AHeight:integer;APos,ADir,AUp:TAdVector3);
begin
  glViewport(0,0,AWidth,AHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective( 45, AWidth / AHeight, 1, abs(apos.z) * 2);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt(APos.x, APos.y, APos.z, ADir.x, ADir.y, ADir.z, AUp.x, AUp.y, AUp.z);
end;

procedure TOGLApplication.SetupManualScene(AMatView, AMatProj:TAdMatrix);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@AMatProj);
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@AMatView);
end;

procedure TOGLApplication.GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @AMatProj);
  glGetFloatv(GL_MODELVIEW_MATRIX, @AMatView);
end;

procedure TOGLApplication.Resize(AWidth,AHeight:integer);
begin

end;

procedure TOGLApplication.SetTextureFilter(AFilterMode:TAd2DFilterMode;AFilter:TAd2DTextureFilter);
begin

end;

{procedure TOGLApplication.SetRenderTarget(ATarget: TAdRenderTargetTexture);
begin
  inherited;

end;}

procedure TOGLApplication.BeginScene;
begin
end;

procedure TOGLApplication.EndScene;
begin
  //Ends the scene and turns all lights off.
end;

procedure TOGLApplication.Flip;
begin
  SwapBuffers(FDC);
end;

procedure TOGLApplication.ClearSurface(AColor: TAndorraColor);
begin
  //glClearColor(AColor.r / 255, AColor.g / 255, AColor.b / 255, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

{ TOGLMesh }

constructor TOGLMesh.Create(AParent: TOGLApplication);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TOGLMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TOGLMesh.Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);
var
  i: integer;
  mode: cardinal;
begin
  if Loaded then
  begin
    with FParent do
    begin
      case ABlendMode of
        bmAlpha: glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
        bmAdd: glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        bmMask: glBlendFunc(GL_ZERO, GL_DST_ALPHA);
      end;

      if (FTexture <> nil) and (FTexture.Loaded) then
      begin
        if (FTexture <> FLastTexture) then
        begin
          FLastTexture := FTexture;
          glBindTexture(GL_TEXTURE_2D,PCardinal(FTexture.Texture)^);
          glEnable(GL_TEXTURE_2D);
        end;
      end
      else
      begin
        FLastTexture := nil;
        glDisable(GL_TEXTURE_2D);
      end;

      case ADrawMode of
        adTriangleStrips: mode := GL_TRIANGLE_STRIP;
        adTriangles: mode := GL_TRIANGLES;
        adLines: mode := GL_LINES;
        adLineStrips: mode := GL_LINE_STRIP;
        adTriangleFan: mode := GL_TRIANGLE_FAN;
        adPoints: mode := GL_POINTS;
      else
        mode := GL_TRIANGLE_STRIP;
      end;

      glPushMatrix;
      glMultMatrixf(@FMatrix);

      glBegin(mode);
      for i := 0 to high(FVertices) do
      begin
        with FVertices[i] do
        begin
          glTexCoord2f(Texture.x,Texture.y);
          glColor4f(Color.r / 255, Color.g / 255, Color.b / 255, Color.a / 255);
          glVertex3f(Position.x,Position.y,Position.z);
        end;
      end;
      glEnd;
      glPopMatrix;
    end;
  end;
end;

function TOGLMesh.GetLoaded: boolean;
begin
  result := length(FVertices) > 0;
end;

procedure TOGLMesh.SetIndex(AIndex: TAdIndexArray);
begin
  //Copys the data from AIndex into FIndices
end;

procedure TOGLMesh.SetMatrix(AMatrix: TAdMatrix);
begin
  FMatrix := AMatrix;
end;

procedure TOGLMesh.SetTexture(ATexture: TAd2DTexture);
begin
  inherited SetTexture(ATexture);
end;

procedure TOGLMesh.SetVertices(AVertices: TAdVertexArray);
begin
  if FVertices <> nil then
  begin
    Finalize(FVertices);
  end;
  FVertices := Copy(AVertices);
end;

procedure TOGLMesh.Update;
begin
  //Nothing to do now.
end;

{ TOGLBitmapTexture }

constructor TOGLBitmapTexture.Create(AParent: TOGLApplication);
begin
  inherited Create;
  FTexture := nil;
end;

destructor TOGLBitmapTexture.Destroy;
begin
  FlushTexture;
  inherited Destroy;
end;

procedure TOGLBitmapTexture.FlushTexture;
begin
  if Loaded then
  begin
    glDeleteTextures(1,FTexture);
    Dispose(PCardinal(FTexture));
    FTexture := nil;
  end;
end;

function TOGLBitmapTexture.GetLoaded: boolean;
begin
  result := FTexture <> nil;
end;

function IsPowerOfTwo(Value: Cardinal): Boolean;
begin
  Result := (Value > 0) and (Value and (Value -1) = 0);
end;

//Convert a 8 Bit Color to a 4 Bit Color
function R8ToR4(r:byte):byte;
begin
  result := (r div 16);
end;

//Converts a A8R8G8B8 Value to a A4R4G4B4
function ABGRTo16Bit(a,b,g,r:byte):Word;
begin
  Result := (R8ToR4(b) shl 12) or (R8ToR4(g) shl 8) or (R8ToR4(r) shl 4) or (R8ToR4(a));
end;

procedure TOGLBitmapTexture.LoadFromBitmap(ABmp: TAdBitmap; ABitDepth: byte);
var
  mem:PByte;
  w,h,x,y:integer;
  pnt32:PRGBARec;
  cur16:PWord;
  cur32:PLongWord;
begin
  FlushTexture;

  new(PCardinal(FTexture));
  glGenTextures(1,FTexture);
  glBindTexture(GL_TEXTURE_2D, PCardinal(FTexture)^);

  w := 1 shl ceil(log2(ABmp.Width));
  h := 1 shl ceil(log2(ABmp.Height));

  FWidth := w;
  FHeight := h;
  FBitCount := ABitDepth;
  FBaseWidth := ABmp.Width;
  FBaseHeight := ABmp.Height;

  GetMem(mem,w*h*ABitDepth div 8);
  try
    if ABitDepth = 32 then
    begin
      cur32 := PLongWord(mem);
      pnt32 := ABmp.ScanLine;
      for y := 0 to ABmp.Height - 1 do
      begin
        for x := 0 to w - 1 do
        begin
          if (x < ABmp.Width) then
          begin
            cur32^ :=  (pnt32^.a shl 24) or (pnt32^.b shl 16) or (pnt32^.g shl 8) or (pnt32^.r);
            inc(pnt32);
          end;
          inc(cur32);
        end;
      end;
      
      glTexImage2D(	GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_BGRA, GL_UNSIGNED_BYTE, mem);
    end
    else
    begin
      cur16 := PWord(mem);
      pnt32 := ABmp.ScanLine;
      for y := 0 to ABmp.Height - 1 do
      begin
        for x := 0 to w - 1 do
        begin
          if (x < ABmp.Width) then
          begin
            cur16^ := ABGRTo16Bit(pnt32^.a,pnt32^.b,pnt32^.g,pnt32^.r);
            inc(pnt32);
          end;
          inc(cur16);
        end;
      end;

      glTexImage2D(	GL_TEXTURE_2D, 0, GL_RGBA16, w, h, 0, GL_RGBA, GL_UNSIGNED_SHORT_4_4_4_4, mem);
    end;

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  finally
    FreeMem(mem,w*h*ABitDepth div 8);
  end;
end;

procedure TOGLBitmapTexture.SaveToBitmap(ABmp: TAdBitmap);
var
  mem, cur1, cur2: PLongWord;
  x,y:integer;
begin
  if Loaded then
  begin
    glBindTexture(GL_TEXTURE_2D, PCardinal(FTexture)^);

    GetMem(mem, FWidth*FHeight*4);
    try
      glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, mem);

      cur1 := mem;
      cur2 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            cur2^ := cur1^;
            inc(cur2);
          end;
          inc(cur1);
        end;
      end;
    finally
      FreeMem(mem,FWidth*FHeight*4);
    end;
  end;
end;

{ TOGLLight }

constructor TOGLLight.Create(AParent:TOGLApplication);
begin
  inherited Create;
end;

destructor TOGLLight.Destroy;
begin
  inherited Destroy;
end;

procedure TOGLLight.Disable;
begin
  //Hides the lightsource.
end;

procedure TOGLLight.Enable;
begin
  //Shows the lightsource. It will automaticly be disabled by TOGLApplication.EndScene
end;

procedure TOGLLight.Restore;
begin
  //Pushs all settings made (position, color, etc.) into the graphic system.
end;

end.
