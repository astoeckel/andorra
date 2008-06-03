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
* Comment: The OpenGL Main unit for Windows and Linux. The OpenGL Initialisation Routine is taken from...
}

unit OGLMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, AdWindowFramework, AdClasses, AdTypes, AdBitmapClass, Math, dglOpenGL
  {$IFDEF WIN32}, Windows{$ENDIF};

type
  TOGLColor = record
    r,g,b,a:byte;
  end;

  TOGLColorArray = array of TOGLColor;
  TOGLVector3Array = array of TAdVector3;
  TOGLVector2Array = array of TAdVector2;

  TOGLWindowType = (wtHandle, wtContext);

  TOGLApplication = class(TAd2DApplication)
    private
      {$IFDEF WIN32}
      FDC : HDC;
      FRC : HGLRC;
      {$ENDIF}
      FWnd: TAdWindowFramework;
      FWindowType:TOGLWindowType;
      FLastTexture:TAd2dTexture;

      FTextures: boolean;
      FMipmaps: boolean;

      FStencilFail: TGLEnum;
      FStencilZFail: TGLEnum;
      FStencilPass: TGLEnum;

      FRenderingToFBO: boolean;
    protected
      procedure SetAmbientColor(AValue: TAndorraColor);override;
      procedure SetViewPort(AValue:TAdRect);override;
      procedure ResetRenderTarget;
    public
      function CreateLight:TAd2DLight;override;
      function CreateBitmapTexture:TAd2DBitmapTexture;override;
      function CreateRenderTargetTexture:TAd2dRenderTargetTexture;override;
      function CreateMesh:TAd2DMesh;override;
      function CreatePixelCounter:TAd2dPixelCounter;override;

      function Initialize(AWnd:TAdWindowFramework):boolean;override;
      procedure Finalize;override;

      procedure SetRenderTarget(ATarget:TAd2dRenderTargetTexture);override;
      procedure SetProperties(ACount: integer; APProps: PAd2dPropertyValue);override;
      procedure SetOptions(AOptions: TAd2dOptions);override;
      procedure SetStencilOptions(AReference, AMask: Word;
        AFunction: TAd2dStencilFunction);override;
      procedure SetStencilEvent(AEvent: TAd2dStencilEvent;
        AOperation: TAd2dStencilOperation);override;  


      procedure Setup2DScene(AWidth, AHeight:integer;
        ANearZ, AFarZ: double);override;
      procedure Setup3DScene(AWidth, AHeight:integer;
        APos, ADir, AUp:TAdVector3; ANearZ, AFarZ: double);override;
      procedure SetupManualScene(AMatView, AMatProj:TAdMatrix);override;
      procedure GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);override;

      function SupportsWindowFramework(AClassId:shortstring):boolean;override;

      procedure ClearSurface(ARect: TAdRect; ALayers: TAd2dSurfaceLayers;
        AColor: TAndorraColor; AZValue: integer; AStencilValue: integer); override;
      procedure BeginScene;override;
      procedure EndScene;override;
      procedure Flip;override;
  end;

  TOGLMesh = class(TAd2DMesh)
    private
      FParent:TOGLApplication;
      FColors:TOGLColorArray;
      FNormals:TOGLVector3Array;
      FTexCoords:TOGLVector2Array;
      FPositions:TOGLVector3Array;
      FMaterial: TAd2dMaterial;
      FUsesMaterial: boolean;
      procedure DivideVertices;
    protected
      procedure SetVertices(AVertices:TAdVertexArray);override;
      procedure SetIndices(AIndices:TAdIndexArray);override;
      procedure SetTexture(ATexture:TAd2DTexture);override;
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TOGLApplication);
      destructor Destroy;override;
      procedure Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);override;
      procedure Update;override;
      procedure SetMaterial(AMaterial: PAd2dMaterial);override;
  end;

  TOGLBitmapTexture = class(TAd2DBitmapTexture)
    private
      FParent: TOGLApplication;
      FHasMipMap: boolean;
      function GetFilter(AFilter: TAd2dTextureFilter): Integer;
      function GetMipFilter(AFilter: TAd2dTextureFilter): Integer;
    protected
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent: TOGLApplication);
      destructor Destroy;override;
      procedure FlushTexture;override;
      procedure LoadFromBitmap(ABmp:TAd2dBitmap; ABitDepth: TAdBitDepth);override;
      procedure SaveToBitmap(ABmp:TAd2dBitmap);override;
      procedure SetFilter;
  end;

  TOGLRenderTargetTexture = class(TAd2dRenderTargetTexture)
    private
      FFBO: GLuint;
      FDepthBuf: GLuint;
    protected
      procedure CheckFBO;
      function GetLoaded:boolean;override;
    public
      constructor Create;
      destructor Destroy;override;

      procedure SetSize(AWidth, AHeight: integer; ABitDepth: TAdBitDepth);override;
      procedure FlushMemory;override;
      procedure SaveToBitmap(ABmp:TAd2dBitmap);override;

      property FBO: GLuint read FFBO; 
  end;

  TOGLPixelCounter = class(TAd2dPixelCounter)
    private
      OGLQuery: Gluint;
    public
      constructor Create;
      destructor Destroy;override;

      procedure StartCount;override;
      procedure StopCount;override;
      function GetCount: Cardinal;override;
  end;
  
implementation

{ TOGLApplication }

function TOGLApplication.CreateMesh: TAd2DMesh;
begin
  result := TOGLMesh.Create(self);
end;

function TOGLApplication.CreatePixelCounter: TAd2dPixelCounter;
begin
  result := TOGLPixelCounter.Create;
end;

function TOGLApplication.CreateBitmapTexture: TAd2DBitmapTexture;
begin
  result := TOGLBitmapTexture.Create(self);
end;

function TOGLApplication.CreateLight: TAd2DLight;
begin
  result := nil;
end;

function TOGLApplication.CreateRenderTargetTexture: TAd2dRenderTargetTexture;
begin
  result := TOGLRenderTargetTexture.Create;
end;

function TOGLApplication.Initialize(AWnd: TAdWindowFramework):boolean;
var
  FHandle:LongInt;
begin
  //Preset a few variales
  FStencilFail := GL_KEEP;
  FStencilZFail := GL_KEEP;
  FStencilPass := GL_KEEP;

  result := false;
  Log('OpenGL', lsInfo, 'Try to init Andorra OpenGL Plugin.');
  if InitOpenGL then
  begin
    //Read the header extensions
    ReadExtensions;

    FWnd := AWnd;

    {$IFDEF WIN32}
    if FWindowType = wtHandle then
    begin
      FHandle := TAdHandleWindowFramework(AWnd).Handle;

      FDC := GetDC(FHandle);
      FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 24, 8, 0, 0, 0);

      FWidth := FWnd.ClientWidth;
      FHeight := FWnd.ClientHeight;

      ActivateRenderingContext(FDC,FRC);
    end else{$ENDIF}
    begin
      ReadImplementationProperties;
    end;    

    result := true;

    glEnable(GL_COLOR_MATERIAL);

    glCullFace(GL_FRONT);

    //Repeat the texture if it wraps over the edges
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );

    FRenderingToFBO := false;
  end
  else
  begin
    Log('OpenGL', lsInfo, 'Error while initializing OpenGL');
  end;
end;

procedure TOGLApplication.Finalize;
begin
  Log('OpenGL', lsInfo, 'Finalize Andorra OpenGL Plugin');
  {$IFDEF WIN32}
  if FWindowType = wtHandle then
  begin
    DeactivateRenderingContext;
    DestroyRenderingContext(FRC);
    ReleaseDC(TAdHandleWindowFramework(FWnd).Handle, FDC);
  end;
  {$ENDIF}
end;

procedure TOGLApplication.SetAmbientColor(AValue: TAndorraColor);
var
  param: array[0..3] of Integer;
begin
  inherited;
  
  param[0] := AValue.r;
  param[1] := AValue.g;
  param[2] := AValue.b;
  param[3] := AValue.a;

  glLightModeliv(GL_LIGHT_MODEL_AMBIENT, @param);
end;

procedure TOGLApplication.SetOptions(AOptions: TAd2dOptions);
begin
  //Blending
  if aoBlending in AOptions then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);

  //Alpha mask
  if aoAlphaMask in AOptions then
    glEnable(GL_ALPHA_TEST)
  else
    glDisable(GL_ALPHA_TEST);    

  //Z-Buffer
  if aoZBuffer in AOptions then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);

  //Light
  if aoLight in AOptions then
    glEnable(GL_LIGHTING)
  else
    glDisable(GL_LIGHTING);

  //Culling
  if aoCulling in AOptions then
  begin
    if FRenderingToFBO then    
      glCullFace(GL_BACK)
    else
      glCullFace(GL_FRONT);
      
    glEnable(GL_CULL_FACE);
  end
  else
    glDisable(GL_CULL_FACE);

  //Stencil
  if aoStencilBuffer in AOptions then
    glEnable(GL_STENCIL_TEST)
  else
    glDisable(GL_STENCIL_TEST);

  //Textures
  FTextures := aoTextures in AOptions;

  //Mipmaps
  FMipmaps := aoMipmaps in AOptions;  
end;

procedure TOGLApplication.SetProperties(ACount: integer;
  APProps: PAd2dPropertyValue);
begin
  //We have no properties
end;

procedure TOGLApplication.SetRenderTarget(ATarget: TAd2dRenderTargetTexture);
begin
  if FRenderingToFBO then
    ResetRenderTarget;

  if ATarget <> nil then
  begin
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, TOGLRenderTargetTexture(ATarget).FBO);
    FRenderingToFBO := true;

    FWidth := ATarget.BaseWidth;
    FHeight := ATarget.BaseHeight;
  end;
end;

procedure TOGLApplication.ResetRenderTarget;
begin
  //Use the default surface now
  if FRenderingToFBO then
  begin
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    FRenderingToFBO := false;

    FWidth := FWnd.ClientWidth;
    FHeight := FWnd.ClientHeight;
  end;
end;

procedure TOGLApplication.SetStencilEvent(AEvent: TAd2dStencilEvent;
  AOperation: TAd2dStencilOperation);
var
  op: TGLEnum;
begin
  //Initialize values
  op := 0;

  //Set operation
  case AOperation of
    asoKeep: op := GL_KEEP;
    asoReplace: op := GL_REPLACE;
    asoIncrement: op := GL_INCR;
    asoDecrase: op := GL_DECR;
    asoZero: op := GL_ZERO;
  end;

  //Set event
  case AEvent of
    aseFail: FStencilFail := op;
    aseZFail: FStencilZFail := op;
    asePass: FStencilPass := op;
  end;

  //Set render states
  glStencilOp(FStencilFail, FStencilZFail, FStencilPass);
end;

procedure TOGLApplication.SetStencilOptions(AReference, AMask: Word;
  AFunction: TAd2dStencilFunction);
var
  func: TGLEnum;
begin
  func := 0;

  //Set the comparison function
  case AFunction of
    asfNever: func := GL_NEVER;
    asfLessThan: func := GL_LESS;
    asfLessThanOrEqual: func := GL_LEQUAL;
    asfEqual: func := GL_EQUAL;
    asfGreaterThanOrEqual: func := GL_GEQUAL;
    asfGreaterThan: func := GL_GREATER;
    asfAlways: func := GL_ALWAYS;
  end;

  //Set reference value and mask
  glStencilFunc(func, AReference, AMask);
end;

procedure TOGLApplication.Setup2DScene(AWidth, AHeight: integer; ANearZ, AFarZ: double);
begin
  glMatrixMode(GL_PROJECTION);

  glLoadIdentity;
  //When rendering to a FBO, the scene is bottom up - so we have to correct this
  if FRenderingToFBO then
    glOrtho(0, AWidth, 0, AHeight, ANearZ, AFarZ)
  else
    glOrtho(0, AWidth, AHeight, 0, ANearZ, AFarZ);   

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TOGLApplication.Setup3DScene(AWidth, AHeight:integer;
  APos, ADir, AUp:TAdVector3; ANearZ, AFarZ: double);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  //When rendering to a FBO, the scene is bottom up - so we have to correct this
  if FRenderingToFBO then
    gluPerspective( 45, -AWidth / AHeight, ANearZ, AFarZ)
  else
    gluPerspective( 45, AWidth / AHeight, ANearZ, AFarZ);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  //When rendering to a FBO, the scene is bottom up - so we have to correct this
  if FRenderingToFBO then
    gluLookAt(APos.x, APos.y, APos.z, ADir.x, ADir.y, ADir.z, -AUp.x, -AUp.y, -AUp.z)
  else
    gluLookAt(APos.x, APos.y, APos.z, ADir.x, ADir.y, ADir.z, AUp.x, AUp.y, AUp.z);
end;

procedure TOGLApplication.SetupManualScene(AMatView, AMatProj:TAdMatrix);
begin
  //Load the projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glLoadMatrixf(@AMatProj);

  //Load the modelview matrix
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glLoadMatrixf(@AMatView);
end;

procedure TOGLApplication.SetViewPort(AValue: TAdRect);
begin
  inherited;

  //Set viewport
  glViewPort(
    AValue.Left,
    FHeight - AValue.Top - (AValue.Bottom-AValue.Top),
    AValue.Right-AValue.Left,
    AValue.Bottom-AValue.Top);
end;

function TOGLApplication.SupportsWindowFramework(AClassId: shortstring): boolean;
begin
  result := false;
  {$IFDEF WIN32}if (Pos('tadhandlewindowframework',lowercase(AClassId)) > 0) then
  begin
    FWindowType := wtHandle;
    result := true;
  end else{$ENDIF}
  if (Pos('tadglcontextgeneratingwindowframework',lowercase(AClassId)) > 0) then
  begin
    FWindowType := wtContext;
    result := true;
  end;
end;

procedure TOGLApplication.GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @AMatProj);
  glGetFloatv(GL_MODELVIEW_MATRIX, @AMatView);
end;

procedure TOGLApplication.BeginScene;
begin
end;

procedure TOGLApplication.EndScene;
begin
  ResetRenderTarget;
end;

procedure TOGLApplication.Flip;
begin
  {$IFDEF WIN32}if FWindowType = wtHandle then
  begin
    SwapBuffers(FDC);
  end else{$ENDIF}
  begin
    TAdGLContextGeneratingWindowFramework(FWnd).Swap;
  end;
end;

procedure TOGLApplication.ClearSurface(ARect: TAdRect; ALayers: TAd2dSurfaceLayers;
  AColor: TAndorraColor; AZValue: integer; AStencilValue: integer);
var
  mask: Cardinal;
begin
  glClearColor(AColor.r / 255, AColor.g / 255, AColor.b / 255, AColor.a / 255);
  glClearDepth(AZValue);
  glClearStencil(AStencilValue);

  //Set flags for clearing the surface
  mask := 0;

  if alColorBuffer in ALayers then
    mask := mask or GL_COLOR_BUFFER_BIT;
  if alStencilBuffer in ALayers then
    mask := mask or GL_STENCIL_BUFFER_BIT;
  if alZBuffer in ALayers then
    mask := mask or GL_DEPTH_BUFFER_BIT;

  glEnable(GL_SCISSOR_TEST);

  glScissor(
    ARect.Left, ARect.Top,
    ARect.Right - ARect.Left,
    ARect.Bottom - ARect.Top);

  glClear(mask);

  if FRenderingToFBO then
  begin
    glBegin(GL_QUADS);

    glColor3f(AColor.r / 255, AColor.g / 255, AColor.b / 255);

    glVertex3f(ARect.Left, ARect.Top, 0);
    glVertex3f(ARect.Right, ARect.Top,0);
    glVertex3f(ARect.Right, ARect.Bottom ,0);
    glVertex3f(ARect.Left, ARect.Bottom ,0);

    glEnd;
  end;

  glDisable(GL_SCISSOR_TEST);
end;

{ TOGLMesh }

constructor TOGLMesh.Create(AParent: TOGLApplication);
begin
  inherited Create;
  FParent := AParent;
  FMatrix := AdMatrix_Identity;
  FTextureMatrix := AdMatrix_Identity;
end;

destructor TOGLMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TOGLMesh.Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);
var
  i: integer;
  mode: cardinal;
  mat: TAdMatrix;
begin
  if Loaded then
  begin
    with FParent do
    begin
      case ABlendMode of
        bmAlpha: glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        bmAdd: glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        bmMask: glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);
      end;

      if (FTexture <> nil) and (FTexture.Loaded) and (FParent.FTextures) then
      begin
        if (FTexture <> FLastTexture) then
        begin
          FLastTexture := FTexture;
          glBindTexture(GL_TEXTURE_2D,PCardinal(FTexture.Texture)^);
        end;
        if FTexture is TOGLBitmapTexture then
          TOGLBitmapTexture(FTexture).SetFilter;

        glEnable(GL_TEXTURE_2D);

        //Set texture matrix
        glMatrixMode(GL_TEXTURE);
        mat := FTextureMatrix;
        mat[3, 0] := mat[2, 0];
        mat[3, 1] := mat[2, 1];
        glLoadMatrixf(@mat);
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

      //Set transformation matrix
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glMultMatrixf(@FMatrix);

      if FIndices = nil then
      begin
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
      end
      else
      begin
	      glEnableClientState(GL_COLOR_ARRAY);
        glEnableClientState(GL_NORMAL_ARRAY);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glEnableClientState(GL_VERTEX_ARRAY);

        glColorPointer(4,GL_UNSIGNED_BYTE,0,@FColors[0]);
        glTexCoordPointer(2,GL_FLOAT,0,@FTexCoords[0]);
        glNormalPointer(GL_FLOAT,0,@FNormals[0]);
        glVertexPointer(3,GL_FLOAT,0,@FPositions[0]);

        glDrawElements(mode,high(FIndices)+1,GL_UNSIGNED_SHORT,@FIndices[0]);

	      glDisableClientState(GL_COLOR_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
      end;
      glPopMatrix;
    end;
  end;
end;

function TOGLMesh.GetLoaded: boolean;
begin
  result := length(FVertices) > 0;
end;

procedure TOGLMesh.SetIndices(AIndices: TAdIndexArray);
begin
  if FIndices <> nil then
  begin
    Finalize(FIndices);
  end;
  FIndices := Copy(AIndices);
  DivideVertices;
end;

procedure TOGLMesh.SetMaterial(AMaterial: PAd2dMaterial);
begin
  if AMaterial <> nil then
  begin
    FMaterial := AMaterial^;
    FUsesMaterial := false;
  end
  else
    FUsesMaterial := true;
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
  DivideVertices;
end;

procedure TOGLMesh.DivideVertices;
var
  i:integer;
begin
  if FColors <> nil then
  begin
    Finalize(FColors);
  end;
  if FNormals <> nil then
  begin
    Finalize(FNormals);
  end;
  if FTexCoords <> nil then
  begin
    Finalize(FTexCoords);
  end;
  if FPositions <> nil then
  begin
    Finalize(FPositions);
  end;

  if (FVertices <> nil) and (FIndices <> nil) then
  begin
    SetLength(FColors,length(FVertices));
    SetLength(FNormals,length(FVertices));
    SetLength(FTexCoords,length(FVertices));
    SetLength(FPositions,length(FVertices));

    for i := 0 to high(FVertices) do
    begin
      FColors[i].r := FVertices[i].Color.r;
      FColors[i].g := FVertices[i].Color.g;
      FColors[i].b := FVertices[i].Color.b;
      FColors[i].a := FVertices[i].Color.a;

      FNormals[i] := FVertices[i].Normal;
      FPositions[i] := FVertices[i].Position;
      FTexCoords[i] := FVertices[i].Texture;
    end;
  end;
end;


procedure TOGLMesh.Update;
begin
  //Nothing to do
end;

{ TOGLBitmapTexture }

constructor TOGLBitmapTexture.Create(AParent: TOGLApplication);
begin
  inherited Create;
  FTexture := nil;
  FParent := AParent;
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

function TOGLBitmapTexture.GetFilter(AFilter: TAd2dTextureFilter): Integer;
begin
  result := GL_NEAREST;
  case AFilter of
    atLinear: result := GL_LINEAR;
    atAnisotropic: result := GL_LINEAR;
  end;
end;

function TOGLBitmapTexture.GetMipFilter(AFilter: TAd2dTextureFilter): Integer;
begin
  result := GL_NEAREST_MIPMAP_LINEAR;
  case AFilter of
    atLinear: result := GL_LINEAR_MIPMAP_LINEAR;
    atAnisotropic: result := GL_LINEAR_MIPMAP_LINEAR;
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

procedure TOGLBitmapTexture.LoadFromBitmap(ABmp: TAd2dBitmap; ABitDepth: TAdBitDepth);
var
  mem:PByte;
  w,h,x,y:integer;
  pnt32:PRGBARec;
  cur16:PWord;
  cur32:PLongWord;
  newtex: boolean;
begin
  w := 1 shl ceil(log2(ABmp.Width));
  h := 1 shl ceil(log2(ABmp.Height));

  if (w <> FWidth) or (h <> FHeight) or
     (ABitDepth <> FBitDepth) or (not Loaded) then
  begin
    FlushTexture;
    new(PCardinal(FTexture));
    glGenTextures(1,FTexture);
    newtex := true;
  end else
    newtex := false;

  glBindTexture(GL_TEXTURE_2D, PCardinal(FTexture)^);

  FWidth := w;
  FHeight := h;
  FBitDepth := ABitDepth;
  FBaseWidth := ABmp.Width;
  FBaseHeight := ABmp.Height;
  FHasMipMap := false;
  
  GetMem(mem,w *h * Ord(ABitDepth) div 8);
  
  try
    if ABitDepth = ad32Bit then
    begin
      if newtex or FParent.FMipmaps then
      begin
        cur32 := PLongWord(mem);
        pnt32 := ABmp.ScanLine;
        for y := 0 to ABmp.Height - 1 do
        begin
          Move(pnt32^, cur32^, ABmp.Width * 4);
          inc(pnt32, ABmp.Width);
          inc(cur32, w);
        end;
      end;

      if FParent.FMipmaps then
      begin
        FHasMipMap := true;
        gluBuild2DMipmaps(GL_TEXTURE_2D, 4, w, h, GL_BGRA, GL_UNSIGNED_BYTE, mem)
      end
      else
      begin
        if newtex then
        begin
          glTexImage2D(
            GL_TEXTURE_2D, 0, GL_RGBA,
            w, h,
            0, GL_BGRA, GL_UNSIGNED_BYTE, mem);
        end else
        begin
          glTexSubImage2D(
            GL_TEXTURE_2D, 0,
            0, 0, BaseWidth, BaseHeight,
            GL_BGRA, GL_UNSIGNED_BYTE, ABmp.ScanLine)
        end;
      end;
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

  finally
    FreeMem(mem,w*h* Ord(ABitDepth) div 8);
  end;
end;

procedure TOGLBitmapTexture.SaveToBitmap(ABmp: TAd2dBitmap);
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

procedure TOGLBitmapTexture.SetFilter;
begin
  if FHasMipMap then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GetMipFilter(Filter));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GetMipFilter(Filter));
  end else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GetFilter(Filter));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GetFilter(Filter));
  end;
end;

{ TOGLRenderTargetTexture }

procedure TOGLRenderTargetTexture.CheckFBO;
var
  error: GlEnum;
begin
  error := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
  case error of
    GL_FRAMEBUFFER_COMPLETE_EXT:
      Exit;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT:
      raise Exception.Create('Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
      raise Exception.Create('Missing attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      raise Exception.Create('Incomplete dimensions');
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      raise Exception.Create('Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
      raise Exception.Create('Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
      raise Exception.Create('Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED_EXT:
      raise Exception.Create('Framebufferobjects unsupported');
  end;
end;

constructor TOGLRenderTargetTexture.Create;
begin
  inherited Create;

  FEditable := false;
end;

destructor TOGLRenderTargetTexture.Destroy;
begin
  FlushMemory;  
  inherited;
end;

procedure TOGLRenderTargetTexture.FlushMemory;
begin
  if Loaded then
  begin
    glDeleteFramebuffersEXT(1, @FFBO);
    glDeleteRenderbuffersEXT(1, @FDepthBuf);
    glDeleteTextures(1, FTexture);
    
    Dispose(PCardinal(FTexture));
    FTexture := nil;
  end;
end;

function TOGLRenderTargetTexture.GetLoaded: boolean;
begin
  result := FTexture <> nil;
end;

procedure TOGLRenderTargetTexture.SaveToBitmap(ABmp: TAd2dBitmap);
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

procedure TOGLRenderTargetTexture.SetSize(AWidth, AHeight: integer;
  ABitDepth: TAdBitDepth);
var
  w, h: integer;
begin
  w := 1 shl ceil(log2(AWidth));
  h := 1 shl ceil(log2(AHeight));
  if (not Loaded) or (w <> FWidth) or (h <> FHeight) or (ABitDepth <> FBitDepth) then
  begin
    FlushMemory;

    //Create framebuffer
    glGenFramebuffersEXT(1, @FFBO);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFBO);

    //Create texture
    New(PCardinal(FTexture));
    glGenTextures(1, FTexture);
    glBindTexture(GL_TEXTURE_2D, PCardinal(FTexture)^);
    
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,  w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );

    //Connect the texture to framebuffer
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT,
      GL_TEXTURE_2D, PCardinal(FTexture)^, 0);

    //Create renderbuffer
    glGenRenderbuffersEXT(1, @FDepthBuf);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuf);

    //Reserve memory for the renderbuffer
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, w, h);

    //Connect renderbuffer to the framebuffer
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, FDepthBuf);

    CheckFBO;

    FWidth := w;
    FHeight := h;
    FBitDepth := ABitDepth;
  end;

  FBaseWidth := AWidth;
  FBaseHeight := AHeight;
end;

{ TDXPixelCounter }

constructor TOGLPixelCounter.Create;
begin
  inherited;
  glGenQueries(1, @OGLQuery);
end;

destructor TOGLPixelCounter.Destroy;
begin
  glDeleteQueries(1, @OGLQuery);
  inherited;
end;

function TOGLPixelCounter.GetCount: Cardinal;
begin
  glGetQueryObjectivARB(OGLQuery, GL_QUERY_RESULT_ARB, @result);
end;

procedure TOGLPixelCounter.StartCount;
begin
  glBeginQueryARB(GL_SAMPLES_PASSED_ARB, OGLQuery);
end;

procedure TOGLPixelCounter.StopCount;
begin
  glEndQuery(OGLQuery);
end;

end.
