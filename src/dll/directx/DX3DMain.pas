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
* File: DX3DMain.pas
* Comment: The Direct 3D DLL unit
}

unit DX3DMain;

interface

uses                 
  SysUtils, AdWindowFramework, d3dx9, Direct3D9, AdClasses, Windows, Math,
  AdTypes, AdBitmapClass;

type
  TDXApplication = class(TAd2DApplication)
    private
      FLastTexture:TAd2DTexture;
      FUsesMaterial:Cardinal;
      FPresent:TD3DPresentParameters;
      FOwnRenderTarget: IDirect3dSurface9;
      FSetToOwnRenderTarget: boolean;
      FWnd: TAdWindowFramework;

      FFullscreen: boolean;
      FResolution: TAd2dResolution;
      FAntialias: boolean;
      FVSync: boolean;
      FTextures: boolean;
      FMipmaps: boolean;
    protected
      procedure WriteLog(ALogSeverity: TAd2dLogSeverity; AMessage: string);
      procedure SetViewPort(AValue:TAdRect);override;
      procedure SetAmbientColor(AValue: TAndorraColor);override;
      procedure ResetRenderTarget;
    public
      Direct3D9:IDirect3D9;
      Direct3DDevice9:IDirect3DDevice9;
      CanAutoGenMipmaps:boolean;
      
      constructor Create;
      destructor Destroy;reintroduce;
      function CreateBitmapTexture:TAd2DBitmapTexture;override;
      function CreateRenderTargetTexture:TAd2dRenderTargetTexture;override;
      function CreateMesh:TAd2DMesh;override;
      function CreatePixelCounter:TAd2dPixelCounter;override;
      function CreateLight: TAd2dLight;override;
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

      function SupportsWindowFramework(AClassId:ShortString):boolean;override;

      procedure ClearSurface(ARect: TAdRect; ALayers: TAd2dSurfaceLayers;
        AColor: TAndorraColor; AZValue: double; AStencilValue: integer); override;
      procedure BeginScene;override;
      procedure EndScene;override;
      procedure Flip;override;
  end;

  TDXMesh = class(TAd2DMesh)
    private
      FVertexBuffer:IDirect3DVertexBuffer9;
      FIndexBuffer:IDirect3DIndexBuffer9;
      FParent:TDXApplication;
      FMaterial: TD3DMaterial9;
      FUsesMaterial: boolean;
      procedure FreeBuffers;
      procedure PushMaterial;
    protected
      procedure SetVertices(AVertices:TAdVertexArray);override;
      procedure SetIndices(AIndex:TAdIndexArray);override;
      procedure SetTexture(ATexture:TAd2DTexture);override;
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TDXApplication);
      destructor Destroy;override;
      procedure Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);override;
      procedure Update;override;
      procedure SetMaterial(AMaterial: PAd2dMaterial);override;
  end;

  TDXBitmapTexture = class(TAd2DBitmapTexture)
    private
      FParent:TDXApplication;
      FHasMipmap: boolean;
    protected
      function GetLoaded:boolean;override;
    public
      procedure SetFilter;
      constructor Create(AParent:TDXApplication);
      destructor Destroy;override;
      procedure FlushTexture;override;
      procedure LoadFromBitmap(ABmp:TAd2dBitmap; ABitDepth: TAdBitDepth);override;
      procedure SaveToBitmap(ABmp:TAd2dBitmap);override;
  end;

  TDXRenderTargetTexture = class(TAd2dRenderTargetTexture)
    private
      FParent:TDXApplication;
    protected
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TDXApplication);
      destructor Destroy;override;

      procedure SetSize(AWidth, AHeight: integer; ABitDepth: TAdBitDepth);override;
      procedure FlushMemory;override;
      procedure SaveToBitmap(ABmp:TAd2dBitmap);override;
  end;

  TDXPixelCounter = class(TAd2dPixelCounter)
    private
      FParent: TDXApplication;
      Direct3DQuery: IDirect3DQuery9;
    public
      constructor Create(AParent: TDXApplication);
      destructor Destroy;override;
      
      procedure StartCount;override;
      procedure StopCount;override;
      function GetCount: Cardinal;override;
  end;

  TDXLight = class(TAd2dLight)
    private
      FParent: TDXApplication;
      FLightNr: Cardinal;
      FLight: TD3DLight9;
    protected
      procedure SetData(AValue: TAd2dLightData);override;
    public
      constructor Create(AParent: TDXApplication);
      procedure EnableLight(ALight: Cardinal);override;
      procedure DisableLight;override;
  end;


//Our Vertex and the definition of the flexible vertex format (FVF)
type TD3DLVertex = record
  position: TD3DXVector3;
  normale: TD3DXVector3;
  diffuse: TD3DColor;
  textur1: TD3DXVector2;
end;

const
  D3DFVF_TD3DLVertex = D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_DIFFUSE or D3DFVF_TEX1;

implementation

function ConvertColor(ACol: TAndorraColor): TD3DCOLORVALUE;
begin
  with result do
  begin
    r := ACol.r / 255;
    g := ACol.g / 255;
    b := ACol.b / 255;
    a := ACol.a / 255;
  end;
end;

{ TDXApplication }

constructor TDXApplication.Create;
begin
  inherited;
  //Create Direct 3D Interface
  Direct3D9 := Direct3DCreate9( D3D_SDK_VERSION );

  //Make some presets
  FFullscreen := false;
  FAntialias := false;
  FVSync := false;
  FMipmaps := false;
  FTextures := true;
  FUsesMaterial := High(Cardinal);
end;

destructor TDXApplication.Destroy;
begin
  Direct3D9 := nil;
  Direct3DDevice9 := nil;
  inherited;
end;

function TDXApplication.CreateMesh: TAd2DMesh;
begin
  result := TDXMesh.Create(self);
end;

function TDXApplication.CreatePixelCounter: TAd2dPixelCounter;
begin
  result := TDXPixelCounter.Create(self);
end;

function TDXApplication.CreateRenderTargetTexture: TAd2dRenderTargetTexture;
begin
  result := TDXRenderTargetTexture.Create(self);
end;

function TDXApplication.CreateBitmapTexture: TAd2DBitmapTexture;
begin
  result := TDXBitmapTexture.Create(self);
end;

function TDXApplication.CreateLight: TAd2dLight;
begin
  result := TDXLight.Create(self);
end;

function TDXApplication.Initialize(AWnd: TAdWindowFramework):boolean;
var
  d3dpp : TD3DPresent_Parameters;
  d3ddm : TD3DDisplayMode;
  dtype : TD3DDevType;
  afmt : D3DFORMAT;
  D3DCaps9 : TD3DCaps9;
  hw : boolean;
  vp : integer;
  hr : HRESULT;
  level:Integer;
begin
  result := false;
  if Direct3D9 <> nil then
  begin
    FWnd := AWnd;

    dtype := D3DDEVTYPE_HAL;

    if Failed(Direct3D9.GetDeviceCaps(D3DADAPTER_DEFAULT, dtype, D3DCaps9)) then
    begin
      WriteLog(lsFatalError,'No connection to the default device.');
    end;
    hw := D3DCaps9.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT <> 0;
    if hw then
    begin
      vp := D3DCREATE_HARDWARE_VERTEXPROCESSING;
    end
    else
    begin
      vp := D3DCREATE_SOFTWARE_VERTEXPROCESSING;
      WriteLog(lsWarning,'The current device does not support "HARDWARE TRANSFORM AND LIGHT".');
    end;

    Fillchar(d3dpp, sizeof(d3dpp),0);
    with d3dpp do
    begin
      Windowed := not FFullscreen;
      SwapEffect := D3DSWAPEFFECT_DISCARD;
      if not FVSync then
      begin
        PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      end;
      if not Windowed then
      begin
        BackBufferWidth := FResolution.Width;
        BackBufferHeight := FResolution.Height;
        if FResolution.Freq > 0 then
        begin
          Fullscreen_RefreshRateInHz := FResolution.Freq;
        end;
        case FResolution.BitDepth of
          ad16Bit: afmt := D3DFMT_X1R5G5B5;
          ad32Bit: afmt := D3DFMT_X8R8G8B8;
        else
          afmt := D3DFMT_X8R8G8B8;
        end;
        if failed(Direct3D9.CheckDeviceType(D3DADAPTER_DEFAULT, dtype, afmt, afmt, false)) then
        begin
          WriteLog(lsWarning, 'The current device settings may be unsupportet.');
          WriteLog(lsInfo, 'Try to use other modes.');
          case FResolution.BitDepth of
            ad16Bit: afmt := D3DFMT_R5G6B5;
            ad32Bit: afmt := D3DFMT_A8R8G8B8;
          else
            afmt := D3DFMT_A8R8G8B8;
          end;
        end;
      end
      else
      begin
        if Failed(Direct3D9.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm)) then
        begin
          WriteLog(lsFatalError, 'Can not access current display settings. Try to run in fullscreen mode.');
          exit;
        end;
        afmt := d3ddm.Format;
      end;
      if failed(Direct3D9.CheckDeviceType(D3DADAPTER_DEFAULT, dtype, afmt, afmt, false)) then
      begin
        WriteLog(lsFatalError, 'The current device settings are unsupportet. Try another adapter mode.');
        exit;
      end;
      BackBufferFormat := afmt;

      EnableAutoDepthStencil := true;
      AutoDepthStencilFormat := D3DFMT_D24S8;

      if FAntialias then
      begin
        MultisampleType := D3DMULTISAMPLE_NONMASKABLE;
        Direct3D9.CheckDeviceMultiSampleType(D3DADAPTER_DEFAULT, dtype, afmt, Windowed, MultisampleType,  @level);
        MultisampleQuality := level - 1;
      end;
    end;

    CanAutoGenMipMaps :=
      (D3DCaps9.Caps2 and D3DCAPS2_CANAUTOGENMIPMAP > 0) and
      (Direct3D9.CheckDeviceFormat(D3DADAPTER_DEFAULT, dtype, afmt, D3DUSAGE_AUTOGENMIPMAP, D3DRTYPE_TEXTURE, afmt) = D3D_OK);
    if not CanAutoGenMipMaps then
    begin
      WriteLog(lsInfo, 'Device can not create mipmaps. Mipmaps will be disabled.');
    end;

    WriteLog(lsInfo, 'Try to initialize the device.');

    FPresent := d3dpp;

    //Create device
    hr := Direct3D9.CreateDevice(D3DADAPTER_DEFAULT,  dtype,
      TAdHandleWindowFramework(AWnd).Handle, vp, @d3dpp, Direct3DDevice9);
    if Failed(hr) then
    begin
      WriteLog(lsFatalError, 'Couldn''t initialize Direct3DDevice! ');
      exit;
    end;

    FWidth := FWnd.ClientWidth;
    FHeight := FWnd.ClientHeight;

    //Set lighting
    Direct3DDevice9.SetRenderState(D3DRS_AMBIENT, $00FFFFFF);

    //Get the number of lights
    FMaxLightCount := d3dcaps9.MaxActiveLights;
    WriteLog(lsInfo, 'Device supports '+inttostr(MaxLights)+' lights');


    WriteLog(lsInfo, Inttostr(Direct3DDevice9.GetAvailableTextureMem div 1024 div 1024)+'MB Texture Memory on this device.');

    //Enable Texture alphablending
    Direct3DDevice9.SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);

    //Make presets for the "Alpha Mask" option
    Direct3DDevice9.SetRenderState(D3DRS_ALPHAREF, 0);
    Direct3DDevice9.SetRenderState(D3DRS_ALPHAFUNC, D3DCMP_GREATER);

    //Enable texture transformation
    Direct3DDevice9.SetTextureStageState(0, D3DTSS_TEXTURETRANSFORMFLAGS, D3DTTFF_COUNT2);

    FSetToOwnRenderTarget := true;

    WriteLog(lsInfo,'Initialization complete.');
    result := true;
  end
  else
  begin
    WriteLog(lsFatalError,'Error while creating DirectX Interface. Please check ' +
     'whether you have the right DirectX Version (9c) installed.');
  end;
end;

procedure TDXApplication.ResetRenderTarget;
begin
  if (not FSetToOwnRenderTarget) and (FOwnRenderTarget <> nil) then
  begin
    Direct3dDevice9.SetRenderTarget(0, FOwnRenderTarget);
    FSetToOwnRenderTarget := true;
    FWidth := FWnd.ClientWidth;
    FHeight := FWnd.ClientHeight;
  end;
end;

procedure TDXApplication.Finalize;
begin
  Direct3d9 := nil;
  Direct3dDevice9 := nil;
  //WriteLog(ltInfo,'Finalization Complete.');
end;

procedure TDXApplication.SetAmbientColor(AValue: TAndorraColor);
begin
  inherited;
  Direct3DDevice9.SetRenderState(D3DRS_AMBIENT,
    D3DColor_ARGB(AValue.a,AValue.r,AValue.g,AValue.b));
end;

procedure TDXApplication.SetOptions(AOptions: TAd2dOptions);
begin
  //Blending
  Direct3DDevice9.SetRenderState(D3DRS_ALPHABLENDENABLE, LongWord(aoBlending in AOptions));

  //Alpha-Mask
  Direct3DDevice9.SetRenderState(D3DRS_ALPHATESTENABLE, LongWord(aoAlphaMask in AOptions));

  //Z-Buffer
  Direct3DDevice9.SetRenderState(D3DRS_ZENABLE, LongWord(aoZBuffer in AOptions));
  
  //Light
  Direct3DDevice9.SetRenderState(D3DRS_LIGHTING, LongWord(aoLight in AOptions));

  //Culling
  if not (aoCulling in AOptions) then
    Direct3DDevice9.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE)
  else
    Direct3DDevice9.SetRenderState(D3DRS_CULLMODE, D3DCULL_CCW);

  //Stencil
  Direct3DDevice9.SetRenderState(D3DRS_STENCILENABLE, LongWord(aoStencilBuffer in AOptions));

  //Textures
  FTextures := aoTextures in AOptions;

  //Mipmaps
  FMipmaps := aoMipmaps in AOptions;
end;

procedure TDXApplication.SetStencilOptions(AReference, AMask: Word;
  AFunction: TAd2dStencilFunction);
var
  func: TD3DCMPFUNC;
begin
  func := 0;

  //Set the comparison function
  case AFunction of
    asfNever: func := D3DCMP_NEVER;
    asfLessThan: func := D3DCMP_LESS;
    asfLessThanOrEqual: func := D3DCMP_LESSEQUAL;
    asfEqual: func := D3DCMP_EQUAL;
    asfGreaterThanOrEqual: func := D3DCMP_GREATEREQUAL;
    asfGreaterThan: func := D3DCMP_GREATER;
    asfAlways: func := D3DCMP_ALWAYS;
  end;

  Direct3DDevice9.SetRenderState(D3DRS_STENCILFUNC, func);

  //Set reference value and mask
  Direct3DDevice9.SetRenderState(D3DRS_STENCILREF, AReference);
  Direct3DDevice9.SetRenderState(D3DRS_STENCILMASK, AMask);
end;

procedure TDXApplication.SetStencilEvent(AEvent: TAd2dStencilEvent;
  AOperation: TAd2dStencilOperation);
var
  op: D3DSTENCILOP;
  state: D3DRENDERSTATETYPE;
begin
  //Initialize values
  op := 0;
  state := D3DRS_STENCILFAIL;

  //Set operation
  case AOperation of
    asoKeep: op := D3DSTENCILOP_KEEP;
    asoReplace: op := D3DSTENCILOP_REPLACE;
    asoIncrement: op := D3DSTENCILOP_INCR;
    asoDecrase: op := D3DSTENCILOP_DECR;
    asoZero: op := D3DSTENCILOP_ZERO;
  end;

  //Set event
  case AEvent of
    aseFail: state := D3DRS_STENCILFAIL;
    aseZFail: state := D3DRS_STENCILZFAIL;
    asePass: state := D3DRS_STENCILPASS;
  end;

  //Set render states
  Direct3DDevice9.SetRenderState(state, op)
end;

procedure TDXApplication.SetProperties(ACount: integer;
  APProps: PAd2dPropertyValue);
var
  i: integer;
begin
  for i := 0 to ACount - 1 do
  begin
    if APProps^.PropName = 'fullscreen' then
      FFullscreen := PBoolean(APProps^.PropValue)^
    else if APProps^.PropName = 'fullscreen_res' then
      FResolution := PAd2dResolution(APProps^.PropValue)^
    else if APProps^.PropName = 'antialias' then
      FAntialias := PBoolean(APProps^.PropValue)^
    else if APProps^.PropName = 'vsync' then
      FVSync := PBoolean(APProps^.PropValue)^;
    inc(APProps);
  end;
end;

procedure TDXApplication.SetRenderTarget(ATarget: TAd2dRenderTargetTexture);
var
  tex_surf: IDirect3DSurface9;
begin
  if ATarget <> nil then
  begin
   if FSetToOwnRenderTarget then
      Direct3DDevice9.GetRenderTarget(0, FOwnRenderTarget);

    FSetToOwnRenderTarget := false;
    IDirect3DTexture9(ATarget.Texture).GetSurfaceLevel(0, tex_surf);
    Direct3DDevice9.SetRenderTarget(0, tex_surf);

    FWidth := ATarget.BaseWidth;
    FHeight := ATarget.BaseHeight;
  end else
  begin
    ResetRenderTarget;
  end;
end;

procedure TDXApplication.Setup2DScene(AWidth, AHeight: integer;
  ANearZ, AFarZ: double);
var
  pos, dir, up : TD3DXVector3;
  matView, matProj: TD3DXMatrix;
begin
  if Direct3DDevice9 <> nil then
  begin
    pos := D3DXVector3 (Awidth/2,AHeight/2,-10);
    dir := D3DXVector3 (Awidth/2,AHeight/2,0);
    up := D3DXVector3 (0,-1,0);

    D3DXMatrixLookAtRH( matView, pos, dir, up);
    Direct3dDevice9.SetTransform(D3DTS_VIEW, matView);

    D3DXMatrixOrthoRH( matProj, Awidth, Aheight, ANearZ, AFarZ);
    Direct3dDevice9.SetTransform(D3DTS_PROJECTION, matProj);
  end;
end;

procedure TDXApplication.Setup3DScene(AWidth, AHeight:integer;
  APos, ADir, AUp:TAdVector3; ANearZ, AFarZ: double);
var
  matView, matProj: TD3DXMatrix;
begin
  if Direct3DDevice9 <> nil then
  begin
    D3DXMatrixLookAtRH( matView, TD3DVector(APos), TD3DVector(ADir), TD3DVector(AUp));
    Direct3dDevice9.SetTransform(D3DTS_VIEW, matView);

    D3DXMatrixPerspectiveFovRH( matProj, D3DX_PI / 4, AWidth / AHeight, ANearZ, AFarZ);
    Direct3dDevice9.SetTransform(D3DTS_PROJECTION, matProj);
  end;
end;

procedure TDXApplication.SetupManualScene(AMatView, AMatProj: TAdMatrix);
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3dDevice9.SetTransform(D3DTS_VIEW, TD3DMatrix(AMatView));
    Direct3dDevice9.SetTransform(D3DTS_PROJECTION, TD3DMatrix(AMatProj));
  end;
end;

procedure TDXApplication.SetViewPort(AValue: TAdRect);
var
  vp:TD3DViewport9;
begin
  inherited;
  vp.X := AValue.Left;
  vp.Y := AValue.Top;
  vp.Width := AValue.Right - AValue.Left;
  vp.Height := AValue.Bottom - AValue.Top;
  vp.MinZ := 0;
  vp.MaxZ := 1;

  direct3ddevice9.SetViewport(vp);
end;

function TDXApplication.SupportsWindowFramework(
  AClassId:ShortString): boolean;
begin
  result :=
    Pos('tadhandlewindowframework',lowercase(AClassId)) > 0;
end;

procedure TDXApplication.WriteLog(ALogSeverity: TAd2dLogSeverity;
  AMessage: string);
begin
  Log('Andorra Direct3D 9', ALogSeverity, PChar(AMessage)); 
end;

procedure TDXApplication.BeginScene;
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.BeginScene;
  end;
end;

procedure TDXApplication.EndScene;
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.EndScene;
    ResetRenderTarget;
  end;
end;

procedure TDXApplication.Flip;
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.Present(nil, nil, 0, nil);
  end;
end;

procedure TDXApplication.GetScene(out AMatView, AMatProj: TAdMatrix);
begin
  Direct3DDevice9.GetTransform(D3DTS_PROJECTION, TD3DMatrix(AMatProj));
  Direct3DDevice9.GetTransform(D3DTS_VIEW, TD3DMatrix(AMatView));
end;

procedure TDXApplication.ClearSurface(ARect: TAdRect; ALayers: TAd2dSurfaceLayers;
  AColor: TAndorraColor; AZValue: double; AStencilValue: integer);
var
  flags: Cardinal;
begin
  if Direct3DDevice9 <> nil then
  begin
    //Set flags for clearing the surface
    flags := 0;

    if alColorBuffer in ALayers then
      flags := flags or D3DCLEAR_TARGET;
    if alStencilBuffer in ALayers then
      flags := flags or D3DCLEAR_STENCIL;
    if alZBuffer in ALayers then
      flags := flags or D3DCLEAR_ZBUFFER;

    //Clear the surface
    Direct3DDevice9.Clear(
      1, @ARect, flags, D3DCOLOR_ARGB(AColor.a,AColor.r,AColor.g,AColor.b),
      AZValue, AStencilValue);
  end;
end;

{ TDXMesh }

constructor TDXMesh.Create(AParent: TDXApplication);
begin
  inherited Create;
  FParent := AParent;
  FIndices := nil;
  FVertices := nil;
  FMatrix := AdMatrix_Identity;
  FTextureMatrix := AdMatrix_Identity;
end;

destructor TDXMesh.Destroy;
begin
  if FIndices <> nil then
  begin
    Finalize(FIndices);
  end;
  if FVertices <> nil then
  begin
    Finalize(FVertices);
  end;
  FreeBuffers;
  inherited Destroy;
end;

function FloatToCardinal(AValue: single): Cardinal; inline;
begin
  result := PCardinal(@AValue)^;
end;

procedure TDXMesh.Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);
var
  Mode:TD3DPrimitiveType;

begin
  if Loaded then
  begin
    with FParent do
    begin

      //Set material settings
      PushMaterial;

      //Set Blendmode
      if ABlendMode = bmAlpha then
      begin
        Direct3DDevice9.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
        Direct3DDevice9.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
      end else      
      if ABlendMode = bmAdd then
      begin
        Direct3DDevice9.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_SRCALPHA);
        Direct3DDevice9.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_ONE);
      end else
      if ABlendMode = bmMask then
      begin
        Direct3DDevice9.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_ZERO);
        Direct3DDevice9.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_INVSRCALPHA);
      end else
      if ABlendMode = bmSub then
      begin
        Direct3DDevice9.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_REVSUBTRACT);
        Direct3DDevice9.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
        Direct3DDevice9.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
      end;

      //Set texture filter
      if FTexture <> nil then
        TDXBitmapTexture(FTexture).SetFilter;

      //Apply transform matrices
      Direct3DDevice9.SetTransform(D3DTS_WORLDMATRIX(0), TD3DMatrix(FMatrix));
      Direct3DDevice9.SetTransform(D3DTS_TEXTURE0, TD3DMatrix(FTextureMatrix));

      //Set texture
      if (FTexture <> nil) and (FTexture.Loaded) and (FParent.FTextures) then
      begin
        if (FTexture <> FLastTexture) then
        begin
          if FTexture is TDXBitmapTexture then
            TDXBitmapTexture(FTexture).SetFilter;
            
          Direct3DDevice9.SetTexture(0,IDirect3DTexture9(FTexture.Texture));
          FLastTexture := FTexture;
        end;
      end
      else
      begin
        Direct3DDevice9.SetTexture(0,nil);
        FLastTexture := nil;
      end;

      //Set vertex stream soure
      Direct3DDevice9.SetStreamSource(0, FVertexBuffer, 0, sizeof(TD3DLVertex));
      Direct3DDevice9.SetFVF(D3DFVF_TD3DLVertex);

      //Set draw mode
      case ADrawMode of
        adTriangleStrips: Mode := D3DPT_TRIANGLESTRIP;
        adTriangles: Mode := D3DPT_TRIANGLELIST;
        adLines: Mode := D3DPT_LINELIST;
        adLineStrips: Mode := D3DPT_LINESTRIP;
        adTriangleFan: Mode := D3DPT_TRIANGLEFAN;
        adPoints: Mode := D3DPT_POINTLIST;
        adPointSprites: Mode := D3DPT_POINTLIST;
      else
        Mode := D3DPT_TRIANGLESTRIP;
      end;

      //Enable pointsprite settings
      if ADrawMode = adPointSprites then
      begin
        Direct3DDevice9.SetRenderState( D3DRS_POINTSPRITEENABLE, 1);
        Direct3DDevice9.SetRenderState( D3DRS_POINTSCALEENABLE, 0);

        if Texture <> nil then
        begin
          Direct3DDevice9.SetRenderState( D3DRS_POINTSIZE, FloatToCardinal(Texture.Width));
        end;
      end;


      //Render primitives
      if FIndices <> nil then
      begin
        Direct3DDevice9.SetIndices(FIndexBuffer);
        Direct3DDevice9.DrawIndexedPrimitive(Mode, 0, 0, VertexCount, 0, FPrimitiveCount);
      end
      else
      begin
        Direct3DDevice9.DrawPrimitive(Mode, 0, FPrimitiveCount);
      end;

      //Disable subtractive blending
      if ABlendMode = bmSub then
      begin
        Direct3DDevice9.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD);
      end;

      //Disable pointsprite settings
      if ADrawMode = adPointSprites then
      begin
        Direct3DDevice9.SetRenderState( D3DRS_POINTSPRITEENABLE, 0);
        Direct3DDevice9.SetRenderState( D3DRS_POINTSCALEENABLE, 0);
      end;
    end;
  end;
end;

procedure TDXMesh.FreeBuffers;
begin
  FIndexBuffer := nil;
  FVertexBuffer := nil;
end;

function TDXMesh.GetLoaded: boolean;
begin
  result := FVertexBuffer <> nil;
end;

procedure TDXMesh.PushMaterial;
begin
  if FUsesMaterial then
  begin
    //Push material settings
    FParent.Direct3DDevice9.SetRenderState(D3DRS_SPECULARENABLE,
      Cardinal(FMaterial.Power > 0.01));
    FParent.Direct3DDevice9.SetMaterial(FMaterial);
  end;
  
  //Set material source
  if FParent.FUsesMaterial <> Cardinal(FUsesMaterial) then
  begin
    if FUsesMaterial then
    begin
      //Use the material as material source
      FParent.Direct3DDevice9.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_MATERIAL);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_MATERIAL);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_MATERIAL);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_EMISSIVEMATERIALSOURCE, D3DMCS_MATERIAL); 
    end else
    begin
      //Use the diffuse color as material source
      FParent.Direct3DDevice9.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_COLOR1);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_COLOR1);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_COLOR1);
      FParent.Direct3DDevice9.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_MATERIAL);

      //Disable specular lightning if we use don't use a material
      FParent.Direct3DDevice9.SetRenderState(D3DRS_SPECULARENABLE, 0);

      //Reset the material settings to zero
      FillChar(FMaterial, SizeOf(FMaterial), 0);

      //Set the material settings
      FParent.Direct3DDevice9.SetMaterial(FMaterial);
    end;


    FParent.FUsesMaterial := Cardinal(FUsesMaterial);
  end;
end;

procedure TDXMesh.SetIndices(AIndex: TAdIndexArray);
begin
  if FIndices <> nil then
    Finalize(FIndices);

  FIndices := Copy(AIndex);
end;

procedure TDXMesh.SetMaterial(AMaterial: PAd2dMaterial);
begin
  if AMaterial <> nil then
  begin
    //Set material data
    FMaterial.Diffuse := ConvertColor(AMaterial^.Diffuse);
    FMaterial.Ambient := ConvertColor(AMaterial^.Ambient);
    FMaterial.Specular := ConvertColor(AMaterial^.Specular);
    FMaterial.Emissive := ConvertColor(AMaterial^.Emissive);
    FMaterial.Power := AMaterial.Power;

    FUsesMaterial := true;
  end else
    FUsesMaterial := false;
end;

procedure TDXMesh.SetTexture(ATexture: TAd2DTexture);
begin
  inherited SetTexture(ATexture);
end;

procedure TDXMesh.SetVertices(AVertices: TAdVertexArray);
begin
  if FVertices <> nil then
  begin
    Finalize(FVertices);
  end;
  FVertices := Copy(AVertices);
end;

procedure TDXMesh.Update;
var
  Vertices:array of TD3DLVertex;
  i:integer;
  PVertices,PIndices:pointer;
  lc:integer;
begin
  SetLength(Vertices,length(FVertices));
  for i := 0 to high(FVertices) do
  begin
    Vertices[i].position.x := FVertices[i].Position.x;
    Vertices[i].position.y := FVertices[i].Position.y;
    Vertices[i].position.z := FVertices[i].Position.z;
    Vertices[i].normale.x := FVertices[i].Normal.x;
    Vertices[i].normale.y := FVertices[i].Normal.y;
    Vertices[i].normale.z := FVertices[i].Normal.z;
    Vertices[i].diffuse := D3DCOLOR_ARGB(FVertices[i].Color.a,FVertices[i].Color.r,
      FVertices[i].Color.g,FVertices[i].Color.b);
    Vertices[i].textur1.x := FVertices[i].Texture.x;
    Vertices[i].textur1.y := FVertices[i].Texture.y;
  end;

  lc := FVertexCount;
  FVertexCount := length(Vertices);

  with FParent do
  begin
    if lc <> FVertexCount then
    begin
      Direct3DDevice9.CreateVertexBuffer(Sizeof(TD3DLVertex)*Vertexcount,
          D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFVF_TD3DLVertex, D3DPOOL_DEFAULT,
          FVertexBuffer, nil);
    end;

    //Lock the vertexbuffer
    FVertexBuffer.Lock(0,Sizeof(TD3DLVertex)*Vertexcount, pVertices, 0);
    //And move the data into it
    Move(Vertices[0], pVertices^, Sizeof(TD3DLVertex)*Vertexcount);
    FVertexBuffer.Unlock;

    if FIndices <> nil then
    begin
      //Create Indexbuffer
      lc := FIndicesCount;
      FIndicesCount := length(FIndices);
      if lc <> IndicesCount then
      begin
        Direct3DDevice9.CreateIndexBuffer(Sizeof(Word)*IndicesCount,
          D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT,
          FIndexBuffer,nil);
      end;
      //Lock the indexbuffer
      FIndexBuffer.Lock(0,Sizeof(Word)*IndicesCount, pIndices, 0);
      Move(FIndices[0], pIndices^, Sizeof(Word)*IndicesCount);
      FIndexBuffer.Unlock;
    end;
  end;
end;

{ TDXBitmapTexture }

constructor TDXBitmapTexture.Create(AParent: TDXApplication);
begin
  inherited Create;
  FParent := AParent;
  FTexture := nil;
  FHasMipMap := false;
end;

destructor TDXBitmapTexture.Destroy;
begin
  FlushTexture;
  inherited Destroy;
end;

procedure TDXBitmapTexture.SetFilter;
var
  f: Cardinal;
begin
  f := D3DTEXF_POINT;
  case FFilter of
    atPoint: f := D3DTEXF_POINT;
    atLinear: f := D3DTEXF_LINEAR;
    atAnisotropic: f := D3DTEXF_ANISOTROPIC;
  end;

  FParent.Direct3DDevice9.SetSamplerState(0, D3DSAMP_MAGFILTER, f);
  FParent.Direct3DDevice9.SetSamplerState(0, D3DSAMP_MINFILTER, f);
  if FHasMipMap then
    FParent.Direct3DDevice9.SetSamplerState(0, D3DSAMP_MIPFILTER, f)
  else
    FParent.Direct3DDevice9.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);
end;

procedure TDXBitmapTexture.FlushTexture;
begin
  if FTexture <> nil then
  begin
    IDirect3DTexture9(FTexture)._Release;
  end;
  FWidth := 0;
  FHeight := 0;
  FBaseWidth := 0;
  FBaseHeight := 0;
  FBitDepth := ad32Bit;
  FTexture := nil;
  FHasMipMap := false;
end;

function TDXBitmapTexture.GetLoaded: boolean;
begin
  Result := FTexture <> nil;
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
  Result := (R8ToR4(a) shl 12) or (R8ToR4(b) shl 8)
                        or (R8ToR4(g) shl 4)
                        or R8ToR4(r);
end;

procedure TDXBitmapTexture.LoadFromBitmap(ABmp: TAd2dBitmap; ABitDepth: TAdBitDepth);
var afmt:TD3DFORMAT;
    w,h,x,y:integer;
    d3dlr:TD3DLocked_Rect;
    pnt32:PRGBARec;
    cur16:PWord;
    cur32:PLongWord;
begin
  w := 1 shl ceil(log2(ABmp.Width));
  h := 1 shl ceil(log2(ABmp.Height));

  if (ABitDepth <> FBitDepth) or (w <> FWidth) or (h <> FHeight) or (FTexture = nil) then
  begin
    case ABitDepth of
      ad16Bit: afmt := D3DFMT_A4R4G4B4;
      ad32Bit: afmt := D3DFMT_A8R8G8B8;
    else
      afmt := D3DFMT_A8R8G8B8;
    end;
    FlushTexture;
    with FParent do
    begin
      if (FParent.FMipmaps) and (FParent.CanAutoGenMipmaps) then
      begin
        D3DXCreateTexture(Direct3DDevice9,w,h,0, D3DUSAGE_AUTOGENMIPMAP, afmt, D3DPOOL_MANAGED, IDirect3DTexture9(FTexture));
        FHasMipMap := true;
      end
      else
      begin
        FHasMipMap := false;
        D3DXCreateTexture(Direct3DDevice9,w,h,0, 0, afmt, D3DPOOL_MANAGED, IDirect3DTexture9(FTexture));
      end;
    end;
  end;

  FHeight := h;
  FWidth := w;
  FBitDepth := ABitDepth;
  FBaseWidth := ABmp.Width;
  FBaseHeight := ABmp.Height;

  with IDirect3DTexture9(FTexture) do
  begin
    if Failed(LockRect(0, d3dlr, nil, 0)) then
    begin
      //FParent.WriteLog(ltError, 'Error while locking the texture!');
    end;

    if FBitDepth = ad16Bit then
    begin
      cur16 := d3dlr.pBits;
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
    end else
    if FBitDepth = ad32Bit then
    begin
      cur32 := d3dlr.pBits;
      pnt32 := ABmp.ScanLine;
      for y := 0 to ABmp.Height - 1 do
      begin
        Move(pnt32^, cur32^, ABmp.Width * 4);
        inc(pnt32, ABmp.Width);
        inc(cur32, w);
      end;
    end;

    UnlockRect(0);
  end;
end;

procedure TDXBitmapTexture.SaveToBitmap(ABmp: TAd2dBitmap);
var
  x,y:integer;
  cur16:PWord;
  cur32:PLongWord;
  ptr32:PRGBARec;
  d3dlr:TD3DLocked_Rect;
begin
  if Loaded then
  begin
    IDirect3DTexture9(FTexture).LockRect(0, d3dlr, nil, 0);

    if FBitDepth = ad32Bit then
    begin
      Cur32 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32^.a := Cur32^ shr 24;
            ptr32^.b := Cur32^ shr 16;
            ptr32^.g := Cur32^ shr 8;
            ptr32^.r := Cur32^;
            inc(ptr32);
          end;
          inc(Cur32);
        end;
      end;
    end else
    if FBitDepth = ad32Bit then
    begin
      Cur16 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32^.a := ($000F and (Cur16^ shr 12))*16;
            ptr32^.b := ($000F and (Cur16^ shr 8))*16;
            ptr32^.g := ($000F and (Cur16^ shr 4))*16;
            ptr32^.r := ($000F and Cur16^)*16;
            inc(ptr32);
          end;
          inc(Cur16);
        end;
      end;
    end;

    IDirect3DTexture9(FTexture).UnlockRect(0)
  end;
end;

{ TDXRenderTargetTexture }

constructor TDXRenderTargetTexture.Create(AParent: TDXApplication);
begin
  inherited Create;
  FEditable := false;
  FParent := AParent;
end;

destructor TDXRenderTargetTexture.Destroy;
begin
  FlushMemory;  
  inherited;
end;

procedure TDXRenderTargetTexture.FlushMemory;
begin
  if FTexture <> nil then
  begin
    IDirect3DTexture9(FTexture)._Release;
  end;
  FWidth := 0;
  FHeight := 0;
  FBitDepth := ad32Bit;
  FTexture := nil;
end;

function TDXRenderTargetTexture.GetLoaded: boolean;
begin
  result := FTexture <> nil;
end;

procedure TDXRenderTargetTexture.SaveToBitmap(ABmp: TAd2dBitmap);
var
  x,y:integer;
  cur16:PWord;
  cur32:PLongWord;
  ptr32:PRGBARec;
  d3dlr: TD3DLocked_Rect;

  tar_surface: IDirect3DSurface9;
  src_surface: IDirect3DSurface9;
  src_desc: TD3DSurfaceDesc;
begin
  if Loaded then
  begin
    IDirect3DTexture9(FTexture).GetSurfaceLevel(0, src_surface);
    src_surface.GetDesc(src_desc);

    FParent.Direct3DDevice9.CreateOffScreenPlainSurface(
      src_desc.Width, src_desc.Height, src_desc.Format,
      D3DPOOL_SYSTEMMEM, tar_surface, nil);

    FParent.Direct3DDevice9.GetRenderTargetData(src_surface, tar_surface);

    tar_surface.LockRect(d3dlr, nil, D3DLOCK_READONLY);

    if FBitDepth = ad32Bit then
    begin
      Cur32 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32^.a := Cur32^ shr 24;
            ptr32^.b := Cur32^ shr 16;
            ptr32^.g := Cur32^ shr 8;
            ptr32^.r := Cur32^;
            inc(ptr32);
          end;
          inc(Cur32);
        end;
      end;
    end;

    if FBitDepth = ad16Bit then
    begin
      Cur16 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32^.a := ($000F and (Cur16^ shr 12))*16;
            ptr32^.b := ($000F and (Cur16^ shr 8))*16;
            ptr32^.g := ($000F and (Cur16^ shr 4))*16;
            ptr32^.r := ($000F and Cur16^)*16;
            inc(ptr32);
          end;
          inc(Cur16);
        end;
      end;
    end;

    tar_surface.UnlockRect;
    tar_surface := nil;    
  end;
end;

procedure TDXRenderTargetTexture.SetSize(AWidth, AHeight: integer; ABitDepth: TAdBitDepth);
var
  w, h: integer;
  afmt:TD3DFORMAT;
begin
  w := 1 shl ceil(log2(AWidth));
  h := 1 shl ceil(log2(AHeight));

  if (not Loaded) or (w <> FWidth) or (h <> FHeight) or (ABitDepth <> FBitDepth) then
  begin
    FlushMemory;

    case ABitDepth of
      ad16Bit: AFMT := D3DFMT_A4R4G4B4;
      ad32Bit: AFMT := D3DFMT_A8R8G8B8;
    else
      AFMT := D3DFMT_R8G8B8;
    end;

    D3DXCreateTexture(
      FParent.Direct3DDevice9, w, h, 1,
      D3DUSAGE_RENDERTARGET,
      AFMT, D3DPOOL_DEFAULT, IDirect3dTexture9(FTexture));

    FWidth := w;
    FHeight := h;
    FBitDepth := ABitDepth;
  end;

  FBaseWidth := AWidth;
  FBaseHeight := AHeight;
end;

{ TDXPixelCounter }

constructor TDXPixelCounter.Create(AParent: TDXApplication);
begin
  inherited Create;

  FParent := AParent;
  FParent.Direct3DDevice9.CreateQuery(D3DQUERYTYPE_OCCLUSION, Direct3DQuery);
end;

destructor TDXPixelCounter.Destroy;
begin
  Direct3DQuery := nil;
  inherited;
end;

function TDXPixelCounter.GetCount: Cardinal;
begin
  while Direct3DQuery.GetData(@result, SizeOf(DWORD), D3DGETDATA_FLUSH) = S_FALSE do;
end;

procedure TDXPixelCounter.StartCount;
begin
  Direct3DQuery.Issue(D3DISSUE_BEGIN);
end;

procedure TDXPixelCounter.StopCount;
begin
  Direct3DQuery.Issue(D3DISSUE_END);
end;

{ TDXLight }

constructor TDXLight.Create(AParent: TDXApplication);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TDXLight.DisableLight;
begin
  FParent.Direct3DDevice9.LightEnable(FLightNr, false);
end;

procedure TDXLight.EnableLight(ALight: Cardinal);
begin
  FLightNr := ALight;
  FParent.Direct3DDevice9.SetLight(ALight, FLight);
  FParent.Direct3DDevice9.LightEnable(ALight, true);
end;


function CalcRange(AValue: TAd2dLightData): single;
//Reciprocal epsilon
const
  epsilon = 10000;
begin
  with AValue do
  begin
    //Set to result to a high value - if a range can not be calculated,
    //it theoreticaly would be infinite
    result := 1 shl 30;

    if IsZero(QuadraticAttenuation) then
    begin
      //af = 1 / (c + lx + qx²) = e
      //if q = 0 ==> x = (1/e - c) / l
      if not IsZero(LinearAttenuation) then
        result := (epsilon - ConstantAttenuation) / LinearAttenuation;
    end else
    begin
      //af = 1 / (c + lx + qx²) = e
      //                   l       -------------
      //if q <> 0 ==> x = --- +-  / l²   c - 1/e
      //                  2*q   \/ --- + -------
      //                           4q²      q
      result := - LinearAttenuation / (2 * QuadraticAttenuation) +
        Sqrt(Sqr(LinearAttenuation) / (4 * Sqr(QuadraticAttenuation)) -
             ((ConstantAttenuation - epsilon) / QuadraticAttenuation));
    end;
  end;
end;

procedure TDXLight.SetData(AValue: TAd2dLightData);
begin
  //Clear FLight record
  FillChar(FLight, SizeOf(FLight), #0);

  //Set light type
  case AValue.LightType of
    altPoint: FLight._Type := D3DLIGHT_POINT;
    altDirectional: FLight._Type := D3DLIGHT_DIRECTIONAL;
  end;

  //Copy light settings
  FLight.Diffuse := ConvertColor(AValue.Diffuse);
  FLight.Specular := ConvertColor(AValue.Specular);
  FLight.Ambient := ConvertColor(AValue.Ambient);
  if AValue.LightType = altDirectional then
    FLight.Direction := PD3DXVector3(@AValue.Position)^
  else
  begin
    FLight.Position := PD3DXVector3(@AValue.Position)^;
    FLight.Range := CalcRange(AValue);
  end;
    
  FLight.Attenuation0 := AValue.ConstantAttenuation;
  FLight.Attenuation1 := AValue.LinearAttenuation;
  FLight.Attenuation2 := AValue.QuadraticAttenuation;
end;

end.
