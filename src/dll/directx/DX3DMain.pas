{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DX3DMain.pas
* Comment: The Direct 3D DLL unit 
}

unit DX3DMain;

interface

uses d3dx9, dxerr9, Direct3D9, AdClasses, Classes, Windows, Graphics, Math, SysUtils;

type
  TDXApplication = class(TAd2DApplication)
    private
      FLights:Array[0..1023] of Boolean;
      FCurrentLights:integer;
    protected
      procedure SetOptions(AValue:TAdOptions);override;
    public
      Direct3D9:IDirect3D9;
      Direct3DDevice9:IDirect3DDevice9;
      constructor Create;override;
      destructor Destroy;reintroduce;
      //function CreateLight:TAdLight;override;
      function CreateBitmapTexture:TAd2DBitmapTexture;override;
      //function CreateRenderTargetTexture:TAdRenderTargetTexture;override;
      function CreateMesh:TAd2DMesh;override;
      //procedure SetRenderTarget(ATarget:TAdRenderTargetTexture);override;
      function Initialize(AWnd:LongWord; AOptions:TAdOptions; ADisplay:TAdDisplay):boolean;override;
      procedure Finalize;override;

      procedure Setup2DScene(AWidth,AHeight:integer);override;

      procedure ClearSurface(AColor: TAndorraColor);override;
      procedure BeginScene;override;
      procedure EndScene;override;
      procedure Flip;override;
  end;

  TDXMesh = class(TAd2DMesh)
    private
      FVertexBuffer:IDirect3DVertexBuffer9;
      FIndexBuffer:IDirect3DIndexBuffer9;
      FParent:TDXApplication;
      FMatrix:TD3DMatrix;
      procedure FreeBuffers;
    protected
      procedure SetVertices(AVertices:TAdVertexArray);override;
      procedure SetIndex(AIndex:TAdIndexArray);override;
      procedure SetTexture(ATexture:TAd2DTexture);override;
      function GetLoaded:boolean;override;
    public
      procedure SetMatrix(AMatrix:TAdMatrix);override;
      constructor Create(AParent:TDXApplication);
      destructor Destroy;override;
      procedure Draw;override;
      procedure Update;override;
  end;

  TDXBitmapTexture = class(TAd2DBitmapTexture)
    private
      FParent:TDXApplication;
    protected
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TDXApplication);
      destructor Destroy;override;
      procedure FlushTexture;override;
      procedure LoadFromBitmap(ABmp:TAdBitmap;ABitDepth:byte=32);override;
      procedure SaveToBitmap(ABmp:TAdBitmap);override;
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

{ TDXApplication }

constructor TDXApplication.Create;
begin
  inherited;
  //Create Direct 3D Interface
  Direct3D9 := Direct3DCreate9( D3D_SDK_VERSION );
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

function TDXApplication.CreateBitmapTexture: TAd2DBitmapTexture;
begin
  result := TDXBitmapTexture.Create(self);
end;

{function TDXApplication.CreateLight: TAdLight;
begin

end;

function TDXApplication.CreateRenderTargetTexture: TAdRenderTargetTexture;
begin

end;    }

function TDXApplication.Initialize(AWnd: LongWord; AOptions: TAdOptions;
  ADisplay: TAdDisplay):boolean;
var
  d3dpp : TD3DPresent_Parameters;
  d3ddm : TD3DDisplayMode;
  dtype : TD3DDevType;
  afmt : D3DFORMAT;
  D3DCaps9 : TD3DCaps9;
  hw : boolean;
  vp,i : integer;
  hr : HRESULT;
begin
  result := false;
  if Direct3D9 <> nil then
  begin
    WriteLog(ltNone,'Try to initialize Andorra Direct3D 9 Plugin.');

    if (doHardware in AOptions) then
    begin
      dtype := D3DDEVTYPE_HAL;
    end
    else
    begin
      dtype := D3DDEVTYPE_REF;
    end;

    if Failed(Direct3D9.GetDeviceCaps(D3DADAPTER_DEFAULT, dtype, D3DCaps9)) then
    begin
      WriteLog(ltFatalError,'No connection to the default device.');
    end;
    hw := D3DCaps9.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT <> 0;
    if hw then
    begin
      vp := D3DCREATE_HARDWARE_VERTEXPROCESSING;
    end
    else
    begin
      vp := D3DCREATE_SOFTWARE_VERTEXPROCESSING;
      WriteLog(ltWarning,'The current device does not support "HARDWARE TRANSFORM AND LIGHT".');
    end;

    Fillchar(d3dpp, sizeof(d3dpp),0);
    with d3dpp do
    begin
      Windowed := not (doFullscreen in AOptions);
      SwapEffect := D3DSWAPEFFECT_DISCARD;
      if not (doVSync in AOptions) then
      begin
        PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      end;
      if not Windowed then
      begin
        BackBufferWidth := ADisplay.Width;
        BackBufferWidth := ADisplay.Height;
        if ADisplay.Freq > 0 then
        begin
          Fullscreen_RefreshRateInHz := ADisplay.Freq;
        end;
        case ADisplay.BitCount of
          16: afmt := D3DFMT_X1R5G5B5;
          32: afmt := D3DFMT_X8R8G8B8;
        else
          afmt := D3DFMT_X8R8G8B8;
        end;
        if failed(Direct3D9.CheckDeviceType(D3DADAPTER_DEFAULT, dtype, afmt, afmt, false)) then
        begin
          WriteLog(ltWarning,'The current device settings may be unsupportet.');
          WriteLog(ltInfo,'Try to use other modes.');
          case ADisplay.BitCount of
            16: afmt := D3DFMT_R5G6B5;
            32: afmt := D3DFMT_A8R8G8B8;
          else
            afmt := D3DFMT_A8R8G8B8;
          end;
        end;
      end
      else
      begin
        if Failed(Direct3D9.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm)) then
        begin
          WriteLog(ltWarning,'Can not access current display settings. Try to run in fullscreen mode.');
          exit;
        end;
        afmt := d3ddm.Format;
      end;
      if failed(Direct3D9.CheckDeviceType(D3DADAPTER_DEFAULT, dtype, afmt, afmt, false)) then
      begin
        WriteLog(ltFatalError,'The current device settings are unsupportet. Try another adapter mode.');
        exit;
      end;
      BackBufferFormat := afmt;
    end;
    WriteLog(ltInfo,'Try to initialize the device.');

    //Create device
    hr := Direct3D9.CreateDevice(D3DADAPTER_DEFAULT,  dtype, AWnd, vp, @d3dpp, Direct3DDevice9);
    if Failed(hr) then
    begin
      WriteLog(ltWarning,DXGetErrorDescription9(hr));
      WriteLog(ltFatalError,'Couldn''t initialize Direct3DDevice! ');
      exit;
    end
    else
    begin
      result := true;
    end;

    FWidth := ADisplay.Width;
    FHeight := ADisplay.Height;

    //Set lighting
    //SetOptions(AAppl,AOptions);
    //Direct3DDevice9.SetRenderState(D3DRS_AMBIENT, $00FFFFFF);

    //Get the number of lights
    FMaxLightCount := d3dcaps9.MaxActiveLights;
    WriteLog(ltInfo,PChar('Device supports '+inttostr(MaxLights)+' lights'));


    WriteLog(ltInfo,PChar(Inttostr(Direct3DDevice9.GetAvailableTextureMem div 1024 div 1024)+'MB Texture Memory on this device.'));

    //Setup Material
    if
      Failed(Direct3DDevice9.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_COLOR1)) or
      Failed(Direct3DDevice9.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_COLOR1)) or
      Failed(Direct3DDevice9.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_COLOR1)) then
    begin
      WriteLog(ltError,'Can''t set material sources.');
    end;

    //No culling
    if Failed(Direct3DDevice9.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE)) then
    begin
      WriteLog(ltError,'Can''t turn culling off.');
      exit;
    end;

    //Enable Texture alphablending
    if
      Failed(Direct3DDevice9.SetRenderState(D3DRS_ALPHABLENDENABLE, LongWord(TRUE))) or
      Failed(Direct3DDevice9.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA)) or
      Failed(Direct3DDevice9.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA)) or
      Failed(Direct3DDevice9.SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE)) then
    begin
      WriteLog(ltWarning,'Alphablending is disabled');
    end;

    for i := 0 to 1023 do
    begin
      FLights[i] := false;
    end;
    FCurrentLights := 0;

    WriteLog(ltInfo,'Initialization complete.');
    result := true;

    Direct3DDevice9.SetRenderState(D3DRS_LIGHTING,LongWord(false));
  end
  else
  begin
    WriteLog(ltFatalError,'Error while connecting DirectX. Check out whether you have the right DirectX Version (9c) installed.');
  end;
end;

procedure TDXApplication.Finalize;
begin
  Direct3d9 := nil;
  Direct3dDevice9 := nil;
  WriteLog(ltInfo,'Finalization Complete.');
  Free;
end;

procedure TDXApplication.SetOptions(AValue: TAdOptions);
begin

end;

procedure TDXApplication.Setup2DScene(AWidth, AHeight: integer);
var pos, dir, up : TD3DXVector3;
    matView, matProj: TD3DXMatrix;
begin
  if Direct3DDevice9 <> nil then
  begin
    pos := D3DXVector3 (Awidth/2,AHeight/2,-10);
    dir := D3DXVector3 (Awidth/2,AHeight/2,0);
    up := D3DXVector3 (0,-1,0);

    D3DXMatrixLookAtRH( matView, pos, dir, up);
    Direct3dDevice9.SetTransform(D3DTS_VIEW, matView);

    D3DXMatrixOrthoRH( matProj, Awidth, Aheight, 0,100);
    if Failed(Direct3dDevice9.SetTransform(D3DTS_PROJECTION, matProj)) then
    begin
      WriteLog(ltError,'Error while setting new view matrix.');
    end
    else
    begin
      WriteLog(ltInfo,'Changed point of view to a 2D Szene.');
    end;
  end;
end;

{procedure TDXApplication.SetRenderTarget(ATarget: TAdRenderTargetTexture);
begin
  inherited;

end;}


procedure TDXApplication.BeginScene;
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.BeginScene;
  end;
end;

procedure TDXApplication.EndScene;
var ares:cardinal;
    i:integer;
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.GetRenderState(D3DRS_LIGHTING,ares);
    if ares = Cardinal(true) then
    begin
      for i := 0 to 1023 do
      begin
        if FLights[i] then
        begin
          Direct3DDevice9.LightEnable(i,false);
        end;
      end;
    end;
    FCurrentLights := 0;
    Direct3DDevice9.EndScene;
  end;
end;

procedure TDXApplication.Flip;
begin
  if Direct3DDevice9 <> nil then
  begin
    if Failed(Direct3DDevice9.Present(nil, nil, 0, nil)) then
    begin
      WriteLog(ltFatalError,'Error while flipping.');
    end;
  end;
end;

procedure TDXApplication.ClearSurface(AColor: TAndorraColor);
begin
  if Direct3DDevice9 <> nil then
  begin
    Direct3DDevice9.Clear( 0, nil, D3DCLEAR_TARGET, D3DCOLOR_ARGB(AColor.a,AColor.r,AColor.g,AColor.b),
      1.0, 0);
  end;
end;

{ TDXMesh }

constructor TDXMesh.Create(AParent: TDXApplication);
begin
  inherited Create;
  FParent := AParent;
  FIndices := nil;
  FVertices := nil;
  D3DXMatrixIdentity(FMatrix);
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

procedure TDXMesh.Draw;
begin
  if Loaded then
  begin
    with FParent do
    begin
      Direct3DDevice9.SetTransform(D3DTS_WORLDMATRIX(0), FMatrix);
      if (FTexture <> nil) and (FTexture.Loaded) then
      begin
        Direct3DDevice9.SetTexture(0,IDirect3DTexture9(FTexture.Texture));
      end;
      Direct3DDevice9.SetStreamSource(0, FVertexBuffer, 0, sizeof(TD3DLVertex));
      Direct3DDevice9.SetFVF(D3DFVF_TD3DLVertex);
      if UseIndexBuffer then
      begin
        Direct3DDevice9.SetIndices(FIndexBuffer);
        Direct3DDevice9.DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, VertexCount, 0, FPrimitiveCount)
      end
      else
      begin
        Direct3DDevice9.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, FPrimitiveCount)
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

procedure TDXMesh.SetIndex(AIndex: TAdIndexArray);
begin
  if FIndices <> nil then
  begin
    Finalize(FIndices);
  end;
  FIndices := Copy(AIndex);
end;

procedure TDXMesh.SetMatrix(AMatrix: TAdMatrix);
var x,y:integer;
begin
  for x := 0 to 3 do
  begin
    for y := 0 to 3 do
    begin
      FMatrix.m[x,y] := AMatrix[x,y];
    end;
  end;
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
var Vertices:array of TD3DLVertex;
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
    Vertices[i].normale.x := FVertices[i].Position.x;
    Vertices[i].normale.y := FVertices[i].Position.y;
    Vertices[i].normale.z := FVertices[i].Position.z;
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

    if UseIndexBuffer then
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
end;

destructor TDXBitmapTexture.Destroy;
begin
  FlushTexture;
  inherited Destroy;
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
  FBitCount := 0;
  FTexture := nil;
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

procedure TDXBitmapTexture.LoadFromBitmap(ABmp: TAdBitmap; ABitDepth: byte);
var afmt:TD3DFORMAT;
    w,h,x,y:integer;
    d3dlr:TD3DLocked_Rect;
    pnt32:PRGBARec;
    cur16:PWord;
    cur32:PLongWord;
begin
  w := 1 shl ceil(log2(ABmp.Width));
  h := 1 shl ceil(log2(ABmp.Height));

  if (ABitDepth <> FBitCount) or (w <> FWidth) or (h <> FHeight) or (FTexture = nil) then
  begin
    case ABitDepth of
      16: afmt := D3DFMT_A4R4G4B4;
    else
      afmt := D3DFMT_A8R8G8B8;
    end;
    FlushTexture;
    with FParent do
    begin
      D3DXCreateTexture(Direct3DDevice9,w,h,0,0,afmt,D3DPOOL_MANAGED, IDirect3DTexture9(FTexture));
    end;
  end;

  FHeight := h;
  FWidth := w;
  FBitCount := ABitDepth;
  FBaseWidth := ABmp.Width;
  FBaseHeight := ABmp.Height;

  with IDirect3DTexture9(FTexture) do
  begin
    if Failed(LockRect(0, d3dlr, nil, 0)) then
    begin
      FParent.WriteLog(ltError, 'Error while locking the texture!');
    end;

    if FBitCount = 16 then
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
    end;

    if FBitCount = 32 then
    begin
      cur32 := d3dlr.pBits;
      pnt32 := ABmp.ScanLine;
      for y := 0 to ABmp.Height - 1 do
      begin
        for x := 0 to w - 1 do
        begin
          if (x < ABmp.Width) then
          begin
            cur32^ := D3DColor_ARGB(pnt32^.a,pnt32^.b,pnt32^.g,pnt32^.r);
            inc(pnt32);
          end;
          inc(cur32);
        end;
      end;
    end;

    UnlockRect(0);
  end;
end;

procedure TDXBitmapTexture.SaveToBitmap(ABmp: TAdBitmap);
var x,y:integer;
    cur16:PWord;
    cur32:PLongWord;
    ptr32:PRGBARec;
    d3dlr:TD3DLocked_Rect;
begin
  if Loaded then
  begin
    IDirect3DTexture9(FTexture).LockRect(0, d3dlr, nil, 0);

    if FBitCount = 32 then
    begin
      Cur32 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32^.b := Cur32^ shr 16;
            ptr32^.g := Cur32^ shr 8;
            ptr32^.r := Cur32^;
            inc(ptr32);
          end;
          inc(Cur32);
        end;
      end;
    end;   

    if FBitCount = 16 then
    begin
      Cur16 := d3dlr.pBits;
      ptr32 := ABmp.Scanline;
      for y := 0 to FBaseHeight-1 do
      begin
        for x := 0 to FWidth-1 do
        begin
          if x < ABmp.Width then
          begin
            ptr32.r := ($000F and (Cur16^ shr 8))*16;
            ptr32.g := ($000F and (Cur16^ shr 4))*16;
            ptr32.b := ($000F and Cur16^)*16;
            inc(ptr32);
          end;
          inc(Cur16);
        end;
      end;
    end;

    IDirect3DTexture9(FTexture).UnlockRect(0)
  end;
end;

end.
