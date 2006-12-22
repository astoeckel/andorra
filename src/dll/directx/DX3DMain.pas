{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DX3DMain.pas
* Comment: The Direct 3D DLL 
}

unit DX3DMain;

interface

uses d3dx9, Direct3D9, dxerr9, AndorraUtils, Classes, Windows, Graphics, Math, SysUtils;

type TAndorraApplicationItem = class
  private
    FLights:Array[0..1023] of Boolean;
  public
    Direct3d9:IDirect3D9;
    Direct3d9Device:IDirect3dDevice9;
    SizeX,SizeY:integer;
    TextureFilter:TD3DTextureFilterType;
    LastTexture:IDirect3dTexture9;
    LastXTextureMode,LastYTextureMode:TAndorraTextureMode;
    LogProc:TAdLogProc;
    LogAppl:Pointer;
    CurrentLights:integer;
    MaxLights:integer;
    function GetFreeLight:integer;
    procedure ReleaseLight(alight:integer);
    procedure WriteLog(Typ:TAdLogTyp;Text:PChar);
end;

type TAndorraLightItem = class
  public
    AAppl:TAndorraApplication;
    AOptions:TLight;
    ALight:integer;
    AVisible:boolean;
    constructor Create(Appl:TAndorraApplication);
    destructor Destroy;override;
end;


type TAndorraTextureItem = class
  public
    AAppl:TAndorraApplication;
    ATexWidth,ATexHeight:integer;
    AFormat:TD3DFormat;
    ATextureImg:IDirect3DTexture9;
    ABaseRect:TRect;
    destructor Destroy;override;
end;

type TAndorraImageItem = class
  private
    FAppl:TAndorraApplication;
    FVertexBuffer:IDirect3DVertexBuffer9;
    FIndexBuffer:IDirect3DIndexBuffer9;
    FImage:IDirect3DTexture9;
    FWidth,FHeight:integer;
    FColor:TAndorraColor;
    FSrcRect:TRect;
    FXTextureMode,FYTextureMode:TAndorraTextureMode;
    FDetails:integer;
    FUseIndexBuffer:boolean;
    procedure SetSourceRect(ARect:TRect);
    function CompRects(Rect1,Rect2:TRect):boolean;
  protected
    function SetupBuffer:HRESULT; 
  public
    constructor Create(Appl:TAndorraApplication);
    procedure Draw(DestApp:TAndorraApplication;DestRect,SourceRect:TRect;Rotation:integer;RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);
    procedure LoadTexture(ATexture:TAndorraTexture);
    procedure SetColor(AColor:TAndorraColor);
    destructor Destroy;override;
    function GetImageInfo:TImageInfo;
    procedure SetXTextureMode(AMode:TAndorraTextureMode);
    procedure SetYTextureMode(AMode:TAndorraTextureMode);
    procedure SetDetails(ADetail:integer);
end;


//Initialization
function CreateApplication:TAndorraApplication;stdcall;
procedure DestroyApplication(Appl:TAndorraApplication);stdcall;
function InitDisplay(Appl:TAndorraApplication; AWindow:hWnd; AOptions:TAdDrawModes; ADisplay:TAdDrawDisplay):boolean;stdcall;
procedure SetTextureQuality(Appl:TAndorraApplication;Quality:TAndorraTextureQuality);stdcall;

//Render Control
procedure BeginScene(Appl:TAndorraApplication);stdcall;
procedure EndScene(Appl:TAndorraApplication);stdcall;
procedure ClearScene(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
procedure SetupScene(Appl:TAndorraApplication;AWidth,AHeight:integer);stdcall;
procedure Flip(Appl:TAndorraApplication);stdcall;
procedure SetOptions(Appl:TAndorraApplication;AOptions:TAdDrawModes);stdcall;
procedure SetAmbientLight(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;

//SpriteControl
function CreateImage(Appl:TAndorraApplication):TAndorraImage;stdcall;
procedure DrawImage(DestApp:TAndorraApplication;Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);stdcall;
procedure DestroyImage(Img:TAndorraImage);stdcall;
procedure ImageLoadTexture(Img:TAndorraImage;ATexture:TAndorraTexture);stdcall;
procedure SetImageColor(Img:TAndorraImage;AColor:TAndorraColor);stdcall;
procedure SetTextureXMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
procedure SetTextureYMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
procedure SetImageDetail(Img:TAndorraImage;ADetail:integer);stdcall;

//Texture Creation
function LoadTextureFromFile(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
function LoadTextureFromFileEx(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;stdcall;
function LoadTextureFromBitmap(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;stdcall;
procedure FreeTexture(ATexture:TAndorraTexture);stdcall;
procedure AddTextureAlphaChannel(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
function GetTextureInfo(Tex:TAndorraTexture):TImageInfo;stdcall;
procedure SetTextureAlpha(Tex:TAndorraTexture;AValue:Byte);stdcall;
function CheckTextureMem(Appl:TAndorraApplication):integer;stdcall;
procedure GetTextureAsBitmap(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
procedure GetTextureAlphaChannelAsBitmap(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;

//Lights
function CreateLight(Appl:TAndorraApplication):TAndorraLight;stdcall;
procedure DestroyLight(ALight:TAndorraLight);stdcall;
procedure RestoreLight(ALight:TAndorraLight;Data:TLight);stdcall;
procedure EnableLight(ALight:TAndorraLight);stdcall;
procedure DisableLight(ALight:TAndorraLight);stdcall;

//LogSystem
procedure SetLogProc(Appl:TAndorraApplication;ALogProc:TAdLogProc;AAppl:Pointer);


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

//The Andorra Texture
destructor TAndorraTextureItem.Destroy;
begin
  if ATextureImg <> nil then
  begin
    ATextureImg._Release;
    ATextureImg := nil;
  end;
  inherited Destroy;
end;

//The Andorra Image
constructor TAndorraImageItem.Create(Appl: Pointer);
begin
  inherited Create;
  if Appl <> nil then
  begin
    FAppl := Appl;
    FColor := Ad_ARGB(255,255,255,255);
    FDetails := 1;
  end
  else
  begin
    Free;
  end;
end;

procedure TAndorraImageItem.SetSourceRect(ARect: TRect);
begin
  FSrcRect.Left := ARect.Left;
  FSrcRect.Right := ARect.Right;
  FSrcRect.Top := ARect.Top;
  FSrcRect.Bottom := ARect.Bottom;
  SetupBuffer;
end;

procedure TAndorraImageItem.SetColor(AColor:TAndorraColor);
begin
  FColor := AColor;
  SetupBuffer;
end;

procedure TAndorraImageItem.SetDetails(ADetail: integer);
begin
  if ADetail > 0 then
  begin
    FDetails := ADetail;
  end;
end;

function TAndorraImageItem.SetupBuffer;
var
  Vertices: Array of TD3DLVertex;
  Indices: Array of Word;
  pVertices,pIndices: Pointer;
  Vertexcount,Indexcount:integer;
  i,x,y:integer;
  ax,ay:double;
  w,h:integer;
begin
  Vertexcount := (FDetails+1)*(FDetails+1);
  Indexcount := FDetails*FDetails*6;

  SetLength(Vertices,Vertexcount);
  SetLength(Indices,Indexcount);

  FUseIndexBuffer := FDetails > 1;


  i := 0;

  w := FSrcRect.Right - FSrcRect.Left;
  h := FSrcRect.Bottom - FSrcRect.Top;

  for y := 0 to FDetails do
  begin
    for x := 0 to FDetails do
    begin
      ay := y*fheight/FDetails;
      ax := x*fwidth/FDetails;
      Vertices[i].position := D3DXVector3(ax,ay,0);
      Vertices[i].diffuse := D3DColor_ARGB(FColor.a,FColor.r,FColor.g,FColor.b);
      Vertices[i].textur1 := D3DXVector2((FSrcRect.Left + w/FDetails*x)/FWidth,(FSrcRect.Top + h/FDetails*y)/FHeight);
      Vertices[i].normale := D3DXVector3(0,0,-1);
      i := i + 1;
    end;
  end;

  if FUseIndexBuffer then
  begin
    i := 0;
    for y := 0 to FDetails - 1 do
    begin
      for x := 0 to FDetails - 1 do
      begin
        Indices[i] :=   y     * (FDetails+1) + x + 1;
        Indices[i+1] := (y+1) * (FDetails+1) + x;
        Indices[i+2] := y     * (FDetails+1) + x;
        Indices[i+3] := y     * (FDetails+1) + x + 1;
        Indices[i+4] := (y+1) * (FDetails+1) + x + 1;
        Indices[i+5] := (y+1) * (FDetails+1) + x;
        i := i + 6;
      end;
    end;

  end;

  //Create Vertexbuffer and store the vertices/indices
  with TAndorraApplicationItem(FAppl) do
  begin
    //Free the buffers
    FVertexBuffer := nil;
    FIndexBuffer := nil;

    //Create Vertexbuffer
    Direct3D9Device.CreateVertexBuffer(Sizeof(TD3DLVertex)*Vertexcount,
      D3DUSAGE_WRITEONLY, D3DFVF_TD3DLVertex, D3DPOOL_DEFAULT,
      FVertexBuffer, nil);

    //Lock the vertexbuffer
    FVertexBuffer.Lock(0,Sizeof(TD3DLVertex)*Vertexcount, pVertices, 0);
    Move(Vertices[0], pVertices^, Sizeof(TD3DLVertex)*Vertexcount);
    FVertexBuffer.Unlock;

    if FUseIndexBuffer then
    begin
      //Create Indexbuffer
      Direct3D9Device.CreateIndexBuffer(Sizeof(Word)*IndexCount,
        D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT,
        FIndexBuffer,nil);

      //Lock the indexbuffer
      FIndexBuffer.Lock(0,Sizeof(Word)*IndexCount, pIndices, 0);
      Move(Indices[0], pIndices^, Sizeof(Word)*IndexCount);
      FIndexBuffer.Unlock;
    end;
  end;
  result := D3D_OK;
end;

procedure TAndorraImageItem.SetXTextureMode(AMode: TAndorraTextureMode);
begin
  FXTextureMode := AMode;
end;

procedure TAndorraImageItem.SetYTextureMode(AMode: TAndorraTextureMode);
begin
  FYTextureMode := AMode;
end;

function TAndorraImageItem.CompRects(Rect1,Rect2:TRect):boolean;
begin
  result := (Rect1.Left = Rect2.Left) and
            (Rect1.Right = Rect2.Right) and
            (Rect1.Top = Rect2.Top) and
            (Rect1.Bottom = Rect2.Bottom);
end;

procedure TAndorraImageItem.Draw(DestApp:TAndorraApplication;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);
var matTrans1,matTrans2:TD3DXMatrix;
    curx,cury:single;
begin
  with TAndorraApplicationItem(DestApp) do
  begin
    if (FWidth > 0) and (FHeight > 0) and (FImage <> nil) then
    begin

      if not CompRects(SourceRect,FSrcRect) then
      begin
        SetSourceRect(SourceRect);
      end;

      //Set the texture addressing mode.
      if FXTextureMode <> LastXTextureMode then
      begin
        case FXTextureMode of
          amWrap: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSU, D3DTADDRESS_WRAP );
          amMirror: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSU, D3DTADDRESS_MIRROR );
          amClamp: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSU, D3DTADDRESS_CLAMP );
        end;
        LastXTextureMode := FXTextureMode;
      end;

      if FYTextureMode <> LastYTextureMode then
      begin
        case FYTextureMode of
          amWrap: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSV, D3DTADDRESS_WRAP );
          amMirror: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSV, D3DTADDRESS_MIRROR );
          amClamp: Direct3D9Device.SetSamplerState(0, D3DSAMP_ADDRESSV, D3DTADDRESS_CLAMP );
        end;
        LastYTextureMode := FYTextureMode;
      end;

      

      //Initialize "The Matrix"
      D3DXMatrixIdentity(matTrans1);
      D3DXMatrixIdentity(matTrans2);

      //Set Blendmode
      if BlendMode = bmAdd then
      begin
        Direct3D9Device.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_SRCALPHA);
        Direct3D9Device.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_ONE);
      end;
      if BlendMode = bmMask then
      begin
        Direct3D9Device.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_ZERO);
        Direct3D9Device.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_INVSRCALPHA);
      end;
      if BlendMode = bmAlpha then
      begin
        Direct3D9Device.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
        Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
      end;

      //Scale the Box
      D3DXMatrixScaling(matTrans1,(DestRect.Right-DestRect.Left)/FWidth,
        (DestRect.Bottom-DestRect.Top)/FHeight,0);
      D3DXMatrixMultiply(matTrans2,matTrans1,matTrans2);

      if (Rotation <> 0) then
      begin
        CurX := (DestRect.Right-DestRect.Left)*RotCenterX;
        CurY := (DestRect.Bottom-DestRect.Top)*RotCenterY;

        D3DXMatrixTranslation(matTrans1,-CurX,-CurY,0);
        D3DXMatrixMultiply(matTrans2,matTrans2,matTrans1);

        D3DXMatrixRotationZ(matTrans1,Rotation/360*2*PI);
        D3DXMatrixMultiply(matTrans2,matTrans2,matTrans1);

        D3DXMatrixTranslation(matTrans1,CurX,CurY,0);
        D3DXMatrixMultiply(matTrans2,matTrans2,matTrans1);
      end;

      //Translate the Box
      D3DXMatrixTranslation(matTrans1,DestRect.Left,DestRect.Top,0);
      D3DXMatrixMultiply(matTrans2,matTrans2,matTrans1);

      with Direct3D9Device do
      begin
        if FImage <> LastTexture then SetTexture(0,FImage);
        LastTexture := FImage;
        SetTransform(D3DTS_WORLD, matTrans2);
        SetStreamSource(0, FVertexbuffer, 0, SizeOf(TD3DLVertex));
        SetFVF(D3DFVF_TD3DLVertex);
        if not FUseIndexBuffer then
        begin
          DrawPrimitive(D3DPT_TRIANGLESTRIP,0,2);
        end
        else
        begin
          SetIndices(FIndexBuffer);
          DrawIndexedPrimitive( d3dpt_trianglelist, 0, 0, (FDetails+1)*(FDetails+1), 0, FDetails*FDetails*2);
        end;
      end;
    end;
  end;
end;

procedure TAndorraImageItem.LoadTexture(ATexture:TAndorraTexture);
begin
  with TAndorraTextureItem(ATexture) do
  begin
    FImage := ATextureImg;
    FWidth := ATexWidth;
    FHeight := ATexHeight;
    FSrcRect.Left   := 0;
    FSrcRect.Top    := 0;
    FSrcRect.Right  := ATexWidth;
    FSrcRect.Bottom := ATexHeight;
    SetupBuffer;
  end;
end;

function TAndorraImageItem.GetImageInfo;
begin
  result.Width := FWidth;
  result.Height := FHeight;
end;

destructor TAndorraImageItem.Destroy;
begin
  FVertexBuffer := nil;
  FImage := nil;
  inherited Destroy;
end;


//Initialization
function CreateApplication:TAndorraApplication;
begin
  result := TAndorraApplicationItem.Create;
  with TAndorraApplicationItem(result) do
  begin
    //Create Direct 3D Interface
    Direct3D9 := Direct3DCreate9( D3D_SDK_VERSION );
    if Direct3D9 = nil then
    begin
      TAndorraApplicationItem(result).Free;
      result := nil;
    end;
  end;
end;

function D3DColorValue(a,r,g,b:single):TD3DColorValue;
begin
  result.a := a;
  result.r := r;
  result.g := g;
  result.b := b;
end;

function InitDisplay(Appl:TAndorraApplication; AWindow:hWnd; AOptions:TAdDrawModes;
   ADisplay:TAdDrawDisplay):boolean;

type TFormat = record
    aform:TD3DFormat;
    adtype:TD3DDevType;
  end;
   
var
  d3dpp:TD3DPresent_Parameters;
  d3ddm:TD3DDisplayMode;
  d3dcaps9:TD3DCaps9;
  d3ddi:TD3DADAPTER_IDENTIFIER9;
  dtype:TD3DDevType;
  hvp:boolean;
  vp : Integer;
  i:integer;
  hasmode:boolean;
  hr:HRESULT;
  afmt:TFormat;

  function GetType(aform:TD3DFormat;adtype:TD3DDevType):TFormat;
  begin
    result.aform := aform;
    result.adtype := adtype;
  end;

  function GetFormat(avalue:integer;bits:byte):TFormat;
  begin
    if bits = 16 then
    begin
      case avalue of
        0 : result := GetType(D3DFMT_R5G6B5,D3DDEVTYPE_HAL);
        1 : result := GetType(D3DFMT_X1R5G5B5,D3DDEVTYPE_HAL);
        2 : result := GetType(D3DFMT_R5G6B5,D3DDEVTYPE_REF);
        3 : result := GetType(D3DFMT_X1R5G5B5,D3DDEVTYPE_REF);
      end;
    end;
    if bits = 32 then
    begin
      case avalue of
        0 : result := GetType(D3DFMT_A8R8G8B8,D3DDEVTYPE_HAL);
        1 : result := GetType(D3DFMT_X8R8G8B8,D3DDEVTYPE_HAL);
        2 : result := GetType(D3DFMT_A8R8G8B8,D3DDEVTYPE_REF);
        3 : result := GetType(D3DFMT_X8R8G8B8,D3DDEVTYPE_REF);
      end;
    end;
  end;

begin
  result := false;
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      WriteLog(ltNone,'Initialize Andorra Direct3D 9 Plugin.');

      if Direct3D9 = nil then
      begin
        WriteLog(ltFatalError,'Direct3D was not initialized.');
        exit;
      end;

      //Get information about the graphics card.
      Direct3D9.GetAdapterIdentifier(D3DADAPTER_DEFAULT,0,d3ddi);
      WriteLog(ltInfo,PChar('Using card '+d3ddi.Description));

      //Get the device capilities.
      if failed(Direct3D9.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dcaps9)) then
      begin
        WriteLog(ltFatalError,PChar('Error while getting adapter capilities.'));
        exit;
      end;

      //Get the current display mode
      if failed(Direct3D9.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm)) then
      begin
        WriteLog(ltFatalError,PChar('Error while getting current adapter displaymode.'));
        exit;
      end;

      dtype := D3DDEVTYPE_REF;

      Fillchar(d3dpp,sizeof(d3dpp),0);
      with d3dpp do
      begin
        Windowed := not (doFullscreen in AOptions);
        SwapEffect := D3DSWAPEFFECT_DISCARD;

        if not (doVSync in AOptions) then
        begin
          PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
        end;

        if (ADisplay.BitCount = 0) or (Windowed) then
        begin
          BackBufferFormat := d3ddm.Format;
          if Windowed then
          begin
            BackBufferFormat := D3DFMT_UNKNOWN;
          end;
          if doHardware in AOptions then
          begin
            dtype := D3DDEVTYPE_HAL;
          end
          else
          begin
            dtype := D3DDEVTYPE_REF;
          end;
        end
        else
        begin
          hasmode := false;
          for i := 0 to 3 do
          begin
            afmt := GetFormat(i,ADisplay.BitCount);
            hr := Direct3D9.CheckDeviceType (D3DADAPTER_DEFAULT, afmt.adtype, afmt.aform, afmt.aform, false);
            if succeeded(hr) then
            begin
              BackBufferFormat := afmt.aform;
              dtype := afmt.adtype;
              hasmode := true;
              break;
            end;            
          end;
          if not hasmode then
          begin
            WriteLog(ltFatalError,'Couldn''t find a available display format');
            exit;
          end;
        end;
        if not Windowed then
        begin
          BackBufferWidth := ADisplay.Width;
          BackBufferHeight := ADisplay.Height;
          if ADisplay.Freq > 0 then
          begin
            Fullscreen_RefreshRateInHz := ADisplay.Freq;
          end;
        end;
      end;

      hvp := d3dcaps9.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT <> 0;

      if hvp then
        vp := D3DCREATE_HARDWARE_VERTEXPROCESSING
      else
        vp := D3DCREATE_SOFTWARE_VERTEXPROCESSING;

      //Create device
      hr := Direct3D9.CreateDevice(D3DADAPTER_DEFAULT,  dtype, AWindow, vp, @d3dpp, Direct3d9Device);
      if Failed(hr) then
      begin
        WriteLog(ltFatalError,'Couldn''t initialize Direct3DDevice! ');
        exit;
      end
      else
      begin
        result := true;
      end; 

      SizeX := ADisplay.Width;
      SizeY := ADisplay.Height;

      //Set lighting
      SetOptions(Appl,AOptions);
      Direct3D9Device.SetRenderState(D3DRS_AMBIENT, $00FFFFFF);

      //Get the number of lights
      MaxLights := d3dcaps9.MaxActiveLights;
      WriteLog(ltInfo,PChar('Device supports '+inttostr(MaxLights)+' lights'));


      WriteLog(ltInfo,PChar(Inttostr(Direct3D9Device.GetAvailableTextureMem div 1024 div 1024)+'MB Texture Memory on this device.'));

      //Setup Material
      if       
        Failed(Direct3D9Device.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_COLOR1)) or
        Failed(Direct3D9Device.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_COLOR1)) or
        Failed(Direct3D9Device.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_COLOR1)) then
      begin
        WriteLog(ltError,'Can''t set material sources.');
      end;

      //No culling
      if Failed(Direct3D9Device.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE)) then
      begin
        WriteLog(ltError,'Can''t turn culling off.');
        exit;
      end;

      //Enable Texture alphablending
      if
        Failed(Direct3D9Device.SetRenderState(D3DRS_ALPHABLENDENABLE, LongWord(TRUE))) or
        Failed(Direct3D9Device.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA)) or
        Failed(Direct3D9Device.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA)) or
        Failed(Direct3D9Device.SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE)) then
      begin
        WriteLog(ltWarning,'Alphablending is disabled');
      end;

      for i := 0 to 1023 do
      begin
        FLights[i] := false;
      end;

      WriteLog(ltInfo,'Initialization complete.');
    end;
  end;
  result := true;
end;

procedure DestroyApplication(Appl:TAndorraApplication);
begin
  with TAndorraApplicationItem(Appl) do
  begin
    WriteLog(ltInfo,'Finalize Device.');
    Direct3d9 := nil;
    Direct3d9Device := nil;
    WriteLog(ltInfo,'Finalization Complete.');
    Free;
  end;
end;

//Log System
procedure SetLogProc(Appl:TAndorraApplication;ALogProc:TAdLogProc;AAppl:Pointer);
begin
  with TAndorraApplicationItem(Appl) do
  begin
    @LogProc := @ALogProc;
    LogAppl := AAppl;
  end;
end;

//Render Control
procedure BeginScene(Appl:TAndorraApplication);
begin
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      Direct3D9Device.BeginScene;
    end;
  end;
end;

procedure EndScene(Appl:TAndorraApplication);
var ares:cardinal;
    i:integer;
begin
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      Direct3D9Device.GetRenderState(D3DRS_LIGHTING,ares);
      if ares = Cardinal(true) then
      begin
        for i := 0 to 1023 do
        begin
          if FLights[i] then
          begin
            Direct3D9Device.LightEnable(i,false);
          end;
        end;
      end;
      CurrentLights := 0;
      Direct3D9Device.EndScene;
    end;
  end;
end;

procedure Flip(Appl:TAndorraApplication);
begin
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      if Failed(Direct3D9Device.Present(nil, nil, 0, nil)) then
      begin
        WriteLog(ltFatalError,'Error while flipping.');
      end;
    end;
  end;
end;

procedure ClearScene(Appl:TAndorraApplication;AColor:TAndorraColor);
begin
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      Direct3D9Device.Clear( 0, nil, D3DCLEAR_TARGET, D3DCOLOR_ARGB(AColor.a,AColor.r,AColor.g,AColor.b),
        1.0, 0);
    end;
  end;
end;

procedure SetupScene(Appl:TAndorraApplication;AWidth,AHeight:integer);
var pos, dir, up : TD3DXVector3;
    matView, matProj: TD3DXMatrix;
begin
  if Appl <> nil then
  begin
    with TAndorraApplicationItem(Appl) do
    begin
      pos := D3DXVector3 (Awidth/2,AHeight/2,-10);
      dir := D3DXVector3 (Awidth/2,AHeight/2,0);
      up := D3DXVector3 (0,-1,0);

      D3DXMatrixLookAtRH( matView, pos, dir, up);
      Direct3d9Device.SetTransform(D3DTS_VIEW, matView);

      D3DXMatrixOrthoRH( matProj, Awidth, Aheight, 0,100);
      if Failed(Direct3d9Device.SetTransform(D3DTS_PROJECTION, matProj)) then
      begin
        WriteLog(ltError,'Error while setting new view matrix.');
      end
      else
      begin
        WriteLog(ltInfo,'Changed point of view to a 2D Szene.');
      end;
    end;
  end;
end;

procedure SetTextureQuality(Appl:TAndorraApplication;Quality:TAndorraTextureQuality);
begin
  with TAndorraApplicationItem(Appl) do
  begin
    case Quality of
      tqNone: TextureFilter := D3DTEXF_POINT;
      tqLinear: TextureFilter := D3DTEXF_LINEAR;
      tqAnisotropic: TextureFilter := D3DTEXF_ANISOTROPIC;
    end;
    if Failed(Direct3D9Device.SetSamplerState(0,D3DSAMP_MAGFILTER, TextureFilter)) or
       Failed(Direct3D9Device.SetSamplerState(0,D3DSAMP_MINFILTER, TextureFilter)) then
    begin
      WriteLog(ltError,'Error while changing texture quality.')
    end;
  end;
end;

procedure SetOptions(Appl:TAndorraApplication;AOptions:TAdDrawModes);stdcall;
begin
  with TAndorraApplicationItem(Appl) do
  begin
    Direct3D9Device.SetRenderState(D3DRS_LIGHTING,LongWord(doLights in AOptions));
  end;
end;

procedure SetAmbientLight(Appl:TAndorraApplication;AColor:TAndorraColor);stdcall;
begin
  with TAndorraApplicationItem(Appl) do
  begin
    Direct3D9Device.SetRenderState(D3DRS_AMBIENT, D3DColor_ARGB(AColor.a,AColor.r,
      AColor.g,AColor.b));
  end;
end;

//Image Controls

function CreateImage(Appl:TAndorraApplication):TAndorraImage;
begin
  result := TAndorraImageItem.Create(Appl);
end;

procedure DestroyImage(Img:TAndorraImage);
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).Destroy;
  end;
end;

procedure DrawImage(DestApp:TAndorraApplication;Img:TAndorraImage;DestRect,SourceRect:TRect;Rotation:integer;
  RotCenterX,RotCenterY:single;BlendMode:TAndorraBlendMode);
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).Draw(DestApp,DestRect,SourceRect,Rotation,RotCenterX,RotCenterY,Blendmode);
  end;
end;

procedure ImageLoadTexture(Img:TAndorraImage;ATexture:TAndorraTexture);
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).LoadTexture(ATexture);
  end;
end;

function AdColorToD3DColor_ARGB(AColor:TAndorraColor):TD3DColor;
begin
  result := D3DColor_ARGB(AColor.a, AColor.r, AColor.g, AColor.b);
end;

procedure SetImageColor(Img:TAndorraImage;AColor:TAndorraColor);
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).SetColor(AColor);
  end;
end;

procedure SetTextureXMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).SetXTextureMode(AMode);
  end;
end;

procedure SetTextureYMode(Img:TAndorraImage;AMode:TAndorraTextureMode);stdcall;
begin
  if Img <> nil then
  begin
    TAndorraImageItem(Img).SetYTextureMode(AMode);
  end;
end;

function GetTextureInfo(Tex:TAndorraImage):TImageInfo;
begin
  if Tex <> nil then
  begin
    with Result do
    begin
      Width := TAndorraTextureItem(Tex).ATexWidth;
      Height := TAndorraTextureItem(Tex).ATexHeight;
      BaseRect := TAndorraTextureItem(Tex).ABaseRect;
    end;
  end;
end;

procedure SetImageDetail(Img:TAndorraImage;ADetail:integer);
begin
  if img <> nil then
  begin
    TAndorraImageItem(Img).SetDetails(ADetail);
  end;
end;


//Texture Creation

function CheckTextureMem(Appl:TAndorraApplication):integer;stdcall;
begin
  with TAndorraApplicationItem(Appl) do
  begin
    result := Direct3D9Device.GetAvailableTextureMem;
    if result > 1024 then
    begin
      WriteLog(ltInfo,PChar(Inttostr(result div 1024 div 1024)+' MB Texture Memory Available.'));
    end
    else
    begin
      WriteLog(ltWarning,PChar('Only '+Inttostr(result div 1024)+' KB Texture Memory Available.'));
    end;
  end;
end;


function LoadTextureFromFile(Appl:TAndorraApplication;AFile:PChar;ATransparentColor:TAndorraColor):TAndorraTexture;
var Info:TD3DXImageInfo;
begin
  result := TAndorraTextureItem.Create;
  with TAndorraApplicationItem(Appl) do
  begin
    WriteLog(ltInfo,PChar('Load file '+AFile));
    with TAndorraTextureItem(result) do
    begin
      AAppl := Appl;
      if not Failed(D3DXCreateTextureFromFileEx( Direct3D9Device, AFile, D3DX_DEFAULT, D3DX_DEFAULT,
          0, 0, D3DFMT_UNKNOWN, D3DPOOL_DEFAULT, TextureFilter, TextureFilter,
          AdColorToD3DColor_ARGB(ATransparentColor) , @Info, nil, ATextureImg)) then
      begin
        ATexWidth := Info.Width;
        ATexHeight := Info.Height;
        ABaseRect := Rect(0,0,ATexWidth,ATexHeight);
        AFormat := Info.Format;
      end
      else
      begin
        WriteLog(ltError,'Error while creating texture from file.');
        TAndorraTextureItem(result).Free;
        result := nil;
        Exit;
      end;
    end;
    CheckTextureMem(Appl);
  end;
end;

function LoadTextureFromFileEx(Appl:TAndorraApplication;AFile:PChar;AWidth,AHeight:integer;AColorDepth:byte;ATransparentColor:TAndorraColor):TAndorraTexture;
var Info:TD3DXImageInfo;
    Format:TD3DFormat;
begin
  result := TAndorraTextureItem.Create;
  with TAndorraApplicationItem(Appl) do
  begin
    WriteLog(ltInfo,PChar('Load file '+AFile));
    with TAndorraTextureItem(result) do
    begin
      AAppl := Appl;
      case AColorDepth of
        16: Format := D3DFMT_A4R4G4B4;
        32: Format := D3DFMT_A8R8G8B8;
      else
        Format := D3DFMT_UNKNOWN;
      end;
      if not Failed(D3DXCreateTextureFromFileEx( Direct3D9Device, AFile, AWidth,AHeight,
          0, 0, Format, D3DPOOL_DEFAULT, TextureFilter, TextureFilter,
          AdColorToD3DColor_ARGB(ATransparentColor) ,@Info, nil, ATextureImg)) then
      begin
        ATexWidth := Info.Width;
        ATexHeight := Info.Height;
        ABaseRect := Rect(0,0,ATexWidth,ATexHeight);
        AFormat := Info.Format;
      end
      else
      begin
        WriteLog(ltError,'Error while creating texture from file (extended).');
        TAndorraTextureItem(result).Free;
        result := nil;
        Exit;
      end;
    end;
    CheckTextureMem(Appl);
  end;
end;

type TRGBRec = packed record
  r,g,b:byte;
end;

type PRGBRec = ^TRGBRec;

procedure FreeTexture(ATexture:TAndorraTexture);
begin
  with TAndorraTextureItem(ATexture) do
  begin
    with TAndorraApplicationItem(AAppl) do
    begin
      WriteLog(ltInfo,PChar('Free texture...'));
      IDirect3DTexture9(TAndorraTextureItem(ATexture).ATextureImg)._Release;
      CheckTextureMem(TAndorraTextureItem(ATexture).AAppl);
    end;
  end;
end;


//Convert a 8 Bit Color to a 4 Bit Color
function R8ToR4(r:byte):byte;
begin
  result := (r div 16);
end;

//Converts a A8R8G8B8 Value to a A4R4G4B4
function RGBTo16Bit(a,r,g,b:byte):Word;
begin
  Result := (R8ToR4(a) shl 12) or (R8ToR4(r) shl 8)
                        or (R8ToR4(g) shl 4)
                        or R8ToR4(b);
end;

function IsPowerOfTwo(Value: Cardinal): Boolean;
begin
  Result := (Value > 0) and (Value and (Value -1) = 0);
end;

function LoadTextureFromBitmap(Appl:TAndorraApplication;ABitmap:Pointer;AColorDepth:byte):TAndorraTexture;
var d3dlr: TD3DLocked_Rect;
    Cursor32: pLongWord;
    Cursor16: pWord;
    BitCur: PRGBRec;
    x,y:integer;
    a:byte;
    tr,tg,tb:byte;
begin
  //Set Result to nil
  result := TAndorraTextureItem.Create;

  with TAndorraApplicationItem(Appl) do
  begin
    with TBitmap(ABitmap) do
    begin
      with TAndorraTextureItem(Result) do
      begin
        ABaseRect := Rect(0,0,Width,Height);
        //Scale the bitmap to a size power two
        if not IsPowerOfTwo(Height) then
        begin
          Width := 1 shl round(log2(Width));
        end;

        if not IsPowerOfTwo(Height) then
        begin
          Height := 1 shl round(log2(Height));
        end;

        
        AAppl := Appl;
        //Set the Textures Pixel Format
        case AColorDepth of
          16: AFormat := D3DFMT_A4R4G4B4;
          24: AFormat := D3DFMT_A8R8G8B8;
          32: AFormat := D3DFMT_A8R8G8B8;
        else
          AFormat := D3DFMT_A8R8G8B8;
        end;
        ATexWidth := Width;
        ATexHeight := Height;
        //Set the Pixel Format of the Bitmap to 24 Bit
        PixelFormat := pf24Bit;

        tr := 0;
        tg := 0;
        tb := 0;
        
        if Transparent then
        begin
          tr := GetRValue(TransparentColor);
          tg := GetGValue(TransparentColor);
          tb := GetBValue(TransparentColor);
        end;


        //Create the Texture
        if D3DXCreateTexture(Direct3D9Device, Width, Height, 0, 0, AFormat, D3DPOOL_MANAGED, ATextureImg) = D3D_OK then
        begin
          ATextureImg.LockRect(0, d3dlr, nil, 0);

          if (AFormat = D3DFMT_A8R8G8B8) then
          begin
            Cursor32 := d3dlr.pBits;

            for y := 0 to Height-1 do
            begin
              BitCur := Scanline[y];
              for x := 0 to Width-1 do
              begin
                if Transparent and
                   (BitCur^.r = tb) and
                   (BitCur^.g = tg) and
                   (BitCur^.b = tr) then
                begin
                  a := 0;
                end
                else
                begin
                  a := 255;
                end;
                Cursor32^ := D3DColor_ARGB(a,BitCur^.b,BitCur^.g,BitCur^.r);
                inc(BitCur);
                inc(Cursor32);
              end;
            end;
          end;

          if AFormat = D3DFMT_A4R4G4B4 then
          begin
            Cursor16 := d3dlr.pBits;
            for y := 0 to Height-1 do
            begin
              BitCur := Scanline[y];
              for x := 0 to Width-1 do
              begin
                if Transparent and
                   (BitCur^.r = tb) and
                   (BitCur^.g = tg) and
                   (BitCur^.b = tr) then
                begin
                  a := 0;
                end
                else
                begin
                  a := 255;
                end;
                Cursor16^ := RGBTo16Bit(a,BitCur^.b,BitCur^.g,BitCur^.r);
                inc(BitCur);
                inc(Cursor16);
              end;
            end;
          end;
        end
        else
        begin
          WriteLog(ltError,'Error while locking texture.');
        end;
        ATextureImg.UnlockRect(0);
      end;
    end;
    CheckTextureMem(Appl);
  end;
end;

procedure AddTextureAlphaChannel(ATexture:TAndorraTexture;ABitmap:Pointer);
var d3dlr: TD3DLocked_Rect;
    Cursor32: pLongWord;
    Cursor16: pWord;
    BitCur: PRGBRec;
    x,y:integer;
begin
  //Set Result to nil
  with TAndorraTextureItem(ATexture) do
  begin
    with TBitmap(ABitmap) do
    begin
      with TAndorraApplicationItem(AAppl) do
      begin
        //Set the Pixel Format of the Bitmap to 24 Bit
        PixelFormat := pf24Bit;

        ATextureImg.LockRect(0, d3dlr, nil, 0);

        if AFormat = D3DFMT_A8R8G8B8 then
        begin
          Cursor32 := d3dlr.pBits;
          for y := 0 to Height-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to Width-1 do
            begin
              Cursor32^ := (((BitCur^.b+BitCur^.g+BitCur^.r) div 3) shl 24) or (Cursor32^ and $00FFFFFF) ;
              inc(BitCur);
              inc(Cursor32);
            end;
          end;
        end;

        if AFormat = D3DFMT_A4R4G4B4 then
        begin
          Cursor16 := d3dlr.pBits;
          for y := 0 to Height-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to Width-1 do
            begin
              Cursor16^ := (((BitCur^.b+BitCur^.g+BitCur^.r) div 48) shl 12) or (Cursor16^ and $0FFF) ;
              inc(BitCur);
              inc(Cursor16);
            end;
          end;
        end;
      end;
      ATextureImg.UnlockRect(0);
    end;
  end;
end;

procedure GetTextureAsBitmap(ATexture:TAndorraTexture;ABitmap:Pointer);
var d3dlr: TD3DLocked_Rect;
    Cursor32: pLongWord;
    Cursor16: pWord;
    BitCur: PRGBRec;
    x,y:integer;
begin
  //Set Result to nil
  with TAndorraTextureItem(ATexture) do
  begin
    with TBitmap(ABitmap) do
    begin
      with TAndorraApplicationItem(AAppl) do
      begin
        ATextureImg.LockRect(0, d3dlr, nil, 0);

        PixelFormat := pf24Bit;
        Width := ATexWidth;
        Height := ATexHeight;

        if AFormat = D3DFMT_A8R8G8B8 then
        begin
          Cursor32 := d3dlr.pBits;
          for y := 0 to ATexHeight-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to ATexWidth-1 do
            begin
              BitCur^.b := Cursor32^ shr 16;
              BitCur^.g := Cursor32^ shr 8;
              BitCur^.r := Cursor32^;
              inc(BitCur);
              inc(Cursor32);
            end;
          end;
        end;

        if AFormat = D3DFMT_A4R4G4B4 then
        begin
          Cursor16 := d3dlr.pBits;
          for y := 0 to ATexHeight-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to ATexWidth-1 do
            begin
              BitCur.r := ($000F and (Cursor16^ shr 8))*16;
              BitCur.g := ($000F and (Cursor16^ shr 4))*16;
              BitCur.b := ($000F and Cursor16^)*16;
              inc(BitCur);
              inc(Cursor16);
            end;
          end;
        end;
      end;
      ATextureImg.UnlockRect(0);
    end;
  end;
end;

procedure GetTextureAlphaChannelAsBitmap(ATexture:TAndorraTexture;ABitmap:Pointer);stdcall;
var d3dlr: TD3DLocked_Rect;
    Cursor32: pLongWord;
    Cursor16: pWord;
    BitCur: PRGBRec;
    x,y:integer;
begin
  //Set Result to nil
  with TAndorraTextureItem(ATexture) do
  begin
    with TBitmap(ABitmap) do
    begin
      with TAndorraApplicationItem(AAppl) do
      begin
        ATextureImg.LockRect(0, d3dlr, nil, 0);

        PixelFormat := pf24Bit;
        Width := ATexWidth;
        Height := ATexHeight;

        if AFormat = D3DFMT_A8R8G8B8 then
        begin
          Cursor32 := d3dlr.pBits;
          for y := 0 to ATexHeight-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to ATexWidth-1 do
            begin
              BitCur^.b := Cursor32^ shr 24;
              BitCur^.g := Cursor32^ shr 24;
              BitCur^.r := Cursor32^ shr 24;
              inc(BitCur);
              inc(Cursor32);
            end;
          end;
        end;

        if AFormat = D3DFMT_A4R4G4B4 then
        begin
          Cursor16 := d3dlr.pBits;
          for y := 0 to ATexHeight-1 do
          begin
            BitCur := Scanline[y];
            for x := 0 to ATexWidth-1 do
            begin
              BitCur.r := ($000F and (Cursor16^ shr 12))*16;
              BitCur.g := ($000F and (Cursor16^ shr 12))*16;
              BitCur.b := ($000F and (Cursor16^ shr 12))*16;
              inc(BitCur);
              inc(Cursor16);
            end;
          end;
        end;
      end;
      ATextureImg.UnlockRect(0);
    end;
  end;
end;


procedure SetTextureAlpha(Tex:TAndorraTexture;AValue:Byte);
var d3dlr: TD3DLocked_Rect;
    Cursor32: pLongWord;
    Cursor16: pWord;
    x,y:integer;
begin
  //Set Result to nil
  with TAndorraTextureItem(Tex) do
  begin
    with TAndorraApplicationItem(AAppl) do
    begin
      ATextureImg.LockRect(0, d3dlr, nil, 0);

      if AFormat = D3DFMT_A8R8G8B8 then
      begin
        Cursor32 := d3dlr.pBits;
        for y := 0 to ATexHeight-1 do
        begin
          for x := 0 to ATexWidth-1 do
          begin
            Cursor32^ := (AValue shl 24) or (Cursor32^ and $00FFFFFF);
            inc(Cursor32);
          end;
        end;
      end;

      if AFormat = D3DFMT_A4R4G4B4 then
      begin
        Cursor16 := d3dlr.pBits;
        for y := 0 to ATexHeight-1 do
        begin
          for x := 0 to ATexWidth-1 do
          begin
            Cursor16^ := (AValue shl 12) or (Cursor16^ and $0FFF);
            inc(Cursor16);
          end;
        end;
      end;
    end;
    ATextureImg.UnlockRect(0);
  end;
end;


{ TAndorraApplicationItem }

function TAndorraApplicationItem.GetFreeLight: integer;
var i:integer;
begin
  result := -1;
  for i := 0 to 1023 do
  begin
    if not FLights[i] then
    begin
      result := i;
      FLights[i] := true;
      break;
    end;
  end;
end;

procedure TAndorraApplicationItem.ReleaseLight(alight: integer);
begin
  if (alight > 0) and (alight < 1024) then
  begin
    Direct3D9Device.LightEnable(Alight,false);
    FLights[alight] := false;
  end;
end;

procedure TAndorraApplicationItem.WriteLog(Typ: TAdLogTyp; Text: PAnsiChar);
var Temp:TAdLogItem;
begin
  if @LogProc <> nil then
  begin
    Temp.Typ := Typ;
    Temp.Text := Text;
    LogProc(Temp,LogAppl);
  end;
end;

{ TAndorraLightItem }

constructor TAndorraLightItem.Create(Appl: TAndorraApplication);
begin
  inherited Create;
  Alight := TAndorraApplicationItem(Appl).GetFreeLight;
  AAppl := Appl;
end;

destructor TAndorraLightItem.Destroy;
begin
  TAndorraApplicationItem(AAppl).ReleaseLight(ALight);
  inherited Destroy;
end;

function CreateLight(Appl:TAndorraApplication):TAndorraLight;
begin
  result := TAndorraLightItem.Create(Appl);
end;

procedure DestroyLight(ALight:TAndorraLight);
begin
  TAndorraLightItem(ALight).Destroy;
end;

procedure RestoreLight(ALight:TAndorraLight;Data:TLight);
var settings:TD3DLight9;
begin
  if ALight <> nil then
  begin
    with TAndorraLightItem(ALight) do
    begin
      if ALight <> -1 then
      begin
        with TAndorraApplicationItem(AAppl) do
        begin
          with Settings do
          begin
            _Type := D3DLIGHT_POINT;
            Ambient := D3DColorValue(0,Data.Color.R/255,Data.Color.G/255,Data.Color.B/255);
            Position := D3DXVector3(Data.X1,Data.Y1,0);
            Range := Data.Range;
            Attenuation0 := 0;
            Attenuation1 := Data.Falloff/(Range);
            Attenuation2 := 0;
          end;
          Direct3D9Device.SetLight(alight,Settings);
        end;
      end;
    end;
  end;
end;

procedure EnableLight(ALight:TAndorraLight);
var tmp:longbool;
begin
  if (Alight <> nil) then
  begin
    with TAndorraLightItem(ALight) do
    begin
      with TAndorraApplicationItem(AAppl) do
      begin
        if CurrentLights < MaxLights then
        begin
          Direct3D9Device.GetLightEnable(Alight,tmp);
          if not tmp then
          begin
            Direct3D9Device.LightEnable(Alight,true);
            CurrentLights := CurrentLights + 1;
          end;
        end;
      end;
    end;
  end;
end;

procedure DisableLight(ALight:TAndorraLight);
var tmp:longbool;
begin
  if Alight <> nil then
  begin
    with TAndorraLightItem(ALight) do
    begin
      with TAndorraApplicationItem(AAppl) do
      begin
        Direct3D9Device.GetLightEnable(Alight,tmp);
        if tmp then
        begin
          Direct3D9Device.LightEnable(Alight,false);
          CurrentLights := CurrentLights - 1;
        end;
      end;
    end;
  end;
end;   

end.
