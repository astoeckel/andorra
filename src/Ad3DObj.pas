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
* File: Ad3dObj.pas
* Comment: This unit contains the 3D-Object generation code
}

{This unit contains some 3D-Object classes.}
unit Ad3DObj;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  AdTypes, AdClasses, AdMath, AdDraws;

type
  TAdMesh = class(TAdRenderingObject)
    private
      FX, FY, FZ: Single;
      FScaleX, FScaleY, FScaleZ: Single;
      FRotX, FRotY, FRotZ: Single;       
      FParent: TAdDraw;
      FMesh: TAd2DMesh;
      FMatrix: TAdMatrix;
      FTexture: TAdCustomTexture;
      FDrawMode: TAd2dDrawMode;
      FMatrixChanged: boolean;

      procedure SetCoeff(AIndex: integer; AValue: single);
      procedure SetMatrix(AMatrix: TAdMatrix);
      procedure SetTexture(ATexture: TAdCustomTexture);
      procedure Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
    protected
      procedure BuildMatrix;
      procedure Initialize;
      procedure Finalize;
      
      procedure LoadMeshData;virtual;abstract;

      property Mesh: TAd2DMesh read FMesh;
      property DrawMode: TAd2dDrawMode read FDrawMode write FDrawMode;
    public
      constructor Create(AParent: TAdDraw);virtual;
      destructor Destroy;override;
      
      procedure Draw(ASurface: TAdSurface; ABlendMode: TAd2dBlendMode = bmAlpha);virtual;
      
      property Parent: TAdDraw read FParent write FParent;
      property Texture: TAdCustomTexture read FTexture write SetTexture;

      property X: single index 0 read FX write SetCoeff;
      property Y: single index 1 read FY write SetCoeff;
      property Z: single index 2 read FZ write SetCoeff;
      property ScaleX: single index 3 read FScaleX write SetCoeff;
      property ScaleY: single index 4 read FScaleY write SetCoeff;
      property ScaleZ: single index 5 read FScaleZ write SetCoeff;
      property RotationX: single index 6 read FRotX write SetCoeff;
      property RotationY: single index 7 read FRotY write SetCoeff;
      property RotationZ: single index 8 read FRotZ write SetCoeff;
      property Matrix: TAdMatrix read FMatrix write SetMatrix;
  end;

  TAdCubeMesh = class(TAdMesh)
    private
      FWidth, FHeight, FDepth: single;
      FColor: TAndorraColor;
      procedure SetSizeCoeff(AIndex: integer; AValue: single);
      procedure SetColor(AColor: TAndorraColor);
    protected
      procedure LoadMeshData;override;
    public
      constructor Create(AParent: TAdDraw);override;
      
      property Width: single index 0 read FWidth write SetSizeCoeff;
      property Height: single index 1 read FHeight write SetSizeCoeff;
      property Depth: single index 2 read FDepth write SetSizeCoeff;
      property Color: TAndorraColor read FColor write SetColor;
  end;


implementation 

{ TAdMesh }

constructor TAdMesh.Create(AParent: TAdDraw);
begin
  inherited Create;

  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);

  //Set some presets
  FX := 0; FY := 0; FZ := 0;
  FScaleX := 1; FScaleY := 1; FScaleZ := 1;
  FRotX := 0; FRotY := 0; FRotZ := 0;

  //Set the default draw mode
  FDrawMode := adTriangles;

  //Initialize the mesh
  Initialize;
end;

destructor TAdMesh.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);

  //Finalize the mesh
  Finalize;
  inherited;
end;

procedure TAdMesh.Initialize;
begin
  //Finalize the mesh object to be sure that no memory leaks will be created
  Finalize;

  //Create a new mesh object
  FMesh := FParent.AdAppl.CreateMesh;
  Texture := FTexture;

  //Create the translation matrix
  BuildMatrix;

  //Tell classes derived from this class, to load its mesh data
  LoadMeshData;
end;

procedure TAdMesh.Finalize;
begin
  //Free the mesh if it was already initialized
  if FMesh <> nil then
    FMesh.Free;

  FMesh := nil;
end;

procedure TAdMesh.Draw(ASurface: TAdSurface; ABlendMode: TAd2dBlendMode);
begin
  if FMatrixChanged then
    BuildMatrix;

  //Activate the surface on which the mesh should be drawn
  if ASurface <> nil then
    ASurface.Activate;
    
  //Draw the mesh with the given paremeters
  FMesh.Draw(ABlendMode, FDrawMode);
end;

procedure TAdMesh.Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    //Use the initialized event, to be sure, that the texture has been properly
    //initialized
    seInitialized: Initialize;
    seFinalize: Finalize;
  end;
end;

procedure TAdMesh.SetCoeff(AIndex: integer; AValue: single);
begin
  //Set the mesh translation coefficient that has been changed
  case AIndex of
    0: FX := AValue;
    1: FY := AValue;
    2: FZ := AValue;
    3: FScaleX := AValue;
    4: FScaleY := AValue;
    5: FScaleZ := AValue;
    6: FRotX := AValue;
    7: FRotY := AValue;
    8: FRotZ := AValue;
  end;

  //Rebuild the model matrix of the object the next time the object is rendered
  FMatrixChanged := true;
end;

procedure TAdMesh.SetMatrix(AMatrix: TAdMatrix);
begin
  FMatrix := AMatrix;
  FMesh.Matrix := AMatrix;
end;

procedure TAdMesh.SetTexture(ATexture: TAdCustomTexture);
begin
  FTexture := ATexture;

  //Deactivate texturing if textures are switched off
  if FTexture = nil then
    FMesh.Texture := nil
  else
    FMesh.Texture := ATexture.Texture;
end;

procedure TAdMesh.BuildMatrix;
var
  FMat1, FMat2: TAdMatrix;
begin
  //Calculate the rotation matrix
  FMat1 := AdMatrix_Rotation(FRotX, FRotY, FRotZ);

  //Calculate the scaling matrix
  FMat2 := AdMatrix_Scale(FScaleX, FScaleY, FScaleZ);
  
  //Calculate the rotation+scale matrix
  FMat1 := AdMatrix_Multiply(FMat1, FMat2);

  //Calculate the translation matrix
  FMat2 := AdMatrix_Translate(FX, FY, FZ);

  //Calculate the rotation+scale+translation matrix
  FMat1 := AdMatrix_Multiply(FMat1, FMat2);

  Matrix := FMat1;
  FMatrixChanged := false;
end;

{ TAdCubeMesh }

constructor TAdCubeMesh.Create(AParent: TAdDraw);
begin
  //Set the default size coefficients
  FWidth := 100;
  FHeight := 100;
  FDepth := 100;

  //Set the default color
  FColor := Ad_ARGB(255, 255, 255, 255);

  inherited;
end;

procedure TAdCubeMesh.LoadMeshData;
var
  vert: TAdVertexArray;
  hx, hy, hz: single;
  i: Integer;
begin
  SetLength(vert, 36);

  hx := FWidth / 2;
  hy := FHeight / 2;
  hz := FDepth / 2;

  //Front side of the cube
  vert[0].Position := AdVector3(-hx, -hy, -hz);
  vert[0].Texture := AdVector2(0, 0);
  vert[0].Normal := AdVector3(0, 0, -1);
  
  vert[1].Position := AdVector3( hx, -hy, -hz);
  vert[1].Texture := AdVector2(1, 0);
  vert[1].Normal := AdVector3(0, 0, -1);
  
  vert[2].Position := AdVector3(-hx,  hy, -hz);
  vert[2].Texture := AdVector2(0, 1);
  vert[2].Normal := AdVector3(0, 0, -1);

  vert[3].Position := AdVector3( hx, -hy, -hz);
  vert[3].Texture := AdVector2(1, 0);
  vert[3].Normal := AdVector3(0, 0, -1);

  vert[4].Position := AdVector3( hx,  hy, -hz);
  vert[4].Texture := AdVector2(1, 1);
  vert[4].Normal := AdVector3(0, 0, -1);

  vert[5].Position := AdVector3(-hx,  hy, -hz);
  vert[5].Texture := AdVector2(0, 1);
  vert[5].Normal := AdVector3(0, 0, -1);

  //Left side of the cube
  vert[6].Position := AdVector3(-hx, -hy,  hz);
  vert[6].Texture := AdVector2(1, 0);
  vert[6].Normal := AdVector3(-1, 0, 0);

  vert[7].Position := AdVector3(-hx, -hy, -hz);
  vert[7].Texture := AdVector2(0, 0);
  vert[7].Normal := AdVector3(-1, 0, 0);

  vert[8].Position := AdVector3(-hx,  hy, -hz);
  vert[8].Texture := AdVector2(0, 1);
  vert[8].Normal := AdVector3(-1, 0, 0);

  vert[9].Position := AdVector3(-hx,  hy,  hz);
  vert[9].Texture := AdVector2(1, 1);
  vert[9].Normal := AdVector3(-1, 0, 0);

  vert[10].Position := AdVector3(-hx, -hy,  hz);
  vert[10].Texture := AdVector2(1, 0);
  vert[10].Normal := AdVector3(-1, 0, 0);

  vert[11].Position := AdVector3(-hx,  hy, -hz);
  vert[11].Texture := AdVector2(0, 1);
  vert[11].Normal := AdVector3(-1, 0, 0);

  //Right side of the cube
  vert[12].Position := AdVector3(hx, -hy, -hz);
  vert[12].Texture := AdVector2(0, 0);
  vert[12].Normal := AdVector3(1, 0, 0);

  vert[13].Position := AdVector3(hx, -hy,  hz);
  vert[13].Texture := AdVector2(1, 0);
  vert[13].Normal := AdVector3(1, 0, 0);

  vert[14].Position := AdVector3(hx,  hy, -hz);
  vert[14].Texture := AdVector2(0, 1);
  vert[14].Normal := AdVector3(1, 0, 0);

  vert[15].Position := AdVector3(hx, -hy,  hz);
  vert[15].Texture := AdVector2(1, 0);
  vert[15].Normal := AdVector3(1, 0, 0);

  vert[16].Position := AdVector3(hx,  hy,  hz);
  vert[16].Texture := AdVector2(1, 1);
  vert[16].Normal := AdVector3(1, 0, 0);

  vert[17].Position := AdVector3(hx,  hy, -hz);
  vert[17].Texture := AdVector2(0, 1);
  vert[17].Normal := AdVector3(1, 0, 0);

  //Back side of the cube
  vert[18].Position := AdVector3( hx, -hy, hz);
  vert[18].Texture := AdVector2(1, 0);
  vert[18].Normal := AdVector3(0, 0, 1);

  vert[19].Position := AdVector3(-hx, -hy, hz);
  vert[19].Texture := AdVector2(0, 0);
  vert[19].Normal := AdVector3(0, 0, 1);

  vert[20].Position := AdVector3(-hx,  hy, hz);
  vert[20].Texture := AdVector2(0, 1);
  vert[20].Normal := AdVector3(0, 0, 1);

  vert[21].Position := AdVector3( hx,  hy, hz);
  vert[21].Texture := AdVector2(1, 1);
  vert[21].Normal := AdVector3(0, 0, 1);

  vert[22].Position := AdVector3( hx, -hy, hz);
  vert[22].Texture := AdVector2(1, 0);
  vert[22].Normal := AdVector3(0, 0, 1);

  vert[23].Position := AdVector3(-hx,  hy, hz);
  vert[23].Texture := AdVector2(0, 1);
  vert[23].Normal := AdVector3(0, 0, 1);

  //Top side of the cube
  vert[24].Position := AdVector3(-hx, -hy, -hz);
  vert[24].Texture := AdVector2(0, 0);
  vert[24].Normal := AdVector3(0, -1, 0);

  vert[25].Position := AdVector3(-hx, -hy,  hz);
  vert[25].Texture := AdVector2(0, 1);
  vert[25].Normal := AdVector3(0, -1, 0);

  vert[26].Position := AdVector3( hx, -hy, -hz);
  vert[26].Texture := AdVector2(1, 0);
  vert[26].Normal := AdVector3(0, -1, 0);

  vert[27].Position := AdVector3( hx, -hy, -hz);
  vert[27].Texture := AdVector2(1, 0);
  vert[27].Normal := AdVector3(0, -1, 0);

  vert[28].Position := AdVector3(-hx, -hy, hz);
  vert[28].Texture := AdVector2(0, 1);
  vert[28].Normal := AdVector3(0, -1, 0);

  vert[29].Position := AdVector3( hx, -hy, hz);
  vert[29].Texture := AdVector2(1, 1);
  vert[29].Normal := AdVector3(0, -1, 0);

  //Bottom side of the cube
  vert[30].Position := AdVector3(-hx, hy,  hz);
  vert[30].Texture := AdVector2(0, 1);
  vert[30].Normal := AdVector3(0, 1, 0);

  vert[31].Position := AdVector3(-hx, hy, -hz);
  vert[31].Texture := AdVector2(0, 0);
  vert[31].Normal := AdVector3(0, 1, 0);

  vert[32].Position := AdVector3( hx, hy, -hz);
  vert[32].Texture := AdVector2(1, 0);
  vert[32].Normal := AdVector3(0, 1, 0);

  vert[33].Position := AdVector3(-hx, hy, hz);
  vert[33].Texture := AdVector2(0, 1);
  vert[33].Normal := AdVector3(0, 1, 0);

  vert[34].Position := AdVector3( hx, hy, -hz);
  vert[34].Texture := AdVector2(1, 0);
  vert[34].Normal := AdVector3(0, 1, 0);

  vert[35].Position := AdVector3( hx, hy, hz);
  vert[35].Texture := AdVector2(1, 1);
  vert[35].Normal := AdVector3(0, 1, 0);

  for i := 0 to High(vert) do
    vert[i].Color := FColor;

  //Store the generated vertex/index data
  Mesh.Vertices := vert;
  Mesh.Indices := nil;
  Mesh.PrimitiveCount := 12;
  Mesh.Update;
end;

procedure TAdCubeMesh.SetColor(AColor: TAndorraColor);
begin
  FColor := AColor;
  LoadMeshData;
end;

procedure TAdCubeMesh.SetSizeCoeff(AIndex: integer; AValue: single);
begin
  case AIndex of
    0: FWidth := AValue;
    1: FHeight := AValue;
    2: FDepth := AValue;  
  end;

  LoadMeshData;
end;

end.
