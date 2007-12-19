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

unit Ad3DObj;

interface

uses Classes, {$INCLUDE AdTypes.inc}, AdClasses, AdDraws;

type
  TAdMesh = class
    private
      FBuffer:TAd2DMesh;
      FParent:TAdDraw;
      FMatrix:TAdMatrix;
      FRotX,FRotY,FRotZ:single;
      FScaleX,FScaleY,FScaleZ:single;
      FX,FY,FZ:single;
      FWidth,FHeight,FDepth:single;
      FDrawMode:TAd2DDrawMode;
      FBlendMode:TAd2DBlendMode;
      FColor:TAndorraColor;
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
      procedure UpdateMatrix(Index:integer;Value:single);virtual;
      property Width:single index 9 read FWidth write UpdateMatrix;
      property Height:single index 10 read FHeight write UpdateMatrix;
      property Depth:single index 11 read FDepth write UpdateMatrix;
      property Matrix:TAdMatrix read FMatrix write FMatrix;
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure Initialize;
      procedure Finalize;

      procedure CreateMesh;virtual;

      procedure Draw;virtual;

      property Buffer:TAd2DMesh read FBuffer;
      property Parent:TAdDraw read FParent;

      property X:single index 0 read FX write UpdateMatrix;
      property Y:single index 1 read FY write UpdateMatrix;
      property Z:single index 2 read FZ write UpdateMatrix;
      property RotationX:single index 3 read FRotX write UpdateMatrix;
      property RotationY:single index 4 read FRotY write UpdateMatrix;
      property RotationZ:single index 5 read FRotZ write UpdateMatrix;
      property ScaleX:single index 6 read FScaleX write UpdateMatrix;
      property ScaleY:single index 7 read FScaleY write UpdateMatrix;
      property ScaleZ:single index 8 read FScaleZ write UpdateMatrix;
      property Color:TAndorraColor read FColor write FColor;

      property BlendMode:TAd2DBlendMode read FBlendMode write FBlendMode;
      property DrawMode:TAd2DDrawMode read FDrawMode write FDrawMode;

      procedure SaveToStream(AStream:TStream);virtual;
      procedure LoadFromStream(AStream:TStream);virtual;
      procedure SaveToFile(AFile:string);
      procedure LoadFromFile(AFile:string);
  end;

  TAdPlane = class(TAdMesh)
    private
    protected
    public
      procedure CreateMesh;override;
      property Width;
      property Height;
  end;

implementation

{ TAdMesh }

constructor TAdMesh.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);
  Initialize;

  FDrawMode := adTriangleStrips;
  FBlendMode := bmAlpha;
  FColor := Ad_ARGB(255,255,255,255);

  FScaleX := 1;
  FScaleY := 1;
  FScaleZ := 1;
end;

destructor TAdMesh.Destroy;
begin         
  Finalize;
  FParent.UnRegisterNotifyEvent(Notify);
  inherited;
end;

procedure TAdMesh.CreateMesh;
begin
  //Nothig to do now
end;

procedure TAdMesh.Draw;
begin
  Buffer.SetMatrix(FMatrix);
  Buffer.Draw(FBlendMode,FDrawMode);
end;

procedure TAdMesh.Finalize;
begin
  if Buffer <> nil then
  begin
    Buffer.Free;
    FBuffer := nil;
  end;              
end;

procedure TAdMesh.Initialize;
begin
  Finalize;
  FBuffer := FParent.AdAppl.CreateMesh;
end;

procedure TAdMesh.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdMesh.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdMesh.LoadFromStream(AStream: TStream);
var c:integer;
begin
  c := 0;
  if Buffer.Vertices <> nil then
  begin
    c := Length(Buffer.Vertices);
  end;
  AStream.Write(c,SizeOf(c));
  if Buffer.Vertices <> nil then
  begin
    AStream.Write(Buffer.Vertices[0],SizeOf(TAdVertex)*c)
  end;
  c := 0;
  if Buffer.IndexBuffer <> nil then
  begin
    c := Length(Buffer.IndexBuffer);
  end;
  AStream.Write(c,SizeOf(c));
  if Buffer.IndexBuffer <> nil then
  begin
    AStream.Write(Buffer.IndexBuffer[0],SizeOf(TAdVertex)*c)
  end;
end;

procedure TAdMesh.SaveToStream(AStream: TStream);
var c:integer;
    vert:TAdVertexArray;
    inde:TAdIndexArray;
begin
  AStream.Read(c,SizeOf(c));
  SetLength(Buffer.Vertices,c);
  AStream.Read(Buffer.Vertices[0],SizeOf(TAdVertex)*c);
  SetLength(Buffer.Indices,c);
  AStream.Read(Buffer.Indices[0],SizeOf(TAdVertex)*c);
end;

procedure TAdMesh.Notify(ASender: TObject; AEvent: TSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

procedure TAdMesh.UpdateMatrix(Index: integer; Value: single);
var Mat:TAdMatrix;
begin
  case Index of
    0:FX := Value;
    1:FY := Value;
    2:FZ := Value;
    3:FRotX := Value;
    4:FRotY := Value;
    5:FRotZ := Value;
    6:FScaleX := Value;
    7:FScaleY := Value;
    8:FScaleZ := Value;
    9:FWidth := Value;
   10:FHeight := Value;
   11:FDepth := Value;
  end;

  FMatrix := AdMatrix_Identity;
  Mat := AdMatrix_Scale(FScaleX,FScaleY,FScaleZ);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationX(FRotX);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationY(FRotY);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationZ(FRotZ);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_Translate(FX+Width / 2,FY+Height / 2,FZ+Depth / 2);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
end;

{ TAdPlane }

procedure TAdPlane.CreateMesh;
var vert:TAdVertexArray;
begin
  SetLength(vert,4);
  vert[0].Position := AdVector3(-Width / 2, Height / 2,0);
  vert[0].Color := Color;
  vert[0].Normal := AdVector3(0,0,-1);
  vert[0].Texture := AdVector2(0,1);

  vert[1].Position := AdVector3(-Width / 2, -Height / 2,0);
  vert[1].Color := Color;
  vert[1].Normal := AdVector3(0,0,-1);
  vert[1].Texture := AdVector2(0,0);

  vert[2].Position := AdVector3(Width / 2, Height / 2,0);
  vert[2].Color := Color;
  vert[2].Normal := AdVector3(0,0,-1);
  vert[2].Texture := AdVector2(1,1);

  vert[3].Position := AdVector3(Width / 2, -Height / 2,0);
  vert[3].Color := Color;
  vert[3].Normal := AdVector3(0,0,-1);
  vert[3].Texture := AdVector2(1,0);

  Buffer.Vertices := Vertices;
  Buffer.IndexBuffer := nil;
  Buffer.PrimitiveCount := 2;
  Buffer.Update;

  UpdateMatrix(-1,0);
end;

end.
