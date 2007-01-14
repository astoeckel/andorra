{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DllMain.pas
* Comment: Only a DLL template
}

unit DllMain;

interface

uses d3XX9, Direct3D9, AdClasses, Classes, Windows, Graphics, Math, SysUtils;

type
  TXXApplication = class(TAd2DApplication)
    private
    protected
      procedure SetOptions(AValue:TAdOptions);override;
    public
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

  TXXMesh = class(TAd2DMesh)
    private
    protected
      procedure SetVertices(AVertices:TAdVertexArray);override;
      procedure SetIndex(AIndex:TAdIndexArray);override;
      procedure SetTexture(ATexture:TAd2DTexture);override;
      function GetLoaded:boolean;override;
    public
      procedure SetMatrix(AMatrix:TAdMatrix);override;
      constructor Create(AParent:TXXApplication);
      destructor Destroy;override;
      procedure Draw(ABlendMode:TAd2DBlendMode);override;
      procedure Update;override;
  end;

  TXXBitmapTexture = class(TAd2DBitmapTexture)
    private
    protected
      function GetLoaded:boolean;override;
    public
      constructor Create(AParent:TXXApplication);
      destructor Destroy;override;
      procedure FlushTexture;override;
      procedure LoadFromBitmap(ABmp:TAdBitmap;ABitDepth:byte=32);override;
      procedure SaveToBitmap(ABmp:TAdBitmap);override;
  end;

implementation

{ TXXApplication }

constructor TXXApplication.Create;
begin
  inherited;
end;

destructor TXXApplication.Destroy;
begin
  inherited;
end;

function TXXApplication.CreateMesh: TAd2DMesh;
begin
  result := TXXMesh.Create(self);
end;

function TXXApplication.CreateBitmapTexture: TAd2DBitmapTexture;
begin
  result := TXXBitmapTexture.Create(self);
end;

{function TXXApplication.CreateLight: TAdLight;
begin

end;

function TXXApplication.CreateRenderTargetTexture: TAdRenderTargetTexture;
begin

end;    }

function TXXApplication.Initialize(AWnd: LongWord; AOptions: TAdOptions;
  ADisplay: TAdDisplay):boolean;
begin
  //Initialize the graphic system
end;

procedure TXXApplication.Finalize;
begin
  //Finalize the graphic system
end;

procedure TXXApplication.SetOptions(AValue: TAdOptions);
begin
  //Sets the options (wether Antialiasing or Lights etc. are turned on or off.)
end;

procedure TXXApplication.Setup2DScene(AWidth, AHeight: integer);
begin
  //Sets the projection and the view matrix to 2D
end;

{procedure TXXApplication.SetRenderTarget(ATarget: TAdRenderTargetTexture);
begin
  inherited;

end;}


procedure TXXApplication.BeginScene;
begin
end;

procedure TXXApplication.EndScene;
begin
  //Ends the scene and turns all lights off.
end;

procedure TXXApplication.Flip;
begin
end;

procedure TXXApplication.ClearSurface(AColor: TAndorraColor);
begin
end;

{ TXXMesh }

constructor TXXMesh.Create(AParent: TXXApplication);
begin
  inherited Create;
end;

destructor TXXMesh.Destroy;
begin
  inherited Destroy;
end;

procedure TXXMesh.Draw(ABlendMode:TAd2DBlendMode);
begin
  //Draws the Mesh with the texture and the matrix.
end;

function TXXMesh.GetLoaded: boolean;
begin
  //A mesh is loaded if the vertexbuffer is filled with data and the mesh can be drawn.
end;

procedure TXXMesh.SetIndex(AIndex: TAdIndexArray);
begin
  //Copys the data from AIndex into FIndices
end;

procedure TXXMesh.SetMatrix(AMatrix: TAdMatrix);
begin
  //Sets the transformation Matrix
end;

procedure TXXMesh.SetTexture(ATexture: TAd2DTexture);
begin
  //Sets the texture of a TXXMesh
  inherited SetTexture(ATexture);
end;

procedure TXXMesh.SetVertices(AVertices: TAdVertexArray);
begin
  //Copys the data from AVertices into FVertices
end;

procedure TXXMesh.Update;
begin
  //Takes the data from FVertices and FIndices and writes it into the graphic system's vertex- and indexbuffer.
  //A new buffer will only be created if the count of the vertices/indices has changed.
end;

{ TXXBitmapTexture }

constructor TXXBitmapTexture.Create(AParent: TXXApplication);
begin
  inherited Create;
  //Creates a BitmapTexture
end;

destructor TXXBitmapTexture.Destroy;
begin
  //Destroys the BitmapTexture and frees its memory
  inherited Destroy;
end;

procedure TXXBitmapTexture.FlushTexture;
begin
  //Frees the textures if a texture is loaded
end;

function TXXBitmapTexture.GetLoaded: boolean;
begin
  //Returns true if a texture is loaded.
end;

procedure TXXBitmapTexture.LoadFromBitmap(ABmp: TAdBitmap; ABitDepth: byte);
begin
  //Loads the texture from a TAdBitmap.
  //The texture can be created in a A4R4G4B4 (16 Bit) and a A8R8G8B8 (32 Bit) mode.
end;

procedure TXXBitmapTexture.SaveToBitmap(ABmp: TAdBitmap);
begin
  //Saves the texture back into a TAdBitmap
end;

end.
