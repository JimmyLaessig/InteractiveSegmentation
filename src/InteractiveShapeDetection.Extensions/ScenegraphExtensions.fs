[<AutoOpen>]
module ScenegraphExtensions

    open System
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics


    module Sg =
    

//        let fullscreenQuad (trafo: IMod<Trafo3d>)=
//                Sg.draw IndexedGeometryMode.TriangleStrip           
//                    |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant  [|V3f(-1.0,-1.0,1.0); V3f(1.0,-1.0,1.0); V3f(-1.0,1.0,1.0);V3f(1.0,1.0,1.0) |])
//                    |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])
//                    |> Sg.trafo trafo

        let ofRay3d (ray : IMod<Ray3d>)(color : IMod<C4f>)(maxDistance : IMod<float>) =
            
            let positions   = Mod.map2 (fun (r: Ray3d) (d : float) -> [|V3f(r.Origin); V3f(r.Origin + r.Direction.Normalized * d)|]) ray maxDistance
            let colors      = Mod.map (fun c -> [|c;c|])  color
            Sg.draw IndexedGeometryMode.LineStrip
                |> Sg.vertexAttribute DefaultSemantic.Positions positions

                |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.uniformColor color |> toEffect]
            

        let fullscreenQuadTextured(texture : IMod<ITexture> )(trafo: IMod<Trafo3d>) = 
            Sg.fullScreenQuad 
                |> Sg.trafo trafo
                |> Sg.effect    [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.diffuseTexture |> toEffect ]
                |> Sg.viewTrafo Trafo3d.Identity'
                |> Sg.projTrafo Trafo3d.Identity'
                |> Sg.texture   DefaultSemantic.DiffuseColorTexture (texture) 


        let frustum' (f: IMod<CameraView>) (proj : IMod<Frustum>) = 
            let invViewProj = Mod.map2 (fun v p -> (CameraView.viewTrafo v * Frustum.projTrafo p).Inverse) f proj

            let positions = 
                [|
                    V3f(-1.0,-1.0,-1.0)
                    V3f( 1.0,-1.0,-1.0)
                    V3f( 1.0, 1.0,-1.0)
                    V3f(-1.0, 1.0,-1.0)
                    V3f(-1.0,-1.0, 1.0)
                    V3f( 1.0,-1.0, 1.0)
                    V3f( 1.0, 1.0, 1.0)
                    V3f(-1.0, 1.0, 1.0)
                |]

            let indices =
                [|
                    1;2; 2;6; 6;5; 5;1;
                    2;3; 3;7; 7;6; 4;5; 
                    7;4; 3;0; 0;4; 0;1;
                |]

            let geometry =
                IndexedGeometry(
                    Mode = IndexedGeometryMode.LineList,
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                            DefaultSemantic.Colors, Array.create indices.Length C4b.Red :> Array
                        ]
                )

            geometry
                |> Sg.ofIndexedGeometry
                |> Sg.trafo invViewProj