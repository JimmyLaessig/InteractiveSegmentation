namespace InteractiveShapeDetection.Visualization

open System
open Aardvark.Base
open Aardvark.Base.Native
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading
open Aardvark.Git
open Aardvark.Database


open InteractiveShapeDetection
open InteractiveShapeDetection.Base
open FShade


module LodIncreaseVisualization =

   
    
    let private vertexColor (f : Surfaces.Shaders.Fragment ) =
            fragment {
                return f.color * 1.2 
            }



    let Init (windowSize : IMod<V2i>)(pointSize :IMod<float>)(lodIncrease : IMod<Point[]>) = 

   
        adaptive {
                let! points = lodIncrease

                match points with
                | [||]  -> 
                   
                    return Sg.ofList []
                | _     -> 
                    let (p,n,c,i) = points  |> Array.map ( fun p -> V3f p.Position, V3f p.Normal, p.Color,  V2f(0.0))
                                            |> Array.unzip4
                    
                    let geometry = 
                        IndexedGeometry(
                            Mode = IndexedGeometryMode.PointList,
                            IndexedAttributes =
                                SymDict.ofList [
                                    DefaultSemantic.Positions,                  p :> Array
                                    DefaultSemantic.Normals,                    n :> Array
                                    DefaultSemantic.Colors,                     c :> Array
                                    DefaultSemantic.DiffuseColorCoordinates,    i :> Array
                                ]
                            )
                    return geometry |> Sg.ofIndexedGeometry 
            }
            |> Sg.dynamic
            |> Sg.uniform "ViewportSize" windowSize
            |> Sg.uniform "PointSize"    pointSize
            |> Sg.effect  [
                DefaultSurfaces.trafo   |> toEffect
                Surfaces.Shaders.sphereImposterGeometry |> toEffect
                Surfaces.Shaders.sphereImposterFragment |> toEffect
                Surfaces.Shaders.simpleLighting         |> toEffect
                vertexColor                             |> toEffect
            ]