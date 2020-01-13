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
open InteractiveShapeDetection.Interaction
open InteractiveShapeDetection.Base
open FShade


module ClusterVisualization =


    let Init (framebufferSignature : IFramebufferSignature)(color : IMod<C4f>)(useBounds : IMod<bool>)(cluster : IMod<Cluster option>) = 
       
        adaptive {
            let! cluster    = cluster
            let! useBounds = useBounds

            let shapes      = 
                if useBounds then 
                    cluster |> Option.toArrayOfT(fun cluster -> [|cluster.bounds|])
                else
                    cluster |> Option.toArrayOfT(fun cluster -> cluster.shapes)
                            |> Array.concat
                    

            let sg  = shapes    |> Array.map (Sg.ofPrimitiveShape)
                                |> Sg.ofArray     
            return sg    
        }
        |> Sg.dynamic
        |> Sg.surface (color |> Surfaces.UniformColor framebufferSignature |> Mod.constant)
        |> Sg.blendMode (Mod.constant BlendMode.Blend)
        |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))