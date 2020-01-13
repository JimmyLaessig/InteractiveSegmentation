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
open Aardvark.Base.Rendering.Effects

open InteractiveShapeDetection
open InteractiveShapeDetection.Interaction
open InteractiveShapeDetection.Base
open FShade


module Sg =
    
    type UniformScope with
        member x.lineLength : float = x?lineLength

    let normalLine (p : Point<Vertex>) =
            line {
               
                let wp0 = p.Value.wp
                let wp1 = p.Value.wp + V4d(p.Value.n * uniform.lineLength , 0.0)

                let p0 = uniform.ViewProjTrafo * wp0
                let p1 = uniform.ViewProjTrafo * wp1


                yield { p.Value with c = p.Value.c; wp = wp0 ; pos = p0  ; tc = p.Value.tc }
                yield { p.Value with c = p.Value.c; wp = wp1 ; pos = p1  ; tc = p.Value.tc }
            }       


    let Normals (framebufferSignature : IFramebufferSignature)(color : IMod<C4f>)(length : IMod<float>)(geometry : IMod<IndexedGeometry>) = 
        
        let surface = [DefaultSurfaces.trafo |> toEffect ; normalLine |> toEffect ; DefaultSurfaces.uniformColor color |> toEffect ] |> Surfaces.compile framebufferSignature |> Mod.constant

        adaptive {
            let! geometry = geometry

            let pointGeometry               = IndexedGeometry()
            
            pointGeometry.IndexArray        <- geometry.IndexArray
            pointGeometry.IndexedAttributes <- geometry.IndexedAttributes
            pointGeometry.Mode              <- IndexedGeometryMode.PointList
             
            return pointGeometry |> Sg.ofIndexedGeometry
        }
        |> Sg.dynamic
        |> Sg.surface surface
        |> Sg.uniform "lineLength" length
        |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))
    