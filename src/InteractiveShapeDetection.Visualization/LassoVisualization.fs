namespace InteractiveShapeDetection.Visualization
open FShade
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
open System.IO
open Aardvark.Database
open InteractiveShapeDetection.Base  
open InteractiveShapeDetection.Interaction


module LassoVisualization =

    module Sg = 

        let ofLasso (framebufferSignature : IFramebufferSignature)(lineWidth : IMod<float>)(viewportSize : IMod<V2i>)(color : IMod<C4f>)(lasso : InteractiveShapeDetection.Interaction.Lasso) =              
            let vertices = 
                adaptive{
                    let! preview = lasso.Contour 
                        
                    return preview |> List.toArray |> Array.map ( fun p -> V3f(p.X, p.Y, -1.0))
                }


            Sg.draw IndexedGeometryMode.LineStrip
            |> Sg.trafo     Trafo3d.Identity'
            |> Sg.projTrafo Trafo3d.Identity'
            |> Sg.viewTrafo Trafo3d.Identity'
            |> Sg.vertexAttribute DefaultSemantic.Positions vertices
            |> Sg.surface ( Surfaces.ThickLineSurface framebufferSignature color |> Mod.constant)
            |> Sg.uniform "LineWidth" lineWidth
            |> Sg.uniform "ViewportSize" viewportSize