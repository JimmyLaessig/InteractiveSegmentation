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
open System.Diagnostics

module VolumetricBrushVisualization =

    module Sg = 

        let ofVolumetricBrush (framebufferSignature : IFramebufferSignature)(color : IMod<C4f>)(renderPass : RenderPass)(brush : InteractiveShapeDetection.Interaction.VolumetricBrush) =              
            
            let sphereIg = IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.OOO, 1.0)) 10 (C4b.Black)
            let cylinderIg = IndexedGeometryPrimitives.solidCylinder (V3d.OOO) -V3d.OOI 1.0 1.0 1.0  10 (C4b.Black)
            
            let sphereTrafos,cylinderTrafos  = 
                let trafos = 
                    adaptive{

                        let! radius = brush.Radius
                        let! trajectory = brush.Trajectory
                        
                        let cylinderTrafos = 
                            if trajectory |> List.length < 1 then 
                                [||]
                            else
                                trajectory  |> List.pairwise
                                            |> List.toArray
                                            |> Array.map( fun ((_,wp1),(_,wp0)) ->

                                                let S = Trafo3d.Scale(radius, radius, V3d.Distance(wp0, wp1))
                                                
                                                let RT = CameraView.LookAt (wp0, wp1, V3d.OOI) |> CameraView.viewTrafo
                                                S * RT.Inverse
                                                )

                        let sphereTrafos = 

                            let S = Trafo3d.Scale(radius)
                            trajectory  |> List.toArray 
                                        |> Array.map(fun (_, wp) -> S *  Trafo3d.Translation(wp))

                        return sphereTrafos, cylinderTrafos
                        }   
                trafos |> Mod.map fst , trafos |> Mod.map snd
                     

            let sg1 = cylinderIg    |> Sg.instancedGeometry cylinderTrafos
            let sg2 = sphereIg      |> Sg.instancedGeometry sphereTrafos

            [sg1 ; sg2] |> Sg.ofList
                        |> Sg.surface (Surfaces.Instanced.UniformColor framebufferSignature color |> Mod.constant)
                        |> Sg.pass renderPass    
                        |> Sg.writeBuffers'  (Set.singleton DefaultSemantic.Colors)
                       
            
