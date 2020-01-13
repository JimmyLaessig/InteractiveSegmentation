namespace InteractiveShapeDetection.Visualization


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
open System
open System.IO
open System.Threading
open Aardvark.Git
open Aardvark.Database
open InteractiveShapeDetection
open Aardvark.VRVis

open InteractiveShapeDetection.Base


module BoundingBoxVisualization =
    
   
    let init (framebufferSignature : IFramebufferSignature)(octree : IMod<Octree>) =


        let trafos = 
            adaptive {

                let! tree   = octree
               
                let nodes   = tree |> Octree.flatten 
      
                let (nodes1, nodes2)    = nodes     |> Array.partition  (fun (node, cell, path) -> node |> OctreeNode.hasPrimitiveShapes )
                let trafos1             = nodes1    |> Array.map        (fun (node, cell, path) -> cell.BoundingBox.UnitBoxTrafo)
                let trafos2             = nodes2    |> Array.map        (fun (node, cell, path) -> cell.BoundingBox.UnitBoxTrafo)
                                
                return  (trafos1 , trafos2)
            }
        
        let trafos1 = trafos |> Mod.map fst
        let trafos2 = trafos |> Mod.map snd
        
        let sg1 = 
                Box3d.Unit  |> IndexedGeometry.ofBox3d
                            |> Sg.instancedGeometry trafos1
                            |> Sg.surface (C4f.Green |> Surfaces.Instanced.ConstantColor framebufferSignature|> Mod.constant)     
                                
        let sg2 = 
                Box3d.Unit  |> IndexedGeometry.ofBox3d
                            |> Sg.instancedGeometry trafos2
                            |> Sg.surface (C4f.Gray |> Surfaces.Instanced.ConstantColor framebufferSignature|> Mod.constant)   

        Sg.group' [sg1; sg2]


        
        