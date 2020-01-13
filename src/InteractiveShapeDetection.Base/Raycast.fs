namespace InteractiveShapeDetection.Base

open System.Diagnostics
open System.IO

open Aardvark.Database
open Aardvark.Git
open Aardvark.Git.Operators
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering


open Aardvark.VRVis

open Aardvark.Base.Native

[<AutoOpen>]
module Ray3dExtensions = 
    

    
    type RaycastHit = 
        {
            node    : OctreeNode
            cell    : GridCell
            path    : int[]
            point   : V3d option
        } 


    type Ray3d with  
   
        member this.performRaycast (octree: Octree) = 

            
            let decisionFun (node : OctreeNode) (cell : GridCell) (path : int[]) = 
                
                let hitPoint = 
                    let t = ref 0.0
                    if cell.BoundingBox.Intersects(this, t) then
                        Some (this.GetPointOnRay t.Value)
                    else
                        None

                let hit = 
                    {
                        node    = node
                        cell    = cell
                        path    = path 
                        point   = hitPoint
                    }

                match hit.point with
                | Some p    -> Some hit , true
                | None      -> None     , false
                
            
            let result = octree  |> Octree.choose decisionFun

            result


        member this.performRaycast(nodes : (OctreeNode * GridCell * int[])[]) =
            
            nodes   |> Array.map (fun (node, cell,path) -> 
                                    let hitPoint = 
                                        let t = ref 0.0
                                        if cell.BoundingBox.Intersects(this, t) then
                                            Some (this.GetPointOnRay t.Value)
                                        else
                                            None
                                    {
                                        node    = node
                                        cell    = cell
                                        path    = path 
                                        point   = hitPoint
                                    }
                                )
                    |> Array.filter (fun hit -> hit.point.IsSome)