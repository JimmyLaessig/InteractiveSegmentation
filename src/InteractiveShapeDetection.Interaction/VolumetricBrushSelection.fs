namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Database
open InteractiveShapeDetection.Base
open Aardvark.Base.Incremental


    
   
module VolumetricBrushSelectionApplicator = 
    

    module Coroutine = 
        
        let volumetricBrushSelectionRoutine (selection : VolumetricBrushSelection)(cluster : Cluster)(octree : Octree) = 
             
            let shapes          = cluster.shapes |> Array.concat

            let nodes = octree  |> Octree.filter (fun node cell path -> 
                                                            match node with
                                                            | Empty         -> false, false
                                                            | Leaf _        -> cluster.bounds.Intersects( cell.BoundingBox) , false
                                                            | Node (_,_)    -> 
                                                                let b = cluster.bounds.Intersects( cell.BoundingBox)
                                                                (b,b)
                                                            )
                                |> Octree.flatten
            
            let nodes = nodes |> Parallel.map (fun (node, cell, path) -> 
                            let density = node.Density.Value
                            let points  = node.Points.Value |> Array.map (fun point -> 

                                let belongsToCluster = Array.TrueForAll(shapes, fun shape -> (shape.GetMinimalDistanceTo point.Position) > density * 6.0) |> not 

                                
                                if belongsToCluster && selection |> VolumetricBrushSelection.Contains point.Position then 
                                    new Point(point.Position, point.Normal, point.Color, point.Primitive, true)
                                else                        
                                    point
                                )
                            (node |> OctreeNode.setPoints points, path)
                            
                )
           
            
            let octree = nodes |> Array.fold(fun (octree) (node, path)  -> octree |> Octree.Insert node (path |> Array.toList) true ) octree
           
            
            octree



    let init (enabled : IMod<bool>)(cluster : IMod<Cluster option>)(sequentialComputationApplicator : SequentialComputationApplicator<Octree>) (brush: VolumetricBrush) = 
        

        brush.Selection |> Mod.unsafeRegisterCallbackKeepDisposable(fun selection ->
                
                let cluster = cluster |> Mod.force
                match selection, cluster with
                | None, _ -> ()
                | _, None -> ()
                | Some sphere, Some cluster ->     
                        sequentialComputationApplicator.DispatchPrioritized (Coroutine.volumetricBrushSelectionRoutine sphere cluster)

                ) |> ignore