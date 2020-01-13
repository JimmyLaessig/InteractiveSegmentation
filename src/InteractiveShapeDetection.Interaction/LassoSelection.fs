namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Database
open InteractiveShapeDetection.Base
open Aardvark.Base.Incremental


module LassoSelectionApplicator = 
    

    module Coroutine = 
        
        let lassoSelectionRoutine'(id : int) (selection : LassoSelection)(octree : Octree) = 

//            let x = System.Diagnostics.Stopwatch()
//            x.Start();

            let viewProjTrafo   = (selection.View |> CameraView.viewTrafo ) * (selection.Frustum |> Frustum.projTrafo)


            let intersects (polygon : Triangle2d[]) (box : Box3d) = 

                let points  = box.ComputeCorners() |> Array.map (fun p -> (viewProjTrafo.Forward.TransformPosProj p).XY)
                let outline = Polygon2d(points).ComputeConvexHullIndexPolygon().ToPolygon2d()

                let result = polygon |> Array.tryPick(fun triangle -> if triangle |>  outline.Intersects then Some triangle else None)
                result.IsSome


            let nodes = octree  |> Octree.filter    (fun node cell path -> 
                                                        match node with
                                                        | Empty         -> false, false
                                                        | Leaf _        -> cell.BoundingBox |> intersects selection.Triangles , false
                                                        | Node (_,_)    -> 
                                                            let b = cell.BoundingBox |> intersects selection.Triangles
                                                            b,b
                                                    )
                                |> Octree.flatten
            
            let nodes = nodes |> Parallel.map (fun (node, cell, path) -> 
                            let density = node.Density.Value
                            let points  = node.Points.Value |> Array.map (fun point -> 

                                            let point_proj = (viewProjTrafo.Forward.TransformPosProj (point.Position)).XY                                    
                                            if selection |> LassoSelection.Contains point_proj then 
                                                new Point(point.Position, point.Normal, point.Color, point.Primitive, true)
                                            else point 
                                        )

                            (node |> OctreeNode.setPoints points, path) 
                )
           
            
//            let txt = sprintf "%i ; false ; %f" id (x.Elapsed.TotalMilliseconds)
//            System.IO.File.AppendAllLines("C:/Users/brainer/desktop/lasso_benchmark.csv",  [|txt|])


            let octree = nodes |> Array.fold(fun (octree) (node, path)  -> octree |> Octree.Insert node (path |> Array.toList) true ) octree
           
            
            octree


        let lassoSelectionRoutine (id : int)(selection : LassoSelection)(cluster : Cluster)(octree : Octree) = 
//            
//            let x = System.Diagnostics.Stopwatch()
//            x.Start();

            let shapes          = cluster.shapes |> Array.concat
            let viewProjTrafo   = (selection.View |> CameraView.viewTrafo ) * (selection.Frustum |> Frustum.projTrafo)


            let nodes = octree  |> Octree.filter (fun node cell path -> 
                                                            match node with
                                                            | Empty         -> false, false
                                                            | Leaf _        -> cluster.bounds.Intersects( cell.BoundingBox) , false
                                                            | Node (_,_)    -> cluster.bounds.Intersects( cell.BoundingBox) , true
                                                           )
                                |> Octree.flatten
            
            let nodes = nodes |> Parallel.map (fun (node, cell, path) -> 
                            let density = node.Density.Value
                            let points  = node.Points.Value |> Array.map (fun point -> 

                                let belongsToCluster = Array.TrueForAll(shapes, fun shape -> (shape.GetMinimalDistanceTo point.Position) > density * 6.0) |> not 

                                
                                if not belongsToCluster then 
                                    point
                                else
                                    let point_proj = (viewProjTrafo.Forward.TransformPosProj (point.Position)).XY                                    
                                    if selection |> LassoSelection.Contains point_proj then 
                                        new Point(point.Position, point.Normal, point.Color, point.Primitive, true)
                                    else point 
                                )
                            (node |> OctreeNode.setPoints points, path)
                            
                )

                    
//            let txt = sprintf "%i ; true ; %f" id (x.Elapsed.TotalMilliseconds)
//            System.IO.File.AppendAllLines("C:/Users/brainer/desktop/lasso_benchmark.csv",  [|txt|])

            let octree = nodes |> Array.fold(fun (octree) (node, path)  -> octree |> Octree.Insert node (path |> Array.toList) true ) octree
           
            
            octree

    
    let init (enabled : IMod<bool>)(cluster : IMod<Cluster option>)(sequentialComputationApplicator : SequentialComputationApplicator<Octree>) (lasso : Lasso)= 
        
        let mutable id = 0    

        lasso.Selection |> Mod.unsafeRegisterCallbackKeepDisposable(fun selection ->
                
                let cluster = cluster |> Mod.force
                
                match selection, cluster with
                | Some selection , Some cluster -> sequentialComputationApplicator.DispatchPrioritized (Coroutine.lassoSelectionRoutine 0 selection cluster)
                | Some selection , None -> sequentialComputationApplicator.DispatchPrioritized (Coroutine.lassoSelectionRoutine' 0 selection)
                | _,_ -> ()

//                match selection, cluster with
//                | Some selection , Some cluster ->  sequentialComputationApplicator.DispatchPrioritized (Coroutine.lassoSelectionRoutine id selection cluster)
//                                                    sequentialComputationApplicator.DispatchPrioritized (Coroutine.lassoSelectionRoutine' id  selection)
//                                                    id <- id + 1 
//                | _,_ -> ()

                ) |> ignore