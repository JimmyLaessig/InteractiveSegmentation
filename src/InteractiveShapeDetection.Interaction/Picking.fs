namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Database
open Aardvark.VRVis
open System.Collections.Generic
open Aardvark.Base.Monads.Option
open InteractiveShapeDetection.Base


module Picking = 


    // Picks a primitiveShape
    let pickPrimitiveShape (raycastHits : RaycastHit[])(viewTrafo : Trafo3d)(projTrafo : Trafo3d)(maxLodDifference : int)(ray : Ray3d) = 
        
        let viewProjTrafo = viewTrafo * projTrafo
        
        let maxLodLevel = 
            match raycastHits with
            | [||]  -> 0
            | _     -> (raycastHits |> Array.maxBy (fun hit -> hit.path |> Array.length)).path |> Array.length   
        let minLodLevel = max 0 (maxLodLevel - maxLodDifference)
        
        let sorted = 
            raycastHits |> Array.filter(fun hit -> hit.path.Length >= minLodLevel)
                        |> Array.sortByDescending (fun hit -> (hit.path |> Array.length |> float) + (1.0 - viewProjTrafo.Forward.TransformPosProj(hit.cell.BoundingBox.Center).Z))
         
        let mutable result = None
            
        sorted |> Array.takeWhile (fun c -> 
            result <- 
                option {
                    let! shapes         = c.node.DetectedPrimitives.Value 

                    
                    let intersections   = shapes |> Array.choose    (fun shape   ->    
                                                                        match shape.Intersect ray with
                                                                        | None  -> None
                                                                        | Some p-> Some (shape,p) 
                                                                    )

                    match intersections  with
                    | [||]  ->  return! None
                    | _     ->  let (shape, point) =  intersections |> Array.minBy (fun (shape, p) -> (p - ray.Origin).Length) 
                                return (c.node, c.cell, c.path, shape, point)
            } 

            result.IsNone

        )   |> ignore                                                                                 
        result
    

    

    let pickPoint'(nodes : OctreeNode[]) (maxDistance : float) (pivot : V3d) : (Point option) =
    
        match nodes with
        | [||]  ->  None
        | _     ->       

            let task (node : OctreeNode) = 
               
                let points          = node.Points.Value
                let positions       = points |> Array.map (fun point -> point.Position)
                let kdTree          = RkDTree.CreateFromData positions (node.KdTreeData.Value) 
                let closestPoints   = kdTree.GetClosest(pivot, maxDistance, 1).ToArray()
                    
                match closestPoints with
                | [||]  -> None
                | _     -> Some (points.[int closestPoints.[0].Index], closestPoints.[0].Dist)

                         
            let result = nodes  |> Parallel.map task
                                |> Array.choose (fun x -> x)
                                |> Array.tryMinBy (fun (p, d)-> d)
                                |> Option.map (fun (p,d) -> p)
           
            result
    

    let pickPoint'2 (pickRay : Ray3d)(maxDistance : float)(vpTrafo : Trafo3d) (cluster : Cluster) : (Point option) =
        
        
        let hits = Array.zip cluster.nodes cluster.cells 
                        |> Array.map(fun (n,c) -> n,c,[||])
                        |> pickRay.performRaycast
        
        let intersection = cluster.Intersect pickRay 
        
                 
        match intersection, hits with
        | _, [||]       -> None
        | None, _       -> None
        | Some pivot, _ ->       
            
            // Determine the pick radius by projecting the intersection to the nearplane and use the nearplane maxDistance to create a point on the sphere, then unproject both point
            let pivotProj           = vpTrafo.Forward.TransformPosProj pivot
            let pointOnSphereProj   = pivotProj + V3d(maxDistance, 0.0, 0.0)
            let pointOnSphere       = vpTrafo.Backward.TransformPosProj pointOnSphereProj

            let pickRadius          = V3d.Distance(pivot, pointOnSphere)


            // task that is executed for each node
            let task (node : OctreeNode) = 

                let epsilon = node.Density.Value * 3.0
                let alpha   = 0.9
                let points  = node.Points.Value

                let candidates  = points |> Array.choose(fun point -> 

                                    let d = V3d.Distance(point.Position, pivot)

                                    //if (point |> cluster.Contains epsilon alpha) && 
                                    if (d <= pickRadius) then 
                                        Some (point, d)
                                    else None 
                                    )
                

                candidates |> Array.tryMinBy (fun (point, d) -> d)

                         
            let result = hits   |> Array.map (fun hit -> hit.node)
                                |> Parallel.map task
                                |> Array.choose id
                                |> Array.tryMinBy (fun (p, d)-> d)
                                |> Option.map (fun (p,d) -> p)
            result


    let intersectsProjCone (cone : Circle2d) (pt : V3d) =
        if pt.Z > 1.0 then
            false
        else
            (pt.XY - cone.Center).Length < cone.Radius
    


    let pickPoint2 (nodes : OctreeNode[]) (pickRadius : float) (mvpTrafo : Trafo3d)(mousePosition: PixelPosition) : (Point option) = 
        let ndc = mousePosition.ToNDC()
        let pickCircle = Circle2d(ndc.XY, pickRadius)

        match nodes with
        | [||]  ->  
            None
        | _     ->       
        
  
            let task (node : OctreeNode) = 
               
                let pick    = node.Points.Value
                                |> Array.map(fun point -> point, mvpTrafo.Forward.TransformPosProj (point.Position))
                                |> Array.sortBy(fun (_, pProj) -> (pProj.XY - pickCircle.Center).Length)
                                |> Array.tryPick(fun (p, pProj) -> if pProj |> intersectsProjCone pickCircle then Some (p, pProj) else None)
                pick

                  
            let result = nodes  |> Parallel.map task
                                |> Array.choose id
                                |> Array.tryMinBy (fun (p, pProj)-> (pProj.XY - pickCircle.Center).Length)
                                |> Option.map (fun (p,d) -> p)
            
            result



    let pickPoint (nodes : OctreeNode[]) (maxDistance : float) (pickRay: Ray3d) : (Point option) = 
        
        match nodes with
        | [||]  ->  
            None
        | _     ->       
        
  
            let task (node : OctreeNode) = 
               
                let points          = node.Points.Value
                let positions       = points |> Array.map (fun point -> point.Position)
                let kdTree          = RkDTree.CreateFromData positions (node.KdTreeData.Value) 
                let closestPoints   = kdTree.GetClosestToLine(pickRay.Origin, pickRay.Origin + pickRay.Direction.Normalized * 100000.0, maxDistance, 1).ToArray()
                    
                match closestPoints with
                | [||]  -> None
                | _     -> Some (points.[int closestPoints.[0].Index], closestPoints.[0].Dist)

                  
            let result = nodes  |> Parallel.map task
                                |> Array.choose (fun x -> x)
                                |> Array.tryMinBy (fun (p, d)-> d)
                                |> Option.map (fun (p,d) -> p)
            result

    