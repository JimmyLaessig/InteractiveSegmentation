namespace InteractiveShapeDetection.Base

open System.Diagnostics
open System.IO

open Aardvark.Database
open Aardvark.Git
open Aardvark.Git.Operators
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering
open System.Diagnostics



open Aardvark.Base.Native
open Aardvark.Base.Monads.Option

     


module OctreeQueries = 


    let clearSelection(octree : Octree) = 
            
            let nodes = octree  |> Octree.flatten
          
            let nodes = nodes   |> Parallel.map (fun (node, cell, path) -> 
                            let density = node.Density.Value
                            let points  = node.Points.Value |> Array.map (fun point -> 

                                    new Point(point.Position, point.Normal, point.Color, point.Primitive, false)
                                )
                            (node |> OctreeNode.setPoints points, path)     
                )
            
            let octree = nodes |> Array.fold(fun (octree) (node, path)  -> octree |> Octree.Insert node (path |> Array.toList) true ) octree
           
            octree
    

    let cullAtRenderHorizon (wantedNearPlaneDistance : float)(view: CameraView)(frustum : Frustum)(octree: Octree) = 
        
        let viewTrafo = view    |> CameraView.viewTrafo
        let projTrafo = frustum |> Frustum.projTrafo

        let viewProjTrafo = viewTrafo * projTrafo

        let hull = viewProjTrafo |> ViewProjection.toFastHull3d
  
        let renderNode(node : OctreeNode )(cell : GridCell) (path : int []) = 

            let inner = 
                match node with 
                | Node (_,children) -> true
                | _                 -> false              
                
            let bb      = cell.BoundingBox
            let lodDataNode = 
                { 
                    id = (node :> obj); level = (Array.length path) ; bounds = bb; 
                    inner = inner; granularity = Fun.Cbrt(bb.Volume / 1000.0); 
                    render = true
                }

            let t = ref 0.0

            lodDataNode |> PointSetLodData.IsRendering hull view frustum wantedNearPlaneDistance
            
        octree  |> Octree.filter renderNode
