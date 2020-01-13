namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base

open Aardvark.Database

open InteractiveShapeDetection.Base
open Aardvark.Base.Incremental



type LoDIncrease = 
    {
        Points  : IMod<Point[]>
        Cells   : IMod<Trafo3d[]>
    }

    static member init (enabled : IMod<bool>) (pickRay : IMod<Ray3d>)(cluster : IMod<Cluster option> )(depth : IMod<int>) (rayHitsOnly : IMod<bool>)(octree : IMod<Octree>) (culledOctree: IMod<Octree>)  = 
        
        let points, trafos = 
            adaptive {

                let! enabled = enabled 
                let! cluster        = cluster
                let! ray            = pickRay
                let! culledOctree   = culledOctree
                let! octree         = octree
                let! depth          = depth
                let! rayHitsOnly    = rayHitsOnly

                match enabled, cluster with
                | false, _              -> return [||], [||]
                | true, None            -> return [||], [||]
                | true, Some cluster    -> 
                    match cluster.nodes with
                    | [||]  -> return [||],[||]
                    | _     -> 

                        let density = (cluster.nodes |> Array.minBy(fun node -> node.Density.Value)).Density.Value
                                        
                                        
                        let shapes  = Array.map2 (fun (node : OctreeNode)(shapes : PrimitiveShape[]) -> shapes |> Array.map(fun shape -> (node.Density.Value, shape))) cluster.nodes cluster.shapes
                                                            |> Array.concat
            
            
                        let diff    = Octree.diff octree culledOctree 1
                        

                        let nodes   = 
                            if rayHitsOnly then 
                                ray.performRaycast diff |> Array.map(fun hit -> hit.node, hit.cell, hit.path)
                            else
                                diff |> Array.filter(fun (node, cell, path) -> cluster.Intersects cell.BoundingBox)
                        


                        let map (node : OctreeNode, cell,path) = 
                            node.Points.Value |> Array.filter(fun point -> 
                                shapes  |> Array.tryPick(fun (density, shape) -> if (shape.GetMinimalDistanceTo point.Position) <= density * 2.0 * 3.0 then Some shape else None) 
                                        |> Option.isSome
                                        )
                        

                        let points = nodes  |> Parallel.map map
                                            |> Array.concat
                        

                        
                        let trafos : Trafo3d[] = nodes |> Array.map (fun (_,cell,_) -> cell.BoundingBox.UnitBoxTrafo)
                        
                        return (points, trafos)        
            }
            |> Mod.unzip
        
        {
            Points = points
            Cells = trafos
        }
            


