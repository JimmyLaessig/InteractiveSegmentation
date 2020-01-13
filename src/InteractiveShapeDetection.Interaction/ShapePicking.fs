namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Database
open Aardvark.VRVis
open System.Collections.Generic
open Aardvark.Base.Monads.Option
open Aardvark.Base.Incremental
open InteractiveShapeDetection.Base
open Aardvark.Base.Camera
open Aardvark.Application


type ShapePicking = 
    {
        CurrentCluster : IMod<Cluster option>
        SelectedCluster : ModRef<Cluster option>
    }

    static member init (enabled : IMod<bool>)(mouse : IMouse)(ray : IMod<Ray3d>)(hits : IMod<RaycastHit[]>)(view : IMod<CameraView>)(frustum : IMod<Frustum>)(maxLodLevelDifference : IMod<int>)(octree : IMod<Octree>) = 
        
        // Shape from octree
        let shape = 
            adaptive {

                let! enabled = enabled

                if not enabled then 
                    return None
                else
                    let! ray = ray
                    let! hits = hits

                    let! view       = view
                    let! frustum    = frustum
                    let! maxLodLevelDifference = maxLodLevelDifference
                    let viewTrafo   = view      |> CameraView.viewTrafo
                    let projTrafo   = frustum   |> Frustum.projTrafo
                    let result      = ray       |> Picking.pickPrimitiveShape hits viewTrafo projTrafo maxLodLevelDifference

                    return result    
            }

        
        // Build a cluster of Primitive Shapes
        let cluster = 
            adaptive {
                let! shape  = shape
               
                match shape with
                | None  ->  return None
                | Some (node,cell,path,shape, point)  ->  
                        let! octree = octree    

                        let epsilon = node.Density.Value * 2.0
                        let! maxLodLevelDifference = maxLodLevelDifference
                        let cluster = octree |> Cluster.findCluster shape path epsilon maxLodLevelDifference
//                        let cluster = 
//                            {
//                                bounds  = shape
//                                shapes  = [|[|shape|]|]
//                                nodes   = [|node|]
//                                cells   = [|cell|]
//                            }
                        return Some cluster
            }


        let selectedCluster = Mod.init None

        mouse.Click.Values.Add(fun _ -> 
           
            if(enabled |> Mod.force) then 
                transact (fun _ -> Mod.change selectedCluster (cluster |> Mod.force))
            )
        
        {
            CurrentCluster = cluster
            SelectedCluster = selectedCluster
        }

