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
open System.IO


type PointPicking = 
    {
        CurrentPoint : IMod<Point option>
        SelectedPoint : ModRef<Point option>
    }


    static member init (enabled : IMod<bool>)(mouse : IMouse)(ray : IMod<Ray3d>)(hits : IMod<RaycastHit[]>)(cluster : IMod<Cluster option>)(cameraView : IMod<CameraView>)(frustum : IMod<Frustum>)(octree : IMod<Octree>) = 


        let currentPoint = 

            adaptive {

                let! enabled    = enabled
   
                let! cluster    = cluster
                let! pickRay    = ray
                let! hits       = hits

                let! viewTrafo = cameraView |> Mod.map CameraView.viewTrafo
                let! projTrafo = frustum    |> Mod.map Frustum.projTrafo

                let viewProjTrafo   = viewTrafo * projTrafo
                let! mousePosition  = mouse.Position


                let hits = hits |> Array.map(fun hit -> hit.node)
                let watch = System.Diagnostics.Stopwatch()
                watch.Start()
                let result = 
                    match enabled, cluster with
                    | false, _      -> None
                    | true, None    -> //return pickRay |> Picking.pickPoint hits 0.1 
                                       mousePosition |> Picking.pickPoint2 hits 0.2 viewProjTrafo
                    | true, Some c  -> 
                                 
                                    option {
                                        let! intersection = c.Intersect pickRay
                                        //return! intersection |> Picking.pickPoint' hits  0.1
                                   
                                        return! c |>  Picking.pickPoint'2 pickRay 0.2 viewProjTrafo
                                    }     
                watch.Stop()    
//                if result.IsSome then
//                    
//                    
//
//                    Log.line "Time: %f" watch.Elapsed.TotalMilliseconds
//                    
//                    let txt = sprintf "%b ; %f" (cluster.IsSome) (watch.Elapsed.TotalMilliseconds)
//                    File.AppendAllLines("picking_benchmark",  [|txt|])
                return result
            }


        let selectedPoint = Mod.init None

        mouse.Down.Values.Add (fun _ -> transact (fun _ -> Mod.change selectedPoint (selectedPoint |> Mod.force)))

        {
            CurrentPoint    = currentPoint
            SelectedPoint   = selectedPoint
        }