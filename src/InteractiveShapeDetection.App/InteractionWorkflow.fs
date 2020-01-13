namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Base.Camera
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

open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
//open Aardvark.VRVis
open Aardvark.Base.Native
open InteractiveShapeDetection
open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Visualization
open Aardvark.Base.Monads.Option
open System.Diagnostics


type InteractionState =
| LassoSelection
| PointSelection
| ShapeSelection
| LoDIncrease
| VolumetricBrushSelection
| NoInteraction
    
    override this.ToString() = 
        match this with 
        | LassoSelection            -> "Lasso" 
        | PointSelection            -> "PointPicking" 
        | ShapeSelection            -> "ShapePicking" 
        | LoDIncrease               -> "LocalLodIncrease" 
        | VolumetricBrushSelection  -> "VolumetricBrushSelection" 
        | NoInteraction             -> "NoInteraction" 


module InteractionWorkflow = 
    
    
    let currentState    = Mod.init NoInteraction      



    let init (initState) = 

        // Change the state
        transact (fun () -> Mod.change currentState initState )  

       

        let wantedNearPlaneDistance = 
            adaptive {
                
                let! windowSize             = Global.windowSizes
                let! targetpointDistance    = Global.pointcloudInfo.targetPointDistance

                return PointSetLodData.WantedNearPlaneDistance windowSize targetpointDistance
            }
        

        // Cull the octree at the render horizon
        let culledOctree = 
            adaptive { 
               
                let! octree = Global.octree
                let! state  = currentState

                match state with
                | NoInteraction ->  return Octree.Empty (octree.store)(octree.offset)(octree.splitCount)
                | _             ->  let! view                       = Global.cameraView
                                    let! frustum                    = Global.frustum
                                    let! wantedNearPlaneDistance    = wantedNearPlaneDistance
                                   
                                    let culledOctree                = octree |> OctreeQueries.cullAtRenderHorizon wantedNearPlaneDistance view frustum    
                                    return culledOctree
            } 


        // Perform Raycast through octree
        let pickRay, raycastHits = 
            let result = 
                adaptive {
                        let! view           = Global.cameraView
                        let! frustum        = Global.frustum
                        let! pp             = Global.Window.Mouse.Position
                        let! culledOctree   = culledOctree


                        let viewTrafo       = view      |> CameraView.viewTrafo
                        let projTrafo       = frustum   |> Frustum.projTrafo                            
                        let pickRay         = pp        |> Ray3d.PickRay viewTrafo projTrafo
                        let result          = pickRay.performRaycast culledOctree 
                    
                        return pickRay,result
                }
            result |> Mod.map fst , result |> Mod.map snd
            


        let shapeEnabled, pointEnabled, lodEnabled, lassoEnabled, brushEnabled = 
            currentState   |> Mod.map(fun state -> 
                match state with
                | ShapeSelection            -> true , false, false, false, false
                | PointSelection            -> false, true , false, false, false
                | LoDIncrease               -> false, false, true , false, false
                | LassoSelection            -> false, false, false, true , false
                | VolumetricBrushSelection  -> false, false, false, false, true 
                | NoInteraction             -> false, false, false, false, false
            )
            |> Mod.unzip5



        let shapePicking    = culledOctree |> ShapePicking.init (shapeEnabled) (Global.Window.Mouse) pickRay raycastHits Global.cameraView Global.frustum (Mod.constant 3)
       
        let pointPicking    = culledOctree |> PointPicking.init (pointEnabled) (Global.Window.Mouse) pickRay raycastHits (shapePicking.SelectedCluster)  Global.cameraView Global.frustum 
        
        let lodIncrease     = culledOctree |> LoDIncrease.init (lodEnabled) pickRay (shapePicking.SelectedCluster)(Mod.constant 1) (Mod.constant false) Global.octree
        
        let lasso           = Global.Window.Mouse |> Lasso.init (lassoEnabled) Global.cameraView Global.frustum
        
        let lassoSelection  = lasso |> LassoSelectionApplicator.init lassoEnabled shapePicking.SelectedCluster Global.OctreeManipulationApplicator
        

        let brush           = Global.Window.Mouse |> VolumetricBrush.init (brushEnabled) (Mod.init 0.5 :> IMod<_>) Global.cameraView Global.frustum shapePicking.SelectedCluster
        let brushSelection  = brush |> VolumetricBrushSelectionApplicator.init (brushEnabled) (shapePicking.SelectedCluster) (Global.OctreeManipulationApplicator)
        


        let mutable tmpState    = initState

        let mutable mouseLock   = false
        let mutable keyLock     = false


        let changeState state = 
            if mouseLock && keyLock then 
                tmpState <- state
            else
               transact(fun () -> Mod.change currentState state) 
        
        
        
        
        Global.Window.Keyboard.Down.Values.Add (fun key -> 
            match key with
                | Aardvark.Application.Keys.D1  -> changeState ShapeSelection
                | Aardvark.Application.Keys.D2  -> changeState PointSelection
                | Aardvark.Application.Keys.D3  -> changeState LassoSelection
                | Aardvark.Application.Keys.D4  -> changeState VolumetricBrushSelection
                | Aardvark.Application.Keys.D5  -> changeState LoDIncrease
                | Aardvark.Application.Keys.D6  -> changeState NoInteraction
                | Keys.C                        -> Global.OctreeManipulationApplicator.DispatchPrioritized (OctreeQueries.clearSelection)
                | Keys.P                        ->  
                    match ShapeDetection.Coroutine.IsRunning() with
                    | false -> ShapeDetection.Coroutine.Start()
                    | true  -> ShapeDetection.Coroutine.Stop()          
                |_          -> ()      
                        )  
        


        // 1 for mouse
        // 2 for keyboard
        let lockInteraction (src) = 
            
            // Only lock if no lock is present
            if not mouseLock && not keyLock then 
                    
                    tmpState <- currentState |> Mod.force
                    transact(fun _ -> Mod.change currentState NoInteraction)


            if src = 1 then 
                mouseLock   <- true
            if src = 2 then 
                keyLock     <- true
        

        // 1 for mouse
        // 2 for keyboard
        let tryUnlockInteraction(src) = 
            if src = 1 then 
                mouseLock   <- false
            if src = 2 then 
                keyLock     <- false
            
            // Only unlock if both locks are cleared
            if not mouseLock && not keyLock then 
                
                transact(fun () -> Mod.change currentState tmpState)
                tmpState <- currentState |> Mod.force
        


        Global.Window.Mouse.Move.Values.Add (fun (_,_) ->
                            
                            if MouseButtons.Right  |>  Global.Window.Mouse.IsDown |> Mod.force then 
                                lockInteraction(1)
                            )
       

        Global.Window.Mouse.Up.Values.Add (fun button ->
                            match button with
                            | MouseButtons.Right    -> 
                                tryUnlockInteraction(1)
                            | _                     -> ()
                            )


        Global.Window.Keyboard.Down.Values.Add(fun key -> 
                            match key with
                            | Keys.W | Keys.A | Keys.S | Keys.D -> 
                                lockInteraction(2)
                            | _                                 -> ()
                            )


        Global.Window.Keyboard.Up.Values.Add(fun key -> 
                            match key with
                            | Keys.W | Keys.A | Keys.S | Keys.D -> 
                                tryUnlockInteraction(2)
                                                                    
                            | _                                 -> ()
                            )



        (culledOctree, pointPicking, shapePicking, lodIncrease, lasso, brush)
        

            




