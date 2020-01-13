namespace InteractiveShapeDetection

open System
open Aardvark.Base
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
open Aardvark.Base.Native


open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Global

open System.Runtime.Serialization



module ShapeDetection = 
    

    module Coroutine = 
        
        open System.Threading
        open System.Threading.Tasks

        let mutable fixedUpdateRate = 100.0

        let mutable private currentTask : Option<Task<_> * ref<bool>> = None
        let private lockObj     = obj()

        let private resetEvent  = new ManualResetEvent(true)



        let private shapeDetectionTask (view)(frustum)(mousePosition)(wantedNearPlaneDistance)(windowSize)(octree : Octree) = 
            
            let result = ShapeDetection.shapeDetectionTask view frustum mousePosition wantedNearPlaneDistance octree


            resetEvent.Set() |> ignore
            
            result



        let private timeoutCallback () = 
            resetEvent.Set() |> ignore
        

        let private ShapeDetectionCoroutine =  
            async {              
                
                let mutable timeSinceLastChange = 0.0
                // Get initial state
                let mutable prevTime        = System.DateTime.Now  
                let mutable prevViewTrafo   = viewTrafo                     |> Mod.force
                let mutable prevMousePos    = Global.Window.Mouse.Position  |> Mod.force
                                           
                    
                // Loop
                while (currentTask.Value |> snd).Value  do  
                    
                    // Update DeltaTime
                    let currentTime = System.DateTime.Now          
                    let delta =   (currentTime - prevTime).TotalMilliseconds
                  
                       
                    // get the current values for the monitored components
                    let currentView     = Global.cameraView             |> Mod.force
                    let currentProj     = Global.frustum                |> Mod.force
                    
                    let currentViewTrafo = currentView |> CameraView.viewTrafo
                    let currentProjTrafo = currentProj |> Frustum.projTrafo                                
                    let currentMousePos = Global.Window.Mouse.Position  |> Mod.force

                    
                    // Update Change counter
                    if currentViewTrafo = prevViewTrafo then
                        timeSinceLastChange <- timeSinceLastChange + delta
                    else
                        timeSinceLastChange <- 0.0
                            

                    //Perform updates
                    if timeSinceLastChange > fixedUpdateRate then                                
                        
                        let targetPointDistance     = Mod.force Global.pointcloudInfo.targetPointDistance
                        let windowSize              = Mod.force Global.windowSizes
                        let wantedNearPlaneDistance = PointSetLodData.WantedNearPlaneDistance windowSize targetPointDistance
                        
                        Global.OctreeManipulationApplicator.Dispatch((shapeDetectionTask currentView currentProj currentMousePos wantedNearPlaneDistance windowSize), 1000,timeoutCallback)
                        //Log.warn "#################################################"
                        timeSinceLastChange <- timeSinceLastChange - fixedUpdateRate
                        resetEvent.Reset() |> ignore    // Block thread 
                        resetEvent.WaitOne() |> ignore  // Wait for worker to signal release                                        
                    

                    // Update state of the monitored components
                    prevViewTrafo   <- currentViewTrafo
                    prevMousePos    <- currentMousePos
                    prevTime        <- currentTime                                                     
            }


        let IsRunning() = currentTask.IsSome

        let Stop() = 
            lock lockObj (fun _ -> 
                match currentTask with
                    | Some (t,cts) ->
                        Log.line "Stopping Shape Detection Coroutine!"
                        //cts.Cancel()
                        cts.Value <- false

                        
                        t.Wait() 

                        currentTask <- None
                        Log.line "Stopped!"
                        //Log.stop ()
                    | None -> ()
            )
        

        let Start() = 
            lock lockObj (fun _ -> 
                match currentTask with
                    | Some (t,cts) ->   
                        Log.line "Shape Detection Coroutine already running!"                           
                        currentTask <- Some (t,cts)
                    | None ->
                        Log.line "Starting Shape Detection Coroutine!"
                        currentTask <- Some (Async.StartAsTask ShapeDetectionCoroutine, ref true)  
                        Log.line "Started!"
            ) 
