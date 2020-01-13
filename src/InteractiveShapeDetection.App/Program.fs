namespace InteractiveShapeDetection

open System
open Aardvark.Base
open Aardvark.Base.Native
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open System.IO
open Aardvark.Database
open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Visualization
open InteractiveShapeDetection.Interaction


module App =
 

    open Aardvark.Base.CameraView
//    open Aardvark.Base.Frustum
    open Aardvark.SceneGraph.AirState


    [<EntryPoint>]
    let main argv = 

        Ag.initialize()
        Aardvark.Init()
        

        MBrace.FsPickler.FsPickler.DeclareSerializable<Aardvark.VRVis.PointRkdTreeDData>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<InteractiveShapeDetection.Base.PrimitiveShape>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<Plane3d>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<Sphere3d>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<Cone3d>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<Cylinder3d>()
        MBrace.FsPickler.FsPickler.DeclareSerializable<Torus3d>()


        let psp         = @"D:\Cache\Technologiezentrum_Teil1.pts"
        let storagePath = @"D:\Cache\vgmCache_technologiezentrum"
                          
                           
        let psp         = @"C:\Cache\JBs_Haus.pts"
        let storagePath = @"C:\Cache\JBs_Haus.pts.Cache"

//
//        let psp         = @"C:\Cache\primitiveScene.pts"
//        let storagePath = @"C:\Cache\primitiveScene.pts.Cache"

//
//
//        let psp         = @"D:\Cache\dragon.pts"
//        let storagePath = @"D:\Cache\vgmCache_dragon"


//        let psp         = @"D:\Cache\kirche.pts"
//        let storagePath = @"D:\Cache\vgmCache3"
        
        
        let s       = Directory.CreateDirectory(storagePath)
        let mem     = Memory.mapped (Path.combine [storagePath;"memory.mapped"]) //Memory.hglobal 0L //Memory.mapped (Path.combine [storagePath;"memory.mapped"])
        let get i   = NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i]) //Memory.hglobal 0L //NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i])
        let store   = new BlobStore(mem,get)
        use db      = new Database(store)     
        let guid = Guid("6f3fd114-f345-4e2d-b82c-2e7172ea6086")
        
        

        let pointset    = Octree.ofPointset psp guid db     
       
        

        use app = new OpenGlApplication()

        let win = app.CreateSimpleRenderWindow()
        win.Text <- "Aardvark rocks \\o/"
        win.Size <- V2i(1920, 1080)
        


        // Init Camera Controller
        let cameraController = 
            controller {                    
                return! AFun.chain [
                    CameraControllers.controlLook win.Mouse
                    CameraControllers.controlWSAD win.Keyboard 10.0
                    CameraControllers.controlPan win.Mouse 0.05
                    CameraControllers.controlScroll win.Mouse 0.5 0.000005
                    ]
            }   

            


        let bounds  = pointset.bounds      

       
        let dir     = (bounds.Min - bounds.Max) |> Vec.normalize    
        
        let camPos      = bounds.Center
        let camTarget   = (pointset.root.Value).Centroid.Value
        let camUp       = V3d.OOI
        
        
        Global.Window       <- win
        Global.Runtime      <- win.Runtime
        // Init global parameters that are needed all over the program
        Global.octree       <- Mod.init pointset
        Global.cameraView   <- AFun.integrate cameraController (CameraView.lookAt camPos camTarget  camUp)
        Global.frustum      <- win.Sizes            |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100000.0 (float s.X / float s.Y))       
        Global.viewTrafo    <- Global.cameraView    |> Mod.map CameraView.viewTrafo
        Global.projTrafo    <- Global.frustum       |> Mod.map Frustum.projTrafo
        Global.windowSizes  <- win.Sizes
        Global.OctreeManipulationApplicator <- SequentialComputationApplicator<Octree>(Global.octree)
        Global.pointcloudInfo               <- PointCloudVisualization.DefaultSettings.pointCloudInfoSettings

        // Init global input modules
        Input.mouse     <- win.Mouse
        Input.keyboard  <- win.Keyboard
       


        let pointcloudSg    = Global.octree |> PointCloudVisualization.Init win.FramebufferSignature  Global.cameraView Global.frustum Global.windowSizes win.Keyboard            
        
        
        
        let (culledOctree, pointPicking, shapePicking, lodIncrease, lasso, brush)  = InteractionWorkflow.init ShapeSelection      



        let color1 = C4f(0.5, 0.5, 0.5, 0.5) |> Mod.constant
        let color2 = C4f(1.0, 0.5, 0.5, 0.5) |> Mod.constant



        let tempClusterSg       = shapePicking.CurrentCluster     |> ClusterVisualization.Init (win.FramebufferSignature) color1 (Mod.constant false)
        let selectedClusterSg   = shapePicking.SelectedCluster    |> ClusterVisualization.Init (win.FramebufferSignature) color2 (Mod.constant false)

        let lodIncreaseSg   = (lodIncrease.Points) |> LodIncreaseVisualization.Init (Global.windowSizes)(PointCloudVisualization.DefaultSettings.lodSettings.PointSize :> IMod<_>) 
            


        let pickRay =
            adaptive {
                let! viewTrafo      = Global.viewTrafo
                let! projTrafo      = Global.projTrafo
                let! pixelPosition  = Input.mouse.Position

                return Ray3d.PickRay viewTrafo projTrafo pixelPosition
            }

        

        let clusterPickSg = 
            let trafos = 
                adaptive {
                    let! viewTrafo = Global.viewTrafo
                    let! projTrafo = Global.projTrafo
                    let! pickRay = pickRay
                    let! cluster = shapePicking.SelectedCluster

                    let viewProjTrafo = viewTrafo * projTrafo
                        

                    match cluster with
                    | None -> return [||]
                    | Some c -> 
                        let pivot   = pickRay    |> c.Intersect 

                        
                        let trafos  =    
                            match pivot with 
                            | None      -> [||]
                            | Some p    -> 
                                let radius = 
                                    let pivotNDC    = viewProjTrafo.Forward.TransformPosProj p
                                    let pointNDC    = pivotNDC + V3d(0.2, 0.0, 0.0)
                                    let point       = viewProjTrafo.Backward.TransformPosProj pointNDC

                                    V3d.Distance(point, p)
                                
                                [|(Trafo3d.Scale radius) * (Trafo3d.Translation p)|]
                        return trafos
                }   
            
            let ig = IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.OOO, 1.0)) 10 (C4b.Black)

            ig  |> Sg.instancedGeometry trafos
                |> Sg.surface (C4f.Blue |> Surfaces.Instanced.ConstantColor win.FramebufferSignature |> Mod.constant)


            
              
                
        let state = Mod.init 0
        
        let enablePoints = state |> Mod.map (fun b ->
            match b with
            | 0 -> true
            | 1 -> true
            | _ -> false
            ) 
        let enableBoxes = state |> Mod.map (fun b -> 
            match b with
            | 0 -> false
            | 1 -> true
            | _ -> true
        ) 

        win.Keyboard.Up.Values.Add(fun key -> 
            match key with
            | Aardvark.Application.Keys.Space -> 
                let oldState = state |> Mod.force
                let newState = (oldState + 1) % 3

                transact(fun () -> Mod.change state newState)
                    
            | _ -> ()
            )


        let pointcloudSg = pointcloudSg |> Sg.onOff enablePoints

        let bbSg = culledOctree |> PrimitiveShapeVisualization.Sg.ofOctree win.FramebufferSignature enableBoxes
                                |> Sg.onOff enableBoxes

        //let pickRaySg = Sg.ofRay3d pickRay (Mod.constant C4f.Yellow) (Mod.constant 100.0)                            
//        let bbSg = BoundingBoxVisualization.init win.FramebufferSignature culledOctree
//                        |> Sg.onOff enableBoxes

        let brushSg = brush |> VolumetricBrushVisualization.Sg.ofVolumetricBrush win.FramebufferSignature (Mod.constant (C4f(0.4, 0.4, 0.4, 0.2))) RenderPasses.geometry


        let boundingboxSg   = BoundingBoxVisualization.init win.FramebufferSignature culledOctree


        let sceneContentSg =                  
            Sg.group' [pointcloudSg;bbSg; lodIncreaseSg; brushSg; tempClusterSg; selectedClusterSg]//; lassoSelectionSg] 
                |> Sg.trafo Trafo3d.Identity'
                |> Sg.viewTrafo Global.viewTrafo
                |> Sg.projTrafo Global.projTrafo
                |> Sg.pass RenderPasses.geometry
                

        
        

        //let minimapSg   = minimapContentSg |> Minimap.Init Global.Window Global.cameraView Global.frustum RenderPasses.UIBack (Mod.constant 0.25)
        

        let cursorColor = 
            adaptive {
                let! point = pointPicking.CurrentPoint
                match point with
                | None      -> return C4f.Red
                | Some _    -> return C4f.Green
            }
        
        let cursorSg    = pointPicking.CurrentPoint  |> Cursor.Init win.FramebufferSignature Global.viewTrafo Global.projTrafo cursorColor RenderPasses.UIFront (Mod.constant (V2i(40,40))) win.Mouse.Position
        
        

        let lassoSg = lasso |> LassoVisualization.Sg.ofLasso win.FramebufferSignature (Mod.constant 5.0) (win.Sizes) (Mod.constant C4f.Red)
        
        let uiOverlaySg = 
            Sg.group' [cursorSg; lassoSg]
            |> Sg.pass RenderPasses.UIFront
        


        let sg = Sg.group' [sceneContentSg; uiOverlaySg]




        
        

        let task (candidate : ShapeDetection.CandidateNode)(octree : Octree) = 

            let density = float32 candidate.node.Density.Value

            let options : ShapeDetection.ShapeDetectionOptions= 
                {
                    Epsilon             = 1.0f * density
                    BitmapThreshold     = 2.0f * density
                    NormalsThreshold    = 0.9f
                    MinimalSupport      = 250
                    Probability         = 0.001f

                    DetectPlanes    = true
                    DetectCylinder  = false
                    DetectCones     = false
                    DetectSpheres   = false
                    DetectTori      = false
                }
            // Detect Shapes
            let shapesAndPoints = candidate |> ShapeDetection.CandidateNode.DetectPrimitiveShapes options               
                            
            match shapesAndPoints with 
            | None -> octree
            | Some (shapes,points) -> 
                            
            // Create a new octree node
            let node = candidate.node   |> OctreeNode.setShapes (Some shapes)
                                        |> OctreeNode.setPoints points
                            

            // Insert octree node into octree, thus creating a new octree
            let octree = octree |> Octree.Insert node (candidate.path |> Array.toList) true
                            
            octree


        
//        Global.octree   |> Mod.force
//                        |> Octree.flatten
//                        |> Array.iter (fun (node,cell,path) -> 
//                                let candidateNode :ShapeDetection.CandidateNode = 
//                                    {
//                                    node                    = node 
//                                    cell                    = cell
//                                    path                    = path
//                                    distanceToFocalPlane    = 0.0
//                                    }
//                                Global.OctreeManipulationApplicator.Dispatch (task candidateNode,  1000, fun () -> ())
//                                )
                                





        Global.OctreeManipulationApplicator.Start()
        

        let task =
            app.Runtime.CompileRender(win.FramebufferSignature, sg)
              //  |> DefaultOverlays.withStatistics
    
        win.Closing.Subscribe(fun _ -> ShapeDetection.Coroutine.Stop())             |> ignore    
        win.Closing.Subscribe(fun _ -> Global.OctreeManipulationApplicator.Stop())  |> ignore  
                
        let clear = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.White, Mod.constant 1.0)
        

        win.RenderTask <- RenderTask.ofList [clear; task]
        win.Run()

           
        0