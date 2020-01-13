namespace InteractiveShapeDetection.Tools.Playground

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
open Aardvark.VRVis
open Aardvark.SceneGraph.IO
open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Visualization
open Assimp
open System.Runtime.Serialization


module PtsCreator =


    let main argv = 
       

        Ag.initialize()
        Aardvark.Init()

        
        use app = new OpenGlApplication()
        let win = app.CreateSimpleRenderWindow()

        win.Text <- "Aardvark rocks \\o/"
        win.Size <- V2i(1280, 1024)

        

        
        
        let floor   = Quad3d (V3d(-20, -20, 0), V3d(20, -20, 0), V3d(-20, 20, 0), V3d(20, 20, 0))
        let wall    = Quad3d (V3d(-20, 20, 0), V3d(20, 20, 0), V3d(-20, 20, 20), V3d(20, 20, 20))
        
        // back row of cylinder
        let cylinder1 = Cylinder3d(V3d(-10, -5, 0), V3d(-10, -5, 20), 2.5)
        let cylinder2 = Cylinder3d(V3d(0, -5, 0), V3d(0, -5, 20), 2.5)
        let cylinder3 = Cylinder3d(V3d(10, -5, 0), V3d(10, -5, 20), 2.5)

        // Hats of the cylinder
        let cone1 = Cone3d(V3d(-10, -5, 25), V3d(0, 0, -1), Conversion.RadiansFromDegrees(30.0))
        let cone2 = Cone3d(V3d(  0, -5, 25), V3d(0, 0, -1), Conversion.RadiansFromDegrees(30.0))
        let cone3 = Cone3d(V3d( 10, -5, 25), V3d(0, 0, -1), Conversion.RadiansFromDegrees(30.0))

       // let sphere = Sphere3d(V3d(0,0,0), 5.0)

        let torus1 = Torus3d(V3d(-10.0, -5.0, 19.75), V3d.OOI.Normalized, 2.75, 0.25)
        let torus2 = Torus3d(V3d(  0.0, -5.0, 19.75), V3d.OOI.Normalized, 2.75, 0.25)
        let torus3 = Torus3d(V3d( 10.0, -5.0, 19.75), V3d.OOI.Normalized, 2.75, 0.25)

        
       
//        use ctx = new Assimp.AssimpContext()
//        let path = "C:/Users/brainer/Desktop/stanford_dragon/dragon.obj"
//        //let path = "C:/Users/brainer/Desktop/sponza_obj/sponza.obj"
//        let scene = ctx.ImportFile(path, PostProcessSteps.Triangulate)
//        let dragon =            
//            scene.Meshes.ToArray() |> Array.map(fun mesh -> 
//                mesh.Faces.ToArray() |> Array.map (fun face -> 
//                    let p0 =  mesh.Vertices.[face.Indices.[0] ]
//                    let p1 =  mesh.Vertices.[face.Indices.[1] ]
//                    let p2 =  mesh.Vertices.[face.Indices.[2] ]
//
//                    let p0 =  V3d(p0.X,  p0.Z , p0.Y )
//                    let p1 =  V3d(p1.X,  p1.Z , p1.Y )
//                    let p2 =  V3d(p2.X,  p2.Z , p2.Y )
//
//                    Triangle3d(p0, p1, p2)
//                    ))
//                    |> Array.concat
            

           
        let pts = 
            [|
//                //sphere |> PointCloudGenerator.Sphere (40000) (C4b.DarkCyan) 0.01;
////                dragon |> PointCloudGenerator.Triangles (1000000) (C4b.DarkGreen) 0.01
//                block1  |> PointCloudGenerator.Box (20000) (C4b.DarkCyan) 0.001;
//                block2  |> PointCloudGenerator.Box (20000) (C4b.DarkCyan) 0.001;
                torus1   |> PointCloudGenerator.Torus (20000) (C4b.DarkCyan) 0.0001;
                torus2   |> PointCloudGenerator.Torus (20000) (C4b.DarkCyan) 0.0001;
                torus3   |> PointCloudGenerator.Torus (20000) (C4b.DarkCyan) 0.0001;
                                                                                
                floor   |> PointCloudGenerator.Plane (80000) (C4b.DarkGreen) 0.0001;
                wall    |> PointCloudGenerator.Plane (80000) (C4b.DarkGreen) 0.0001;
                                                                                
                cylinder1 |> PointCloudGenerator.Cylinder (64000) (C4b.Gray) 0.0001;
                cylinder2 |> PointCloudGenerator.Cylinder (64000) (C4b.Gray) 0.0001;
                cylinder3 |> PointCloudGenerator.Cylinder (64000) (C4b.Gray) 0.0001;
                                                                                
                cone1 |> PointCloudGenerator.Cone (20000)  (C4b.DarkRed) 5.0 0.0001;
                cone2 |> PointCloudGenerator.Cone (20000)  (C4b.DarkRed) 5.0 0.0001;
                cone3 |> PointCloudGenerator.Cone (20000)  (C4b.DarkRed) 5.0 0.0001;
            |]
            |> Array.concat

        let positions  = pts |> Array.map (fun p -> p.Position)
        let normals = pts |> Array.map (fun p -> p.Normal)



        
        let (pos, normals, colors, texcoords) = pts |> Array.map (fun p -> V3f p.Position, V3f p.Normal, p.Color, V2f.OO)
                                                    |> Array.unzip4
        
        let pointGeometry = IndexedGeometry (
                                Mode                = IndexedGeometryMode.PointList,       
                                IndexedAttributes   = SymDict.ofList [  DefaultSemantic.Positions,  pos     :> Array
                                                                        DefaultSemantic.Normals,    normals :> Array
                                                                        DefaultSemantic.Colors,     colors  :> Array
                                                                        DefaultSemantic.DiffuseColorCoordinates,     texcoords  :> Array
                                                                    ]
                            )         
          


        let cameraView  =  DefaultCameraController.control win.Mouse win.Keyboard win.Time (CameraView.LookAt(V3d(0, -30, 5), V3d(0, 0, 5), V3d.OOI))    
        let frustum     =  win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))       
            
        let viewTrafo   = cameraView    |> Mod.map CameraView.viewTrafo
        let projTrafo   = frustum       |> Mod.map Frustum.projTrafo        


                              
        let pointSg = 
            pointGeometry   |> Sg.ofIndexedGeometry
                            |> Sg.trafo Trafo3d.Identity'
                            |> Sg.uniform "PointSize" (Mod.constant 0.05)
                            |> Sg.uniform "ViewportSize" (win.Sizes)
                            |> Sg.viewTrafo viewTrafo
                            |> Sg.projTrafo projTrafo
                            |> Sg.surface (InteractiveShapeDetection.Visualization.Surfaces.PointSpriteLighting win.FramebufferSignature |> Mod.constant)

      
        
        let normalsSg = pointGeometry |> Mod.constant
                                |> Sg.Normals(win.FramebufferSignature) (Mod.constant C4f.Blue) (Mod.constant 0.1)
                                |> Sg.trafo Trafo3d.Identity'
                                |> Sg.viewTrafo viewTrafo
                                |> Sg.projTrafo projTrafo
        


        let sg_final = Sg.group' [ pointSg; normalsSg]
                         
        

        let clearTask = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.White, Mod.constant 1.0)

        let renderTask =
            app.Runtime.CompileRender(win.FramebufferSignature, sg_final)
              //  |> DefaultOverlays.withStatistics
        

        win.RenderTask <- RenderTask.ofList [clearTask; renderTask]
        //win.RenderTask <- renderTask
        win.Run()



        // Write to file

        let ptsData = pts |> Array.map(fun point ->
                    let p = point.Position
                    let c = point.Color
                    
                    (p.X.ToString("N6")) + " " + p.Y.ToString("N6") + " " + p.Z.ToString("N6") + " 0 " + c.R.ToString() + " " + c.G.ToString() + " " + c.B.ToString();
                    )

        let numVertices = [|"" + pts.Length.ToString()|];

        Log.startTimed "Writing..."
        File.WriteAllLines(@"C:\Cache\primitiveScene.pts", numVertices);
        File.AppendAllLines(@"C:\Cache\primitiveScene.pts", ptsData);
        Log.stop()
        System.Console.ReadKey() |> ignore

        0