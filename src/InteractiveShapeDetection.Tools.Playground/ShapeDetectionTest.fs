namespace InteractiveShapeDetection.Tools.Playground

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
open Aardvark.VRVis
open InteractiveShapeDetection.Base


module ShapeDetectionTest =
 

    open Aardvark.Base.CameraView
    open Aardvark.Base.Frustum
    open Aardvark.SceneGraph.AirState


    let importPTS path = 
        Log.startTimed "Importing File"
        let points = path   |> File.ReadAllLines
                            |> Array.choose (fun s -> 

                                let items = s.SplitOnWhitespace()
                                if items |> Array.length <> 7 then 
                                    None
                                else
                                    let position    = V3d(Double.Parse(items.[0]), Double.Parse(items.[1]), Double.Parse(items.[2]))
                                    let normal      = V3d.OOO
                                    let color       = C4b(Int32.Parse(items.[4]), Int32.Parse(items.[5]), Int32.Parse(items.[6]), 255)
                                    Point(position, normal, color) |> Some
                                )
        // Create rkdTree to calculate normals
        Log.stop()
         // Compute one Normal for the point in the given neighborhood
        let computeNormal( point : V3d)(neighbors : V3d[]) =
            if (neighbors |> Array.length) < 2 then V3d.Zero
            else 
                let plane = Plane3d.fittingPlane(neighbors)
                plane.Normal


            // Compute Normals for one node
       
        let positions = points |> Array.map(fun p -> p.Position)
        
        Log.startTimed "Building rkdTree"
        // Create a kdTree
        let kdTree = positions.CreateRkdTreeDist2 (1e-6)  
        Log.stop()

        Log.startTimed "Calculating Normals"
        
        let pointsWithNormals = points |> Array.map  (fun p -> 
                                            let point       = p.Position
                                            let indices     = kdTree.GetClosest(point,Double.MaxValue, 10) |> Seq.toList
                                            let neighbors   = indices |> Seq.map (fun i -> points.[(int i.Index)].Position) |> Seq.toArray                                           
                                            let normal      = computeNormal point neighbors
                                            Point(p.Position, normal, p.Color)
                                            )
        Log.stop()

        Log.startTimed "Calculating Density"
        let density     = (calcDensity pointsWithNormals kdTree) |> float32 

        Log.stop()

        Log.startTimed "Calculating Centroid"
        let centroid    = calcCentroid pointsWithNormals
        Log.stop()

        Log.startTimed "Calculating Bounding Box"
        let boundingBox = pointsWithNormals |> Array.fold( fun (minValues:V3d, maxValues:V3d) point -> 
                            
                                                                    let min = V3d(
                                                                                min (minValues.X) (point.Position.X),
                                                                                min (minValues.Y) (point.Position.Y),
                                                                                min (minValues.Z) (point.Position.Z) )
                                                                    let max = V3d(
                                                                                max (maxValues.X) (point.Position.X), 
                                                                                max (maxValues.Y) (point.Position.Y), 
                                                                                max (maxValues.Z) (point.Position.Z))
                                                                    (min, max)
                                                                    ) (V3d.PositiveInfinity, V3d.NegativeInfinity)
                                                                |> Box3d             
        Log.stop()
        pointsWithNormals, centroid, boundingBox, density  
    


    let benchmark (points : Point[]) (name: string) (options : ShapeDetection.ShapeDetectionOptions) (numRuns : int) = 
        

        let (positions, normals) = points   |> Array.map(fun p -> (p.Position, p.Normal))
                                            |> Array.unzip
            
        // Run Schnabel algorithm
        let mutable (pointsPerShape : V3d[][]) = [||]
        
        Log.warn "Starting Benchmark: %s Only Planes: %b" name (not options.DetectCones)
        let mutable shapes = null
        let results = Array.init numRuns (fun _ -> "")

        results |> Array.mapi (fun i _ -> 
            
            Log.startTimed "Run: %i" i

            let watch = System.Diagnostics.Stopwatch()
            watch.Start() 
            
                    // Run Schnabel ShapeDetection
            shapes <- Aardvark.VRVis.PrimitiveShapesWrapper.Run(
                            positions, normals, &pointsPerShape,
                            options.Epsilon, options.BitmapThreshold, options.NormalsThreshold, options.MinimalSupport, options.Probability, 
                            options.DetectPlanes, options.DetectCylinder, options.DetectCones, options.DetectSpheres, options.DetectTori);
            Log.stop() 
            watch.Stop()

            let time = watch.Elapsed.TotalMilliseconds
            let numShapes = shapes |> Array.length

            let optStar = if (not options.DetectCones) then "*" else ""

            sprintf "%s%s: time = %f ; numShape = %i" name optStar time numShapes
            )
              

    let main argv = 

        //use app = new OpenGlApplication()

        //let win = app.CreateSimpleRenderWindow(4)

//        win.Text <- "Aardvark rocks \\o/"
//        win.Size <- V2i(1920, 1080)
        
        
//        let pointset1, _, _, density1 = importPTS (Path.combine [@"C:/Cache/" ; "primitiveScene.pts"])
//       
//        
//
//
//    
//        let o1 : ShapeDetection.ShapeDetectionOptions = 
//            {
//                Epsilon             = 1.0f * density1
//                BitmapThreshold     = 2.0f * density1
//                NormalsThreshold    = 0.95f
//                MinimalSupport      = 500
//                Probability         = 0.001f
//
//                DetectPlanes    = true
//                DetectCylinder  = false
//                DetectCones     = false
//                DetectSpheres   = false
//                DetectTori      = false
//            }   
//        
//        let o2 : ShapeDetection.ShapeDetectionOptions = 
//            {
//                Epsilon             = 1.0f * density1
//                BitmapThreshold     = 2.0f * density1
//                NormalsThreshold    = 0.95f
//                MinimalSupport      = 500
//                Probability         = 0.001f
//
//                DetectPlanes    = true
//                DetectCylinder  = true
//                DetectCones     = true
//                DetectSpheres   = true
//                DetectTori      = true
//            } 
//        
//       // let x1 = benchmark pointset1 "primitiveScene.pts" o1 5
//       // let x2 = benchmark pointset1 "primitiveScene.pts" o2 5
//        
//           
//        let pointset2, _, _, density2 = importPTS (Path.combine [@"C:/Cache/" ; "JBs_Haus.pts"])
//        
//        let o3 : ShapeDetection.ShapeDetectionOptions = 
//            {
//                Epsilon             = 1.0f * density2
//                BitmapThreshold     = 2.0f * density2
//                NormalsThreshold    = 0.95f
//                MinimalSupport      = 500
//                Probability         = 0.001f
//
//                DetectPlanes    = true
//                DetectCylinder  = false
//                DetectCones     = false
//                DetectSpheres   = false
//                DetectTori      = false
//            }   
//        
//        let o4 : ShapeDetection.ShapeDetectionOptions = 
//            {
//                Epsilon             = 1.0f * density2
//                BitmapThreshold     = 2.0f * density2
//                NormalsThreshold    = 0.95f
//                MinimalSupport      = 500
//                Probability         = 0.001f
//
//                DetectPlanes    = true
//                DetectCylinder  = true
//                DetectCones     = true
//                DetectSpheres   = true
//                DetectTori      = true
//            } 
  
        
        //let x3 = benchmark pointset2 "JBs_Haus.pts" o3 5;
        //let x4 = benchmark pointset2 "JBs_Haus.pts" o4 5;

        let pointset3, _, _, density3 = importPTS (Path.combine [@"C:/Cache/" ; "Technologiezentrum_Teil1.pts"])

        let o5 : ShapeDetection.ShapeDetectionOptions = 
            {
                Epsilon             = 1.0f * density3
                BitmapThreshold     = 2.0f * density3
                NormalsThreshold    = 0.95f
                MinimalSupport      = 500
                Probability         = 0.001f

                DetectPlanes    = true
                DetectCylinder  = false
                DetectCones     = false
                DetectSpheres   = false
                DetectTori      = false
            }  
        
        let o6 : ShapeDetection.ShapeDetectionOptions = 
            {
                Epsilon             = 1.0f * density3
                BitmapThreshold     = 2.0f * density3
                NormalsThreshold    = 0.95f
                MinimalSupport      = 500
                Probability         = 0.001f

                DetectPlanes    = true
                DetectCylinder  = true
                DetectCones     = true
                DetectSpheres   = true
                DetectTori      = true
            }  

        let x5 = benchmark pointset3 "Technologiezentrum_Teil1.pts" o5 5;
       // let x6 = benchmark pointset3 "Technologiezentrum_Teil1.pts" o6 5;
        

       // let results = [|x1;x2;x3;x4;x5;x6|] |> Array.concat

        
        x5 |> Array.iter (fun s -> Log.line "%s" s)


//        let shapes = 
//            Array.Resize(&pointsPerShape, (Array.length pointsPerShape - 1))
//            Array.zip shapes pointsPerShape 
//        
//
//        // Update PrimitiveShape for each assigned point
//        let shapes = shapes |> Array.mapi(fun shapeIndex (shape: Aardvark.VRVis.PrimitiveShape, positions:V3d[]) ->
//            //Log.warn "Shape found %A" shape
//            let shape =
//                match shape with
//                | :? Aardvark.VRVis.PlaneShape      as s    -> PlaneShape   (s.Plane.BoundingQuad3d positions)                                                
//                | :? Aardvark.VRVis.SphereShape     as s    -> SphereShape  (s.Sphere)        
//                | :? Aardvark.VRVis.CylinderShape   as s    -> CylinderShape(s.Cylinder.Ranged positions)  
//                | :? Aardvark.VRVis.TorusShape      as s    -> TorusShape   (Torus3d(s.Position, s.Direction, s.MajorRadius, s.MinorRadius))
//                | :? Aardvark.VRVis.ConeShape       as s    ->
//                                                        
//                    let range = s.Cone.Range positions
//
//                    let (flipDirection, range) = 
//                        match range.Min >= 0.0 , range.Max >= 0.0 with 
//                        | true, true    -> false, range
//                        | true, false   -> true , Range1d(0.0, -range.Max)
//                        | false, true   -> false, Range1d(0.0, range.Max)
//                        | false, false  -> true, Range1d([-range.Min; -range.Max])
//
//                    let direction = if flipDirection then -s.Cone.Direction else s.Cone.Direction
//                    let cone = Cone3d(s.Cone.Origin, direction, s.Cone.Angle)
//                    ConeShape(cone,  range)            
//                | _                                     -> failwith ("Type Matching Error - Unsupported PrimitiveShape: " +  shape.GetType().Name)                               
//            shape 
//        )
//        Log.warn "Num Shapes: %i" (shapes.Length)
//        let (pos, normals, colors, texcoords) = points  |> Array.map (fun p -> V3f p.Position, V3f p.Normal, p.Color, V2f.OO)
//                                                        |> Array.unzip4
//
//
//        let pointGeometry = IndexedGeometry (
//                                Mode                = IndexedGeometryMode.PointList,       
//                                IndexedAttributes   = SymDict.ofList [  DefaultSemantic.Positions,  pos     :> Array
//                                                                        DefaultSemantic.Normals,    normals :> Array
//                                                                        DefaultSemantic.Colors,     colors  :> Array
//                                                                        DefaultSemantic.DiffuseColorCoordinates,     texcoords  :> Array
//                                                                    ]
//                                            )
//        
//
//
//        let camUp       = V3d.OOI
//        let position    = boundingBox.Center
//        let target      = centroid
//
//        let cameraView  =  DefaultCameraController.control win.Mouse win.Keyboard win.Time (CameraView.LookAt(position, target, V3d.OOI))    
//        let frustum     =  win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))       
//            
//        let viewTrafo   = cameraView    |> Mod.map CameraView.viewTrafo
//        let projTrafo   = frustum       |> Mod.map Frustum.projTrafo        
//
//
//        let state = Mod.init true
//
//        
//        let enabledPoints = state |> Mod.map (fun b -> b) 
//        let enabledShapes = state |> Mod.map (fun b -> not b) 
//
//
//        let random = Random()
//        let shapesSg = shapes   |> Array.map (fun shape -> 
//                                            let color = C4f(random.NextDouble() |> float32, random.NextDouble()|> float32, random.NextDouble()|> float32, 0.8f)
//                                            let color2 = color * 0.8
//                                            let sg = shape   |> Sg.ofPrimitiveShape
//                                            
//                                            let sg1 = sg    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Gray |> toEffect; DefaultSurfaces.constantColor color  |> toEffect]
//                                            let sg2 = sg    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Gray |> toEffect; DefaultSurfaces.constantColor color2  |> toEffect]
//                                                            |> Sg.fillMode (FillMode.Line |> Mod.constant)  
//                                                            //|> Sg.depthTest (DepthTestMode.None |> Mod.constant)  
//                                            Sg.ofList [sg1 ; sg2]
//                                    )
//                                |> Sg.ofArray
//                                |> Sg.trafo Trafo3d.Identity'
//                                //|> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Gray |> toEffect; DefaultSurfaces.uniformColor (C4f(0.5, 0.5, 0.5, 0.5) |> Mod.constant )  |> toEffect]
//                                |> Sg.blendMode (BlendMode.Blend |> Mod.constant)
//                                |> Sg.viewTrafo viewTrafo
//                                |> Sg.projTrafo projTrafo
//                                |> Sg.onOff enabledShapes
//                   
//                              
//        let pointSg = 
//            pointGeometry   |> Sg.ofIndexedGeometry
//                            |> Sg.trafo Trafo3d.Identity'
//                            |> Sg.uniform "PointSize" (Mod.constant 0.1)
//                            |> Sg.uniform "ViewportSize" (win.Sizes)
//                            |> Sg.viewTrafo viewTrafo
//                            |> Sg.projTrafo projTrafo
//                            |> Sg.surface (InteractiveShapeDetection.Visualization.Surfaces.PointSpriteLighting win.FramebufferSignature |> Mod.constant)
//                            |> Sg.onOff enabledPoints
//      
//        
//
//        win.Keyboard.Up.Values.Add(fun key -> 
//            match key with
//            | Aardvark.Application.Keys.Space -> 
//                let oldState = state |> Mod.force
//                let newState = not oldState
//
//                transact(fun () -> Mod.change state newState)
//                    
//            | _ -> ()
//            )
//
//        let sg_final = Sg.group' [pointSg;  shapesSg]
//
//
//        let clearTask = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.White, Mod.constant 1.0)
//
//        let renderTask =
//            app.Runtime.CompileRender(win.FramebufferSignature, sg_final)
//              //  |> DefaultOverlays.withStatistics
//        
//
//        win.RenderTask <- RenderTask.ofList [clearTask; renderTask]
//
//
//
//        
//        win.Run()
       


        Console.ReadKey() |> ignore
        0