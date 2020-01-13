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
open Aardvark.VRVis

open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Global
open System.Runtime.Serialization


module App =
    

    open Aardvark.Base.Ag



    [<EntryPoint>]
    let main argv = 


        Ag.initialize()
        Aardvark.Init()

        
        use app = new OpenGlApplication()

        let win = app.CreateSimpleRenderWindow()
        win.Text <- "Aardvark rocks \\o/"
        win.Size <- V2i(1280, 1024)
        

        
        let cone1 = Cone3d(V3d.OOO, V3d.IOO, Conversion.RadiansFromDegrees(45.0))
        let cone2 = Cone3d(-V3d.IOO, -V3d.IOO, Conversion.RadiansFromDegrees(45.0))
        

        let pointsCone1 = cone1 |> PointCloudGenerator.Cone 1000 (C4b.Black) 1.0 0.001 
        let pointsCone2 = cone2 |> PointCloudGenerator.Cone 1000 (C4b.Black) 1.0 0.001

        

        
           

        
        let (pos, normals) = pointsCone1    |> Array.append pointsCone2 
                                            |> Array.map(fun point -> (point.Position, point.Normal))
                                            |> Array.unzip

        
        let kdTree = pos.CreateRkdTreeDist2(1e-6)
        let avgDist = pos |> Array.averageBy(fun p -> kdTree.GetClosest(p, System.Double.MaxValue,2).[0].Dist) |> float32


        let options  = Aardvark.VRVis.PlanarSegmentationSchnabelOptions(1.0f * avgDist, 2.0f * avgDist, 0.9f, 500, 0.001f)
        
                

        // run Schnabel algorithm
        let mutable (pointsPerShape : V3d[][]) = [||]
        

        Log.startTimed "Shape Detection"
        let shapes = Aardvark.VRVis.PrimitiveShapesWrapper.Run(
                        pos, normals, &pointsPerShape,
                        options.Threshold, options.BitmapThreshold, options.NormalThreshold, options.MinimalSupport, options.Probability,
                        true, true, true, true, false
                        );
        Log.stop()
                    
        
        
        let camPos      = V3d(0, 0, 120)
        let camTarget   = V3d.OOO
        let camUp       = V3d.OIO
        
       


        let cameraView  =  DefaultCameraController.control win.Mouse win.Keyboard win.Time (CameraView.LookAt(camPos, camTarget, camUp))

       
        let frustum     =  win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))       
        
              

        let viewTrafo   = cameraView    |> Mod.map CameraView.viewTrafo
        let projTrafo   = frustum       |> Mod.map Frustum.projTrafo        



        let color = C4f(0.5f, 0.5f, 0.5f, 0.5f)

        let shapes = shapes |> Array.mapi (fun i (shape : Aardvark.VRVis.PrimitiveShape) -> 
                                            match shape with
                                            | :? Aardvark.VRVis.PlaneShape      as s -> PlaneShape(s.Plane.BoundingQuad3d(pointsPerShape.[i]))                                   
                                            | :? Aardvark.VRVis.SphereShape     as s -> SphereShape(s.Sphere)                                   
                                            | :? Aardvark.VRVis.CylinderShape   as s -> CylinderShape(s.Cylinder.Ranged(pointsPerShape.[i]))            
                                            | :? Aardvark.VRVis.TorusShape      as s -> TorusShape(Torus3d(s.Position, s.Direction, s.MajorRadius, s.MinorRadius))          
                                            | :? Aardvark.VRVis.ConeShape       as s -> 
                                                
                                                let range = s.Cone.Range (pointsPerShape.[i])

                                                let (flipDirection, range) = 
                                                    match range.Min >= 0.0 , range.Max >= 0.0 with 
                                                    | true, true    -> false, range
                                                    | true, false   -> true , Range1d(0.0, -range.Max)
                                                    | false, true   -> false, Range1d(0.0, range.Max)
                                                    | false, false  -> true, Range1d([-range.Min; -range.Max])

                                                let direction = if flipDirection then -s.Cone.Direction else s.Cone.Direction
                                                let cone = Cone3d(s.Cone.Origin, direction, s.Cone.Angle)
                                                ConeShape(cone,  s.Cone.Range (pointsPerShape.[i]))     
                                                                               
                                            | _ -> failwith "Unsupported Aardvark.VRVis.PrimitiveShape"    
                                            )
        
        let intersects = 
            adaptive {
                let! pp         = win.Mouse.Position
                let! viewTrafo  = viewTrafo
                let! projTrafo  = projTrafo

                let pickRay     = pp |> Ray3d.PickRay viewTrafo projTrafo

                let raycastHits = shapes |> Array.choose (fun s -> s.Intersect pickRay)
                

                return raycastHits |> Array.length > 0
            }

        //intersects |> Mod.unsafeRegisterCallbackKeepDisposable(fun b -> Log.warn "%b" b) |> ignore

        
                                
        let shapeSg = shapes    |> Array.map (fun shape -> Sg.ofPrimitiveShape shape)
                                |> Sg.ofArray
                                |> Sg.trafo Trafo3d.Identity'
                                |> Sg.viewTrafo viewTrafo
                                |> Sg.projTrafo projTrafo
                                |> Sg.effect [DefaultSurfaces.trafo|> toEffect; DefaultSurfaces.constantColor color |> toEffect]
                                |> Sg.blendMode (Mod.constant BlendMode.Blend)
                                |> Sg.writeBuffers' (Set.singleton DefaultSemantic.Colors)


        
        let pointGeometry = IndexedGeometry (
                                Mode                = IndexedGeometryMode.PointList,       
                                IndexedAttributes   = SymDict.ofList [DefaultSemantic.Positions, pos :> Array]
                                            )
        
        

        let normalLines = Array.map2 (fun p n -> [|p; p + n * 0.05|]) pos normals
                            |> Array.concat

        let normalsGeometry = IndexedGeometry (
                                Mode                = IndexedGeometryMode.LineList,       
                                IndexedAttributes   = SymDict.ofList [DefaultSemantic.Positions, normalLines :> Array]
                                
                                )

        
        let sg = 
            pointGeometry   |> Sg.ofIndexedGeometry
                            |> Sg.trafo Trafo3d.Identity'
                            |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.pointSprite |> toEffect; DefaultSurfaces.constantColor C4f.Red |> toEffect]
                            |> Sg.uniform "PointSize" (Mod.constant 5)
                            |> Sg.uniform "ViewportSize" (win.Sizes)
                            |> Sg.viewTrafo viewTrafo
                            |> Sg.projTrafo projTrafo
        
        let sg2 = 
            normalsGeometry |> Sg.ofIndexedGeometry

                            |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Blue |> toEffect]
                            |> Sg.trafo Trafo3d.Identity'
                            |> Sg.viewTrafo viewTrafo
                            |> Sg.projTrafo projTrafo

        let sg_final = Sg.group' [ Sg.ofList []; sg; sg2;shapeSg ]
                         
        

        let clearTask = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.White, Mod.constant 1.0)

        let renderTask =
            app.Runtime.CompileRender(win.FramebufferSignature, sg_final)
                |> DefaultOverlays.withStatistics
        

        win.RenderTask <- RenderTask.ofList [clearTask; renderTask]
        //win.RenderTask <- renderTask
        win.Run()

        
        0