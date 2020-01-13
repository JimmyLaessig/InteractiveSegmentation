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
open FShade

open Aardvark.SceneGraph.Semantics  
open Aardvark.Base.Rendering.Effects

module Effects = 
    
    type RulerVertex = 
        {
            [<Position>]            pos : V4d
            [<TexCoordAttribute>]   tc  : V2d
            [<Semantic("Length")>]  length   : float
        }
        

    let thickLine (line : Line<Vertex>) =
        triangle {

            let width = uniform.LineWidth


            let pp0 = line.P0.wp
            let pp1 = line.P1.wp

            let p0 = pp0.XYZ / pp0.W
            let p1 = pp1.XYZ / pp1.W

            let up      = V3d.OOI
            let forward = (p1.XYZ - p0.XYZ).Normalized
            
            
            let p00 = p0 - width * 0.5 * up
            let p10 = p0 + width * 0.5 * up
            let p11 = p1 + width * 0.5 * up
            let p01 = p1 - width * 0.5 * up

            
            let length = V3d.Distance(line.P0.wp.XYZ , line.P1.wp.XYZ)
 
            yield {  pos = uniform.ViewProjTrafo *  V4d(p00, 1.0); tc = V2d(0.0, 0.0); length = length}
            yield {  pos = uniform.ViewProjTrafo *  V4d(p10, 1.0); tc = V2d(1.0, 0.0); length = length}
            yield {  pos = uniform.ViewProjTrafo *  V4d(p01, 1.0); tc = V2d(0.0, 1.0); length = length}
            yield {  pos = uniform.ViewProjTrafo *  V4d(p11, 1.0); tc = V2d(1.0, 1.0); length = length}
        }  



    let ruler (vertex : RulerVertex) = 
        fragment {
            
            let rulerThickness = 0.001 // 1mm


            let distanceToOrigin = vertex.tc.Y * vertex.length 
            let cmDistance = distanceToOrigin - (distanceToOrigin |> int |> float)
            let color = V4d(cmDistance, cmDistance, cmDistance, 1.0)

            let isDmDash(distanceToOrigin : float)(dashThickness : float) = 
                
                let dashThicknessInDm = dashThickness * 10.0
                let distanceToOrignInDm = distanceToOrigin * 10.0
                let normalizedDmDistance = distanceToOrignInDm - (distanceToOrignInDm |> int |> float)
                
                if normalizedDmDistance <= dashThicknessInDm * 0.5  || normalizedDmDistance >= 1.0 - (dashThicknessInDm * 0.5) then 
                    true
                else 
                    false

            let isCmDash(distanceToOrigin : float)(dashThickness : float) = 
                
                let dashThicknessInCm = dashThickness * 100.0
                let distanceToOrignInCm = distanceToOrigin * 100.0
                let normalizedCmDistance = distanceToOrignInCm - (distanceToOrignInCm |> int |> float)
                
                if normalizedCmDistance <= dashThicknessInCm * 0.5  || normalizedCmDistance >= 1.0 - (dashThicknessInCm * 0.5) then 
                    true
                else 
                    false

            let color = 
                if isDmDash distanceToOrigin 0.01 || isCmDash distanceToOrigin 0.002 then 
                    V4d(0, 0, 0, 1)
                else
                    V4d(0.9, 0.9, 0.9, 1.0)
            return color
            //return V4d(1, 0, 0, 1)
        }

    
module Test =
 
  

    let main argv = 

        Ag.initialize()
        Aardvark.Init()
        
        use app = new OpenGlApplication()

        let win = app.CreateSimpleRenderWindow(4)

        win.Text <- "Aardvark rocks \\o/"
        win.Size <- V2i(1280, 1024)

   


        let cameraView  =  DefaultCameraController.control win.Mouse win.Keyboard win.Time (CameraView.LookAt(V3d(0, -1, 1), V3d(0, 0, 0), V3d.OOI))    
        let frustum     =  win.Sizes    |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))       
            
        let viewTrafo   = cameraView    |> Mod.map CameraView.viewTrafo
        let projTrafo   = frustum       |> Mod.map Frustum.projTrafo        


        let lines = [|Line3d(V3d(0.0, 0.0, 0.5), V3d(1.0, 0.0, 0.5))|] |> Mod.constant



                // Init Basic Dummy Geometry
        let planeGeometry =
            IndexedGeometry (
                Mode = IndexedGeometryMode.TriangleList,
                IndexArray = [| 0; 1; 2; 0; 2; 3 |],
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions,                  [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO |] :> Array
                        DefaultSemantic.DiffuseColorCoordinates,    [| V2f.OO; V2f.IO; V2f.II; V2f.OI |] :> Array
                        DefaultSemantic.Normals,                    [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |] :> Array
                    
                    ]        
            )    

        // Basis Scenegraph
        let sceneGraph =
            planeGeometry 
                |> Sg.ofIndexedGeometry
                |> Sg.trafo Trafo3d.Identity'
                |> Sg.viewTrafo     viewTrafo
                |> Sg.projTrafo     projTrafo
                |> Sg.effect        [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Gray |> toEffect]
                |> Sg.pass          Rendering.RenderPass.main 
                |> Sg.cullMode      (CullMode.None |> Mod.constant)

       

        let sg = lines  |> Sg.lines (Mod.constant C4b.White)
                        |> Sg.effect [DefaultSurfaces.trafo |> toEffect ; Effects.thickLine |> toEffect ; Effects.ruler |> toEffect]
                        //|> Sg.effect [DefaultSurfaces.trafo |> toEffect ; DefaultSurfaces.thickLine |> toEffect ; DefaultSurfaces.constantColor C4f.Red |> toEffect]
                        |> Sg.uniform "LineWidth" (Mod.constant 0.1)
                        |> Sg.trafo         Trafo3d.Identity'
                        |> Sg.viewTrafo     viewTrafo
                        |> Sg.projTrafo     projTrafo
                        


        let sg_final = Sg.group' [sg; sceneGraph]


        let clearTask = app.Runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.White, Mod.constant 1.0)

        let renderTask =
            app.Runtime.CompileRender(win.FramebufferSignature, sg_final)
              //  |> DefaultOverlays.withStatistics
        

        win.RenderTask <- RenderTask.ofList [clearTask; renderTask]

        win.Run()

        
        0