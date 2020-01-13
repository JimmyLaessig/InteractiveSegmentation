namespace InteractiveShapeDetection.Visualization


open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg

open Aardvark.SceneGraph

open Aardvark.Application.WinForms




module Minimap = 
              
    let private effectMinimap  = 
        let e   = FShade.SequentialComposition.compose [  DefaultSurfaces.diffuseTexture |> toEffect ]
        let s   = FShadeSurface(e) :> ISurface 
        s |> Mod.constant


    let Init (win : SimpleRenderWindow)(view : IMod<CameraView>)(frustum : IMod<Frustum>)(renderPassMinimap : RenderPass)(scale: IMod<float>)(contentSg: ISg) = 
        
        let viewTrafo = view |> Mod.map CameraView.viewTrafo

        // View trafo to view the scene from above the camera
        let topViewTrafo    = viewTrafo |> Mod.map 
                                (fun v -> 
                                    let center      = v.Inverse.GetModelOrigin()
                                    let camPos      = center + V3d.OOI  * 200.0
                                    let camTarget   = camPos - V3d.OOI
                                    let camUp       = V3d.OIO                                           
                                    CameraView.lookAt camPos camTarget camUp |> CameraView.viewTrafo                                                                                    
                                )
        
        
        let topProjTrafo = Box3d.Unit.ComputeCorners()  |> Array.map   ( fun c -> 
                                                                                let x = (c.X * 2.0 - 1.0) * 100.0
                                                                                let y = (c.Y * 2.0 - 1.0) * 100.0
                                                                                let z = (c.Z * 2.0 - 1.0) * 1000.0
                                                                                V3d(x,y,z)
                                                                            )
                                                        |> Box3d
                                                        |> Frustum.ortho
                                                        |> Frustum.orthoTrafo
                                                        |> Mod.constant



        // Simple sphere marker at the position of the camera in world space
        let cameraMarkerSg = 
            Sg.unitSphere 10 (Mod.constant C4b.Red)
            |> Sg.trafo     (viewTrafo |> Mod.map(fun v-> v.Inverse))
            |> Sg.effect    [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Blue |> toEffect]


        // Simple frustum geometry visualizing the view frustum of the camera
        let frustumSg = 
            Sg.frustum'  view frustum
                |> Sg.effect    [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor  C4f.Magenta |> toEffect ]
        
        

        // The scenegraph collecting all objects depicted on the minimap
        let contentSg = 
            Sg.group' [contentSg; cameraMarkerSg; frustumSg]
                |> Sg.viewTrafo topViewTrafo
                |> Sg.projTrafo topProjTrafo
                |> Sg.pass (renderPassMinimap |> RenderPass.before "minimapContent" RenderPassOrder.Arbitrary)


        // The texture that contains the rendered minimap content
        let minimapTexture, _ = 
            contentSg
                |> Sg.compile win.Runtime win.FramebufferSignature
                |> RenderTask.renderToColorAndDepth' win.Sizes (C4f(0.5, 0.5, 0.5, 0.8)) 1.0
                
        

        // Trafo to scale and translate the minimap quad on the screen
        let minimapTrafo = Mod.map2 (fun (s: float) (size: V2i) -> 
                                                let ratio   = (float size.X) / (float size.Y)
                                                let scale   = V3d(s, s * ratio, 1.0) |> Trafo3d.Scale 
                                                
                                                let translation = Trafo3d.Translation((1.0 - s), -(1.0 - s * ratio), 0.0) 
                                                
                                                scale * translation
                                            )   scale win.Sizes                            

        // Complete scenegraph with the minimap
        Sg.fullscreenQuadTextured minimapTexture minimapTrafo
            |> Sg.pass renderPassMinimap   
            |> Sg.blendMode (BlendMode.Blend |> Mod.constant)