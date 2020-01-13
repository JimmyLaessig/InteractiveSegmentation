[<AutoOpen>]
module Ray3dExtensions

    open Aardvark.Base
    

    type Ray3d with

        static member PickRay (viewTrafo: Trafo3d)(projTrafo: Trafo3d)(pp: PixelPosition) = 
            
            let viewProjectionTrafo = viewTrafo * projTrafo        
            let cameraPos = viewTrafo.Inverse.GetModelOrigin()
            
            let n   = pp.NormalizedPosition
            let ndc = V3d(2.0 * n.X - 1.0, 1.0 - 2.0 * n.Y, -1.0)

            let pixelPosWorld = viewProjectionTrafo.Backward.TransformPosProj ndc
            let worldDir = (pixelPosWorld - cameraPos).Normalized

            let p1 = pixelPosWorld
            //let p2 = pixelPosWorld + worldDir * 5000.0
            
            Ray3d(p1, worldDir)


            