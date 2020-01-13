[<AutoOpen>]
module Line3dExtensions

    open Aardvark.Base
    
    type Line3d with
        member x.toPixelCoordinates (wvpTrafo : Trafo3d) (viewportSize : V2d) = 
            
            // [-1;1]
            let lineProj = wvpTrafo.Forward.TransformLineProj(x)

            // [0;1] * viewport
            let p0 = V2d(lineProj.P0.X * 0.5 + 0.5, 0.5 - lineProj.P0.Y * 0.5) * viewportSize
            let p1 = V2d(lineProj.P0.X * 0.5 + 0.5, 0.5 - lineProj.P0.Y * 0.5) * viewportSize
                       
            Line2d(p0, p1)