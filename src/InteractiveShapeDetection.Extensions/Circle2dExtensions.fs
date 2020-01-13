[<AutoOpen>]
module Circle2dExtensions

    open Aardvark.Base

    type Circle2d with
            member x.Intersects(line : Line2d) = 
                let line3d = Line3d(V3d(line.P0, 0.0),  V3d(line.P1, 0.0))            
                line3d.GetMinimalDistanceTo(V3d(x.Center, 0.0)) < x.Radius