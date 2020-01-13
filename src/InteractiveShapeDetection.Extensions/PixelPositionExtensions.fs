
[<AutoOpen>]
module PixelPositionExtensions
    
    open Aardvark.Base

    type PixelPosition with

        member x.ToNDC() =         
            let n   = x.NormalizedPosition
            V3d(2.0 * n.X - 1.0, 1.0 - 2.0 * n.Y, -1.0)       