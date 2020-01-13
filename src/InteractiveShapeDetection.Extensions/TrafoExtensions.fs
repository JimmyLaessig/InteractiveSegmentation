[<AutoOpen>]
module TrafoExtensions

    open Aardvark.Base
    open Aardvark.Base.Incremental


    type M44d with 

        member x.TransformLine (line: Line3d)       = Line3d(x.TransformPos (line.P0), x.TransformPos (line.P1))

        member x.TransformLineProj (line: Line3d)   = Line3d(x.TransformPosProj (line.P0), x.TransformPos (line.P1))
       

    type Trafo3d with
        
        static member Identity' = Trafo3d.Identity |> Mod.constant