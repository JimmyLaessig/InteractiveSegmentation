[<AutoOpen>]
module CameraViewExtensions

    open Aardvark.Base
    open Aardvark.Base.Incremental

    module CameraView = 

        let position (view : CameraView) =  (view |> CameraView.viewTrafo).Backward.TransformPos(V3d.Zero)
            
