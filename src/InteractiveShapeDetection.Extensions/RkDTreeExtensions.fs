[<AutoOpen>]
module RkDTreeExtensions

    open Aardvark.Base
    open System

    module RkDTree =
        open Aardvark.VRVis


        let EmptyV3d ()= ([|V3d.Zero|]).CreateRkdTreeDist1(1e-6)


        let CreateFromData (points)(data) = 
                                Aardvark.VRVis.PointRkdTreeD<V3d[], V3d>(
                                        int64 3, 
                                        (int64 <| Array.length points), 
                                        points,
                                        Func<_,_,_>(fun _ps  i -> _ps.[int i]), 
                                        Func<_,_,_>(fun v i -> v.[i]),
                                        Func<_,_,_>(fun a b -> V3d.Distance(a, b)), 
                                        Func<_,_,_,_>(fun i a b -> b - a),
                                        Func<_,_,_,_>(fun a b c -> VecFun.DistanceToLine(a, b, c)), 
                                        Func<_,_,_,_>(fun a b c -> VecFun.Lerp(a,b,c)), 
                                        1e-6, 
                                        data);

        