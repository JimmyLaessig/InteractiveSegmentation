namespace InteractiveShapeDetection.Visualization

open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph.NaiveLod.Sg



[<CustomEquality; NoComparison>]
type LodData =
    {
        id          : obj
        level       : int
        bounds      : Box3d
        inner       : bool
        granularity : float
        render      : bool

        [<DefaultValue>]
        mutable uniqueId : int
    }

    override x.GetHashCode() = x.id.GetHashCode()
    override x.Equals o =
        match o with
            | :? LodData as o -> x.id.Equals(o.id)
            | _ -> false



type ILodProvider =

    abstract member BoundingBox     : Box3d
    abstract member Traverse        : unit -> HashSet<LodData>
    abstract member Dependencies    : list<IMod>
    abstract member GetData         : LodData -> Async<Option<IndexedGeometry>>










