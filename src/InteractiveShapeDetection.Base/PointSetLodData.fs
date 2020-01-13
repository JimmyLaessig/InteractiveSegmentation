namespace InteractiveShapeDetection.Base

open Aardvark.Base
open Aardvark.Base.Native
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.Base.Rendering
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading
open Aardvark.Git
open Aardvark.Database
open System
  


type PointSetLodData(tree : IMod<Octree>, nodeCount : IMod<float>) =

    let bounds = tree.GetValue().bounds

    let mutable cells = []
    
    
    abstract member Traverse : (LodDataNode -> bool) ->unit
    abstract member BoundingBox : Box3d
    abstract member GetData : LodDataNode -> Async<Option<IndexedGeometry>>


    default x.Traverse (f : (LodDataNode -> bool)) = 
        let rec traverse (level : int) (cell : GridCell) (n : OctreeNode)  =
            let bb = cell.BoundingBox
            match n with
                | Empty -> ()
                | Node (points,children)  ->
                    let nn = { id = (n :> obj); level = level; bounds = bb; 
                                inner = true; granularity = Fun.Cbrt(bb.Volume / 1000.0); 
                                render = true}

                    if f nn then 
                        children |> Array.iteri (fun i child -> traverse (level + 1) (cell.GetChild i) child.Value) 
                | Leaf points ->                             
                    let nn = { id = (n :> obj); level = level; bounds = bb; 
                                inner = false; granularity = Fun.Cbrt(bb.Volume / 1000.0); render = true }
                    f nn |> ignore
                            
               
        let tree = Mod.force tree
        let v = tree.root.Value
        traverse 0 tree.cell tree.root.Value


    default x.BoundingBox = bounds

    
    default x.GetData (n : LodDataNode) : Async<Option<IndexedGeometry>> =           

        async {
            let node = n.id |> unbox<OctreeNode>

            

            let points =
                match node with
                    | Node (points,_) | Leaf points  -> points.Value
                    | Empty -> [||]
                    

            let (p,n,c,i) = points  |> Array.map ( fun p -> V3f p.Position, V3f p.Normal, p.Color,  V2f(if p.IsSelected then 1.0 else 0.0))
                                    |> Array.unzip4

            let r = 
                IndexedGeometry(
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions,                  p :> Array
                            DefaultSemantic.Normals,                    n :> Array
                            DefaultSemantic.Colors,                     c :> Array
                            DefaultSemantic.DiffuseColorCoordinates,    i :> Array
                            //(Sym.ofString "IsSelected"),                i :> Array
                        ]
                )

            return Some r
        }
            
    
    interface ILodData with
            
        member x.BoundingBox    = bounds
        member x.Traverse f     = x.Traverse f
        member x.GetData n      = x.GetData n
        member x.Dependencies   = [ tree ]
      


    static member IsRendering (hull: FastHull3d)(cameraView: CameraView)(frustum: Frustum)(wantedNearPlaneDistance: float)(node : LodDataNode) = 
            
        let viewTrafo = CameraView.viewTrafo cameraView

        let mutable renderNode  = false
        let mutable traverse    = false
       

        if hull.Intersects(node.bounds) then
            
            renderNode <- true
            
            if node.inner then
                let bounds = node.bounds

                let depthRange =
                    bounds.ComputeCorners()
                        |> Array.map viewTrafo.Forward.TransformPos
                        |> Array.map (fun v -> -v.Z)
                        |> Range1d

                let depthRange      = Range1d(clamp frustum.near frustum.far depthRange.Min, clamp frustum.near frustum.far depthRange.Max)
                let projAvgDistance = abs (node.granularity / depthRange.Min)
                

                if projAvgDistance > wantedNearPlaneDistance then
                    traverse <- true


        (renderNode, traverse)

        
    static member WantedNearPlaneDistance (windowSize : V2i)(targetPointDistance : float) = 
        2.0 * float targetPointDistance / float (max windowSize.X windowSize.Y)