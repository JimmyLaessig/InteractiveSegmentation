namespace InteractiveShapeDetection.Visualization
open Aardvark.Base
open InteractiveShapeDetection.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open Aardvark.Database
open System
  


type PointcloudLodProvider(tree : IMod<Octree>, nodeCount : IMod<float>, decisionFun : LodData -> bool * bool) =
    
    let bounds = tree.GetValue().bounds

    interface ILodProvider with
        
       
        member x.BoundingBox    = bounds


        member x.Traverse ()    = 

            let nodes = HashSet.empty

            let rec traverse (n : OctreeNode)(cell : GridCell)(path : int list)   =
                let bb = cell.BoundingBox
                let level = path |> List.length
                match n with
                | Empty             -> ()
                | Leaf _            -> 
                                let nn : LodData  = { id = (n :> obj); level = level; bounds = bb; 
                                    inner = true; granularity = Fun.Cbrt(bb.Volume / 1000.0); 
                                    render = true}

                                let useNode, traverseChildren = decisionFun nn
                                match useNode with
                                | true  -> nodes.Add nn |> ignore
                                | false -> ()

                                                                           
                | Node (_,children) ->   
                                let nn  : LodData = { id = (n :> obj); level = level; bounds = bb; 
                                    inner = false; granularity = Fun.Cbrt(bb.Volume / 1000.0); render = true }

                                let useNode, traverseChildren = decisionFun nn

                                match useNode,traverseChildren with
                                | false, false  -> ()
                                | false, true   ->  n.Children      |> Array.iteri(fun i child -> traverse (child.Value) (cell.GetChild i) (List.append path [i]))
                                                                    
                                | true, false   ->  nodes.Add nn    |> ignore
                                | true, true    ->  
                                                    n.Children      |> Array.iteri(fun i child -> traverse (child.Value) (cell.GetChild i) (List.append path [i]))
                                                    nodes.Add nn    |> ignore
                        
            let tree = Mod.force tree
            traverse tree.root.Value tree.cell []

            // Return collected LodDatas
            nodes



        member x.GetData n      = 
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
