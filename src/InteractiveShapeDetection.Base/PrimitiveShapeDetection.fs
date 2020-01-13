namespace InteractiveShapeDetection.Base

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Rendering.NanoVg
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading

open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native


open System.Runtime.Serialization



module ShapeDetection = 
    
    type ShapeDetectionOptions = 
        {
        Epsilon             : float32
        BitmapThreshold     : float32
        NormalsThreshold    : float32
        MinimalSupport      : int
        Probability         : float32

        DetectPlanes        : bool
        DetectCylinder      : bool
        DetectCones         : bool
        DetectSpheres       : bool
        DetectTori          : bool
        }
    


    type CandidateNode =
        {
        path                    : int[]
        node                    : OctreeNode
        cell                    : GridCell
        distanceToFocalPlane    : float 
        }


        static member DetectPrimitiveShapes (o : ShapeDetectionOptions) (candidateNode : CandidateNode) = 
            
            let node = candidateNode.node
        
            
            let (positions, normals) = node.Points.Value    |> Array.map(fun p -> (p.Position, p.Normal))
                                                            |> Array.unzip
            
            // Run Schnabel algorithm
            let mutable (pointsPerShape : V3d[][]) = [||]
        
       
            if(positions |> Array.length < 2) then 
                None
            else 
                
                // Run Schnabel ShapeDetection
                let shapes = Aardvark.VRVis.PrimitiveShapesWrapper.Run(
                                positions, normals, &pointsPerShape,
                                o.Epsilon, o.BitmapThreshold, o.NormalsThreshold, o.MinimalSupport, o.Probability, 
                                o.DetectPlanes, o.DetectCylinder, o.DetectCones, o.DetectSpheres, o.DetectTori);

        
                let unusedPoints = pointsPerShape |> Array.last
                Array.Resize(&pointsPerShape, (Array.length pointsPerShape - 1))
                
                
                let shapes = Array.zip shapes pointsPerShape 


                // Rebuild the node

                // Clear previously assigned Primitive Shapes
                let points = node.Points.Value |> Array.map(fun point -> Point(point.Position, point.Normal, point.Color, None, point.IsSelected))
                                
                // Create KDTree for this node
                let kdTree = RkDTree.CreateFromData (node.Positions()) (node.KdTreeData.Value)


                // Update PrimitiveShape for each assigned point
                let shapes = shapes |> Array.mapi(fun shapeIndex (shape: Aardvark.VRVis.PrimitiveShape, positions:V3d[]) ->
                                //Log.warn "Shape found %A" shape
                                let shape =
                                    match shape with
                                    | :? Aardvark.VRVis.PlaneShape      as s    -> PlaneShape (s.Plane.BoundingQuad3d positions)                                                
                                    | :? Aardvark.VRVis.SphereShape     as s    -> SphereShape (s.Sphere)        
                                    | :? Aardvark.VRVis.CylinderShape   as s    -> CylinderShape (s.Cylinder.Ranged positions)  
                                    | :? Aardvark.VRVis.TorusShape      as s    -> TorusShape  (Torus3d(s.Position, s.Direction, s.MajorRadius, s.MinorRadius))
                                    | :? Aardvark.VRVis.ConeShape       as s    ->
                                                        
                                        let range = s.Cone.Range positions

                                        let (flipDirection, range) = 
                                            match range.Min >= 0.0 , range.Max >= 0.0 with 
                                            | true, true    -> false, range
                                            | true, false   -> true , Range1d(0.0, -range.Max)
                                            | false, true   -> false, Range1d(0.0, range.Max)
                                            | false, false  -> true, Range1d([-range.Min; -range.Max])

                                        let direction = if flipDirection then -s.Cone.Direction else s.Cone.Direction
                                        let cone = Cone3d(s.Cone.Origin, direction, s.Cone.Angle)
                                        ConeShape(cone,  range)            
                                    | _                                     -> failwith ("Type Matching Error - Unsupported PrimitiveShape: " +  shape.GetType().Name)
                                                
                                // Assign the correct shape index to each point
                                positions |> Array.iter(fun  position -> 
                                                            // Get index of point uisng kdTree
                                                            let (index : int)   = int (kdTree.GetClosest(position).Index)
                                                            let point           = points.[index]
                                                            points.[index]      <- Point(point.Position, point.Normal, point.Color, (Some shapeIndex), point.IsSelected)
                                                            ) 
                                // return the shape
                                shape 
                            )
                Some (shapes, points)

        
        static member SelectCandidateNode (view)(frustum)(mousePosition)(wantedNearPlaneDistance)(octree : Octree) = 
            
            
            let cameraPosition  = view  |> CameraView.position
            let cameraForward   = (view |> CameraView.forward).Normalized
            let focalDistance   = 10.0
            let focalPlane      = Plane3d(cameraForward, cameraPosition + cameraForward * focalDistance)
        
            let viewTrafo = view    |> CameraView.viewTrafo
            let projTrafo = frustum |> Frustum.projTrafo
            
            
            let pickRay = mousePosition |> Ray3d.PickRay viewTrafo projTrafo
            
            // ViewFrustum Hull
            let hull =   (viewTrafo * projTrafo)    |> ViewProjection.toFastHull3d                    
            

            // Tests if a candidateNode is used and if this candidate node can be traversed further
            let isCandidate(node: OctreeNode)(cell:GridCell)(path : int[]) = 
                let candidateNode = 
                    {
                    node                    = node 
                    cell                    = cell
                    path                    = path
                    distanceToFocalPlane    = focalPlane.GetMinimalDistanceTo(cell.BoundingBox.Center)
                    }
                let inner = 
                    match node with 
                    | Node (_,children)     -> true
                    | _                     -> false
                

                let bb      = cell.BoundingBox
                let lodDataNode = 
                    { 
                        id = (node :> obj); level = (Array.length path) ; bounds = bb; 
                        inner = inner; granularity = Fun.Cbrt(bb.Volume / 1000.0); 
                        render = true
                    }


                let render, traverseChildren = lodDataNode |> PointSetLodData.IsRendering hull view frustum wantedNearPlaneDistance
                
                let hasPrimitiveShapes  = node |> OctreeNode.hasPrimitiveShapes
                let t                   = ref 0.0
                let intersects          = cell.BoundingBox.Intersects (pickRay, t)
                 
                let candidateNodeOption       = 
                    if render && not hasPrimitiveShapes && intersects then 
                        Some candidateNode
                    else None

                (candidateNodeOption, traverseChildren && intersects)
               

            // Compares two candidate nodes and returns the better one
            let compare (c1 : CandidateNode) (c2 : CandidateNode)  = 
                if (Array.length c1.path) > (Array.length c2.path) then 
                    c1 
                else
                    c2  


            // Find all candidate nodes along the pick ray
            let candidateNodes = octree |> Octree.choose isCandidate
                                        |> Array.toList
            

            // Find the best candidate by comparing nodes using the compare function
            let candidate = 
                match candidateNodes with
                | [] -> None
                | head::tail -> Some (tail |> List.fold (fun c1 c2 -> compare c1 c2) head)
            
            candidate


        
    //
    // Entry Point for Shape detection. The routine selects the best candidate node from the octree and performs shape detection on it. 
    // It returns a new octree. 
    //
    let shapeDetectionTask view frustum mousePosition wantedNearplaneDistance octree = 

        let candidate = CandidateNode.SelectCandidateNode view frustum mousePosition wantedNearplaneDistance octree
        
        let octree = 
            match candidate with
            | None      ->  octree
            | Some c    ->  let density = float32 c.node.Density.Value
                            let options = 
                                {
                                    Epsilon             = 1.0f * density
                                    BitmapThreshold     = 2.0f * density
                                    NormalsThreshold    = 0.99f
                                    MinimalSupport      = 500
                                    Probability         = 0.00001f

                                    DetectPlanes    = true
                                    DetectCylinder  = false
                                    DetectCones     = false
                                    DetectSpheres   = false
                                    DetectTori      = false
                                }
                            // Detect Shapes
                            let shapesAndPoints = c |> CandidateNode.DetectPrimitiveShapes options               
                            
                            match shapesAndPoints with 
                            | None -> octree
                            | Some (shapes,points) -> 
                            
                            // Create a new octree node
                            let node = c.node   |> OctreeNode.setShapes (Some shapes)
                                                |> OctreeNode.setPoints points
                            

                            // Insert octree node into octree, thus creating a new octree
                            let octree = octree |> Octree.Insert node (c.path |> Array.toList) true
                            
                            octree
        
        octree

