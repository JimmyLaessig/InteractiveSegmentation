namespace InteractiveShapeDetection.Interaction

open Aardvark.Base

open Aardvark.Database
open System.Collections.Generic
open InteractiveShapeDetection.Base

open Aardvark.Base.Monads


[<AutoOpen>]
module Internal = 


    let rec matchFloats (d1 : float)(d2 : float) (epsilon : float) = 
        
        if (d1 > d2 ) then 
            matchFloats d2 d1 epsilon
        else
        // d1 is smaller than d2
           d1 / d2 >= epsilon


    let matchPositions (p1 : V3d)(p2: V3d)(epsilon : float) = 
        V3d.Distance(p1, p2) <= epsilon


    let matchDirections (v1 : V3d)(v2 : V3d)(epsilon : float)  = 
        abs ((V3d.Dot( v1, v2)))  >= epsilon


    

    let matchAxis (p00 : V3d)(p01 : V3d)(p10 : V3d)(p11 : V3d)(epsilon : float) = 
        let r1 = Ray3d(p00, (p01 - p00).Normalized)
        let r2 = Ray3d(p10, (p11 - p10).Normalized)

        let d = 
            [|
                r1.GetMinimalDistanceTo p10
                r1.GetMinimalDistanceTo p11
                r2.GetMinimalDistanceTo p00
                r2.GetMinimalDistanceTo p01
            |]
            |> Array.max

        d <= epsilon

    let matchRays (r1 : Ray3d)(r2 : Ray3d) = 
        matchDirections (r1.Direction)( r2.Direction)(0.95) &&  // Directions is similar
        r1.GetMinimalDistanceTo(r2) < 0.01
        
        


    let matchPlanes     (q0 : Quad3d)       (q1 : Quad3d)       = 
        let p0 = q0.Plane3d
        let p1 = q1.Plane3d
        
        
        let d = 
            [| 
            p0.GetMinimalDistanceTo q1.P0 
            p0.GetMinimalDistanceTo q1.P1 
            p0.GetMinimalDistanceTo q1.P2
            p0.GetMinimalDistanceTo q1.P3 

            p1.GetMinimalDistanceTo q0.P0 
            p1.GetMinimalDistanceTo q0.P1 
            p1.GetMinimalDistanceTo q0.P2
            p1.GetMinimalDistanceTo q0.P3 

            |] |> Array.max
        
        matchDirections (p0.Normal)(p1.Normal)(0.95) &&  // Normals point in the correct direction
        d <= 0.1


    let matchSpheres    (s1 : Sphere3d)     (s2 : Sphere3d)     = 
        matchFloats     (s1.Radius)(s2.Radius)(0.99) &&     // Radii are similar
        matchPositions  (s1.Center)(s2.Center)(0.01)        // Origins are close enough


    let matchCylinders  (c1 : Cylinder3d)   (c2 : Cylinder3d)   =   
        let r1 = c1.Axis.Ray3d
        let r2 = c2.Axis.Ray3d
        
        let d = 
            [| 
            r1.GetMinimalDistanceTo c2.P0 
            r1.GetMinimalDistanceTo c2.P1 
            r2.GetMinimalDistanceTo c1.P0
            r2.GetMinimalDistanceTo c1.P1 
            |] |> Array.max

        matchFloats (c1.Radius)(c2.Radius)(0.95)            &&         
        matchDirections (r1.Direction)(r2.Direction)(0.95)  &&  
        d <= 0.1
                                                            
                                                             
    let matchCones  (c1 : Cone3d) (c2 : Cone3d) = 
        matchFloats     (c1.Angle)      (c2.Angle)      0.99 &&      // Angles are similar
        matchDirections (c1.Direction)  (c2.Direction)  0.95 &&      // Axis are similar
        matchPositions  (c1.Origin)     (c2.Origin)     0.1


    let matchTori       (t1 : Torus3d)  (t2 : Torus3d)      = 
        matchPositions  (t1.Position)   (t2.Position)   (0.1)   &&
        matchDirections (t1.Direction)  (t2.Direction)  (0.95)  &&
        matchFloats     (t1.MajorRadius)(t2.MajorRadius)(0.99)  &&
        matchFloats     (t1.MinorRadius)(t2.MinorRadius)(0.99)

   

type Cluster = 
    {
        bounds  : PrimitiveShape
        nodes   : OctreeNode[]
        cells   : GridCell[]
        shapes  : PrimitiveShape[][]
    }


    member this.Intersect (ray : Ray3d) = 
        
            this.shapes |> Array.map(fun (shapes: PrimitiveShape[]) -> 
                                shapes |> Array.choose (fun shape -> ray |> shape.Intersect)
                        ) 
                        |> Array.concat
                        |> Array.tryMinBy(fun point -> 
                                            let t = ray.GetTOfProjectedPoint point
                                            if t < 0.0 then infinity else t
                                            ) 
                                    
    
    member this.Contains(epsilon)(alpha)(point : Point) = 
        point |> this.bounds.Contains epsilon alpha

    member this.Intersects(box : Box3d) = 
        
        this.bounds.Intersects box
    

    static member findClusterTemplate<'T when 'T : equality> 
        (baseShape          : 'T)
        (path               : int[])
        (maxLoDDifference   : int)
        (chooseFun          : PrimitiveShape -> 'T -> 'T option)
        (mapInverseFun      : 'T -> PrimitiveShape)
        (distanceFun        : 'T -> 'T -> float)
        (epsilon            : float)
        (calculateBoundsFun : 'T[] -> 'T)
        (octree             : Octree) : Cluster =
         

        
         // Find matching shapes for this baseShape
        let similarShapes = 
            octree  |> Octree.map   (fun n c p ->     
                if abs (p.Length - path.Length) > maxLoDDifference then       
                    [||]
                else       
                    match n.DetectedPrimitives.Value with
                    | None          -> [||]
                    | Some shapes   -> shapes   |> Array.choose (fun shape -> chooseFun shape baseShape )
                                                |> Array.map (fun s -> n, c, s) ) 
                    |> Array.concat
                    
       
        let similarShapes = 
            match similarShapes with
            // This should not happen, however to be on the safe side
            | [||]  ->  Log.warn "No similar shapes found"; 
                        match octree |> Octree.find path with
                        | None -> [|(octree.root.Value , octree.cell, baseShape)|]
                        | Some (n,c, p) ->[|n,c, baseShape|]
            | _     -> similarShapes
                
                      
        // Store index for each vertex node
        let graphVertices = similarShapes   |> Array.map (fun (a, b, c) -> c)
                                            |> Array.indexed    
        

        // Find the vertex for the base primitive Shape
        let baseVertex = 
            match graphVertices |> Array.tryFind (fun (i,v) ->  baseShape = v) with
            | None      -> graphVertices.[0]
            | Some v    -> v 


        let equalityFun (v0 : int *'T) (v1 : int * 'T) = (v0 |> fst) = (v1 |> fst)
        let distanceFun (v0 : int *'T) (v1 : int * 'T) = 
            if (v0 |> fst) = (v1 |> fst) then 
                0.0
            else 
                distanceFun (v0 |> snd) (v1 |> snd)


        // Create graph for to find connected component
        let graph           = Graph.CompleteGraph graphVertices equalityFun distanceFun  
        let cluster         = graph |> Graph.ConnectedComponent baseVertex epsilon
        let clusterShapes   = cluster.vertices.ToArray(cluster.vertices.Count) 
                                    |> Array.map (fun (i,v) -> similarShapes.[i])
        match clusterShapes with
        | [||] -> Log.warn "Empty cluster: Level : %i" (path |> Array.length)
        | _ -> ()      
                                    
        // Calculate Bounds
        let bounds = clusterShapes  |> Array.map(fun (a,b,c) -> c)
                                    |> calculateBoundsFun
            

        // Group shapes by node
        let (nodes, cells, shapes)   = clusterShapes                        
                                        |> Array.groupBy (fun (node, cell, shape) -> (node, cell))
                                        |> Array.map(fun (key, shapes) ->   
                                                        let shapes = shapes |> Array.map (fun (_,_,t) -> mapInverseFun t)
                                                        let node, cell = key
                                                        node, cell, shapes)
                                        |> Array.unzip3
        // Build cluster struct
        {
            bounds  = mapInverseFun bounds
            nodes   = nodes
            cells   = cells
            shapes  = shapes
        }
    

    static member findPlaneCluster(basePlane : Quad3d)(path: int[]) (epsilon : float)(maxLodDifference : int)(octree : Octree) : Cluster = 
        

        let chooseFun primitiveShape q1  = 
            match primitiveShape with
            | PlaneShape q2     -> if matchPlanes q1 q2 then Some q2 else None
            | _                 -> None
        

        let inverseMapFun shape = PlaneShape shape

        let distanceFun (q1 : Quad3d )(q2 : Quad3d) = q1.GetMinimalDistanceTo q2
            

        let calculateBoundsFun (shapes  : Quad3d[]) = 
            if shapes |> Array.length <= 1 then
                basePlane
            else
                let points  = shapes    |> Array.map (fun q -> q.GetPointArray())
                                        |> Array.concat
                let plane   = points    |> Plane3d.fittingPlane
                let bounds  = points    |> plane.BoundingQuad3d
                bounds


        Cluster.findClusterTemplate basePlane path maxLodDifference chooseFun inverseMapFun distanceFun epsilon calculateBoundsFun octree
        

     
    static member private findCylinderCluster(baseCylinder : Cylinder3d)(path: int[]) (epsilon : float)(maxLodDifference : int)(octree : Octree) : Cluster = 
        

        let chooseFun primitiveShape c1  = 
            match primitiveShape with
            | CylinderShape c2  -> if matchCylinders c1 c2 then Some c2 else None
            | _                 -> None


        let inverseMapFun shape = CylinderShape shape


        let distanceFun (c1 :Cylinder3d) (c2 :Cylinder3d) = 
            let r1 = Range1d([|c1.Axis.Ray3d.GetT (c1.P0); c1.Axis.Ray3d.GetT (c1.P1);|])
            let r2 = Range1d([|c2.Axis.Ray3d.GetT (c2.P0); c2.Axis.Ray3d.GetT (c2.P1);|])
            r1.GetMinimalDistanceTo r2

        
        let calculateBoundsFun (shapes  : Cylinder3d[]) = 

            if (shapes |> Array.length) <= 1 then
                baseCylinder
            else 
                let axis = baseCylinder.Axis.Ray3d

                let range = shapes  |> Array.map(fun c -> [|axis.GetTOfProjectedPoint(c.P0) ; axis.GetTOfProjectedPoint(c.P1)|]) 
                                    |> Array.concat 
                                    |> Range1d

                let p0 = axis.GetPointOnRay range.Min
                let p1 = axis.GetPointOnRay range.Max
                
                Cylinder3d(p0, p1, baseCylinder.Radius)
             

        Cluster.findClusterTemplate baseCylinder path maxLodDifference chooseFun inverseMapFun distanceFun epsilon calculateBoundsFun octree
        

    static member private findConeCluster(baseCone : Cone3d * Range1d)(path: int[]) (epsilon : float)(maxLodDifference : int)(octree : Octree) = 
        

        let chooseFun primitiveShape (c1, r1)  = 
            match primitiveShape with
            | ConeShape (c2, r2)    -> if matchCones c1 c2 then Some (c2, r2)else None
            | _                     -> None


        let inverseMapFun shape = ConeShape shape


        let distanceFun (c1 , r1 : Range1d) (c2, r2 : Range1d) = r1.GetMinimalDistanceTo r2

        
        let calculateBoundsFun (shapes  : (Cone3d * Range1d)[]) = 
            
            if (shapes |> Array.length) <= 1 then
                baseCone
            else 

            let range = shapes  |> Array.map(fun (c, r) -> [|r.Min; r.Max|]) 
                                |> Array.concat 
                                |> Range1d     
            (baseCone |> fst , range)
            

        Cluster.findClusterTemplate baseCone path maxLodDifference chooseFun inverseMapFun distanceFun epsilon calculateBoundsFun octree



    static member private findSphereCluster(baseSphere : Sphere3d)(path: int[]) (epsilon : float)(maxLodDifference : int)(octree : Octree) = 
        

        let chooseFun primitiveShape s1  = 
            match primitiveShape with
            | SphereShape s2    -> if matchSpheres s1 s2 then Some s2 else None
            | _                 -> None


        let inverseMapFun shape = SphereShape shape


        let distanceFun (s1 :Sphere3d) (s2 :Sphere3d) = V3d.Distance(s1.Center , s2.Center)

        

        let calculateBoundsFun (shapes  : (Sphere3d)[]) = 
            if shapes |> Array.length <= 1 then 
                baseSphere
            else
                
                let radius = shapes |> Array.map(fun s -> s.Radius) |> Array.max
                let center = shapes |> Array.map(fun s -> Point(s.Center, V3d.OOO, C4b.Black)) 
                                    |> calcCentroid                 
                                
                Sphere3d(center, radius)
            

        Cluster.findClusterTemplate baseSphere path maxLodDifference chooseFun inverseMapFun distanceFun epsilon calculateBoundsFun octree



    static member private findTorusCluster(baseTorus : Torus3d)(path: int[]) (epsilon : float)(maxLodDifference : int)(octree : Octree) = 
        
        let chooseFun primitiveShape t1  = 
            match primitiveShape with
            | TorusShape t2 -> if matchTori t1 t2 then Some t2 else None
            | _             -> None


        let inverseMapFun shape = TorusShape shape


        let distanceFun (t1 :Torus3d) (t2 :Torus3d) = V3d.Distance(t1.Position , t2.Position)
        

        let calculateBoundsFun (shapes  : Torus3d[]) = 

            if shapes |> Array.length <= 1 then 
                baseTorus
            else
                let majorRadius = shapes   |> Array.map(fun t -> t.MajorRadius) |> Array.max
                let minorRadius = shapes   |> Array.map(fun t -> t.MinorRadius) |> Array.max
                
                let direction   = baseTorus.Position
                let center      = baseTorus.Direction   
                                
                Torus3d(center, direction, majorRadius, minorRadius)
                
        Cluster.findClusterTemplate baseTorus path maxLodDifference chooseFun inverseMapFun distanceFun epsilon calculateBoundsFun octree
    
                

    static member findCluster (baseShape : PrimitiveShape)(path : int[])(epsilon : float)(maxLoDDifference : int)(octree : Octree) : Cluster= 
        
        match baseShape with
            | PlaneShape q      -> (Cluster.findPlaneCluster       q path epsilon maxLoDDifference octree)
            | CylinderShape c   -> (Cluster.findCylinderCluster    c path epsilon maxLoDDifference octree)
            | SphereShape s     -> (Cluster.findSphereCluster      s path epsilon maxLoDDifference octree)
            | ConeShape (c,r)   -> (Cluster.findConeCluster        (c,r) path epsilon maxLoDDifference octree)
            | TorusShape t      -> (Cluster.findTorusCluster       t path epsilon maxLoDDifference octree)


