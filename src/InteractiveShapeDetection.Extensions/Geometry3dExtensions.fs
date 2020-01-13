
[<AutoOpen>]
module Geometry3dExtensions


    let solveQuadratic (a:float) (b:float) (c:float) (x0: ref<float>) (x1:ref<float>) = 

        let det = b*b - 4.0 * a * c

        match det >= 0.0 with
        | false -> false
        | true  -> 
            let q = 
                if (b > 0.0) then
                    -0.5 * ( b + sqrt det) 
                else     
                    -0.5 * ( b - sqrt det) 
            x0.Value <- q / a
            x1.Value <- c / q
            true

        

    open Aardvark.Base
    
    let (.*) (u:V3d) (v:V3d) = V3d.Dot(u, v)
    

    type Plane3d with 
        

        member x.BoundingQuad3d (points: V3d[]) = 
                            
            let pointsOnPlane = x.ProjectToPlaneSpace points

            let minX = (pointsOnPlane |> Array.minBy (fun p -> p.X)).X
            let minY = (pointsOnPlane |> Array.minBy (fun p -> p.Y)).Y

            let maxX = (pointsOnPlane |> Array.maxBy (fun p -> p.X)).X
            let maxY = (pointsOnPlane |> Array.maxBy (fun p -> p.Y)).Y  
            
            
            let p0 = x.Unproject(V2d(minX, minY))
            let p1 = x.Unproject(V2d(maxX, minY))
            let p2 = x.Unproject(V2d(minX, maxY))
            let p3 = x.Unproject(V2d(maxX, maxY))


            let polygon         = Polygon2d(pointsOnPlane)
            let convexContour   = polygon.ComputeConvexHullIndexPolygon().ToPolygon2d()
            
            let points          = convexContour.ComputeOrientedBoundingBox().GetPointArray()
            


            let p0 = x.Unproject(points.[1])
            let p1 = x.Unproject(points.[2])
            let p2 = x.Unproject(points.[0])
            let p3 = x.Unproject(points.[3])

            Quad3d(p0, p1, p2, p3)     



        static member fittingPlane (points : V3d[]) = 

            let centroid = (points |> Array.fold ( fun x y -> x + y ) V3d.Zero) / (float)(points |> Array.length)

            // Calc full 3x3 covariance matrix, excluding symmetries:
            let mutable xx = 0.0; 
            let mutable xy = 0.0; 
            let mutable xz = 0.0;
            let mutable yy = 0.0; 
            let mutable yz = 0.0; 
            let mutable zz = 0.0;


            points |> Array.iter (fun point -> 
                                    let p = point - centroid
                                    xx <- xx + p.X * p.X
                                    xy <- xy + p.X * p.Y
                                    xz <- xz + p.X * p.Z
                                    yy <- yy + p.Y * p.Y
                                    yz <- yz + p.Y * p.Z
                                    zz <- zz + p.Z * p.Z
                                    )

        
            let det_X = yy * zz - yz * yz
            let det_Y = xx * zz - xz * xz
            let det_Z = xx * yy - xy * xy

            let det_max = max (max det_X det_Y) det_Z
            let x = (det_max > 0.0) //"The points don't span a plane"

            let normal = 
                if (det_max = det_X) then
                    let x = 1.0
                    let y = (xz * yz - xy * zz) / det_X
                    let z = (xy * yz - xz * yy) / det_X
                    V3d(x, y, z).Normalized
                else if (det_max = det_Y) then
                    let x = (yz * xz - xy * zz) / det_Y
                    let y = 1.0
                    let z = (xy * xz - yz * xx) / det_Y
                    V3d(x , y, z).Normalized
                else
                    let x = (yz * xy - xz * yy) / det_Z
                    let y = (xz * xy - yz * xx) / det_Z
                    let z = 1.0
                    V3d(x,y,z).Normalized

            Plane3d(normal, centroid)
        
        
        member this.Intersect (ray :Ray3d) = 
            let t = ref 0.0
            if ray.Intersects(this, t) then 
                Some( ray.Origin + ray.Direction * t.Value)
            else
                None


    type Quad3d with
        
        member this.Intersect (ray : Ray3d) = 
            let t = ref 0.0
            match this.Intersects(ray, t) with
            | false -> None
            | true -> Some (ray.GetPointOnRay !t)


        member this.Plane3d = Plane3d(this.P0, this.P1, this.P2)


        member this.GetMinimalDistanceTo (quad : Quad3d) = 

            let mutable minDistance = infinity
            
            
            this.EdgeLineArray |> Array.iter (fun edge1 -> 
                quad.EdgeLineArray |> Array.iter (fun edge2 ->                    
                        minDistance <- min minDistance (edge1.GetMinimalDistanceTo(edge2) )      
                    )
                )

            minDistance


    type Sphere3d with
        

        member this.GetMinimalDistanceTo(point : V3d) = 
            V3d.Distance(this.Center , point) - this.Radius


        // Calculates the intersection points of the sphere with the ray and returns the point closest to the ray's origin
        member this.Intersect (ray : Ray3d) = 

            let L = ray.Origin - this.Center

            let a = V3d.Dot(ray.Direction, ray.Direction)
            let b = V3d.Dot(ray.Direction, L) * 2.0
            let c = V3d.Dot(L, L) - this.RadiusSquared    
            
            
            let ts = Aardvark.Base.Polynomial.RealRootsOf( a, b, c)
            
            let res = 
                [|ts.E0; ts.E1|] |> Array.choose(fun t -> 
                    if  t.IsNaN() then None
                    else
                        t |> ray.GetPointOnRay |> Some
                    )
            
            res


    type Cylinder3d with
        
       
        member this.Ranged(points :V3d[]) = 
            
            let axis    = Ray3d(this.P0, this.Axis.Direction.Normalized)
            
            let range = points |> Array.map  (fun p -> 
                                        let mutable t =  0.0
                                        let p = axis.GetClosestPointOn(p, &t)
                                        t
                                )
                                |> Range1d 
            let p0 = axis.GetPointOnRay range.Min
            let p1 = axis.GetPointOnRay range.Max       
        
            Cylinder3d(p0, p1, this.Radius)


        member this.GetMinimalDistanceTo (point : V3d) = 
           ((this.GetClosestPointOn point) - point).Length 



        member this.Intersect (ray : Ray3d) = 
            
            let a = this.Circle0.Center
            let b = this.Circle1.Center
            let r = this.Radius

            let o = ray.Origin
            let v = ray.Direction

            let ab = b - a
            let ao = o - a
            let aoXab = V3d.Cross(ao, ab)
            let vXab = V3d.Cross(v, ab)

            let a = vXab .* vXab
            let b = 2.0 * (vXab .* aoXab)
            let c = (aoXab .* aoXab) - r*r * (ab .*ab)


            let t0 = ref 0.0
            let t1 = ref 0.0

            let t = Aardvark.Base.Polynomial.RealRootsOf( a, b, c)

            [|t.E0; t.E1|] |> Array.choose(fun t -> 
                if t.IsNaN() then None
                else
                    t |> ray.GetPointOnRay |> Some
                    )

           
    type Cone3d with

        member this.BoundingCylinder3d(range : Range1d) = 

            let r1 = this.GetRadius(range.Min) |> abs
            let r2 = this.GetRadius(range.Max) |> abs
            let r = max r1 r2

            let p0 = this.GetAxis().GetPointOnRay range.Min
            let p1 = this.GetAxis().GetPointOnRay range.Max

            Cylinder3d(p0, p1, r)


        
        member this.Range (points : V3d[]) = 
            let axis = Ray3d(this.Origin, this.Direction.Normalized)

            points |> Array.map (fun p -> 
                                    let mutable t = 0.0
                                    let p = axis.GetClosestPointOn (p, &t)
                                    t
                                )
                    |> Range1d



        ///
        /// Fixed version for Cone3d Get closest point
        ///
        member this.GetClosestPointOn (p : V3d) = 
            

            let p_proj  = this.GetAxis().GetClosestPointOn p
            let height  = (this.Origin - p_proj).Length
            

            let radius  = this.GetRadius height
           
            let dir     = (p - p_proj).Normalized * radius

            let p_proj2 = p_proj + dir

            let ray2 = Ray3d(this.Origin, (p_proj2 - this.Origin).Normalized)

            ray2.GetClosestPointOn p
        

         member this.GetMinimalDistanceTo (point : V3d) = 
            ((this.GetClosestPointOn point) - point).Length



        ///
        /// http://lousodrome.net/blog/light/2017/01/03/intersection-of-a-ray-and-a-cone/
        ///
        member this.Intersect (ray : Ray3d) = 

            let D = ray.Direction.Normalized
            let V = this.Direction.Normalized
            let C = this.Origin
            let O = ray.Origin

            let CO = O - C

            let cosTheta_squared = cos( this.Angle) * cos( this.Angle)

            let COdotV = V3d.Dot(CO, V)
            let DdotV = V3d.Dot(D,V)

            let a = DdotV * DdotV - cosTheta_squared
            let b = 2.0 * (DdotV * COdotV - V3d.Dot(D, CO) * cosTheta_squared)
            let c = COdotV * COdotV - V3d.Dot(CO, CO) * cosTheta_squared
            
            let t0 = ref 0.0
            let t1 = ref 0.0

            match solveQuadratic a b c t0 t1 with
            | false -> [||]
            | true  -> 
                let p0 = ray.GetPointOnRay t0.Value
                let p1 = ray.GetPointOnRay t1.Value
                [|p0; p1|]

    
    type Circle3d with
        member this.GetClosestPointOn(point : V3d) = 

            let pProj = this.Plane.GetClosestPointOn point

            let v = (pProj - this.Center).Normalized
            let ray = Ray3d(this.Center, v)

            ray.GetPointOnRay this.Radius


    type Torus3d with

        member this.GetClosestPointOn (point : V3d) = 
          let pointOnCircle = this.MajorCircle.GetClosestPointOn point

          let v = (point - pointOnCircle).Normalized
          let ray = Ray3d(pointOnCircle, v)

          ray.GetPointOnRay this.MinorRadius

        
        member this.BoundingCylinder3d = 
            let r = this.MajorRadius + this.MinorRadius
            let p0 = this.Position - this.Direction.Normalized * this.MinorRadius
            let p1 = this.Position + this.Direction.Normalized * this.MinorRadius
            Cylinder3d(p0, p1, r)
        

        member this.Intersect (ray: Ray3d) = 
            
            let C = this.Position
            let A = this.Direction.Normalized
            let R = this.MajorRadius
            let r = this.MinorRadius

            let P0 = ray.Origin
            let P1 = ray.Direction.Normalized

            let Q = P0 - C

            let u = V3d.Dot(A, Q)
            let v = V3d.Dot(A, P1)

            let a = V3d.Dot(P1, P1)  - v * v
            let b = 2.0 * (V3d.Dot(Q, P1) - u * v)
            let c =  V3d.Dot(Q, Q) - u * u

            let d = V3d.Dot(Q, Q) + R * R - r * r
            
            let A =   1.0
            let B =   4.0 * V3d.Dot(Q, P1)
            let C =   2.0 * d + 0.25 * B * B - 4.0 * R * R * a
            let D =   B * d - 4.0 * R * R * b
            let E =   d * d - 4.0 * R * R * c

            let ts = Aardvark.Base.Polynomial.RealRootsOf(A, B, C, D, E)
            
            [|ts.E0 ; ts.E1 ; ts.E2 ; ts.E3|] |> Array.choose(fun t -> 
                if t.IsNaN() then None
                else t |> ray.GetPointOnRay |> Some
                    )



    type Box3d with
        
        member this.GetFaces() = 
                [|
                    Quad3d(this.OIO, this.IIO, this.OOO, this.IOO)
                    Quad3d(this.OOI, this.IOI, this.OII, this.III)
                    Quad3d(this.OOO, this.IOO, this.OOI, this.IOI)
                    Quad3d(this.IOO, this.IIO, this.IOI, this.III)
                    Quad3d(this.IIO, this.OIO, this.III, this.OII)
                    Quad3d(this.OIO, this.OOO, this.OII, this.OOI)
                |]