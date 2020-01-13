namespace InteractiveShapeDetection.Base

    open Aardvark.Base
    open Aardvark.Base.Monads
    open System
    open System.Runtime.Serialization



    module Internal = 
        
        let IntersectTorus (ray : Ray3d) (torus : Torus3d) = 
            
            ray |> torus.Intersect
                |> Array.tryMinBy(fun point ->  let t = ray.GetTOfProjectedPoint point
                                                if t < 0.0 then infinity
                                                else t)
                                              



        let IntersectCone (ray : Ray3d)(range : Range1d) (cone : Cone3d) = 
            
            ray |> cone.Intersect 
                |> Array.filter (fun point -> 
                                    point   |> cone.GetHeight
                                            |> range.Contains
                                                    )
               |> Array.tryMinBy(fun point -> (point - ray.Origin).Length)
                                
            


        let IntersectCylinder (ray : Ray3d) (cylinder : Cylinder3d) = 
                           
            let intersections = cylinder.Intersect ray
            
            match intersections with
            | [||]  -> None
            | _     -> 
                
                let range = Range1d(0.0, cylinder.Height)
                intersections   |> Array.filter(fun point -> 
                                                point   |> cylinder.GetHeight
                                                        |> range.Contains
                                            )
                                |> Array.tryMinBy(fun point -> (point - ray.Origin).Length)
                                               
        


        let IntersectSphere (ray : Ray3d) (sphere : Sphere3d) = 
            ray |> sphere.Intersect
                |> Array.tryMinBy(fun point ->  let t = ray.GetTOfProjectedPoint point
                                                if t < 0.0 then infinity
                                                else t)
                               
                


    [<CustomEquality; NoComparison; Serializable>]
    type PrimitiveShape = 
    | PlaneShape    of Quad3d
    | SphereShape   of Sphere3d
    | CylinderShape of Cylinder3d
    | TorusShape    of Torus3d
    | ConeShape     of Cone3d * Range1d



        member this.Intersects(box : Box3d) = 
            match this with
            | PlaneShape    p       -> box.Intersects p
            | SphereShape   s       -> box.Intersects s
            | CylinderShape c       -> box.Intersects c
            | ConeShape     (c, r)  -> box.Intersects (c.BoundingCylinder3d r)
            | TorusShape    t       -> box.Intersects (t.BoundingCylinder3d)



        member this.Intersect (ray: Ray3d) = 
            match this with
            | PlaneShape    q           -> q.Intersect ray
            | SphereShape   s           -> s |> Internal.IntersectSphere ray
            | CylinderShape c           -> c |> Internal.IntersectCylinder ray
            | ConeShape     (c, r)      -> c |> Internal.IntersectCone ray r 
            | TorusShape    t           -> t |> Internal.IntersectTorus ray 
            


        member this.GetMinimalDistanceTo (point: V3d) = 
            match this with
            | PlaneShape    p       -> p.Plane3d.GetMinimalDistanceTo point
            | SphereShape   s       -> s.GetMinimalDistanceTo point
            | CylinderShape c       -> c.GetMinimalDistanceTo point
            | ConeShape     (c,r)   -> c.GetMinimalDistanceTo point
            | TorusShape    t       -> t.GetMinimalDistance   point

        

        override this.Equals(other) = 
            match other with
            | :? PrimitiveShape as p -> 
                match this, p with
                | PlaneShape    p1      , PlaneShape    p2      -> p1 = p2
                | SphereShape   s1      , SphereShape   s2      -> s1 = s2
                | CylinderShape c1      , CylinderShape c2      -> c1 = c2 
                | ConeShape     (c1, r1), ConeShape     (c2, r2)-> c1 = c2 && r1 = r2
                | TorusShape    t1      , TorusShape    t2      -> t1 = t2
                | _, _          -> false
            | _                 -> false 

        
       