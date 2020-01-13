namespace InteractiveShapeDetection.Base
open System
open Aardvark.Base
open Aardvark.Base.Native
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
//open Aardvark.VRVis
open InteractiveShapeDetection
[<AutoOpen>]
module Cylinder3d = 
    
    let alignAxis (positions: V3d[]) (cylinder : Cylinder3d)= 
               

        let ray = Ray3d(cylinder.P0, cylinder.Axis.Direction.Normalized)
        
        let mutable minT = Double.MaxValue
        let mutable maxT = Double.MinValue

        positions |> Array.iter  (fun p -> 
                                let mutable t =  0.0
                                let p = ray.GetClosestPointOn(p, &t)

                                if t > maxT then maxT <- t
                                if t < minT then minT <- t
                              )


        let p0 = ray.GetPointOnRay(minT)
        let p1 = ray.GetPointOnRay(maxT)
        let axis = Line3d(p0, p1)

        Cylinder3d(axis, cylinder.Radius)



[<AutoOpen>]
module PrimitiveShapeExtensions = 
        

    module Sg =
        

        let ofPrimitiveShape (shape : PrimitiveShape)= 
            
            match shape with
            | PlaneShape q      -> q    |> IndexedGeometry.ofQuad3d
                                        |> Sg.ofIndexedGeometry
                                        |> Sg.cullMode (Mod.constant CullMode.None)
                                  
            | SphereShape s     ->  IndexedGeometryPrimitives.solidSubdivisionSphere s 10 C4b.Black
                                        |> Sg.ofIndexedGeometry
                                        |> Sg.cullMode (Mod.constant CullMode.Clockwise)
                                                   
            | CylinderShape c   ->  IndexedGeometryPrimitives.solidCylinder c.P0 c.Axis.Direction.Normalized c.Height c.Radius c.Radius 32 C4b.Black
                                        |> Sg.ofIndexedGeometry
                                        |> Sg.cullMode (Mod.constant CullMode.Clockwise)
                                                                                       
            | ConeShape (c,r)   ->  c   |> IndexedGeometry.ofCone3d (r) (32) (C4b.Black)
                                        |> Sg.ofIndexedGeometry
                                        |> Sg.cullMode (Mod.constant CullMode.Clockwise)

            | TorusShape t      -> IndexedGeometryPrimitives.solidTorus t C4b.Black 72 32 
                                        |> Sg.ofIndexedGeometry
                                        |> Sg.cullMode (Mod.constant CullMode.Clockwise)

    type PrimitiveShape with 
        
        member this.GetClosestPointOn (point : V3d) = 
            match this with

            | PlaneShape q      -> q.Plane3d.GetClosestPointOn point          
            | SphereShape s     -> s.GetClosestPointOn point
                                                   
            | CylinderShape c   -> c.GetClosestPointOn point
                                                                                       
            | ConeShape (c,r)   -> c.GetClosestPointOn point

            | TorusShape t      -> t.GetClosestPointOn point
                                   
                                   
 
        member this.GetNormal (point:V3d) = 

           match this with
            | PlaneShape q      ->  q.Normal       
            | SphereShape s     ->  (point - s.Center).Normalized                     
            | CylinderShape c   ->  let pProj = c.Axis.Ray3d.GetClosestPointOn point
                                    (point - pProj).Normalized                                                   
            | ConeShape (c,r)   ->  let p0      = c.GetAxis().GetClosestPointOn point
                                    let sgn     = (p0 |> c.GetAxis().GetT |> sign |> float)
            
                                    let tangent = V3d.Cross ((p0 - point).Normalized, c.GetAxis().Direction.Normalized)
            
                                    let l       = Line3d(c.Origin, point)
                                    let n       =  sgn * V3d.Cross(tangent, l.Direction.Normalized).Normalized
                                    n

            | TorusShape t      ->  let pProj = t.MajorCircle.GetClosestPointOn point
                                    (point - pProj).Normalized


        member this.Contains (epsilon)(alpha)(point : Point) = 

            (this.GetMinimalDistanceTo point.Position) <= epsilon //&&
            //V3d.Dot(this.GetNormal point.Position , point.Normal) |> abs >= alpha


        static member trianglulate (shape : PrimitiveShape) = 

            
            match shape with
            | PlaneShape q          ->  
                                        [|V3f(q.P0) ; V3f(q.P1) ; V3f(q.P2) ;
                                            V3f(q.P3) ; V3f(q.P2) ; V3f(q.P1)|]
                                        
            | SphereShape s         ->  let sphere = IndexedGeometryPrimitives.solidSubdivisionSphere s 10 C4b.Black
                                        match sphere.IndexedAttributes.Get (DefaultSemantic.Positions) with
                                        | :? (V3f[]) as p   -> p
                                        | :? (V3d[]) as p   -> p |> Array.map (fun v -> V3f(v))
                                        | _                 ->  [||]
                                        
            | CylinderShape c       ->  let cylinder = IndexedGeometryPrimitives.solidCylinder (c.P0)(c.Axis.Direction.Normalized )(c.Height)(c.Radius)(c.Radius)(32) (C4b.Black)
                                        match cylinder.IndexedAttributes.Get(DefaultSemantic.Positions) with
                                        | :? (V3f[]) as p   -> p
                                        | :? (V3d[]) as p   -> p |> Array.map (fun v -> V3f(v))
                                        | _                 -> [||]  
                                                                                                
            | _                     ->  Log.warn "Unsupported PrimitiveType : %s" (shape.GetType().Name); [||]

    