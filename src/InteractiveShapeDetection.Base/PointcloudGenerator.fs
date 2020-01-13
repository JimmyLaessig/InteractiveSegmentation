namespace InteractiveShapeDetection.Base


open System
open Aardvark.Base

open Aardvark.Database

open InteractiveShapeDetection.Base
open Aardvark.Base.Incremental


module PointCloudGenerator = 
    

    let Noise (numPoints : int)(color :C4b)(boundingBox : Box3d) = 

        let random = Random()
        

        let points = Array.init numPoints (fun _ -> 

                // Random position
                let py  = (random.NextDouble() * 2.0 - 1.0) * boundingBox.SizeY 
                let pz  = (random.NextDouble() * 2.0 - 1.0) * boundingBox.SizeZ
                let px  = (random.NextDouble() * 2.0 - 1.0) * boundingBox.SizeX
                
                let p   = V3d(px, py, pz) + boundingBox.Center

                // Random normal
                let nx  = random.NextDouble() * 2.0 - 1.0
                let ny  = random.NextDouble() * 2.0 - 1.0
                let nz  = random.NextDouble() * 2.0 - 1.0
                
                let n   = V3d(nx, ny, nz).Normalized

                Point(p, n, color)
            )

        points


    let Cylinder (numPoints : int)(color :C4b)(jigger : float)(cylinder : Cylinder3d) = 

        let random  = Random()

        
        let axis    = cylinder.Axis.Ray3d
        
        let r = RandomSystem()

        let circle = cylinder.Circle0

        //let rndPoints = bounds |> Noise numPoints color

        Array.init numPoints (fun _ -> 
            

            let xyz = V3d( V2d(random.NextDouble() * 2.0 - 1.0 , random.NextDouble() * 2.0 - 1.0 ).Normalized * cylinder.Radius, random.NextDouble() * cylinder.Height)

            let vx = circle.AxisU.Normalized
            let vy = circle.AxisV.Normalized
            let vz = cylinder.Axis.Direction.Normalized
            
            let p = cylinder.P0 + vx * xyz.X + vy * xyz.Y + vz * xyz.Z
            let p0 = axis.GetClosestPointOn p

            let n =(p - p0).Normalized

            let disp = (random.NextDouble() * 2.0 - 1.0) * jigger

            let p = p + n * disp

            Point(p, n, color)
        )


    let Sphere(numPoints : int)(color :C4b)(jigger : float)(sphere : Sphere3d) = 

        let random = Random()


        let points = Array.init numPoints (fun _ -> 
            
            let x = (random.NextDouble() * 2.0 - 1.0)
            let y = (random.NextDouble() * 2.0 - 1.0)
            let z = (random.NextDouble() * 2.0 - 1.0)

            let normal  = V3d(x, y, z).Normalized
            let disp    = (random.NextDouble() * 2.0 - 1.0) * jigger

            let pos     = sphere.Center + normal * (sphere.Radius + disp)

            Point(pos, normal, color)
            )
        points


    let Cone (numPoints : int)(color : C4b)(length : float)(jigger: float)(cone : Cone3d) = 
        
        let bounds = cone.BoundingCylinder3d (Range1d(0.0, length))

        let rndPoints   = bounds |> Cylinder numPoints color jigger 


        let random      = Random()
        
        ()
        let points  = rndPoints |> Array.map (fun point -> 
            
            let p   = point.Position
            
            let p0  = cone.GetAxis().GetClosestPointOn p
              
            let height = (p0 - cone.Origin).Length
            let radius = cone.GetRadius(height)

            
            let p = p0 + (p - p0).Normalized * radius

            
            let sgn = (p0 |> cone.GetAxis().GetT |> sign |> float)
            
            let tangent = V3d.Cross ((p0 - p).Normalized, cone.GetAxis().Direction.Normalized)
            
            let l = Line3d(cone.Origin, p)

            
            let n =  sgn * V3d.Cross(tangent, l.Direction.Normalized).Normalized

            let disp = (random.NextDouble() * 2.0 - 1.0) * jigger

            let p = p + n * disp

            Point(p, n, color)
            )              
        points


    let Torus (numPoints : int)(color : C4b) (jigger : float)(torus : Torus3d) = 

        let random = Random()
        
        Array.init numPoints (fun _ ->
            
            let pointOnAxis = torus.MajorCircle.GetPoint(random.NextDouble() * 2.0 * Math.PI)
            
            let tangent     = V3d.Cross (torus.MajorCircle.Normal , (pointOnAxis - torus.Position).Normalized)

            // Minor circle at this axis Position
            let circle  = Circle3d(pointOnAxis , tangent.Normalized, torus.MinorRadius )

            let pos   = circle.GetPoint(random.NextDouble() * 2.0 * Math.PI)

            let disp = (random.NextDouble() * 2.0 - 1.0) * jigger

            let normal = (pos - pointOnAxis).Normalized

            let pos = pos + normal * disp 
            Point(pos, normal, color)   
        )

    
    let Triangle (numPoints : int) (color : C4b) (jigger : float) (triangle : Triangle3d) = 
        
        let random = Random()

        Array.init numPoints (fun _ -> 
            
            let a, b =     
                let a = random.NextDouble()
                let b = random.NextDouble()
                if a + b > 1.0 then 
                    1.0 - a , 1.0 - b
                else
                    a,b
            

            let disp= (random.NextDouble() * 2.0 - 1.0) * jigger
            

            let pos =   triangle.P0 + triangle.Edge01 * a + triangle.Edge02 * b + triangle.Normal * disp

            
            //let disp    = (random.NextDouble() * 2.0 - 1.0) * jigger

           
            Point(pos, -triangle.Normal, color)
        )


    
    let Triangles (numPoints : int)(color : C4b) (jigger : float) (triangles : Triangle3d[]) = 
        let sumArea = triangles |> Array.sumBy(fun t -> t.Area)
        
        triangles   |> Array.map (fun triangle ->         
                                    let numPoints = int ((float numPoints) * (triangle.Area / sumArea))
                                    triangle |> Triangle numPoints color jigger
                                )
                    |> Array.concat



    let Plane (numPoints : int)(color : C4b)(jigger: float)(quad : Quad3d) = 

        [|
            Triangle3d(quad.P2 , quad.P1 , quad.P0)
            Triangle3d(quad.P3 , quad.P1 , quad.P2) 
        |] |> Triangles numPoints color jigger


    let Box (numPoints : int)(color : C4b)(jigger : float)(box : Box3d) = 
        
        box.GetFaces()  |> Array.map (fun quad -> [| Triangle3d(quad.P2 , quad.P1 , quad.P0); Triangle3d(quad.P3 , quad.P1 , quad.P2) |])
                        |> Array.concat
                        |> Triangles numPoints color jigger

