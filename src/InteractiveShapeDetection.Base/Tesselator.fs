namespace InteractiveShapeDetection.Base


module Tesselator = 

    open Aardvark.Base
    open Aardvark.VRVis

    open System.Linq  
    open LibTessDotNet.Double



    let tesselateLibTess (poly : Polygon2d) =
        

        if poly.PointCount < 3 then 
            ([||], [||])
        else 
            let contourVertices = poly.Points.ToArray() |> Array.map (fun p -> 
                                                                        let mutable vertex = ContourVertex()
                                                                        vertex.Position <- Vec3(X = p.X, Y = p.Y)                                                                           
                                                                        vertex
                                                                    )            
                                 
            let triangleTess = Tess()
            triangleTess.AddContour(contourVertices, ContourOrientation.Clockwise)
            triangleTess.Tessellate (WindingRule.EvenOdd, ElementType.Polygons, 3)
        

            let indices     = triangleTess.Elements
            let positions   = triangleTess.Vertices |> Array.map (fun v -> V2d(v.Position.X, v.Position.Y))
        

            let triangles = Array.zeroCreate (indices.Length / 3)
            for ti in 0 .. triangles.Length-1 do
                    let i0 = indices.[3 * ti + 0]
                    let i1 = indices.[3 * ti + 1]
                    let i2 = indices.[3 * ti + 2]
                    triangles.[ti] <- Triangle2d(positions.[i0], positions.[i1], positions.[i2])

            let contourTess = Tess()
            
            contourTess.AddContour (contourVertices, ContourOrientation.Clockwise)
            contourTess.Tessellate(WindingRule.EvenOdd, ElementType.BoundaryContours, 2);
            

            let contour = Array.zeroCreate (contourTess.ElementCount)
            
            for i in 0 .. contourTess.ElementCount-1 do 
                let index = contourTess.Elements.[i * 2]
                let count = contourTess.Elements.[i* 2 + 1]
                let positions = Array.sub contourTess.Vertices index count
                                    |> Array.map (fun v -> V2d(v.Position.X, v.Position.Y)) 
                
                let polygon = Polygon2d(Array.append positions [|positions.[0]|])

                if polygon.ComputeWindingNumber() <= 0 then 
                    contour.[i] <- polygon.Reversed                      
                else        
                    contour.[i] <- polygon
            

            (triangles , contour)    
