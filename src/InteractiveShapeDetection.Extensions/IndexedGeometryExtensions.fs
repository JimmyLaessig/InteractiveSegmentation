[<AutoOpen>]
module IndexedGeometryExtensions

    open System
    open Aardvark.Base
    
    


    type IndexedGeometry with  

        static member ofCone3d (range : Range1d)(tesselation : int)(color : C4b)(cone : Cone3d) = 
            
            let length = range.Max - range.Min
            
            let direction = 
                if length < 0.0 then 
                    -cone.Direction
                else
                    cone.Direction

            let length = abs length 

            let radiusSmall = abs(cone.GetRadius range.Min)
            let radiusLarge = abs(cone.GetRadius range.Max)

            let origin = cone.Origin + cone.Direction * range.Min
            IndexedGeometryPrimitives.Cylinder.cylinderWithCol origin direction length  radiusSmall radiusLarge tesselation IndexedGeometryMode.TriangleList color
            
        


        static member ofBox3d ( box : Box3d ) : IndexedGeometry =
        
            let pa = [|
                V3f(box.Min.X, box.Min.Y, box.Min.Z);
                V3f(box.Max.X, box.Min.Y, box.Min.Z);
                V3f(box.Max.X, box.Max.Y, box.Min.Z);
                V3f(box.Min.X, box.Max.Y, box.Min.Z);
                V3f(box.Min.X, box.Min.Y, box.Max.Z);
                V3f(box.Max.X, box.Min.Y, box.Max.Z);
                V3f(box.Max.X, box.Max.Y, box.Max.Z);
                V3f(box.Min.X, box.Max.Y, box.Max.Z);
                |]

            let pos = [|
                    pa.[0]; pa.[1]; pa.[1]; pa.[2]; pa.[2]; pa.[3]; pa.[3]; pa.[0];
                    pa.[4]; pa.[5]; pa.[5]; pa.[6]; pa.[6]; pa.[7]; pa.[7]; pa.[4];
                    pa.[0]; pa.[4]; pa.[1]; pa.[5]; pa.[2]; pa.[6]; pa.[3]; pa.[7];
                    |]
        
            let attrs = [
                            (DefaultSemantic.Positions, pos :> Array)
                        ] |> SymDict.ofList
            
        
            IndexedGeometry(
                        Mode = IndexedGeometryMode.LineList,
                        IndexedAttributes = attrs
                    )



        static member ofQuad3d (quad : Quad3d) = 

            let pos = [|quad.P0; quad.P1; quad.P2; quad.P3|]
            let attrs = 
                [
                (DefaultSemantic.Positions, pos :> Array)
                ] |> SymDict.ofList
            
            
            IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleStrip,                    
                    IndexedAttributes = attrs
                )




        static member solidBox3d (color : C4f) ( box : Box3d ): IndexedGeometry =
            
            let pos, normals, colors = box.GetFaces() |> Array.map (fun quad -> 
                                                    [|  
                                                        quad.P0, quad.Normal, color 
                                                        quad.P1, quad.Normal, color 
                                                        quad.P2, quad.Normal, color 
                                                        quad.P2, quad.Normal, color 
                                                        quad.P1, quad.Normal, color 
                                                        quad.P3, quad.Normal, color
                                                    |])
                                    |> Array.concat
                                    |> Array.unzip3
            
            let pos      = pos      |> Array.map V3f
            let normals = normals   |> Array.map V3f
            let colors  = colors    |> Array.map C4b

            let attrs = [
                            (DefaultSemantic.Positions, pos :> Array)
                            (DefaultSemantic.Normals, normals :> Array)
                            (DefaultSemantic.Colors, colors :> Array)
                        ] |> SymDict.ofList
        
            IndexedGeometry(
                        Mode = IndexedGeometryMode.TriangleList,
                        IndexedAttributes = attrs
                    )