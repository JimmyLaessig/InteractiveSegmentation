namespace InteractiveShapeDetection.Visualization

open System
open Aardvark.Base


open Aardvark.Base.Incremental




open InteractiveShapeDetection.Base

module PointCloudVisualization2 =

    open System.Collections.Concurrent


    type LodUserSettings =
        {
            NodeCount           : IModRef<float>
            PointSize           : IModRef<float>
        }


    module DefaultSettings = 

        let lodSettings =
            {   
                NodeCount = Mod.init 150.0
                PointSize = Mod.init 0.075
            }


        let pointCloudInfoSettings =
            {
                targetPointDistance     = Mod.init 25.0
                maxReuseRatio           = 0.5
                minReuseCount           = 1L <<< 20
                pruneInterval           = 500
                customView              = None
                customProjection        = None
                attributeTypes =
                    Map.ofList [
                        DefaultSemantic.Positions, typeof<V3f>
                        DefaultSemantic.Colors, typeof<C4b>
                        DefaultSemantic.Normals, typeof<V3f>
                        DefaultSemantic.DiffuseColorCoordinates, typeof<V2f>
                        Sym.ofString "SelectionFlags", typeof<bool>
                    ]
                boundingBoxSurface      = None //Some Effects.ConstantRed
            }
   
    

    module Effects = 

        open FShade
        open Aardvark.Base.Rendering.Effects
        open Aardvark.Base.Rendering

        type IsSelectedAttribute() = inherit SemanticAttribute("IsSelected")
               

        type Fragment =
            {
                [<Color>]
                color : V4d

                [<Depth>]
                depth : float

                [<Normal>]
                normal : V3d

               // [<WorldPosition>]
                wp : V3d
            }

        let sphereImposterGeometry (p : Point<Vertex>) =
            triangle {
                let ratio = V2d uniform.ViewportSize

                //let s   = uniform.PointSize * V2d(ratio.Y / ratio.X, 1.0) * 0.5
            
                let s   = uniform.PointSize * 0.5
            

                let pos = p.Value.pos
                let wp  = p.Value.wp
                let vp  = uniform.ViewTrafo * wp

                let pxyz = vp.XYZ / vp.W

            
                let vp00 = vp + V4d( -s, -s, 0.0, 0.0 )
                let vp01 = vp + V4d( -s,  s, 0.0, 0.0 )
                let vp10 = vp + V4d(  s, -s, 0.0, 0.0 )
                let vp11 = vp + V4d(  s,  s, 0.0, 0.0 )


                let wp00 = uniform.ViewTrafoInv * vp00 
                let wp01 = uniform.ViewTrafoInv * vp01 
                let wp10 = uniform.ViewTrafoInv * vp10 
                let wp11 = uniform.ViewTrafoInv * vp11 


                let p00 = uniform.ProjTrafo * vp00
                let p01 = uniform.ProjTrafo * vp01
                let p10 = uniform.ProjTrafo * vp10
                let p11 = uniform.ProjTrafo * vp11


                // Apply selected color
                let highlightColor = V4d(2, 0, 0, 1)
                //let color = if p.Value.c.W = 0.0 then highlightColor else p.Value.c
                let color = if (p.Value.tc.X = 1.0) then highlightColor else p.Value.c

                yield { p.Value with c = color; wp = wp00 / wp00.W; pos = p00 / p00.W; tc = V2d.OO }
                yield { p.Value with c = color; wp = wp01 / wp01.W; pos = p01 / p01.W; tc = V2d.OI }
                yield { p.Value with c = color; wp = wp10 / wp10.W; pos = p10 / p10.W; tc = V2d.IO }
                yield { p.Value with c = color; wp = wp11 / wp11.W; pos = p11 / p11.W; tc = V2d.II }
            }       


        let sphereImposterFragment (v : Vertex) =
           fragment {
                let c = 2.0 * v.tc - V2d.II
                if c.Length > 1.0 then
                    discard()

                let z = sqrt (1.0 - c.LengthSquared)
                
                let n = V3d(c.XY,z)
                let d = z * uniform.PointSize * 0.5
                
                let vp = uniform.ViewTrafo * v.wp
                let vp = V4d(vp.X, vp.Y, vp.Z + d, 1.0)

                let sp = uniform.ProjTrafo *vp
                let sp = sp / sp.W
   

                return {color = v.c; depth = sp.Z * 0.5 + 0.5; normal = v.n ; wp = v.wp.XYZ / v.wp.W} 
            }
    
        
//        let lighting (f : Fragment) =
//            fragment {
//                let c = f.color
//                let N = f.normal
//                let V = uniform.CameraLocation - f.wp
//                let L = -V3d.IIO
//                let H = (V + L).Normalized
//                
//                
//                let ambient = 0.3
//                let diffuse = Vec.dot L N |> abs
//                let specular = pown (max 0.0 (V3d.Dot(N,H))) 32
//
//                let c = (ambient + diffuse + specular) * c.XYZ
//                   
//
//                return V4d(c, f.color.W)
//            }

        let vertexColor (f : Fragment) =
            fragment {
                return f.color
                //let c = V4d(abs(f.normal.X), abs(f.normal.Y), abs(f.normal.Z), 1.0)
                //return c
            }
    
        
        let defaultEffect = 
            [
                DefaultSurfaces.trafo   |> toEffect
                sphereImposterGeometry  |> toEffect
                sphereImposterFragment  |> toEffect
                //vertexColor             |> toEffect
                (DefaultSurfaces.simpleLighting )  |> toEffect
            ]



    let Init (framebufferSignature) (view)(frustum)(windowSize)(octree) = 
        
        let lodSettings     = DefaultSettings.lodSettings
        let pointcloudInfo  = DefaultSettings.pointCloudInfoSettings

        let pointsetLodData = PointSetLodData( octree, lodSettings.NodeCount)
        

        Sg.pointCloud' pointsetLodData pointcloudInfo LodProgress.Progress.empty
            |> Sg.uniform "ViewportSize" windowSize
            |> Sg.uniform "PointSize" lodSettings.PointSize
            |> Sg.effect Effects.defaultEffect






    




