namespace InteractiveShapeDetection.Visualization

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



module Surfaces = 

    let compile (framebufferSignature: IFramebufferSignature)(effects) = 
        let surface   = FShade.SequentialComposition.compose effects |> FShadeSurface
        framebufferSignature.Runtime.PrepareSurface(framebufferSignature,surface):> ISurface


    open FShade
    open Aardvark.SceneGraph.Semantics  
    open Aardvark.Base.Rendering.Effects
    [<AutoOpen>]
    module Shaders = 

        module Instanced = 
            
            type InstanceColor() = inherit SemanticAttribute("InstanceColor")


            type InstanceVertex = 
                { 
                    [<Position>] pos        : V4d 
                    [<Color>] col           : V4d
                    [<InstanceTrafo>] trafo : M44d
                    
                }


            let internal instancedTrafo (v : InstanceVertex) =
                vertex {
                     
                    return  {
                        wp  = v.trafo * v.pos
                        pos = uniform.ViewProjTrafo * v.trafo * v.pos
                        c   = v.col
                        n   = V3d.OOO
                        b   = V3d.OOO
                        t   = V3d.OOO   
                        tc  = V2d.OO
                    }
                }

        

        type ThickLineVertex = {
            [<Position>]                pos     : V4d
            [<Color>]                   c       : V4d
            [<Semantic("LineCoord")>]   lc      : V2d
            [<Semantic("Width")>]       w       : float
            }

       
        let internal thickLine (line : Line<ThickLineVertex>) =
            triangle {
                let t = uniform.LineWidth
                let sizeF = V3d(float uniform.ViewportSize.X, float uniform.ViewportSize.Y, 1.0)

                let pp0 = line.P0.pos
                let pp1 = line.P1.pos

                let p0 = pp0.XYZ / pp0.W
                let p1 = pp1.XYZ / pp1.W

                let fwp = (p1.XYZ - p0.XYZ) * sizeF

                let fw = V3d(fwp.XY * 2.0, 0.0) |> Vec.normalize
                let r = V3d(-fw.Y, fw.X, 0.0) / sizeF
                let d = fw / sizeF
                let p00 = p0 - r * t - d * t
                let p10 = p0 + r * t - d * t
                let p11 = p1 + r * t + d * t
                let p01 = p1 - r * t + d * t

                let rel = t / (Vec.length fwp)

                yield { line.P0 with pos = V4d(p00, 1.0); lc = V2d(-1.0, -rel); w = rel }
                yield { line.P0 with pos = V4d(p10, 1.0); lc = V2d( 1.0, -rel); w = rel }
                yield { line.P1 with pos = V4d(p01, 1.0); lc = V2d(-1.0, 1.0 + rel); w = rel }
                yield { line.P1 with pos = V4d(p11, 1.0); lc = V2d( 1.0, 1.0 + rel); w = rel }
            }  
               


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


        let internal sphereImposterGeometry (p : Point<Vertex>) =
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


        let internal sphereImposterFragment (v : Vertex) =
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

            
        let internal sphereImposterFragment' (v : Vertex) =
           fragment {
                let c = 2.0 * v.tc - V2d.II
                if c.Length > 1.0 then
                    discard()
                
                let z = sqrt (1.0 - c.LengthSquared)
                
                let n = V3d(c.XY,z)
                
                let vp = uniform.ViewTrafo * v.wp
                let vp = V4d(vp.X, vp.Y, vp.Z , 1.0)

                let sp = uniform.ProjTrafo *vp
                let sp = sp / sp.W

                return {color = v.c; depth = sp.Z * 0.5 + 0.5; normal = v.n ; wp = v.wp.XYZ / v.wp.W} 
            }
    
        
        let internal simpleLighting (v : Fragment) =
            fragment {
                let n = v.normal |> Vec.normalize
                let c = uniform.LightLocation - v.wp.XYZ |> Vec.normalize

                let ambient = 0.7
                let diffuse = Vec.dot c n |> abs

                return V4d(v.color.XYZ * (diffuse + ambient), v.color.W)
            }



    module Instanced = 
        
        let VertexColor (framebufferSignature: IFramebufferSignature)  = 
            [ Instanced.instancedTrafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect]  |> compile framebufferSignature

        
        let ConstantColor (framebufferSignature: IFramebufferSignature) (color :C4f)  = 
            [ Instanced.instancedTrafo |> toEffect; DefaultSurfaces.constantColor color |> toEffect ] |> compile framebufferSignature
           

        let UniformColor (framebufferSignature: IFramebufferSignature) (color :IMod<C4f>)  = 
            [ Instanced.instancedTrafo |> toEffect; DefaultSurfaces.uniformColor color |> toEffect ] |> compile framebufferSignature      
    

    let PointSpriteLighting(framebufferSignature : IFramebufferSignature) = 
        [DefaultSurfaces.trafo |> toEffect ; sphereImposterGeometry |> toEffect ; sphereImposterFragment  |> toEffect;  simpleLighting |> toEffect ] |> compile framebufferSignature
    
    let PointSpriteLighting'(framebufferSignature : IFramebufferSignature) = 
        [DefaultSurfaces.trafo |> toEffect ; sphereImposterGeometry |> toEffect ; sphereImposterFragment'  |> toEffect;  simpleLighting |> toEffect ] |> compile framebufferSignature


    let VertexColor (framebufferSignature: IFramebufferSignature)  = 
        [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect] |> compile framebufferSignature
        

    let ConstantColor (framebufferSignature: IFramebufferSignature) (color :C4f)  = 
        [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor color |> toEffect ] |> compile framebufferSignature



    let UniformColor (framebufferSignature: IFramebufferSignature) (color :IMod<C4f>)  = 
        [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.uniformColor color |> toEffect ] |> compile framebufferSignature

  

    let ThickLineSurface (framebufferSignature: IFramebufferSignature) (color :IMod<C4f>) = 
        [ DefaultSurfaces.trafo |> toEffect; thickLine |> toEffect; DefaultSurfaces.uniformColor color |> toEffect ] |> compile framebufferSignature

    
        

        