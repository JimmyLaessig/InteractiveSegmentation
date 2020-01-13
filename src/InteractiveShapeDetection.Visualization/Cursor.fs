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
open InteractiveShapeDetection.Base

module Cursor =     

    
    let p = 
        [|
        V3d(0.0 , 0.0, 0.0); 
        V3d(1.0 , 0.0, 0.0); 
        V3d(0.0 , 1.0, 0.0); 
        V3d(1.0 , 1.0, 0.0)
        |] 

        |> Array.map (fun p -> p - V3d (0.5, 0.5, 0.0))
    
    
    let private cursorIg = IndexedGeometry (
                            Mode                = IndexedGeometryMode.TriangleStrip, 
                            IndexedAttributes   = SymDict.ofList [DefaultSemantic.Positions, p :> Array])


    let Init (signature : IFramebufferSignature)(viewTrafo: IMod<Trafo3d>)(projTrafo : IMod<Trafo3d>)(color : IMod<C4f>)(renderPass : RenderPass) (sizeInPixels : IMod<V2i>)(pixelPosition : IMod<PixelPosition>)(snappedPoint : IMod<Point option>)= 
        
        //0.4 0.2 0.4
        
        let margin  = 0.3


        let trafos = 
            adaptive {
                
                

               

                let! view = viewTrafo
                let! proj = projTrafo
                
                let! ps             = sizeInPixels
                let! pp             = pixelPosition
                let! pointOption    = snappedPoint

                let margin = 
                    match pointOption with
                    | None      -> margin
                    | Some _    -> max (margin - 0.1) 0.0
                
                let width   = (1.0 - margin) / 2.0
                let height  = 1.0 / 7.5

                let windowSize  = pp.Bounds.Size2d + 1.0

                let S1 = Trafo3d.Scale(width , height , 1.0)
                let T1 = Trafo3d.Translation( (width + margin) * 0.5 , 0.0, 0.0)
                let T2 = Trafo3d.Translation(-(width + margin) * 0.5 , 0.0, 0.0)

                let S2 = Trafo3d.Scale(height, width , 1.0)
                let T3 = Trafo3d.Translation( 0.0 , (width + margin) * 0.5 , 0.0)
                let T4 = Trafo3d.Translation( 0.0 ,-(width + margin) * 0.5 , 0.0)
                
                let S = Trafo3d.Scale(2.0 * (float ps.X) / windowSize.X, 2.0 * (float ps.Y) / windowSize.Y, 1.0)                                                               
                let T = 
                    match pointOption with
                    | None          -> Trafo3d.Translation(pp.ToNDC()) 
                    | Some point    -> let ndc = (view * proj).Forward.TransformPosProj (point.Position)
                                       Trafo3d.Translation(V3d(ndc.XY, -1.0));     
                                        
                return 
                    [|
                        S1 * T1 * S * T; 
                        S1 * T2 * S * T; 
                        S2 * T3 * S * T; 
                        S2 * T4 * S * T
                    |]
            }
        
        cursorIg    |> Sg.instancedGeometry trafos
                    |> Sg.surface (color |> Surfaces.Instanced.UniformColor signature |> Mod.constant)     
                    
                    |> Sg.pass renderPass