namespace InteractiveShapeDetection.Visualization

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


open InteractiveShapeDetection
open InteractiveShapeDetection.Interaction
open InteractiveShapeDetection.Base
open FShade
open System.Diagnostics

module PrimitiveShapeVisualization =

    module Sg = 

        let ofOctree (framebufferSignature : IFramebufferSignature)(enabled : IMod<bool>)(octree : IMod<Octree>) =              
            
            let random = Random()
            let numEffects = 32
            let effects = Array.init numEffects (fun index -> 

                                            let color = C4f(random.NextDouble() |> float32, random.NextDouble()|> float32, random.NextDouble()|> float32, 0.8f)
                                            let color2 = color * 0.8
                                            
                                            let sg1 =  [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor color  |> toEffect]
                                            let sg2 =  [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor color2 |> toEffect]
                                                            
                                            (sg1, sg2)           
                                            )
            
            let sg  = 
                    adaptive{
                        let! enabled = enabled
                        if enabled then 
                            let! octree = octree
                        

                            let shapesSg = octree  |> Octree.flatten 
                                            |> Array.choose (fun (n,_,_) -> 
                                                                n.DetectedPrimitives.Value
                                                        )
                                            |> Array.concat
                                            |> Array.map (fun shape -> 
                                                            let effectSurface, effectLine = effects.[(int) (random.NextDouble() * (float) numEffects)]
                                                            let shapeSg = shape |> Sg.ofPrimitiveShape
                                                            let sg1 = shapeSg |> Sg.effect effectSurface 
                                                            let sg2 = shapeSg |> Sg.effect effectLine 
                                                                                |> Sg.fillMode (Mod.constant FillMode.Line)
                                                                            
                                                            [sg1 ; sg2 ] |> Sg.ofList
                                                            )

                            return shapesSg |> Sg.ofArray        
                        else 
                            return Sg.ofList []
                    }
            sg |> Sg.dynamic