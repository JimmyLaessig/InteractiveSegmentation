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

open InteractiveShapeDetection.Base

module PointCloudVisualization =

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


        let targetPointDistance = Mod.init 25.0


        let pointCloudInfoSettings =
            {
                targetPointDistance     = targetPointDistance
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
   

    let Init (framebufferSignature)(view)(frustum)(windowSize)(keyboard: IKeyboard)(octree) = 
        
        
        keyboard.Down.Values.Add(fun key -> 
            match key with
            | Keys.T    -> 
                let value = DefaultSettings.pointCloudInfoSettings.targetPointDistance |> Mod.force
                let newValue = 
                    match value with
                    | 10.0  -> 25.0
                    | 25.0  -> 50.0
                    | _     -> 10.0
                transact(fun () -> Mod.change DefaultSettings.targetPointDistance newValue)

            | _         -> () 
        )
       

        let surface1 = (Surfaces.PointSpriteLighting framebufferSignature)
        let surface2 = (Surfaces.PointSpriteLighting' framebufferSignature)

        
        let surface = Mod.init surface1

        keyboard.Down.Values.Add(fun key -> 
                match key with
                | Keys.X -> 
                    let current = surface |> Mod.force
                    let newSurface = 
                        if current = surface1 then 
                            surface2
                        else
                            surface1
                    transact(fun () -> Mod.change surface newSurface)
                | _ -> ()
            )

        let lodSettings     = DefaultSettings.lodSettings
        let pointcloudInfo  = DefaultSettings.pointCloudInfoSettings

        let pointsetLodData = PointSetLodData( octree, lodSettings.NodeCount)
        

        Sg.pointCloud' pointsetLodData pointcloudInfo LodProgress.Progress.empty
            |> Sg.uniform "ViewportSize" windowSize
            |> Sg.uniform "PointSize" lodSettings.PointSize
            |> Sg.surface surface