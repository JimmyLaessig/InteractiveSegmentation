namespace InteractiveShapeDetection

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
open InteractiveShapeDetection.Base


module Global = 
    
    let mutable cameraView  : IMod<CameraView>  = null
    let mutable frustum     : IMod<Frustum>     = null
    let mutable windowSizes : IMod<V2i>         = null
    let mutable viewTrafo   : IMod<Trafo3d>     = null
    let mutable projTrafo   : IMod<Trafo3d>     = null 
    let mutable octree      : ModRef<Octree>    = Unchecked.defaultof<ModRef<Octree>> 
    let mutable pointcloudInfo : PointCloudInfo = Unchecked.defaultof<PointCloudInfo> 

    let mutable OctreeManipulationApplicator : SequentialComputationApplicator<Octree> = null

    let mutable sceneDepthTexture : IMod<ITexture>  = null
    let mutable sceneColorTexture : IMod<ITexture>  = null


    let mutable Window  : SimpleRenderWindow        = Unchecked.defaultof<SimpleRenderWindow>
    let mutable Runtime : IRuntime                  = Unchecked.defaultof<IRuntime>
    

  


module Input = 
   
    let mutable mouse : IMouse              = Unchecked.defaultof<IMouse>
    let mutable keyboard : IKeyboard        = Unchecked.defaultof<IKeyboard>
    

module RenderPasses = 
    let mutable geometry    = RenderPass.main
    let mutable overlay     = geometry  |> RenderPass.after "" RenderPassOrder.Arbitrary
    let mutable UIBack      = overlay   |> RenderPass.after "" RenderPassOrder.Arbitrary
    let mutable UIFront     = UIBack    |> RenderPass.after "" RenderPassOrder.Arbitrary