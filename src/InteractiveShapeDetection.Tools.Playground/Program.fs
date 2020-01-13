namespace InteractiveShapeDetection.Tools.Playground

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
open Aardvark.VRVis
open Aardvark.SceneGraph.IO
open InteractiveShapeDetection.Base
open InteractiveShapeDetection.Visualization
open Assimp
open System.Runtime.Serialization


module App =
     

    [<EntryPoint>]
    let main argv = 
       
       //PtsCreator.main argv
       // PickingBenchmark.main argv
       // ShapeDetectionBenchmark.main argv
       // BenchmarkEvaluation.main argv
       ShapeDetectionTest.main argv
       //Test.main argv