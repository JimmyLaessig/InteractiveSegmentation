namespace InteractiveShapeDetection.Tools.Playground

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
open System.IO
open Aardvark.Database
open InteractiveShapeDetection.Base


module PickingBenchmark =
 


    let main argv = 

        let file = File.readAllLines("C:/Users/brainer/desktop/lasso_benchmark.csv")
        
        let data = file |> Array.map (fun line -> 
                        let line = line.Split (';')
                        Double.Parse(line.[0]) |> int ,  bool.Parse line.[1], Double.Parse(line.[2])
                        )
        //data |> Array.iter(fun (a,b) -> printfn "%b : %f" a b)
        
       
        
        let dataTrue = data     |> Array.filter (fun (id, a,b) -> a = true ) 
                                |> Array.map (fun (id, a,b) -> b)
        let dataFalse = data    |> Array.filter (fun (id, a,b) -> a = false )
                                |> Array.map (fun (id, a,b) -> b)


        let avgTrue = dataTrue |> Array.average
        let minTrue = dataTrue |> Array.min
        let maxTrue = dataTrue |> Array.max

        let avgFalse = dataFalse |> Array.average
        let minFalse = dataFalse |> Array.min
        let maxFalse = dataFalse |> Array.max


        printfn "True: %f ; %f ; %f" minTrue maxTrue avgTrue
        printfn "False: %f ; %f ; %f" minFalse maxFalse avgFalse


        let ratioPerLasso = data |> Array.groupBy (fun (id, _, _) -> id)
                             |> Array.map (fun (_, data) -> 
                                let (_,_,valueTrue)     = data  |> Array.find(fun (a,b,c) -> b = true)
                                let (_,_,valueFalse)    = data  |> Array.find(fun (a,b,c) -> b = false)
                                (float valueTrue) / (float valueFalse)
                                )

        let avgRatio = ratioPerLasso |> Array.average

        printfn "Ration between computation time: %f " avgRatio

        0