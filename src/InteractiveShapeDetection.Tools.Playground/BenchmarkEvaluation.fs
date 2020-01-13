namespace InteractiveShapeDetection.Tools.Playground

open System
open System.Linq
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


module BenchmarkEvaluation =
 

    open Aardvark.Base.CameraView
    open Aardvark.Base.Frustum
    open Aardvark.SceneGraph.AirState


   
    let main argv = 

//        Ag.initialize()
//        Aardvark.Init()
        

//        MBrace.FsPickler.FsPickler.DeclareSerializable<Aardvark.VRVis.PointRkdTreeDData>()
//        
//        MBrace.FsPickler.FsPickler.DeclareSerializable<Plane3d>()
//        MBrace.FsPickler.FsPickler.DeclareSerializable<Sphere3d>()
//        MBrace.FsPickler.FsPickler.DeclareSerializable<Cone3d>()
//        MBrace.FsPickler.FsPickler.DeclareSerializable<Cylinder3d>()
//        MBrace.FsPickler.FsPickler.DeclareSerializable<Torus3d>()
//        MBrace.FsPickler.FsPickler.DeclareSerializable<InteractiveShapeDetection.Base.PrimitiveShape>()
//
//        let psp         = @"C:\Cache\Technologiezentrum_Teil1.pts"
//        let storagePath = @"C:\Cache\vgmCache_technologiezentrum.CacheBenchmark"
//
//                          
//        let psp         = @"C:\Cache\JBs_Haus.pts"
//        let storagePath = @"C:\Cache\JBs_Haus.pts.CacheBenchmark"
////
////
//        let psp         = @"D:\Cache\primitiveScene.pts"
//        let storagePath = @"D:\Cache\primitiveScene.pts.CacheBenchmark"
//
//
//        let psp         = @"D:\Cache\dragon.pts"
//        let storagePath = @"D:\Cache\vgmCache_dragon"


//        let psp         = @"D:\Cache\kirche.pts"
//        let storagePath = @"D:\Cache\vgmCache3"
        
        
//        let s       = Directory.CreateDirectory(storagePath)
//        let mem     = Memory.mapped (Path.combine [storagePath;"memory.mapped"]) //Memory.hglobal 0L //Memory.mapped (Path.combine [storagePath;"memory.mapped"])
//        let get i   = NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i]) //Memory.hglobal 0L //NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i])
//        let store   = new BlobStore(mem,get)
//        use db      = new Database(store)     
//        let guid = Guid("6f3fd114-f345-4e2d-b82c-2e7172ea6086")
//       
//        
//
//        let octree    = Octree.ofPointset psp guid db 
//         
//        let nodes       = octree    |> Octree.flatten
//        let numNodes    = nodes     |> Array.length
//        let maxLoD      = nodes     |> Array.map(fun (a,b,c) -> c |> Array.length) |> Array.max
        
        
        let evaluate path = 
            
            // Array that looks like this: 
            // [|1; 2; 3]
            let toArrayOfInt (s:string) = 
                // Eliminate non numerial chars from string
                let x = s.Replace("[|", "").Replace("|]", "").Replace(";", " ").Split(' ')
                
                x |> Array.choose(fun s -> 
                    let mutable value = 0
                    if s.Length <= 0 then 
                        None
                    else 
                        Int32.Parse(s) |> Some
                    )


            let benchmarks = File.ReadAllLines("C:/Users/brainer/desktop/benchmarks/" + path)
            let benchmarks = benchmarks  |> Array.map (fun s -> 
                                                            let x = s.Split(']')
     
                                                            let path        = x.[0] + "]"
                                                            let path        = path |> toArrayOfInt

                                                            let y           = x.[1].Split(';') 
                                                            let numPoints   = System.Int32.Parse(y.[1])
                                                            let numShapes   = System.Int32.Parse(y.[2])
                                                            let time        = System.Double.Parse(y.[3])
                                                            (path, numPoints, numShapes, time)
                                                        )
        

            let averageNode (values : (int[] * int * int * float)[]) =
                let (path, _, _, _) = (values.[0])

                let avgNodes    = values |> Array.averageBy(fun (s, a, b, c) -> float a) |> int
                let avgShapes   = values |> Array.averageBy(fun (s, a, b, c) -> float b)
                let avgTime     = values |> Array.averageBy(fun (s, a, b, c) -> c) 
          
                (path, avgNodes, avgShapes, avgTime)
            

            let groupedByNode   = benchmarks    |> Array.groupBy (fun (path,b,c,d) -> path)
            let avgByNode       = groupedByNode |> Array.map (fun (key,values) -> averageNode values)
        
            let (minPoints, maxPoints)  = (benchmarks |> Array.map(fun (s, a, b, c) -> a)|> Array.min, benchmarks |> Array.map(fun (s, a, b, c) -> a) |> Array.max)
            let (minShapes, maxShapes)  = (benchmarks |> Array.map(fun (s, a, b, c) -> b)|> Array.min, benchmarks |> Array.map(fun (s, a, b, c) -> b) |> Array.max)
            let (minTime, maxTime)      = (benchmarks |> Array.map(fun (s, a, b, c) -> c)|> Array.min, benchmarks |> Array.map(fun (s, a, b, c) -> c) |> Array.max)
        
            let (_, avgPoints, avgShapes, avgTime)        = averageNode benchmarks

            
            let groupedByLoD = avgByNode    |> Array.groupBy(fun (path,a,b,c) -> path |> Array.length)
//                                        |> Array.map (fun (key, v) -> int key , (averageNode v))
//                                      |> Array.sortBy(fun (key,_) -> key)

            let numShapesPerLod = groupedByLoD  |> Array.map (fun (key, values) -> 
                                                                let numShapes = values |> Array.sumBy(fun (s,a,b,c) -> b )
                                                                key, numShapes / (values |> Array.length |> float)
                                                                )  
                                                |> Array.sortBy(fun (key,_) -> key)
                      
//            let series1 = avgByLoD |> Array.map (fun (key, (s,a,b,c)) -> [|key; b|])
//            let series2 = avgByLoD |> Array.map (fun (key, (s,a,b,c)) -> [|key; c|])
////
            let x = sprintf "%A" numShapesPerLod
            let x = x.Replace("|", "").Replace(";", ",").Replace("\n", "")
            
            //let y = sprintf "%A" series2
            //let y = y.Replace("|", "").Replace(";", ",").Replace("\n", "")
            x
            //x + "\n"+ y
            //sprintf "%s ; %i ; %i ; %i ; %i ; %i ; %f ; %f ; %f ; %f ;" path minPoints maxPoints avgPoints minShapes maxShapes (avgShapes) minTime maxTime avgTime    


        let s1 = evaluate "benchmark_JB_Haus_PlanesOnly.txt"
        let s2 = evaluate "benchmark_JB_Haus_AllShapes.txt"
        let s3 = evaluate "benchmark_technologiezentrum_PlanesOnly.txt"
        let s4 = evaluate "benchmark_technologiezentrum_AllShapes.txt"
        let s5 = evaluate "benchmark_syntheticScene_PlanesOnly.txt"
        let s6 = evaluate "benchmark_syntheticScene_AllShapes.txt"


         //  let header = ";minPoints ; maxPoints ; avgPoints ; minShapes ; maxShapes ; avgShapes ; minTime ; maxTime ; avgTime"
        


        File.WriteAllLines ( "C:/users/brainer/desktop/numShapes_vs_LoD.txt", [|s1; s2; s3; s4; s5; s6|])
        0