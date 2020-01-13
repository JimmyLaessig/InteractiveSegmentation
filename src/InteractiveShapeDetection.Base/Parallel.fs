namespace InteractiveShapeDetection.Base


open System




module Parallel = 

    let map (map : 'T1 -> 'T2)(t : 'T1[])  = 

        let result = Array.zeroCreate t.Length

        let parallelTask (i : int) = 
            let ti = t.[i]
            result.[i] <- map ti
        
        System.Threading.Tasks.Parallel.For(0, t |> Array.length, Action<int>(parallelTask)) |> ignore

        result

