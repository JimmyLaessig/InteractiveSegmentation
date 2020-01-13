namespace InteractiveShapeDetection.Base

open Aardvark.Base
open Aardvark.Base.Incremental

[<AutoOpen>]
module IModExtensions = 

    module Mod  = 
        let unzip (m : IMod<'a * 'b>)=  m |> Mod.map fst , m |> Mod.map snd

        let zip m1 m2 = Mod.map2 (fun a b -> a,b) m1 m2


        let unzip4(m : IMod<'a * 'b * 'c * 'd>) = 
            m |> Mod.map (fun (a,b,c,d) -> a), 
            m |> Mod.map (fun (a,b,c,d) -> b),
            m |> Mod.map (fun (a,b,c,d) -> c), 
            m |> Mod.map (fun (a,b,c,d) -> d)


        let unzip5(m : IMod<'a * 'b * 'c * 'd * 'e>) = 
            m |> Mod.map (fun (a,b,c,d,e) -> a), 
            m |> Mod.map (fun (a,b,c,d,e) -> b),
            m |> Mod.map (fun (a,b,c,d,e) -> c), 
            m |> Mod.map (fun (a,b,c,d,e) -> d),
            m |> Mod.map (fun (a,b,c,d,e) -> e)

        

        let inline differentiate (m : IMod< ^T>)=
            let lastValue = ref None

            adaptive {
                let! newValue = m
                match !lastValue with
                    | Some last ->
                        let d = newValue - last
                        lastValue := Some newValue
                        return last, Some d
                    | None ->
                        lastValue := Some newValue
                        return newValue, None
            }