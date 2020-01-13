[<AutoOpen>]
module OptionExtensions
    
    open Aardvark.Base

    module Option = 
        
        let toArrayOfT(map : ('a -> 'T[]))(o: 'a option) = 
            match o with
            | None -> [||]
            | Some v -> map v


        let map2 (projection : ('T1 -> 'T2 -> 'T3)) (o1 : 'T1 option) (o2 : 'T2 option) = 
            match o1, o2 with
            | None , _ -> None
            | _ , None -> None
            | Some v1, Some v2 -> Some (projection v1 v2)