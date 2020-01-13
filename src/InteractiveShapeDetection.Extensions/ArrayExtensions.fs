[<AutoOpen>]
module ArrayExtensions
   

    module Array = 
        
        let unzip4 (array : ('T1 * 'T2 * 'T3 * 'T4)[]) =

            let mutable t1 = []
            let mutable t2 = []
            let mutable t3 = []
            let mutable t4 = []
            array |> Array.iteri(fun i (a,b,c,d)-> 
                                            t1 <- a :: t1 
                                            t2 <- b :: t2
                                            t3 <- c :: t3
                                            t4 <- d ::t4
                                    )
            let t1 = t1 |> List.rev|> List.toArray
            let t2 = t2 |> List.rev|> List.toArray
            let t3 = t3 |> List.rev|> List.toArray
            let t4 = t4 |> List.rev|> List.toArray

            (t1, t2, t3, t4)


        let tryMinBy (projection : 'T -> 'U when 'U : comparison) (array : 'T[]) = 
            match array with
            | [||]  -> None
            | _     -> array |> Array.minBy projection |> Some