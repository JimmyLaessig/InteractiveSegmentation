[<AutoOpen>]
module Box3dExtensions
    
    open Aardvark.Base

    type Box3d with
        member x.UnitBoxTrafo = 
            let shift       = Trafo3d.Translation (x.Center)
            let bias        = Trafo3d.Translation(-V3d.III * 0.5)
            let scale       = Trafo3d.Scale (x.SizeX, x.SizeY, x.SizeZ)
            bias * scale * shift 
        


        member x.Lines = 
            
            //left side (x = 0)
            //000 001   
            //001 011
            //011 010
            //010 000
            let l00 = Line3d(x.Corner(0),x.Corner(1))
            let l01 = Line3d(x.Corner(1),x.Corner(3))
            let l02 = Line3d(x.Corner(3),x.Corner(2))
            let l03 = Line3d(x.Corner(2),x.Corner(0))

            // Right side( x = 1)
            // 100 101
            // 101 111
            // 111 110
            // 110 100
            let l04 = Line3d(x.Corner(4),x.Corner(5))
            let l05 = Line3d(x.Corner(5),x.Corner(7))
            let l06 = Line3d(x.Corner(7),x.Corner(6))
            let l07 = Line3d(x.Corner(6),x.Corner(4))

            // Connection Lines 
            // 000 100
            // 001 101
            // 010 110
            // 011 111            
            let l08 = Line3d(x.Corner(0),x.Corner(4))
            let l09 = Line3d(x.Corner(1),x.Corner(5))
            let l10 = Line3d(x.Corner(2),x.Corner(6))
            let l11 = Line3d(x.Corner(3),x.Corner(7))

            [|l00; l01; l02; l03; l04; l05; l06; l07; l08; l09; l10; l11|]


            
