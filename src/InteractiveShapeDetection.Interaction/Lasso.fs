namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open InteractiveShapeDetection.Base


    
    type LassoSelection =
        {
            View        : CameraView
            Frustum     : Frustum
            Contour     : V2d[]
            Triangles   : Triangle2d[]
            
        }

        static member Contains (point : V2d)(selection : LassoSelection) = 

            Array.TrueForAll(selection.Triangles, fun triangle -> not (triangle.Contains point)) |> not
               


    type Lasso =
        {
            Contour     : ModRef<V2d list>
            Selection   : ModRef<LassoSelection option>
            Enabled     : ModRef<bool>      
        }


        static member init (enabled : IMod<bool>)(view : IMod<CameraView>)(frustum : IMod<Frustum>)(mouse : IMouse) = 
            

            let addPoint (point : V2d) (contour : V2d list) = 
                if(contour |> List.length = 0) then 
                    point ::contour
                else if (contour.[0] - point).Length > 0.005 then 
                    point ::contour
                else
                    contour
        
            
            let lasso   = {Contour = Mod.init [] ; Selection = Mod.init None; Enabled = Mod.init true}


            let mouseMove ((oldPos : PixelPosition), (newPos : PixelPosition)) =
                if enabled |> Mod.force then 
                    if (MouseButtons.Left |> mouse.IsDown |> Mod.force) then
                        
                        let ndc = newPos.ToNDC() 
                        let contour = lasso.Contour |> Mod.force
                                                    |> addPoint ndc.XY
                
                        transact(fun _ -> Mod.change lasso.Contour contour )
                    else
                        ()


            let mouseDown (b: MouseButtons) = 
                if enabled |> Mod.force then 
                    
                    match b with
                    | MouseButtons.Left -> 
                        transact(fun _ -> Mod.change lasso.Contour [])
                        transact(fun _ -> Mod.change lasso.Selection None)                    
                    | _ -> ()



            let mouseUp (b : MouseButtons) =  
                if enabled |> Mod.force then 
                    match b with
                    | MouseButtons.Left -> 
                       

                        let contour = lasso.Contour |> Mod.force

                        let selection =  
                            if contour |> List.length < 3 then 
                                None
                            else
                                let contour =  List.append contour [contour.[0]]
                                let triangles, _ = Tesselator.tesselateLibTess (Polygon2d(contour))
                                Some {View = view |> Mod.force ; Frustum = frustum |> Mod.force ; Contour = contour |> List.toArray; Triangles = triangles}

                    
                        transact(fun _ -> Mod.change lasso.Selection selection) 
                        transact(fun _ -> Mod.change lasso.Contour [])     
                    | _ -> ()
            

            mouse.Down.Values.Add mouseDown 
            mouse.Up.Values.Add mouseUp 
            mouse.Move.Values.Add mouseMove


            lasso

