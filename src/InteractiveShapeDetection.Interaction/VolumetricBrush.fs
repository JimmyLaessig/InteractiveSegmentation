namespace InteractiveShapeDetection.Interaction

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open InteractiveShapeDetection.Base

    
    type VolumetricBrushSelection =
        {
            Positions   : V3d[]
            Radius      : float
        }

        static member Contains (point : V3d)(selection : VolumetricBrushSelection) = 
            
            selection.Positions |> Array.pairwise
                                |> Array.tryPick(fun (p0, p1) -> 
                                let line = Line3d(p0, p1)

                                if line.GetMinimalDistanceTo point <= selection.Radius then 
                                    Some point
                                else 
                                    None
                                )
                                |> Option.isSome 


    type VolumetricBrush =
        {
            Selection   : ModRef<VolumetricBrushSelection option>
            Radius      : IMod<float>
            Trajectory  : ModRef<(V2d * V3d) list>  
            Enabled     : IMod<bool>
        }



        static member init (enabled : IMod<bool>)(radius : IMod<float>)(view : IMod<CameraView>)(frustum : IMod<Frustum>)(cluster : IMod<Cluster option>)(mouse : IMouse) = 
            
            let addPoint (screenPosition : V2d)(worldPosition : V3d) (trajectory : (V2d * V3d) list) = 
                if(trajectory |> List.length = 0) then 
                    (screenPosition , worldPosition) ::trajectory
                else if V2d.Distance(trajectory.[0] |> fst , screenPosition) > 0.01 then 
                    (screenPosition , worldPosition)::trajectory
                else
                    trajectory
        
            
            let brush   = {Trajectory = Mod.init [] ; Selection = Mod.init None; Radius = radius; Enabled = enabled}

            

            let mouseMove ((oldPos : PixelPosition), (newPos : PixelPosition)) =
                
                    if (MouseButtons.Left |> mouse.IsDown |> Mod.force) then
                        
                        let enabled     = enabled   |> Mod.force
                        let cluster     = cluster   |> Mod.force
                        let viewTrafo   = view      |> Mod.force |> CameraView.viewTrafo
                        let projTrafo   = frustum   |> Mod.force |> Frustum.projTrafo
                        let ndc = newPos.ToNDC()
                        let pickRay = newPos |> Ray3d.PickRay viewTrafo projTrafo

                        match enabled, cluster with
                        | false, _       -> ()
                        | true , None    -> ()
                        | true , Some cluster -> 
                            let intersection = pickRay |> cluster.Intersect 
                            if intersection |> Option.isSome then 
                                let p = intersection.Value  
                                let contour = brush.Trajectory  |> Mod.force
                                                                |> addPoint ndc.XY p
                                transact(fun _ -> Mod.change brush.Trajectory contour )
             
                    else
                        ()


            let mouseDown (b: MouseButtons) = 
                if enabled |> Mod.force then 
                    
                    match b with
                    | MouseButtons.Left -> 
                        transact(fun _ -> Mod.change brush.Trajectory [])
                        transact(fun _ -> Mod.change brush.Selection None)                    
                    | _ -> ()



            let mouseUp (b : MouseButtons) =  
                if enabled |> Mod.force then 
                    match b with
                    | MouseButtons.Left -> 
                       

                        let contour = brush.Trajectory  |> Mod.force
                        let radius = brush.Radius       |> Mod.force
                        let selection =  
                            if contour |> List.length < 3 then 
                                None
                            else
                                Some {Positions = contour |> List.toArray |> Array.map snd; Radius = radius}
                    
                        transact(fun _ -> Mod.change brush.Selection selection) 
                        transact(fun _ -> Mod.change brush.Trajectory [])     
                    | _ -> ()
            

            mouse.Down.Values.Add mouseDown 
            mouse.Up.Values.Add mouseUp 
            mouse.Move.Values.Add mouseMove

            brush