[<AutoOpen>]
module RenderTaskExtensions

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    open Aardvark.Rendering.NanoVg

    
    let private getResult (sem : Symbol) (t : IOutputMod<IFramebuffer>) =
            t.GetOutputTexture sem


    let private renderSemantics (sem : Set<Symbol>) (size : IMod<V2i>) (clearColor : C4f) (clearDepth : double)(task : IRenderTask) =
        let runtime     = task.Runtime.Value
        let signature   = task.FramebufferSignature.Value

        let clear       = runtime.CompileClear(signature, Mod.constant clearColor, Mod.constant clearDepth)
        let fbo         = runtime.CreateFramebuffer(signature, sem, size)

        let res = 
            new RenderTask.SequentialRenderTask([|clear; task|]) |> RenderTask.renderTo fbo
            
        sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq

    
    type RenderTask with
        
        static member renderToColorAndDepth' (size : IMod<V2i>) (clearColor : C4f) (clearDepth : double) (task : IRenderTask) =

            let map = task |> renderSemantics (Set.ofList [DefaultSemantic.Depth; DefaultSemantic.Colors]) size clearColor clearDepth
            (Map.find DefaultSemantic.Colors map, Map.find DefaultSemantic.Depth map)






        static member renderToColor' (size : IMod<V2i>) (clearColor : C4f) (task : IRenderTask) =

            let runtime     = task.Runtime.Value
            let signature   = task.FramebufferSignature.Value
            let clear       = runtime.CompileClear(signature, Mod.constant clearColor)
            let fbo         = runtime.CreateFramebuffer(signature, Set.ofList[DefaultSemantic.Colors], size)

            let result = new RenderTask.SequentialRenderTask([|clear;task|]) 
                            |> RenderTask.renderTo fbo
                
            result.GetOutputTexture DefaultSemantic.Colors    

            
