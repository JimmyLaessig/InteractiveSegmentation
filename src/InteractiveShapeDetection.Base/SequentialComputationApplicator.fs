namespace InteractiveShapeDetection.Base


open System.Diagnostics
open System.IO
open System
open System.Threading
open System.Threading.Tasks
open Aardvark.Database
open Aardvark.Git
open Aardvark.Git.Operators
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering


open Aardvark.VRVis

open Aardvark.Base.Native
open System.Collections.Concurrent
open InteractiveShapeDetection.Base

type Computation<'T> = 
    {
        Computation     : 'T -> 'T
        Timeout         : int
        TimeoutCallback : unit -> unit
    }

[<AllowNullLiteral>]
type SequentialComputationApplicator<'T> = 

    val mutable private  maxPriority        : int
    val private tasks                       : BlockingCollection<Collections.Generic.KeyValuePair<int, Computation<'T>>>

    val private value                       : ModRef<'T>
    val mutable private activeComputation   : Option<Task<unit> * CancellationTokenSource>
    val lockObj : obj


    new (value) =
        {
        tasks               = new BlockingCollection<_>(new ConcurrentPriorityQueue<int, Computation<'T>>() :> IProducerConsumerCollection<_>)
        maxPriority         = 0
        value               = value
        activeComputation   = None
        lockObj             = obj()
        }   
        
    

    member public this.Stop() = 
        
        lock this.lockObj (fun _ -> 
            match this.activeComputation with
                | Some (t,cts) ->

                    // Kill the computation thread
                    Log.startTimed "Stopping AsyncComputationApplicator"
                    cts.Cancel()   
                    try
                        t.Wait()
                    with
                    | :? System.AggregateException as e -> 
                        ()
                    this.activeComputation <- None
                    Log.stop ()
                | None -> ()
                
        )



    member public this.Start() = 
            lock this.lockObj (fun _ -> 
                match this.activeComputation with
                    | Some (t,cts) -> ()
                    | None -> this.activeComputation <- Some (this.Coroutine())  
            ) 
       


    member private this.Coroutine () = 
        

        let cts = new CancellationTokenSource()


        let computationApplicatorCoroutine = 
            async {
                do! Async.SwitchToNewThread()

                let mutable workerThread :Thread   = null
                
                try 

                    while not cts.Token.IsCancellationRequested do
                        
                        
                        // Get the next task and timeout 
                        //Log.warn "%i tasks enqueued" this.tasks.Count

                        let computation     = this.tasks.Take(cts.Token)
                        this.maxPriority    <- computation.Key
                        let task            = computation.Value.Computation
                        let timeout         = computation.Value.Timeout
                        let timeoutCallback = computation.Value.TimeoutCallback

                        //Log.line "Starting new computation task"
                    
                        // Get the current value
                        let currentValue    = this.value |> Mod.force
                    

                        let mutable result  = None
                        
                        // Switch to new thread
                        workerThread <- Thread(fun () -> 
                            
                            try 
                                result <- task currentValue |> Some
                            with
                            | :? ThreadAbortException as e -> 
                                Log.line "Worker thread abort called"
                                
                         )


                        // Start worker thread
                        workerThread.Start()
                        // But wait for its completion for specific time
                        let timeout = if timeout > 0 then timeout else Timeout.Infinite
                        if not (workerThread.Join(timeout)) then 
                            Log.warn "Task computation exceeded timeout threshold! Terminating computation"
                            workerThread.Abort()
                            timeoutCallback()


                        // Perform changes at last
                        if not cts.IsCancellationRequested && result.IsSome then 
                            transact(fun _  -> Mod.change this.value result.Value)   
                        
                with
                | :? System.OperationCanceledException as e ->  
                        if workerThread <> null then 
                            workerThread.Abort()
                
            } 
        
        (Async.StartAsTask(computationApplicatorCoroutine , TaskCreationOptions.None, cts.Token), cts)
   

    member public this.DispatchPrioritized(computation : 'T -> 'T, timeout : int, timeoutCallback  : unit -> unit) = 
        
        this.maxPriority <- this.maxPriority + 1
        let computation = {Computation = computation ; Timeout = timeout ; TimeoutCallback = timeoutCallback}
        let pair        = new Collections.Generic.KeyValuePair<int, Computation<'T>>(this.maxPriority , computation)    

        this.tasks.Add pair


    member public this.DispatchPrioritized(computation : 'T -> 'T) = 
        this.DispatchPrioritized(computation, 0, fun () -> ())
        


    member public this.Dispatch(computation : 'T -> 'T, timeout : int, timeoutCallback  : unit -> unit) = 

        let computation = {Computation = computation ; Timeout = timeout ; TimeoutCallback = timeoutCallback}
        let pair        = new Collections.Generic.KeyValuePair<int, Computation<'T>>(0 , computation)
       // Log.line "Num Tasks Enqueued: %A" this.tasks.Count 
        this.tasks.Add pair  
        
        
     member public this.Dispatch(computation : 'T -> 'T) = 
        this.Dispatch(computation, 0, fun () -> ())  
        
        