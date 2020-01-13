namespace InteractiveShapeDetection.Base
open Aardvark.Base


type Edge<'T> = 
    val mutable public V0  : 'T
    val mutable public V1  : 'T
    val mutable public d   : float
        

    new (v0, v1,d) = 
        {
        V0 = v0
        V1 = v1
        d = d
        }


type Graph<'T when 'T : equality> = 

    val mutable public vertices    : System.Collections.Generic.HashSet<'T> 
    val mutable public edges       : Edge<'T> []
    val mutable public equalityFun : ('T ->'T-> bool)
    val mutable public distanceFun : ('T ->'T-> float)
        

    new (vertices, edges, equalityFun, distanceFun) =
        {
            vertices    = vertices
            edges       = edges
            equalityFun = equalityFun
            distanceFun = distanceFun
        }        


    member this.computeDistanceMatrix (epsilon:float) = 
           
        let numEdges    = this.edges |> Array.length
        let numVertices = this.vertices.Count
        let distances   = Array2D.create numVertices numVertices infinity
        let vertices = this.vertices.ToArray(numVertices)
        this.edges |> Array.iter(fun edge -> 
                                    let v0 = edge.V0
                                    let v1 = edge.V1

                                    let x = vertices |> Array.findIndex(fun v -> this.equalityFun v v0)
                                    let y = vertices |> Array.findIndex(fun v -> this.equalityFun v v1)
                   
                                    if edge.d < epsilon then 
                                        distances.[x,y] <- edge.d
                                )
        distances
   
     
     
type Graph with  
        
    static member Add (edge : Edge<'T>) (graph : Graph<'T>) = 
        let vertices = System.Collections.Generic.HashSet<'T>(graph.vertices)

        vertices.Add edge.V0 |> ignore
        vertices.Add edge.V1 |> ignore
            
        let edges = Array.append [|edge|] graph.edges

        Graph(vertices, edges, graph.equalityFun, graph.distanceFun)


    static member CompleteGraph<'T> vertices equalityFun distanceFun  = 
        let edges = 
            let numVertices = vertices |> Array.length 
            let mutable edges = []
            for i in 0.. numVertices-1 do
                for j in 0.. numVertices-1 do
                    if i = j then 
                        ()
                    else
                        let v0 = vertices.[i]
                        let v1 = vertices.[j]
                        let d = distanceFun v0 v1
                        edges <- Edge(v0,v1,d) :: edges
            edges |> List.toArray
        let vertices = System.Collections.Generic.HashSet<'T>(vertices)

        Graph<'T>(vertices, edges, equalityFun, distanceFun )


    static member FindClosest(vertex:'T)(epsilon : float)(graph : Graph<'T>) =  
        
        if graph.vertices.Count = 0 then 
            None
        else
            graph.vertices.ToArray(graph.vertices.Count)  
                |> Parallel.map (fun v ->(v , graph.distanceFun v vertex))
                |> Array.tryMinBy (fun (t,d) -> d)
                |> Option.map (fun (t,d) -> t)



    static member ConnectedComponent (vertex :'T) (epsilon:float) (graph :Graph<'T>) = 
            
        let vertex = graph |> Graph.FindClosest vertex epsilon
        match vertex with
        | None          ->  
            Graph(HashSet.empty, [||], graph.equalityFun, graph.distanceFun)
        | Some vertex   -> 

            let mutable edges   = graph.edges   |> Array.copy 
                                                |> Array.filter (fun e -> e.d < epsilon) 
                                                |> Array.toList                                             
        
            let rec grow (vertex : 'T) : (Edge<'T> list) = 
                let (neighbors,remaining) = edges   |> List.partition (fun edge -> graph.equalityFun edge.V0 vertex) 
                edges <- remaining  
                
                neighbors   |> List.map (fun edge -> grow edge.V1)
                            |> List.concat
                            |> List.append neighbors               
            
            let edges       = grow vertex |> List.toArray
            
            let vertices = System.Collections.Generic.HashSet<'T>()
            vertices.Add(vertex) |> ignore

            for edge in edges do
                vertices.Add(edge.V0) |> ignore
                vertices.Add(edge.V1) |> ignore    

            Graph(vertices, edges, graph.equalityFun, graph.distanceFun)
        

    static member ContainsCircle (graph : Graph<'T> )  = 
                                                
            
        let remove value list =    
            list    |> Array.filter(fun v -> v <> value)                          


        let mutable disjointSet = 
            graph.vertices.ToArray(graph.vertices.Count)    |> Array.map (fun v -> [|v|])  
        let mutable isCyclic = false
            
        graph.edges |> Array.takeWhile(fun edge -> 
            let rep1 = disjointSet |> Array.findIndex (fun list -> list |> Array.contains edge.V0)
            let rep2 = disjointSet |> Array.findIndex (fun list -> list |> Array.contains edge.V1)
                
            if rep1 = rep2 then 
                isCyclic <- true
                false
            else
                // Union set1 set2
                let set1 = disjointSet.[rep1]
                let set2 = disjointSet.[rep2]

                // remove old sets
                disjointSet <- disjointSet  |> remove set1
                                            |> remove set2

                // add union set
                let union = Array.append set1 set2
                disjointSet <- Array.append [|union|] disjointSet
                    
                true

            ) |> ignore
        isCyclic
             


    static member SpanningTreeKruskal (graph : Graph<'T>) = 
                           
        let mutable tree = Graph<'T>(HashSet.empty,[||], graph.equalityFun, graph.distanceFun)
            
        graph.edges |> Array.sortBy(fun edge -> edge.d)
                    |> Array.iter  (fun edge -> 

                                            let treeWithEdge = tree |> Graph.Add edge
                
                                            if treeWithEdge |> Graph.ContainsCircle |> not then 
                                                tree <- treeWithEdge
                                                    
                                        ) |> ignore
        tree



    static member SpanningTreePrim (vertex:'T) (graph : Graph<'T>) = ()


