#load @"paket-files/build/vrvis/Aardvark.Fake/DefaultSetup.fsx"

open Fake
open System
open System.IO
open System.Diagnostics
open Aardvark.Fake
open Fake.Testing
open Mono.Cecil
open System.IO.Compression


do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

DefaultSetup.install ["src/Vgm.sln"]

Target "Tests" (fun () ->
    NUnit3 id [@"bin\Release\VRVis.Vgm.Tests.dll"]
)

"Compile" ==> "Tests"

Target "Deliverable" (fun () ->
    let temp = System.IO.Path.GetTempPath()
    let binrelease = "bin/release"

    let unpackDependencies (folder : string) (assemblyPath : string) =
        try
            let symbols = File.Exists (Path.ChangeExtension(assemblyPath, "pdb"))
        
            let a = AssemblyDefinition.ReadAssembly(assemblyPath,ReaderParameters(ReadSymbols=symbols))

            let res = a.MainModule.Resources |> Seq.tryFind (fun r -> r.Name = "native.zip")
            match res with
                | Some res -> 
                    tracefn "found dependency for %A" assemblyPath 
                    let data = (res :?> EmbeddedResource).GetResourceStream()
                    data.Position <- 0L
                    let archive = new ZipArchive(data, ZipArchiveMode.Read, true)
                    let entries = archive.Entries
                    for e in entries do
                        if e.FullName.Contains("windows") then
                            if e.FullName.Contains("AMD64") then
                                tracefn "extracting %A" e.FullName 
                                let filename = Path.Combine(folder,e.Name)
                                if File.Exists filename then File.Delete filename
                                e.ExtractToFile filename
                | None -> 
                    //tracefn "not found anything for %A" assemblyPath 
                    ()
        with
            | :? System.BadImageFormatException ->  //is not a Mono.Cecil assembly!
                //tracefn "bad image format %A" assemblyPath
                ()

    let d = binrelease |> Path.GetFullPath
    let ns = d |> Directory.GetFiles |> Seq.map ( fun f -> f |> Path.GetFileNameWithoutExtension)
    //tracefn "unpacking native dependencies for %A files" (ns |> Seq.length)
    for n in ns do
        let paths = [
                Path.Combine("bin/Debug", n + ".dll") |> Path.GetFullPath
                Path.Combine("bin/Debug", n + ".exe") |> Path.GetFullPath
                Path.Combine("bin/Release", n + ".dll") |> Path.GetFullPath
                Path.Combine("bin/Release", n + ".exe") |> Path.GetFullPath
            ]

        for p in paths do
            if File.Exists p then
                unpackDependencies d p

    let files = Fake.FileHelper.copyRecursive (directoryInfo binrelease) (directoryInfo temp) true
    let tag = Fake.Git.Information.getLastTag()
    if System.IO.Directory.Exists "deliverable" |> not then System.IO.Directory.CreateDirectory "deliverable" |> ignore

    Fake.ZipHelper.Zip temp ( "deliverable/VGM-" + tag + ".zip" ) files

)

"Compile" ==> "Deliverable"

#if DEBUG
do System.Diagnostics.Debugger.Launch() |> ignore
#endif


entry()