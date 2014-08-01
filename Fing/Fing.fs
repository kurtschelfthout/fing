// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
#if INTERACTIVE
#I @"..\packages\FSharp.Compiler.Service.0.0.58\lib\net45"
#r "FSharp.Compiler.Service"
#endif
module Fing
open Microsoft.FSharp.Compiler.SourceCodeServices
open Types
open Util
open Search

type Result = {
  ent : FSharpEntity
  mem : FSharpMemberOrVal
  typ : Typ
}
let checker = InteractiveChecker.Create()
let projectOptions = 
    checker.GetProjectOptionsFromCommandLineArgs
       ("Fing.fsproj",
        [| yield "--simpleresolution" 
           yield "--noframework" 
           yield "--debug:full" 
           yield "--define:DEBUG" 
           yield "--optimize-" 
           yield "--out:" + "Fing.dll"
           yield "--warn:3" 
           yield "--fullpaths" 
           yield "--flaterrors" 
           yield "--target:library" 
           yield @"C:\Users\Kurt\Projects\fing\Fing\Util.fs"
           //yield Inputs.fileName1
           //yield Inputs.fileName2
           let references = 
             [ @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\mscorlib.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Core.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Numerics.dll" 
               @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll"]  
           for r in references do
                 yield "-r:" + r |])
 
let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
let referenced = wholeProjectResults.ProjectContext.GetReferencedAssemblies()
let fslib = referenced |> List.find (fun r -> r.SimpleName = "FSharp.Core")

let entite { ent = e } = e
let membre { mem = m } = m
let tipe { typ = t } = t
let private formatResult { ent = e; mem = m; typ = t } = 
  sprintf "%s.%s\t\t%s" e.DisplayName m.DisplayName (format t)
// TODO: Cache this on disk or something
let mutable private assemblies : Set<string> = Set.empty
let mutable private types : list<Result> = []
let private updateReferences (refs : seq<FSharpAssembly>) =
  types <- 
    [for ref in Seq.append (Seq.singleton fslib) refs do
     for e in ref.Contents.Entities do
     for m in e.MembersOrValues do
     yield {ent=e; mem=m; typ=FSharpTypes.cvt m.FullType |> index |> FSharpTypes.debinarize} ]
let addReferences news =
  assemblies <- assemblies |>Set.union<| set news
  let optionAssembly assembly = None
//    try
//      FSharpAssembly.FromFile assembly |> Some
//    with
//      | :? System.IO.FileNotFoundException -> printfn "%s was not found" assembly; None
//      // Indicates a C# assembly, someday I'll handle this
//      | :? System.ArgumentException -> printfn "%s is not an F# assembly" assembly; None 
  updateReferences (Seq.choose id (Seq.map optionAssembly assemblies))
do addReferences []
// Public interface
// (other functions aren't private yet because it's so inconvenient;
// probably I should just move everything else to another module.)
let typeFind s =
  let ty = Parser.parse s |> index |> ParsedTypes.dealias
  types |> Seq.filter (tipe >> matches ty)
let nameFind s = 
  types |> Seq.filter (fun {mem = m} -> m.DisplayName = s)
let search (s : string) =
  if s.Contains "->" then typeFind s else nameFind s
let textSearch s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (formatResult >> printfn "\t%s") (search s)
let debug t t' = matches t t'