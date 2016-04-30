// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Fing

open Microsoft.FSharp.Compiler.SourceCodeServices
open Types
open Util
open Search
open System.IO

type Result = {
  Entity : FSharpEntity
  Member : FSharpMemberOrFunctionOrValue
  Typ : Typ
}

let loadAssemblies refs =
  let base1 = System.IO.Path.GetTempFileName()
  let fileName1 = Path.ChangeExtension(base1, ".fs")
  let fileSource1 = """
module Blah
    """
  File.WriteAllText(fileName1, fileSource1)
  let checker = FSharpChecker.Create()
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
             yield fileName1
             let references = 
               [ @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\mscorlib.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\System.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\System.Core.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\System.Numerics.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\System.Configuration.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\System.Xml.dll" 
                 @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"]
                 @ (Seq.toList refs)
             for r in references do
                   yield "-r:" + r |])
 
  let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
  let referenced = wholeProjectResults.ProjectContext.GetReferencedAssemblies()
  referenced

let entite { Entity = e } = e
let membre { Member = m } = m
let tipe { Typ = t } = t
let private formatResult { Entity = e; Member = m; Typ = t } = 
  sprintf "%s.%s\t\t%s" e.DisplayName m.DisplayName (format t)
// TODO: Cache this on disk or something
let mutable private assemblies : Set<string> = Set.empty
let mutable private types : list<Result> = []

let private updateReferences (refs : seq<FSharpAssembly>) =
  types <- 
    [ for ref in refs do
        for e in ref.Contents.Entities do
          for m in e.MembersFunctionsAndValues do
            yield {Entity=e; Member=m; Typ=FSharpTypes.cvt m.FullType |> index |> FSharpTypes.debinarize} ]

let addReferences news =
  assemblies <- assemblies + (set news)
  assemblies |> loadAssemblies |> updateReferences



// Public interface
// (other functions aren't private yet because it's so inconvenient;
// probably I should just move everything else to another module.)
let typeFind s =
  let ty = Parser.parse s |> index |> Types.dealias
  types |> Seq.filter (tipe >> matches ty)
let nameFind s = 
  types |> Seq.filter (fun {Member = m} -> m.DisplayName = s)
let search (s : string) =
  if s.Contains "->" then typeFind s else nameFind s
let textSearch s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (formatResult >> printfn "\t%s") (search s)
let debug t t' = matches t t'
