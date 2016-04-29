
#I "../packages/FParsec.1.0.1/lib/net40-client/"
#I "../packages/FSharp.Compiler.Service.0.0.59/lib/net45"
#I "../Fing/bin/Debug/"

#r "FParsecCS"
#r "FParsec"
#r "FSharp.Compiler.Service"

#load @"..\Fing\Util.fs"
#load @"..\Fing\Types.fs"
#load @"..\Fing\ParsedTypes.fs"
#load @"..\Fing\FSharpTypes.fs"
#load @"..\Fing\Parser.fs"
#load @"..\Fing\Search.fs"
#load @"..\Fing\Fing.fs"
#load @"TestCases.fs"
// #load "Tester.fs"
open Types
open Util
let core = Microsoft.FSharp.Metadata.FSharpAssembly.FSharpLibrary
let parsec = Microsoft.FSharp.Metadata.FSharpAssembly.FromFile "C:/src/Fing/Fing/Fing/bin/Debug/FParsec.dll"
let ts = seq { // Seq.choose id (seq { 
  for e in core.Entities do
  for m in e.MembersOrValues do
  yield {Fing.ent=e; Fing.mem=m; Fing.typ=FSharpTypes.cvt m.Type |> Types.index |> FSharpTypes.debinarize} 
}
let rawts = seq {
  for e in core.Entities do
  for m in e.MembersOrValues do
  yield m
}
let indices = 1 |> Seq.unfold (fun i -> Some(i, i+1))
// look for the indices of things that fail in cvt
// TODO: Turn this into a test
Seq.zip indices rawts
 |> Seq.iter (fun (i,t) ->
  try
    (FSharpTypes.cvt t.Type) |> ignore
    printf "." 
  with _ ->
    printfn "%d: %s . %s" i t.LogicalEnclosingEntity.DisplayName t.DisplayName
 )
FParsec.Primitives.preturn
for i,t in Seq.zip indices ts do
  printfn "%d. %s : %s" i (t.mem.DisplayName) (format <| Fing.tipe t)
  