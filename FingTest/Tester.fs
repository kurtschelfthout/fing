// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Fing.Tester
open Util
open Types
open TestCases
open NUnit.Framework
open System.Text.RegularExpressions

//// testing utils ////
// these are surprisingly easy to write and understand in their non-general tuple forms

let mapSnd f (a,b) = (a, f b)

let safezip l1 l2 = 
  let len = min (List.length l1) (List.length l2)
  Seq.zip (Seq.take len l1) (Seq.take len l2)

let testall results =
  results
  //|> Seq.take 20
  |> Seq.iteri (fun i (exp,act) -> Assert.AreEqual(exp :> obj,act :> obj, sprintf "%d. %A" i act))


let testWith f pairs = List.map (mapSnd f) pairs |> testall

//// correctness ////
// NOTE: Should this be part of production code instead of testing?
// it is an error to have a Choice [typar1;typar2;typar3] without typars 1 2 and 3 being
// bound by a containing generic type
// there are probably lots of other errors like this one (basically anything in a constraint)
//let rec unboundVars env : Typ -> option<Set<Typar>> = function
//  | Arrow ts -> Seq.tryPick (unboundVars env) ts
//  | Var (Choice vars) -> 
//        match Set.difference (Set.ofList vars) env with
//            | vs when vs = Set.empty -> None
//            | unbounds -> Some unbounds
//  | _ -> None

[<TestFixture>] 
type Tester() =
  let core = loadAssemblies [] |> Seq.find (fun asm -> asm.SimpleName.Contains("FSharp.Core"))
  // let parsec = Microsoft.FSharp.Metadata.FSharpAssembly.FromFile "Y:/src/Fing/Fing/bin/Debug/FParsec.dll"
  let ts =
    core.Contents.Entities
    |> Seq.map (fun e -> e.MembersFunctionsAndValues 
                         |> Seq.map (fun m -> {Fing.Entity=e
                                               Fing.Member=m
                                               Fing.Typ=FSharpTypes.cvt m.FullType |> Types.index |> FSharpTypes.debinarize
                                               }))
    |> Seq.concat
    |> Seq.toArray
  let actuallyFound expected (t : Result) =
    let toFind = format t.Typ
    let exp = format expected.Typ
    let found = Fing.typeFind toFind |> Seq.toArray

    found |> Seq.tryFind ((=) expected)

  let arrayShuffle (ara : 'a[]) =
    let rnd = System.Random()
    for i in [ara.Length .. -1 .. 1] do
      let j = rnd.Next i
      let tmp = ara.[j]
      ara.[j] <- ara.[i-1]
      ara.[i-1] <- tmp
    ara

  /// Make sure that every function in FSharp.Core can be found if you at least search
  /// for the exact type obtained from the FSharpType itself.
  [<Test>]
  member this.SmokeTest() =
    Fing.addReferences [] //inits the type list
    Seq.zip (Seq.map Some ts) (Seq.map2 actuallyFound ts ts) |> testall

//  [<Test>]
//  member this.ShuffleSmokeTest() =
//    let rec shuffled = function
//    | Arrow ts when List.length ts > 2  && List.length ts < 7 -> 
//      let args,res = Seq.butLast (List.map shuffled ts)
//      let args' = arrayShuffle (Array.ofSeq args) |> List.ofArray
//      Arrow (args' @ [res])
//    | Tuple ts when List.length ts < 6 ->
//      Tuple (arrayShuffle (Array.ofSeq (List.map shuffled ts)) |> List.ofArray)
//    | t -> t
//    ts |> Seq.iteri (fun i t ->
//                      let t' = {t with typ = shuffled t.typ }
//                      Assert.AreEqual(Some t,
//                                      actuallyFound t t',
//                                      sprintf "%d. %s: %s" i t.mem.DisplayName (format t'.typ)))


module TypeTester =

  [<Test>]
  let testFormat() =
    safezip (List.map Parser.parse passes) 
            (List.map (Types.format >> Parser.parse) passresults )
    |> testall

  [<Test>]
  let testIndex() =
    let usedIndices t =
      let usedIndicesTyp = function
      | Var v -> Some (Set.singleton v)
      | _ -> None
      printfn "%s" (Types.format t) // DEBUG
      (t, Types.fold usedIndicesTyp Set.unionMany Set.empty t)
    let randomise seq = Seq.zip seq (seq |> Array.ofSeq |> Array.shuffle)
    let subst (t, shuffles) =
      let map = Map.ofSeq shuffles
      let subst' = function
      | Var v -> Some (Var (Map.find v map))
      | _ -> None
      Types.map subst' id t
    safezip (List.map (usedIndices >> mapSnd randomise >> subst >> Types.index) 
                      passresults)
            passresults
    |> testall

  [<Test>]
  let testRevMap() =
    testWith Types.revMap [
      Map.empty, Map.empty
      Map.ofList [("a","b")], Map.ofList [("b","a")]
      Map.ofList [("a","b");("a","c")], Map.ofList [("c", "a")]
    ]


module ParseTester =

  [<Test>]
  let parseTest() = 
    Assert.AreEqual(List.length passes, List.length passresults)
    testall (List.zip passresults (List.map Parser.parse passes))
    List.zip passresults (List.map Parser.parse passes) 
    |> List.iteri (fun i (exp,act) -> Assert.AreEqual(exp,act,sprintf "%d. %A" i act))

