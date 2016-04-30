// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Fing.Tester

open Util
open Types
open TestCases
open Fuchu
open Swensen.Unquote

//// testing utils ////
// these are surprisingly easy to write and understand in their non-general tuple forms

let mapSnd f (a,b) = (a, f b)

let testAll l = 
        l |> Seq.mapi (fun i (exp,act) -> testCase (sprintf "%i" i) (fun _ -> test <@ exp = act @>))

let testWith f pairs = Seq.map (mapSnd f) pairs |> testAll

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

  /// Make sure that every function in FSharp.Core can be found if you at least search
  /// for the exact type obtained from the FSharpType itself.
  [<Tests>]
  member __.SmokeTest =
    testList "smokeTest" <|
        let init = lazy Fing.addReferences [] //inits the type list
        seq { do init.Value
              yield! Seq.map2 actuallyFound ts ts
                     |> Seq.zip (Seq.map Some ts)  
                     |> testAll
        }

module TypeTester =

  //[<Tests>]
  let testIndex =
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
    testList "testIndex" ( 
        validParserResults
        |> Seq.zip (validParserResults |> Seq.map (usedIndices >> mapSnd randomise >> subst >> Types.index))
        |> testAll)

  [<Tests>]
  let testRevMap =
    testList "testRevMap" <|
        testWith Types.revMap [
          Map.empty, Map.empty
          Map.ofList [("a","b")], Map.ofList [("b","a")]
          Map.ofList [("a","b");("a","c")], Map.ofList [("c", "a")]
        ]


module ParserTest =

    [<Tests>]
    let testFormat =
        testList "testFormat" (
            Seq.map (Types.format >> Parser.parse) validParserResults
            |> Seq.zip (Seq.map Parser.parse validParserInputs)      
            |> testAll)
            

    [<Tests>]
    let parseTest = 
        testList "parseTest" <|
            seq {
                yield testCase "precheck" (fun _ -> test <@ List.length validParserInputs = List.length validParserResults @>)
                yield! Seq.map Parser.parse validParserInputs
                       |> Seq.zip validParserResults 
                       |> testAll
            }

