namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Fing")>]
[<assembly: AssemblyProductAttribute("Fing")>]
[<assembly: AssemblyDescriptionAttribute("Search for F# functions by function signature.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
    let [<Literal>] InformationalVersion = "1.0"
