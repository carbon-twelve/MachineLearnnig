// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open LDAVariationalBayes

[<EntryPoint>]
let main argv = 
    let generator = LdaGenerator()
    let result = generator.Generate([|1.0; 1.0; 1.0|], [|0.5; 0.3; 0.2|], 3, (fun _ -> 10), 10)
    printf "%A" result
    0 // return an integer exit code

