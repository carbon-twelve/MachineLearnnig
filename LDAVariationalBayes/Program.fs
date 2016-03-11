module Program

open System.Diagnostics

let convertDoc (originalDocs: LdaGenerator.Doc array): VariationalBayes.Doc array =
    [|
        for (docIndex, originalDoc) in Array.indexed originalDocs do
            let words = seq { for word in originalDoc -> VariationalBayes.VocabIndex(word)}
            yield VariationalBayes.Doc(VariationalBayes.DocIndex(docIndex), words)
    |]

let log =
    let log = TraceSource("Program")
    use consoleListener = new ConsoleTraceListener()
    log.Listeners.Add(consoleListener) |> ignore
    log.Switch.Level <- SourceLevels.Information
    log
    

[<EntryPoint>]
let main argv =
    let generator = LdaGenerator.LdaGenerator()
    let alpha = [| 40.; 10.; 20. |]
    let beta = [|0.5; 0.5; 0.5; 0.5; 0.5|]
    log.TraceInformation("Generating documents"); log.Flush()
    let (docs, hiddenVars) = generator.Generate(alpha, beta, 100, (fun _ -> 10000))
    log.TraceInformation("Generated documents: {0}", docs); log.Flush()
    log.TraceInformation("Started inference"); log.Flush()
    let inferredVars = VariationalBayes.infer(convertDoc docs, alpha.Length, beta.Length)
    log.TraceInformation("Completed inference"); log.Flush()
    printf
        "Original alpha: %A\nInferred alpha: %A\n\nOriginal beta: %A\nInferred beta: %A\n\nOriginal theta: %A\nInferred theta: %A\n\nOriginal phi: %A\nInferred phi: %A\n\n" //Original z: %A\nInferred z: %A\n\n"
        alpha inferredVars.alpha
        beta inferredVars.beta
        hiddenVars.thetas inferredVars.thetas
        hiddenVars.phis inferredVars.phis
        //hiddenVars.zs inferredVars.zs
    0 // return an integer exit code

