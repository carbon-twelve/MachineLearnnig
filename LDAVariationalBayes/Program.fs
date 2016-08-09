module Program

open System.Diagnostics

let undefined() = raise (System.NotImplementedException())

let convertDoc (originalDocs: LdaGenerator.Doc array): VariationalBayes.Doc array =
    [|
        for (docIndex, originalDoc) in Array.indexed originalDocs do
            let words = seq { for word in originalDoc -> VariationalBayes.VocabIndex(word)}
            yield VariationalBayes.Doc(VariationalBayes.DocIndex(docIndex), words)
    |]

let log = TraceSource("Program")
    
let testByMockData() =
    let generator = LdaGenerator.LdaGenerator()
    let alpha = [| 40.; 10.; 20. |]
    let beta = [| 0.5; 0.5; 0.5; 0.5; 0.5 |]
    Trace.WriteLine("Generating documents"); Trace.Flush()
    let (docs, hiddenVars) = generator.Generate(alpha, beta, 100, (fun _ -> 10000))
    log.TraceInformation("Generated documents: {0}", docs)
    log.TraceInformation("Started inference")
    let inferredVars = VariationalBayes.infer(convertDoc docs, alpha.Length, beta.Length)
    log.TraceInformation("Completed inference")
    printf
        "Original alpha: %A\nInferred alpha: %A\n\nOriginal beta: %A\nInferred beta: %A\n\nOriginal theta: %A\nInferred theta: %A\n\nOriginal phi: %A\nInferred phi: %A\n\n" //Original z: %A\nInferred z: %A\n\n"
        alpha inferredVars.alpha
        beta inferredVars.beta
        hiddenVars.thetas inferredVars.thetas
        hiddenVars.phis inferredVars.phis
        //hiddenVars.zs inferredVars.zs

open System.Collections.Generic        
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO

let test新人間革命() =
    let wordToIndex = Dictionary<string, int>()
    let corpus: JsonValue = JsonValue.Load(@"C:\Users\Takayuki\Projects\MachineLearnnig\LDAVariationalBayes\data\corpus.json\corpus.json")
    let docs: VariationalBayes.Doc array =
        [|        
            for paragraphIndex, paragraph in Array.indexed (corpus.AsArray()) do
                let doc =
                    [
                        for word in paragraph.AsArray() do
                            let word = word.AsString()
                            let vocabIndex =
                                try wordToIndex.[word]
                                with _ ->
                                    let index = wordToIndex.Count
                                    wordToIndex.[word] <- index
                                    index
                            yield VariationalBayes.VocabIndex(vocabIndex)
                    ]
                    |> List.toSeq
                yield VariationalBayes.Doc(VariationalBayes.DocIndex(paragraphIndex), doc)
        |]
    let indexToWord = Dictionary<int, string>()
    for entry in wordToIndex do
        indexToWord.[entry.Value] <- entry.Key
    log.TraceInformation("Started inference"); log.Flush()
    let inferredVars: VariationalBayes.InferredVars = VariationalBayes.infer(docs, 50, wordToIndex.Count)
    log.TraceInformation("Inference completed"); log.Flush()
    printf
        "Inferred theta: %A\n\nInferred phi: %A\n" 
        inferredVars.thetas
        inferredVars.phis
        //hiddenVars.zs inferredVars.zs

    let output: JsonValue =
        [|
            for phiPosterior in inferredVars.phis.Array do
                yield Array.indexed phiPosterior.Lambda
                |> Array.sortByDescending snd
                |> Array.map (fun (vocabIndex, _) -> JsonValue.String(indexToWord.[vocabIndex]))
                |> JsonValue.Array
        |]
        |> JsonValue.Array
    use outputWriter = new StreamWriter(@"C:\Users\Takayuki\Projects\MachineLearnnig\LDAVariationalBayes\output\output.json", false, System.Text.Encoding.UTF8)
    output.WriteTo(outputWriter, JsonSaveOptions.None)

[<EntryPoint>]
let main argv =
    testByMockData()
    0 // return an integer exit code

