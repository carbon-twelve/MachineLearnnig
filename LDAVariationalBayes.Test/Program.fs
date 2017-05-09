module VariationalBayesTest

open Persimmon
open UseTestNameByReflection
open VariationalBayes
open System
open Persimmon.Assertions

let undefined() = raise (System.NotImplementedException())

let assertEqualsFloatArray acceptableError (expected: float array) (actual: float array) =
    let passes =
        Array.zip expected actual
        |> Array.forall (fun (expected, actual) -> Math.Abs(expected - actual) < acceptableError)
    if passes then pass ()
    else fail (sprintf "Expect: %A\nActual: %A" expected actual)
    
let thetaPosteriorUpdateTest = test {
    let thetaPosterior = ThetaPosterior(Alpha([|0.1; 0.1; 0.1|]), [|0.2; 0.3; 0.4|])
    do! assertEqualsFloatArray 0.001 [|0.3; 0.4; 0.5|] thetaPosterior.Gamma
}

let phiPosteriorUpdateTest = test {
    let phiPosterior = PhiPosterior(Beta(3, 0.1), [|0.2; 0.3; 0.4|])
    do! assertEqualsFloatArray 0.001 [|0.3; 0.4; 0.5|] phiPosterior.Lambda
}

let thetaPosteriorsUpdateTest = test {
    let thetaPosteriors =
        ThetaPosteriors(
            Alpha([|0.1; 0.2; 0.3; 0.4|]),
            Ndk(
                [|
                    [|0.01; 0.02; 0.03; 0.04|];
                    [|0.02; 0.04; 0.08; 0.16|];
                    [|0.03; 0.09; 0.27; 0.81|];
                |]
            )
        )
    do! assertEqualsFloatArray 0.001 [|0.11; 0.22; 0.33; 0.44|] thetaPosteriors.Array.[0].Gamma
    do! assertEqualsFloatArray 0.001 [|0.12; 0.24; 0.38; 0.56|] thetaPosteriors.Array.[1].Gamma
    do! assertEqualsFloatArray 0.001 [|0.13; 0.29; 0.57; 1.21|] thetaPosteriors.Array.[2].Gamma
}

let phiPosteriorsUpdateTest = test {
    let phiPosteriors =
        PhiPosteriors(
            Beta(4, 0.01),
            Nkv(
                [|
                    [|0.01; 0.02; 0.03; 0.04|];
                    [|0.02; 0.04; 0.08; 0.16|];
                    [|0.03; 0.09; 0.27; 0.81|];
                |]
            )
        )
    do! assertEqualsFloatArray 0.001 [|0.02; 0.03; 0.04; 0.05|] phiPosteriors.Array.[0].Lambda
    do! assertEqualsFloatArray 0.001 [|0.03; 0.05; 0.09; 0.17|] phiPosteriors.Array.[1].Lambda
    do! assertEqualsFloatArray 0.001 [|0.04; 0.10; 0.28; 0.82|] phiPosteriors.Array.[2].Lambda
}

let zPosteriorsUpdateTest = test {
    let documents: Doc array =
        let rawDocuments =
            [
                [0; 2; 5];
                [5; 5; 3; 4; 1];
                [4; 2; 3; 4];
            ]
        [|
            for (index, doc) in List.indexed rawDocuments do
                let words =
                    seq {
                        for (index, word) in List.indexed doc do
                            yield VocabIndex(word)
                    }
                yield Doc(DocIndex(index), words)
        |]
    let thetaPosteriors =
        ThetaPosteriors(
            [|
                ThetaPosterior([|0.1; 0.3|]);
                ThetaPosterior([|0.5; 1.|]);
                ThetaPosterior([|10.; 0.2|]);
            |]
        )
    let phiPosteriors =
        PhiPosteriors(
            [|
                PhiPosterior([||]);
                PhiPosterior([||]);
            |]
        )
    let zPosteriors = ZPosteriors()
    do! assertEquals 0 0
}