namespace LDAVariationalBayes

open System
open MathNet.Numerics.Distributions

type Word = int

type Document = Word seq

type LdaGenerator() =

    let undefined () = NotImplementedException() |> raise
    let findOneIndex (a: int array): int =
        Array.zip a [| 0 .. a.Length - 1 |]
        |> Array.find (fun (e, i) -> e = 1)
        |> snd

    member this.Generate(alpha: float array, beta: float array, numOfDocs: int, numOfWords: int -> int, numOfTopics: int): Document seq =
        let topicDirichlet = Dirichlet(beta)
        let phis: Multinomial array = [| for k in [ 0 .. numOfTopics ] -> Multinomial(topicDirichlet.Sample(), 1) |]
        let documentDirichlet = Dirichlet(alpha)
        seq {
            for d in [ 0 .. numOfDocs - 1 ] do
                let theta: float array = documentDirichlet.Sample()
                let mult = Multinomial(theta, 1)
                yield seq {
                    for w in [ 0 .. numOfWords d - 1 ] do
                        let z = findOneIndex (mult.Sample())
                        let phi = phis.[z]
                        let w = findOneIndex (phi.Sample())
                        yield w
                }
        }
