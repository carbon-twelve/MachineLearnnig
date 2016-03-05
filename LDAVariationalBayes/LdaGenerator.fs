namespace LDAVariationalBayes

open System
open MathNet.Numerics.Distributions

type Word = {
    z: int
    w: int
}

type Document = {
    theta: float array
    d: Word seq
}

type Topic = float array

type LdaGenerator() =

    let findOneIndex (a: int array): int =
        Array.zip a [| 0 .. a.Length - 1 |]
        |> Array.find (fun (e, i) -> e = 1)
        |> snd

    member this.Generate(alpha: float array, beta: float array, numOfDocs: int, numOfWords: int -> int, numOfTopics: int): (Document seq * Topic array) =
        let topicDirichlet = Dirichlet(beta)
        let topics: Topic array = [| for k in [ 0 .. numOfTopics ] -> topicDirichlet.Sample() |]
        let phis: Multinomial array = [| for topic in topics -> Multinomial(topic, 1) |]
        let documentDirichlet = Dirichlet(alpha)
        let documents = seq {
            for d in [ 0 .. numOfDocs - 1 ] do
                let theta: float array = documentDirichlet.Sample()
                let mult = Multinomial(theta, 1)
                let d = seq {
                    for w in [ 0 .. numOfWords d - 1 ] do
                        let z = findOneIndex (mult.Sample())
                        let phi = phis.[z]
                        let w = findOneIndex (phi.Sample())
                        yield { z = z; w = w }
                }
                yield { theta = theta; d = d }
        }
        (documents, topics)
