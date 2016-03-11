namespace LdaGenerator

open System
open System.Diagnostics
open MathNet.Numerics.Distributions

type Alpha = float array
type Beta = float array
type Theta = float array
type ThetaDist = Dirichlet
type Phi = float array
type PhiDist = Dirichlet
type Z = int
type ZDist = Multinomial
type WDist = Multinomial
type Word = int
type Doc = Word list
type HiddenVar = {
    thetas: Theta array
    phis: Phi array
    zs: int array array
}

type LdaGenerator() =
    
    let log =
        let log = TraceSource("LdaGenerator")
        use consoleListener = new ConsoleTraceListener()
        log.Listeners.Add(consoleListener) |> ignore
        log.Switch.Level <- SourceLevels.Information
        log

    let findOneIndex (a: int array): int =
        Array.indexed a
        |> Array.find (fun (i, e) -> e = 1)
        |> fst

    member this.Generate(alpha: Alpha, beta: Beta, numOfDocs: int, numOfWords: int -> int): (Doc array * HiddenVar) =
        let numOfTopics = alpha.Length
        log.TraceInformation("Generating Phi"); log.Flush()
        let phiDist: PhiDist = Dirichlet(beta)
        let phis: Phi array = Array.init numOfTopics (fun _ -> phiDist.Sample())
        let wDists: WDist array = [| for phi in phis -> Multinomial(phi, 1) |]
        log.TraceInformation("Generating Theta"); log.Flush()
        let thetaDist: ThetaDist = Dirichlet(alpha)
        let thetas: Theta array = Array.init numOfDocs (fun _ -> thetaDist.Sample())
        log.TraceInformation("Generating Z"); log.Flush()
        let zDists: ZDist array = [| for theta in thetas -> Multinomial(theta, 1) |]
        let zs: int array array = Array.init numOfDocs (fun d -> Array.create (numOfWords d) 0)
        let docs: Doc array =
            [|
                for d in [ 0 .. numOfDocs - 1 ] do
                    let zDist = zDists.[d]
                    yield [
                        for w in [ 0 .. numOfWords d - 1 ] do
                            let z = findOneIndex (zDist.Sample())
                            let wDist = wDists.[z]
                            yield findOneIndex (wDist.Sample())
                    ]
            |]
        (docs, { thetas = thetas; phis = phis; zs = zs })
