module VariationalBayes

open System
open System.Collections.Generic
open System.Diagnostics
open MathNet.Numerics
open MathNet.Numerics.Distributions

let undefined() = raise (System.NotImplementedException())
let psi = SpecialFunctions.DiGamma
let addArray (array1: float array) (array2: float array): float array =
    Array.zip array1 array2 |> Array.map (fun (a, b) -> a + b)
let log =
    let log =TraceSource("VariationalBayes")
    use consoleListener = new ConsoleTraceListener()
    log.Listeners.Add(consoleListener) |> ignore
    log.Switch.Level <- SourceLevels.Information
    log

type DocIndex(d: int) =
    member this.Index = d

type TopicIndex(k: int) =
    member this.Index = k

type VocabIndex(v: int) =
    member this.Index = v

type WordIndex(i: int) =
    member this.Index = i

type Word(i: WordIndex, v: VocabIndex) =
    member this.Index: WordIndex = i
    member this.VocabIndex: VocabIndex = v

type Doc(docIndex: DocIndex, doc: VocabIndex seq) =
    member this.Length: int = Seq.length doc
    member this.Words: Word seq =
        Seq.indexed doc
        |> Seq.map (fun (i, v) -> Word(WordIndex(i), v))
    member this.Index: DocIndex = docIndex
    member this.WordIndexRange: WordIndex seq =
        seq { for i in [ 0 .. this.Length - 1] -> WordIndex(i) }

type InvertedDoc(documents: Doc seq, vocabularySize: int) =
    let invertedDoc: (DocIndex * WordIndex) list array =
        let array = Array.create vocabularySize List.empty
        let groupsByVocab = 
            query { 
                for doc in documents do
                    for word in doc.Words do
                        groupBy word.VocabIndex.Index into g
                        select g
            }
        for group in groupsByVocab do
            let vocab = group.Key
            array.[vocab] <- [ for (doc, word) in group -> (doc.Index, word.Index) ]
        array
        
    member this.Item(vocabIndex: VocabIndex): (DocIndex * WordIndex) list =
        invertedDoc.[vocabIndex.Index]
    member this.Array = invertedDoc

type Alpha(alphaArray : float array) = 
    new(numOfTopics: int, initValue: float) = new Alpha(Array.create numOfTopics initValue)
    member this.Array = alphaArray
    override this.ToString() = sprintf "%A" alphaArray
    member this.Update(documents: Doc array, ndk: Ndk): Alpha =
        let sumAlpha : float = Array.sum alphaArray
        let newAlphaArray =
            let b =
                documents
                |> Array.map (fun doc -> psi ((float) doc.Length + sumAlpha) - psi (sumAlpha))
                |> Array.sum
            let aArray =
                [|
                    for k in [ 0 .. alphaArray.Length - 1 ] do
                        yield List.sum [ for (nd: float array) in ndk.Array -> (psi(nd.[k] + alphaArray.[k]) - psi(alphaArray.[k])) * alphaArray.[k] ]
                |]
            let newAlphaArray = Array.map (fun a -> a / b) aArray
            assert (Array.forall (fun a -> not (Double.IsNaN a) && a > 0.) newAlphaArray)
            newAlphaArray
        Alpha(newAlphaArray)

and Beta(vocabSize: int, beta: float) =
    let array = Array.create vocabSize beta
    member this.Value: float = beta
    member this.AsArray: float array = array
    override this.ToString() = sprintf "%A" array
    member this.Update(nkv: Nkv): Beta =
        let a =
            seq {
                for nk in nkv.Array do
                    for n in nk do
                        yield (psi(n + beta) - psi(beta)) * beta
            }
            |> Seq.sum
        let b =
            seq {
                for nk in nkv.Array do
                    let vSum = Seq.sum nk
                    yield psi(vSum + beta) - psi((float) vocabSize * beta)
            }
            |> Seq.sum
        let newBeta = a/ b / (float) vocabSize
        assert (newBeta > 0.)
        Beta(vocabSize, newBeta)

and ThetaPosterior = Dirichlet
and Theta(array: ThetaPosterior array) =
    new (docSize: int, init: int -> ThetaPosterior) =
        Theta(Array.init docSize init)
    member this.Item
        with get(docIndex: DocIndex): ThetaPosterior = array.[docIndex.Index]
        and set (docIndex: DocIndex) (value: ThetaPosterior) = array.[docIndex.Index] <- value
    member this.Array = array
    member this.Update(alpha: Alpha, ndk: Ndk): Theta =
        let newArray =
            [|
                for nd in ndk.Array do
                    let dirAlpha = addArray nd alpha.Array
                    assert (Array.forall (fun a -> not (Double.IsNaN a))dirAlpha)
                    yield Dirichlet(dirAlpha)
            |]
        Theta(newArray)
        
    override this.ToString() = sprintf "%A" (Array.map (fun (e: Dirichlet) -> e.Mean) array)

and PhiPosterior = Dirichlet
and Phi(array: PhiPosterior array) =
    new (numOfTopics, init: int -> PhiPosterior) =
        Phi(Array.init numOfTopics init)
    member this.Array = array
    member this.Update(nkv: Nkv, beta: Beta): Phi =
        let newArray =
            [|
                for nk in nkv.Array do
                    let alphaArray = addArray nk beta.AsArray
                    assert (Array.forall (fun a -> not (Double.IsNaN a)) alphaArray)
                    yield Dirichlet(alphaArray)
            |]
        Phi(newArray)

    override this.ToString() = sprintf "%A" (Array.map (fun (e: Dirichlet) -> e.Mean) array)

and ZPosterior = Multinomial
and Z(array: ZPosterior array array) =
    new (docSize: int, numOfWords: int -> int, init: int -> int -> ZPosterior) =
        Z(Array.init docSize (fun d -> Array.init (numOfWords d) (fun i -> init d i)))
    member this.Item
        with get(docIndex: DocIndex, wordIndex: WordIndex): ZPosterior = array.[docIndex.Index].[wordIndex.Index]
        and set (docIndex: DocIndex, wordIndex: WordIndex) (value: ZPosterior) = array.[docIndex.Index].[wordIndex.Index] <- value
    member this.Array = array
    member this.Update(documents: Doc array, thetas: Theta, phis: Phi): Z =
        let newArray: ZPosterior array array =
            [|
                for (doc: Doc, theta: ThetaPosterior) in Array.zip documents thetas.Array do
                    yield async {
                        return [|
                            for word in doc.Words do
                                yield async {
                                    let multParam = 
                                        [| for (phiPosterior: PhiPosterior, thetaPosteriorAlpha: float) in Array.zip phis.Array theta.Alpha -> 
                                                let a = Math.Exp(psi(phiPosterior.Alpha.[word.VocabIndex.Index]))
                                                let b = Math.Exp(psi(Array.sum phiPosterior.Alpha))
                                                let c = Math.Exp(psi(thetaPosteriorAlpha))
                                                let d = Math.Exp(psi(Array.sum theta.Alpha))
                                                let e = (a / b) * (c / d)
                                                //log.TraceInformation("a: {0}, b: {1}, c: {2}, d: {3}, e: {4}", a, b, c, d, e); log.Flush()
                                                e |]
                                    return Multinomial(Array.map (fun p -> p / Array.sum multParam) multParam, 1)
                                }
                        |]
                        |> Async.Parallel
                    }
            |]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Async.Parallel
            |> Async.RunSynchronously
        Z(newArray)

    override this.ToString() = sprintf "%A" (Array.map (fun (e: ZPosterior array) -> Array.map (fun (e': ZPosterior) -> e'.P) e) array)

and Ndk(array: float array array) =
    member this.Array = array
    member this.Update(documents: Doc array, zs: Z, numOfTopics: int): Ndk =
        let newArray: float array array =
            [|
                for (doc: Doc, zd: ZPosterior array) in Array.zip documents zs.Array do
                    let nd =
                        zd
                        |> Array.map (fun zdi -> zdi.P)
                        |> Array.fold addArray (Array.zeroCreate numOfTopics)
                    assert (Math.Abs(Array.sum nd - (float) doc.Length) < 1e-8)
                    yield nd
            |]
        Ndk(newArray)

and Nkv(array: float array array) =
    member this.Update(invertedDoc: InvertedDoc, zs: Z, numOfTopics: int): Nkv =
        let newArray =
            let newArray2D =
                [|
                    for vocabPositions in invertedDoc.Array do
                        yield
                            vocabPositions
                            |> Seq.map (fun (docIndex, wordIndex) -> zs.[docIndex, wordIndex].P)
                            |> Seq.fold addArray (Array.zeroCreate numOfTopics)
                |]
                |> array2D
            [|
                for k in [ 0 .. Array2D.length2 newArray2D - 1 ] do
                    yield [|
                        for v in [ 0 .. Array2D.length1 newArray2D - 1 ] do
                            yield newArray2D.[v, k]
                    |]
            |]
        Nkv(newArray)
    member this.Array: float array array = array

type InferredVars = {
    alpha: Alpha
    beta: Beta
    thetas: Theta
    phis: Phi
    zs: Z
}

let converges count = count > 10000

let alphaLowerBound (alpha: Alpha) (ndk: Ndk) =
    [
        for nd in ndk.Array do
            let a = SpecialFunctions.GammaLn(Array.sum alpha.Array)
            let b = SpecialFunctions.GammaLn(Array.sum (addArray nd alpha.Array))
            let c =
                [ for (n, a) in Array.zip nd alpha.Array -> SpecialFunctions.GammaLn(n + a) - SpecialFunctions.GammaLn(a)]
                |> List.sum
            yield a - b + c
    ]
    |> List.sum

let betaLowerBound (beta: Beta) (nkv: Nkv) =
    [
        for nk in nkv.Array do
            let a = SpecialFunctions.GammaLn(Array.sum beta.AsArray)
            let b = SpecialFunctions.GammaLn(Array.sum (addArray nk beta.AsArray))
            let c =
                [
                    for (n, b) in Array.zip nk beta.AsArray do
                        yield SpecialFunctions.GammaLn(n + b) - SpecialFunctions.GammaLn(b)
                ]
                |> List.sum
            yield a + c - b
    ]
    |> List.sum

let update (documents: Doc array, invertedDoc: InvertedDoc, numOfTopics: int) ((count: int), (zs: Z, ndk: Ndk, nkv: Nkv, thetas: Theta, phis: Phi, alpha: Alpha, beta: Beta)) =
    //log.TraceInformation("Count: {0}", count); log.Flush()
    let lowerBound = alphaLowerBound alpha ndk
    log.TraceInformation("Lower bound: {0}\nAlpha: {1}", lowerBound, alpha); log.Flush()
    if converges count then None
    else
        let zs = zs.Update(documents, thetas, phis)
        let ndk = ndk.Update(documents, zs, numOfTopics)
        let nkv = nkv.Update(invertedDoc, zs, numOfTopics)
        let thetas = thetas.Update(alpha, ndk)
        let phis = phis.Update(nkv, beta)
        let alpha = alpha.Update(documents, ndk)
        //let beta = beta.Update(nkv)
        let vars = zs, ndk, nkv, thetas, phis, alpha, beta
        Some(vars, (count + 1, vars))

let infer (documents : Doc array, numOfTopics : int, vocabularySize : int) : InferredVars =
    let invertedDoc : InvertedDoc = InvertedDoc(documents, vocabularySize)
    let docSize = Seq.length documents
    let alpha: Alpha = Alpha([| 100.; 100.; 100. |])
    let beta: Beta = Beta(vocabularySize, 0.5)
    let thetas : Theta = Theta(docSize, (fun _ -> Dirichlet(alpha.Array)))
    let phis : Phi = Phi(numOfTopics, (fun _ -> Dirichlet(beta.AsArray)))
    let zs : Z =
        let numOfWords =
            seq { for d in documents -> (d.Index.Index, Seq.length d.Words) }
            |> Map.ofSeq
            |> (fun map d -> Map.find d map)
        Z(docSize, numOfWords, (fun d _ -> Multinomial(thetas.[DocIndex(d)].Sample(), 1)))
    let ndk : Ndk = Ndk(Array.init docSize (fun _ -> Array.init numOfTopics (fun _ -> 0.))).Update(documents, zs, numOfTopics)
    let nkv : Nkv = Nkv(Array.init numOfTopics (fun _ -> Array.init vocabularySize (fun _ -> 0.))).Update(invertedDoc, zs, numOfTopics)
    let (zs, ndk, nkv, thetas, phis, alpha, beta) =
        (0, (zs, ndk, nkv, thetas, phis, alpha, beta))
        |> Seq.unfold (update (documents, invertedDoc, numOfTopics))
        |> Seq.last
    { alpha = alpha; beta = beta; thetas = thetas; phis = phis; zs = zs }
