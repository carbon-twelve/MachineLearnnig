module VariationalBayes

open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open System
open System.Collections.Generic
open System.Diagnostics

//let undefined() = raise (System.NotImplementedException())
let psi = SpecialFunctions.DiGamma
let addArray (array1: float array) (array2: float array): float array = Array.zip array1 array2 |> Array.map(fun (a, b) -> a + b)

let log = 
    let log = TraceSource("VariationalBayes")
    use consoleListener = new ConsoleTraceListener()
    log.Listeners.Add(consoleListener) |> ignore
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
    member this.Words: Word seq = Seq.indexed doc |> Seq.map(fun (i, v) -> Word(WordIndex(i), v))
    member this.Index: DocIndex = docIndex
    member this.WordIndexRange: WordIndex seq = Seq.map (fun i -> WordIndex(i)) [ 0..this.Length - 1 ]

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
    
    member this.Item(vocabIndex: VocabIndex): (DocIndex * WordIndex) list = invertedDoc.[vocabIndex.Index]
    member this.Array = invertedDoc

type Alpha(alphaArray: float array) = 
    new(numOfTopics: int, initValue: float) = new Alpha(Array.create numOfTopics initValue)
    override this.ToString() = sprintf "%A" alphaArray
    member this.Array = alphaArray
    member this.Update(documents: Doc array, ndk: Ndk): Alpha = 
        let sumAlpha: float = Array.sum alphaArray
        
        let newAlphaArray = 
            let b = 
                documents
                |> Array.map(fun doc -> psi((float) doc.Length + sumAlpha) - psi(sumAlpha))
                |> Array.sum
            
            let aArray = 
                [| for k in [ 0..alphaArray.Length - 1 ] do
                       yield List.sum [ for (nk: float array) in ndk.Array -> (psi(nk.[k] + alphaArray.[k]) - psi(alphaArray.[k])) * alphaArray.[k] ] |]
            
            let newAlphaArray = Array.map (fun a -> a / b) aArray
            assert (Array.forall (fun a -> not(Double.IsNaN a) && a > 0.) newAlphaArray)
            newAlphaArray
        Alpha(newAlphaArray)

and Beta(vocabSize: int, eta: float) = 
    let array = Array.create vocabSize eta
    override this.ToString() = sprintf "%A" array
    member this.Value: float = eta
    member this.AsArray: float array = array
    member this.Update(nkv: Nkv): Beta = 
        let a = 
            seq { 
                for nk in nkv.Array do
                    for n in nk do
                        yield (psi(n + eta) - psi(eta)) * eta
            }
            |> Seq.sum
        
        let b = 
            seq { 
                for nk in nkv.Array do
                    let vSum = Seq.sum nk
                    yield psi(vSum + eta) - psi((float) vocabSize * eta)
            }
            |> Seq.sum
        
        let newBeta = a / b / (float) vocabSize
        assert (newBeta > 0.)
        Beta(vocabSize, newBeta)

and ThetaPosterior(gamma: float array) =
    new (alpha: Alpha, expectedTopicCount: float array) = ThetaPosterior(addArray expectedTopicCount alpha.Array)
    override this.ToString() = sprintf "%A" gamma
    member this.Distribution = Dirichlet(gamma)
    member this.Gamma = gamma

and ThetaPosteriors(array: ThetaPosterior array) =
    new(docSize: int, init: int -> ThetaPosterior) = ThetaPosteriors(Array.init docSize init)
    new(alpha: Alpha, ndk: Ndk) =
        let array =
            ndk.Array
            |> Array.map (fun expectedTopicCount -> async { return ThetaPosterior(alpha, expectedTopicCount) } )
            |> Async.Parallel
            |> Async.RunSynchronously
        ThetaPosteriors(array)
    override this.ToString() = sprintf "%A" array
    member this.Array = array

and PhiPosterior(lambda: float array) =
    new (beta: Beta, expectedVocabularyCount: float array) = PhiPosterior(addArray beta.AsArray expectedVocabularyCount)
    override this.ToString() = sprintf "%A" lambda
    member this.Distribution = Dirichlet(lambda)
    member this.Lambda = lambda

and PhiPosteriors(array: PhiPosterior array) = 
    new(numOfTopics, init: int -> PhiPosterior) = PhiPosteriors(Array.init numOfTopics init)
    new(beta: Beta, nkv: Nkv) =
        let array =
            nkv.Array
            |> Array.map (fun expectedVocabularyCount -> async { return PhiPosterior(beta, expectedVocabularyCount) } )
            |> Async.Parallel
            |> Async.RunSynchronously
        PhiPosteriors(array)
    override this.ToString() = sprintf "%A" array
    member this.Array = array

and ZPosterior = Multinomial

and ZPosteriors(array: ZPosterior array array) = 
    new(docSize: int, numOfWords: int -> int, init: int -> int -> ZPosterior) = ZPosteriors(Array.init docSize (fun d -> Array.init (numOfWords d) (fun i -> init d i)))
    new (documents: Doc array, thetas: ThetaPosteriors, phis: PhiPosteriors) = 
        let wordUpdateAsync (thetaPosterior: ThetaPosterior) (doc: Doc) (word: Word): Async<ZPosterior> =
            async {
                let multParam =
                    Array.zip phis.Array thetaPosterior.Gamma
                    |> Array.map (fun (phiPosterior: PhiPosterior, thetaPosteriorAlpha: float) ->
                            let a = psi(phiPosterior.Lambda.[word.VocabIndex.Index])
                            let b = psi(Array.sum phiPosterior.Lambda)
                            let c = psi(thetaPosteriorAlpha)
                            let d = psi(Array.sum thetaPosterior.Gamma)
                            let e' = (a + c) - (b + d)
                            e'
                    )
                let normalizedMultParam =
                    let max = Array.max multParam
                    let scaledMultParam = Array.map (fun e -> Math.Exp(e - max)) multParam
                    Array.map (fun p -> p / Array.sum scaledMultParam) scaledMultParam
                if not (Array.forall (fun e -> e >= 0.) normalizedMultParam) then
                    failwith (sprintf "Error:\nnormalized mult param: %Amult param: %A" normalizedMultParam multParam)
                return Multinomial(normalizedMultParam, 1)
            }
        let docUpdateAsync (doc: Doc, thetaPosterior: ThetaPosterior): Async<ZPosterior array> =
            async {            
                return! doc.Words
                |> Seq.map (wordUpdateAsync thetaPosterior doc)
                |> Async.Parallel
            }
        let array =
            Array.zip documents thetas.Array
            |> Array.map docUpdateAsync
            |> Async.Parallel
            |> Async.RunSynchronously
        ZPosteriors(array)
    override this.ToString() = sprintf "%A" (Array.map (fun (e: ZPosterior array) -> Array.map (fun (e': ZPosterior) -> e'.P) e) array)
    member this.Array = array

    
and Ndk(array: float array array) = 
    new (zs: ZPosteriors, numOfTopics: int) = 
        let array = 
            zs.Array
            |> Array.map (fun (zd: ZPosterior array) ->
                async {
                    return zd
                    |> Array.map (fun zdi -> zdi.P)
                    |> Array.fold addArray (Array.zeroCreate numOfTopics)
                    //assert (Math.Abs(Array.sum nd - (float) doc.Length) < 1e-8)
                })
            |> Async.Parallel
            |> Async.RunSynchronously
        Ndk(array)
    member this.Array: float array array = array

and Nkv(array: float array array) = 
    new (invertedDoc: InvertedDoc, zs: ZPosteriors, numOfTopics: int) = 
        let newArray2D =
            [|
                for vocabPositions in invertedDoc.Array do
                    yield async {
                        return vocabPositions
                        |> Seq.map(fun (docIndex, wordIndex) -> zs.Array.[docIndex.Index].[wordIndex.Index].P)
                        |> Seq.fold addArray (Array.zeroCreate numOfTopics)
                    }
            |]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> array2D
        let array =
            [|
                for k in [ 0..Array2D.length2 newArray2D - 1 ] do
                    yield async {
                        return
                            [|
                                for v in [ 0..Array2D.length1 newArray2D - 1 ] do
                                yield newArray2D.[v, k]
                            |]
                    }
            |]
            |> Async.Parallel
            |> Async.RunSynchronously
        Nkv(array)
    member this.Array: float array array = array

type InferredVars = 
    { alpha: Alpha
      beta: Beta
      thetas: ThetaPosteriors
      phis: PhiPosteriors
      zs: ZPosteriors }

let converges count = count >= 100

let alphaLowerBound (alpha: Alpha) (ndk: Ndk) = 
    [ for nd in ndk.Array do
          let a = SpecialFunctions.GammaLn(Array.sum alpha.Array)
          let b = SpecialFunctions.GammaLn(Array.sum(addArray nd alpha.Array))
          
          let c = 
              [ for (n, a) in Array.zip nd alpha.Array -> SpecialFunctions.GammaLn(n + a) - SpecialFunctions.GammaLn(a) ]
              |> List.sum
          yield a - b + c ]
    |> List.sum

let betaLowerBound (beta: Beta) (nkv: Nkv) = 
    [ for nk in nkv.Array do
          let a = SpecialFunctions.GammaLn(Array.sum beta.AsArray)
          let b = SpecialFunctions.GammaLn(Array.sum(addArray nk beta.AsArray))
          
          let c = 
              [ for (n, b) in Array.zip nk beta.AsArray do
                    yield SpecialFunctions.GammaLn(n + b) - SpecialFunctions.GammaLn(b) ]
              |> List.sum
          yield a + c - b ]
    |> List.sum

let calculateLikelihood(doc: Doc, zs: ZPosterior array, theta: ThetaPosterior, phis: PhiPosterior array, alpha: Alpha, beta: Beta): float =
    let a = SpecialFunctions.GammaLn(Array.sum alpha.Array)
    let b = - (alpha.Array |> Array.map (fun a -> SpecialFunctions.GammaLn(a)) |> Array.sum)
    let c = + (Array.zip alpha.Array theta.Gamma |> Array.map (fun (a, g) -> (a - 1.) * (psi(g) - psi(Array.sum theta.Gamma))) |> Array.sum)
    let d = + (zs |> Array.map (fun z -> (Array.zip z.P theta.Gamma |> Array.map (fun (i, g) -> i * (psi(g) - psi(Array.sum theta.Gamma))) |> Array.sum)) |> Array.sum)
    let e =
        [|
            for n in [0 .. doc.Length - 1] do
                for i in [0 .. alpha.Array.Length - 1] do
                    yield zs.[n].P.[i] * Math.Log(phis.[i].Lambda.[(Seq.item n doc.Words).VocabIndex.Index])
        |]
        |> Array.sum
    let f = - SpecialFunctions.GammaLn(Array.sum theta.Gamma)
    let g = + (theta.Gamma |> Array.map (fun g -> SpecialFunctions.GammaLn(g)) |> Array.sum)
    let h = - (theta.Gamma |> Array.map (fun g -> (g - 1.) * (psi(g) - psi(Array.sum theta.Gamma))) |> Array.sum)
    let i =
        -(
            [|
                for z in zs do
                    for i in z.P do
                        if i > 0. then yield i * Math.Log(i)
                        else yield 0.
            |]
            |> Array.sum
        )
    log.TraceInformation(sprintf "a:%f b:%f c:%f d:%f e:%f f:%f g:%f h:%f i:%f" a b c d e f g h i)
    a + b + c + d + e + f + g +  h + i

let update (documents: Doc array, invertedDoc: InvertedDoc, numOfTopics: int) ((count: int), (zs: ZPosteriors, ndk: Ndk, nkv: Nkv, thetas: ThetaPosteriors, phis: PhiPosteriors, alpha: Alpha, beta: Beta)) = 
    //let lowerBound = alphaLowerBound alpha ndk
    //log.TraceInformation("Lower bound: {0}\nAlpha: {1}", lowerBound, alpha)
    //log.Flush()
    log.TraceInformation("Iteration {0}", count)
    //log.TraceInformation("Z_1_0: {0}", sprintf "%A" zs.Array.[1].[0].P)
    //log.TraceInformation("Theta_1: {0}", sprintf "%A" thetas.Array.[1].Gamma)
    //log.TraceInformation("Phi_0: {0}", sprintf "%A" (Array.indexed phis.Array.[0].Lambda |> Array.sortByDescending (fun (_, l) -> l) |> Array.take 10))
    //log.TraceInformation("Phi_1: {0}", sprintf "%A" (Array.indexed phis.Array.[1].Lambda |> Array.sortByDescending (fun (_, l) -> l) |> Array.take 10))
    //log.TraceInformation("Ndk_1: {0}", sprintf "%A" ndk.Array.[1])
    log.TraceInformation("Likelihood: {0}", calculateLikelihood(documents.[1], zs.Array.[1], thetas.Array.[1], phis.Array, alpha, beta))
    log.Flush()
    if converges count then None
    else
        log.TraceInformation("Updating Z"); log.Flush()
        let zs = ZPosteriors(documents, thetas, phis)
        log.TraceInformation("Updating Ndk"); log.Flush()
        let ndk = Ndk(zs, numOfTopics)
        log.TraceInformation("Updating Nkv"); log.Flush()
        let nkv = Nkv(invertedDoc, zs, numOfTopics)
        log.TraceInformation("Updating Theta"); log.Flush()
        let thetas = ThetaPosteriors(alpha, ndk)
        log.TraceInformation("Updating Phi"); log.Flush()
        let phis = PhiPosteriors(beta, nkv)
        //let alpha = alpha.Update(documents, ndk)
        log.TraceInformation("Update completed"); log.Flush()
        let vars = zs, ndk, nkv, thetas, phis, alpha, beta
        Some(vars, (count + 1, vars))

let infer(documents: Doc array, numOfTopics: int, vocabularySize: int): InferredVars = 
    let invertedDoc: InvertedDoc = InvertedDoc(documents, vocabularySize)
    let docSize = Seq.length documents
    let alpha: Alpha = Alpha(SystemRandomSource.Doubles(numOfTopics, 1000))
    let beta: Beta = Beta(vocabularySize, 0.001)
    let thetas: ThetaPosteriors =
        let init d =
            ThetaPosterior(addArray alpha.Array (SystemRandomSource.Doubles(numOfTopics, d)))
        ThetaPosteriors(docSize, init)
    let phis: PhiPosteriors = PhiPosteriors(numOfTopics, (fun i -> PhiPosterior(addArray beta.AsArray (SystemRandomSource.Doubles(vocabularySize, i)))))
    let zs: ZPosteriors =
        let dir = Dirichlet(Array.create numOfTopics 1.)
        let numOfWords d = documents.[d].Length
        ZPosteriors(docSize, numOfWords, (fun d i ->
            Multinomial(dir.Sample(), 1)))
    let ndk: Ndk = Ndk(zs, numOfTopics)
    let nkv: Nkv = Nkv(invertedDoc, zs, numOfTopics)
    let (zs, ndk, nkv, thetas, phis, alpha, beta) = 
        (0, (zs, ndk, nkv, thetas, phis, alpha, beta))
        |> Seq.unfold(update(documents, invertedDoc, numOfTopics))
        |> Seq.last
    { alpha = alpha
      beta = beta
      thetas = thetas
      phis = phis
      zs = zs }
