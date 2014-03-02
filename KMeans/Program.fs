module KMeans

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions

let undefined () = System.NotImplementedException () |> raise
let kMenas (k: int) (data: DenseVector seq) (dim: int): DenseVector seq =
    let initialMeans: DenseVector seq =
        let max = Seq.map (fun (x: DenseVector) -> x.AbsoluteMaximum()) data |> Seq.max
        Seq.init k (fun _ -> DenseVector.randomCreate dim (new ContinuousUniform(-max, max)))
    let distance (x: DenseVector) (y: DenseVector): float =
        let d = x - y
        System.Math.Sqrt(d * d)
    let cluster (means: DenseVector seq): (int * DenseVector seq) seq =
        let nearest (x: DenseVector): int =
            Seq.mapi (fun i mean -> (i, distance x mean)) means
            |> Seq.minBy (fun (_, d) -> d)
            |> fst
        Seq.groupBy nearest data
    let updateMeans (oldMeans: DenseVector seq): DenseVector seq =
        let mean (data: DenseVector seq): DenseVector =
            let sum = Seq.fold (+) (DenseVector.zeroCreate dim) data
            sum / ((float) (Seq.length data))
        cluster oldMeans
        |> Seq.map (fun (_, data) -> mean data)
    let converges (oldMeans: DenseVector seq) (newMeans: DenseVector seq): bool =
        cluster oldMeans = cluster newMeans
    let rec search oldMeans =
        let newMeans = updateMeans initialMeans
        if converges oldMeans newMeans then newMeans else search newMeans
    search initialMeans