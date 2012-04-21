module TinyNLP.POST.Model

open TinyNLP.POST.Corpus

let inline processTrigram index trigram = 
    let t1t2t3 = string (fst trigram)
    let t1t2 = (t1t2t3.Split [|delimiter|]).[0] + " " + (t1t2t3.Split [|' '|]).[1]
    printfn "%A" t1t2t3
    
let calculateLambdas (corpus:CorpusData) = 
    let corpus_size = Seq.fold (fun accum x -> accum + x) 0 corpus.Unigrams.Values 
    corpus.Trigrams |> Seq.mapi processTrigram