module TinyNLP.POST.Model

open TinyNLP.POST.Corpus

// LINEAR INTERPOLATION LANGUAGE MODEL

let delimeterChar = 
    char TinyNLP.POST.Corpus.delimiter

let sumTuples (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

let normalizeLambdas (a:int, b:int, c:int) = 
    (float a / float (a + b + c),
     float b / float (a + b + c), 
     float c / float (a + b + c))
    
let calculateLambdas (corpus:CorpusData) =     
        
    let processTrigram trigram corpus_size = 
        let getl3p b = 
             if corpus.Bigrams.ContainsKey(b) then
                    float (snd trigram - 1) / float (corpus.Bigrams.[b] - 1)
                else 
                    0.0
        let getl2p b u = 
            if corpus.Unigrams.ContainsKey(u) && corpus.Bigrams.ContainsKey(b) then
                float (corpus.Bigrams.[b] - 1) / float (corpus.Unigrams.[u] - 1)
            else
                0.0

        let getl1p u = 
            if corpus.Unigrams.ContainsKey(u) then
                float (corpus.Unigrams.[u] - 1) / float corpus_size
            else
                0.0
           

        let t1t2t3 = string (fst trigram)
        let t1t2 = (t1t2t3.Split [|delimeterChar|]).[0] + TinyNLP.POST.Corpus.delimiter + (t1t2t3.Split [|delimeterChar|]).[1]
        let l3p = getl3p t1t2
        let t2t3 = ((t1t2t3.Split [|delimeterChar|]).[1] + TinyNLP.POST.Corpus.delimiter + (t1t2t3.Split [|delimeterChar|]).[2])
        let l2p = getl2p t2t3 (t1t2t3.Split [|delimeterChar|]).[1]
        let l1p = getl1p (t1t2t3.Split [|delimeterChar|]).[2]
        [
            (l1p, (snd trigram, 0, 0));
            (l2p, (0, snd trigram, 0));
            (l3p, (0, 0, snd trigram))
        ] |> List.maxBy (fun x -> fst x) |> snd      
        

    let corpus_size = Seq.fold (fun accum x -> accum + x) 0 corpus.Unigrams.Values 
    let ls = corpus.Trigrams |> Seq.fold (fun accum x -> sumTuples (processTrigram (string x.Key, x.Value) corpus_size) accum) (0, 0, 0)
    normalizeLambdas ls