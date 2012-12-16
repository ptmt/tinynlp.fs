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
        

    //let corpus_size = Seq.fold (fun accum x -> accum + x) 0 corpus.Unigrams.Values 
    let ls = corpus.Trigrams |> Seq.fold (fun accum x -> sumTuples (processTrigram (string x.Key, x.Value) corpus.Size) accum) (0, 0, 0)
    normalizeLambdas ls

let trigramProb (trigram:string) (corpus_data:CorpusData) = 
    //let corpus_size = Seq.fold (fun accum x -> accum + x) 0 corpus_data.Unigrams.Values 
    let t1t2t3 = trigram.Split [|delimeterChar|]
    let t3 = t1t2t3.[2]
    let t3prob = if corpus_data.Unigrams.ContainsKey(t3) then float corpus_data.Unigrams.[t3] / float corpus_data.Size else 0.0
    //printfn "t3prob = %A" t3prob
    let t2t3 = t1t2t3.[1] + TinyNLP.POST.Corpus.delimiter + t1t2t3.[2]
    let t2t3prob = 
        if corpus_data.Unigrams.ContainsKey(t1t2t3.[1]) && corpus_data.Bigrams.ContainsKey(t2t3) then
            float corpus_data.Bigrams.[t2t3] / float corpus_data.Unigrams.[t1t2t3.[1]] 
        else
            0.0
    //printfn "t2t3prob = %A" t2t3prob
    let t1t2 = t1t2t3.[0] + TinyNLP.POST.Corpus.delimiter + t1t2t3.[1]
    let t1t2t3prob = 
        if corpus_data.Trigrams.ContainsKey(trigram) && corpus_data.Bigrams.ContainsKey(t1t2) then
            float corpus_data.Trigrams.[trigram] / float corpus_data.Bigrams.[t1t2]
        else 
            0.0
    //printfn "t1t2t3prob = %A" t1t2t3prob
    let lambda1, lambda2, lambda3 = Kevo.Store.memo<(float*float*float)> (fun () -> calculateLambdas corpus_data)
    System.Math.Log (lambda1 * t3prob + lambda2 * t2t3prob + lambda3 * t1t2t3prob)
//public double triGramProb(TriGram triGram) {
//		// If we have cached the likelihood for this trigram, return it.
//		if (d_triGramCache.containsKey(triGram))
//			return d_triGramCache.get(triGram);
//
//		// Unigram likelihood P(t3)
//		UniGram t3 = new UniGram(triGram.t3());
//		double uniGramProb = d_uniGramFreqs.get(t3) / (double) d_corpusSize;
//
//		// Bigram likelihood P(t3|t2).
//		BiGram t2t3 = new BiGram(triGram.t2(), triGram.t3());
//		UniGram t2 = new UniGram(triGram.t2());
//		double biGramProb = 0.0;
//		if (d_uniGramFreqs.containsKey(t2) && d_biGramFreqs.containsKey(t2t3))
//			biGramProb = d_biGramFreqs.get(t2t3) / (double) d_uniGramFreqs.get(t2);
//
//		// Trigram likelihood P(t3|t1,t2).
//		BiGram t1t2 = new BiGram(triGram.t1(), triGram.t2());
//		double triGramProb = 0.0;
//		if (d_biGramFreqs.containsKey(t1t2) && d_triGramFreqs.containsKey(triGram))
//			triGramProb = d_triGramFreqs.get(triGram) / (double) d_biGramFreqs.get(t1t2);
//
//		double prob = Math.log(d_l1 * uniGramProb + d_l2 * biGramProb +
//			d_l3 * triGramProb);
//
//		d_triGramCache.put(triGram, prob);
//
//		return prob;	
//	}