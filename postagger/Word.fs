module TinyNLP.POST.Word

open TinyNLP.POST.Corpus
open System.Collections.Generic

type prinner = Dictionary<int, double>
type mapprobs = Dictionary<string, prinner>

let getWordHandler (corpus_data:CorpusData) = 
    let processLex (probs:mapprobs) (word : KeyValuePair<string,Dictionary<string,int>>) = 

        if probs.ContainsKey(word.Key) then 
            probs.Add(word.Key, new prinner())
        word.Value
         |> Seq.iter (fun x -> 
            // P(w|t) = f(w,t) / f(t)
            probs.[word.Key].Add(x.Value, System.Math.Log (float x.Value) / float corpus_data.Unigrams.[x.Key])) 
        probs

    corpus_data.Lexicon |> Seq.fold (fun a x -> processLex a x) (new mapprobs())
