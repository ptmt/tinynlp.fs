﻿module TinyNLP.POST.Word

open TinyNLP.POST.Corpus
open System.Collections.Generic
open TinyNLP.POST.Suffix

type prinner_dict = Dictionary<string, float>
type mapprobs = Dictionary<string, prinner_dict>

let getWordProbs (corpus_data:CorpusData) = 
    let processLex (probs:mapprobs) (word : KeyValuePair<string,Dictionary<string,int>>) = 

        if probs.ContainsKey(word.Key) then 
            probs.Add(word.Key, new prinner_dict())
        word.Value
         |> Seq.iter (fun x -> 
            // P(w|t) = f(w,t) / f(t)
            probs.[word.Key].Add(x.Key, System.Math.Log (float x.Value) / float corpus_data.Unigrams.[x.Key])) 
        probs

    corpus_data.Lexicon |> Seq.fold (fun a x -> processLex a x) (new mapprobs())

let getProbsBySuffix (token:string) (suffix_tree:SuffixTree) (theta:float) (corpus_data:CorpusData)=    
   let maxTags = 10
   let prinner_e = new prinner_dict();
   suffixTagProbs token suffix_tree theta corpus_data 
    |> Seq.sortBy (fun x -> x.Value) 
    |> Seq.take maxTags
    |> Seq.iter (fun x -> prinner_e.Add(x.Key, x.Value))
   prinner_e
     
   