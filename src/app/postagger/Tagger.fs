﻿module TinyNLP.POST.Tagger

open System.Collections.Generic
open TinyNLP.POST.Corpus
open TinyNLP.POST.Suffix

// tagmatrix entry
type TE = {
        Tag: string;
        Probs: Dictionary<TE, float>;
        BPS: Dictionary<TE, TE>
    }

type TagMatrix = System.Collections.Generic.List<System.Collections.Generic.List<TE>>

   
let emptyTE =  {Tag = ""; Probs = new Dictionary<TE, double>(); BPS = new Dictionary<TE, TE>() }
    
let beam_factor = System.Math.Log (1000.0)

let viterbi 
  (token_list:string list)
  (corpus_data:CorpusData, suffix_tree:SuffixTree, known_words_tags_probs:TinyNLP.POST.Word.mapprobs) 
  
   = 

    let viterbi_token (token:string) (tag_matrix:TagMatrix) (beam:float) = 
        let loopTrigram t2 (tagEntry:KeyValuePair<string, float>) (newEntry: TE) =           
            let highestProbs =
                (t2.Probs |> Seq.fold (fun (highestProb, highestProbBp) t1 -> 
                    if t1.Value < beam then
                        (highestProb, highestProbBp)
                    else
                        let trigram = t1.Key.Tag + TinyNLP.POST.Corpus.delimiter + t2.Tag + TinyNLP.POST.Corpus.delimiter + tagEntry.Key
                        // TODO: Async logger
                        //let timer = new System.Diagnostics.Stopwatch()    
                        //timer.Start()  
                        let tProb = TinyNLP.POST.Model.trigramProb trigram corpus_data
                        //printfn "tProb %A in %f ms" token timer.Elapsed.TotalMilliseconds
                        
                        let prob = tProb + t1.Value + tagEntry.Value
                        if prob > highestProb then
                            (prob, t1.Key)
                        else 
                            (highestProb, highestProbBp)
                    )
                    (System.Double.NegativeInfinity, emptyTE) )      
            //printfn "beam = %A" beam    
            newEntry.Probs.Add(t2, fst highestProbs)
            newEntry.BPS.Add(t2, snd highestProbs)
            (newEntry, fst highestProbs)

        let loopentries (tagEntry:KeyValuePair<string, float>) (chp:float) (current_index:int) = 
                
                let j = if current_index > 1 then current_index - 1 else 0
                let new_entry, columnHighestProbCandidate = 
                    Seq.fold (fun a t2 -> loopTrigram t2 tagEntry (fst a)) 
                        ({Tag = tagEntry.Key; Probs = new Dictionary<TE, float>(); BPS = new Dictionary<TE, TE>()}, chp) 
                        tag_matrix.[j]   
                //printfn "columnHighestProbCandidate %A" columnHighestProbCandidate
                //let c = current_index - 1
                tag_matrix.[current_index].Add(new_entry)
                columnHighestProbCandidate
        let columnHighestProb = System.Double.NegativeInfinity        
        let tagProbs =            
            let a = known_words_tags_probs.TryGetValue(token.ToLower())
            //printfn "%A for %A" a token
            match a with 
                | (false, _) -> TinyNLP.POST.Word.getProbsBySuffix (token.ToLower()) suffix_tree corpus_data
                | (true, tag) -> tag
        let printAndNext ccc = 
            //printfn "probs: %A" ccc
            ccc                            
        
        tag_matrix.Add(new List<TE>())
        let current_index = tag_matrix.Count - 1
        //printfn "%A / %A" current_index token_list.Length
        let t = tagProbs |> printAndNext |> Seq.fold (fun (candidate:float) tagEntry ->  loopentries tagEntry candidate current_index) columnHighestProb
        t - beam_factor    
        
    let theta = suffix_tree.Theta
    let beam_start = 0.0      
    let tag_matrix = new TagMatrix()
    // --
    let timer = new System.Diagnostics.Stopwatch()    
    timer.Start()   
    // ---
    tag_matrix.Add(new List<TE>())
    let first_entry = {Tag = token_list.[0]; Probs = new Dictionary<TE, double>(); BPS = new Dictionary<TE, TE>() }
    tag_matrix.[0].Add(first_entry)
    tag_matrix.Add(new List<TE>())
    let bpsinit = new Dictionary<TE, TE>()
    let probsinit = new Dictionary<TE, double>()
    bpsinit.Add (first_entry, emptyTE)
    probsinit.Add(first_entry, 0.0)    
    tag_matrix.[1].Add({Tag = token_list.[0]; Probs = probsinit; BPS = bpsinit })
    token_list |> List.tail |> List.fold (fun beam x -> viterbi_token x tag_matrix beam) beam_start |> ignore    
    printfn "viterbi in %f ms" timer.Elapsed.TotalMilliseconds
    tag_matrix


let highestProbabilitySequence (tag_matrix:TagMatrix) = 
    let printTEhash (te:TE) = 
        string (te.GetHashCode())
    let a1, b1, c1 =         
        Seq.fold (fun (b:TE option, f:TE option, hp:float) (x:TE) -> 
            Seq.fold (fun (before, tail, h) (y:KeyValuePair<TE,float>) -> if y.Value > hp then  (Some x, Some y.Key, y.Value)  else (before, tail, h)) (b, f, hp) x.Probs ) 
            (None, None, System.Double.NegativeInfinity )
            (tag_matrix.[tag_matrix.Count - 1] )

    let rec constructSequence tagSeq (tail:TE) (before:TE option) =         
        let newa = if tail.Tag <> "" then tagSeq @ [tail.Tag] else tagSeq
        if before.IsSome then 
            
            let new_before_tail =
                if tail.BPS <> null && tail.BPS.ContainsKey(before.Value)  then
                    Some tail.BPS.[before.Value]
                else None
            
            constructSequence newa before.Value new_before_tail
        else
            newa

    
    //printfn "a1 = %A" (printTEhash a1.Value)
   
    (constructSequence [] (a1.Value) b1) |> List.rev |> List.tail




let printTagMatrix (tag_matrix: TagMatrix) =    
    let rec printTE (te:TE) (tab:string) = 
        let tag = "\n" + tab + (sprintf "tag: %A" te.Tag)
        let bps:string = 
            if te.BPS.Count > 0 then
                "\n" + tab + "[" + Seq.fold (fun a (x:KeyValuePair<TE,TE>) -> a + printTE x.Key (tab+"\t") + "\n" + tab + ": " + printTE x.Value (tab+"\t") + "\n" + tab + "]") "" te.BPS 
            else
                "\n" + tab + "\tbps empty"
        let probs:string = 
            if te.Probs.Count > 0 then
                "\n" + tab + "\tprobs:[" +  Seq.fold (fun a (x:KeyValuePair<TE,float>) -> a + "key: " + x.Key.Tag + " value: " + (string x.Value)) "" te.Probs + "]"
            else "\n" + tab + "\tprobs empty"
        (tag + bps)
    let empty_string = ""
    Seq.fold (fun s x -> s + "\t" + Seq.fold (fun a y -> a +  (printTE y "\t")) empty_string x) empty_string  tag_matrix

let printTagMatrixHashes (tag_matrix: TagMatrix) = 
    let printTEhash (te:TE) = 
        string (te.GetHashCode())
    let rec printTE (te:TE) (tab:string) = 
        let tag = "\n" + tab + printTEhash te
        let bps:string = 
            if te.BPS.Count > 0 then
                "\n" + tab + "[" + Seq.fold (fun a (x:KeyValuePair<TE,TE>) -> a + printTE x.Key (tab+"\t") + "\n" + tab + ": " + printTE x.Value (tab+"\t") + "\n" + tab + "]") "" te.BPS 
            else
                "\n" + tab + "\tbps empty"
        let probs:string = 
            if te.Probs.Count > 0 then
                "\n" + tab + "\tprobs:[" +  Seq.fold (fun a (x:KeyValuePair<TE,float>) -> a + "key: " + x.Key.Tag + " value: " + (string x.Value)) "" te.Probs + "]"
            else "\n" + tab + "\tprobs empty"
        (tag + bps)
    let empty_string = ""
    Seq.fold (fun s x -> s + "\t" + Seq.fold (fun a y -> a +  (printTE y "\t")) empty_string x) empty_string  tag_matrix