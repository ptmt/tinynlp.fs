﻿module Samples

//open TinyNLP.Tokenizer
open TinyNLP.Synonymizer


let input_string = "Я хочу сказать что-нибудь прекрасное и возвышенное как о Рио-де-Жанейро так и о г. Бердске"

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    timer.Stop()
    printfn "%A Ellapsed Time: %f ms" (f.GetType().ToString()) timer.Elapsed.TotalMilliseconds
    returnValue 

//let stemSample input = 
//    let r = TinyNLP.Tokenizer.tokenize input_string 
//    match r with
//        | None -> ["Error"]
//        | _ -> r.Value |> List.map (fun x -> TinyNLP.Stemming.Stem x)
//
//let sampleTinyNLPSynonyms = 
//    let word1 = "слово"
//    let word2 = "дело"
//    printfn "synonyms for %A is %A" word1 (getSynonyms (TinyNLP.Stemming.Stem word1))
//    printfn "synonyms for %A is %A" word2 (getSynonyms (TinyNLP.Stemming.Stem word2))    
//    
//let tokenizeSample = 
//    let r = TinyNLP.Tokenizer.tokenize input_string
//    match r with
//        | None -> ["Error"]
//        | _ -> r.Value

//duration (fun () -> sampleTinyNLPSynonyms) |> ignore

//printfn "token %A" (duration (fun () -> tokenizeSample))

//printfn "stem %A" (duration (fun () -> stemSample input_string))

let cor =    
    use f = System.IO.File.OpenText("annot.opcorpora.xml")
    let corpus_data = TinyNLP.POST.Corpus.readCorpus f
    
    let lambdas = TinyNLP.POST.Model.calculateLambdas corpus_data
  //  Util.append_log (sprintf "lambdas: %A" lambdas)
    let sf = TinyNLP.POST.Suffix.buildSuffixTree corpus_data
    let mp = TinyNLP.POST.Word.getWordProbs corpus_data
    let a = TinyNLP.POST.Tagger.viterbi ["<S>";"привет"; "всем";"берегам"; ","; "морям";"</S>"] (corpus_data, sf, mp)
    let b = TinyNLP.POST.Tagger.highestProbabilitySequence a
  //  Util.append_log (sprintf "%A" ( TinyNLP.POST.Tagger.printTagMatrixHashes a))
    Util.append_log (sprintf "answer: %A" b)
    //printfn "%A" sf
    //printfn "%A" mp

let convertSynonyms =
    let dict = Kevo.Core.getDictionary<TinyNLP.Synonymizer.WordItem>
    dict |> Seq.map (fun x -> x.Value.Part) |> Seq.distinct |> printfn "%A"
        

duration (fun () -> convertSynonyms)

System.Console.ReadLine |> ignore