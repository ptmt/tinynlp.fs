module Samples

//open TinyNLP.Tokenizer
open TinyNLP.Synonyms

let input_string = "Я хочу сказать что-нибудь о Рио-де-Жанейро и о г.Бердске"

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "%A Ellapsed Time: %f ms" (f.GetType().ToString()) timer.Elapsed.TotalMilliseconds
    returnValue 



let sampleTinyNLPSynonyms = 
    let word1 = "слово"
    let word2 = "дело"
    printfn "synonyms for %A is %A" word1 (getSynonyms word1)
    printfn "synonyms for %A is %A" word2 (getSynonyms word2)    
    
let tokenizeSample = 
    let r = TinyNLP.Tokenizer.tokenize input_string
    match r with
        | None -> ["Error"]
        | _ -> r.Value

duration (fun () -> sampleTinyNLPSynonyms) |> ignore

printfn "%A" (duration (fun () -> tokenizeSample))