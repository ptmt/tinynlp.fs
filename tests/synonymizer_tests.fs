module TinyNLP.Tests.SynonymizerTests

open NUnit.Framework  
open TinyNLP
open TinyNLP.Tests.Helpers


[<Test>]
let ``stemming should make lower case of word`` () =
    TinyNLP.Stemming.Stem "Слово"  |> fun x -> x = "слов" |> shouldBeTrue
 
[<Test>]
let ``synonymizer should not be able to synonymize usual unstemmed word`` () =
    TinyNLP.Synonymizer.getSynonyms "слово" "NOUN" |> fun x -> x.Length = 0 |> shouldBeTrue

[<Test>]
let ``synonymizer should not be able to synonymize unknown word`` () =
    TinyNLP.Synonymizer.getSynonyms "adfadfa23" "NOUN"  |> fun x -> x.Length = 0 |> shouldBeTrue

[<Test>]
let ``synonymizer should be able to synonymize stemmed word`` () =
    TinyNLP.Synonymizer.getSynonyms (TinyNLP.Stemming.Stem "слово") "NOUN" |> fun x -> x.Length > 0 |> shouldBeTrue

[<Test>]
let ``synonymizer should be able to synonymize verb word`` () =
    TinyNLP.Synonymizer.getSynonyms (TinyNLP.Stemming.Stem "пишу") "VERB" |> fun x -> x.Length > 0 |> shouldBeTrue