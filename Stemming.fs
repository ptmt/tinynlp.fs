module TinyNLP.Stemming

open System;
open System.Collections.Generic;
open System.Linq;
open System.Text;
open System.Text.RegularExpressions;

let c_vower = "аеиоуыэюя"
let c_perfectiveground = "((ив|ивши|ившись|ыв|ывши|ывшись)|((?<=[ая])(в|вши|вшись)))$"
let c_reflexive = "(с[яь])$"
let c_adjective = "(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|еых|ую|юю|ая|яя|ою|ею)$"
let c_participle = "((ивш|ывш|ующ)|((?<=[ая])(ем|нн|вш|ющ|щ)))$"
let c_verb = "((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ены|ить|ыть|ишь|ую|ю)|((?<=[ая])(ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно)))$"
let c_noun = "(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|и|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$"
let c_rvre = "^(.*?[аеиоуыэюя])(.*)$"
let c_derivational = "[^аеиоуыэюя][аеиоуыэюя]+[^аеиоуыэюя]+[аеиоуыэюя].*(?<=о)сть?$"
let c_eng = "[a-z0-9]"
let c_i = "и$"
let c_ost = "ость?$"
let empty_string = String.Empty
       
let RegexReplace (original:string, regx:string, value:string) =    
    let reg = new Regex(regx)
    let n = reg.Replace(original, value)
    (original.Equals(n), n)

let RegexMatch (original:string, regx:string) =
    let reg = new Regex(regx)
    reg.Match(original)

let RegexMatches (original:string, regx:string) =
    let reg = new Regex(regx, RegexOptions.Multiline);
    reg.Matches(original)

let inline isEnd word = 
    let matches = RegexMatches(word, c_rvre)
    matches.Count < 1

let inline step1 word = 
    let a = RegexReplace (word, c_perfectiveground, empty_string)
    match a with 
        | (false, w) -> w
        | (true, w) -> 
            let b1 = RegexReplace (w, c_reflexive, empty_string) 
            let b2 = RegexReplace(snd(b1), c_adjective, empty_string)
            match b2 with
                | (false, w1) -> snd(RegexReplace(w1, c_participle, empty_string))
                | (true, w1) -> 
                    match RegexReplace(w1, c_verb, empty_string) with
                        | (true, w2) -> snd(RegexReplace(w2, c_noun, empty_string))
                        | (false, w2) -> w2

let inline step2 word = 
    snd(RegexReplace(word, c_i, empty_string))

let inline step3 word =
    let m = RegexMatch(word, c_derivational)
    match m.Success with
        | true -> snd(RegexReplace(word, c_ost, empty_string))
        | _ -> word

let inline step4 word =
     let m = RegexReplace(word, "ь$", empty_string)
     match m with
        | (false, w) -> w
        | (true, w) -> snd(RegexReplace(snd(RegexReplace(w, "ейше?", empty_string)), "нн$", "н"))

let Stem (inword:string) =         
        let word = inword.ToLower().Trim().Replace("ё", "е");   
        //printfn "stemming %A" word // TODO LoggerAgent
        match isEnd word with
            | true -> word
            | false -> (RegexMatches(word, c_rvre).Item 0).Value |> step1 |> step2 |> step3 |> step4 
   