module TinyNLP.Synonymizer

open ProtoBuf
open Kevo.Store


type LexicalClass =
    | Noun = 1 // существительное
    | Verb = 2 // глагол
    | Adverb = 3 // наречие
    | Adjective = 4 // прилагательное
    | Prepositions = 5 // предлоги
    | Others = 10 

let empty_string = ""

[<ProtoContract(ImplicitFields = ImplicitFields.AllPublic)>]
type WordItem (word : string, wordst : string, suff : string, part : LexicalClass, syn : int array, prefix : string) = class  
    member val Word : string = word with get, set
    member val Wordst : string = wordst with get, set  
    member val Suff : string = suff with get, set 
    member val Part : LexicalClass = part with get, set 
    member val Syn : int array = syn with get, set 
    member val Prefix : string = prefix with get, set     
    new() = WordItem(empty_string, empty_string, empty_string, LexicalClass.Others, [||], empty_string)
    override x.ToString() = x.Word
 end  

let matchTagByPart (tag:string) = 
    match tag with 
        | "ADVB" -> LexicalClass.Adverb    
        | "NOUN" -> LexicalClass.Noun
        | "VERB" | "INFN" -> LexicalClass.Verb
        | "PRTF" | "ADJF" -> LexicalClass.Adjective
        | _ -> LexicalClass.Others


let getSynonyms for_word tag =
    let query (x:WordItem) =
        x.Wordst = for_word
    let getWord id = 
        let witem = Kevo.Store.findById<WordItem> id
        match witem with
            | None -> (empty_string, LexicalClass.Others)
            | _ -> (witem.Value.Word, witem.Value.Part)

    let getRelations (word_ids: int array) =
        word_ids 
        |> Array.map (fun x -> getWord x)         
        |> Array.filter (fun x -> 
            (matchTagByPart tag) = snd x) 
        |> List.ofArray 
        |> List.map (fun x -> fst x)
    Kevo.Store.findByQuery<WordItem> query |> List.collect (fun x -> getRelations x.Syn) 