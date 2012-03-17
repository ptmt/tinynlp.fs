module TinyNLP.Tokenizer

open FParsec

type TokenType = 
    | Word
    | Other



// just for example
//let skipWords = pstring "Нью-Йорк" <|> pstring "Рио-де-Жанейро"

// fucking simple
//let sentenceDelimiter = pstring ". " <|> pstring "! " <|> pstring "? "

let tokenChars = manySatisfy (function ' '|'\t'| '\n' -> false | _ -> true) 

let tokenDelimiter = pstring " " 

let tokens = sepBy tokenChars tokenDelimiter

let t = pipe2

//let sentences = sepBy tokens sentenceDelimiter |>> 

let tokenizeParser = tokens //|>> (fun x -> printfn "%A" x)

let tokenize (inputText:string)  =
    match run tokenizeParser inputText with
    | Success(result, _, _)   -> 
        Some(result)
    | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        None
        
      
