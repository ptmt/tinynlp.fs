module TinyNLP.Tokenizer

open FParsec

type TokenClass = 
    | Word = 1
    | Comma = 2
    | Dash = 3
    | Other = 100


let commasExtractAndTrim (str:string) =
    str.Replace(",", " ,").Trim() 

let tokenChars = manySatisfy (function ' '|'\t'| '\n' -> false | _ -> true) 

let tokenDelimiter = pchar ' ' <|>  pchar '\u0085' <|> pchar '\u2028' <|> pchar '\u2028' 

let tokens = sepBy tokenChars tokenDelimiter

//let sentences = sepBy tokens sentenceDelimiter |>> 

let tokenizeParser = tokens 

let tokenize (inputText:string)  =
    match run tokenizeParser (commasExtractAndTrim inputText) with
    | Success(result, _, _)   ->        
        printfn "%A" result
        Some(result)
    | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        None

let tokenClassifier = function
    | "," -> TokenClass.Comma
    | "-" -> TokenClass.Dash
    | _ -> TokenClass.Word
        
      
