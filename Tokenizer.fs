module TinyNLP.Tokenizer

open FParsec

type TokenClass = 
    | Word = 1
    | Comma = 2
    | Dash = 3
    | Other = 100

// just for example
//let skipWords = pstring "Нью-Йорк" <|> pstring "Рио-де-Жанейро"

// fucking simple
//let sentenceDelimiter = pstring ". " <|> pstring "! " <|> pstring "? "
let commasExtractAndTrim (str:string) =
    str.Replace(",", " ,").Trim() //.Replace(" ","")

let tokenChars = manySatisfy (function ' '|'\t'| '\n' -> false | _ -> true) // | '\u0085' | '\u2028' | '\u2029' 

let tokenDelimiter = pchar ' ' <|>  pchar '\u0085' <|> pchar '\u2028' <|> pchar '\u2028' // | '\u2028' | '\u2029'  //pstring " "

let tokens = sepBy tokenChars tokenDelimiter

//let sentences = sepBy tokens sentenceDelimiter |>> 

let tokenizeParser = tokens 

let tokenize (inputText:string)  =
    match run tokenizeParser (commasExtractAndTrim inputText) with
    | Success(result, _, _)   -> 
       //printfn "%A" (result.Item(0).ToCharArray() |> Array.map (fun x -> (int)x))
      //  printfn "%A" (System.Text.Encoding.UTF8.GetBytes result.Head)
        printfn "%A" result
        Some(result)
    | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        None

let tokenClassifier = function
    | "," -> TokenClass.Comma
    | "-" -> TokenClass.Dash
    | _ -> TokenClass.Word
        
      
