module TinyNLP.Tokenizer

open FParsec

type TokenClass = 
    | Word = 1
    | Comma = 2
    | Dash = 3
    | Parenthesis = 4
    | Other = 100


let commasExtractAndTrim (str:string) =
    str.Replace(",", " ,").Trim() 

let tokenChars = manySatisfy (function '/' | '(' | ')' | '|' | ' ' | '-' | '\t'| '\n' -> false | _ -> true) 

let tokenDelimiter = pchar ' ' <|> pchar '-' <|> pchar '\u0085' <|> pchar '\u2028' <|> pchar '\u2028'  <|> pchar '/' <|> pchar '(' <|> pchar ')'

let tokens = sepBy tokenChars tokenDelimiter

//let sentenceDelimiter = pchar '.' <|> pchar '!' <|> pchar '?'

//let spaces = 

//let sentences = sepBy tokens (sentenceDelimiter >> spaces)

let tokenizeParser = tokens 

let tokenize (inputText:string)  =
    match run tokenizeParser (commasExtractAndTrim inputText) with
    | Success(result, _, _)   ->        
        //printfn "%A" result
        Some(result)
    | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        None

let tokenClassifier = function
    | "," -> TokenClass.Comma
    | "-" -> TokenClass.Dash
    | ")" | "(" -> TokenClass.Parenthesis
    | _ -> TokenClass.Word
        
// TODO improve this simplest ever tokenizer      
let splitSentences text = 
    let r = new System.Text.RegularExpressions.Regex("[\.!?]+")//[\w ,]+[\.!?]+")
    r.Split(text)