module TinyNLP.POST.Suffix

open TinyNLP.POST.Corpus
open System.Collections.Generic

type dict = Dictionary<string, int>

//type 'a Tree = Node of 'a  * 'a Tree list

type TreeNode =    
    Dictionary<string, int> * int 

type ChTree = {
     Map : Map<char, ChTree> 
     Freqs : Dictionary<string, int>
     TotalFreq : int
   }
 
type TO = ChTree option 
let empty : TO = None


type SuffixTree = {
      Unigrams : Dictionary<string, int>;
      Tree: TO;
      MaxLength: int;
      Theta: float }

let inline private ($) a b = b a
let cardinalPattern = "^([0-9]+)|([0-9]+\\.)|([0-9.,:-]+[0-9]+)|([0-9]+[a-zA-Z]{1,3})$"
let MAX_LENGTH = 10
let MAX_SUFFIX_LENGTH = 2

let inline reverseStr str =       
    let s = (string str).ToCharArray() |> Array.rev     
    if s.Length > MAX_SUFFIX_LENGTH then
        (new string(s)).Substring(0, MAX_SUFFIX_LENGTH)
    else
        new string(s)

let inline filldict (dict:dict) (a:string) (b:int) = 
        dict.Add(a, b)
        dict
           

let mergedict (dict1:dict, dict2:dict) = 
    let mergein (a:dict) (x:KeyValuePair<string,int>) = 
        if a.ContainsKey (x.Key) then
            a.[x.Key] <- x.Value + a.[x.Key]
            a
        else
            filldict a x.Key x.Value

    match dict1, dict2 with
        | (null, _) -> dict2
        | (_, null) -> dict1
        | (_, _) -> dict1 |> Array.ofSeq |> Array.fold mergein dict2      


let getEmptySuffixTree (unigrams:Dictionary<string,int>, theta, maxLength) = 
    let getInitRoot = 
        let dict = unigrams |> Seq.fold (fun accum x -> filldict accum x.Key x.Value) (new Dictionary<string, int>())
        let freq = unigrams |> Seq.fold (fun accum x -> accum + x.Value) 0
        Some {Map = Map.empty; Freqs = dict; TotalFreq = freq}

    {Unigrams = unigrams; Tree = getInitRoot; MaxLength = MAX_LENGTH; Theta = theta}
    

let sumdict (dict:dict) = 
    dict |> Seq.fold (fun accum x -> accum + x.Value) 0

let rec addSuffix (tree:TO, suffix:string, tags:dict) =         
    if suffix.Length = 0 then {Map = Map.empty; Freqs = null; TotalFreq = 0}
    else
        let transitionChar = suffix.ToCharArray().[0]
        match tree with 
                | None -> // пустое дерево в которое нужно добавить и добавление вершины в новую букву (новое сочетание букв)
                    let subNode = addSuffix (None, suffix.Substring(1), tags)
                    let map = Map.empty $ Map.add transitionChar subNode
                    {Map=map; Freqs = tags; TotalFreq = sumdict tags}
                | Some tree -> 
                     match tree.Map.TryFind transitionChar with
                        // такой буквы ещё нет, добавляем по ней поддерево, добавляем её в хэш, возвращаем ноду с новым хэшем, частотами
                        | None ->  let subNode = addSuffix (None, suffix.Substring(1), tags)
                                   let map = tree.Map $ Map.add transitionChar subNode
                                   {Map=map; Freqs = tags; TotalFreq = sumdict tags}
                        // такая буква уже есть, сливаем хэши 
                        | Some subNode ->                                     
                                   let mergedTags = mergedict (tags, subNode.Freqs)
                                   {Map=tree.Map; Freqs = mergedTags; TotalFreq = sumdict mergedTags}

//let emptydict = new dict()
//let f = addSuffix (empty, (reverseStr "плов"), filldict emptydict "a" 2 ) 
//let f2 = addSuffix (Some f, (reverseStr "кров"), filldict emptydict "b" 3) 
//let f3 = addSuffix (Some f2, (reverseStr "срыв"), filldict emptydict "c" 1) 
//f3 |> printfn "%A" 
let caclulateTheta (corpus_data:CorpusData) =
            let pAvg = 1.0 / float corpus_data.Unigrams.Count
            let freqSum = corpus_data.Unigrams.Values |> Seq.sum
            //printfn "%A" freqSum
            let stdDevSum = corpus_data.Unigrams |> Seq.fold (fun sum x -> sum + System.Math.Pow( (float x.Value/ float freqSum), 2.0)) 0.0
            //printfn "%A" stdDevSum
            System.Math.Sqrt (stdDevSum / float (corpus_data.Unigrams.Count - 1)) 
   
let buildSuffixTree (corpus_data:CorpusData) = 
        let wordProc (lex:string, tags:Dictionary<string, int>, suffix_tree:SuffixTree) = 
           // Util.append_log (sprintf "lex: %A" lex)
            let wordFreq = tags.Values |> Seq.fold (fun a x -> a + x) 0
            let reverse = reverseStr lex
            {Unigrams = suffix_tree.Unigrams; Tree = Some(addSuffix (suffix_tree.Tree, reverse, tags)); MaxLength = MAX_LENGTH; Theta = suffix_tree.Theta}  
        let theta = caclulateTheta corpus_data        
        let suffix_tree = getEmptySuffixTree (corpus_data.Unigrams, theta, 10)
       // Util.append_log (sprintf "%A" (printSuffixTree (suffix_tree)))
        let news = corpus_data.Lexicon |>  Seq.fold (fun a x -> wordProc (x.Key.ToLower(), x.Value, a)) suffix_tree
       // Util.append_log (sprintf "%A" (printSuffixTree (news)))
        news

let suffixTagProbs (word:string) (suffix_tree:SuffixTree) (corpus_data:CorpusData)= 
    let bayesianInversion (probs: Dictionary<string, float>) =
        probs |> Seq.fold (fun (a:Dictionary<string, float>) x -> a.Add(x.Key, x.Value / float corpus_data.Unigrams.[x.Key]); a) (new Dictionary<string, float>())

    let rec _suffixTagProbs (lex:string) (probs: Dictionary<string, float>) (node: ChTree)= 
        let tagFreqProcess (p:Dictionary<string, float>) (x:string) = 
            //P(t|reverseSuffix)
            let pt = if node.Freqs <> null && node.Freqs.ContainsKey x then float node.Freqs.[x] / (float node.TotalFreq) else 0.0            
            let a =  if p.ContainsKey x then suffix_tree.Theta * p.[x] else 0.0
            p.[x] <- (a + pt) / (suffix_tree.Theta + 1.0)
            p

        let newprobs = suffix_tree.Tree.Value.Freqs |> Seq.fold (fun p x -> tagFreqProcess p x.Key) probs
        if lex.Length = 0 then
            bayesianInversion probs
        else
            let transitionChar = lex.[0]
            if node.Map.ContainsKey(transitionChar) = false then 
                bayesianInversion probs
            else
                _suffixTagProbs (lex.Substring(1)) newprobs node.Map.[transitionChar]

    let reverse = reverseStr word
    _suffixTagProbs reverse (new Dictionary<string, float>()) suffix_tree.Tree.Value
//

