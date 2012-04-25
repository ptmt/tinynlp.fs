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

let cardinalPattern = "^([0-9]+)|([0-9]+\\.)|([0-9.,:-]+[0-9]+)|([0-9]+[a-zA-Z]{1,3})$"
let MAX_LENGTH = 10
let MAX_SUFFIX_LENGTH = 2

let reverseStr str =       
    let s = (string str).ToCharArray() |> Array.rev     
    if s.Length > MAX_SUFFIX_LENGTH then
        (new string(s)).Substring(0, MAX_SUFFIX_LENGTH)
    else
        new string(s)

let mergedict (dict1:dict, dict2:dict) = 
    let mergein (a:dict) (x:KeyValuePair<string,int>) = 
        if a.ContainsKey (x.Key) then
            a.[x.Key] <- x.Value + a.[x.Key]
            a
        else
            filldict (a, x.Key, x.Value)

    dict1 |> Array.ofSeq |> Array.fold mergein dict2      
    
let getEmptySuffixTree (unigrams:Dictionary<string,int>, theta, maxLength) = 
    let filldict (dict:Dictionary<string, int>, a:string, b:int) = 
        dict.Add(a, b)
        dict

    let getInitRoot = 
        let dict = unigrams |> Seq.fold (fun accum x -> filldict (accum, x.Key, x.Value)) (new Dictionary<string, int>())
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

//let printSuffixTree (stree: SuffixTree) = 
//    let level2str level = 
//        if level = 0 then ""
//        else
//            [0..level] |> List.fold (fun a x -> a + "\t") ""
//
//    let rec loop (node: TreeNode Tree, level:int) = 
//        match node with 
//            | Node (a, []) -> "\n" + level2str level + string (chOfTreeNode a) + " freq:" + string (freqOfTreeNode a)
//            | Node (a, xs) -> xs |> List.fold (fun acc x -> acc + loop (x, level + 1)) ""
//    loop (stree.Root, 0)
    
let buildSuffixTree (corpus_data:CorpusData) = 
        let caclulateTheta =
            let pAvg = 1.0 / float corpus_data.Unigrams.Count
            let freqSum = corpus_data.Unigrams.Values |> Seq.sum
            //printfn "%A" freqSum
            let stdDevSum = corpus_data.Unigrams |> Seq.fold (fun sum x -> sum + System.Math.Pow( (float x.Value/ float freqSum), 2.0)) 0.0
            //printfn "%A" stdDevSum
            System.Math.Sqrt (stdDevSum / float (corpus_data.Unigrams.Count - 1))

        let wordProc (lex:string, tags:Dictionary<string, int>, suffix_tree:SuffixTree) = 
           // Util.append_log (sprintf "lex: %A" lex)
            let wordFreq = tags.Values |> Seq.fold (fun a x -> a + x) 0
            let reverse = reverseStr lex
            {Unigrams = suffix_tree.Unigrams; Tree = Some(addSuffix (suffix_tree.Tree, reverse, tags)); MaxLength = MAX_LENGTH; Theta = suffix_tree.Theta}

  
        let theta = caclulateTheta         
        let suffix_tree = getEmptySuffixTree (corpus_data.Unigrams, theta, 10)
       // Util.append_log (sprintf "%A" (printSuffixTree (suffix_tree)))
        let news = corpus_data.Lexicon |>  Seq.fold (fun a x -> wordProc ((string x.Key), x.Value, a)) suffix_tree
       // Util.append_log (sprintf "%A" (printSuffixTree (news)))
        news

let suffixTagProbs (word:string) (suffix_tree:SuffixTree) = 
    let _suffixTagProbs (lex:string) (Dictionary<int, double>) = 
        printfn "adf"
        
    let reverse = reverseStr lex
    _suffixTagProbs reverse (new Dictionary<int, double>())
//
//		if (reverseWord.length() > d_maxLength)
//			reverseWord = reverseWord.substring(0, d_maxLength);
//
//		return d_root.suffixTagProbs(reverseWord, new HashMap<Integer, Double>());