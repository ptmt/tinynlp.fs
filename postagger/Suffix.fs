module TinyNLP.POST.Suffix

open TinyNLP.POST.Corpus
open System.Collections.Generic

type dict = Dictionary<string, int>

type 'a Tree = Node of 'a  * 'a Tree list

type TreeNode =    
    Dictionary<string, int> * char * int 


type SuffixTree = {
      Unigrams : Dictionary<string, int>;
      Root: TreeNode Tree;
      MaxLength: int;
      Theta: float }


let tagsOfTreeNode (tags, _, _) = tags
let chOfTreeNode (_, ch, _) = ch
let probOfTreeNode (_, _, i) = i

let fstn = function 
    | Node(f, _) -> f

let sndn = function
    | Node(_, s) -> s

let cardinalPattern = "^([0-9]+)|([0-9]+\\.)|([0-9.,:-]+[0-9]+)|([0-9]+[a-zA-Z]{1,3})$"
let MAX_LENGTH = 10
let MAX_SUFFIX_LENGTH = 2

let reverseStr str = 
//    if str = "" then ""
//    else        
    let s = (string str).ToCharArray() |> Array.rev     
    if s.Length > MAX_SUFFIX_LENGTH then
        (new string(s)).Substring(0, MAX_SUFFIX_LENGTH)
    else
        new string(s)

let isChildrensContains (node: TreeNode Tree, ch: char) = 
    match node with
    | Node (_, []) -> false
    | Node ((_, c, _), xs) -> 
        let l = xs |> List.filter (fun x -> c = ch) 
        if l.Length > 0 then true else false
            
    
let getEmptySuffixTree (unigrams:Dictionary<string,int>, theta, maxLength) = 
    let filldict (dict:Dictionary<string, int>, a:string, b:int) = 
        dict.Add(a, b)
        dict

    let getInitRoot = 
        let dict = unigrams |> Seq.fold (fun accum x -> filldict (accum, x.Key, x.Value)) (new Dictionary<string, int>())
        let freq = unigrams |> Seq.fold (fun accum x -> accum + x.Value) 0
        Node((dict, char 0, freq), [])

    {Unigrams = unigrams; Root = getInitRoot; MaxLength = MAX_LENGTH; Theta = theta}

let rec addSuffix (node:TreeNode Tree, suffix:string, tags:dict) =         
    let transitionChar = suffix.ToCharArray().[0]
//    Util.append_log (sprintf "char: %A" transitionChar)
//    if isChildrensContains (node, transitionChar) = false then
//        printfn "no ch"
//    else
//        printfn "add new"
    let childrens = sndn node
    let new_children = Node((new dict(), transitionChar, 10), []) //addSuffix(node, suffix.Substring(1), tags)
    Node (fstn node, (childrens @ [new_children]) )

let printSuffixTree (stree: SuffixTree) = 
    let level2str level = 
        if level = 0 then ""
        else
            [0..level] |> List.fold (fun a x -> a + "\t") ""

    let rec loop (node: TreeNode Tree, level:int) = 
        match node with 
            | Node (a, []) -> "\n" + level2str level + string (chOfTreeNode a) + " freq:" + string (probOfTreeNode a)
            | Node (a, xs) -> xs |> List.fold (fun acc x -> acc + loop (x, level + 1)) ""
    loop (stree.Root, 0)
    
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
            {Unigrams = suffix_tree.Unigrams; Root = addSuffix (suffix_tree.Root, reverse, tags); MaxLength = MAX_LENGTH; Theta = suffix_tree.Theta}

  
        let theta = caclulateTheta         
        let suffix_tree = getEmptySuffixTree (corpus_data.Unigrams, theta, 10)
        Util.append_log (sprintf "%A" (printSuffixTree (suffix_tree)))
        let news = corpus_data.Lexicon |>  Seq.fold (fun a x -> wordProc ((string x.Key), x.Value, a)) suffix_tree
        Util.append_log (sprintf "%A" (printSuffixTree (news)))
        news

