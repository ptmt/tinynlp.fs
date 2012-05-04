module TinyNLP.POST.Tagger

open System.Collections.Generic
open TinyNLP.POST.Corpus
open TinyNLP.POST.Suffix

// tagmatrix entry
type TE = {
        Tag: string;
        Probs: Dictionary<TE, float>;
        BPS: Dictionary<TE, TE>
    }

type TagMatrix = System.Collections.Generic.List<System.Collections.Generic.List<TE>>

//type HMM (corpus_data : CorpusData) = class
//     
//    let mutable corpus_data = {Lexicon:new Dictionary<string, Dictionary
//
//    member x.CorpusData = corpus_data
//    
//    member x.Train = 
//        use f = System.IO.File.OpenText("annot.opcorpora.xml")
//        corpus_data <- TinyNLP.POST.Corpus.readCorpus f    
// 
//    member x.Deposit(value) = amount <- amount + value
//    member x.Withdraw(value) = amount <- amount - value
//end

let trigram_prob (trigram:string) = 
    1.0
   
let emptyTE =  {Tag = ""; Probs = new Dictionary<TE, double>(); BPS = new Dictionary<TE, TE>() }
    
let beam_factor = 1.0

let viterbi 
  (token_list:string list)
  (known_words_tags_probs:TinyNLP.POST.Word.mapprobs) 
  (suffix_tree:SuffixTree)
  (theta:float)
  (corpus_data:CorpusData) = 

    let viterbi_token (i:int) (token:string) (tag_matrix:TagMatrix) (beam:float) = 
        let loopTrigram t2 (tagEntry:KeyValuePair<string, float>) (newEntry: TE) =           
            let highestProbs = (t2.Probs |> Seq.fold (fun (highestProb, highestProbBp) t1 -> 
                    if t1.Value < beam then
                        (highestProb, highestProbBp)
                    else
                        let trigram = t1.Key.Tag + TinyNLP.POST.Corpus.delimiter + t2.Tag
                        let prob = trigram_prob trigram + t1.Value + tagEntry.Value
                        if prob > highestProb then
                            (prob, t1.Key)
                        else 
                            (highestProb, highestProbBp)
                    )
                    (System.Double.NegativeInfinity, emptyTE) )          
            newEntry.Probs.Add(t2, fst highestProbs)
            newEntry.BPS.Add(t2, snd highestProbs)
            (newEntry, fst highestProbs)

        let loopentries (tagEntry:KeyValuePair<string, float>) (chp:float) = 
                tag_matrix.Add(new List<TE>())
                let new_entry, columnHighestProbCandidate = Seq.fold (fun a t2 -> loopTrigram t2 tagEntry (fst a)) ({Tag = tagEntry.Key; Probs = new Dictionary<TE, float>(); BPS = new Dictionary<TE, TE>()}, chp) tag_matrix.[i-1]   
                let c = tag_matrix.Count
                tag_matrix.[c].Add(new_entry)
                columnHighestProbCandidate
        let columnHighestProb = System.Double.NegativeInfinity        
        let tagProbs =            
            let a = known_words_tags_probs.TryGetValue(token.ToLower())
            match a with 
                | (false, _) -> known_words_tags_probs.[token.ToLower()] 
                | (true, tag) -> TinyNLP.POST.Word.getProbsBySuffix (token.ToLower()) suffix_tree theta corpus_data
                                    
                    
        let t = tagProbs |> Seq.fold (fun (candidate:float) tagEntry ->  loopentries tagEntry candidate) columnHighestProb
        columnHighestProb - beam_factor    
        

    let beam = 0.0    
    let tag_matrix = new TagMatrix()
    tag_matrix.Add(new List<TE>())
    tag_matrix.[0].Add({Tag = token_list.[0]; Probs = new Dictionary<TE, double>(); BPS = new Dictionary<TE, TE>() })
    //{Tag = token_list.[0]; Probs = new Dictionary<TE, double>(); BPS = new Dictionary<TE, TE>() }]]
    token_list |> List.tail |> List.mapi (fun i x -> viterbi_token i x tag_matrix beam) |> ignore
    printfn "%A" tag_matrix
//    List<List<TagMatrixEntry>> tagMatrix = new ArrayList<List<TagMatrixEntry>>(sentence.size());
//
//		int startTag = d_model.tagNumbers().get(sentence.get(0));
//
//		// Prepare initial matrix entries;
//		TagMatrixEntry firstEntry = new TagMatrixEntry(startTag);
//		tagMatrix.add(new ArrayList<TagMatrixEntry>(1));
//		tagMatrix.get(0).add(firstEntry);
//
//		tagMatrix.add(new ArrayList<TagMatrixEntry>(1));
//		tagMatrix.get(1).add(new TagMatrixEntry(startTag));
//		tagMatrix.get(1).get(0).probs.put(firstEntry, 0.0);
//		tagMatrix.get(1).get(0).bps.put(firstEntry, null);
//
//		double beam = 0.0;
//
//		// Loop through the tokens.
//		for (int i = 2; i < sentence.size(); ++i) {
//			double columnHighestProb = Double.NEGATIVE_INFINITY;
//
//			tagMatrix.add(new ArrayList<TagMatrixEntry>());
//
//			for (Entry<Integer, Double> tagEntry:
//					d_wordHandler.tagProbs(sentence.get(i)).entrySet()) {
//				TagMatrixEntry newEntry = new TagMatrixEntry(tagEntry.getKey());
//
//				// Loop over all possible trigrams
//				for (TagMatrixEntry t2: tagMatrix.get(i - 1)) {
//					double highestProb = Double.NEGATIVE_INFINITY;
//					TagMatrixEntry highestProbBp = null;
//
//					for (Map.Entry<TagMatrixEntry, Double> t1Entry: t2.probs.entrySet()) {
//						if (t1Entry.getValue() < beam)
//							continue;
//
//						TriGram curTriGram = new TriGram(t1Entry.getKey().tag, t2.tag,
//							tagEntry.getKey());
//
//						double triGramProb = d_languageModel.triGramProb(curTriGram);
//						double prob = triGramProb + tagEntry.getValue() + t1Entry.getValue();
//
//						if (prob > highestProb)
//						{
//							highestProb = prob;
//							highestProbBp = t1Entry.getKey();
//						}
//					}
//
//					newEntry.probs.put(t2, highestProb);
//					newEntry.bps.put(t2, highestProbBp);
//
//					if (highestProb > columnHighestProb)
//						columnHighestProb = highestProb;
//				}
//
//				tagMatrix.get(i).add(newEntry);
//			}
//
//			beam = columnHighestProb - d_beamFactor;
//		}
//
//		return tagMatrix;
