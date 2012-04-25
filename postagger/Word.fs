module TinyNLP.POST.Word

open TinyNLP.POST.Corpus
open System.Collections.Generic

type prinner = Dictionary<int, double>
type mapprobs = Dictionary<string, prinner>

let getWordProbs (corpus_data:CorpusData) = 
    let processLex (probs:mapprobs) (word : KeyValuePair<string,Dictionary<string,int>>) = 

        if probs.ContainsKey(word.Key) then 
            probs.Add(word.Key, new prinner())
        word.Value
         |> Seq.iter (fun x -> 
            // P(w|t) = f(w,t) / f(t)
            probs.[word.Key].Add(x.Value, System.Math.Log (float x.Value) / float corpus_data.Unigrams.[x.Key])) 
        probs

    corpus_data.Lexicon |> Seq.fold (fun a x -> processLex a x) (new mapprobs())

let getProbsBySuffix () = 
//    public Map<Integer, Double> tagProbs(String word) {
//		WordSuffixTree suffixTree = null;
//		if (s_cardinalPattern.matcher(word).matches())
//			suffixTree = d_cardinalSuffixTrie;
//		else {
//			boolean isUpper = Character.isUpperCase(word.charAt(0));
//			suffixTree = isUpper ? d_upperSuffixTrie : d_lowerSuffixTrie;
//		}

        // выбираем из трех деревьев нужное (у меня пока одно)
//
//		Set<Entry<Integer, Double>> orderedTags =
//			new TreeSet<Entry<Integer,Double>>(new ProbEntryComparator());

        // создаем структуру для хранения упорядоченных тегов?
        // SortedSet<KeyValuePair<Integer,Double>>
        // добавляем это в дерево
//		orderedTags.addAll(suffixTree.suffixTagProbs(word).entrySet());
//
//		// Get first N results, ordered by descending probability.
//		Map<Integer, Double> results = new HashMap<Integer, Double>();
//		Iterator<Entry<Integer, Double>> iter = orderedTags.iterator();
//		for (int i = 0; i < d_maxTags && iter.hasNext(); ++i) {
//			Entry<Integer, Double> entry = iter.next();
//			results.put(entry.getKey(), Math.log(entry.getValue()));
//		}
//
//		return results;
//	}
