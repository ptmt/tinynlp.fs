module TinyNLP.POST.Suffix

open TinyNLP.POST.Corpus

let buildSuffixTree (corpus_data:CorpusData) = 
        let wordProc = 
            1
        let theta = 1
        corpus_data.Unigrams |> List.iter (fun x -> wordProc x.Key) |> ignore

//		double theta = WordSuffixTree.calculateTheta(uniGrams);
//
//		d_upperSuffixTrie = new WordSuffixTree(uniGrams, theta, maxSuffixLength);
//		d_lowerSuffixTrie = new WordSuffixTree(uniGrams, theta, maxSuffixLength);
//		d_cardinalSuffixTrie = new WordSuffixTree(uniGrams, theta, maxSuffixLength);
//		d_maxTags = maxTags;
//
//		for (Entry<String, Map<Integer, Integer>> wordEntry: lexicon.entrySet()) {
//			String word = wordEntry.getKey();
//
//			// Incorrect lexicon entry.
//			if (word.length() == 0)
//				continue;
//
//			int wordFreq = 0;
//			for (Entry<Integer, Integer> tagEntry: wordEntry.getValue().entrySet())
//				wordFreq += tagEntry.getValue();
//
//			// Select the correct tree.
//			WordSuffixTree suffixTree = null;
//			if (s_cardinalPattern.matcher(word).matches()) {
//				if (wordFreq > cardinalMaxFreq)
//					continue;
//
//				suffixTree = d_cardinalSuffixTrie;
//			}
//			else {
//				boolean isUpper = Character.isUpperCase(word.charAt(0));
//
//				if (!isUpper && wordFreq > lowerMaxFreq)
//					continue;
//				if (isUpper && wordFreq > upperMaxFreq)
//					continue;
//
//				suffixTree = isUpper ? d_upperSuffixTrie : d_lowerSuffixTrie;
//			}
//
//			suffixTree.addWord(word, wordEntry.getValue());
//		}
//
