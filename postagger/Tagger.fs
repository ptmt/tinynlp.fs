module TinyNLP.POST.Tagger

open System.Collections.Generic

// tagmatrix entry
type TE = {
        Tag: string;
        Probs: Dictionary<TE, double>;
        PBS: Dictionary<TE, TE>
    }

type TagMatrix = TE list list

let viterbi (token_list:string list) = 
    tokents |> List.map ()
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
