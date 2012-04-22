module TinyNLP.POST.Corpus

open System.Collections.Generic

type TaggedWord = string * string 


type CorpusData = {
    Lexicon:Dictionary<string, Dictionary<string, int>>;
    Unigrams:Dictionary<string, int>;
    Bigrams:Dictionary<string, int>;
    Trigrams:Dictionary<string, int>
    }

// read and parse corpus xml file
// CC-BY-SA license
// http://opencorpora.org/?page=downloads
// http://opencorpora.org/dict.php?act=gram

let delimiter = " "

let inline taggedToStr word =  fst word + delimiter + snd word

let inline readTags (xmlReader:System.Xml.XmlReader) = 
    seq {
        while xmlReader.ReadToFollowing("g") do
            yield xmlReader.GetAttribute("v")
    } |> Seq.fold (fun accum x  -> accum + "," + x) ""

let inline readSentence (xmlReader:System.Xml.XmlReader) =     
    [
        yield "<S>", "<S>"
        while xmlReader.ReadToFollowing("token") do    
            let a = xmlReader.GetAttribute("text") 
            xmlReader.ReadToFollowing("l") |> ignore       
            yield a, readTags (xmlReader.ReadSubtree())
        yield "</S>", "</S>"
     ]

let inline incDict (dict:Dictionary<'a, int>, key:'a) = 
    if dict.ContainsKey(key) then         
        dict.Item key <- dict.Item key + 1
    else
        dict.Add(key, 1)
    dict
    

let handleSentence (sentence:TaggedWord list, corpus_data:CorpusData):CorpusData = 
    
    let rec handleWord (c:CorpusData,i:int) =
        let getLexicon (dict:Dictionary<string, Dictionary<string, int>>, word:TaggedWord) = 
            if dict.ContainsKey(fst word) then 
                dict.[fst word] <- incDict (dict.[fst word], snd word)
            else
                let innerDict = new Dictionary<string, int>()
                innerDict.Add(snd word, 1)
                dict.[fst word] <- innerDict
            dict
                
        let getBigrams = 
            if (i > 0) then
                incDict (c.Bigrams, ( (snd sentence.[i - 1]) + delimiter + (snd sentence.[i]) ));
            else 
                c.Bigrams
        let getTrigrams = 
            if (i > 1) then
                incDict (c.Trigrams, ( (snd sentence.[i - 2]) + delimiter + (snd sentence.[i - 1]) + delimiter + (snd sentence.[i]) ));
            else 
                c.Trigrams
        if i = sentence.Length - 1 then
            c
        else            
            let corpusd = {
                Lexicon = getLexicon (c.Lexicon, sentence.[i]);
                Unigrams =  incDict (c.Unigrams, (snd sentence.[i]));
                Bigrams =  getBigrams;
                Trigrams = getTrigrams;}
            handleWord (corpusd , i+1)  

    handleWord (corpus_data, 0)

let readCorpus (input_stream:System.IO.Stream) =    
   
    use xmlReader = System.Xml.XmlReader.Create(input_stream)   
    seq {
        while xmlReader.ReadToFollowing("sentence") do
            yield xmlReader.ReadSubtree()
        } |> Seq.fold (fun corpus_data sentence_tree -> handleSentence ((readSentence sentence_tree), corpus_data))
                {Lexicon =  new Dictionary<string, Dictionary<string, int>>(); 
                Unigrams =  new Dictionary<string, int>(); 
                Bigrams =  new Dictionary<string, int>(); 
                Trigrams =  new Dictionary<string, int>()}


//
// public void handleSentence(List<TaggedWord> sentence) {
//			for (int i = 0; i < sentence.size(); ++i) {
//				addLexiconEntry(sentence.get(i));
//				addUniGram(sentence, i);
//				if (i > 0)
//					addBiGram(sentence, i);
//				if (i > 1)
//					addTriGram(sentence, i);
//			}
//		}  