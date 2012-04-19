module TinyNLP.POST.Corpus

open System.Collections.Generic

type TaggedWord = string * string

type CorpusData = {
    Lexicon:Map<string, Map<string, int>>;
    Unigrams:Map<string, int>;
    Bigrams:Map<string, int>;
    Trigrams:Map<string, int>
    }

// read and parse corpus xml file
// CC-BY-SA license
// http://opencorpora.org/?page=downloads
// http://opencorpora.org/dict.php?act=gram


let inline readTags (xmlReader:System.Xml.XmlReader) = 
    seq {
        while xmlReader.ReadToFollowing("g") do
            yield xmlReader.GetAttribute("v")
    } |> Seq.fold (fun accum x  -> accum + "," + x) ""

let inline readSentence (xmlReader:System.Xml.XmlReader) =     
    seq {
        yield "<S>", "<S>"
        while xmlReader.ReadToFollowing("token") do    
            let a = xmlReader.GetAttribute("text") 
            xmlReader.ReadToFollowing("l") |> ignore       
            yield a, readTags (xmlReader.ReadSubtree())
        yield "</S>", "</S>"
        } 

let inline incDict (dict:Dictionary<'a, int>, key:'a) = 
    if dict.ContainsKey(key) then         
        dict.Item key <- dict.Item key + 1
    else
        dict.Add(key, 1)
    

let handleSentence (sentence:seq<TaggedWord>, corpus_data:CorpusData):CorpusData = 
    let handleWord tagged_word i = 
        //add lexiconentry
        corpus_data.Lexicon |> Map.filter (fun x y -> x) 
        {Lexicon = incDict corpus_data.Lexicon.Add(tagged_word); 
         Unigrams = incDict corpus_data.Unigrams tagged_word; 
         Bigrams = new Dictionary<string, int>(); 
         Trigrams = new Dictionary<string, int>}

    sentence |> Seq.mapi (fun x i -> handleWord x i)

let readCorpus (input_stream:System.IO.Stream, handler) =    
   
    use xmlReader = System.Xml.XmlReader.Create(input_stream)   
    seq {
        while xmlReader.ReadToFollowing("sentence") do
            yield xmlReader.ReadSubtree()
        } |> Seq.fold (fun corpus_data sentence_tree -> handleSentence ((readSentence sentence_tree) corpus_data))
                {Lexicon = Map.empty; 
                Unigrams = Map.empty; 
                Bigrams = Map.empty; 
                Trigrams = Map.empty}


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