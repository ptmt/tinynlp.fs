#if INTERACTIVE
#time
#I @"D:\projects\tinynlp.fs\src\app\bin\Debug\"
#r "TinyNLP.fs.dll"
#endif

TinyNLP.Tokenizer.tokenize "слово - не воробей"

TinyNLP.Tokenizer.tokenize "слово не воробей"

TinyNLP.Tokenizer.tokenize "слово | не воробей"

TinyNLP.Tokenizer.tokenize "(слово/выражение) не воробей"

TinyNLP.Tokenizer.splitSentences "Что бы мне такого сделать на сто миллионов? Ну что-то. Ничего! совсем — даже (ни в коем случае) и не пытайся" 

TinyNLP.Synonymizer.getSynonyms (TinyNLP.Stemming.Stem "пишу") "VERB"