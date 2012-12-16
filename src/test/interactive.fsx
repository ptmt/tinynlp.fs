#if INTERACTIVE
#time
#I @"D:\projects\tinynlp.fs\src\app\bin\Debug\"
#r "TinyNLP.fs.dll"
#endif

TinyNLP.Tokenizer.tokenize "слово - не воробей"

TinyNLP.Tokenizer.tokenize "слово не воробей"

TinyNLP.Tokenizer.tokenize "(слово/выражение) не воробей"

