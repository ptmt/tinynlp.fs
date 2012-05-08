module Util

open System.Runtime.Caching
open Kevo.MemoryCache

let append_log str = 
    use f = System.IO.File.AppendText("out.txt")
    f.WriteLine (string str)
    f.Close()

let inline addToDict (dict:System.Collections.Generic.Dictionary<'a, 'b>) (key:'a) (value:'b) = 
    dict.Add(key, value)
    dict

