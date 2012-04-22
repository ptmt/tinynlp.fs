module Util

let append_log str = 
    use f = System.IO.File.AppendText("out.txt")
    f.WriteLine (string str)
    f.Close()