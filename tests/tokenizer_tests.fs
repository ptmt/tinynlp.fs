module TinyNLP.Tests.TokenizerTests

open NUnit.Framework
open TinyNLP
open TinyNLP.Tests.Helpers
  

[<Test>]
let ``tokenizer should be able to tokenize nothing`` () =
    TinyNLP.Tokenizer.tokenize "" |> fun x -> x.Value.Length = 1 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize simple word`` () =
    TinyNLP.Tokenizer.tokenize "�����" |> fun x -> x.Value.Length = 1 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize two simple words`` () =
    TinyNLP.Tokenizer.tokenize "��� �����" |> fun x -> x.Value.Length = 2 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize three simple words`` () =
    TinyNLP.Tokenizer.tokenize "��� ������� �����" |> fun x -> x.Value.Length = 3 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize two simple words and one preposition`` () =
    TinyNLP.Tokenizer.tokenize "����� � ���������" |> fun x -> x.Value.Length = 3 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize three simple words with one comma`` () =
    TinyNLP.Tokenizer.tokenize "�����, ��� ����" |> (fun x -> x.Value.Length = 4 && x.Value.Item(0) = "�����") |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize New York`` () =
    TinyNLP.Tokenizer.tokenize "���-����" |> fun x -> x.Value.Length = 1 |> shouldBeTrue

[<Test>]
let ``tokenizer should be able to tokenize word with dash`` () =
    TinyNLP.Tokenizer.tokenize "����� - �� �������" |> fun x -> x.Value.Length = 4 |> shouldBeTrue

