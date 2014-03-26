module ParserCombinators.Tests.Core
open NUnit.Framework
open ParserCombinators.Core

let str2cs (s : string) = List.ofArray <| s.ToCharArray()

let isSucc (value : 'a) =
    function 
    | S(v, _) -> v = value
    | _ -> false

let isFail =
    function
    | F _ -> true
    | _ -> false

[<TestFixture>]
type ``Basic parser operators``() =
    [<Test>]
    member x.``Value tests``() =
        Assert.True(isSucc 5 <| run (value 5) "")
        Assert.True(isSucc "abc" <| run (value "abc") "")
        Assert.True(isFail <| run (value 9) "a")

    [<Test>]
    member x.``Empty tests``() =
        Assert.True(run empty "" |> isFail)
        Assert.True(run empty "abcd" |> isFail)

    [<Test>]
    member x.``Symf tests``() =
        Assert.True(isSucc 'a' <| run (symf ((=) 'a')) "a")
        Assert.True(isSucc 'b' <| run (symf ((<>) 'a')) "b")
        Assert.True(run (symf ((=) 'a')) "b" |> isFail)
        Assert.True(isSucc 'd' <| run (symf (fun c -> List.exists ((=) c) ['a'..'e'])) "d")

    [<Test>]
    member x.``Sym tests``() =
        Assert.True(isSucc 'a' <| run (sym 'a') "a")
        Assert.True(isFail <| run (sym 'a') "b")
        Assert.True(isFail <| run (sym 'a') "ax")

    [<Test>]
    member x.``Operator (<|>) tests``() =
        Assert.True(isSucc 'a' <| run (sym 'b' <|> sym 'a') "a")
        Assert.True(isSucc 'a' <| run (sym 'a' <|> sym 'b' <|> sym 'c') "a")
        Assert.True(isSucc 'b' <| run (sym 'a' <|> sym 'b' <|> sym 'c') "b")
        Assert.True(isSucc 'c' <| run (sym 'a' <|> sym 'b' <|> sym 'c') "c")
        Assert.True(isFail <| run (sym 'b' <|> sym 'a') "cx")

    [<Test>]
    member x.``Operator (>>=) tests``() =
        Assert.True(isSucc 4 <| run (sym 'b' >>= (fun _ -> value 4)) "b")
        Assert.True(isFail <| run (sym 'b' >>= (fun _ -> value 4)) "a")
        Assert.True(isSucc 'b' <| run (sym 'b' >>= (fun x -> sym x)) "bb")
        Assert.True(isFail <| run (sym 'b' >>= (fun _ -> sym 'a')) "bx")

[<TestFixture>]
type ``All parsers``() =
    [<Test>]
    member x.``Parser (between) tests``() =
        Assert.True(isSucc ['+'] <| run (between (sym '[') (many <| sym '+') (sym ']')) "[+]")

    [<Test>]
    member x.``Parser (opt) tests``() =
        Assert.True(isSucc (Some 'a') <| run (opt (sym 'a')) "a")
        Assert.True(isSucc None <| run (opt (sym 'a')) "" )
        Assert.True(isSucc 'a' <| run (sym 'a' .>> opt (sym 'b')) "ab")
        Assert.True(isSucc 'a' <| run (sym 'a' .>> opt (sym 'b')) "a")
        Assert.True(isFail <| run (sym 'a' .>> opt (sym 'b')) "ba")

    [<Test>]
    member x.``Parser (syms) tests``() =
        Assert.True(isSucc 'a' <| run (syms ['a';'b';'c']) "a")
        Assert.True(isSucc 'b' <| run (syms ['a';'b';'c']) "b")
        Assert.True(isSucc 'c' <| run (syms ['a';'b';'c']) "c")
        Assert.True(isFail <| run (syms ['a';'b';'c']) "d")
    
[<TestFixture>]
type ``Integer parsing``() =
        
    [<Test>]
    member x.``One digit integers``() =
        Assert.True(isSucc '7' <| run pdigit "7")
        Assert.True(isSucc '0' <| run pdigit "0")
        Assert.True(isSucc '9' <| run pdigit "9")
        Assert.True(isFail <| run pdigit "a")

    [<Test>]
    member x.``Int parsing``() =
        Assert.True(isSucc 42 <| run pint "42")
        Assert.True(isSucc 42 <| run pint "+42")
        Assert.True(isSucc -7 <| run pint "-7")
        Assert.True(isSucc 0 <| run pint "0")
        Assert.True(isSucc 0 <| run pint "00")
        Assert.True(isSucc 123 <| run pint "0123")
        Assert.True(isSucc 0 <| run pint "-0")
        Assert.True(isFail <| run pint "3.14")
