
module ParserCombinators.Core

open System

let chars2str cs = string (new System.String (cs |> List.toArray))
let chars2int s = int (new System.String(s |> List.toArray))
let str2chars (s : string) = List.ofArray <| s.ToCharArray()     

type Position(position : int, line : int, column : int) =
  class  
    new() = new Position(0, 0, 0)
    member x.Position = position
    member x.Line = line
    member x.Column = column
    member x.Update(c : char) = 
        let newline = c = '\n'
        new Position(position + 1, line + (if newline then 1 else 0), if newline then 0 else column + 1)

    interface IComparable with
        member x.CompareTo(o : obj) =
            match o with
            | :? Position as y -> y.Position.CompareTo(position)
            | _ -> -1

    override x.Equals(o : obj) =
        match o with
        | :? Position as y -> position.Equals(y.Position)
        | _ -> false

    override x.GetHashCode() = (position ^^^ column).GetHashCode()
  end

type ParserError(err : string) =
  class
    member x.ErrorMessage = err
  end

type ParserInfo(str : char list, pos : Position, err : ParserError option) =
  class
    new(str : char list, pos : Position) = new ParserInfo(str, pos, None)

    member x.Rest = str
    member x.Position = pos
    member x.HasError = Option.isSome err
    member x.Error = 
        match err with
        | Some e -> e
        | None   -> new ParserError("No parser error")

    member x.Stop(err : ParserError) =
        new ParserInfo(str, pos, Some err)

    member x.IsEof = str.Length = 0

    static member StartInfo(s : string) = new ParserInfo(str2chars s, new Position())
  end
    
type ParserResult<'a> = 
    | S of 'a * ParserInfo
    | F of ParserInfo

type internal Parser<'a> = ParserInfo -> (ParserResult<'a>) seq
  
let internal yield' x = seq { yield x }

/// Always returns a Success with x as result
let value x : Parser<'a> =
    fun pi -> yield' <| S(x, pi)

/// Always empty.
let empty : Parser<'a> =
    fun _ -> Seq.empty

/// Alwaus fails.
let fails (err : ParserError) : Parser<'a> =
    fun pi -> yield' <| F(pi.Stop(err))

/// Bind operator. Applies f to the result of parser p.
let (>>=) (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
    let apply (f : 'a -> Parser<'b>) = 
        function
        | S(a, pi) -> (f a) pi
        | F(pi) -> yield' <| F(pi)
    fun pi -> Seq.concat (seq { for x in p pi do yield apply f x })

/// Applies the first parser and if it fails, applies the second one.
let (<|>) (p1 : Parser<'a>) (p2 : Parser<'a>) : Parser<'a> =
    fun pi -> Seq.append (p1 pi) (p2 pi)

/// Parses characters which satisfy the given function.
let symf f : Parser<char> =
    fun pi -> 
        match pi.Rest with
        | c::nrest when f c -> yield' <| S(c, new ParserInfo(nrest, pi.Position.Update(c)))
        | _ -> yield' <| F(pi.Stop(new ParserError ("Symf error")))

/// Runs the given parser against the given string.
let run (p : Parser<'a>) (s : string) = 
    let results = p <| ParserInfo.StartInfo(s)
    let succ = results
               |> Seq.filter (function S(_, pi) -> pi.IsEof | _ -> false)
    if Seq.isEmpty succ 
    then 
         let errors = 
             results
             |> Seq.map (function S(_, pi) -> pi | F(pi) -> pi)
             |> Seq.sortBy (fun pi -> pi.Position)
         if Seq.isEmpty errors
         then F(ParserInfo.StartInfo("").Stop(new ParserError("Unknown parser error")))
         else errors
              |> Seq.head
              |> F
    else Seq.head succ

type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = value y

let parse = new ParserBuilder()

/// Runs p as many times as posible, returning a list with the results.
let rec many p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs
    } <|> value []

/// If p succeeds returns the result x.
let (>>%) p x : Parser<'b> =
    p >>= (fun _ -> value x)

/// Applies p1 and p2 returning the result of p2.
let (>>.) p1 p2 : Parser<'b> =
    p1 >>= (fun _ -> p2)

/// Applies p1 and p2 returning the result of p1.
let (.>>) p1 p2 : Parser<'a> =
    p1 >>= (fun x -> p2 >>% x)
    
/// Applies p1 and p2 returning as a result a tuple with both results.
let (.>>.) p1 p2 : Parser<'a * 'b> =
    p1 >>= (fun x -> p2 >>= (fun y -> value (x, y)))
    
/// If p is successful applies f to the parsed element.
let (|>>) p f : Parser<'b> =
    p >>= (fun x -> value (f x))

/// Runs phead and ptail, then create a list
let (>|>>) phead ptail : Parser<'a list> =
    phead .>>. ptail >>= (fun (h, t) -> value (h :: t))

/// Runs p as many times as possible with at least one Succcess.
let many1 p : Parser<list<'a>> =
    p >|>> many p
    
/// Runs p n times.
let rec times n p : Parser<list<'a>> =
    parse {
        if n <= 0 then return []
        else
            let! x = p
            let! xs = times (n - 1) p
            return x :: xs
    }

/// Returns the first successful result of the given parser sequence.
let any ps : Parser<'a> =
    Seq.reduce (fun p1 p2 -> p1 <|> p2) ps

/// Tries to run p
let opt (p : Parser<'a>) : Parser<'a option> = 
    p |>> Some <|> value None

/// Tries to run p. If it success then apply f to result else return default value
let optf f default' (p : Parser<'a>) = 
    opt p |>> (fun x -> match x with Some y -> f y | None -> default')

/// Applies the parsers popen, p and pclose in sequence. It returns the result of p.
let between popen p pclose =
    popen >>. p .>> pclose

/// Parses the given character.
let sym c : Parser<char> = 
    symf ((=) c) <|> fails (new ParserError(sprintf "symbol \'%c\' is expected" c))

/// Parses any of given character
let syms cs : Parser<char> =
    any (List.map sym cs)
    
/// Parses the given string.
let pstr (s : string) : Parser<string> =
    let chars2str = (fun cs -> new System.String(cs |> List.toArray))
    let rec chars (cs : list<char>) =
        match cs with
        | [] -> value []
        | c::cs' -> sym c >|>> chars cs'
    chars (Seq.toList s) |>> chars2str

/// Parses letter of English alphabet
let palpha = 
    syms <| ['a'..'z'] @ ['A'..'Z']

/// Parses any char sequence
let pword = palpha
            |> many1 |>> chars2str

/// Parses white space characters. Useful for skipping them.
let pws = many <| syms [' '; '\t'; '\n'; '\r']

/// Makes p skip white space before its parsing
let skipws p = pws >>. p

/// Parses the given char and skips white space characters.
let symw c : Parser<char> = skipws (sym c)

/// Parses the given string and skips white space characters before string.
let pstrw s : Parser<string> = skipws (pstr s)

/// Parses any char sequence and skips white space characters before.
let pwordw : Parser<string> = skipws pword

/// Parses any digit
let pdigit = symf (fun c -> c >= '0' && c <= '9')

/// Parses mathematical sign /+|-/
let psign = sym '+' <|> sym '-' <|> value '+'

/// Parses integer number which match /[+-]?\d+/
let pint : Parser<int> =
    psign >|>> many1 pdigit |>> chars2int

/// Parses float numbers which match /[+-]?(\d+(\.\d+)?)/
let pfloat : Parser<float> =
    parse {
        let! sign = psign
        let! i = many1 pdigit
        let! d = sym '.' >|>> many1 pdigit |> optf id []
        return float (chars2str <| sign::(i @ d))
    }