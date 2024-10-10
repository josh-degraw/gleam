namespace gleam

// Re-export some core types under the gleam namespace
type Dict<'key, 'value when 'key: comparison> = Map<'key, 'value>
type Set<'key when 'key: comparison> = Microsoft.FSharp.Collections.Set<'key>
type Option<'a> = Microsoft.FSharp.Core.Option<'a>
type Result<'T, 'TErr> = Microsoft.FSharp.Core.Result<'T, 'TErr>
type StringBuilder = System.Text.StringBuilder
type Regex = System.Text.RegularExpressions.Regex

// Manual Override
//type Iterator<'a> = System.Collections.Generic.IEnumerable<'a>

// Gleam-specific types

// For tuples above 1 element we can use built-in tuples
// But gleam supports tuples of any size so we need to define our own
// to represent an empty tuple and a single item tuple
// type Tuple1<'a> = Tuple1 of 'a

type EmptyTuple = EmptyTuple

type BitArray = BitArray of int64

[<Struct>]
type UtfCodepoint = UtfCodepoint of System.Text.Rune

[<CustomEquality; CustomComparison>]
type Dynamic =
    private
    | Dynamic of obj

    interface System.IComparable with
        member this.CompareTo(obj) =
            System.StringComparer.InvariantCulture.Compare($"%A{this}", $"%A{obj}")

    override this.Equals(obj) =
        match obj with
        | :? Dynamic as dynamic -> System.StringComparer.InvariantCulture.Equals($"%A{this}", $"%A{dynamic}")
        | _ -> false

    override this.GetHashCode() =
        System.StringComparer.InvariantCulture.GetHashCode($"%A{this}")

    static member From(a: obj) : Dynamic =
        match a with
        | :? Dynamic as (Dynamic(d)) -> Dynamic.From d
        | a -> Dynamic(a)

type DecodeError = {
    expected: string
    found: string
    path: list<string>
}

type DecodeErrors = list<DecodeError>
type UnknownTuple = UnknownTuple of Dynamic list

type Order =
    | Lt
    | Eq
    | Gt

type Match = {
    content: string
    submatches: list<Option<string>>
}

type RegexOptions = {
    case_insensitive: bool
    multi_line: bool
}

type CompileError = { error: string; byte_index: int64 }

type Uri = {
    scheme: Option<string>
    userinfo: Option<string>
    host: Option<string>
    port: Option<int64>
    path: string
    query: Option<string>
    fragment: Option<string>
}

[<AutoOpen>]
module Prelude =
    /// Check if a string starts with a prefix
    let (|Gleam__codegen__prefix|_|) (prefix: string) (target: string) : string option =
        if target.StartsWith(prefix) then
            Some(target.Substring(prefix.Length))
        else
            None

    /// Split a string into a prefix and a suffix
    let (|Gleam_codegen_string_parts|_|) (p: string) (s: string) : Option<string * string> =
        if s.StartsWith(p) then
            Some(p, s.Substring(p.Length))
        else
            None

    let (|Dynamic|) (Dynamic v) = v

    let (|Tuple1|) (t: System.Tuple<'a>) = t.Item1
    let Tuple1 (a: 'a) = System.Tuple.Create(a)
