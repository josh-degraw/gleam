module FSharpCustomBehavior

open System.Collections.Generic

type SomeCustomType =
    | Thing
    | OtherThing

let print_string_and_int s (i: int64) = printfn "Hello from F#: %s %d" s i

let print_result s = printfn "Hello from F#: %A" s

let make_stack a = Stack([ a ])

let get_some_custom_type () = Thing

/// Hack to work around needing to assume int64 but main method needs to return an int32 value
let exit (i: int64) : 'a =
    System.Environment.Exit(int i)
    Unchecked.defaultof<_>
