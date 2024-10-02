module rec my.``mod``

let go () =
    begin
        let thingy = fun (f: unit -> 'u11) -> begin f () end
        thingy (fun () -> begin failwith "Not implemented" end)
    end
