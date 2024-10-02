module rec my.``mod``

type private Box<'t8> = Box of 't8

let private apply (arg: 'u10) (``fun``: 'u10 -> 'u14) = begin ``fun`` (arg) end

let go () =
    begin
        apply (Box(1L)) (fun (_use0: Box<int64>) ->
            begin
                let (Box(x)) = _use0
                x
            end)
    end
