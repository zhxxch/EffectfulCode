#load "../src/EffectfulCode.fs"
#nowarn "3501"

let ef = new EffectfulCode.CodeBuilder()
let withHandler = new EffectfulCode.AddHandlerBuilder()

type FactEffect = Fact of int

let rec fact_handler (Fact x) =
    if x = 0 then
        ef { return 1 }
    else
        ef {
            let! (fact_x_minus_1: int) = ef { yield Fact(x - 1) }
            return x * fact_x_minus_1
        }
        |> withHandler { yield fact_handler }

withHandler {
    return!
        ef {
            let! (r: int) = ef { yield Fact 10 }
            return r
        }

    yield fact_handler
}

type WriteLogEffect = WriteLog of string
let h_log (WriteLog s) = ef { return printfn "-- log: %s" s }

withHandler {
    return!
        ef {
            for i in [| 1; 2; 3 |] do
                do! ef { yield WriteLog(sprintf "%i" i) }
        }

    yield h_log
}
