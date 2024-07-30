#load "../src/EffectfulCode.fs"

let ef = new EffectfulCode.CodeBuilder()
let withHandler = new EffectfulCode.AddHandlerBuilder()

type WriteLogEffect = WriteLog of string
let inline h_log (WriteLog s) = ef { return printfn "-- log: %s" s }

withHandler {
    return ef {
        for i in [| 1; 2; 3 |] do
            do! ef{yield WriteLog(sprintf "%i" i)}
        }
    yield h_log
} |> fun code -> code () |> ignore

let inline seq_example1 first last = ef {
    for i in [| first..last |] do
        do! ef{yield WriteLog(sprintf "%i" i)}
    }

withHandler {
    return seq_example1 100 110
    yield h_log
} |> fun code -> code () |> ignore
