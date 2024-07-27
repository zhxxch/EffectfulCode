#load "../src/EffectfulCode.fs"
#nowarn "3501"

// declear computation expression (CE) builder
let ef = new EffectfulCode.CodeBuilder()
let withHandler = new EffectfulCode.AddHandlerBuilder()

// declear effects
type AskInputEffect = AskInput of string
type WriteLogEffect = WriteLog of string
type MultiplyEffect = Mul of float * float
type AddEffect = Add of float * float

// define effectful code
let example1 =
    ef {
        do! ef { yield WriteLog "effectful code start" }
        let! x = ef { yield AskInput "x = " }
        printfn "variable x = %g" x

        let! y = ef { yield AskInput "y = " }
        printfn "variable y = %g" y

        let! xy = ef { yield Add(x, y) }
        printfn "x + y = %g" xy

        let! z = ef { yield AskInput "z = " }
        printfn "variable z = %g" z

        let! xyz = ef { yield Mul(xy, z) }
        printfn "(x + y) * z = %g" xyz

        do! ef { yield WriteLog "effectful code about to return" }

        let xyz' = (x + y) * z
        return (xyz, xyz')
    }

// define handlers
let h_log (WriteLog s) = ef { return printfn "-- log: %s" s }

let h_input (AskInput question) =
    ef {
        printf "%s" question
        let input_str = System.Console.ReadLine()
        let x = System.Double.Parse input_str
        do! ef { yield WriteLog(sprintf "resume `AskInput` with %g" x) }
        return x
    }

let h_add (Add(x, y)) =
    ef {
        do! ef { yield WriteLog(sprintf "resume `Add` with %g" (x + y)) }
        return x + y
    }

let h_mul (Mul(x, y)) =
    ef {
        do! ef { yield WriteLog(sprintf "resume `Mul` with %g" (x * y)) }
        return x * y
    }
// define alternative handlers
let h_add_tropical (Add(x, y)) =
    ef {
        do! ef { yield WriteLog(sprintf "resume `Add` with %g" (min x y)) }
        return min x y
    }

let h_mul_tropical (Mul(x, y)) =
    ef {
        do! ef { yield WriteLog(sprintf "resume `Mul` with %g" (x + y)) }
        return x + y
    }

// add handlers to effectful code
example1
|> withHandler.Add h_mul
|> withHandler.Add h_add
|> withHandler.Add h_input
|> withHandler.Add h_log
|> ef.Compile
|> fun effectfulCode -> effectfulCode ()
|> fun (a, a') -> printfn "(x + y) * z = %g (%g)" a a'

let example1' = example1 |> withHandler.Add h_input

// add handlers(alternative way)
example1'
|> withHandler {
    yield h_add
    yield h_mul
    yield h_log
}
|> withHandler.Run
|> fun (a, a') -> printfn "(x + y) * z = %g (%g)" a a'

// add handlers(alternative way & different handlers)
withHandler {
    return example1'
    yield h_add_tropical
    yield h_mul_tropical
    yield h_log
}
|> fun (a, a') -> printfn "(x + y) * z = %g (%g)" a a'

// trying to run effectful code with any unhandled effect raises an exception
withHandler {
    return example1'
    yield h_log
    yield h_add_tropical
    yield h_mul_tropical
}
|> fun (a, a') -> printfn "(x + y) * z = %g (%g)" a a'
