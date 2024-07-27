module EffectfulCode.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

let ef = new EffectfulCode.CodeBuilder()
let withHandler = new EffectfulCode.AddHandlerBuilder()

type Eff1 = Eff1 of int
type Eff2 = Eff2 of int
type Eff3 = Eff3 of int

let example1 =
    ef {
        let! x = ef { return 1 }
        let! (y: float) = ef { yield Eff3 x }
        let! (z: float) = ef { yield Eff1 11 }
        return x, y, z
    }

let example2 =
    ef {
        let! (x: int) = ef { return 1 }
        let! (y: float) = ef { yield Eff2 100 }
        let! (z: float) = ef { yield Eff1 9 }
        return x, y, z
    }

let example3 =
    ef {
        let! (x: int) = ef { return 1 }

        let! (y: float) =
            ef { yield Eff2 100 }
            |> withHandler.Add(fun (Eff2 x) -> ef { return x |> float |> (*) 50.0 })

        let! (z: float) = ef { yield Eff1 9 }
        return x, y, z
    }

let eff1handler (Eff1 x) = ef { return (x |> float |> (*) 0.1) }

let eff2handler (Eff2 x) = ef { return x |> float |> (*) 5.0 }

let eff3handler (Eff3 x) =
    ef {
        let! y = ef { yield Eff1(x + 99) }
        return y * 99.0
    }

type Eff4 = Eff4 of int

let eff4handler (Eff4 x) = ef { return (x, x + 1) }

let example4 =
    ef {
        let! (r: int * int) = ef { yield Eff4 9 }
        return r
    }

let example2h12 =
    withHandler {
        return example2
        yield eff2handler
        yield eff1handler
    }

let example2h21 =
    withHandler {
        return example2
        yield eff2handler
        yield eff1handler
    }

[<TestClass>]
type TestSimple() =

    [<TestMethod>]
    member self.Test1() =
        let r = [| example2h12; example2h12 |]
        CollectionAssert.AreEqual(r, [| (1, 500.0, 0.9); (1, 500.0, 0.9) |])

    [<TestMethod>]
    member self.Test2() =
        let r =
            withHandler {
                return example1
                yield eff3handler
                yield eff2handler
                yield eff1handler
            }

        Assert.AreEqual(r, (1, 990.0, 1.1))

    [<TestMethod>]
    member self.Test3() =
        let r = example4 |> EffectfulCode.addHandler eff4handler
        Assert.AreEqual(EffectfulCode.compile r (), (9, 10))
