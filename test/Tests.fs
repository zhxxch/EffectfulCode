module EffectfulCode.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open EffectfulCode
open EffectfulCode.Operators

type Eff1 = Eff1 of int
type Eff2 = Eff2 of int
type Eff3 = Eff3 of int

let example1 = ef {
    let! x = ef{return 1} in
    let! (y: float) = ef {yield Eff3 x} in
    let! (z: float) = ef {yield Eff1 11} in
    return x, y, z
}

let example2 = ef {
    let! x = ef {return 1} in
    let! (y:float) = ef{yield Eff2 100} in
    let! (z:float) = ef{yield Eff1 9} in
    return x, y, z
}

let example3 = ef {
    let! (x:int) = ef{return 1} in
    let! (y:float) = ef{yield Eff2 100} ./ (fun (Eff2 x) -> ef{return x |> float |> (*) 50.0}) in
    let! (z:float) = ef{yield Eff1 9} in
    return x, y, z
}

let eff1handler (Eff1 x) = ef {return (x |> float |> (*) 0.1)}

let eff2handler (Eff2 x) = ef {
    return x |> float |> (*) 5.0
}

let eff3handler (Eff3 x) = ef {
    let! y = ef {yield Eff1 (x+99)} in
    return y*99.0
}

[<TestClass>]
type TestSimple () =

    [<TestMethod>]
    member self.Test1 () =
        let example2h12 = example2 ./ eff1handler ./eff2handler
        let example2h21 = example2 ./eff2handler ./eff1handler
        let r = [|example2h12; example2h12|] |> Array.map (.())
        CollectionAssert.AreEqual(r, [|(1, 500.0, 0.9);(1,500.0,0.9)|]);
    [<TestMethod>]
    member self.Test2 () =
        let r = 
            (example1 ./ eff2handler ./ eff3handler ./eff1handler) |> (.())
        Assert.AreEqual(r, (1,990.0,1.1))
