module EffectfulCode

open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

type Type = System.Type

type EffectfulCodeState =
    | NoEffect
    | Raising of obj * Type
    | Resuming of Type * (obj * Type)

let unbox_resumed<'T> effectful_code_state =
    match effectful_code_state with
    | Resuming (e_typ, (r_obj, r_typ)) ->
        if r_typ = typeof<'T>
        then Some (unbox<'T> r_obj)
        else failwith (sprintf "mismatch handler result type! expect: %A; fact: %A." typeof<'T> r_typ);None
    | _ -> failwith (sprintf "handler not resume! expect %A; fact: %A." typeof<'T> effectful_code_state);None

let unbox_raised<'T> effectful_code_state =
    match effectful_code_state with
    | Raising (e_obj, e_typ) ->
        if e_typ = typeof<'T>
        then Some (unbox<'T> e_obj)
        else None
    | _ -> None

type EffectfulCodeSM =
    ResumableStateMachine<EffectfulCodeState>
type EffectfulResumptionFunc =
    ResumptionFunc<EffectfulCodeState>
type EffectfulResumptionDynamicInfo =
    ResumptionDynamicInfo<EffectfulCodeState>
/// 'T: resumable code fragment result type
type EffectfulCode<'T> =
    ResumableCode<EffectfulCodeState, 'T>

type EffectfulCodeBuilder() =
    member inline __.Zero () = ResumableCode.Zero ()
    member inline __.Delay (generator: unit -> EffectfulCode<'T>) =
        ResumableCode.Delay generator
    member inline __.Combine (
        code1: EffectfulCode<unit>,
        code2: EffectfulCode<'T>) =
        ResumableCode.Combine (code1, code2)
    member inline __.Return (x: 'T) =
        EffectfulCode<'T> (fun sm ->
            sm.Data <- Resuming (typeof<unit>, (box x, typeof<'T>)); true)
    member inline __.Yield (x: 'e) =
        EffectfulCode<'T> (fun sm ->
            sm.Data <- Raising ((box x), typeof<'e>)
            ResumableCode.Yield().Invoke(&sm))
    member self.BindDynamic (
        sm: byref<EffectfulCodeSM>,
        eff: EffectfulCode<'Th>,
        cont: 'Th -> EffectfulCode<'Tc>) =
        ResumableCode.CombineDynamic(
            &sm,
            EffectfulCode<_>(fun sm -> eff.Invoke(&sm)),
            EffectfulCode<'Tc>(fun sm' ->
                let resumed = unbox_resumed<'Th> sm'.Data in
                match resumed with
                | Some r -> 
                    sm'.Data <- NoEffect
                    (cont r).Invoke(&sm')
                | _ -> false
            ))
    member inline self.Bind (eff: EffectfulCode<'Th>, cont: 'Th -> EffectfulCode<'Tc>) = 
        EffectfulCode<'Tc>(fun sm ->
            self.BindDynamic(&sm, eff, cont))

let rec handle_with_dynamic<'e, 'Th, 'Tc> (sm: byref<EffectfulCodeSM>) (handler: 'e -> EffectfulCode<'Th>) (code: EffectfulCode<'Tc>) =
    let __stack_code_fin = code.Invoke(&sm)
    if __stack_code_fin then true
    else
        let raised = unbox_raised<'e> sm.Data
        let rf = sm.ResumptionDynamicInfo.ResumptionFunc
        sm.ResumptionDynamicInfo.ResumptionFunc <-
            match raised with
            | Some e -> 
                EffectfulResumptionFunc(fun sm ->
                    ResumableCode.CombineDynamic(
                        &sm,
                        EffectfulCode<_>(fun sm -> (handler e).Invoke(&sm)),
                        EffectfulCode<'Tc>(fun sm ->
                            (handle_with_dynamic &sm handler (EffectfulCode<'Tc>(fun sm -> rf.Invoke(&sm)))))))
            | _ -> EffectfulResumptionFunc(fun sm ->
                    (handle_with_dynamic &sm handler (EffectfulCode<'Tc>(fun sm -> rf.Invoke(&sm)))))
        false

let inline handle_with<'e, 'Th, 'Tc> (handler: 'e -> EffectfulCode<'Th>) (code: EffectfulCode<'Tc>) =
    EffectfulCode<'Tc>(fun sm -> handle_with_dynamic &sm handler code)

let inline async_sm_next(x: byref<'T> when 'T :> IAsyncStateMachine) = x.MoveNext()

let inline run_effectful_code<'T> (code: EffectfulCode<'T>) =
    let initialResumptionFunc = EffectfulResumptionFunc(fun sm -> code.Invoke(&sm))
    let resumptionInfo = {
        new EffectfulResumptionDynamicInfo(initialResumptionFunc) with 
            member info.MoveNext(sm) = 
                if info.ResumptionFunc.Invoke(&sm) then
                    sm.ResumptionPoint <- -1
            member info.SetStateMachine(sm, state) = ()
    }
    let mutable effectful_code_sm = new EffectfulCodeSM()
    effectful_code_sm.ResumptionDynamicInfo <- resumptionInfo
    effectful_code_sm.Data <- NoEffect
    while effectful_code_sm.ResumptionPoint <> -1 do 
        async_sm_next &effectful_code_sm
    match effectful_code_sm.Data with
        | Resuming (e_typ, (r_obj, r_typ)) -> unbox<'T> r_obj
        | _ -> failwith "unhandled"

module Operators =
    let ef = new EffectfulCodeBuilder()
    let inline (/) (m: EffectfulCode<'Tc>) (h: 'e -> EffectfulCode<'Th>) =
        handle_with h m
    let inline (.()) (m: EffectfulCode<'T>) =
        run_effectful_code m
