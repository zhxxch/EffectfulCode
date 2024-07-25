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

type EffectfulCodeType<'codeT, 'raiseT, 'handleT> =
    'codeT * 'raiseT * 'handleT

type Effect<'raiseT, 'resumeT> =
    | RaisingT of 'raiseT
    | ResumingT of 'resumeT

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
type EffectfulCode<'codeT, 'raiseT, 'handleT> =
    ResumableCode<EffectfulCodeState,
        EffectfulCodeType<'codeT, 'raiseT, 'handleT>>

let inline CombineDynamic(
        sm: byref<EffectfulCodeSM>,
        code1: EffectfulCode<
            'code1T, 'raise1T, 'handle1T>,
        code2: EffectfulCode<
            'code2T, 'raise2T, 'handle2T>) =
    if code1.Invoke(&sm) then code2.Invoke(&sm)
    else
        let rec resume (mf: EffectfulResumptionFunc) =
            EffectfulResumptionFunc(fun sm ->
                if mf.Invoke(&sm)
                then code2.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionFunc <-
                        (resume (sm.ResumptionDynamicInfo.ResumptionFunc))
                    false)
        sm.ResumptionDynamicInfo.ResumptionFunc <-
            (resume (sm.ResumptionDynamicInfo.ResumptionFunc))
        false
let inline BindDynamic (
    sm: byref<EffectfulCodeSM>,
    code: EffectfulCode<
        'codeT, 'codeRaiseT, 'codeHandleT>,
    cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>) =
    CombineDynamic(&sm, code,
        EffectfulCode<'contT, 'contRaiseT, 'contHandleT>(fun sm' ->
            let resumed =
                unbox_resumed<'codeT> sm'.Data
            match resumed with
            | Some r -> 
                sm'.Data <- NoEffect
                (cont r).Invoke(&sm')
            | _ -> false
        ))
type EffectfulCodeBuilder() =
    member inline __.Delay (generator:
        unit -> EffectfulCode<'codeT, 'raiseT, 'handleT>) =
        ResumableCode.Delay generator
    member inline __.Combine (
        code1: EffectfulCode<'code1T, 'raise1T, 'handle1T>,
        code2: EffectfulCode<'code2T, 'raise2T, 'handle2T>) =
        EffectfulCode<'code2T, 'raise1T * 'raise2T, 'handle1T * 'handle2T>(fun sm ->
            CombineDynamic(&sm, code1, code2))
    member inline __.Return (x: 'T) =
        EffectfulCode<'T, unit, unit> (fun sm ->
            sm.Data <- Resuming (typeof<unit>, (box x, typeof<'T>)); true)
    member inline self.Zero () = self.Return (())
    member inline __.Yield (x: 'raiseT) =
        EffectfulCode<'T, Effect<'raiseT,'T>, unit> (fun sm ->
            sm.Data <- Raising ((box x), typeof<'raiseT>)
            ResumableCode.Yield().Invoke(&sm))
    member inline self.Bind (
        code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>,
        cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>) = 
        EffectfulCode<'contT, 'codeRaiseT * 'contRaiseT, 'codeHandleT * 'contHandleT>(fun sm ->
            BindDynamic(&sm, code, cont))

let rec handle_with_dynamic
    (sm: byref<EffectfulCodeSM>)
    (handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>)
    (code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>) =
    let __stack_code_fin = code.Invoke(&sm)
    if __stack_code_fin then true
    else
        let raised = unbox_raised<'e> sm.Data
        let rf = sm.ResumptionDynamicInfo.ResumptionFunc
        sm.ResumptionDynamicInfo.ResumptionFunc <-
            match raised with
            | Some e -> 
                EffectfulResumptionFunc(fun sm ->
                    CombineDynamic(
                        &sm,
                        (handler e),
                        EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>(fun sm ->
                            (handle_with_dynamic &sm handler (EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>(fun sm -> rf.Invoke(&sm)))))))
            | _ -> EffectfulResumptionFunc(fun sm ->
                    (handle_with_dynamic &sm handler (EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>(fun sm -> rf.Invoke(&sm)))))
        false

let inline handle_with
    (handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>)
    (code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>) =
    EffectfulCode<'codeT, 'codeRaiseT * 'handlerRaiseT, (Effect<'e, 'resumeT> -> 'codeHandleT) * 'handlerHandleT>(fun sm -> handle_with_dynamic &sm handler code)

let inline async_sm_next(x: byref<'T> when 'T :> IAsyncStateMachine) = x.MoveNext()

type TypeStackSymbol =
    | Instance of Type
    | Grouping

let rec type_check_iter (raising: Type) (resuming: Type) (raising_stack: TypeStackSymbol list) (resuming_stack: TypeStackSymbol list) = ()

let inline compile (code: EffectfulCode<'codeT, 'raiseT, 'handleT>) =
    let initialResumptionFunc = EffectfulResumptionFunc(fun sm -> code.Invoke(&sm))
    let resumptionInfo = {
        new EffectfulResumptionDynamicInfo(initialResumptionFunc) with 
            member info.MoveNext(sm) = 
                if info.ResumptionFunc.Invoke(&sm) then
                    sm.ResumptionPoint <- -1
            member info.SetStateMachine(sm, state) = ()
    }
    let mutable sm = new EffectfulCodeSM()
    sm.ResumptionDynamicInfo <- resumptionInfo
    sm.Data <- NoEffect
    fun () ->
        while sm.ResumptionPoint <> -1 do
            async_sm_next &sm
        match sm.Data with
            | Resuming (e_typ, (r_obj, r_typ)) ->
                unbox<'codeT> r_obj
            | _ -> failwith "unhandled"

let ef = new EffectfulCodeBuilder();;

module Operators =
    let inline (./) (m: EffectfulCode<'codeT, 'raiseT, 'handleT>) (h: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>) =
        handle_with h m
    let inline (.()) (m: EffectfulCode<'codeT, 'raiseT, 'handleT>) =
        (compile m)()
