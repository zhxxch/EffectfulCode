module EffectfulCode

//#nowarn "3513" // warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定义新的低级别可恢复代码，请取消显示此警告。

open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

type EffectfulCodeState =
    | NoEffect
    | Raising of obj * System.Type
    | Resuming of System.Type * (obj * System.Type)

type EffectfulCodeType<'codeT, 'raiseT, 'handleT> = 'codeT * 'raiseT * 'handleT

type Effect<'raiseT, 'resumeT> =
    | RaisingT of 'raiseT
    | ResumingT of 'resumeT

let unbox_resumed<'T> effectful_code_state =
    match effectful_code_state with
    | Resuming(e_typ, (r_obj, r_typ)) ->
        if r_typ = typeof<'T> then
            Some(Unchecked.unbox<'T> r_obj)
        else
            failwith (sprintf "mismatch handler result type! expect: %A; fact: %A." typeof<'T> r_typ)
            None
    | _ ->
        failwith (sprintf "handler not resume! expect %A; fact: %A." typeof<'T> effectful_code_state)
        None

let unbox_raised<'T> effectful_code_state =
    match effectful_code_state with
    | Raising(e_obj, e_typ) ->
        if e_typ = typeof<'T> then
            Some(Unchecked.unbox<'T> e_obj)
        else
            None
    | _ -> None

type EffectfulCodeSM = ResumableStateMachine<EffectfulCodeState>
type EffectfulResumptionFunc = ResumptionFunc<EffectfulCodeState>
type EffectfulResumptionDynamicInfo = ResumptionDynamicInfo<EffectfulCodeState>

/// 'codeT: resumable code fragment result type
type EffectfulCode<'codeT, 'raiseT, 'handleT> =
    ResumableCode<EffectfulCodeState, EffectfulCodeType<'codeT, 'raiseT, 'handleT>>

module DynamicCode =
    let inline combine
        (
            sm: byref<EffectfulCodeSM>,
            code1: EffectfulCode<'code1T, 'raise1T, 'handle1T>,
            code2: EffectfulCode<'code2T, 'raise2T, 'handle2T>
        ) =
        if code1.Invoke(&sm) then
            code2.Invoke(&sm)
        else
            let rec resume (mf: EffectfulResumptionFunc) =
                EffectfulResumptionFunc(fun sm ->
                    if mf.Invoke(&sm) then
                        code2.Invoke(&sm)
                    else
                        sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (sm.ResumptionDynamicInfo.ResumptionFunc))
                        false)

            sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (sm.ResumptionDynamicInfo.ResumptionFunc))
            false

    let inline bind
        (
            sm: byref<EffectfulCodeSM>,
            code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>,
            cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>
        ) =
        combine (
            &sm,
            code,
            EffectfulCode<'contT, 'contRaiseT, 'contHandleT>(fun sm' ->
                let resumed = unbox_resumed<'codeT> sm'.Data

                match resumed with
                | Some r ->
                    sm'.Data <- NoEffect
                    (cont r).Invoke(&sm')
                | _ -> false)
        )

    let rec handle
        (sm: byref<EffectfulCodeSM>)
        (handler: 'e -> EffectfulCode<'h, 'hr, 'hh>)
        (code: EffectfulCode<'c, 'cr, 'ch>)
        =
        let __stack_code_fin = code.Invoke(&sm)

        if __stack_code_fin then
            true
        else
            let raised = unbox_raised<'e> sm.Data
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                match raised with
                | Some e ->
                    EffectfulResumptionFunc(fun sm ->
                        combine (
                            &sm,
                            (handler e),
                            EffectfulCode<'c, 'cr, 'ch>(fun sm ->
                                (handle &sm handler (EffectfulCode<'c, 'cr, 'ch>(fun sm -> rf.Invoke(&sm)))))
                        ))
                | _ ->
                    EffectfulResumptionFunc(fun sm ->
                        (handle &sm handler (EffectfulCode<'c, 'cr, 'ch>(fun sm -> rf.Invoke(&sm)))))

            false

module TypeCheck =
    open FSharp.Reflection

    let rec recursiveChecker (handle_stack: System.Type list) (raising: System.Type, resuming: System.Type) =
        if FSharpType.IsFunction resuming then
            let handled_eff, resuming_tail = FSharpType.GetFunctionElements resuming
            recursiveChecker (handled_eff :: handle_stack) (raising, resuming_tail)
        elif FSharpType.IsTuple raising then
            let raising_arr = FSharpType.GetTupleElements raising
            let resuming_arr = FSharpType.GetTupleElements resuming
            Array.tryPick (recursiveChecker handle_stack) (Array.zip raising_arr resuming_arr)
        elif raising <> typeof<unit> then
            if List.contains raising handle_stack then
                None
            else
                Some(raising, handle_stack)
        else
            None

    let tryFindUnhandled (code: EffectfulCode<'codeT, 'raiseT, 'handleT>) =
        recursiveChecker [] (typeof<'raiseT>, typeof<'handleT>)

    let check code = tryFindUnhandled code |> Option.isNone

let inline addHandler
    (handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>)
    (code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>)
    =
    EffectfulCode<'codeT, 'codeRaiseT * 'handlerRaiseT, (Effect<'e, 'resumeT> -> 'codeHandleT) * 'handlerHandleT>
        (fun sm -> DynamicCode.handle &sm handler code)

let inline compile (code: EffectfulCode<'codeT, 'raiseT, 'handleT>) =
    let initialResumptionFunc = EffectfulResumptionFunc(fun sm -> code.Invoke(&sm))

    let resumptionInfo =
        { new EffectfulResumptionDynamicInfo(initialResumptionFunc) with
            member info.MoveNext(sm) =
                if info.ResumptionFunc.Invoke(&sm) then
                    sm.ResumptionPoint <- -1

            member info.SetStateMachine(sm, state) = () }

    let mutable sm = new EffectfulCodeSM()
    sm.ResumptionDynamicInfo <- resumptionInfo
    sm.Data <- NoEffect

    fun () ->
        while sm.ResumptionPoint <> -1 do
            sm.ResumptionDynamicInfo.MoveNext(&sm)

        match sm.Data with
        | Resuming(e_typ, (r_obj, r_typ)) -> unbox<'codeT> r_obj
        | _ -> failwith "unhandled"

let inline run sm = sm ()

type Builder() =
    (*
    member inline __.Delay (generator:
        unit -> EffectfulCode<'codeT, 'raiseT, 'handleT>) =
        ResumableCode.Delay generator
        *)
    member inline __.Combine
        (code1: EffectfulCode<'code1T, 'raise1T, 'handle1T>, code2: EffectfulCode<'code2T, 'raise2T, 'handle2T>)
        =
        EffectfulCode<'code2T, 'raise1T * 'raise2T, 'handle1T * 'handle2T>(fun sm ->
            DynamicCode.combine (&sm, code1, code2))

    member inline __.Return(x: 'T) =
        EffectfulCode<'T, unit, unit>(fun sm ->
            sm.Data <- Resuming(typeof<unit>, (box x, typeof<'T>))
            true)

    member inline self.Zero() = self.Return(())

    member inline __.Yield(x: 'raiseT) =
        EffectfulCode<'T, Effect<'raiseT, 'T>, unit>(fun sm ->
            sm.Data <- Raising((box x), typeof<'raiseT>)
            ResumableCode.Yield().Invoke(&sm))

    member inline self.Bind
        (
            code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>,
            cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>
        ) =
        EffectfulCode<'contT, 'codeRaiseT * 'contRaiseT, 'codeHandleT * 'contHandleT>(fun sm ->
            DynamicCode.bind (&sm, code, cont))

    member inline __.With
        (handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>)
        (code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>)
        =
        addHandler handler code

type AddHandlerBuilder() =
    member inline __.Delay g = g ()

    member inline __.Combine
        (
            handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>,
            code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>
        ) =
        addHandler handler code

    member inline __.ReturnFrom(code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>) = code
    member inline __.YieldFrom(handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>) = handler

    member inline __.Run code =
        match TypeCheck.tryFindUnhandled code with
        | Some e -> failwith (sprintf "Unhandled %A" e)
        | None -> compile code ()
(*
C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fs(56,12): warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定 义新的低级别可恢复代码，请取消显示此警告。
[C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fsproj]
C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fs(56,35): warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定 义新的低级别可恢复代码，请取消显示此警告。
[C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fsproj]
C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fs(61,26): warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定 义新的低级别可恢复代码，请取消显示此警告。
[C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fsproj]
C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fs(88,32): warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定 义新的低级别可恢复代码，请取消显示此警告。
[C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fsproj]
C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fs(131,67): warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定义新的低级别可恢复代码，请取消显示此警告。
 [C:\Users\zhxxc\Documents\Source\zxc.fs\src\EffectfulCode.fsproj]
    5 个警告
    0 个错误
*)
