(* EffectfulCode.fs - one-shot algebraic effects for F#

Copyright (C) 2024 github.com/zhxxch

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

module EffectfulCode

//#nowarn "3513" // warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定义新的低级别可恢复代码，请取消显示此警告。

open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

[<Struct>]
type EffectfulCodeState =
    { mutable Value: obj
      mutable EffectType: System.Type }

type NoEffect = NoEffect
exception UnhandledEffect of string

type IEffectfulCodeSM = IResumableStateMachine<EffectfulCodeState>
type EffectfulCodeSM = ResumableStateMachine<EffectfulCodeState>
type EffectfulResumptionFunc = ResumptionFunc<EffectfulCodeState>
type EffectfulResumptionDynamicInfo = ResumptionDynamicInfo<EffectfulCodeState>

type EffectfulCode<'codeT> = ResumableCode<EffectfulCodeState, 'codeT>

module DynamicCode =
    let inline combine (sm: byref<EffectfulCodeSM>, code1: EffectfulCode<_>, code2: EffectfulCode<_>) =
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

    let inline bind (sm: byref<EffectfulCodeSM>, code: EffectfulCode<'codeT>, cont: 'codeT -> EffectfulCode<'contT>) =
        combine (
            &sm,
            code,
            EffectfulCode<'contT>(fun sm' ->
                if sm'.Data.EffectType <> typeof<NoEffect> then
                    raise (UnhandledEffect sm'.Data.EffectType.Name)

                let resumed = Unchecked.unbox<'codeT> sm'.Data.Value
                (cont resumed).Invoke(&sm'))
        )

    let rec handle (sm: byref<EffectfulCodeSM>) (handler: 'e -> EffectfulCode<_>) (code: EffectfulCode<'c>) =
        let __stack_code_fin = code.Invoke(&sm)

        if __stack_code_fin then
            true
        elif sm.Data.EffectType <> typeof<'e> then
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                EffectfulResumptionFunc(fun sm' ->
                    handle &sm' handler (EffectfulCode<'c>(fun sm'' -> rf.Invoke(&sm''))))

            false
        else
            let raised = Unchecked.unbox<'e> sm.Data.Value
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                EffectfulResumptionFunc(fun sm ->
                    combine (
                        &sm,
                        handler raised,
                        EffectfulCode<'c>(fun sm -> handle &sm handler (EffectfulCode<'c>(fun sm -> rf.Invoke(&sm))))
                    ))

            false

let inline async_sm_next(x: byref<'T> when 'T :> IAsyncStateMachine) = x.MoveNext()

let inline addHandler (handler: 'e -> EffectfulCode<'resumeT>) (code: EffectfulCode<'codeT>) =
    EffectfulCode<'codeT>(fun sm -> 
        if __useResumableCode
        then
            let __stack_fin1 = code.Invoke(&sm)
            if __stack_fin1 then
                true
            elif sm.Data.EffectType <> typeof<'e> then
                true
            else
                let code_resume_point = sm.ResumptionPoint
                let raised = Unchecked.unbox<'e> sm.Data.Value
                let __stack_fin2 = (handler raised).Invoke(&sm)
                if __stack_fin2 then
                    sm.ResumptionPoint <- code_resume_point
                    false
                else
                    false
        else
            DynamicCode.handle &sm handler code
    )

let inline compile (code: EffectfulCode<'codeT>) =
    if __useResumableCode
    then
        fun () -> (__stateMachine<EffectfulCodeState, 'codeT>
            (MoveNextMethodImpl<_>(fun sm -> 
                __resumeAt sm.ResumptionPoint
                let __stack_code_fin = code.Invoke(&sm)
                if __stack_code_fin then
                    sm.ResumptionPoint  <- -1))
            (SetStateMachineMethodImpl<_>(fun sm state -> ()))
            (AfterCode<_,_>(fun sm ->
                async_sm_next &sm
                while sm.ResumptionPoint <> -1 do async_sm_next &sm
                Unchecked.unbox<'codeT> sm.Data.Value)))
    else
        let initialResumptionFunc = EffectfulResumptionFunc(fun sm -> code.Invoke(&sm))

        let resumptionInfo =
            { new EffectfulResumptionDynamicInfo(initialResumptionFunc) with
                member info.MoveNext(sm) =
                    if info.ResumptionFunc.Invoke(&sm) then
                        sm.ResumptionPoint <- -1

                member info.SetStateMachine(sm, state) = () }

        fun () ->
            let mutable sm = new EffectfulCodeSM()
            sm.ResumptionDynamicInfo <- resumptionInfo
            sm.Data.EffectType <- typeof<NoEffect>
            while sm.ResumptionPoint <> -1 do
                async_sm_next &sm
            Unchecked.unbox<'codeT> sm.Data.Value

type CodeBuilder() =
    member inline __.Delay(g: unit -> EffectfulCode<'T>) = ResumableCode.Delay g

    member inline __.Combine(code1: EffectfulCode<unit>, code2: EffectfulCode<'codeT>) =
        ResumableCode.Combine(code1, code2)

    member inline __.Return(x: 'T) =
        EffectfulCode<'T>(fun sm ->
            sm.Data.Value <- box x
            sm.Data.EffectType <- typeof<NoEffect>
            true)

    member inline self.Zero() = self.Return(())

    member inline __.Yield(x: 'raiseT) =
        EffectfulCode<_>(fun sm ->
            sm.Data.Value <- box x
            sm.Data.EffectType <- typeof<'raiseT>
            ResumableCode.Yield().Invoke(&sm))

    member inline __.Bind(code: EffectfulCode<'codeT>, cont: 'codeT -> EffectfulCode<'contT>) =
        EffectfulCode<'contT>(fun sm -> 
            if __useResumableCode
            then
                let __stack_code_fin = code.Invoke(&sm)
                if __stack_code_fin
                then
                    let resumed = Unchecked.unbox<'codeT> sm.Data.Value
                    (cont resumed).Invoke(&sm)
                else false
            else
                DynamicCode.bind (&sm, code, cont)
        )

    member inline __.While(cond: unit -> bool, code: EffectfulCode<unit>) = ResumableCode.While(cond, code)

    member inline __.TryWith(code: EffectfulCode<'T>, catch: exn -> EffectfulCode<'T>) =
        ResumableCode.TryWith(code, catch)

    member inline __.TryFinally(code: EffectfulCode<'T>, fin: EffectfulCode<unit>) = ResumableCode.TryFinally(code, fin)
    member inline __.Using(resource: 'T, code: 'T -> EffectfulCode<'codeT>) = ResumableCode.Using(resource, code)
    member inline __.For(seq: seq<'T>, code: 'T -> EffectfulCode<unit>) = ResumableCode.For(seq, code)

type AddHandlerBuilder() =
    member inline __.Delay g = g ()
    member inline __.Combine(handlerAdder1, handlerAdder2) = handlerAdder1 >> handlerAdder2

    member inline __.Combine(code, handlerAdder) = handlerAdder code

    member inline __.ReturnFrom(code: unit -> EffectfulCode<_>) = code ()
    
    member inline __.ReturnFrom(code: EffectfulCode<_>) = code
    member inline __.Yield(handler: _ -> EffectfulCode<_>) = addHandler handler
    member inline __.Run([<InlineIfLambda>]code: unit -> EffectfulCode<_>) = compile (code ())
    member inline __.Run(code: EffectfulCode<_>) = compile code
    
    member inline __.Run(handlerAdder: EffectfulCode<_> -> EffectfulCode<_>) = handlerAdder
