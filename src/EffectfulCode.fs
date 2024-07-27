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

#nowarn "3513" // warning FS3513: 可恢复的代码调用。如果要根据现有可恢复代码定义新的低级别可恢复代码，请取消显示此警告。

open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

[<Struct>]
type EffectfulCodeState =
    { mutable Value: obj
      mutable EffectType: System.Type }

type EffectfulCodeType<'codeT, 'raiseT, 'handleT> = 'codeT * 'raiseT * 'handleT

type Effect<'raiseT, 'resumeT> =
    | RaisingT of 'raiseT
    | ResumingT of 'resumeT

type EffectfulCodeSM = ResumableStateMachine<EffectfulCodeState>
type EffectfulResumptionFunc = ResumptionFunc<EffectfulCodeState>
type EffectfulResumptionDynamicInfo = ResumptionDynamicInfo<EffectfulCodeState>

/// 'codeT: resumable code fragment result type
type EffectfulCode<'codeT, 'raiseT, 'handleT> =
    ResumableCode<EffectfulCodeState, EffectfulCodeType<'codeT, 'raiseT, 'handleT>>

module DynamicCode =
    let inline combine (sm: byref<EffectfulCodeSM>, code1: EffectfulCode<_, _, _>, code2: EffectfulCode<_, _, _>) =
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
            code: EffectfulCode<'codeT, _, _>,
            cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>
        ) =
        combine (
            &sm,
            code,
            EffectfulCode<'contT, 'contRaiseT, 'contHandleT>(fun sm' ->
                let resumed = Unchecked.unbox<'codeT> sm'.Data.Value
                sm'.Data.EffectType <- typeof<unit>
                (cont resumed).Invoke(&sm'))
        )

    let rec handle
        (sm: byref<EffectfulCodeSM>)
        (handler: 'e -> EffectfulCode<_, _, _>)
        (code: EffectfulCode<'c, 'cr, 'ch>)
        =
        let __stack_code_fin = code.Invoke(&sm)

        if __stack_code_fin then
            true
        elif sm.Data.EffectType <> typeof<'e> then
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                EffectfulResumptionFunc(fun sm' ->
                    handle &sm' handler (EffectfulCode<'c, 'cr, 'ch>(fun sm'' -> rf.Invoke(&sm''))))

            false
        else
            let raised = Unchecked.unbox<'e> sm.Data.Value
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                EffectfulResumptionFunc(fun sm ->
                    combine (
                        &sm,
                        handler raised,
                        EffectfulCode<'c, 'cr, 'ch>(fun sm ->
                            handle &sm handler (EffectfulCode<'c, 'cr, 'ch>(fun sm -> rf.Invoke(&sm))))
                    ))

            false

module TypeCheck =
    open FSharp.Reflection

    let rec recursiveChecker
        (handle_stack: System.Type list)
        (raise_stack: System.Type list)
        (raising: System.Type, resuming: System.Type)
        =
        if FSharpType.IsFunction raising then
            let raised_eff, handler_raising = FSharpType.GetFunctionElements raising
            recursiveChecker handle_stack (raised_eff :: raise_stack) (handler_raising, resuming)
        elif FSharpType.IsFunction resuming then
            let handled_eff, handler_resuming = FSharpType.GetFunctionElements resuming
            recursiveChecker (handled_eff :: handle_stack) raise_stack (raising, handler_resuming)
        elif FSharpType.IsTuple raising then
            let raising_arr = FSharpType.GetTupleElements raising
            let resuming_arr = FSharpType.GetTupleElements resuming
            Array.tryPick (recursiveChecker handle_stack raise_stack) (Array.zip raising_arr resuming_arr)
        elif raising <> typeof<unit> then
            if List.contains raising handle_stack then
                None
            else
                Some(raising, handle_stack, raise_stack)
        else
            None

    let tryFindUnhandled (code: EffectfulCode<_, 'raiseT, 'handleT>) =
        recursiveChecker [] [] (typeof<'raiseT>, typeof<'handleT>)

    let printUnhandledCode (raising, handle_stack, raise_stack) =
        let indent (s: string) = "    " + s

        let printEffectType (t: System.Type) =
            let genArgs = t.GetGenericArguments()
            sprintf "%s -> %s" genArgs.[0].Name genArgs.[1].Name

        let folder (folded: string array) (curr: System.Type) =
            Array.concat
                [| Array.singleton (sprintf "yield %s {" (printEffectType curr))
                   Array.map indent folded
                   Array.singleton "}" |]

        let raise_code =
            raise_stack
            |> List.fold folder [| sprintf "yield %s // unhandled" (printEffectType raising) |]

        let handle_code = List.map printEffectType handle_stack

        [| [| "in EffectfulCode:"; "{" |]
           Array.map indent raise_code
           [| "}"; "with handlers:" |]
           List.toArray handle_code |> Array.map indent
           Array.singleton "" |]
        |> Array.concat
        |> String.concat "\n"

    let check code = tryFindUnhandled code |> Option.isNone

let inline addHandler
    (handler: 'e -> EffectfulCode<'resumeT, 'handlerRaiseT, 'handlerHandleT>)
    (code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>)
    =
    EffectfulCode<
        'codeT,
        'codeRaiseT * (Effect<'e, 'resumeT> -> 'handlerRaiseT),
        (Effect<'e, 'resumeT> -> 'codeHandleT) * 'handlerHandleT
     >
        (fun sm -> DynamicCode.handle &sm handler code)

exception UnhandledEffect of string

let inline compile (code: EffectfulCode<'codeT, _, _>) =
    match TypeCheck.tryFindUnhandled code with
    | Some e -> raise (UnhandledEffect(TypeCheck.printUnhandledCode e))
    | None -> ()

    let initialResumptionFunc = EffectfulResumptionFunc(fun sm -> code.Invoke(&sm))

    let resumptionInfo =
        { new EffectfulResumptionDynamicInfo(initialResumptionFunc) with
            member info.MoveNext(sm) =
                if info.ResumptionFunc.Invoke(&sm) then
                    sm.ResumptionPoint <- -1

            member info.SetStateMachine(sm, state) = () }

    let mutable sm = new EffectfulCodeSM()
    sm.ResumptionDynamicInfo <- resumptionInfo
    sm.Data.EffectType <- typeof<unit>

    fun () ->
        while sm.ResumptionPoint <> -1 do
            sm.ResumptionDynamicInfo.MoveNext(&sm)

        Unchecked.unbox<'codeT> sm.Data.Value

type CodeBuilder() =
    member inline __.Combine
        (code1: EffectfulCode<'code1T, 'raise1T, 'handle1T>, code2: EffectfulCode<'code2T, 'raise2T, 'handle2T>)
        =
        EffectfulCode<'code2T, 'raise1T * 'raise2T, 'handle1T * 'handle2T>(fun sm ->
            DynamicCode.combine (&sm, code1, code2))

    member inline __.Return(x: 'T) =
        EffectfulCode<'T, unit, unit>(fun sm ->
            sm.Data.Value <- box x
            sm.Data.EffectType <- typeof<unit>
            true)

    member inline self.Zero() = self.Return(())

    member inline __.Yield(x: 'raiseT) =
        EffectfulCode<'T, Effect<'raiseT, 'T>, unit>(fun sm ->
            sm.Data.Value <- box x
            sm.Data.EffectType <- typeof<'raiseT>
            ResumableCode.Yield().Invoke(&sm))

    member inline __.Bind
        (
            code: EffectfulCode<'codeT, 'codeRaiseT, 'codeHandleT>,
            cont: 'codeT -> EffectfulCode<'contT, 'contRaiseT, 'contHandleT>
        ) =
        EffectfulCode<'contT, 'codeRaiseT * 'contRaiseT, 'codeHandleT * 'contHandleT>(fun sm ->
            DynamicCode.bind (&sm, code, cont))

    member inline __.Compile code = compile code

type AddHandlerBuilder() =
    member inline __.Delay g = g ()
    member inline __.Combine(handlerAdder1, handlerAdder2) = handlerAdder1 >> handlerAdder2

    member inline __.Combine(code, handlerAdder) = handlerAdder code

    member inline __.Return(code: EffectfulCode<_, _, _>) = code

    member inline __.Yield(handler: _ -> EffectfulCode<_, _, _>) = addHandler handler
    member inline __.Run(code: EffectfulCode<_, _, _>) = compile code ()
    member inline __.Run(handlerAdder: EffectfulCode<_, _, _> -> EffectfulCode<_, _, _>) = handlerAdder
    member inline self.Add handler = self.Yield handler
