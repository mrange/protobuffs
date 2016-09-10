// ----------------------------------------------------------------------------------------------
// Copyright 2016 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
namespace ProtobufFs.Compiler

module TextGenerator =
  open FSharp.Core.Printf
  open System.Collections.Generic
  open System.Text

  type GeneratorOutput() =
    let sb            = StringBuilder 16
    let mutable ind   = 0
    let mutable hpos  = 0

    let indent () = if ind > hpos then sb.Append (' ', ind - hpos) |> ignore; hpos <- ind

    member x.Value = sb.ToString ()

    member x.NewLine () : unit =
      sb.AppendLine () |> ignore
      hpos <- 0

    member x.Write (msg : string) : unit =
      indent ()

      let inline flush b e    = if b < e then sb.Append (msg, b, e - b) |> ignore
      let inline newLine b e  = flush b e; x.NewLine (); indent ()
      let rec expectAnyChar p b i =
        if i < msg.Length then
          match msg.[i] with
          | '\n'  -> newLine b i; expectAnyChar 0 (i + 1) (i + 1)
          | '\r'  ->
            newLine b i
            let i = i + 1
            if i < msg.Length then
              match msg.[i] with
              | '\n'  -> expectAnyChar 0 (i + 1) (i + 1)
              | _     -> expectAnyChar 0 i i
            else
              0
          | _     -> expectAnyChar (p + 1) b (i + 1)
        else
          flush b i
          p

      hpos <- expectAnyChar hpos 0 0

    member x.WriteLine (msg : string) : unit =
      x.Write msg
      x.NewLine ()

    member x.IndentedWrite (indent : int) (msg : string) : unit =
      let p = x.ReadIndent ()
      x.IncrementIndent indent
      x.Write msg
      x.WriteIndent p

    member x.IndentedWriteLine (indent : int) (msg : string) : unit =
      x.IndentedWrite indent msg
      x.NewLine ()

    member x.IncrementIndent (i : int) : unit = ind <- max 0 (ind + i)
    member x.ReadIndent () : int              = ind
    member x.WriteIndent (i : int) : unit     = ind <- max 0 i

  type GeneratorContext(output : GeneratorOutput, parent : GeneratorContext option) =
    let env = Dictionary<string, obj> ()

    member x.Output = output

    member x.ReadEnv (k : string) (dv : 'T) : 'T =
      match env.TryGetValue k with
      | true, (:? 'T as v)  -> v
      | _   , _             ->
        match parent with
        | Some p  -> p.ReadEnv k dv
        | _       -> dv
    member x.WriteEnv (k : string) (v : 'T) : unit =
      env.[k] <- box v

  type Generator<'T> = GeneratorContext -> 'T

  module Generator =

    module Details =
      module Loop =
        let rec gmany ctx (ts : Generator<'T> []) (r : 'T []) i  =
          if i < ts.Length then
            let t = ts.[i]
            r.[i] <- t ctx
            gmany ctx ts r (i + 1)

    open Details

    // Monad
    let inline gbind t uf     : Generator<'U> =
      fun ctx ->
        let tv  = t ctx
        let u   = uf tv
        u ctx
    let inline (>>=) t uf     = gbind t uf
    let inline greturn v      : Generator<'T> = fun ctx -> v
    let gzero ()              = greturn ()

    // Arrow
    let inline garr f         = f >> greturn
    let inline gkleisli tf uf = fun v -> (tf v) >>= uf
    let inline (>=>) tf uf    = gkleisli tf uf

    // Applicative
    let inline gpure v        = greturn v
    let inline gap tf u       = tf >>= fun f -> u >>= garr f
    let inline (<*>) tf u     = gap tf u

    // Functor
    let inline gmap m t       = t >>= garr m
    let inline (>>!) t m      = gmap m t

    // Misc
    let gcontext () : GeneratorContext = GeneratorContext (GeneratorOutput (), None)
    let grun (ctx : GeneratorContext) (t : Generator<'T>) : 'T*string =
      let tv  = t ctx
      tv, ctx.Output.Value
    let inline gscope (t : Generator<'T>) : Generator<'T> =
      fun ctx ->
        let ctx = GeneratorContext (ctx.Output, Some ctx)
        t ctx

    let inline gkeepLeft t u  : Generator<'T> =
      fun ctx ->
        let tv = t ctx
        u ctx |> ignore
        tv
    let inline (.>>) t u      = gkeepLeft t u
    let inline gkeepRight t u : Generator<'U> =
      fun ctx ->
        t ctx |> ignore
        u ctx
    let inline (>>.) t u      = gkeepRight t u
    let inline gpair t u      : Generator<'T*'U> =
      fun ctx ->
        let tv = t ctx
        let uv = u ctx
        tv, uv
    let inline (.>>.) t u     = gpair t u

    let gbetween b e t        = b >>. t .>> e

    let inline gignore (t : Generator<_>) : Generator<unit> =
      fun ctx ->
        t ctx |> ignore

    let inline gmany (ts : Generator<'T> []) : Generator<'T []> =
      fun ctx ->
        let r = Array.zeroCreate ts.Length
        Loop.gmany ctx ts r 0
        r

    let inline gforeach (tf : 'T -> Generator<_>) (vs : 'T []) : Generator<unit> =
      gmany (vs |> Array.map tf) |> gignore

    let inline gforeachi (tf : int -> 'T -> Generator<_>) (vs : 'T []) : Generator<unit> =
      gmany (vs |> Array.mapi tf) |> gignore

    let gnewLine : Generator<unit> =
      fun ctx ->
        ctx.Output.NewLine ()
    let inline gwrite msg : Generator<unit> =
      fun ctx ->
        ctx.Output.Write msg
    let inline gwriteLine msg : Generator<unit> =
      fun ctx ->
        ctx.Output.WriteLine msg
    let inline gwritef     format = kprintf gwrite     format
    let inline gwriteLinef format = kprintf gwriteLine format
    let giwrite indent msg : Generator<unit> =
      fun ctx ->
        ctx.Output.IndentedWrite indent msg
    let giwriteLine indent msg : Generator<unit> =
      fun ctx ->
        ctx.Output.IndentedWriteLine indent msg
    let inline giwritef     indent format = kprintf (giwrite     indent) format
    let inline giwriteLinef indent format = kprintf (giwriteLine indent) format

    let inline greadEnv k dv : Generator<'T> =
      fun ctx ->
        ctx.ReadEnv k dv
    let inline gwriteEnv k v : Generator<unit> =
      fun ctx ->
        ctx.WriteEnv k v

    let inline gincrementIndent (i : int) (t : Generator<'T>) : Generator<'T> =
      fun ctx ->
        let pi = ctx.Output.ReadIndent ()
        ctx.Output.IncrementIndent i
        let tv = t ctx
        ctx.Output.WriteIndent pi
        tv
    let greadIndent : Generator<int> =
      fun ctx ->
        ctx.Output.ReadIndent ()

    type GeneratorBuilder() =
      member inline x.Bind        (t, uf) = gbind       t uf
      member inline x.Combine     (t, u)  = gkeepRight  t u
      member inline x.Return      v       = greturn     v
      member inline x.ReturnFrom  t       = t
      member inline x.Zero        ()      = gzero       ()

  let generator = Generator.GeneratorBuilder()
