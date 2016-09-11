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

module FsGenerator =
  open FSharp.Core.Printf

  open Common
  open TextGenerator
  open TextGenerator.Generator
  open CommonGenerator

  open ProtobufFs.Specification.Model

  type DeclaredType =
    | DeclaredEnum      of EnumDefinition
    | DeclaredMessage   of MessageDefinition

  module Internals =
    module Env =
      let CurrentScope  : EnvironmentKey<_> = genvKey "PBF__CURRENT_SCOPE"  []
      let DeclaredTypes : EnvironmentKey<_> = genvKey "PBF__DECLARED_TYPES" Map.empty
      let Namespace     : EnvironmentKey<_> = genvKey "PBF__NAMESPACE"      "ProtobufFs"

  open Internals

  let computeType (ns : string) (path : string []) =
    let ctx = Array.append [|ns|] path.[0..(path.Length - 1)] |> List.ofArray |> List.rev
    let nsn = Array.append [|ns|] path.[0..(path.Length - 2)] |> joinPath
    let idn = path.[path.Length - 1]
    ctx, nsn, idn

  let rec gresolve (TypeId (rooted, path)) : Generator<DeclaredType option> =
    generator {
      let! declaredTypes = greadEnv Env.DeclaredTypes
      let! currentScope  = greadEnv Env.CurrentScope
      
      return
        if rooted then
          Map.tryFind (path |> List.ofArray) declaredTypes
        else
          let rec loop cs path =
            match cs, Map.tryFind path declaredTypes with
            | _       , Some dt -> Some dt 
            | []      , None    -> None
            | c::cs   , None    -> loop cs (c::path)
          loop currentScope (path |> List.ofArray)
    }

  let rec gfsType repeated d =
    // TODO: Cache as globals
    let fs_bool    = "bool"               , "false"
    let fs_int32   = "int32"              , "0"
    let fs_uint32  = "uint32"             , "0U"
    let fs_int64   = "int64"              , "0L"
    let fs_uint64  = "uint64"             , "0UL"
    let fs_float32 = "float32"            , "0.f"
    let fs_float64 = "float"              , "0."
    let fs_string  = "string"             , "\"\""
    let fs_bytes   = "ResizeArray<byte>"  , "ResizeArray ()"
    if repeated then
      gfsType false d
      >>= fun (t, _) ->
        let t     = sprintf "ResizeArray<%s>" t
        let dv    = "ResizeArray ()"
        greturn (t, dv)
    else
      match d with
      | TypeDefinition.Bool              -> greturn fs_bool
      | TypeDefinition.Int32             -> greturn fs_int32
      | TypeDefinition.Int64             -> greturn fs_int64
      | TypeDefinition.Uint32            -> greturn fs_uint32
      | TypeDefinition.Uint64            -> greturn fs_uint64
      | TypeDefinition.Sint32            -> greturn fs_int32
      | TypeDefinition.Sint64            -> greturn fs_int32
      | TypeDefinition.Fixed32           -> greturn fs_uint32
      | TypeDefinition.Sfixed32          -> greturn fs_int32
      | TypeDefinition.Float32           -> greturn fs_float32
      | TypeDefinition.Fixed64           -> greturn fs_uint64
      | TypeDefinition.Sfixed64          -> greturn fs_int64
      | TypeDefinition.Float64           -> greturn fs_float64
      | TypeDefinition.Bytes             -> greturn fs_bytes
      | TypeDefinition.String            -> greturn fs_string
      | TypeDefinition.DeclaredType tid  ->
        // TODO: Handle rooted
        let (TypeId (rooted, path)) = tid
        let t = joinPath path
        gresolve tid
        >>! fun odt ->
          match odt with
          | Some (DeclaredEnum (Enum (Id id, _))) -> 
            t, sprintf "%s 0" id
          | Some (DeclaredMessage e) -> 
            t, "LanguagePrimitives.GenericZero"
          | None -> 
            t, "``Unrecognized type``" // TODO: Handle not found

  let gtodo d = gwriteLinef "// TODO: %A" d

  let gimport d = gtodo d

  let gpackage (Package (FullId path)) =
    greadEnv Env.Namespace
    >>= fun ns ->
      let ctx, nsn, idn = computeType ns path
      gwriteEnv Env.CurrentScope ctx
      >>. gwriteLinef "namespace %s" nsn
      >>. (gwriteLinef "module %s =" idn |> gindent)

  let goption d = gtodo d

  let gmessageField (MessageField (repeated, t, Id id, tag, options)) =
    // TODO: handle options like default value
    gfsType repeated t 
    >>= fun (fst, fsdv) ->
      gwriteLinef "member val %s : %s = %s with get, set" id fst fsdv

  let genumField (EnumField (Id id, Tag tag, options)) =
    gwriteLinef "| %s = %d" id tag

  let genumMember d =
    match d with
    | EnumMemberOption  d -> gtodo d
    | EnumMemberField   d -> genumField d

  let genumMembers ds = gforeach genumMember ds

  let genum (Enum (Id id, ds)) =
    let ms = 
      if ds.Length = 0 then 
        // TODO: Move into globals
        [| EnumField (Id "DEFAULT", Tag 0UL, [||]) |> EnumMemberField |] 
      else 
        ds
    gwriteLinef "type %s =" id
    >>. (genumMembers ms |> gindent)
    |> gdelimitedf "ENUM: %s" id
    |> gindent
    |> gindent

  let gmessageMember d =
    match d with
    | MessageMemberField    d -> gmessageField d
    | MessageMemberEnum     d -> gtodo d
    | MessageMemberInner    d -> gtodo d
    | MessageMemberOption   d -> gtodo d
    | MessageMemberOneOf    d -> gtodo d
    | MessageMemberMapField d -> gtodo d
    | MessageMemberReserved d -> gtodo d

  let gmessageMembers ds = gforeach gmessageMember ds

  let gmessage (Message (Id id, ds)) =
    gwriteLinef "type [<RequireQualifiedAccess>] %s() =" id
    >>. (gbetween (gwriteLine "class") (gwriteLine "end") (gmessageMembers ds |> gindent) |> gindent)
    |> gdelimitedf "MESSAGE: %s" id
    |> gindent 
    |> gindent

  let gservice d = gtodo d

  let gtop top =
    match top with
    | TopImport     d -> gimport  d
    | TopPackage    d -> gpackage d
    | TopOption     d -> goption  d
    | TopMessage    d -> gmessage d
    | TopEnum       d -> genum    d
    | TopService    d -> gservice d

  let gtops ds = gforeach gtop ds

  let gspecification ((path, ProtoV3 ds) : string*Specification) =
    gtops ds
    |> gdelimitedf "PROTO SPECIFICATION: %s" path

  let gspecifications specs = gforeach gspecification specs

  let generateFs ns (specs : (string*Specification) []) =
    let rec foldMessageMember (ctx, dts) d =
      let notModified = ctx, dts
      match d with
      | MessageMemberField    d -> notModified
      | MessageMemberEnum     d -> foldEnum     notModified d
      | MessageMemberInner    d -> foldMessage  notModified d
      | MessageMemberOption   d -> notModified
      | MessageMemberOneOf    d -> notModified
      | MessageMemberMapField d -> notModified
      | MessageMemberReserved d -> notModified

    and foldMessage (ctx, dts) d =
      let (Message (Id id, ms)) = d
      ms |> Array.fold foldMessageMember (ctx, dts |> Map.add (id::ctx) (DeclaredMessage d))

    and foldEnum (ctx, dts) d =
      let (Enum (Id id, _) as e) = d
      ctx, dts |> Map.add (id::ctx) (DeclaredEnum e)

    let foldPackage (ctx, dts) d =
      let (Package (FullId path)) = d
      let ctx, _, _ = computeType ns path
      ctx, dts
      
    let rec foldTop (ctx, dts) d =
      let notModified = ctx, dts
      match d with
      | TopImport   d -> notModified
      | TopPackage  d -> foldPackage  notModified d
      | TopOption   d -> notModified
      | TopMessage  d -> foldMessage  notModified d
      | TopEnum     d -> foldEnum     notModified d
      | TopService  d -> notModified
    let dt = specs |> Array.fold (fun s (_, ProtoV3 tops) -> tops |> Array.fold foldTop s) ([ns], Map.empty)
    printfn "%A" dt
    gwriteEnv Env.CurrentScope []
    >>. gwriteEnv Env.DeclaredTypes dt
    >>. gwriteEnv Env.Namespace ns
    >>. gspecifications specs
    |> gdelimited "F# Protobuf wrappers"
