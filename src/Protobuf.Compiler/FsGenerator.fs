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

    let defaultEnumMembers = [| EnumField (Id "DEFAULT", Tag 0UL, [||]) |> EnumMemberField |] 

    module Env =
      let CurrentScope  : EnvironmentKey<_> = genvKey "PBF__CURRENT_SCOPE"  []
      let DeclaredTypes : EnvironmentKey<_> = genvKey "PBF__DECLARED_TYPES" Map.empty
      let Namespace     : EnvironmentKey<_> = genvKey "PBF__NAMESPACE"      "ProtobufFs"

    module KnownTypes =
      module Converters =
        let bconv t dv c = 
          let ts v = if v then "true" else "false"
          match c with 
          | Some (ConstantFullId  _) -> t, dv   // TODO: This should mean referencing an option value
          | Some (ConstantInteger v) -> t, ts (v <> 0L)
          | Some (ConstantFloat   v) -> t, ts (v <> 0.)
          | Some (ConstantString  v) -> t, ts (v <> "")
          | Some (ConstantBoolean v) -> t, ts v
          | None                     -> t, dv

        let bsconv t dv c = 
          // TODO: Can bytes have default values?
          t, dv

        let nconv t dv c = 
          match c with 
          | Some (ConstantFullId  _) -> t, dv // TODO: This should mean referencing an option value
          | Some (ConstantInteger v) -> t, sprintf "%s %d" t v
          | Some (ConstantFloat   v) -> t, sprintf "%s %f" t v
          | Some (ConstantString  v) -> t, sprintf "%s \"%s\"" t v
          | Some (ConstantBoolean v) -> t, sprintf "%s %d" t (if v then 1 else 0)
          | None                     -> t, dv

        let sconv t dv c = 
          let es s = sprintf "\"%s\"" s // TODO: escape string
          match c with 
          | Some (ConstantFullId  (FullId path))  -> t, es (joinPath path)  // TODO: This should mean referencing an option value
          | Some (ConstantInteger v)              -> t, sprintf "\"%d\"" v
          | Some (ConstantFloat   v)              -> t, sprintf "\"%f\"" v
          | Some (ConstantString  v)              -> t, es v
          | Some (ConstantBoolean v)              -> t, if v then "\"true\"" else "\"false\""
          | None                                  -> t, dv

      open Converters
      
      let Bool    = bconv   "bool"              "false"         
      let Int32   = nconv   "int32"             "0"             
      let Uint32  = nconv   "uint32"            "0U"            
      let Int64   = nconv   "int64"             "0L"            
      let Uint64  = nconv   "uint64"            "0UL"           
      let Float32 = nconv   "float32"           "0.f"           
      let Float64 = nconv   "float"             "0."            
      let String  = sconv   "string"            "\"\""          
      let Bytes   = bsconv  "ResizeArray<byte>" "ResizeArray ()"

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
            | c::cs   , None    -> loop cs (path@[c])
          let res = loop currentScope (path |> List.ofArray)
          res
    }

  let rec gfsType (MessageField (repeated, t, id, tag, options)) =
    if repeated then
      gfsType (MessageField (false, t, id, tag, options))
      >>= fun (t, _) ->
        let t     = sprintf "ResizeArray<%s>" t
        let dv    = "ResizeArray ()"
        greturn (t, dv)
    else
      let optionName oid =
        match oid with
        | OptionId      path            -> joinPath path
        | OptionPairId  (first, second) -> Array.append first second |> joinPath

      let defaultOption = 
        options
        |> Array.tryPick (fun (Option (oid, c)) -> if optionName oid = "default" then Some c else None)
      match t with
      | TypeDefinition.Bool              -> greturn <| KnownTypes.Bool    defaultOption
      | TypeDefinition.Int32             -> greturn <| KnownTypes.Int32   defaultOption
      | TypeDefinition.Int64             -> greturn <| KnownTypes.Int64   defaultOption
      | TypeDefinition.Uint32            -> greturn <| KnownTypes.Uint32  defaultOption
      | TypeDefinition.Uint64            -> greturn <| KnownTypes.Uint64  defaultOption
      | TypeDefinition.Sint32            -> greturn <| KnownTypes.Int32   defaultOption
      | TypeDefinition.Sint64            -> greturn <| KnownTypes.Int32   defaultOption
      | TypeDefinition.Fixed32           -> greturn <| KnownTypes.Uint32  defaultOption
      | TypeDefinition.Sfixed32          -> greturn <| KnownTypes.Int32   defaultOption
      | TypeDefinition.Float32           -> greturn <| KnownTypes.Float32 defaultOption
      | TypeDefinition.Fixed64           -> greturn <| KnownTypes.Uint64  defaultOption
      | TypeDefinition.Sfixed64          -> greturn <| KnownTypes.Int64   defaultOption
      | TypeDefinition.Float64           -> greturn <| KnownTypes.Float64 defaultOption
      | TypeDefinition.Bytes             -> greturn <| KnownTypes.Bytes   defaultOption
      | TypeDefinition.String            -> greturn <| KnownTypes.String  defaultOption
      | TypeDefinition.DeclaredType tid  ->
        // TODO: Handle rooted
        let (TypeId (rooted, path)) = tid
        let t = joinPath path
        gresolve tid
        >>! fun odt ->
          match odt with
          | Some (DeclaredEnum (Enum (Id id, _))) -> 
            let dv =
              match defaultOption with 
              | Some (ConstantFullId  _) -> "enum 0"  // TODO: This should mean referencing an enum value
              | Some (ConstantInteger v) -> sprintf "enum %d" v
              | Some (ConstantFloat   v) -> sprintf "enum (int %f)" v
              | Some (ConstantString  v) -> sprintf "enum (int \"%s\")" v
              | Some (ConstantBoolean v) -> sprintf "enum %d" (if v then 1 else 0)
              | None                     -> "enum 0"
            t, dv
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
      >>. 
        (
          gwriteLine "open ProtobufFs.Wire"
          >>. gnewLine
          >>. gwriteLinef "module %s =" idn 
          |> gindent
        )

  let goption d = gtodo d

  let genumField (EnumField (Id id, Tag tag, options)) =
    gwriteLinef "| %s = %d" id tag

  let genumMember d =
    match d with
    | EnumMemberOption  d -> gtodo d
    | EnumMemberField   d -> genumField d

  let genumMembers ds = gforeach genumMember ds

  let genum (Enum (Id id, ds)) =
    let ds = if ds.Length = 0 then defaultEnumMembers else  ds
    gwriteLinef "type [<RequireQualifiedAccess>] %s =" id
    >>. (genumMembers ds |> gindent)
    |> gdelimitedf "ENUM: %s" id
    |> gindent
    |> gindent

  let gmessageFieldBacking (MessageField (repeated, t, Id id, tag, options) as d) =
    gfsType d
    >>= fun (fst, fsdv) ->
      gwriteLinef "let default_%s : %s = %s" id fst fsdv
      >>. gwriteLinef "let mutable backing_%s = default_%s" id id

  let gmessageMemberBacking d =
    match d with
    | MessageMemberField    d -> gmessageFieldBacking d
    | MessageMemberEnum     d -> gtodo d
    | MessageMemberInner    d -> gtodo d
    | MessageMemberOption   d -> gtodo d
    | MessageMemberOneOf    d -> gtodo d
    | MessageMemberMapField d -> gtodo d
    | MessageMemberReserved d -> gtodo d

  let gmessageFieldPublic (MessageField (repeated, t, Id id, tag, options) as d) =
    gfsType d
    >>= fun (fst, fsdv) ->
      gwriteLinef "member x.%s" id
      >>. 
        (
          gwriteLinef "with get () = backing_%s" id
          >>. gwriteLinef "and  set v  = backing_%s <- v" id
          |> gindent
        )

  let gmessageMemberPublic d =
    match d with
    | MessageMemberField    d -> gmessageFieldPublic d
    | MessageMemberEnum     d -> gtodo d
    | MessageMemberInner    d -> gtodo d
    | MessageMemberOption   d -> gtodo d
    | MessageMemberOneOf    d -> gtodo d
    | MessageMemberMapField d -> gtodo d
    | MessageMemberReserved d -> gtodo d

  let gmessageMembers tn ds = 

    gforeach gmessageMemberBacking ds
    >>. gnewLine
    >>. gwriteLinef "static member ComputeWireSize (x : %s) : int = 0" tn
    >>. gwriteLinef "static member Write  (w : Writer, x : %s) : unit = ()" tn
    >>. gwriteLinef "static member Read   (r : Reader, x : byref<%s>) : bool = false" tn
    >>. gnewLine
    >>. gforeach gmessageMemberPublic ds

  let gmessage (Message (Id id, ds)) =
    gwriteLinef "type %s() =" id
    >>. (gbetween (gwriteLine "class") (gwriteLine "end") (gmessageMembers id ds |> gindent) |> gindent)
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
      let (Enum (Id id, _) as d) = d
      ctx, dts |> Map.add (id::ctx) (DeclaredEnum d)

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
    let _, dt = specs |> Array.fold (fun s (_, ProtoV3 tops) -> tops |> Array.fold foldTop s) ([ns], Map.empty)
    gwriteEnv Env.CurrentScope []
    >>. gwriteEnv Env.DeclaredTypes dt
    >>. gwriteEnv Env.Namespace ns
    >>. gspecifications specs
    |> gdelimited "F# Protobuf wrappers"
