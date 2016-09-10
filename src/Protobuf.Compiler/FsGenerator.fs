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

module Common =
  open FSharp.Core.Printf

  open TextGenerator
  open Generator

  let joinPath (path : string []) : string = System.String.Join (".", path)

  let memo (f : 'T -> 'U) : 'T -> 'U =
    let dic = System.Collections.Concurrent.ConcurrentDictionary<'T, 'U> ()
    let ff  = System.Func<'T, 'U> f
    fun v -> dic.GetOrAdd (v, ff)

  let hpos = 50
  let inline gindent t = gincrementIndent 2 t

  let gdelimiter =
    let delimiter =
      let f i =
        let l = 80 - i
        if l > 0 then
          System.String ('-', l)
        else
          ""
      memo f
    greadIndent >>= (delimiter >> gwriteLine)

  let inline gdelimited m t =
    let gpre  =
      gdelimiter
      >>. gwriteLinef "// %s" m
      >>. gdelimiter
    let gpost =
      gdelimiter
      >>. gnewLine
    gbetween gpre gpost t
  let gdelimitedf format = kprintf gdelimited format

module FsGenerator =
  open FSharp.Core.Printf

  open TextGenerator
  open Generator

  open Common

  open ProtobufFs.Specification.Model
(*
  let tag ft i =
    let rec wt t =
      match t with
      | Bool
      | Int32
      | Int64
      | Uint32
      | Uint64
      | Sint32
      | Sint64            -> WireType.Varint
      | Fixed32
      | Sfixed32
      | Float32           -> WireType._32bit
      | Fixed64
      | Sfixed64
      | Float64           -> WireType._64bit
      | Bytes
      | String
      | DeclaredType tid  -> 
    (i <<< 3) ||| int (wt ft)
  let rec fsType ft =
    let s_bool    = "bool"
    let s_int32   = "int32"
    let s_uint32  = "uint32"
    let s_int64   = "int64"
    let s_uint64  = "uint64"
    let s_float32 = "float32"
    let s_float64 = "float"
    let s_string  = "string"
    let s_bytes   = "ResizeArray<byte>"
    match ft with
    | Bool              -> s_bool
    | Int32             -> s_int32
    | Int64             -> s_int64
    | Uint32            -> s_uint32
    | Uint64            -> s_uint64
    | Sint32            -> s_int32
    | Sint64            -> s_int32
    | Fixed32           -> s_uint32
    | Sfixed32          -> s_int32
    | Float32           -> s_float32
    | Fixed64           -> s_uint64
    | Sfixed64          -> s_int64
    | Float64           -> WireType._64bit
    | Bytes
    | String
    | DeclaredType tid  -> 
    | Bool                -> s_bool
    | Enum          e     -> e
    | Int32               -> s_int32
    | UInt32              -> s_uint32
    | SInt32              -> s_int32
    | Int64               -> s_int64
    | UInt64              -> s_uint64
    | SInt64              -> s_int64
    | FixedInt32          -> s_int32
    | FixedSInt32         -> s_int32
    | Float32             -> s_float32
    | FixedInt64          -> s_int64
    | FixedSInt64         -> s_int64
    | Float64             -> s_float64
    | String              -> s_string
    | Bytes               -> s_bytes
    | Message       m     -> m
    | PackedRepeat  t
    | Repeat        t     -> sprintf "std::vector<%s>" (cppType t)
    | Optional      t     -> sprintf "std::optional<%s>" (cppType t)
    | DefaultValue  (t, _)-> cppType t

  let rec cppInitializer ft =
    match ft with
    | DefaultValue (_, i) -> i
    | _                   -> ""

  let rec cppSuffix ft =
    match ft with
    | PackedRepeat  _     -> "PackedRepeat"
    | Repeat        _     -> "Repeat"
    | Optional      _     -> "Optional"
    | DefaultValue  (_, _)-> "DefaultValue"
    | _                   -> ""

  let rec isRequired ft =
    match ft with
    | Optional _      -> false
    | DefaultValue _  -> false
    | _               -> true

  let rec defaultValue ft =
    match ft with
    | DefaultValue (_, dv)  -> Some dv
    | _                     -> None

  let inline gnamespace n t =
    gbetween
      (gwriteLinef "namespace %s\n{" n)
      (gwriteLine "}")
      (gindent t)

  let gfield (FieldDefinition (t, n, id)) =
    gwrite (cppType t)
    >>. giwrite hpos n
    >>. gwriteLine ";"

  let gfields fs = gforeach gfield fs

  let gdefaultCtor n =
    let ctor msg  = gwrite msg >>. giwriteLine hpos "= default;"
    let ctorf fmt = kprintf ctor fmt
    ctorf "%s (%s const &)" n n
    >>. ctorf "%s (%s &&)" n n
    >>. ctorf "%s & operator= (%s const &)" n n
    >>. ctorf "%s & operator= (%s &&)" n n
    >>. gnewLine

  let gctor n fs =
    let initializer i (FieldDefinition (t, n, id)) =
      let sep = if i = 0  then ":" else ","
      gwritef "%s %s" sep n
      >>. giwriteLinef hpos "{%s}" (cppInitializer t)
    gwriteLinef "%s ()" n
    >>. (gforeachi initializer fs)
    >>. gwriteLine "{\n}"
    >>. gnewLine

  let gtagDescription : Generator<unit> =
    gwriteLine "// Tags are octal literals"
    >>. gwriteLine "//  lower octet is WireType"
    >>. gwriteLine "//    0 = Varint"
    >>. gwriteLine "//    1 = 64bit"
    >>. gwriteLine "//    2 = Length delimited"
    >>. gwriteLine "//    3 = Start group (deprecated)"
    >>. gwriteLine "//    4 = End group (deprecated)"
    >>. gwriteLine "//    5 = 32bit"
    >>. gwriteLine "//  upper octets forms the field id"

  let gserializeSubContext t =
    gwriteLine "std::uint64_t length = SizeOfs::sizeOf (v);"
    >>. gnewLine
    >>. gwriteLine "auto subCtx = ctx.createSubContext (length);"
    >>. gnewLine
    >>. t

  let gunserializeSubContext t =
    gwriteLine "std::uint64_t length = 0;"
    >>. gwriteLine "if (!ctx.readVarint (length)) return false; // TODO: Trace"
    >>. gnewLine
    >>. gwriteLine "auto subCtx = ctx.createSubContext (length);"
    >>. gnewLine
    >>. t

  let gmessage (MessageDefinition (n, fs)) =
    gwriteLinef "struct %s\n{" n
    >>. (gctor n fs >>. gdefaultCtor n >>. gfields fs |> gindent)
    >>. gwriteLine "};"
    |> gdelimitedf "STRUCT: %s" n

  let gmessages ms = gforeach gmessage ms

  let gfieldSizeOf (FieldDefinition (t, n, id)) =
    match defaultValue t with
    | Some dv ->
      gwriteLinef "sizeOf += sizeOf%s (ctx, 0%o, v.%s, %s); // %s" (cppSuffix t) (tag t id) n (cppType t) dv
    | None ->
      gwriteLinef "sizeOf += sizeOf%s (ctx, 0%o, v.%s); // %s" (cppSuffix t) (tag t id) n (cppType t)

  let gfieldSizeOfs fs = gforeach gfieldSizeOf fs

  let gmessageSizeOf (MessageDefinition (n, fs)) =
    gwriteLinef "std::uint64_t sizeOf (%s const & v)\n{" n
    >>.
      (
        gwriteLinef "auto sizeOf = 0;"
        >>. gtagDescription
        >>. gfieldSizeOfs fs
        >>. gwriteLinef "return sizeOf;"
        |> gindent
      )
    >>. gwriteLine "}"
    >>. gnewLine
    >>. gwriteLinef "std::uint64_t sizeOf (std::uint32_t tag, %s const & v)\n{" n
    >>.
      (
        gwriteLine "ASSERT_IS_LENGTH_DELIMITED_TAG(tag);"
        >>. gwriteLinef "return sizeOfTag (tag) + sizeOf (v);"
        |> gindent
      )
    >>. gwriteLine "}"
    |> gdelimitedf "SIZEOF: %s" n

  let gmessageSizeOfs ms =
    gforeach gmessageSizeOf ms
    |> gnamespace "SizeOfs"

  let gfieldSerializer (FieldDefinition (t, n, id)) =
    match defaultValue t with
    | Some dv ->
      gwriteLinef "serialize%s (ctx, 0%o, v.%s, %s); // %s" (cppSuffix t) (tag t id) n (cppType t) dv
    | None ->
      gwriteLinef "serialize%s (ctx, 0%o, v.%s); // %s" (cppSuffix t) (tag t id) n (cppType t)

  let gfieldSerializers fs = gforeach gfieldSerializer fs

  let gmessageSerializer (MessageDefinition (n, fs)) =
    gwriteLinef "void serialize (SerializeContext & ctx, %s const & v)\n{" n
    >>. (gtagDescription >>. gfieldSerializers fs |> gserializeSubContext |> gindent)
    >>. gwriteLine "}"
    >>. gnewLine
    >>. gwriteLinef "void serialize (SerializeContext & ctx, std::uint32_t tag, %s const & v)\n{" n
    >>.
      (
        gwriteLine "ASSERT_IS_LENGTH_DELIMITED_TAG(tag);"
        >>. gnewLine
        >>. gwriteLine "ctx.writeVarInt (tag);"
        >>. gnewLine
        >>. gwriteLine "serialize (subCtx, v);"
        |> gindent
      )
    >>. gwriteLine "}"
    |> gdelimitedf "SERIALIZE: %s" n

  let gmessageSerializers ms =
    gforeach gmessageSerializer ms
    |> gnamespace "Serializers"

  let gfieldUnserializer (req : int64 option, FieldDefinition (t, n, id)) =
    let updater =
      match req with
      | Some mask -> sprintf "seen |= 0x%x;" mask
      | None      -> ""
    match defaultValue t with
    | Some dv ->
      gwriteLinef "case 0%o: if (!unserialize%s (subCtx, tag, v.%s, %s)) return false; %s break; // %s" (tag t id) (cppSuffix t) n dv updater (cppType t)
    | None    ->
      gwriteLinef "case 0%o: if (!unserialize%s (subCtx, tag, v.%s)) return false; %s break; // %s" (tag t id) (cppSuffix t) n updater (cppType t)

  let gfieldUnserializers fs =
    let shift, ffs =
      let folder (shift, fs) (FieldDefinition (t, n, id) as f) =
        let req, nm =
          if isRequired t then
            Some (1L <<< shift), (shift + 1)
          else
            None, shift
        nm, (req, f)::fs
      fs
      |> Array.fold folder (0, [])
    let required =
      if shift = 64 then
        uint64 -1
      else
        (1UL <<< shift) - 1UL
    let ffs  =
      ffs
      |> List.rev
      |> List.toArray
    if shift > 64 then
      gwriteLine "ERROR: Too many required fields, 64 is the maximum number of required fields supported"
    else
      gwriteLine "while (subCtx.hasMoreData ())\n{"
      >>.
        (
          gwriteLine      "std::uint64_t seen     = 0x0;"
          >>. gwriteLinef "std::uint64_t required = 0x%x;" required
          >>. gnewLine
          >>. gwriteLine "auto tag = subCtx.readVarint ();"
          >>. gnewLine
          >>. gwriteLine "switch (tag)\n{"
          >>. gwriteLine "case 0: return false; // TODO: Invalid tag, trace"
          >>. gforeach gfieldUnserializer ffs
          >>. gwriteLine "default: subCtx.skipUnknownTag(tag); break;"
          >>. gwriteLine "}"
          |> gindent
        )
      >>. gwriteLine "}"

  let gmessageUnserializer (MessageDefinition (n, fs)) =
    gwriteLinef "bool unserialize (UnserializeContext & ctx, %s & v)\n{" n
    >>.
      (
        (gtagDescription >>. gfieldUnserializers fs |> gunserializeSubContext)
        >>. gnewLine
        >>. gwriteLine "return required == seen"
        |> gindent
      )
    >>. gwriteLine "}"
    >>. gnewLine
    >>. gwriteLinef "bool unserialize (UnserializeContext & ctx, std::uint32_t tag, %s & v)\n{" n
    >>.
      (
        gwriteLine "// Don't write the tag here as that has been read by the caller already"
        >>. gwriteLine "if (!isLengthDelimitedTag (tag)) return false; // TODO: Trace"
        >>. gnewLine
        >>. gwriteLine "return unserialize (subCtx, v);"
        |> gindent
      )
    >>. gwriteLine "}"
    |> gdelimitedf "UNSERIALIZE: %s" n

  let gmessageUnserializers ms =
    gforeach gmessageUnserializer ms
    |> gnamespace "Unserializers"

  let gfull model =
    gwriteLine license
    >>. gmessages model
    >>. gmessageSizeOfs        model
    >>. gmessageSerializers    model
    >>. gmessageUnserializers  model
    |> gnamespace "Model"
*)

  type DeclaredType =
    | DeclaredEnum    of EnumDefinition
    | DeclaredMessage of MessageDefinition

(*
  let rec gresolve (TypeId (rooted, path)) =
    generator {
      let! declaredTypes = greadEnv "DECLARED_TYPES" Map.empty
      let! currentScope  = greadEnv "CURRENT_SCOPE"  []
      
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
*)

  let rec fsType r d =
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
    if r then
      let t, _  = fsType false d
      let t     = sprintf "ResizeArray<%s>" t
      let dv    = "ResizeArray ()"
      t, dv
    else
      match d with
      | TypeDefinition.Bool              -> fs_bool
      | TypeDefinition.Int32             -> fs_int32
      | TypeDefinition.Int64             -> fs_int64
      | TypeDefinition.Uint32            -> fs_uint32
      | TypeDefinition.Uint64            -> fs_uint64
      | TypeDefinition.Sint32            -> fs_int32
      | TypeDefinition.Sint64            -> fs_int32
      | TypeDefinition.Fixed32           -> fs_uint32
      | TypeDefinition.Sfixed32          -> fs_int32
      | TypeDefinition.Float32           -> fs_float32
      | TypeDefinition.Fixed64           -> fs_uint64
      | TypeDefinition.Sfixed64          -> fs_int64
      | TypeDefinition.Float64           -> fs_float64
      | TypeDefinition.Bytes             -> fs_bytes
      | TypeDefinition.String            -> fs_string
      | TypeDefinition.DeclaredType tid  ->
        // TODO: Handle rooted
        let (TypeId (rooted, path)) = tid
        joinPath path, "LanguagePrimitives.GenericZero"

  let gtodo d = gwriteLinef "// TODO: %A" d
  
  let gimport d = gtodo d

  let gpackage (Package (FullId ns)) =
    let nm = joinPath ns
    gwriteLinef "module %s =" nm

  let goption d = gtodo d

  let gmessageField (MessageField (repeated, t, Id id, tag, options)) =
    // TODO: handle options like default value
    let fst, fsdv = fsType repeated t
    gwriteLinef "member val x.%s : %s = %s with get, set" id fst fsdv

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
    gwriteLinef "type %s() =" id
      >>. (gbetween (gwriteLine "class") (gwriteLine "end") (gmessageMembers ds |> gindent) |> gindent)
    |> gdelimitedf "MESSAGE: %s" id
    |> gindent

  let genumMember d =
    match d with
    | EnumMemberOption d  -> gtodo d
    | EnumMemberField d   -> gtodo d

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
    gwriteLinef "namespace %s" ns
    >>. gspecifications specs
    |> gdelimited "F# Protobuf wrappers"
