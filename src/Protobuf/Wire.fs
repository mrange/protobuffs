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
namespace ProtobufFs

module Wire =
  open System.Runtime.InteropServices

  let inline sizeOfVarInt (v : uint64) : int =
    let rec loop sz v =
      let more = v >>> 7
      if more <> 0UL then
        loop (sz + 1) more
      else
        sz
    loop 0 v

  type Reader(bytes : byte [], b : int, e : int) =
    let mutable pos = b

    member x.Position = pos

    member x.ReadVarInt ([<Out>] v : uint64 byref) : bool =
      // Tail-recursion is often better than for/while,
      //  however F# doesn't allow capturing of byrefs nor allows
      //  byrefs as parameters to F# functions
      let first         = pos
      let mutable cont  = true
      let mutable res   = 0UL
      let mutable shift = 0
      while cont && pos < e do
        let b       = bytes.[pos]
        pos         <- pos + 1
        cont        <- (b >>> 7) <> 0uy
        res         <- res ||| (uint64 b <<< shift)
        shift       <- shift + 7

      if cont || first = pos then
        v <- 0UL
        false
      else
        v <- res
        true

    member x.ReadFixed32 ([<Out>] v : uint32 byref) : bool =
      // TODO: This could be improved on little endian machines like x64
      if pos + 4 <= e then
        v <-
          0u
          ||| (uint32 bytes.[pos + 0]) <<< 0
          ||| (uint32 bytes.[pos + 1]) <<< 8
          ||| (uint32 bytes.[pos + 2]) <<< 16
          ||| (uint32 bytes.[pos + 3]) <<< 24
        pos <- pos + 4
        true
      else
        false

    member x.ReadFixed64 ([<Out>] v : uint64 byref) : bool =
      // TODO: This could be improved on little endian machines like x64
      if pos + 8 <= e then
        v <-
          0UL
          ||| (uint64 bytes.[pos + 0]) <<< 0
          ||| (uint64 bytes.[pos + 1]) <<< 8
          ||| (uint64 bytes.[pos + 2]) <<< 16
          ||| (uint64 bytes.[pos + 3]) <<< 24
          ||| (uint64 bytes.[pos + 4]) <<< 32
          ||| (uint64 bytes.[pos + 5]) <<< 40
          ||| (uint64 bytes.[pos + 6]) <<< 48
          ||| (uint64 bytes.[pos + 7]) <<< 56
        pos <- pos + 4
        true
      else
        false

    member x.ReadBytes ([<Out>] v : byte [] byref) : bool =
      let mutable xx = 0UL
      match x.ReadVarInt () with
      | true  , l ->
        let l = int32 l
        let res = Array.zeroCreate l
        Array.blit bytes pos res 0 l
        pos <- pos + l
        v <- res
        true
      | false , _ ->
        v <- Array.empty
        false

  type Reader with
    member inline x.ReadAndMapVarInt (m : uint64 -> 'T, dv : 'T, [<Out>] v : 'T byref) : bool =
      match x.ReadVarInt () with
      | true  , r -> v <- m r ; true
      | false , _ -> v <- dv  ; false
    // TODO: Make sure these methods are properly inlined
    member inline x.ReadBool      ([<Out>] v : bool  byref) : bool = x.ReadAndMapVarInt ((<>) 0UL  , false , &v)
    member inline x.ReadInt32     ([<Out>] v : int32 byref) : bool = x.ReadAndMapVarInt (int32     , 0     , &v)
    member inline x.ReadInt64     ([<Out>] v : int64 byref) : bool = x.ReadAndMapVarInt (int64     , 0L    , &v)
    member inline x.ReadSInt32    ([<Out>] v : int32 byref) : bool = x.ReadAndMapVarInt (int32     , 0     , &v)
    member inline x.ReadSInt64    ([<Out>] v : int64 byref) : bool = x.ReadAndMapVarInt (int64     , 0L    , &v)
    member inline x.ReadSfixed32  ([<Out>] v : int32 byref) : bool =
      match x.ReadFixed32 () with
      | true  , r -> v <- int32 r ; true
      | false , _ -> v <- 0       ; false
    member inline x.ReadSfixed64  ([<Out>] v : int64 byref) : bool =
      match x.ReadFixed64 () with
      | true  , r -> v <- int64 r ; true
      | false , _ -> v <- 0L      ; false

  type Writer(bytes : byte [], b : int) =
    let mutable pos = b

    member x.Position = pos

    member x.WriteVarInt (v : uint64) : unit =
      let rec loop v =
        let more    = v >>> 7
        let res     = byte v &&& 0x7fuy
        let tbw     = if more <> 0UL then 0x80uy ||| res else res
        bytes.[pos] <- tbw
        pos         <- pos + 1
        if more <> 0UL then loop more
      loop v

    member x.WriteFixed32 (v : uint32) : unit =
      // TODO: This could be improved on little endian machines like x64
      bytes.[pos + 0] <- byte (v >>> 0)
      bytes.[pos + 1] <- byte (v >>> 8)
      bytes.[pos + 2] <- byte (v >>> 16)
      bytes.[pos + 3] <- byte (v >>> 24)
      pos <- pos + 4

    member x.WriteFixed64 (v : uint64) : unit =
      // TODO: This could be improved on little endian machines like x64
      bytes.[pos + 0] <- byte (v >>> 0)
      bytes.[pos + 1] <- byte (v >>> 8)
      bytes.[pos + 2] <- byte (v >>> 16)
      bytes.[pos + 3] <- byte (v >>> 24)
      bytes.[pos + 4] <- byte (v >>> 32)
      bytes.[pos + 5] <- byte (v >>> 40)
      bytes.[pos + 6] <- byte (v >>> 48)
      bytes.[pos + 7] <- byte (v >>> 56)
      pos <- pos + 8

    member x.WriteBytes (v : byte []) : unit =
      x.WriteVarInt (uint64 v.Length)
      Array.blit v 0 bytes pos bytes.Length
      pos <- pos + v.Length

  type Writer with
    member inline x.WriteBool     (v : bool )   : unit = x.WriteVarInt (if v then 1UL else 0UL)
    member inline x.WriteInt32    (v : int32)   : unit = x.WriteVarInt (uint64 v)
    member inline x.WriteInt64    (v : int64)   : unit = x.WriteVarInt (uint64 v)
    member inline x.WriteSInt32   (v : int32)   : unit = x.WriteVarInt (uint64 ((v <<< 1) ^^^ (v >>> 31)))
    member inline x.WriteSInt64   (v : int64)   : unit = x.WriteVarInt (uint64 ((v <<< 1) ^^^ (v >>> 63)))
    member inline x.WriteSfixed32 (v : int32)   : unit = x.WriteFixed32 (uint32 v)
    member inline x.WriteSfixed64 (v : int64)   : unit = x.WriteFixed64 (uint64 v)
