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
namespace ProtobufFs.Specification

// FParsec:             http://www.quanttec.com/fparsec/reference/
// Google Protobuf v3:  https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
module Parser =

  module internal Internal =
    open System
    open FParsec
    open FParsec.CharParsers
    open Model

    let pws           = spaces
    let pws1          = spaces1
    let pch c         = skipChar c >>. pws
    let pstr s        = skipString s >>. pws
    let pch1 c        = skipChar c >>. pws1
    let pstr1 s       = skipString s >>. pws1
    let pflag p       = (p >>% true) <|>% false
    let pend          = pch ';'
    let pbool         = (skipString "true" >>% true) <|> (skipString "false" >>% false) .>> pws
    let pnumber       =
      // TODO: Support hex/oct
      let numberFormat =
        NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent

      numberLiteral numberFormat "number"
        |>> fun nl ->
            if nl.IsInteger then
              ConstantInteger (int64 nl.String)
            else
              ConstantFloat (float nl.String)
    let ptag          = puint64 .>> pws |>> Tag // TODO: Hex and Oct literals
    let pstring       =
      let poct      =
        let isOct c   = c >= '0' &&  c <= '7'
        let mapOct c  = int c - int '0'
        satisfy isOct |>> mapOct
      let phex      =
        let isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
        let mapHex c=
          if c >= '0' && c <= '9' then int c - int '0'
          elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
          else int c - int 'a' + 10
        satisfy isHex |>> mapHex
      let pnormal term =
        satisfy (function '\x00' | '\n' | '\\' -> false | c -> c <> term)
      let pescape     =
        skipChar '\\'
        >>. choice
          [
            charReturn  'a'   '\a'
            charReturn  'b'   '\b'
            charReturn  'f'   '\f'
            charReturn  'n'   '\n'
            charReturn  'r'   '\r'
            charReturn  't'   '\t'
            charReturn  'v'   '\v'
            charReturn  '\\'  '\\'
            charReturn  '\''  '\''
            charReturn  '"'   '"'
            pipe3 poct poct poct (fun a b c -> ((a <<< 6) ||| (b <<< 3) ||| c) &&& 0xFF |> char)
            pipe3 (skipChar 'x' <|> skipChar 'X') phex phex (fun a b c -> (b <<< 4) ||| c |> char)
          ]
      let pchar term  =
        choice
          [
            pnormal term
            pescape
          ]
      let pchars term = manyChars (pchar term)
      let b c fp      = between (skipChar c) (skipChar c) (fp c)
      choice
        [
          b '"'   pchars
          b '\''  pchars
        ]
        <?> "string" .>> pws
    let pbody p       = between (pch '{') (pch '}') (many p |>> List.toArray)


    let pid, pfullId, poptionId, ptypeId =
      let period  = skipChar '.'
      let pid     = many1Satisfy2 isLetter (fun ch -> isLetter ch || isDigit ch || ch = '_') <?> "identifier"
      let pids    = sepBy1 pid period |>> List.toArray
      let ptid    = pflag period .>>. pids .>> pws |>> TypeId
      let poid  =
        choice
          [
            pids .>> pws |>> OptionId
            pch '(' >>. pids .>> pch ')' .>>. (many (period >>. pid) |>> List.toArray) .>> pws |>> OptionPairId
          ]
      pid .>> pws |>> Id, pids .>> pws |>> FullId, poid, ptid

    let pconstant     =
      choice
        [
          pnumber
          pstring   |>> ConstantString
          pbool     |>> ConstantBoolean |> attempt    // Attempt needed to rollback for ids like truest
          pfullId   |>> ConstantFullId
        ] .>> pws
    let ptype         =
      let map =
        [|
          "int32"     , TypeDefinition.Int32
          "int64"     , TypeDefinition.Int64
          "uint32"    , TypeDefinition.Uint32
          "uint64"    , TypeDefinition.Uint64
          "sint32"    , TypeDefinition.Sint32
          "sint64"    , TypeDefinition.Sint64
          "fixed32"   , TypeDefinition.Fixed32
          "fixed64"   , TypeDefinition.Fixed64
          "sfixed32"  , TypeDefinition.Sfixed32
          "sfixed64"  , TypeDefinition.Sfixed64
          "float32"   , TypeDefinition.Float32
          "float64"   , TypeDefinition.Float64
          "bytes"     , TypeDefinition.Bytes
          "bool"      , TypeDefinition.Bool
          "string"    , TypeDefinition.String
        |] |> Map.ofArray
      let m (TypeId (rooted, path) as tn) =
        if not rooted && path.Length = 1 then
          match Map.tryFind path.[0] map with
          | Some v  -> v
          | None    -> DeclaredType tn
        else
          DeclaredType tn
      ptypeId |>> m
    let pkeyType      =
      let map =
        [|
          "int32"     , KeyTypeDefinition.Int32
          "int64"     , KeyTypeDefinition.Int64
          "uint32"    , KeyTypeDefinition.Uint32
          "uint64"    , KeyTypeDefinition.Uint64
          "sint32"    , KeyTypeDefinition.Sint32
          "sint64"    , KeyTypeDefinition.Sint64
          "fixed32"   , KeyTypeDefinition.Fixed32
          "fixed64"   , KeyTypeDefinition.Fixed64
          "sfixed32"  , KeyTypeDefinition.Sfixed32
          "sfixed64"  , KeyTypeDefinition.Sfixed64
          "bool"      , KeyTypeDefinition.Bool
          "string"    , KeyTypeDefinition.String
        |] |> Map.ofArray
      pid >>= fun (Id id) ->
        match Map.tryFind id map with
        | Some v  -> preturn v
        | None    -> fail "keytype"
    let popt          = poptionId .>> pch '=' .>>. pconstant |>> Option
    let popts         = between (pch '[') (pch ']') (sepBy popt (pch ',')) |>> List.toArray <|>% [||]

    let psyntax       = pstr "syntax" >>. pch '=' >>. (pstr "\"proto3\"" >>% ProtoV3) .>> pend
    let pimport       =
      let pic =
        choice
          [
            pstr1 "public" >>% Public
            pstr1 "weak"   >>% Weak
            preturn Private
          ]
      pstr1 "import" >>. pic .>>. pstring .>> pend |>> Import
    let ppackage      = pstr1 "package" >>. pfullId .>> pend |>> Package
    let poption       = pstr1 "option" >>. popt .>> pend
    let penumMember   =
      let pfield      = tuple3 (pid .>> pch '=') ptag (popts .>> pend) |>> EnumField
      choice
        [
          poption     |>> EnumMemberOption
          pfield      |>> EnumMemberField
          // TODO: Handle empty statement
        ]
    let penum         = pstr1 "enum" >>. pid .>>. pbody penumMember |>> Enum

    let poneOfMember  =
      let pfield      = tuple4 ptype pid (pch '=' >>. ptag) (popts .>> pend) |>> OneOfField
      choice
        [
          pfield      |>> OneOfMemberField
          // TODO: Handle empty statement
        ]

    let poneOf        = pstr1 "oneof" >>. pid .>>. pbody poneOfMember |>> OneOf

    let preserved     =
      let pcomma  = pch ','
      let prange  = pint32 >>= fun b -> pws1 >>. pstr1 "to" >>. pint32 <|>% b |>> fun e -> Range (b,e)
      // TODO: The spec says idents separated by commas but the examples are strings
      let pranges = sepBy1 prange pcomma  .>> pend |>> List.toArray
      let pnames  = sepBy1 pid pcomma     .>> pend |>> List.toArray
      let p =
        choice
          [
            pranges |>> ReservedRanges
            pnames  |>> ReservedNames
          ]
      pstr1 "reserved" >>. p

    let pmapField     =
      tuple5
        (pstr "map" >>. pch '<' >>. pkeyType)
        (pch ',' >>. ptype)
        (pch '>' >>. pid)
        (pch '=' >>. ptag)
        (popts .>> pend)
      |>> Map

    let pmessage, pmessageRef = createParserForwardedToRef ()

    let pmessageMember=
      let pfield      = tuple5 (pflag (pstr1 "repeated")) ptype pid (pch '=' >>. ptag) (popts .>> pend) |>> MessageField
      choice
        [
          penum       |>> MessageMemberEnum
          pmessage    |>> MessageMemberInner
          preserved   |>> MessageMemberReserved
          poneOf      |>> MessageMemberOneOf    |> attempt  // Attempt needed to rollback for option (which also starts on o)
          poption     |>> MessageMemberOption
          pmapField   |>> MessageMemberMapField |> attempt  // Attempt needed to rollback in case the type starts with map
          pfield      |>> MessageMemberField
          // TODO: Handle empty statement
        ]
    do
      pmessageRef     := pstr1 "message" >>. pid .>>. pbody pmessageMember |>> Message

    let pserviceMember=
      let prpc        =
        // TODO: Support "stream", the spec implies only one option and encapsulated in {}
        tuple4
          (pstr1 "rpc" >>. pid)
          (pch '(' >>. ptypeId .>> pch ')' .>> pstr "returns")
          (pch '(' >>. ptypeId .>> pch ')')
          (popts .>> pend)
        |>> Rpc
      choice
        [
          poption     |>> ServiceMemberOption
          prpc        |>> ServiceMemberRpc
          // TODO: Handle stream statement
          // TODO: Handle empty statement
        ]
    let pservice      = pstr1 "service" >>. pid .>>. pbody pserviceMember |>> Service

    let ptop =
      choice
        [
          pimport     |>> TopImport
          ppackage    |>> TopPackage
          poption     |>> TopOption
          penum       |>> TopEnum
          pmessage    |>> TopMessage
          pservice    |>> TopService
          // TODO: Handle empty statement
        ]

    let pspecification : Parser<_, unit> =
      pws >>. psyntax >>= fun f -> (many ptop |>> List.toArray) .>> eof |>> fun tops -> f tops

  type ParseResult =
    | ParseSuccessful of Model.Specification
    | ParseFailure    of string*int64*int64*int64
  let parse (proto : string) : ParseResult =
    let res = FParsec.CharParsers.runParserOnString Internal.pspecification () "protobuf" proto
    match res with
    | FParsec.CharParsers.ParserResult.Success (v, _, _) -> ParseSuccessful v
    | FParsec.CharParsers.ParserResult.Failure (m, e, _) -> ParseFailure (m, e.Position.Index, e.Position.Line, e.Position.Column)
