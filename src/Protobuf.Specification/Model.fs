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

module Model =

  type IdDefinition       = Id        of string
  type FullIdDefinition   = FullId    of string []
  type OptionIdDefinition =
    | OptionId      of string []
    | OptionPairId  of string []*string []
  type TypeIdDefinition   = TypeId    of bool*string []
  type RangeDefinition    = Range     of int*int

  type TagDefinition      = Tag of uint64

  type TypeDefinition =
    | Bool
    | Int32
    | Int64
    | Uint32
    | Uint64
    | Sint32
    | Sint64
    | Fixed32
    | Sfixed32
    | Float32
    | Fixed64
    | Sfixed64
    | Float64
    | Bytes
    | String
    | DeclaredType      of TypeIdDefinition

  type KeyTypeDefinition  =
    | Int32
    | Int64
    | Uint32
    | Uint64
    | Sint32
    | Sint64
    | Fixed32
    | Fixed64
    | Sfixed32
    | Sfixed64
    | Bool
    | String

  type ConstantDefinition =
    | ConstantFullId    of FullIdDefinition
    | ConstantInteger   of int64
    | ConstantFloat     of float
    | ConstantString    of string
    | ConstantBoolean   of bool

  type ImportClass =
    | Public
    | Weak
    | Private

  type ImportDefinition =
    | Import            of ImportClass*string

  type PackageDefinition =
    | Package           of FullIdDefinition

  type OptionDefinition =
    | Option            of OptionIdDefinition*ConstantDefinition

  type EnumFieldDefinition =
    | EnumField         of IdDefinition*TagDefinition*OptionDefinition []

  type EnumMemberDefinition =
    | EnumMemberOption  of OptionDefinition
    | EnumMemberField   of EnumFieldDefinition

  type EnumDefinition =
    | Enum              of IdDefinition*EnumMemberDefinition []

  type OneOfFieldDefinition =
    | OneOfField        of TypeDefinition*IdDefinition*TagDefinition*OptionDefinition []

  type OneOfMemberDefinition =
    | OneOfMemberField  of OneOfFieldDefinition

  type OneOfDefinition =
    | OneOf             of IdDefinition*OneOfMemberDefinition []

  type MapFieldDefinition =
    | Map               of KeyTypeDefinition*TypeDefinition*IdDefinition*TagDefinition*OptionDefinition []

  type ReservedDefinition =
    | ReservedRanges    of RangeDefinition []
    | ReservedNames     of IdDefinition []

  type MessageFieldDefinition =
    | MessageField      of bool*TypeDefinition*IdDefinition*TagDefinition*OptionDefinition []

  type MessageMemberDefinition =
    | MessageMemberField    of MessageFieldDefinition
    | MessageMemberEnum     of EnumDefinition
    | MessageMemberInner    of MessageDefinition
    | MessageMemberOption   of OptionDefinition
    | MessageMemberOneOf    of OneOfDefinition
    | MessageMemberMapField of MapFieldDefinition
    | MessageMemberReserved of ReservedDefinition

  and MessageDefinition =
    | Message         of IdDefinition*MessageMemberDefinition []

  type RpcDefinition = Rpc of IdDefinition*TypeIdDefinition*TypeIdDefinition*OptionDefinition []

  type ServiceMemberDefinition =
    | ServiceMemberOption   of OptionDefinition
    | ServiceMemberRpc      of RpcDefinition
    | ServiceMemberStream       // TODO:

  type ServiceDefinition =
    | Service         of IdDefinition*ServiceMemberDefinition []

  type TopDefinition =
    | TopImport       of ImportDefinition
    | TopPackage      of PackageDefinition
    | TopOption       of OptionDefinition
    | TopMessage      of MessageDefinition
    | TopEnum         of EnumDefinition
    | TopService      of ServiceDefinition

  type Specification =
    | ProtoV3 of TopDefinition []
