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

let protofile = """
syntax = "proto3";

package BankAccount.Movements;

message AccountMovement {
  uint64  toAccountId   = 1 ;
  int64   amountInCents = 2 ;
  string  currency      = 3 [default = "SEK"];
}

message AccountMovements {
  uint64                    fromAccountId = 1 ;
  repeated AccountMovements movements     = 2 ;
}

package BankAccount.Movements2;

message AccountMovement {
  uint64  toAccountId   = 1 ;
  int64   amountInCents = 2 ;
  string  currency      = 3 [default = "SEK"];
}

message AccountMovements {
  uint64                    fromAccountId = 1 ;
  repeated AccountMovements movements     = 2 ;
}
"""

open System
open System.IO

open ProtobufFs.Compiler.TextGenerator.Generator
open ProtobufFs.Compiler.FsGenerator
open ProtobufFs.Specification.Parser

[<EntryPoint>]
let main argv =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
  match parse protofile with
  | ParseSuccessful spec ->
    let ctx       = gcontext ()
    let _, result = grun ctx (generateFs "Hello" [|"hello.proto", spec |])
    let p         = Path.GetFullPath "../../../HelloProto.fs"
    printfn "%s" result
    File.WriteAllText (p, result)
    printfn "Proto file created: %s" p
    0
  | ParseFailure (msg, _, _, _) ->
    printfn "Failed with: %A" msg
    999
