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

option BuildLegacyWrapper = true;

option SomeString = "\101\x42\X43\tD'\"";
option SomeInt    = 123;
option SomeFloat  = -123.456;

import "root/fwk/legacy.proto";
import "google/protobuf/any.proto";

enum Test {
  X     = 3;
  XY    = 5 [];
  XYZ   = 7 [DoIt=true, Hello="90"];
}

message Hello {
  reserved  1, 2, 20 to 31, 32;
  reserved  hello, there;
  int32 There             = 1;
  repeated int32 content  = 2 [packed=false];
  map<string, int32> dic  = 3;

  oneof NameOrId {
    string      name        = 4;
    int32       id          = 5;
  }
}

service SearchService {
  option REST             = true;
  option (Some.Stuff).a.b = true;
  rpc Search (SearchRequest) returns (SearchResponse);
}

package BankAccount.Movements;
"""

[<EntryPoint>]
let main argv =
  printfn "%A" <| ProtobufFs.Specification.Parser.parse protofile
  0
