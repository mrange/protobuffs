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

module CommonGenerator =
  open FSharp.Core.Printf

  open Common
  open TextGenerator.Generator

  let hpos = 50
  let inline gindent t = gincrementIndent 2 t

  let gdelimiter =
    let delimiter =
      let t   = "// --"
      let f i =
        let l = 80 - i
        if l > t.Length then
          t + System.String ('-', l - t.Length)
        else
          "// --"
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


