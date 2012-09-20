(* {{{ COPYING *(

  This file is part of a low-level binding for OCaml to Mongo database.

  Copyright (C) 2012  Frédéric Bour  <frederic.bour(_)lakaban.net>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

  Original code released under public domain by Kiyoto Tamura, see
  https://github.com/kiyoto/caml_mongo

)* }}} *)
type value =
  | Double of float
  | String of string
  | Document of document
  | Array of value array
  | ObjectId of string
  | BinaryData of (binary_subtype * string)
(*char is the subtype, string is the
  bytes of data  *)
  | Boolean of bool
  | UTCDatetime of int64
  | Null
  | Regex of (string * string)
  | JSCode of string
  | JSCodeWithScope of (string * document)
  | Symbol of string
  | Int32 of int32
  | Int64 of int64
  | Timestamp of int64
  | MinKey
  | MaxKey
and document = (string * value) list
and binary_subtype = Generic | Function | GenericOld
                     | UUID | MD5 | UserDefined of char

val document_to_bson : document -> string
val bson_to_document : string -> document
val bson_to_multidocs : string -> document list
