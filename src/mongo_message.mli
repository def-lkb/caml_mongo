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

(* depends on /bson/bson.mli *)

type msg = int32 -> string

val update :
  coll:string -> flags:int32 -> selector:Bson.document ->
  update:Bson.document -> msg

val delete : coll:string -> flags:int32 -> selector:Bson.document -> msg

val insert : coll:string -> docs:Bson.document list -> msg

val query : proj:Bson.document -> coll:string ->
  flags:int32 -> num_skip:int32 -> num_rtn:int32 -> query:Bson.document -> msg

val getmore: coll:string -> num_rtn:int32 -> cursor_id:int64 -> msg

val kill_cursors : int64 list -> msg
