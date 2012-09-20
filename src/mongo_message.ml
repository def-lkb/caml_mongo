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

(* module to create messages *)
open Mongo_binary
module S = Mongo_string

type msg = int32 -> string

let zero = pack_signed_32 0l

let create_header ~mlen ~request_id ~response_to ~opcode =
  (pack_signed_32 mlen) ^
  (pack_signed_32 request_id) ^
  (pack_signed_32 response_to) ^
  (pack_signed_32 opcode)

let create_msg ?(flags=zero) ~response_to ~opcode body =
  let header_size = 16 in
  let mlen = List.fold_left (+) header_size (List.map S.length (flags :: body)) in
  fun request_id ->
    let header = create_header ~mlen:(Int32.of_int mlen)
      ~request_id ~response_to ~opcode in
    S.concat "" (header :: flags :: body)

let _update opcode coll flags selector update =
  let selector_bson = Bson.document_to_bson selector in
  let update_bson = match update with
    | Some u -> Bson.document_to_bson u
    | None -> "" in
  let coll_cstr = S.to_c_string coll in
  let flags = pack_signed_32 flags in
  create_msg ~response_to:0l ~opcode [coll_cstr;flags;selector_bson;update_bson]

let update ~coll ~flags ~selector ~update =
  _update 2001l coll flags selector (Some update)

(* delete is just update without any update *)
let delete ~coll ~flags ~selector =
  _update 2006l coll flags selector None

let insert ~coll ~docs =
  let doc_bson = List.fold_left (fun x y -> x ^ Bson.document_to_bson y)
                                ""  docs in
  let coll_cstr = S.to_c_string coll in
  create_msg ~response_to:0l ~opcode:2002l [coll_cstr;doc_bson]

let query ~proj ~coll ~flags ~num_skip
          ~num_rtn ~query =
  let query_bson = Bson.document_to_bson query in
  let proj_bson = Bson.document_to_bson proj in
  let coll_cstr = S.to_c_string coll in
  let num_skip = pack_signed_32 num_skip in
  let num_rtn = pack_signed_32 num_rtn in
  let flags = pack_signed_32 flags in
  create_msg ~flags ~response_to:0l ~opcode:2004l
    [coll_cstr;num_skip;num_rtn;query_bson;proj_bson]

let getmore ~coll ~num_rtn ~cursor_id =
  let coll_cstr = S.to_c_string coll in
  let num_rtn = pack_signed_32 num_rtn in
  let cursor_id = pack_signed_64 cursor_id in
  create_msg ~response_to:0l ~opcode:2005l [coll_cstr;num_rtn;cursor_id]

let kill_cursors cursor_ids =
  let num_cursor = Int32.of_int (List.length cursor_ids) in
  let num_cursor_packed = pack_signed_32 num_cursor  in
  let cursors = String.concat "" (List.map pack_signed_64 cursor_ids) in
  create_msg ~response_to:0l ~opcode:2007l [num_cursor_packed;cursors]
