(* module to create messages *)
open Binary
module S = Xstring
module Bson = Bson

let zero = pack_signed_32 0l

let create_header ~mlen ~req_id ~req_to ~opcode =
  (pack_signed_32 mlen) ^
  (pack_signed_32 req_id) ^
  (pack_signed_32 req_to) ^
  (pack_signed_32 opcode)

let _update opcode coll_name flags selector update =
  let selector_bson = Bson.document_to_bson selector in
  let update_bson = match update with
    | Some u -> Bson.document_to_bson u 
    | None -> "" in
  let coll_cstr = S.to_c_string coll_name in
  let flags = pack_signed_32 flags in
  (* 16 header 4 zero 4 flags *)
  let mlength = Int32.of_int (
    16 + 4 + S.length coll_cstr + 4 + S.length selector_bson +
    S.length update_bson ) in
  let header = create_header ~mlen:mlength ~req_id:0l ~req_to:0l
                             ~opcode:opcode in
  header ^ zero ^ coll_cstr ^ flags ^ selector_bson ^ update_bson

let update ~coll_name ~flags ~selector ~update =
  _update 2001l coll_name flags selector (Some update)

(* delete is just update without any update *)
let delete ~coll_name ~flags ~selector =
  _update 2006l coll_name flags selector None

let insert ~coll_name ~docs = 
  let doc_bson = List.fold_left (fun x y -> x ^ Bson.document_to_bson y) 
                                ""  docs in
  let coll_cstr = S.to_c_string coll_name in
  (* 16 is for the header, 4 is for the zero *)
  let mlength = Int32.of_int (16 + 4 + S.length coll_cstr + S.length doc_bson) in
  let header = create_header ~mlen:mlength ~req_id:0l ~req_to:0l
                             ~opcode:2002l in
  header ^ zero ^ coll_cstr ^ doc_bson

let query ?(ret_field_selector = []) ~coll_name ~flags ~num_skip 
          ~num_rtn ~query =
  let query_bson = Bson.document_to_bson query in
  let ret_field_selector_bson = Bson.document_to_bson ret_field_selector in
  let coll_cstr = S.to_c_string coll_name in
  let num_skip = pack_signed_32 num_skip in
  let num_rtn = pack_signed_32 num_rtn in
  let flags = pack_signed_32 flags in
  let mlength = Int32.of_int (16 + 4 + S.length coll_cstr + 
                              4 + 4 + S.length query_bson +
                              S.length ret_field_selector_bson) in
  let header = create_header ~mlen:mlength ~req_id:0l ~req_to:0l
                             ~opcode:2004l in
  header ^ flags ^ coll_cstr ^ num_skip ^ num_rtn ^ query_bson ^ 
  ret_field_selector_bson

