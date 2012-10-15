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

open Mongo_binary
module N = Lwt_unix
module S = Mongo_string
module Message = Mongo_message

type reply_header =
    { mlen : int32
    ; response_to : int32
    ; req_id : int32
    ; opcode : int32
    }

type reply_meta = {
  cursor_not_found : bool;
  query_failure : bool;
  shard_config_stale : bool;
  await_capable : bool;
}
type reply_body = {
  response_flags : reply_meta;
  cursor_id : int64;
  starting_from : int32;
  num_docs : int32;
  docs : Bson.document Lwt_stream.t;
}

type socket = {
  socket : N.file_descr;
  socket_addr : N.sockaddr;
  socket_id : int;
  socket_rev : int;
  socket_next_req : int ref;
  socket_reqs : (int32, (reply_header * reply_body) Lwt.u) Hashtbl.t;
}

type init = {
  init_status : int option array;
  init_func : socket -> unit Lwt.t
}

type t (* this is the connection pool *) = {
  pool : socket Lwt_pool.t;
  num_conn : int;
  hostname : string;
  port : int;
  init : init option
}

type raw_cursor = {
  conn : t;
  coll : string;
  mutable remaining: int option; (* limit *)
  mutable reply : reply_body;
}

type cursor = Bson.document Lwt_stream.t 
type delete_option = [ `All | `One ]

module Communicate = struct
  let send_message msg { socket } =
    let mlen = (String.length msg) in
    let rec loop_write off =
      let len = mlen - off in
      lwt res = N.write socket msg off len in
      if res = len
      then Lwt.return_unit
      else loop_write (off + res)
    in
    lwt () = loop_write 0 in
    Lwt.return_unit

  let parse_reply_header header =
    let readat = unpack_signed_32 ~buf:header in
    {
      mlen = readat ~pos:0;
      req_id = readat ~pos:4;
      response_to = readat ~pos:8;
      opcode = readat ~pos:12
    }

  let parse_response_flags f =
    {
      cursor_not_found = Int32.logand f 1l = 1l;
      query_failure = Int32.logand f 2l = 2l;
      shard_config_stale = Int32.logand f 4l = 4l;
      await_capable = Int32.logand f 8l = 8l;
    }

  let parse_reply_body body =
    let readat32 = unpack_signed_32 ~buf:body in
    let readat64 = unpack_signed_64 ~buf:body in
    {
      response_flags = parse_response_flags (readat32 ~pos:0);
      cursor_id = readat64 ~pos:4;
      starting_from = readat32 ~pos:12;
      num_docs = readat32 ~pos:16;
      docs = Lwt_stream.of_list (Bson.bson_to_multidocs (S.drop body 20))
    }

  let read_message_body mlen sock =
    let buffer_size = 4096 in
    let buffer = String.create 4096 in
    let rec reader msg_sofar n =
      if n >= mlen then Lwt.return msg_sofar
      else begin
        lwt bytes_read = N.read sock buffer 0 buffer_size in
        reader (msg_sofar ^ (String.sub buffer 0 bytes_read))
          (Int32.add n (Int32.of_int bytes_read))
      end in
    reader "" 0l

  let socket_reader socket socket_reqs =
    let rec loop () =
      let header_buffer = String.create 16 in
      match N.state socket with
        | N.Closed | N.Aborted _ -> Lwt.return_unit
        | N.Opened ->
            match_lwt N.read socket header_buffer 0 16 with
              | 16 ->
                  let header = parse_reply_header header_buffer in
                  begin
                    try
                      let u = Hashtbl.find socket_reqs header.response_to in
                      Hashtbl.remove socket_reqs header.response_to;
                      lwt body = read_message_body
                        (Int32.sub header.mlen 16l) socket in  
                      Lwt.wakeup u (header, parse_reply_body body);
                      loop ()
                    with Not_found ->
                      raise_lwt (Failure (Printf.sprintf "unknown request_id, response_to = %ld" header.response_to))
                  end
              | _ ->
                  raise_lwt (Failure (Printf.sprintf "unexpected content")) 
    in
    loop ()

  let send_and_receive_message msg conn req_id =
    let t,u = Lwt.wait () in
    Hashtbl.add conn.socket_reqs req_id u;
    lwt () = send_message (msg req_id) conn in
    Lwt.map (fun (header,body) -> body) t
end

let create_single_connection counter port hostname () =
  lwt host = N.gethostbyname hostname in
  let socket_addr = N.ADDR_INET (host.N.h_addr_list.(0), port) in
  let socket = N.socket N.PF_INET N.SOCK_STREAM 0 in
  lwt () = N.connect socket socket_addr in
  let socket_reqs = Hashtbl.create 5 in
  Lwt.async (fun () -> Communicate.socket_reader socket socket_reqs);
  let conn = { socket ; socket_addr ; socket_id = counter () ; socket_rev = 0 ; socket_next_req = ref 1 ; socket_reqs } in
  Lwt_gc.finalise (fun { socket } -> N.shutdown socket N.SHUTDOWN_ALL; N.close socket) conn;
  Lwt.return conn

let connect ?(num_conn = 10) ?(port = 27017) hostname =
  let counter =
    let value = ref 0 in
    (fun () -> let r = !value in incr value; r)
  in
  let pool = Lwt_pool.create num_conn (create_single_connection counter port hostname) in
  let conn = { num_conn ; port ; pool ; hostname ; init = None } in
  conn

let with_socket_init init_func conn =
  let init_status = Array.make conn.num_conn None in
  { conn with init = Some { init_status ; init_func }}

let batch_size = function
  | Some l -> let l' = min 10 l in (Int32.of_int l', Some (l - l'))
  | None   -> (10l, None)

let check_sock conn ({ socket_id ; socket_rev } as sock) =
  match conn.init with
    | None -> Lwt.return_unit;
    | Some { init_status ; init_func } ->
        match init_status.(socket_id) with
          | Some r when r = socket_rev ->
              Lwt.return_unit
          | _ ->
              lwt () = init_func sock in
              init_status.(socket_id) <- Some socket_rev;
              Lwt.return_unit

let next_req { socket_next_req } =
  let r = !socket_next_req in
  if r > 0
  then (incr socket_next_req; Int32.of_int r)
  else (socket_next_req := 2; 1l)

let use_pool conn ?sock f = match sock with
  | None -> Lwt_pool.use conn.pool (fun sock -> Lwt.bind (check_sock conn sock) (fun () -> f sock))
  | Some sock ->
      Lwt.bind (check_sock conn sock) (fun () -> f sock)

let use_pool_with_req conn ?sock f =
  use_pool conn ?sock (fun sock -> f sock (next_req sock))
    
    
module Cursor = struct
  type t = raw_cursor

  let kill_cursor { conn ; reply = { cursor_id } } =
    match cursor_id with
      | 0L -> Lwt.return_unit
      | n ->
          let msg = Message.kill_cursors [n] 0l in
          use_pool conn (Communicate.send_message msg)

  let next_batch = function
    | { reply = { cursor_id = 0L ; docs } } ->
        Lwt.return_none
    | { reply = { response_flags = { cursor_not_found = true } } } as c ->
        Lwt.ignore_result (kill_cursor c);
        Lwt.return_none
    | cursor ->
        let batch, remaining = batch_size cursor.remaining in
        cursor.remaining <- remaining;
        let msg = Message.getmore ~coll:cursor.coll
          ~num_rtn:batch ~cursor_id:cursor.reply.cursor_id in
        lwt reply = use_pool_with_req cursor.conn
          (Communicate.send_and_receive_message msg)
        in
        cursor.reply <- reply;
        lwt cursor_end = Lwt_stream.is_empty reply.docs in
        if cursor_end
        then begin
          Lwt.ignore_result (kill_cursor cursor);
          Lwt.return_none
        end
        else Lwt.return (Some reply.docs)

  let make_gcable cursor =
    Lwt_gc.finalise_or_exit kill_cursor cursor;
    cursor

  let to_stream cursor = Lwt_stream.append 
    cursor.reply.docs
    (Lwt_stream.concat (Lwt_stream.from (fun () -> next_batch cursor)))
end

let update conn ?(upsert = false) ?(multi = false) ?sock ~coll ~query:selector update =
  let flags = match upsert, multi with
    | false, false -> 0l
    | true, false -> 1l
    | false, true -> 2l
    | true, true -> 3l in
  let msg = Message.update ~coll:coll ~flags:flags 0l 
    ~selector:selector ~update:update in
  use_pool conn ?sock (Communicate.send_message msg)

let insert conn ?sock ~coll docs =
  match docs with
    | [] -> Lwt.return_unit
    | _ ->
        let msg = Message.insert ~coll:coll ~docs:docs 0l in
        use_pool conn ?sock (Communicate.send_message msg)

let delete conn ?sock ~coll ?(mode = `All) selector =
  let flags = (function `All -> 0l | `One -> 1l) mode in
  let msg = Message.delete ~coll:coll ~flags:flags ~selector 0l  in
  use_pool conn ?sock (Communicate.send_message msg)

let find conn ?limit ?(skip=0) ?(proj = []) ?sock ~coll selector =
  let batch, remaining = batch_size limit in
  let msg = Message.query ~proj
    ~coll:coll ~query:selector ~flags:0l
    ~num_skip:(Int32.of_int skip) ~num_rtn:batch in
  lwt reply = use_pool_with_req ?sock conn
    (Communicate.send_and_receive_message msg)
  in
  Lwt.return (Cursor.to_stream (Cursor.make_gcable { conn ; coll ; remaining ; reply }))

let find_one conn ?skip ?proj ?sock ~coll doc =
  Lwt.bind
    (find conn ?skip ~limit:1 ?proj ?sock ~coll doc)
    Lwt_stream.get

let run_command conn ?sock ~db ?(args=[]) command =
  find_one conn ?sock ~coll:(db ^ ".$cmd") ((command, Bson.Int32 1l) :: args)

let admin_command conn ?sock ?args =
  run_command conn ?sock ~db:"admin" ?args

let pw_hash ~user ~pass =
  Digest.to_hex (Digest.string (user ^ ":mongo:" ^ pass))

let pw_key ~nonce ~user ~pass =
  Digest.to_hex (Digest.string (nonce ^ user ^ pw_hash user pass))
    
let auth ~user ~pass ~db conn =
  let auth' sock =
    match_lwt (run_command conn ~sock ~db "getnonce") with
      | None -> raise_lwt (Invalid_argument "authentication")
      | Some nonce_doc ->
          begin
            match (try List.assoc "nonce" nonce_doc with Not_found -> Bson.Null) with
              | Bson.String nonce as b_nonce ->
                  begin
                    let key = pw_key ~nonce ~user ~pass in
	            match_lwt (run_command conn ~sock ~db
       			              ~args:[ "user", Bson.String user
       			                    ; "nonce", b_nonce
       			                    ; "key", Bson.String key
       			                    ]
                                      "authenticate")
                    with
                      | None -> raise_lwt (Invalid_argument "authentication") 
                      | Some result ->
                          match List.assoc "ok" result with
                            | Bson.Double 1. -> Lwt.return_unit
                            | _ -> raise_lwt (Invalid_argument "authentication")
                  end
              | _ -> raise_lwt (Invalid_argument "authentication")
          end
  in
  with_socket_init auth' conn
