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

type t
type socket
type cursor = Bson.document Lwt_stream.t

type delete_option = [ `All | `One ]

val connect :
  ?num_conn:int -> ?port:int -> string -> t

val with_socket_init : (socket -> unit Lwt.t) -> t -> t

val update :
  t -> ?upsert:bool -> ?multi:bool -> ?sock:socket -> db:string -> coll:string ->
  query:Bson.document -> Bson.document -> unit Lwt.t

val insert :
  t -> ?sock:socket -> db:string -> coll:string -> Bson.document list -> unit Lwt.t

val delete :
  t -> ?sock:socket -> db:string -> coll:string -> ?mode:delete_option -> Bson.document -> unit Lwt.t

(* there are actually many options on find, but for now I am ignoring
 * all of them *)
val find :
  t -> ?limit:int -> ?skip:int -> ?proj:Bson.document ->
  ?sock:socket -> db:string -> coll:string -> Bson.document -> cursor Lwt.t

val find_one :
  t -> ?skip:int -> ?proj:Bson.document -> 
  ?sock:socket -> db:string -> coll:string -> Bson.document -> Bson.document option Lwt.t

val run_command : t -> ?sock:socket -> db:string -> string -> ?arg:Bson.value -> Bson.document -> Bson.document option Lwt.t

val admin_command : t -> ?sock:socket -> string -> ?arg:Bson.value -> Bson.document -> Bson.document option Lwt.t

(* authentication *)

val pw_hash : user:string -> pass:string -> string
val pw_key  : nonce:string -> user:string -> pass:string -> string

val auth : user:string -> pass:string -> db:string -> t -> t

(* count *)

val count : t -> ?limit:int -> ?skip:int -> ?sock:socket -> db:string -> coll:string -> Bson.document -> int Lwt.t
