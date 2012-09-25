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

(** Basic combinators *)

let identity x = x
  
let ( ++ ) b1 b2 bson =
  try b1 bson
  with Not_found -> b2 bson

let ( ** ) b1 b2 bson =
  (b1 bson, b2 bson)

let maybe k a =
  try Some (k a)
  with Not_found -> None

let lift f g x =
  f (g x)

let rec first_of l x =
  match l with
    | [] -> raise Not_found
    | f :: fs ->
        try f x
        with Not_found ->
          first_of fs x

(** Documents *)

let key_as f name attribs = 
  try 
    f (List.assoc name attribs)
  with Invalid_argument _ | Not_found as exn ->
    Lwt_log.ign_error_f ~exn "Wrong type for attribute %s" name;
    raise Not_found

let key name attribs = 
  List.assoc name attribs

let assoc f l = 
  List.map (fun (k,v) -> k, f v) l

let assoc_key f l =
  List.map (fun (k,v) -> k, f k v) l

let document f = function
  | Bson.Document d -> f d
  | _ -> raise Not_found

(** Arrays *)

let array f = function
  | Bson.Array a -> f a
  | _ -> raise Not_found

let array_filter f a =
  Array.fold_right
    (fun cell acc -> 
        match (try Some (f acc) with Not_found -> None) with
          | Some x -> x :: acc
          | None   -> acc)
    a []

(** Atomic types *)

let bool = function
  | Bson.Boolean b -> b
  | _ -> raise Not_found
      
let double = function
  | Bson.Double f -> f
  | _ -> raise Not_found

let string = function
  | Bson.String s -> s
  | _ -> raise Not_found
      
let symbol = function
  | Bson.Symbol s -> s
  | _ -> raise Not_found

let int32 = function
  | Bson.Int32 i -> i
  | _ -> raise Not_found

let int64 = function
  | Bson.Int64 i -> i
  | _ -> raise Not_found

let object_id = function
  | Bson.ObjectId s -> s
  | _ -> raise Not_found

let null = function
  | Bson.Null -> ()
  | _ -> raise Not_found

let timestamp = function
  | Bson.Timestamp t -> t
  | _ -> raise Not_found

let utcdatetime = function
  | Bson.UTCDatetime t -> t
  | _ -> raise Not_found

(** Easier access to atomic types *)

let int = function
  | Bson.Int32 i -> Int32.to_int i
  | Bson.Int64 i -> Int64.to_int i
  | _ -> raise Not_found

let big_int = function
  | Bson.Int32 i -> Int64.of_int32 i
  | Bson.Int64 i -> i
  | _ -> raise Not_found

let real = function
  | Bson.Int32 i -> Int32.to_float i
  | Bson.Int64 i -> Int64.to_float i
  | Bson.Double d -> d
  | _ -> raise Not_found

(*type value =
  | BinaryData of (binary_subtype * string)
(*char is the subtype, string is the
  bytes of data  *)
  | Regex of (string * string)
  | JSCode of string
  | JSCodeWithScope of (string * document)
  | MinKey
  | MaxKey*)
