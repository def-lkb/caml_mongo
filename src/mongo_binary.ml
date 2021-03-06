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

(* packing and unpacking binary
 * Heavily reference Jane Street Core, but packing is done in place,
 * and per BSON's spec, little endian is used. *)

let pack_signed_32 n =
  let buf = String.create 4 in
    buf.[3] <-
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24));
    buf.[2] <-
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16));
    buf.[1] <-
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8));
    buf.[0] <- Char.unsafe_chr (0xFF land Int32.to_int n);
    buf

let unpack_signed_32 ~buf ~pos =
  Int32.logor
    (Int32.shift_left (Int32.of_int (Char.code buf.[pos + 3])) 24)
    (Int32.of_int
      ((Char.code buf.[pos + 2] lsl 16)
        lor (Char.code buf.[pos + 1] lsl 8)
        lor (Char.code buf.[pos])))

let pack_signed_64 v =
  let buf = String.create 8 in
    let top3 = Int64.to_int (Int64.shift_right v 40) in
    let mid3 = Int64.to_int (Int64.shift_right v 16) in
    let bot2 = Int64.to_int v in
    buf.[7] <- Char.unsafe_chr (0xFF land (top3 lsr 16));
    buf.[6] <- Char.unsafe_chr (0xFF land (top3 lsr 8));
    buf.[5] <- Char.unsafe_chr (0xFF land top3);
    buf.[4] <- Char.unsafe_chr (0xFF land (mid3 lsr 16));
    buf.[3] <- Char.unsafe_chr (0xFF land (mid3 lsr 8));
    buf.[2] <- Char.unsafe_chr (0xFF land mid3);
    buf.[1] <- Char.unsafe_chr (0xFF land (bot2 lsr 8));
    buf.[0] <- Char.unsafe_chr (0xFF land bot2);
    buf

let unpack_signed_64 ~buf ~pos =
  Int64.logor
    (Int64.logor
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos + 7] lsl 16
                        lor Char.code buf.[pos + 6] lsl 8
                        lor Char.code buf.[pos + 5]))
        40)
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos + 4] lsl 16
                        lor Char.code buf.[pos + 3] lsl 8
                        lor Char.code buf.[pos + 2]))
        16))
    (Int64.of_int (Char.code buf.[pos + 1] lsl 8
                    lor Char.code buf.[pos]))

let pack_float f = pack_signed_64 (Int64.bits_of_float f)

let unpack_float ~buf ~pos = Int64.float_of_bits (unpack_signed_64 buf pos)
