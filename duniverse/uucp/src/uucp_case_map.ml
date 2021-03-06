(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let untagged_get m u = match Uucp_tmap.get m u with
| [] -> `Self | us -> `Uchars us

let to_upper u = untagged_get Uucp_case_map_data.upper_map_map (Uchar.to_int u)
let to_lower u = untagged_get Uucp_case_map_data.lower_map_map (Uchar.to_int u)
let to_title u = untagged_get Uucp_case_map_data.title_map_map (Uchar.to_int u)

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
