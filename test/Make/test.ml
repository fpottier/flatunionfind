(******************************************************************************)
(*                                                                            *)
(*                                  Skeleton                                  *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Monolith

(* This is the reference implementation. *)
module R = Reference
let valid = R.is_valid

(* This is the candidate implementation. *)
module C = FlatUnionFind.Make()

let () =
  dprintf "          open FlatUnionFind.Make();;\n"

(* Testing [iter] imposes a huge performance penalty (about 200x), so
   it is disabled by default. The reason why it costs so much is that
   one call to [iter] returns a list of all points created since the
   beginning of time, so (in the eyes of Monolith) it creates a large
   of number of (potentially) new points. *)

(* When [test_iter] is set, Monolith > 20241126 must be used; otherwise
   the error message "Argh -- reached max environment size" is observed. *)

let test_iter =
  false

(* -------------------------------------------------------------------------- *)

(* The abstract type [point]. *)

let point =
  declare_abstract_type()

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> int in
  declare "population" spec R.population C.population;

  (* The specification of [C.iter] does not indicate in what order the
     points are produced. We implement [R.iter] so that it uses the
     same order. This way, we can avoid a sorting step, which would be
     problematic, as we do not have an ordering function on points. *)
  if test_iter then begin
    let spec = iter (unit ^> list point) in
    declare "(fun f () -> iter)" spec
      (fun f () -> R.iter f)
      (fun f () -> C.iter f)
  end;

  let spec = unit ^> point in
  declare "fresh" spec R.fresh C.fresh;

  let spec = (R.can_be_dropped % point) ^> unit in
  declare "drop" spec R.drop C.drop;

  (* We test that [find], [is_representative], and [union] produce exactly
     the same results in the reference implementation and in the candidate
     implementation. This test is stricter than necessary: because [union]
     is in principle free to decide which of the two points becomes the
     representative of the new class, the two implementations could disagree.
     They happen to agree, so this strict test is acceptable. *)

  let spec = (valid % point) ^> point in
  declare "find" spec R.find C.find;

  let spec = (valid % point) ^> bool in
  declare "is_representative" spec R.is_representative C.is_representative;

  let spec = (valid % point) ^> (valid % point) ^> bool in
  declare "equal" spec R.equal C.equal;
  declare "equiv" spec R.equiv C.equiv;

  let spec = (valid % point) ^> (valid % point) ^> point in
  declare "union" spec R.union C.union;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = if test_iter then 8 else 128 in
  main fuel
