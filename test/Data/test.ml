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
module C = FlatUnionFind.MakeWithData(struct type t = int end)()

let () =
  dprintf "          open FlatUnionFind.MakeWithData(struct type t = int end)();;\n"

(* -------------------------------------------------------------------------- *)

(* The abstract type [point]. *)

let point =
  declare_abstract_type()

(* The concrete type [data]. *)

let data =
  lt 32

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> int in
  declare "population" spec R.population C.population;

  (* We do not test [iter]. *)

  let spec = data ^> point in
  declare "make" spec R.make C.make;

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

  let spec = (valid % point) ^> data in
  declare "get" spec R.get C.get;

  let spec = (valid % point) ^> data ^> unit in
  declare "set" spec R.set C.set;

  let spec = (valid % point) ^> (valid % point) ^> point in
  declare "merge (-)" spec (R.merge (-)) (C.merge (-));

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel
