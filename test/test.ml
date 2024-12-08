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

(* -------------------------------------------------------------------------- *)

(* The abstract type [point]. *)

let point =
  declare_abstract_type()

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> int in
  declare "population" spec R.population C.population;

  (* TODO The list of points needs to be sorted,
          or we must ensure a deterministic order.
  let spec = iter (unit ^> list point) in
  declare "(fun f () -> iter)" spec
    (fun f () -> R.iter f)
    (fun f () -> C.iter f);
   *)

  let spec = unit ^> point in
  declare "fresh" spec R.fresh C.fresh;

  let spec = (R.can_be_dropped % point) ^> unit in
  declare "drop" spec R.drop C.drop;

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
  dprintf "          open FlatUnionFind;;\n";
  let fuel = 128 in
  main fuel
