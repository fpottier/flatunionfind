(******************************************************************************)
(*                                                                            *)
(*                               FlatUnionFind                                *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* This reference implementation exploits [UnionFind], a separate library. *)

(* Some work is required because [UnionFind] does not offer [drop]. We have
   to implement not only [drop], but also the dynamic tests [can_be_dropped]
   (which determines whether a point can be dropped) and [is_valid]
   (which determines whether a point has not been dropped). *)

(* A point is invalidated when [drop] is applied to it. The precondition
   [can_be_dropped] guarantees that such a point is isolated (it is the
   single member of its equivalence class). Thus, an equivalence class
   can never contain both valid and invalid points. This fact allows us
   to attach the validity flag to a class, and gives us an efficient way
   of testing whether a point is valid. *)

type flag =
  Valid | Invalid

type point =
  flag UnionFind.elem

(* [valid] stores a list of all valid points, in reverse order of
   their creation. *)

let valid : point Stack.t =
  Stack.create()

(* [pop] stores the number of valid points. *)

let pop : int ref =
  ref 0

let population () =
  !pop

(* [droppable] keeps track of the points that have been created since the
   last [union] operation. These points can be dropped (in order). *)

let droppable =
  Stack.create()

let fresh () =
  let p = UnionFind.make Valid in
  incr pop;
  Stack.push p valid;
  Stack.push p droppable;
  p

let is_representative =
  UnionFind.is_representative

let can_be_dropped p =
  match Stack.top_opt droppable with
  | Some p' ->
      p == p'
  | None ->
      false

let is_valid p =
  UnionFind.get p = Valid

let drop p =
  assert (can_be_dropped p);
  decr pop;
  let p' = Stack.pop valid in
  assert (p' == p);
  let p' = Stack.pop droppable in
  assert (p' == p);
  UnionFind.set p Invalid

let find =
  UnionFind.find

let equal =
  (==)

let equiv =
  UnionFind.eq

let union x y =
  Stack.clear droppable;
  UnionFind.union x y

let iter f =
  (* Stack.iter f valid *)
  (* We iterate from oldest to newest elements because we want to
     produce points in the same order as the candidate implementation. *)
  valid
  |> Stack.to_seq
  |> List.of_seq
  |> List.rev
  |> List.iter f
