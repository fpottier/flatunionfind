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

include Signatures

module V = Hector.Int

(* A parent is a point, that is, an index into the vector. *)
type parent = int

(* The rank of a vertex is the maximum length, in edges, of an uncompressed
   path that leads to this vertex. In other words, the rank of [x] is the
   height of the tree rooted at [x] that would exist if we did not perform
   path compression. *)
type rank = int

(* We encode the disjoint sum [parent + rank] as a single integer value.
   Our convention is as follows: a parent is encoded as itself; a rank
   [rank] is encoded as the negative integer [-(rank+1)]. *)
type info = int (* [parent] or [rank] *)

let[@inline] is_root (info : info) : bool =
  info < 0  (* info is a rank *)

let[@inline] has_parent (info : info) : bool =
  info >= 0 (* info is a parent *)

let[@inline] decode_parent (info : info) : parent =
  assert (has_parent info);
  info

let[@inline] encode_parent (parent : parent) : info =
  assert (0 <= parent);
  parent

let[@inline] decode_rank (info : info) : rank =
  assert (is_root info);
  -(info+1) (* [info = -(rank+1)], so [rank = -(info+1)]. *)

let[@inline] encode_rank (rank : rank) : info =
  assert (0 <= rank);
  -(rank+1)

module[@inline] Make () = struct

(* A union-find data structure is represented as a single flat vector,
   which maps a point (a valid index) to an info, that is, to either
   a parent or a rank. *)
let uf : V.vector =
  V.create()

(* A point is a valid index into the vector [uf]. *)
type point = int
type t = point

let[@inline] validate (x : point) =
  assert (0 <= x && x < V.length uf)

let[@inline] info (x : point) : info =
  validate x;
  V.unsafe_get uf x

let[@inline] set_parent (x : point) (y : point) : point =
  validate x;
  V.unsafe_set uf x (encode_parent y);
  y

let[@inline] set_rank (x : point) (rank : rank) =
  validate x;
  V.unsafe_set uf x (encode_rank rank)

type population = int

let[@inline] population () : population =
  V.length uf

let[@inline] iter f =
  for x = 0 to population() - 1 do
    f x
  done

let[@inline] fresh () : point =
  (* [x] is the index of the new point. *)
  let x = V.length uf in
  (* This point is a root; its rank is 0. *)
  let info = encode_rank 0 in
  (* Push this information into the vector. *)
  V.push uf info;
  (* Return [x]. *)
  x

let[@inline] drop (x : point) =
  (* [x] must be the last point. *)
  assert (x + 1 = population());
  V.drop uf

(* [find x] finds the representative vertex of the equivalence class of [x].
   It does so by following the path from [x] to the root. Path compression
   is performed (on the way back) by making every vertex along the path a
   direct child of the representative vertex. No rank is altered. *)

(* [find_non_root x info_x] is equivalent to [find x], but is specialized
   for the case where we have already retrieved [info_x] and determined
   that [x] is not a root. *)

(* This formulation allows us to avoid a test [if z <> y then ...]
   after the recursive call. Indeed, in this formulation, we can be
   certain that [z <> y] is true. *)

let rec find_non_root (x : point) (info_x : info) : point =
  assert (has_parent info_x);
  (* [x] has parent [y]. *)
  let y = decode_parent info_x in
  let info_y = info y in
  if is_root info_y then
    (* [y] is a root. There is nothing to do. *)
    y
  else
    (* [y] is not a root. Find its representative [z]. *)
    let z = find_non_root y info_y in
    (* Because [y] is not a root, we must have [z <> y]. *)
    assert (z <> y);
    (* Perform path compression. *)
    set_parent x z

let find (x : point) : point =
  let info_x = info x in
  if is_root info_x then
    (* [x] is a root. There is nothing to do. *)
    x
  else
    (* [x] is not a root. *)
    find_non_root x info_x

let[@inline] is_representative (x : point) : bool =
  is_root (info x)

let equal =
  Int.equal

let compare =
  Int.compare

let hash (x : point) =
  Hashtbl.hash x (* in OCaml 5.1, [Int.hash] can be preferred *)

let show (x : point) =
  Printf.sprintf "%d" (x :> int)

(* [equiv x y] could be implemented as [equal (find x) (find y)].
   Here is a supposedly more efficient implementation. *)

let equiv (x : point) (y : point) : bool =
  (* If [x] and [y] are equal then we are done (fast path). *)
  equal x y ||
  (* Otherwise, look them up. *)
  let info_x, info_y = info x, info y in
  match is_root info_x, is_root info_y with
  | true, true ->
      (* [x] and [y] are both roots, and we have already verified
         that they are different points. So, they are not equivalent. *)
      false
  | true, false ->
      (* [x] is a root; [y] is not. *)
      let y = find_non_root y info_y in
      equal x y
  | false, true ->
      (* [y] is a root; [x] is not. *)
      let x = find_non_root x info_x in
      equal x y
  | false, false ->
      (* Neither is a root. *)
      let x = find_non_root x info_x
      and y = find_non_root y info_y in
      equal x y

(* [union x y] merges the equivalence classes of [x] and [y] by installing a
   link from one root vertex to the other. *)

(* Linking is by rank: the smaller-ranked vertex is made to point to the
   larger. If the two vertices have the same rank, then an arbitrary choice
   is made, and the rank of the new root is incremented by one. *)

let union (x : point) (y : point) : point =
  let x = find x
  and y = find y in
  (* [x] and [y] are now roots. *)
  if x = y then x else
  let rx = decode_rank (info x)
  and ry = decode_rank (info y) in
  if rx < ry then
    set_parent x y
  else if rx > ry then
    set_parent y x
  else begin
    set_rank x (rx + 1);
    set_parent y x
  end

(* A vector of points is just a vector of integers. The equality between
   the types [point] and [int] is not known outside of this module, which
   is why we must define and export this submodule [Vector]. *)

module Vector =
  V

(* A hash set that contains points is just a hash set that contains
   integers. The equality between the types [point] and [int] is not
   known outside of this module, which is why we must define and
   export this submodule [HashSet]. We use the special values (-1)
   and (-2) as sentinels. These values are not valid points, unless
   we have exhausted the space of all integers, in which case we are
   (literally) dead anyway. *)

module S = struct
  type t = int
  let void = -1
  let tomb = -2
end

module HashSet (K : sig
  type t = point
  val hash  : point -> int
  val equal : point -> point -> bool
end) =
  Hachis.HashSet.Make_(K)(S)(Hector.IntArray)
    (* Using the module [Hector.IntArray], which offers arrays of integers,
       as if this module offered arrays of points, is slightly unsafe.
       Indeed, [point] is a strict subtype of integer. In a newly created
       array, all slots are initialized with arbitrary integer values. Thus,
       if an array slot was read before it is ever written, then an
       arbitrary integer value would be silently converted to a point. *)

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* We now extend the above implementation of union-find with the ability of
   associating a piece of data with each point. *)

(* This functor does not take [U : UF] as a parameter, because that would be
   unsafe. A user would be able to pass a module [U] that has already been
   used (so there already exists a non-zero number of points). Instead, we
   call [Make()] internally, so we are certain that we start with a pristine
   union-find data structure. *)

module MakeWithData (D : sig type t end) () = struct

include Make()

(* [point], [union], [find], [is_representative], [population], [iter],
   [validate], [equal], [compare], [hash], [show], [equiv], [Vector],
   [HashSet] are defined by the previous line. *)

type data =
  D.t

(* We set up a data vector, which represents a map of points to data. *)

module V =
  Hector.Mono.Make(D)

let data : V.t =
  V.create()

let[@inline] make (d : data) : point =
  V.push data d;
  fresh()

let[@inline] drop (x : point) =
  drop x;
  V.drop data

let[@inline] get_root (x : point) : data =
  assert (is_representative x);
  V.get data (x :> int)

let[@inline] get (x : point) : data =
  get_root (find x)

let[@inline] set_root (x : point) (d : data) : unit =
  assert (is_representative x);
  V.set data (x :> int) d

let[@inline] set (x : point) (d : data) =
  set_root (find x) d

let eq =
  equiv

(* The use of [union x y] in [merge] could sped up a little bit if we had
   a function [union_root], which assumes that [x] and [y] are roots. *)

let merge (f : data -> data -> data) (x : point) (y : point) : point =
  let x = find x
  and y = find y in
  if eq x y then
     x
   else
     let vx, vy = get_root x, get_root y in
     let v = f vx vy in
     let z = union x y in
     set_root z v;
     z

(* [fresh] must not be exported, as it does not extend the data vector.
   OCaml does not give us a way of removing a structure component, so
   we hide it with a new (useless) definition of [fresh]. *)

let fresh =
  ()

end (* MakeWithData *)
