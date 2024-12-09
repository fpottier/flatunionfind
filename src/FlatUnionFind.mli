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

(**This module offers a union-find data structure based on disjoint set
   forests, with path compression and linking by rank. The data structure
   is stored in a flat integer vector. *)

(** @inline *)
include module type of Signatures

(**Applying the functor [Make] creates a fresh instance of the union-find
   data structure. This functor is generative: each application returns a
   new module, with its own mutable internal state and its own abstract type
   [point]. *)
module Make () : UF

(**Applying the functor [MakeWithData] creates a fresh instance of the
   union-find data structure. In comparison with [Make], this data structure
   offers one additional feature: it maintains a map of equivalence classes
   to user data. This functor is generative: each application returns a new
   module, with its own mutable internal state and its own abstract type
   [point]. *)
module MakeWithData (D : sig type t end) () :
  UFD with type data = D.t
