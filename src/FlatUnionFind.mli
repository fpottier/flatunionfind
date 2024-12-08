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
