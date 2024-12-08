module type UF = sig

  (**[point] is the type of a point in the union-find data structure. It is
     an abstract type. The keyword [private] indicates that a point [x] can
     be converted to an integer index, by writing [x :> int]. This integer
     index lies in the semi-open interval from 0 to [population()]. The
     conversion in the reverse direction is not permitted. *)
  type point = private int
  type t = point

  type population = int

  (**[population()] is the total number of points created so far. *)
  val population : unit -> population

  (**[iter f] applies the user-supplied function [f] to every point
     that was ever created (whose creation was not undone via [drop]). *)
  val iter : (point -> unit) -> unit

  (**[fresh()] creates a new point, which forms a new singleton
     equivalence class. *)
  val fresh : unit -> point

  (**[drop x] undoes the creation of the point [x]. This is permitted only
     if no new point was created since [x] was created (or if those new
     points were themselves dropped) and no [union] operation took place
     since [x] was created. This operation is unsafe: the user must promise
     to not use the point [x] afterwards. *)
  val drop : point -> unit

  (**If runtime assertions are enabled, then [validate x] checks that
     the point [x] is a valid point, and fails otherwise. [drop] is
     the only operation that can cause a point to become invalid. *)
  val validate : point -> unit

  (**[find x] finds the current representative element of the
     equivalence class of the point [x]. *)
  val find : point -> point

  (**[is_representative x] determines whether [x] is currently
     the representative element of its equivalence class. *)
  val is_representative : point -> bool

  (**[equal x y] determines whether [x] and [y] are equal,
     that is, whether they are the same point.
     It is equivalent to [(x :> int) = (y :> int)]. *)
  val equal : point -> point -> bool

  (**[compare] is a total order on points.
     [compare x y] is equivalent to [Int.compare (x :> int) (y :> int)]. *)
  val compare : point -> point -> int

  (**[hash] is a hash function on points.
     [hash x] equivalent to [Int.hash (x :> int)]. *)
  val hash : point -> int

  (**[show x] produces a human-readable representation of the point [x]. *)
  val show : point -> string

  (**[equiv x y] determines whether [x] and [y] are equivalent,
     that is, whether they are members of the same equivalence
     class. (If they are now, then they will be forever.) *)
  val equiv : point -> point -> bool

  (**If [equiv x y] is true, then [union x y] has no observable effect.
     Otherwise, [union x y] merges the equivalence classes of the points [x]
     and [y]. In either case, after the call, [equiv x y] is true.

     [union x y] returns a point [z] such that [equiv x z] and [equiv y z]
     and [is_representative z] are true. *)
  val union : point -> point -> point

  (**The submodule [Vector] offers vectors that contain points. *)
  module Vector : Hector.MONOVECTOR with type element = point

  (**The submodule [HashSet] offers hash sets that contain points. It
     is a functor: an equivalence relation on points must be chosen by
     the user. *)
  module HashSet (_ : sig
    type t = point
    val hash  : point -> int
    val equal : point -> point -> bool
  end)
  : Hachis.HashSet.SET with type element = point

end
