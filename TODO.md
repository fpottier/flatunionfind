# To Do

* Add support for associating a piece of data (of a fixed type) with each
  equivalence class. This allows supporting the traditional API with `make`,
  `get`, `set`, `merge`, etc. It should be possible to do this via a wrapper
  on top of the existing API.

* Add a truly read-only variant of `find`,
  for use in concurrent accesses.
