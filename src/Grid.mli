type t

val newGrid: unit -> t

val print: t -> unit

val getCell: t -> int -> int -> string
val replaceCell: t -> int -> int -> string -> t

val checkNestVictory: t -> int -> int -> Player.t -> t
val checkVictory: t -> Player.t -> bool
val checkFull: t -> bool
