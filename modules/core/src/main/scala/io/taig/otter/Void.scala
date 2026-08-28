package io.taig.otter

/** Marker for an absent direction of a schema.
  *
  * A schema that can only be written has type `F[A, Void]`, one that can only be read has type `F[Void, A]`. `Void` is
  * uninhabited, so neither can produce or consume a value in the missing direction.
  *
  * `Nothing` deliberately is not used here: as a bottom type it conforms to every read type, which would make
  * `Decoder.decode(writeOnlySchema, ...)` compile and fail at runtime instead of being rejected by the compiler.
  */
sealed trait Void:
  def absurd[A]: A
