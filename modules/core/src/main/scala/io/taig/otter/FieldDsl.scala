package io.taig.otter

abstract class FieldDsl[Key[_], Value[_], Record[_]](key: Key[String])(using
    invariant: FieldInvariant[Key, Value, Record]
):
  export invariant.apply as field

  def field[A](name: String, codec: => Value[A]): Field[Key, Value, A] = field(name, key, value = codec)
