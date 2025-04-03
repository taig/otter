package io.taig.otter

abstract class FieldDsl[Key[_], Value[_], Record[_]](key: Key[String])(using
    invariant: FieldInvariant[Key, Value, Record]
):
  export invariant.{apply}

  def apply[A](name: String, codec: => Value[A]): Field[Key, Value, A] = apply(name, key, value = codec)
