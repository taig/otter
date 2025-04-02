package io.taig.otter

abstract class Fields[S[_], T[_]](strings: Primitives.Strings[S]):
  def field[A, B](name: A, key: => S[A], value: => T[B]): Field.Required[S, T, B]

  final def field[A](name: String, codec: => T[A]): Field.Required[S, T, A] =
    field(name, key = strings.string, value = codec)

  // extension [A](self: Field[S, T, A])
  //   def toRecord: T[A]
  //   def :*[B](field: Field[S, T, B]): T[(A, B)]
  //   def *:[B](field: Field[S, T, B]): T[(B, A)]
