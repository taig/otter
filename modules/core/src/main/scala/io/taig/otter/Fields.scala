package io.taig.otter

trait Fields[S[_], T[_]]:
  def field[A, B](name: A, key: => S[A], value: => T[B]): Field.Required[S, T, B]

  final def field[A](name: String, codec: => T[A]): Field.Required[S, T, A] =
    field(name, key = ???, value = codec)

  extension [A](self: Field[S, T, A])
    def toRecord: T[A]
    def :*[B](field: Field[S, T, B]): T[(A, B)]
    def *:[B](field: Field[S, T, B]): T[(B, A)]
