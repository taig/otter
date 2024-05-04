package io.taig.otter

import io.taig.otter.validation.Validation

sealed trait Primitive[A]
    extends Schema.Operation[[_, a] =>> Primitive[a], Nothing, A]
    with Primitive.Read[A]
    with Primitive.Write[A]

object Primitive:
  trait Operation:
    def tpe: Type[?]

  sealed trait Required[A]
      extends Schema.Operation[[_, a] =>> Primitive.Required[a], Nothing, A]
      with Primitive.Required.Read[A]
      with Primitive.Required.Write[A]

  object Required:
    sealed trait Read[A]
        extends Schema.Operation.Read[[_, a] =>> Primitive.Required.Read[a], Nothing, A]
        with Primitive.Operation

    sealed trait Write[A]
        extends Schema.Operation.Write[[_, a] =>> Primitive.Required.Write[a], Nothing, A]
        with Primitive.Operation

  sealed trait Read[A] extends Schema.Operation.Read[[_, a] =>> Primitive.Read[a], Nothing, A] with Primitive.Operation

  sealed trait Write[A]
      extends Schema.Operation.Write[[_, a] =>> Primitive.Write[a], Nothing, A]
      with Primitive.Operation
