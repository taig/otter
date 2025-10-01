package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.PrimitiveSchemaInvariant

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Coerce[A](self: Self.Coerce[Json.Primitive, A]) extends Json[A]

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  final case class Primitive[A](self: Annotation[Self.Primitive[A]]) extends Json[A]

  object Primitive:
    given PrimitiveSchemaInvariant[Json.Primitive] = ???
    //   val fK = [A] => (self: Schema.Primitive[A]) => Json.Primitive(self)
    //   val gK = [A] => (json: Json.Primitive[A]) => json.self
    //   PrimitiveSchemaInvariant[Schema.Primitive].imapK(fK)(gK)

  final case class Record[A](self: Annotation[Self.Record[Json, A]]) extends Json[A]

  opaque type Field[A] = Annotation[Self.Field[Json, A]]
