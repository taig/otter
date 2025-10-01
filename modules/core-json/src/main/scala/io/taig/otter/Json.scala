package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.otter.operation.FieldSchemaInvariant
import io.taig.otter.operation.RecordSchemaInvariant

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Coerce[A](self: Self.Coerce[Json.Primitive, A]) extends Json[A]

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  final case class Primitive[A](self: Annotation[Self.Primitive[A]]) extends Json[A]

  object Primitive:
    given PrimitiveSchemaInvariant[Json.Primitive] =
      val fK = [A] => (self: Annotation[Self.Primitive[A]]) => Json.Primitive(self)
      val gK = [A] => (json: Json.Primitive[A]) => json.self
      PrimitiveSchemaInvariant[[a] =>> Annotation[Self.Primitive[a]]].imapK(fK)(gK)

  final case class Record[A](self: Annotation[Self.Record[Json.Field, A]]) extends Json[A]

  object Record:
    given RecordSchemaInvariant[Json.Record, Json.Field] =
      val fK = [A] => (self: Annotation[Self.Record[[a] =>> Annotation[Self.Field[Json, a]], A]]) => Record(self)
      val gK = [A] => (schema: Json.Record[A]) => schema.self
      RecordSchemaInvariant[
        [a] =>> Annotation[Self.Record[[b] =>> Annotation[Self.Field[Json, b]], a]],
        [a] =>> Annotation[Self.Field[Json, a]]
      ].imapK(fK)(gK)

  opaque type Field[A] = Annotation[Self.Field[Json, A]]

  object Field:
    given FieldSchemaInvariant[Json.Field, Json] = FieldSchemaInvariant.schema
