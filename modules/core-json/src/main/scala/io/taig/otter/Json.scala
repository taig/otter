package io.taig.otter

import io.taig.otter as Self
import Self.operation.PrimitiveOperation
import Self.operation.RecordOperation
import Self.operation.FieldOperation
import Self.operation.CoerceOperation

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Coerce[A](self: Self.Coerce[Json.Primitive, A]) extends Json[A]

  object Coerce:
    val liftK = [A] => (self: Self.Coerce[Json.Primitive, A]) => Coerce(self)
    val unliftK = [A] => (json: Json.Coerce[A]) => json.self

    given invariant: Invariant[Json.Coerce] = Invariant[[a] =>> Self.Coerce[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: CoerceOperation[Json.Coerce, Json.Primitive] =
      CoerceOperation[[a] =>> Self.Coerce[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  final case class Primitive[A](self: Annotation[Self.Primitive[A]]) extends Json[A]

  object Primitive:
    val liftK = [A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self)
    val unliftK = [A] => (json: Json.Primitive[A]) => json.self

    given invariant: Invariant[Json.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

    given operation: PrimitiveOperation[Json.Primitive] =
      PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

  final case class Record[A](self: Annotation[Self.Record[Json.Field, A]]) extends Json[A]

  object Record:
    val liftK = [A] => (self: Annotation[Self.Record[Json.Field, A]]) => Record(self)
    val unliftK = [A] => (json: Json.Record[A]) => json.self

    given invariant: Invariant[Json.Record] =
      Invariant[[a] =>> Annotation[Self.Record[Json.Field, a]]].imapK(liftK)(unliftK)

    given operation: RecordOperation[Json.Record, Json.Field] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field, a]], Json.Field].imapK(liftK)(unliftK)

  opaque type Field[A] = Annotation[Self.Field[Json, A]]

  object Field:
    given invariant(using invariant: Invariant[[a] =>> Annotation[Self.Field[Json, a]]]): Invariant[Json.Field] =
      invariant

    given operation(using
        operation: FieldOperation[[a] =>> Annotation[Self.Field[Json, a]], Json]
    ): FieldOperation[Json.Field, Json] = operation
