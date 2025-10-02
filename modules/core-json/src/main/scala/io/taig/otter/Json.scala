package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.CoerceOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.operation.RecordOperation
import Self.operation.NullableOperation
import Self.operation.ConstantOperation

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Coerce[A](self: Self.Coerce[Json.Primitive, A]) extends Json[A]

  object Coerce:
    val liftK = [A] => (self: Self.Coerce[Json.Primitive, A]) => Coerce(self)
    val unliftK = [A] => (json: Json.Coerce[A]) => json.self

    given invariant: Invariant[Json.Coerce] = Invariant[[a] =>> Self.Coerce[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: CoerceOperation[Json.Coerce, Json.Primitive] =
      CoerceOperation[[a] =>> Self.Coerce[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Constant[A](self: Self.Constant[Json.Primitive, A]) extends Json[A]

  object Constant:
    val liftK = [A] => (self: Self.Constant[Json.Primitive, A]) => Constant(self)
    val unliftK = [A] => (json: Json.Constant[A]) => json.self

    given invariant: Invariant[Json.Constant] =
      Invariant[[a] =>> Self.Constant[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: ConstantOperation[Json.Constant, Json.Primitive] =
      ConstantOperation[[a] =>> Self.Constant[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  object Nullable:
    val liftK = [A] => (self: Self.Nullable[Json, A]) => Nullable(self)
    val unliftK = [A] => (json: Json.Nullable[A]) => json.self

    given invariant: Invariant[Json.Nullable] = Invariant[[a] =>> Self.Nullable[Json, a]].imapK(liftK)(unliftK)

    given operation: NullableOperation[Json.Nullable, Json] =
      NullableOperation[[a] =>> Self.Nullable[Json, a], Json].mapK(liftK)

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

  given invariant: Invariant[Json] with
    extension [A](json: Json[A])
      override def imap[B](f: A => B)(g: B => A): Json[B] = json match
        case json: Json.Coerce[A]    => json.imap(f)(g)
        case json: Json.Constant[A]  => json.imap(f)(g)
        case json: Json.Nullable[A]  => json.imap(f)(g)
        case json: Json.Primitive[A] => json.imap(f)(g)
        case json: Json.Record[A]    => json.imap(f)(g)
