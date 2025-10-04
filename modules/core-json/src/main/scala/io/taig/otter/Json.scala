package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.CoerceOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.operation.RecordOperation
import io.taig.otter.operation.NullableOperation
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.CollectionOperation
import Self.operation.DictionaryOperation
import Self.operation.EnumerationOperation
import Self.operation.TupleOperation

sealed abstract class Json[+S[a] <: Json[?, a], A] extends Product with Serializable

object Json:
  final case class Coerce[A](self: Self.Coerce[Json.Primitive, A]) extends Json[Json.Primitive, A]

  object Coerce:
    val liftK = [A] => (self: Self.Coerce[Json.Primitive, A]) => Coerce(self)
    val unliftK = [A] => (json: Json.Coerce[A]) => json.self

    given invariant: Invariant[Json.Coerce] = Invariant[[a] =>> Self.Coerce[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: CoerceOperation[Json.Coerce, Json.Primitive] =
      CoerceOperation[[a] =>> Self.Coerce[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Collection[+S[a] <: Json[?, a], A](self: Self.Collection[S, A]) extends Json[S, A]

  object Collection:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Self.Collection[S, A]) => Collection(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Collection[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Collection[S, *]] =
      Invariant[[a] =>> Self.Collection[S, a]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: CollectionOperation[Json.Collection[S, *], S] =
      CollectionOperation[[a] =>> Self.Collection[S, a], S].mapK(liftK[S])

  final case class Constant[A](self: Self.Constant[Json.Primitive, A]) extends Json[Json.Primitive, A]

  object Constant:
    val liftK = [A] => (self: Self.Constant[Json.Primitive, A]) => Constant(self)
    val unliftK = [A] => (json: Json.Constant[A]) => json.self

    given invariant: Invariant[Json.Constant] =
      Invariant[[a] =>> Self.Constant[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: ConstantOperation[Json.Constant, Json.Primitive] =
      ConstantOperation[[a] =>> Self.Constant[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Dictionary[+S[a] <: Json[?, a], A](self: Self.Dictionary[S, A]) extends Json[S, A]

  object Dictionary:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Self.Dictionary[S, A]) => Dictionary(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Dictionary[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Dictionary[S, *]] =
      Invariant[[a] =>> Self.Dictionary[S, a]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: DictionaryOperation[Json.Dictionary[S, *], S] =
      DictionaryOperation[[a] =>> Self.Dictionary[S, a], S].mapK(liftK[S])

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[Json.Primitive, A]

  object Enumeration:
    val liftK = [A] => (self: Self.Enumeration[Json.Primitive, A]) => Enumeration(self)
    val unliftK = [A] => (json: Json.Enumeration[A]) => json.self

    given invariant: Invariant[Json.Enumeration] =
      Invariant[[a] =>> Self.Enumeration[Json.Primitive, a]].imapK(liftK)(unliftK)

    given operation: EnumerationOperation[Json.Enumeration, Json.Primitive] =
      EnumerationOperation[[a] =>> Self.Enumeration[Json.Primitive, a], Json.Primitive].mapK(liftK)

  final case class Nullable[+S[a] <: Json[?, a], A](self: Self.Nullable[S, A]) extends Json[S, A]

  object Nullable:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Self.Nullable[S, A]) => Nullable(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Nullable[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Nullable[S, *]] =
      Invariant[[a] =>> Self.Nullable[S, a]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: NullableOperation[Json.Nullable[S, *], S] =
      NullableOperation[[a] =>> Self.Nullable[S, a], S].mapK(liftK[S])

  final case class Primitive[A](self: Annotation[Self.Primitive[A]]) extends Json[Nothing, A]

  object Primitive:
    val liftK = [A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self)
    val unliftK = [A] => (json: Json.Primitive[A]) => json.self

    given invariant: Invariant[Json.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

    given operation: PrimitiveOperation[Json.Primitive] =
      PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

  final case class Record[+S[a] <: Json[?, a], A](self: Annotation[Self.Record[Json.Field[S, *], A]]) extends Json[S, A]

  object Record:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Annotation[Self.Record[Json.Field[S, *], A]]) => Record(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Record[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Record[S, *]] =
      Invariant[[a] =>> Annotation[Self.Record[Json.Field[S, *], a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: RecordOperation[Json.Record[S, *], Json.Field[S, *]] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field[S, *], a]], Json.Field[S, *]]
        .imapK(liftK[S])(unliftK[S])

  final case class Tuple[+S[a] <: Json[?, a], A](self: Annotation[Self.Tuple[S, A]]) extends Json[S, A]

  object Tuple:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Annotation[Self.Tuple[S, A]]) => Tuple(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Tuple[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[Self.Tuple[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: TupleOperation[Json.Tuple[S, *], S] =
      TupleOperation[[a] =>> Annotation[Self.Tuple[S, a]], S].imapK(liftK[S])(unliftK[S])

  final case class Field[+S[a] <: Json[?, a], A](self: Annotation[Self.Field[S, A]]) extends AnyVal

  object Field:
    def liftK[S[a] <: Json[?, a]] = [A] => (self: Annotation[Self.Field[S, A]]) => Field(self)
    def unliftK[S[a] <: Json[?, a]] = [A] => (json: Json.Field[S, A]) => json.self

    given invariant[S[a] <: Json[?, a]]: Invariant[Json.Field[S, *]] =
      Invariant[[a] =>> Annotation[Self.Field[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Json[?, a]]: FieldOperation[Json.Field[S, *], S] =
      FieldOperation[[a] =>> Annotation[Self.Field[S, a]], S].imapK(liftK[S])(unliftK[S])

  given invariant[S[a] <: Json[?, a]]: Invariant[Json[S, *]] with
    extension [A](json: Json[S, A])
      override def imap[B](f: A => B)(g: B => A): Json[S, B] = json match
        case json: Json.Coerce[?]        => json.imap(f)(g)
        case json: Json.Collection[?, ?] => json.imap(f)(g)
        case json: Json.Constant[?]      => json.imap(f)(g)
        case json: Json.Nullable[?, ?]   => json.imap(f)(g)
        case json: Json.Primitive[?]     => json.imap(f)(g)
        case json: Json.Record[?, ?]     => json.imap(f)(g)
