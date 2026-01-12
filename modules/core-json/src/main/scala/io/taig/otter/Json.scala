package io.taig.otter

import cats.Apply
import cats.Contravariant
import cats.ContravariantSemigroupal
import cats.Functor
import cats.Invariant
import cats.InvariantSemigroupal
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.CollectionOperation
import io.taig.otter.operation.DictionaryOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.operation.RecordOperation
import io.taig.otter.operation.RecordableOperation
import io.taig.otter.operation.TupleOperation
import io.taig.otter.operation.TupleableOperation
import io.taig.otter.syntax.all.*

sealed abstract class Json[A] extends Json.Read[A], Json.Write[A]:
  override def self: Annotation[Json.Of[A]]

object Json:
  sealed trait Read[+A]:
    def self: Annotation[Json.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Collection.Read[Json.Read, A] | Self.Dictionary.Read[Json.Read, A] | Self.Primitive.Read[A] |
      Self.Record.Read[Json.Field.Read, A] | Self.Tuple.Read[Json.Read, A]

    given Functor[Json.Read]:
      override def map[A, B](json: Json.Read[A])(f: A => B): Read[B] = json match
        case json: Json.Collection.Read[A] => json.map(f)
        case json: Json.Dictionary.Read[A] => json.map(f)
        case json: Json.Primitive.Read[A]  => json.map(f)
        case json: Json.Record.Read[A]     => json.map(f)
        case json: Json.Tuple.Read[A]      => json.map(f)

    given TupleableOperation[Json.Read, Json.Tuple.Read] = TupleableOperation.derived

  sealed trait Write[-A]:
    def self: Annotation[Json.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Collection.Write[Json.Write, A] | Self.Dictionary.Write[Json.Write, A] |
      Self.Primitive.Write[A] | Self.Record.Write[Json.Field.Write, A] | Self.Tuple.Write[Json.Write, A]

    given Contravariant[Json.Write]:
      override def contramap[A, B](json: Json.Write[A])(f: B => A): Write[B] = json match
        case json: Json.Collection.Write[A] => json.contramap(f)
        case json: Json.Dictionary.Write[A] => json.contramap(f)
        case json: Json.Primitive.Write[A]  => json.contramap(f)
        case json: Json.Record.Write[A]     => json.contramap(f)
        case json: Json.Tuple.Write[A]      => json.contramap(f)

    given TupleableOperation[Json.Write, Json.Tuple.Write] = TupleableOperation.derived

  sealed abstract class Collection[A] extends Json[A], Json.Collection.Read[A], Json.Collection.Write[A]:
    override def self: Annotation[Self.Collection[Json, A]]

  object Collection:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Collection.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Collection.Read[Json.Read, A]]): Json.Collection.Read[A] =
        new Json.Collection.Read[A]:
          override def self: Annotation[Self.Collection.Read[Json.Read, A]] = annotation

      given Functor[Json.Collection.Read] = Functor[[a] =>> Annotation[Self.Collection.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Collection.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Collection.Read[A]) => json.self
        )

      given [A] => Annotated[Json.Collection.Read[A]] =
        Annotated[Annotation[Self.Collection.Read[Json.Read, A]]].imap(Read.apply)(_.self)

      given CollectionOperation.Read[Json.Collection.Read, Json.Read] =
        CollectionOperation
          .Read[[a] =>> Annotation[Self.Collection.Read[Json.Read, a]], Json.Read]
          .imapK([A] => (self: Annotation[Self.Collection.Read[Json.Read, A]]) => Read(self))([A] =>
            (json: Json.Collection.Read[A]) => json.self
          )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Collection.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Collection.Write[Json.Write, A]]): Json.Collection.Write[A] =
        new Json.Collection.Write[A]:
          override def self: Annotation[Self.Collection.Write[Json.Write, A]] = annotation

      given Contravariant[Json.Collection.Write] =
        Contravariant[[a] =>> Annotation[Self.Collection.Write[Json.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Collection.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Collection.Write[A]) => json.self
          )

      given [A] => Annotated[Json.Collection.Write[A]] =
        Annotated[Annotation[Self.Collection.Write[Json.Write, A]]].imap(Write.apply)(_.self)

      given CollectionOperation.Write[Json.Collection.Write, Json.Write] =
        CollectionOperation
          .Write[[a] =>> Annotation[Self.Collection.Write[Json.Write, a]], Json.Write]
          .imapK([A] => (self: Annotation[Self.Collection.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Collection.Write[A]) => json.self
          )

    def apply[A](annotation: Annotation[Self.Collection[Json, A]]): Json.Collection[A] = new Collection[A]:
      override def self: Annotation[Self.Collection[Json, A]] = annotation

    given Invariant[Json.Collection] = Invariant[[a] =>> Annotation[Self.Collection[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
        (json: Json.Collection[A]) => json.self
      )

    given [A] => Annotated[Json.Collection[A]] =
      Annotated[Annotation[Self.Collection[Json, A]]].imap(Collection.apply)(_.self)

    given CollectionOperation[Json.Collection, Json] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
          (json: Json.Collection[A]) => json.self
        )

  sealed abstract class Dictionary[A] extends Json[A], Json.Dictionary.Read[A], Json.Dictionary.Write[A]:
    override def self: Annotation[Self.Dictionary[Json, A]]

  object Dictionary:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Dictionary.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Dictionary.Read[Json.Read, A]]): Json.Dictionary.Read[A] =
        new Json.Dictionary.Read[A]:
          override def self: Annotation[Self.Dictionary.Read[Json.Read, A]] = annotation

      given Functor[Json.Dictionary.Read] = Functor[[a] =>> Annotation[Self.Dictionary.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Dictionary.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Dictionary.Read[A]) => json.self
        )

      given DictionaryOperation.Read[Json.Dictionary.Read, Json.Read] =
        DictionaryOperation
          .Read[[a] =>> Annotation[Self.Dictionary.Read[Json.Read, a]], Json.Read]
          .imapK([A] => (self: Annotation[Self.Dictionary.Read[Json.Read, A]]) => Read(self))([A] =>
            (json: Json.Dictionary.Read[A]) => json.self
          )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Dictionary.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Dictionary.Write[Json.Write, A]]): Json.Dictionary.Write[A] =
        new Json.Dictionary.Write[A]:
          override def self: Annotation[Self.Dictionary.Write[Json.Write, A]] = annotation

      given Contravariant[Json.Dictionary.Write] =
        Contravariant[[a] =>> Annotation[Self.Dictionary.Write[Json.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Dictionary.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Dictionary.Write[A]) => json.self
          )

      given DictionaryOperation.Write[Json.Dictionary.Write, Json.Write] =
        DictionaryOperation
          .Write[[a] =>> Annotation[Self.Dictionary.Write[Json.Write, a]], Json.Write]
          .imapK([A] => (self: Annotation[Self.Dictionary.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Dictionary.Write[A]) => json.self
          )

    def apply[A](annotation: Annotation[Self.Dictionary[Json, A]]): Json.Dictionary[A] = new Dictionary[A]:
      override def self: Annotation[Self.Dictionary[Json, A]] = annotation

    given Invariant[Json.Dictionary] = Invariant[[a] =>> Annotation[Self.Dictionary[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
        (json: Json.Dictionary[A]) => json.self
      )

    given DictionaryOperation[Json.Dictionary, Json] =
      DictionaryOperation[[a] =>> Annotation[Self.Dictionary[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
          (json: Json.Dictionary[A]) => json.self
        )

  sealed abstract class Primitive[A] extends Json[A], Json.Primitive.Read[A], Json.Primitive.Write[A]:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Primitive.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Primitive.Read[A]]): Json.Primitive.Read[A] = new Read[A]:
        override def self: Annotation[Self.Primitive.Read[A]] = annotation

      given Functor[Json.Primitive.Read] = Functor[[a] =>> Annotation[Self.Primitive.Read[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Read[A]]) => Read(self))([A] =>
          (json: Json.Primitive.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Primitive.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Primitive.Write[A]]): Json.Primitive.Write[A] = new Write[A]:
        override def self: Annotation[Self.Primitive.Write[A]] = annotation

      given Contravariant[Json.Primitive.Write] = Contravariant[[a] =>> Annotation[Self.Primitive.Write[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Write[A]]) => Write(self))([A] =>
          (json: Json.Primitive.Write[A]) => json.self
        )

    sealed trait Boolean[A] extends Json.Primitive[A], Json.Primitive.Boolean.Read[A], Json.Primitive.Boolean.Write[A]:
      override def self: Annotation[Self.Primitive.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Json.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Boolean.Read[A]]): Json.Primitive.Boolean.Read[A] =
          new Json.Primitive.Boolean.Read[A]:
            override def self: Annotation[Self.Primitive.Boolean.Read[A]] = annotation

        given Functor[Json.Primitive.Boolean.Read] = Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Boolean.Read[A]) => json.self
          )

        given PrimitiveOperation.Boolean.Read[Json.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
          .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Boolean.Read[A]) => json.self
          )

      sealed trait Write[-A] extends Json.Primitive.Write[A]:
        override def self: Annotation[Self.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Boolean.Write[A]]): Json.Primitive.Boolean.Write[A] =
          new Json.Primitive.Boolean.Write[A]:
            override def self: Annotation[Self.Primitive.Boolean.Write[A]] = annotation

        given Contravariant[Json.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Write[A]]) => Write(self))([A] =>
              (json: Json.Primitive.Boolean.Write[A]) => json.self
            )

        given PrimitiveOperation.Boolean.Write[Json.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
          .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Write[A]]) => Write(self))([A] =>
            (json: Json.Primitive.Boolean.Write[A]) => json.self
          )

      def apply[A](annotation: Annotation[Self.Primitive.Boolean[A]]): Json.Primitive.Boolean[A] =
        new Json.Primitive.Boolean[A]:
          override def self: Annotation[Self.Primitive.Boolean[A]] = annotation

      given Invariant[Json.Primitive.Boolean] = Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self))([A] =>
          (json: Json.Primitive.Boolean[A]) => json.self
        )

      given PrimitiveOperation.Boolean[Json.Primitive.Boolean] = PrimitiveOperation
        .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self))([A] =>
          (json: Json.Primitive.Boolean[A]) => json.self
        )

    sealed trait Number[A] extends Json.Primitive[A], Json.Primitive.Number.Read[A], Json.Primitive.Number.Write[A]:
      override def self: Annotation[Self.Primitive.Number[A]]

    object Number:
      sealed trait Read[+A] extends Json.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Number.Read[A]]): Json.Primitive.Number.Read[A] =
          new Json.Primitive.Number.Read[A]:
            override def self: Annotation[Self.Primitive.Number.Read[A]] = annotation

        given Functor[Json.Primitive.Number.Read] = Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Number.Read[A]) => json.self
          )

        given PrimitiveOperation.Number.Read[Json.Primitive.Number.Read] = PrimitiveOperation.Number
          .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Number.Read[A]) => json.self
          )

      sealed trait Write[-A] extends Json.Primitive.Write[A]:
        override def self: Annotation[Self.Primitive.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Number.Write[A]]): Json.Primitive.Number.Write[A] =
          new Json.Primitive.Number.Write[A]:
            override def self: Annotation[Self.Primitive.Number.Write[A]] = annotation

        given Contravariant[Json.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Number.Write[A]]) => Write(self))([A] =>
              (json: Json.Primitive.Number.Write[A]) => json.self
            )

        given PrimitiveOperation.Number.Write[Json.Primitive.Number.Write] = PrimitiveOperation.Number
          .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Write[A]]) => Write(self))([A] =>
            (json: Json.Primitive.Number.Write[A]) => json.self
          )

      def apply[A](annotation: Annotation[Self.Primitive.Number[A]]): Json.Primitive.Number[A] =
        new Json.Primitive.Number[A]:
          override def self: Annotation[Self.Primitive.Number[A]] = annotation

      given Invariant[Json.Primitive.Number] = Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self))([A] =>
          (json: Json.Primitive.Number[A]) => json.self
        )

      given PrimitiveOperation.Number[Json.Primitive.Number] = PrimitiveOperation
        .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self))([A] =>
          (json: Json.Primitive.Number[A]) => json.self
        )

    sealed trait Text[A] extends Json.Primitive[A], Json.Primitive.Text.Read[A], Json.Primitive.Text.Write[A]:
      override def self: Annotation[Self.Primitive.Text[A]]

    object Text:
      sealed trait Read[+A] extends Json.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Json.Primitive.Text.Read[A] =
          new Json.Primitive.Text.Read[A]:
            override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[Json.Primitive.Text.Read] = Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Text.Read[A]) => json.self
          )

        given PrimitiveOperation.Text.Read[Json.Primitive.Text.Read] = PrimitiveOperation.Text
          .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Text.Read[A]) => json.self
          )

      sealed trait Write[-A] extends Json.Primitive.Write[A]:
        override def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Json.Primitive.Text.Write[A] =
          new Json.Primitive.Text.Write[A]:
            override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[Json.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self))([A] =>
              (json: Json.Primitive.Text.Write[A]) => json.self
            )

        given PrimitiveOperation.Text.Write[Json.Primitive.Text.Write] = PrimitiveOperation.Text
          .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self))([A] =>
            (json: Json.Primitive.Text.Write[A]) => json.self
          )

      def apply[A](annotation: Annotation[Self.Primitive.Text[A]]): Json.Primitive.Text[A] = new Text[A]:
        override def self: Annotation[Self.Primitive.Text[A]] = annotation

      given Invariant[Json.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
          (json: Json.Primitive.Text[A]) => json.self
        )

      given PrimitiveOperation.Text[Json.Primitive.Text] = PrimitiveOperation
        .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
          (json: Json.Primitive.Text[A]) => json.self
        )

    def apply[A](annotation: Annotation[Self.Primitive[A]]): Json.Primitive[A] = new Primitive[A]:
      override def self: Annotation[Self.Primitive[A]] = annotation

    given Invariant[Json.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]]
      .imapK([A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self))([A] =>
        (json: Json.Primitive[A]) => json.self
      )

  sealed abstract class Record[A] extends Json[A], Json.Record.Read[A], Json.Record.Write[A]:
    override def self: Annotation[Self.Record[Json.Field, A]]

  object Record:
    sealed trait Read[+A] extends Json.Read[A]:
      override def self: Annotation[Self.Record.Read[Json.Field.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Record.Read[Json.Field.Read, A]]): Json.Record.Read[A] =
        new Json.Record.Read[A]:
          override def self: Annotation[Self.Record.Read[Json.Field.Read, A]] = annotation

      given Apply[Json.Record.Read] = Apply[[a] =>> Annotation[Self.Record.Read[Json.Field.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Record.Read[Json.Field.Read, A]]) => Read(self))([A] =>
          (json: Json.Record.Read[A]) => json.self
        )

      given RecordOperation.Read[Json.Record.Read, Json.Field.Read] = RecordOperation
        .Read[[a] =>> Annotation[Self.Record.Read[Json.Field.Read, a]], Json.Field.Read]
        .imapK([A] => (self: Annotation[Self.Record.Read[Json.Field.Read, A]]) => Read(self))([A] =>
          (json: Json.Record.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      override def self: Annotation[Self.Record.Write[Json.Field.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Record.Write[Json.Field.Write, A]]): Json.Record.Write[A] =
        new Json.Record.Write[A]:
          override def self: Annotation[Self.Record.Write[Json.Field.Write, A]] = annotation

      given ContravariantSemigroupal[Json.Record.Write] =
        ContravariantSemigroupal[[a] =>> Annotation[Self.Record.Write[Json.Field.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Record.Write[Json.Field.Write, A]]) => Write(self))([A] =>
            (json: Json.Record.Write[A]) => json.self
          )

      given RecordOperation.Write[Json.Record.Write, Json.Field.Write] = RecordOperation
        .Write[[a] =>> Annotation[Self.Record.Write[Json.Field.Write, a]], Json.Field.Write]
        .imapK([A] => (self: Annotation[Self.Record.Write[Json.Field.Write, A]]) => Write(self))([A] =>
          (json: Json.Record.Write[A]) => json.self
        )

    def apply[A](annotation: Annotation[Self.Record[Json.Field, A]]): Json.Record[A] = new Record[A]:
      override def self: Annotation[Self.Record[Json.Field, A]] = annotation

    given InvariantSemigroupal[Json.Record] = InvariantSemigroupal[[a] =>> Annotation[Self.Record[Json.Field, a]]]
      .imapK([A] => (self: Annotation[Self.Record[Json.Field, A]]) => Record(self))([A] =>
        (json: Json.Record[A]) => json.self
      )

    given RecordOperation[Json.Record, Json.Field] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field, a]], Json.Field]
        .imapK([A] => (self: Annotation[Self.Record[Json.Field, A]]) => Record(self))([A] =>
          (json: Json.Record[A]) => json.self
        )

  sealed abstract class Tuple[A] extends Json[A], Json.Tuple.Read[A], Json.Tuple.Write[A]:
    def self: Annotation[Self.Tuple[Json, A]]

  object Tuple:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Tuple.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Tuple.Read[Json.Read, A]]): Json.Tuple.Read[A] =
        new Json.Tuple.Read[A]:
          override def self: Annotation[Self.Tuple.Read[Json.Read, A]] = annotation

      given Apply[Json.Tuple.Read] = Apply[[a] =>> Annotation[Self.Tuple.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Tuple.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Tuple.Read[A]) => json.self
        )

      given TupleOperation.Read[Json.Tuple.Read, Json.Read] = TupleOperation
        .Read[[a] =>> Annotation[Self.Tuple.Read[Json.Read, a]], Json.Read]
        .imapK([A] => (self: Annotation[Self.Tuple.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Tuple.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Tuple.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Tuple.Write[Json.Write, A]]): Json.Tuple.Write[A] =
        new Json.Tuple.Write[A]:
          override def self: Annotation[Self.Tuple.Write[Json.Write, A]] = annotation

      given ContravariantSemigroupal[Json.Tuple.Write] =
        ContravariantSemigroupal[[a] =>> Annotation[Self.Tuple.Write[Json.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Tuple.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Tuple.Write[A]) => json.self
          )

      given TupleOperation[Json.Tuple.Write, Json.Write] = TupleOperation
        .Write[[a] =>> Annotation[Self.Tuple.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Tuple.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Tuple.Write[A]) => json.self
        )

    def apply[A](annotation: Annotation[Self.Tuple[Json, A]]): Json.Tuple[A] = new Tuple[A]:
      override def self: Annotation[Self.Tuple[Json, A]] = annotation

    given InvariantSemigroupal[Json.Tuple] = InvariantSemigroupal[[a] =>> Annotation[Self.Tuple[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

    given TupleOperation[Json.Tuple, Json] = TupleOperation[[a] =>> Annotation[Self.Tuple[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

  sealed abstract class Field[A] extends Json.Field.Read[A], Json.Field.Write[A]:
    override def self: Annotation[Self.Field[Json, A]]

  object Field:
    sealed trait Read[+A]:
      def self: Annotation[Self.Field.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Field.Read[Json.Read, A]]): Json.Field.Read[A] = new Read[A]:
        override def self: Annotation[Self.Field.Read[Json.Read, A]] = annotation

      given Functor[Json.Field.Read]:
        override def map[A, B](fa: Json.Field.Read[A])(f: A => B): Json.Field.Read[B] = fa.map(f)

      given operation: FieldOperation.Read[Json.Field.Read, Json.Read] = FieldOperation
        .Read[[a] =>> Annotation[Self.Field.Read[Json.Read, a]], Json.Read]
        .imapK([A] => (self: Annotation[Self.Field.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Field.Read[A]) => json.self
        )

      given recordable: RecordableOperation.Read[Json.Field.Read, Json.Record.Read] = RecordableOperation.Read.derived

    sealed trait Write[-A]:
      def self: Annotation[Self.Field.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Field.Write[Json.Write, A]]): Json.Field.Write[A] = new Write[A]:
        override def self: Annotation[Self.Field.Write[Json.Write, A]] = annotation

      given operation: FieldOperation.Write[Json.Field.Write, Json.Write] = FieldOperation
        .Write[[a] =>> Annotation[Self.Field.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Field.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Field.Write[A]) => json.self
        )

      given recordable: RecordableOperation.Write[Json.Field.Write, Json.Record.Write] =
        RecordableOperation.Write.derived

    def apply[A](annotation: Annotation[Self.Field[Json, A]]): Json.Field[A] = new Field[A]:
      override def self: Annotation[Self.Field[Json, A]] = annotation

    given operation: FieldOperation[Json.Field, Json] = FieldOperation[[a] =>> Annotation[Self.Field[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Field[Json, A]]) => Field(self))([A] => (json: Json.Field[A]) => json.self)

    given recordable: RecordableOperation[Json.Field, Json.Record] = RecordableOperation.derived

  type Of[A] = Self.Collection[Json, A] | Self.Dictionary[Json, A] | Self.Primitive[A] | Self.Record[Json.Field, A] |
    Self.Tuple[Json, A]

  given Invariant[Json]:
    override def imap[A, B](json: Json[A])(f: A => B)(g: B => A): Json[B] = json match
      case json: Json.Collection[A] => json.imap(f)(g)
      case json: Json.Dictionary[A] => json.imap(f)(g)
      case json: Json.Primitive[A]  => json.imap(f)(g)
      case json: Json.Record[A]     => json.imap(f)(g)
      case json: Json.Tuple[A]      => json.imap(f)(g)

  given TupleableOperation[Json, Json.Tuple] = TupleableOperation.derived
