package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.syntax.all.*
import cats.syntax.all.*
import cats.Invariant
import cats.Contravariant
import cats.Functor
import io.taig.otter.operation.TupleableOperation
import io.taig.otter.operation.TupleOperation
import cats.InvariantSemigroupal
import cats.Apply
import cats.ContravariantSemigroupal
import Self.operation.PrimitiveOperation

sealed abstract class Json[A] extends Json.Read[A], Json.Write[A]:
  override def self: Annotation[Json.Of[A]]

object Json:
  sealed trait Read[+A]:
    def self: Annotation[Json.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Primitive.Read[A] | Self.Tuple.Read[Json.Read, A]

    given Functor[Json.Read]:
      override def map[A, B](json: Json.Read[A])(f: A => B): Read[B] = json match
        case json: Json.Primitive.Read[A] => json.map(f)
        case json: Json.Tuple.Read[A]     => json.map(f)

    given TupleableOperation[Json.Tuple.Read, Json.Read] = TupleableOperation.derived

  sealed trait Write[-A]:
    def self: Annotation[Json.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Primitive.Write[A] | Self.Tuple.Write[Json.Write, A]

    given Contravariant[Json.Write]:
      override def contramap[A, B](json: Json.Write[A])(f: B => A): Write[B] = json match
        case json: Json.Primitive.Write[A] => json.contramap(f)
        case json: Json.Tuple.Write[A]     => json.contramap(f)

    given TupleableOperation[Json.Tuple.Write, Json.Write] = TupleableOperation.derived

  sealed abstract class Primitive[A] extends Json[A], Json.Primitive.Read[A], Json.Primitive.Write[A]:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Primitive.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Primitive.Read[A]]): Json.Primitive.Read[A] = new Read[A]:
        override def self: Annotation[Self.Primitive.Read[A]] = annotation

      def unapply[A](json: Json.Primitive.Read[A]): Annotation[Self.Primitive.Read[A]] = json.self

      given Functor[Json.Primitive.Read] = Functor[[a] =>> Annotation[Self.Primitive.Read[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Read[A]]) => Read(self))([A] =>
          (json: Json.Primitive.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Primitive.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Primitive.Write[A]]): Json.Primitive.Write[A] = new Write[A]:
        override def self: Annotation[Self.Primitive.Write[A]] = annotation

      def unapply[A](json: Json.Primitive.Write[A]): Annotation[Self.Primitive.Write[A]] = json.self

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

        def unapply[A](json: Json.Primitive.Boolean.Read[A]): Annotation[Self.Primitive.Boolean.Read[A]] =
          json.self

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

        def unapply[A](json: Json.Primitive.Boolean.Write[A]): Annotation[Self.Primitive.Boolean.Write[A]] =
          json.self

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

      def unapply[A](json: Json.Primitive.Boolean[A]): Annotation[Self.Primitive.Boolean[A]] = json.self

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

        def unapply[A](json: Json.Primitive.Number.Read[A]): Annotation[Self.Primitive.Number.Read[A]] =
          json.self

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

        def unapply[A](json: Json.Primitive.Number.Write[A]): Annotation[Self.Primitive.Number.Write[A]] =
          json.self

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

      def unapply[A](json: Json.Primitive.Number[A]): Annotation[Self.Primitive.Number[A]] = json.self

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

        def unapply[A](json: Json.Primitive.Text.Read[A]): Annotation[Self.Primitive.Text.Read[A]] =
          json.self

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

        def unapply[A](json: Json.Primitive.Text.Write[A]): Annotation[Self.Primitive.Text.Write[A]] =
          json.self

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

      def unapply[A](json: Json.Primitive.Text[A]): Annotation[Self.Primitive.Text[A]] = json.self

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

    def unapply[A](json: Json.Primitive[A]): Annotation[Self.Primitive[A]] = json.self

    given Invariant[Json.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]]
      .imapK([A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self))([A] =>
        (json: Json.Primitive[A]) => json.self
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

      def unapply[A](json: Json.Tuple.Read[A]): Annotation[Self.Tuple.Read[Json.Read, A]] = json.self

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

      def unapply[A](json: Json.Tuple.Write[A]): Annotation[Self.Tuple.Write[Json.Write, A]] = json.self

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

    def unapply[A](json: Json.Tuple[A]): Annotation[Self.Tuple[Json, A]] = json.self

    given InvariantSemigroupal[Json.Tuple] = InvariantSemigroupal[[a] =>> Annotation[Self.Tuple[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

    given TupleOperation[Json.Tuple, Json] = TupleOperation[[a] =>> Annotation[Self.Tuple[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

  type Of[A] = Self.Primitive[A] | Self.Tuple[Json, A]

  given Invariant[Json]:
    override def imap[A, B](json: Json[A])(f: A => B)(g: B => A): Json[B] = json match
      case json: Json.Primitive[A] => json.imap(f)(g)
      case json: Json.Tuple[A]     => json.imap(f)(g)

  given TupleableOperation[Json.Tuple, Json] = TupleableOperation.derived
