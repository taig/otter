package io.taig.otter

import cats.Apply
import cats.Contravariant
import cats.ContravariantSemigroupal
import cats.Functor
import cats.Invariant
import cats.InvariantSemigroupal
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.*
import io.taig.otter.syntax.all.*

sealed abstract class Json[A] extends Json.Read[A], Json.Write[A]:
  override def self: Annotation[
    Self.Collection[Json, A] | Self.Constant[Json.Primitive, A] | Self.Dictionary[Json, A] | Self.Enumeration[Json, A] |
      Self.Optional[Json, A] | Self.Primitive[Json.Primitive, A] | Self.Record[Json.Field, A] | Self.Tuple[Json, A] |
      Self.Union[Json.Branch, A]
  ]

object Json:
  sealed trait Read[+A]:
    def self: Annotation[
      Self.Collection.Read[Json.Read, A] | Self.Constant.Read[Json.Primitive, A] | Self.Dictionary.Read[Json.Read, A] |
        Self.Enumeration.Read[Json, A] | Self.Optional.Read[Json.Read, A] |
        Self.Primitive.Read[Json.Primitive.Read, A] | Self.Record.Read[Json.Field.Read, A] |
        Self.Tuple.Read[Json.Read, A] | Self.Union.Read[Json.Branch.Read, A]
    ]

  object Read:
    given Functor[Json.Read]:
      override def map[A, B](json: Json.Read[A])(f: A => B): Json.Read[B] = json match
        case json: Json.Collection.Read[A]  => json.map(f)
        case json: Json.Constant.Read[A]    => json.map(f)
        case json: Json.Dictionary.Read[A]  => json.map(f)
        case json: Json.Enumeration.Read[A] => json.map(f)
        case json: Json.Optional.Read[A]    => json.map(f)
        case json: Json.Primitive.Read[A]   => json.map(f)
        case json: Json.Record.Read[A]      => json.map(f)
        case json: Json.Tuple.Read[A]       => json.map(f)
        case json: Json.Union.Read[A]       => json.map(f)

    given [A] => Annotated[Json.Read[A]]:
      extension (self: Json.Read[A])
        override def lens: (Metadata, Metadata => Json.Read[A]) = self match
          case json: Json.Collection.Read[A]  => Annotated[Json.Collection.Read[A]].lens(json)
          case json: Json.Constant.Read[A]    => Annotated[Json.Constant.Read[A]].lens(json)
          case json: Json.Dictionary.Read[A]  => Annotated[Json.Dictionary.Read[A]].lens(json)
          case json: Json.Enumeration.Read[A] => Annotated[Json.Enumeration.Read[A]].lens(json)
          case json: Json.Optional.Read[A]    => Annotated[Json.Optional.Read[A]].lens(json)
          case json: Json.Primitive.Read[A]   => Annotated[Json.Primitive.Read[A]].lens(json)
          case json: Json.Record.Read[A]      => Annotated[Json.Record.Read[A]].lens(json)
          case json: Json.Tuple.Read[A]       => Annotated[Json.Tuple.Read[A]].lens(json)
          case json: Json.Union.Read[A]       => Annotated[Json.Union.Read[A]].lens(json)

    given optionalable: OptionalableOperation.Read[Json.Read, Json.Optional.Read] = OptionalableOperation.Read.derived

    given tupleable: TupleableOperation.Read[Json.Read, Json.Tuple.Read] = TupleableOperation.Read.derived

  sealed trait Write[-A]:
    def self: Annotation[
      Self.Collection.Write[Json.Write, A] | Self.Constant.Write[Json.Primitive.Write, A] |
        Self.Dictionary.Write[Json.Write, A] | Self.Enumeration.Write[Json.Write, A] |
        Self.Optional.Write[Json.Write, A] | Self.Primitive.Write[Json.Primitive.Write, A] |
        Self.Record.Write[Json.Field.Write, A] | Self.Tuple.Write[Json.Write, A] |
        Self.Union.Write[Json.Branch.Write, A]
    ]

  object Write:
    given Contravariant[Json.Write]:
      override def contramap[A, B](json: Json.Write[A])(f: B => A): Write[B] = json match
        case json: Json.Collection.Write[A]  => json.contramap(f)
        case json: Json.Constant.Write[A]    => json.contramap(f)
        case json: Json.Dictionary.Write[A]  => json.contramap(f)
        case json: Json.Enumeration.Write[A] => json.contramap(f)
        case json: Json.Optional.Write[A]    => json.contramap(f)
        case json: Json.Primitive.Write[A]   => json.contramap(f)
        case json: Json.Record.Write[A]      => json.contramap(f)
        case json: Json.Tuple.Write[A]       => json.contramap(f)
        case json: Json.Union.Write[A]       => json.contramap(f)

    given [A] => Annotated[Json.Write[A]]:
      extension (self: Json.Write[A])
        override def lens: (Metadata, Metadata => Json.Write[A]) = self match
          case json: Json.Collection.Write[A]  => Annotated[Json.Collection.Write[A]].lens(json)
          case json: Json.Constant.Write[A]    => Annotated[Json.Constant.Write[A]].lens(json)
          case json: Json.Dictionary.Write[A]  => Annotated[Json.Dictionary.Write[A]].lens(json)
          case json: Json.Enumeration.Write[A] => Annotated[Json.Enumeration.Write[A]].lens(json)
          case json: Json.Optional.Write[A]    => Annotated[Json.Optional.Write[A]].lens(json)
          case json: Json.Primitive.Write[A]   => Annotated[Json.Primitive.Write[A]].lens(json)
          case json: Json.Record.Write[A]      => Annotated[Json.Record.Write[A]].lens(json)
          case json: Json.Tuple.Write[A]       => Annotated[Json.Tuple.Write[A]].lens(json)
          case json: Json.Union.Write[A]       => Annotated[Json.Union.Write[A]].lens(json)

    given optionalable: OptionalableOperation.Write[Json.Write, Json.Optional.Write] =
      OptionalableOperation.Write.derived

    given tupleable: TupleableOperation.Write[Json.Write, Json.Tuple.Write] = TupleableOperation.Write.derived

  final case class Collection[A](self: Annotation[Self.Collection[Json, A]])
      extends Json[A],
        Json.Collection.Read[A],
        Json.Collection.Write[A]

  object Collection:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Collection.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Collection.Read[Json.Read, A]]): Json.Collection.Read[A] =
        new Read[A]:
          override def self: Annotation[Self.Collection.Read[Json.Read, A]] = annotation

      given Functor[Json.Collection.Read] = Functor[[a] =>> Annotation[Self.Collection.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Collection.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Collection.Read[A]) => json.self
        )

      given [A] => Annotated[Json.Collection.Read[A]] = Annotated[Annotation[Self.Collection.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

      given CollectionOperation.Read[Json.Collection.Read, Json.Read] = CollectionOperation
        .Read[[a] =>> Annotation[Self.Collection.Read[Json.Read, a]], Json.Read]
        .imapK([A] => (self: Annotation[Self.Collection.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Collection.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Collection.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Collection.Write[Json.Write, A]]): Json.Collection.Write[A] =
        new Write[A]:
          override def self: Annotation[Self.Collection.Write[Json.Write, A]] = annotation

      given Contravariant[Json.Collection.Write] =
        Contravariant[[a] =>> Annotation[Self.Collection.Write[Json.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Collection.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Collection.Write[A]) => json.self
          )

      given [A] => Annotated[Json.Collection.Write[A]] = Annotated[Annotation[Self.Collection.Write[Json.Write, A]]]
        .imap(Write.apply)(_.self)

      given CollectionOperation.Write[Json.Collection.Write, Json.Write] = CollectionOperation
        .Write[[a] =>> Annotation[Self.Collection.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Collection.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Collection.Write[A]) => json.self
        )

    given Invariant[Json.Collection] = Invariant[[a] =>> Annotation[Self.Collection[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
        (json: Json.Collection[A]) => json.self
      )

    given [A] => Annotated[Json.Collection[A]] = Annotated[Annotation[Self.Collection[Json, A]]]
      .imap(Collection.apply)(_.self)

    given CollectionOperation[Json.Collection, Json] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Collection[Json, A]]) => Collection(self))([A] =>
          (json: Json.Collection[A]) => json.self
        )

  final case class Constant[A](self: Annotation[Self.Constant[Json.Primitive, A]])
      extends Json[A],
        Json.Constant.Read[A],
        Json.Constant.Write[A]

  object Constant:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Constant.Read[Json.Primitive, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Constant.Read[Json.Primitive, A]]): Json.Constant.Read[A] =
        new Read[A]:
          override def self: Annotation[Self.Constant.Read[Json.Primitive, A]] = annotation

      given Functor[Json.Constant.Read] = Functor[[a] =>> Annotation[Self.Constant.Read[Json.Primitive, a]]]
        .imapK([A] => (self: Annotation[Self.Constant.Read[Json.Primitive, A]]) => Read(self))([A] =>
          (json: Json.Constant.Read[A]) => json.self
        )

      given [A] => Annotated[Json.Constant.Read[A]] = Annotated[Annotation[Self.Constant.Read[Json.Primitive, A]]]
        .imap(Read.apply)(_.self)

      given ConstantOperation.Read[Json.Constant.Read, Json.Primitive] = ConstantOperation
        .Read[[a] =>> Annotation[Self.Constant.Read[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Constant.Read[Json.Primitive, A]]) => Read(self))([A] =>
          (json: Json.Constant.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Constant.Write[Json.Primitive.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Constant.Write[Json.Primitive.Write, A]]): Json.Constant.Write[A] =
        new Json.Constant.Write[A]:
          override def self: Annotation[Self.Constant.Write[Json.Primitive.Write, A]] = annotation

      given Contravariant[Json.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[Json.Primitive.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Constant.Write[Json.Primitive.Write, A]]) => Write(self))([A] =>
            (json: Json.Constant.Write[A]) => json.self
          )

      given [A] => Annotated[Json.Constant.Write[A]] =
        Annotated[Annotation[Self.Constant.Write[Json.Primitive.Write, A]]].imap(Write.apply)(_.self)

      given ConstantOperation.Write[Json.Constant.Write, Json.Primitive.Write] = ConstantOperation
        .Write[[a] =>> Annotation[Self.Constant.Write[Json.Primitive.Write, a]], Json.Primitive.Write]
        .imapK([A] => (self: Annotation[Self.Constant.Write[Json.Primitive.Write, A]]) => Write(self))([A] =>
          (json: Json.Constant.Write[A]) => json.self
        )

    given Invariant[Json.Constant] = Invariant[[a] =>> Annotation[Self.Constant[Json.Primitive, a]]]
      .imapK([A] => (self: Annotation[Self.Constant[Json.Primitive, A]]) => Constant(self))([A] =>
        (json: Json.Constant[A]) => json.self
      )

    given [A] => Annotated[Json.Constant[A]] =
      Annotated[Annotation[Self.Constant[Json.Primitive, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Json.Constant, Json.Primitive] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Constant[Json.Primitive, A]]) => Constant(self))([A] =>
          (json: Json.Constant[A]) => json.self
        )

    given Translator[Value.Constant, Json.Constant]:
      override def translate[A](value: Value.Constant[A]): Json.Constant[A] = Json.Constant(
        value.self.map(
          _.mapK([A] =>
            (value: Value.Primitive.Text[A]) => Translator[Value.Primitive.Text, Json.Primitive.Text].translate(value)
          )
        )
      )

  final case class Dictionary[A](self: Annotation[Self.Dictionary[Json, A]])
      extends Json[A],
        Json.Dictionary.Read[A],
        Json.Dictionary.Write[A]

  object Dictionary:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Dictionary.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Dictionary.Read[Json.Read, A]]): Json.Dictionary.Read[A] = new Read[A]:
        override def self: Annotation[Self.Dictionary.Read[Json.Read, A]] = annotation

      given Functor[Json.Dictionary.Read] = Functor[[a] =>> Annotation[Self.Dictionary.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Dictionary.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Dictionary.Read[A]) => json.self
        )

      given [A] => Annotated[Json.Dictionary.Read[A]] = Annotated[Annotation[Self.Dictionary.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

      given DictionaryOperation.Read[Json.Dictionary.Read, Json.Read] = DictionaryOperation
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

      given [A] => Annotated[Json.Dictionary.Write[A]] =
        Annotated[Annotation[Self.Dictionary.Write[Json.Write, A]]].imap(Write.apply)(_.self)

      given DictionaryOperation.Write[Json.Dictionary.Write, Json.Write] = DictionaryOperation
        .Write[[a] =>> Annotation[Self.Dictionary.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Dictionary.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Dictionary.Write[A]) => json.self
        )

    given Invariant[Json.Dictionary] = Invariant[[a] =>> Annotation[Self.Dictionary[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
        (json: Json.Dictionary[A]) => json.self
      )

    given [A] => Annotated[Json.Dictionary[A]] = Annotated[Annotation[Self.Dictionary[Json, A]]]
      .imap(Dictionary.apply)(_.self)

    given DictionaryOperation[Json.Dictionary, Json] =
      DictionaryOperation[[a] =>> Annotation[Self.Dictionary[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Dictionary[Json, A]]) => Dictionary(self))([A] =>
          (json: Json.Dictionary[A]) => json.self
        )

  final case class Enumeration[A](self: Annotation[Self.Enumeration[Json.Primitive, A]])
      extends Json[A],
        Json.Enumeration.Read[A],
        Json.Enumeration.Write[A]

  object Enumeration:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Enumeration.Read[Json.Primitive, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Enumeration.Read[Json.Primitive, A]]): Json.Enumeration.Read[A] =
        new Read[A]:
          override def self: Annotation[Self.Enumeration.Read[Json.Primitive, A]] = annotation
      given Functor[Json.Enumeration.Read] = Functor[[a] =>> Annotation[Self.Enumeration.Read[Json.Primitive, a]]]
        .imapK([A] => (self: Annotation[Self.Enumeration.Read[Json.Primitive, A]]) => Read(self))([A] =>
          (json: Json.Enumeration.Read[A]) => json.self
        )

      given [A] => Annotated[Json.Enumeration.Read[A]] = Annotated[Annotation[Self.Enumeration.Read[Json.Primitive, A]]]
        .imap(Read.apply)(_.self)

      given EnumerationOperation.Read[Json.Enumeration.Read, Json.Primitive] = EnumerationOperation
        .Read[[a] =>> Annotation[Self.Enumeration.Read[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Enumeration.Read[Json.Primitive, A]]) => Read(self))([A] =>
          (json: Json.Enumeration.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]]): Json.Enumeration.Write[A] =
        new Write[A]:
          override def self: Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]] = annotation

      given Contravariant[Json.Enumeration.Write] =
        Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Json.Primitive.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]]) => Write(self))([A] =>
            (json: Json.Enumeration.Write[A]) => json.self
          )

      given [A] => Annotated[Json.Enumeration.Write[A]] =
        Annotated[Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]]]
          .imap(Write.apply)(_.self)

      given EnumerationOperation.Write[Json.Enumeration.Write, Json.Primitive.Write] = EnumerationOperation
        .Write[[a] =>> Annotation[Self.Enumeration.Write[Json.Primitive.Write, a]], Json.Primitive.Write]
        .imapK([A] => (self: Annotation[Self.Enumeration.Write[Json.Primitive.Write, A]]) => Write(self))([A] =>
          (json: Json.Enumeration.Write[A]) => json.self
        )

    given Invariant[Json.Enumeration] = Invariant[[a] =>> Annotation[Self.Enumeration[Json.Primitive, a]]]
      .imapK([A] => (self: Annotation[Self.Enumeration[Json.Primitive, A]]) => Enumeration(self))([A] =>
        (json: Json.Enumeration[A]) => json.self
      )

    given [A] => Annotated[Json.Enumeration[A]] = Annotated[Annotation[Self.Enumeration[Json.Primitive, A]]]
      .imap(Enumeration.apply)(_.self)

    given EnumerationOperation[Json.Enumeration, Json.Primitive] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Json.Primitive, a]], Json.Primitive]
        .imapK([A] => (self: Annotation[Self.Enumeration[Json.Primitive, A]]) => Enumeration(self))([A] =>
          (json: Json.Enumeration[A]) => json.self
        )

  final case class Optional[A](self: Annotation[Self.Optional[Json, A]])
      extends Json[A],
        Json.Optional.Read[A],
        Json.Optional.Write[A]

  object Optional:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Optional.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Optional.Read[Json.Read, A]]): Json.Optional.Read[A] = new Read[A]:
        override def self: Annotation[Self.Optional.Read[Json.Read, A]] = annotation

      given Functor[Json.Optional.Read]:
        override def map[A, B](fa: Json.Optional.Read[A])(f: A => B): Json.Optional.Read[B] = fa.map(f)

      given [A] => Annotated[Json.Optional.Read[A]] = Annotated[Annotation[Self.Optional.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

      given operation: OptionalOperation.Read[Json.Optional.Read, Json.Read] = OptionalOperation
        .Read[[a] =>> Annotation[Self.Optional.Read[Json.Read, a]], Json.Read]
        .imapK([A] => (self: Annotation[Self.Optional.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Optional.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Optional.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Optional.Write[Json.Write, A]]): Json.Optional.Write[A] =
        new Write[A]:
          override def self: Annotation[Self.Optional.Write[Json.Write, A]] = annotation

      given Contravariant[Json.Optional.Write]:
        override def contramap[A, B](fa: Json.Optional.Write[A])(f: B => A): Json.Optional.Write[B] = fa.contramap(f)

      given [A] => Annotated[Json.Optional.Write[A]] =
        Annotated[Annotation[Self.Optional.Write[Json.Write, A]]].imap(Write.apply)(_.self)

      given operation: OptionalOperation.Write[Json.Optional.Write, Json.Write] = OptionalOperation
        .Write[[a] =>> Annotation[Self.Optional.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Optional.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Optional.Write[A]) => json.self
        )

    given Invariant[Json.Optional] = Invariant[[a] =>> Annotation[Self.Optional[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Optional[Json, A]]) => Optional(self))([A] =>
        (json: Json.Optional[A]) => json.self
      )

    given [A] => Annotated[Json.Optional[A]] = Annotated[Annotation[Self.Optional[Json, A]]]
      .imap(Optional.apply)(_.self)

    given operation: OptionalOperation[Json.Optional, Json] =
      OptionalOperation[[a] =>> Annotation[Self.Optional[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Optional[Json, A]]) => Optional(self))([A] =>
          (json: Json.Optional[A]) => json.self
        )

  sealed abstract class Primitive[A] extends Json[A], Json.Primitive.Read[A], Json.Primitive.Write[A]:
    override def self: Annotation[Self.Primitive[Json.Primitive, A]]

  object Primitive:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Primitive.Read[Json.Primitive.Read, A]]

    object Read:
      given [A] => Annotated[Json.Primitive.Read[A]]:
        extension (self: Json.Primitive.Read[A])
          override def lens: (Metadata, Metadata => Json.Primitive.Read[A]) = self match
            case json: Json.Primitive.Boolean.Read[A] => Annotated[Json.Primitive.Boolean.Read[A]].lens(json)
            case json: Json.Primitive.Coerce.Read[A]  => Annotated[Json.Primitive.Coerce.Read[A]].lens(json)
            case json: Json.Primitive.Number.Read[A]  => Annotated[Json.Primitive.Number.Read[A]].lens(json)
            case json: Json.Primitive.Text.Read[A]    => Annotated[Json.Primitive.Text.Read[A]].lens(json)

      given Functor[Json.Primitive.Read]:
        override def map[A, B](json: Json.Primitive.Read[A])(f: A => B): Json.Primitive.Read[B] = json match
          case json: Json.Primitive.Boolean.Read[A] => json.map(f)
          case json: Json.Primitive.Coerce.Read[A]  => json.map(f)
          case json: Json.Primitive.Number.Read[A]  => json.map(f)
          case json: Json.Primitive.Text.Read[A]    => json.map(f)

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Primitive.Write[Json.Primitive.Write, A]]

    object Write:
      given [A] => Annotated[Json.Primitive.Write[A]]:
        extension (self: Json.Primitive.Write[A])
          override def lens: (Metadata, Metadata => Json.Primitive.Write[A]) = self match
            case json: Json.Primitive.Boolean.Write[A] => Annotated[Json.Primitive.Boolean.Write[A]].lens(json)
            case json: Json.Primitive.Coerce.Write[A]  => Annotated[Json.Primitive.Coerce.Write[A]].lens(json)
            case json: Json.Primitive.Number.Write[A]  => Annotated[Json.Primitive.Number.Write[A]].lens(json)
            case json: Json.Primitive.Text.Write[A]    => Annotated[Json.Primitive.Text.Write[A]].lens(json)

      given Contravariant[Json.Primitive.Write]:
        override def contramap[A, B](json: Json.Primitive.Write[A])(f: B => A): Json.Primitive.Write[B] =
          json match
            case json: Json.Primitive.Boolean.Write[A] => json.contramap(f)
            case json: Json.Primitive.Coerce.Write[A]  => json.contramap(f)
            case json: Json.Primitive.Number.Write[A]  => json.contramap(f)
            case json: Json.Primitive.Text.Write[A]    => json.contramap(f)

    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
        extends Json.Primitive[A],
          Json.Primitive.Boolean.Read[A],
          Json.Primitive.Boolean.Write[A]

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

        given [A] => Annotated[Json.Primitive.Boolean.Read[A]] = Annotated[Annotation[Self.Primitive.Boolean.Read[A]]]
          .imap(Read.apply)(_.self)

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

        given [A] => Annotated[Json.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Boolean.Write[Json.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
          .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Write[A]]) => Write(self))([A] =>
            (json: Json.Primitive.Boolean.Write[A]) => json.self
          )

      given Invariant[Json.Primitive.Boolean] = Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self))([A] =>
          (json: Json.Primitive.Boolean[A]) => json.self
        )

      given PrimitiveOperation.Boolean[Json.Primitive.Boolean] = PrimitiveOperation
        .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self))([A] =>
          (json: Json.Primitive.Boolean[A]) => json.self
        )

      given [A] => Annotated[Json.Primitive.Boolean[A]] = Annotated[Annotation[Self.Primitive.Boolean[A]]]
        .imap(Boolean.apply)(_.self)

    sealed abstract class Coerce[A]
        extends Json.Primitive[A],
          Json.Primitive.Coerce.Read[A],
          Json.Primitive.Coerce.Write[A]:
      override def self: Annotation[Self.Primitive.Coerce[Json.Primitive, A]]

    object Coerce:
      sealed trait Read[+A] extends Json.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Coerce.Read[Json.Primitive.Read, A]]

      object Read:
        given [A] => Annotated[Json.Primitive.Coerce.Read[A]]:
          extension (self: Json.Primitive.Coerce.Read[A])
            override def lens: (Metadata, Metadata => Json.Primitive.Coerce.Read[A]) = self match
              case json: Json.Primitive.Coerce.Boolean.Read[A] =>
                Annotated[Json.Primitive.Coerce.Boolean.Read[A]].lens(json)
              case json: Json.Primitive.Coerce.Number.Read[A] =>
                Annotated[Json.Primitive.Coerce.Number.Read[A]].lens(json)
              case json: Json.Primitive.Coerce.Text.Read[A] => Annotated[Json.Primitive.Coerce.Text.Read[A]].lens(json)

        given Functor[Json.Primitive.Coerce.Read]:
          override def map[A, B](json: Json.Primitive.Coerce.Read[A])(f: A => B): Json.Primitive.Coerce.Read[B] =
            json match
              case json: Json.Primitive.Coerce.Boolean.Read[A] => json.map(f)
              case json: Json.Primitive.Coerce.Number.Read[A]  => json.map(f)
              case json: Json.Primitive.Coerce.Text.Read[A]    => json.map(f)

      sealed trait Write[-A] extends Json.Primitive.Write[A]:
        override def self: Annotation[Self.Primitive.Coerce.Write[Json.Primitive.Write, A]]

      object Write:
        given [A] => Annotated[Json.Primitive.Coerce.Write[A]]:
          extension (self: Json.Primitive.Coerce.Write[A])
            override def lens: (Metadata, Metadata => Json.Primitive.Coerce.Write[A]) = self match
              case json: Json.Primitive.Coerce.Boolean.Write[A] =>
                Annotated[Json.Primitive.Coerce.Boolean.Write[A]].lens(json)
              case json: Json.Primitive.Coerce.Number.Write[A] =>
                Annotated[Json.Primitive.Coerce.Number.Write[A]].lens(json)
              case json: Json.Primitive.Coerce.Text.Write[A] =>
                Annotated[Json.Primitive.Coerce.Text.Write[A]].lens(json)

        given Contravariant[Json.Primitive.Coerce.Write]:
          override def contramap[A, B](json: Json.Primitive.Coerce.Write[A])(
              f: B => A
          ): Json.Primitive.Coerce.Write[B] =
            json match
              case json: Json.Primitive.Coerce.Boolean.Write[A] => json.contramap(f)
              case json: Json.Primitive.Coerce.Number.Write[A]  => json.contramap(f)
              case json: Json.Primitive.Coerce.Text.Write[A]    => json.contramap(f)

      final case class Boolean[A](self: Annotation[Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, A]])
          extends Json.Primitive.Coerce[A],
            Json.Primitive.Coerce.Boolean.Read[A],
            Json.Primitive.Coerce.Boolean.Write[A]

      object Boolean:
        sealed trait Read[+A] extends Json.Primitive.Coerce.Read[A]:
          override def self: Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]]
          ): Json.Primitive.Coerce.Boolean.Read[A] = new Json.Primitive.Coerce.Boolean.Read[A]:
            override def self: Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]] =
              annotation

          given Functor[Json.Primitive.Coerce.Boolean.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, a]]]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]]) => Read(self)
              )([A] => (json: Json.Primitive.Coerce.Boolean.Read[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Boolean.Read[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]]]
              .imap(Read.apply)(_.self)

          given PrimitiveOperation.Coerce.Boolean.Read[
            Json.Primitive.Coerce.Boolean.Read,
            Json.Primitive.Boolean.Read
          ] = PrimitiveOperation.Coerce.Boolean
            .Read[
              [a] =>> Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, a]],
              Json.Primitive.Boolean.Read
            ]
            .imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Boolean.Read[Json.Primitive.Boolean.Read, A]]) => Read(self)
            )([A] => (json: Json.Primitive.Coerce.Boolean.Read[A]) => json.self)

        sealed trait Write[-A] extends Json.Primitive.Coerce.Write[A]:
          override def self: Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]]
          ): Json.Primitive.Coerce.Boolean.Write[A] = new Json.Primitive.Coerce.Boolean.Write[A]:
            override def self: Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]] =
              annotation

          given Contravariant[Json.Primitive.Coerce.Boolean.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, a]]]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]]) => Write(self)
              )([A] => (json: Json.Primitive.Coerce.Boolean.Write[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Boolean.Write[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]]]
              .imap(Write.apply)(_.self)

          given PrimitiveOperation.Coerce.Boolean.Write[
            Json.Primitive.Coerce.Boolean.Write,
            Json.Primitive.Boolean.Write
          ] = PrimitiveOperation.Coerce.Boolean
            .Write[
              [a] =>> Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, a]],
              Json.Primitive.Boolean.Write
            ]
            .imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Boolean.Write[Json.Primitive.Boolean.Write, A]]) => Write(self)
            )([A] => (json: Json.Primitive.Coerce.Boolean.Write[A]) => json.self)

        given Invariant[Json.Primitive.Coerce.Boolean] =
          Invariant[[a] =>> Annotation[Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, a]]]
            .imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, A]]) => Boolean(self)
            )([A] => (json: Json.Primitive.Coerce.Boolean[A]) => json.self)

        given [A] => Annotated[Json.Primitive.Coerce.Boolean[A]] =
          Annotated[Annotation[Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, A]]]
            .imap(Boolean.apply)(_.self)

        given PrimitiveOperation.Coerce.Boolean[Json.Primitive.Coerce.Boolean, Json.Primitive.Boolean] =
          PrimitiveOperation.Coerce
            .Boolean[[a] =>> Annotation[
              Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, a]
            ], Json.Primitive.Boolean]
            .imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Boolean[Json.Primitive.Boolean, A]]) => Boolean(self)
            )([A] => (json: Json.Primitive.Coerce.Boolean[A]) => json.self)

      final case class Number[A](self: Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, A]])
          extends Json.Primitive.Coerce[A],
            Json.Primitive.Coerce.Number.Read[A],
            Json.Primitive.Coerce.Number.Write[A]

      object Number:
        sealed trait Read[+A] extends Json.Primitive.Coerce.Read[A]:
          override def self: Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]]
          ): Json.Primitive.Coerce.Number.Read[A] = new Read[A]:
            override def self: Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]] = annotation

          given Functor[Json.Primitive.Coerce.Number.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, a]]].imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]]) => Read(self)
            )([A] => (json: Json.Primitive.Coerce.Number.Read[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Number.Read[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]]]
              .imap(Read.apply)(_.self)

          given PrimitiveOperation.Coerce.Number.Read[Json.Primitive.Coerce.Number.Read, Json.Primitive.Number.Read] =
            PrimitiveOperation.Coerce.Number
              .Read[
                [a] =>> Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, a]],
                Json.Primitive.Number.Read
              ]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Number.Read[Json.Primitive.Number.Read, A]]) => Read(self)
              )([A] => (json: Json.Primitive.Coerce.Number.Read[A]) => json.self)

        sealed trait Write[-A] extends Json.Primitive.Coerce.Write[A]:
          override def self: Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]]
          ): Json.Primitive.Coerce.Number.Write[A] = new Write[A]:
            override def self: Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]] =
              annotation

          given Contravariant[Json.Primitive.Coerce.Number.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, a]]]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]]) => Write(self)
              )([A] => (json: Json.Primitive.Coerce.Number.Write[A]) => json.self)

          given PrimitiveOperation.Coerce.Number.Write[
            Json.Primitive.Coerce.Number.Write,
            Json.Primitive.Number.Write
          ] = PrimitiveOperation.Coerce.Number
            .Write[
              [a] =>> Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, a]],
              Json.Primitive.Number.Write
            ]
            .imapK([A] =>
              (self: Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]]) => Write(self)
            )([A] => (json: Json.Primitive.Coerce.Number.Write[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Number.Write[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Number.Write[Json.Primitive.Number.Write, A]]]
              .imap(Write.apply)(_.self)

        given Invariant[Json.Primitive.Coerce.Number] =
          Invariant[[a] =>> Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, A]]) => Number(self))(
              [A] => (json: Json.Primitive.Coerce.Number[A]) => json.self
            )

        given [A] => Annotated[Json.Primitive.Coerce.Number[A]] =
          Annotated[Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, A]]]
            .imap(Number.apply)(_.self)

        given PrimitiveOperation.Coerce.Number[Json.Primitive.Coerce.Number, Json.Primitive.Number] =
          PrimitiveOperation.Coerce
            .Number[[a] =>> Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, a]], Json.Primitive.Number]
            .imapK([A] => (self: Annotation[Self.Primitive.Coerce.Number[Json.Primitive.Number, A]]) => Number(self))(
              [A] => (json: Json.Primitive.Coerce.Number[A]) => json.self
            )

      final case class Text[A](self: Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, A]])
          extends Json.Primitive.Coerce[A],
            Json.Primitive.Coerce.Text.Read[A],
            Json.Primitive.Coerce.Text.Write[A]

      object Text:
        sealed trait Read[+A] extends Json.Primitive.Coerce.Read[A]:
          override def self: Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]]
          ): Json.Primitive.Coerce.Text.Read[A] = new Read[A]:
            override def self: Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]] = annotation

          given Functor[Json.Primitive.Coerce.Text.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, a]]]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]]) => Read(self)
              )([A] => (json: Json.Primitive.Coerce.Text.Read[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Text.Read[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]]]
              .imap(Read.apply)(_.self)

          given PrimitiveOperation.Coerce.Text.Read[Json.Primitive.Coerce.Text.Read, Json.Primitive.Text.Read] =
            PrimitiveOperation.Coerce.Text
              .Read[
                [a] =>> Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, a]],
                Json.Primitive.Text.Read
              ]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Text.Read[Json.Primitive.Text.Read, A]]) => Read(self)
              )([A] => (json: Json.Primitive.Coerce.Text.Read[A]) => json.self)

        sealed trait Write[-A] extends Json.Primitive.Coerce.Write[A]:
          override def self: Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]]
          ): Json.Primitive.Coerce.Text.Write[A] = new Json.Primitive.Coerce.Text.Write[A]:
            override def self: Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]] = annotation

          given Contravariant[Json.Primitive.Coerce.Text.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, a]]]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]]) => Write(self)
              )([A] => (json: Json.Primitive.Coerce.Text.Write[A]) => json.self)

          given [A] => Annotated[Json.Primitive.Coerce.Text.Write[A]] =
            Annotated[Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]]]
              .imap(Write.apply)(_.self)

          given PrimitiveOperation.Coerce.Text.Write[Json.Primitive.Coerce.Text.Write, Json.Primitive.Text.Write] =
            PrimitiveOperation.Coerce.Text
              .Write[
                [a] =>> Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, a]],
                Json.Primitive.Text.Write
              ]
              .imapK([A] =>
                (self: Annotation[Self.Primitive.Coerce.Text.Write[Json.Primitive.Text.Write, A]]) => Write(self)
              )([A] => (json: Json.Primitive.Coerce.Text.Write[A]) => json.self)

        given Invariant[Json.Primitive.Coerce.Text] =
          Invariant[[a] =>> Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, A]]) => Text(self))([A] =>
              (json: Json.Primitive.Coerce.Text[A]) => json.self
            )

        given [A] => Annotated[Json.Primitive.Coerce.Text[A]] =
          Annotated[Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, A]]]
            .imap(Text.apply)(_.self)

        given PrimitiveOperation.Coerce.Text[Json.Primitive.Coerce.Text, Json.Primitive.Text] =
          PrimitiveOperation.Coerce
            .Text[[a] =>> Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, a]], Json.Primitive.Text]
            .imapK([A] => (self: Annotation[Self.Primitive.Coerce.Text[Json.Primitive.Text, A]]) => Text(self))([A] =>
              (json: Json.Primitive.Coerce.Text[A]) => json.self
            )

      given [A] => Annotated[Json.Primitive.Coerce[A]]:
        extension (self: Json.Primitive.Coerce[A])
          override def lens: (Metadata, Metadata => Json.Primitive.Coerce[A]) = self match
            case json: Json.Primitive.Coerce.Boolean[A] => Annotated[Json.Primitive.Coerce.Boolean[A]].lens(json)
            case json: Json.Primitive.Coerce.Number[A]  => Annotated[Json.Primitive.Coerce.Number[A]].lens(json)
            case json: Json.Primitive.Coerce.Text[A]    => Annotated[Json.Primitive.Coerce.Text[A]].lens(json)

      given Invariant[Json.Primitive.Coerce]:
        override def imap[A, B](fa: Json.Primitive.Coerce[A])(f: A => B)(g: B => A): Json.Primitive.Coerce[B] =
          fa match
            case json: Json.Primitive.Coerce.Boolean[A] => json.imap(f)(g)
            case json: Json.Primitive.Coerce.Number[A]  => json.imap(f)(g)
            case json: Json.Primitive.Coerce.Text[A]    => json.imap(f)(g)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
        extends Json.Primitive[A],
          Json.Primitive.Number.Read[A],
          Json.Primitive.Number.Write[A]

    object Number:
      sealed trait Read[+A] extends Json.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Number.Read[A]]): Json.Primitive.Number.Read[A] =
          new Read[A]:
            override def self: Annotation[Self.Primitive.Number.Read[A]] = annotation

        given Functor[Json.Primitive.Number.Read] = Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Number.Read[A]) => json.self
          )

        given [A] => Annotated[Json.Primitive.Number.Read[A]] = Annotated[Annotation[Self.Primitive.Number.Read[A]]]
          .imap(Read.apply)(_.self)

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

        given [A] => Annotated[Json.Primitive.Number.Write[A]] =
          Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

      given Invariant[Json.Primitive.Number] = Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self))([A] =>
          (json: Json.Primitive.Number[A]) => json.self
        )

      given [A] => Annotated[Json.Primitive.Number[A]] = Annotated[Annotation[Self.Primitive.Number[A]]]
        .imap(Number.apply)(_.self)

      given PrimitiveOperation.Number[Json.Primitive.Number] = PrimitiveOperation
        .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self))([A] =>
          (json: Json.Primitive.Number[A]) => json.self
        )

    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends Json.Primitive[A],
          Json.Primitive.Text.Read[A],
          Json.Primitive.Text.Write[A]

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

        given [A] => Annotated[Json.Primitive.Text.Read[A]] = Annotated[Annotation[Self.Primitive.Text.Read[A]]]
          .imap(Read.apply)(_.self)

        given PrimitiveOperation.Text.Read[Json.Primitive.Text.Read] = PrimitiveOperation.Text
          .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (json: Json.Primitive.Text.Read[A]) => json.self
          )

        given Translator[Value.Primitive.Text.Read, Json.Primitive.Text.Read]:
          override def translate[A](value: Value.Primitive.Text.Read[A]): Json.Primitive.Text.Read[A] =
            Json.Primitive.Text.Read(value.self)

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

        given [A] => Annotated[Json.Primitive.Text.Write[A]] =
          Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

        given Translator[Value.Primitive.Text.Write, Json.Primitive.Text.Write]:
          override def translate[A](value: Value.Primitive.Text.Write[A]): Json.Primitive.Text.Write[A] =
            Json.Primitive.Text.Write(value.self)

      given Invariant[Json.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
          (json: Json.Primitive.Text[A]) => json.self
        )

      given [A] => Annotated[Json.Primitive.Text[A]] = Annotated[Annotation[Self.Primitive.Text[A]]]
        .imap(Text.apply)(_.self)

      given PrimitiveOperation.Text[Json.Primitive.Text] = PrimitiveOperation
        .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
          (json: Json.Primitive.Text[A]) => json.self
        )

      given Translator[Value.Primitive.Text, Json.Primitive.Text]:
        override def translate[A](value: Value.Primitive.Text[A]): Json.Primitive.Text[A] =
          Json.Primitive.Text(value.self)

    given Invariant[Json.Primitive]:
      override def imap[A, B](json: Json.Primitive[A])(f: A => B)(g: B => A): Json.Primitive[B] = json match
        case json: Json.Primitive.Boolean[A] => json.imap(f)(g)
        case json: Json.Primitive.Coerce[A]  => json.imap(f)(g)
        case json: Json.Primitive.Number[A]  => json.imap(f)(g)
        case json: Json.Primitive.Text[A]    => json.imap(f)(g)

    given [A] => Annotated[Json.Primitive[A]]:
      extension (self: Json.Primitive[A])
        override def lens: (Metadata, Metadata => Json.Primitive[A]) = self match
          case json: Json.Primitive.Boolean[A] => Annotated[Json.Primitive.Boolean[A]].lens(json)
          case json: Json.Primitive.Coerce[A]  => Annotated[Json.Primitive.Coerce[A]].lens(json)
          case json: Json.Primitive.Number[A]  => Annotated[Json.Primitive.Number[A]].lens(json)
          case json: Json.Primitive.Text[A]    => Annotated[Json.Primitive.Text[A]].lens(json)

  final case class Record[A](self: Annotation[Self.Record[Json.Field, A]])
      extends Json[A],
        Json.Record.Read[A],
        Json.Record.Write[A]

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

      given [A] => Annotated[Json.Record.Read[A]] = Annotated[Annotation[Self.Record.Read[Json.Field.Read, A]]]
        .imap(Read.apply)(_.self)

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

      given [A] => Annotated[Json.Record.Write[A]] = Annotated[Annotation[Self.Record.Write[Json.Field.Write, A]]]
        .imap(Write.apply)(_.self)

      given RecordOperation.Write[Json.Record.Write, Json.Field.Write] = RecordOperation
        .Write[[a] =>> Annotation[Self.Record.Write[Json.Field.Write, a]], Json.Field.Write]
        .imapK([A] => (self: Annotation[Self.Record.Write[Json.Field.Write, A]]) => Write(self))([A] =>
          (json: Json.Record.Write[A]) => json.self
        )

    given InvariantSemigroupal[Json.Record] = InvariantSemigroupal[[a] =>> Annotation[Self.Record[Json.Field, a]]]
      .imapK([A] => (self: Annotation[Self.Record[Json.Field, A]]) => Record(self))([A] =>
        (json: Json.Record[A]) => json.self
      )

    given [A] => Annotated[Json.Record[A]] = Annotated[Annotation[Self.Record[Json.Field, A]]]
      .imap(Record.apply)(_.self)

    given RecordOperation[Json.Record, Json.Field] =
      RecordOperation[[a] =>> Annotation[Self.Record[Json.Field, a]], Json.Field]
        .imapK([A] => (self: Annotation[Self.Record[Json.Field, A]]) => Record(self))([A] =>
          (json: Json.Record[A]) => json.self
        )

  final case class Tuple[A](self: Annotation[Self.Tuple[Json, A]])
      extends Json[A],
        Json.Tuple.Read[A],
        Json.Tuple.Write[A]

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

      given [A] => Annotated[Json.Tuple.Read[A]] = Annotated[Annotation[Self.Tuple.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

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

      given [A] => Annotated[Json.Tuple.Write[A]] = Annotated[Annotation[Self.Tuple.Write[Json.Write, A]]]
        .imap(Write.apply)(_.self)

      given TupleOperation[Json.Tuple.Write, Json.Write] = TupleOperation
        .Write[[a] =>> Annotation[Self.Tuple.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Tuple.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Tuple.Write[A]) => json.self
        )

    given InvariantSemigroupal[Json.Tuple] = InvariantSemigroupal[[a] =>> Annotation[Self.Tuple[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

    given [A] => Annotated[Json.Tuple[A]] = Annotated[Annotation[Self.Tuple[Json, A]]]
      .imap(Tuple.apply)(_.self)

    given TupleOperation[Json.Tuple, Json] = TupleOperation[[a] =>> Annotation[Self.Tuple[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Annotation[Self.Union[Json.Branch, A]])
      extends Json[A],
        Json.Union.Read[A],
        Json.Union.Write[A]

  object Union:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Union.Read[Json.Branch.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Union.Read[Json.Branch.Read, A]]): Json.Union.Read[A] =
        new Json.Union.Read[A]:
          override def self: Annotation[Self.Union.Read[Json.Branch.Read, A]] = annotation

      given Functor[Json.Union.Read]:
        override def map[A, B](fa: Json.Union.Read[A])(f: A => B): Json.Union.Read[B] = fa.map(f)

      given [A] => Annotated[Json.Union.Read[A]] = Annotated[Annotation[Self.Union.Read[Json.Branch.Read, A]]]
        .imap(Read.apply)(_.self)

      given UnionOperation.Read[Json.Union.Read, Json.Branch.Read] = UnionOperation
        .Read[[a] =>> Annotation[Self.Union.Read[Json.Branch.Read, a]], Json.Branch.Read]
        .imapK([A] => (self: Annotation[Self.Union.Read[Json.Branch.Read, A]]) => Read(self))([A] =>
          (json: Json.Union.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Union.Write[Json.Branch.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Union.Write[Json.Branch.Write, A]]): Json.Union.Write[A] =
        new Json.Union.Write[A]:
          override def self: Annotation[Self.Union.Write[Json.Branch.Write, A]] = annotation

      given Contravariant[Json.Union.Write]:
        override def contramap[A, B](fa: Json.Union.Write[A])(f: B => A): Json.Union.Write[B] = fa.contramap(f)

      given [A] => Annotated[Json.Union.Write[A]] = Annotated[Annotation[Self.Union.Write[Json.Branch.Write, A]]]
        .imap(Write.apply)(_.self)

      given UnionOperation.Write[Json.Union.Write, Json.Branch.Write] = UnionOperation
        .Write[[a] =>> Annotation[Self.Union.Write[Json.Branch.Write, a]], Json.Branch.Write]
        .imapK([A] => (self: Annotation[Self.Union.Write[Json.Branch.Write, A]]) => Write(self))([A] =>
          (json: Json.Union.Write[A]) => json.self
        )

    given Invariant[Json.Union] = Invariant[[a] =>> Annotation[Self.Union[Json.Branch, a]]]
      .imapK([A] => (self: Annotation[Self.Union[Json.Branch, A]]) => Union(self))([A] =>
        (json: Json.Union[A]) => json.self
      )

    given [A] => Annotated[Json.Union[A]] = Annotated[Annotation[Self.Union[Json.Branch, A]]]
      .imap(Union.apply)(_.self)

    given UnionOperation[Json.Union, Json.Branch] =
      UnionOperation[[a] =>> Annotation[Self.Union[Json.Branch, a]], Json.Branch]
        .imapK([A] => (self: Annotation[Self.Union[Json.Branch, A]]) => Union(self))([A] =>
          (json: Json.Union[A]) => json.self
        )

  final case class Branch[A](self: Annotation[Self.Branch[Json, A]]) extends Json.Branch.Read[A], Json.Branch.Write[A]

  object Branch:
    sealed trait Read[+A]:
      def self: Annotation[Self.Branch.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Branch.Read[Json.Read, A]]): Json.Branch.Read[A] = new Read[A]:
        override def self: Annotation[Self.Branch.Read[Json.Read, A]] = annotation

      given Functor[Json.Branch.Read]:
        override def map[A, B](fa: Json.Branch.Read[A])(f: A => B): Json.Branch.Read[B] = fa.map(f)

      given [A] => Annotated[Json.Branch.Read[A]] = Annotated[Annotation[Self.Branch.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

      given operation: BranchOperation.Read[Json.Branch.Read, Json.Read] = BranchOperation
        .Read[[a] =>> Annotation[Self.Branch.Read[Json.Read, a]], Json.Read]
        .imapK([A] => (self: Annotation[Self.Branch.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Branch.Read[A]) => json.self
        )

      given unionable: UnionableOperation.Read[Json.Branch.Read, Json.Union.Read] = UnionableOperation.Read.derived

    sealed trait Write[-A]:
      def self: Annotation[Self.Branch.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Branch.Write[Json.Write, A]]): Json.Branch.Write[A] = new Write[A]:
        override def self: Annotation[Self.Branch.Write[Json.Write, A]] = annotation

      given [A] => Annotated[Json.Branch.Write[A]] = Annotated[Annotation[Self.Branch.Write[Json.Write, A]]]
        .imap(Write.apply)(_.self)

      given operation: BranchOperation.Write[Json.Branch.Write, Json.Write] = BranchOperation
        .Write[[a] =>> Annotation[Self.Branch.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Branch.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Branch.Write[A]) => json.self
        )

      given unionable: UnionableOperation.Write[Json.Branch.Write, Json.Union.Write] = UnionableOperation.Write.derived

    given [A] => Annotated[Json.Branch[A]] = Annotated[Annotation[Self.Branch[Json, A]]]
      .imap(Branch.apply)(_.self)

    given operation: BranchOperation[Json.Branch, Json] =
      BranchOperation[[a] =>> Annotation[Self.Branch[Json, a]], Json]
        .imapK([A] => (self: Annotation[Self.Branch[Json, A]]) => Branch(self))([A] =>
          (json: Json.Branch[A]) => json.self
        )

    given unionable: UnionableOperation[Json.Branch, Json.Union] = UnionableOperation.derived

  final case class Field[A](self: Annotation[Self.Field[Json, A]]) extends Json.Field.Read[A], Json.Field.Write[A]

  object Field:
    sealed trait Read[+A]:
      def self: Annotation[Self.Field.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Field.Read[Json.Read, A]]): Json.Field.Read[A] = new Read[A]:
        override def self: Annotation[Self.Field.Read[Json.Read, A]] = annotation

      given Functor[Json.Field.Read]:
        override def map[A, B](fa: Json.Field.Read[A])(f: A => B): Json.Field.Read[B] = fa.map(f)

      given [A] => Annotated[Json.Field.Read[A]] = Annotated[Annotation[Self.Field.Read[Json.Read, A]]]
        .imap(Read.apply)(_.self)

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

      given [A] => Annotated[Json.Field.Write[A]] = Annotated[Annotation[Self.Field.Write[Json.Write, A]]]
        .imap(Write.apply)(_.self)

      given operation: FieldOperation.Write[Json.Field.Write, Json.Write] = FieldOperation
        .Write[[a] =>> Annotation[Self.Field.Write[Json.Write, a]], Json.Write]
        .imapK([A] => (self: Annotation[Self.Field.Write[Json.Write, A]]) => Write(self))([A] =>
          (json: Json.Field.Write[A]) => json.self
        )

      given recordable: RecordableOperation.Write[Json.Field.Write, Json.Record.Write] =
        RecordableOperation.Write.derived

    given [A] => Annotated[Json.Field[A]] = Annotated[Annotation[Self.Field[Json, A]]]
      .imap(Field.apply)(_.self)

    given operation: FieldOperation[Json.Field, Json] = FieldOperation[[a] =>> Annotation[Self.Field[Json, a]], Json]
      .imapK([A] => (self: Annotation[Self.Field[Json, A]]) => Field(self))([A] => (json: Json.Field[A]) => json.self)

    given recordable: RecordableOperation[Json.Field, Json.Record] = RecordableOperation.derived

  given Invariant[Json]:
    override def imap[A, B](json: Json[A])(f: A => B)(g: B => A): Json[B] = json match
      case json: Json.Collection[A]  => json.imap(f)(g)
      case json: Json.Constant[A]    => json.imap(f)(g)
      case json: Json.Dictionary[A]  => json.imap(f)(g)
      case json: Json.Enumeration[A] => json.imap(f)(g)
      case json: Json.Optional[A]    => json.imap(f)(g)
      case json: Json.Primitive[A]   => json.imap(f)(g)
      case json: Json.Record[A]      => json.imap(f)(g)
      case json: Json.Tuple[A]       => json.imap(f)(g)
      case json: Json.Union[A]       => json.imap(f)(g)

  given [A] => Annotated[Json[A]]:
    extension (self: Json[A])
      override def lens: (Metadata, Metadata => Json[A]) = self match
        case json: Json.Collection[A]  => Annotated[Json.Collection[A]].lens(json)
        case json: Json.Constant[A]    => Annotated[Json.Constant[A]].lens(json)
        case json: Json.Dictionary[A]  => Annotated[Json.Dictionary[A]].lens(json)
        case json: Json.Enumeration[A] => Annotated[Json.Enumeration[A]].lens(json)
        case json: Json.Optional[A]    => Annotated[Json.Optional[A]].lens(json)
        case json: Json.Primitive[A]   => Annotated[Json.Primitive[A]].lens(json)
        case json: Json.Record[A]      => Annotated[Json.Record[A]].lens(json)
        case json: Json.Tuple[A]       => Annotated[Json.Tuple[A]].lens(json)
        case json: Json.Union[A]       => Annotated[Json.Union[A]].lens(json)

  given optionalable: OptionalableOperation[Json, Json.Optional] = OptionalableOperation.derived

  given tupleable: TupleableOperation[Json, Json.Tuple] = TupleableOperation.derived

  given Translator[Value, Json]:
    override def translate[A](value: Value[A]): Json[A] = value match
      case value: Value.Constant[A]       => Translator[Value.Constant, Json.Constant].translate(value)
      case value: Value.Primitive.Text[A] => Translator[Value.Primitive.Text, Json.Primitive.Text].translate(value)
