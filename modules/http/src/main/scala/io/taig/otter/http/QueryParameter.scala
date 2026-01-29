package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Functor
import cats.Invariant
import cats.Contravariant

sealed abstract class QueryParameter[A] extends QueryParameter.Read[A], QueryParameter.Write[A]:
  override def self: Annotation[
    Self.Constant[QueryParameter.Primitive.Text, A] | Self.Enumeration[QueryParameter.Primitive.Text, A] |
      Self.Primitive.Text[A] | Self.Union[QueryParameter.Branch, A] | Self.Optional[QueryParameter, A] |
      Self.Collection[QueryParameter, A]
  ]

object QueryParameter:
  sealed trait Read[+A]:
    def self: Annotation[
      Self.Constant.Read[QueryParameter.Primitive.Text.Read, A] |
        Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A] | Self.Primitive.Text.Read[A] |
        Self.Union.Read[QueryParameter.Branch.Read, A] | Self.Optional.Read[QueryParameter.Read, A] |
        Self.Collection.Read[QueryParameter.Read, A]
    ]

  sealed trait Write[-A]:
    def self: Annotation[
      Self.Constant.Write[QueryParameter.Primitive.Text.Write, A] |
        Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, A] | Self.Primitive.Text.Write[A] |
        Self.Union.Write[QueryParameter.Branch.Write, A] | Self.Optional.Write[QueryParameter.Write, A] |
        Self.Collection.Write[QueryParameter.Write, A]
    ]

  final case class Constant[A](self: Annotation[Self.Constant[QueryParameter.Primitive.Text, A]])
      extends QueryParameter[A],
        QueryParameter.Constant.Read[A],
        QueryParameter.Constant.Write[A]

  object Constant:
    sealed trait Read[+A] extends QueryParameter.Read[A]:
      def self: Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]]
      ): QueryParameter.Constant.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]] = annotation

      given Functor[QueryParameter.Constant.Read] =
        Functor[[a] =>> Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Constant.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Constant.Read[A]] =
        Annotated[Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

      given ConstantOperation.Read[QueryParameter.Constant.Read, QueryParameter.Primitive.Text.Read] =
        ConstantOperation
          .Read[
            [a] =>> Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, a]],
            QueryParameter.Primitive.Text.Read
          ]
          .imapK([A] => (self: Annotation[Self.Constant.Read[QueryParameter.Primitive.Text.Read, A]]) => Read(self))(
            [A] => (parameter: QueryParameter.Constant.Read[A]) => parameter.self
          )

    sealed trait Write[-A] extends QueryParameter.Write[A]:
      def self: Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]]
      ): QueryParameter.Constant.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]] =
          annotation

      given Contravariant[QueryParameter.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (parameter: QueryParameter.Constant.Write[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Constant.Write[A]] =
        Annotated[Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]]]
          .imap(Write.apply)(_.self)

      given ConstantOperation.Write[QueryParameter.Constant.Write, QueryParameter.Primitive.Text.Write] =
        ConstantOperation
          .Write[
            [a] =>> Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, a]],
            QueryParameter.Primitive.Text.Write
          ]
          .imapK([A] =>
            (annotation: Annotation[Self.Constant.Write[QueryParameter.Primitive.Text.Write, A]]) => Write(annotation)
          )([A] => (parameter: QueryParameter.Constant.Write[A]) => parameter.self)

    given Invariant[QueryParameter.Constant] =
      Invariant[[a] =>> Annotation[Self.Constant[QueryParameter.Primitive.Text, a]]].imapK([A] =>
        (self: Annotation[Self.Constant[QueryParameter.Primitive.Text, A]]) => Constant(self)
      )([A] => (parameter: QueryParameter.Constant[A]) => parameter.self)

    given [A] => Annotated[QueryParameter.Constant[A]] =
      Annotated[Annotation[Self.Constant[QueryParameter.Primitive.Text, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[QueryParameter.Constant, QueryParameter.Primitive.Text] = ConstantOperation[
      [a] =>> Annotation[Self.Constant[QueryParameter.Primitive.Text, a]],
      QueryParameter.Primitive.Text
    ].imapK([A] => Constant(_))([A] => _.self)

  final case class Enumeration[A](self: Annotation[Self.Enumeration[QueryParameter.Primitive.Text, A]])
      extends QueryParameter[A],
        QueryParameter.Enumeration.Read[A],
        QueryParameter.Enumeration.Write[A]

  object Enumeration:
    sealed trait Read[+A] extends QueryParameter.Read[A]:
      override def self: Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A]]
      ): QueryParameter.Enumeration.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A]] =
          annotation

      given Functor[QueryParameter.Enumeration.Read] =
        Functor[[a] =>> Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Enumeration.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Enumeration.Read[A]] =
        Annotated[Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, A]]]
          .imap(Read.apply)(_.self)

      given EnumerationOperation.Read[QueryParameter.Enumeration.Read, QueryParameter.Primitive.Text.Read] =
        EnumerationOperation
          .Read[
            [a] =>> Annotation[Self.Enumeration.Read[QueryParameter.Primitive.Text.Read, a]],
            QueryParameter.Primitive.Text.Read
          ]
          .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A] extends QueryParameter.Write[A]:
      override def self: Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, A]]
      ): QueryParameter.Enumeration.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, A]] =
          annotation

      given Contravariant[QueryParameter.Enumeration.Write] =
        Contravariant[[a] =>> Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[QueryParameter.Enumeration.Write[A]] =
        Annotated[Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, A]]]
          .imap(Write.apply)(_.self)

      given EnumerationOperation.Write[QueryParameter.Enumeration.Write, QueryParameter.Primitive.Text.Write] =
        EnumerationOperation
          .Write[
            [a] =>> Annotation[Self.Enumeration.Write[QueryParameter.Primitive.Text.Write, a]],
            QueryParameter.Primitive.Text.Write
          ]
          .imapK([A] => Write(_))([A] => _.self)

    given Invariant[QueryParameter.Enumeration] =
      Invariant[[a] =>> Annotation[Self.Enumeration[QueryParameter.Primitive.Text, a]]]
        .imapK([A] => Enumeration(_))([A] => _.self)

    given [A] => Annotated[QueryParameter.Enumeration[A]] =
      Annotated[Annotation[Self.Enumeration[QueryParameter.Primitive.Text, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[QueryParameter.Enumeration, QueryParameter.Primitive.Text] =
      EnumerationOperation[
        [a] =>> Annotation[Self.Enumeration[QueryParameter.Primitive.Text, a]],
        QueryParameter.Primitive.Text
      ].imapK([A] => Enumeration(_))([A] => _.self)

  sealed abstract class Primitive[A]
      extends QueryParameter[A],
        QueryParameter.Primitive.Read[A],
        QueryParameter.Primitive.Write[A]

  object Primitive:
    sealed trait Read[+A] extends QueryParameter.Read[A]

    sealed trait Write[-A] extends QueryParameter.Write[A]

    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
        extends QueryParameter.Primitive.Boolean.Read[A],
          QueryParameter.Primitive.Boolean.Write[A]

    object Boolean:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Primitive.Boolean.Read[A]]
        ): QueryParameter.Primitive.Boolean.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Primitive.Boolean.Read[A]] = annotation

        given Functor[QueryParameter.Primitive.Boolean.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Boolean.Read[QueryParameter.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
          .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Boolean.Write[A]]
        ): QueryParameter.Primitive.Boolean.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Primitive.Boolean.Write[A]] = annotation

        given Contravariant[QueryParameter.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Boolean.Write[QueryParameter.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
          .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[QueryParameter.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK([A] => Primitive.Boolean(_))([A] => _.self)

      given [A] => Annotated[QueryParameter.Primitive.Boolean[A]] =
        Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

      given PrimitiveOperation.Boolean[QueryParameter.Primitive.Boolean] = PrimitiveOperation
        .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => Primitive.Boolean(_))([A] => _.self)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
        extends QueryParameter.Primitive.Number.Read[A],
          QueryParameter.Primitive.Number.Write[A]

    object Number:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Number.Read[A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Primitive.Number.Read[A]]
        ): QueryParameter.Primitive.Number.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Primitive.Number.Read[A]] = annotation

        given Functor[QueryParameter.Primitive.Number.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Number.Read[A]] =
          Annotated[Annotation[Self.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Number.Read[QueryParameter.Primitive.Number.Read] = PrimitiveOperation.Number
          .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Number.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Number.Write[A]]
        ): QueryParameter.Primitive.Number.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Primitive.Number.Write[A]] = annotation

        given Contravariant[QueryParameter.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Number.Write[A]] =
          Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Number.Write[QueryParameter.Primitive.Number.Write] = PrimitiveOperation.Number
          .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[QueryParameter.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] => Primitive.Number(_))([A] => _.self)

      given [A] => Annotated[QueryParameter.Primitive.Number[A]] =
        Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

      given PrimitiveOperation.Number[QueryParameter.Primitive.Number] = PrimitiveOperation
        .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => Primitive.Number(_))([A] => _.self)

    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends QueryParameter.Primitive[A],
          QueryParameter.Primitive.Text.Read[A],
          QueryParameter.Primitive.Text.Write[A]

    object Text:
      sealed trait Read[+A] extends QueryParameter.Read[A]:
        def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): QueryParameter.Primitive.Text.Read[A] =
          new Read[A]:
            override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[QueryParameter.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Text.Read[A]] =
          Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Text.Read[QueryParameter.Primitive.Text.Read] = PrimitiveOperation.Text
          .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends QueryParameter.Write[A]:
        def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Text.Write[A]]
        ): QueryParameter.Primitive.Text.Write[A] =
          new Write[A]:
            override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[QueryParameter.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[QueryParameter.Primitive.Text.Write[A]] =
          Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Text.Write[QueryParameter.Primitive.Text.Write] = PrimitiveOperation.Text
          .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[QueryParameter.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => Text(_))([A] => _.self)

      given [A] => Annotated[QueryParameter.Primitive.Text[A]] =
        Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

      given PrimitiveOperation.Text[QueryParameter.Primitive.Text] = PrimitiveOperation
        .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => Text(_))([A] => _.self)

  final case class Union[A](self: Annotation[Self.Union[QueryParameter.Branch, A]])
      extends QueryParameter[A],
        QueryParameter.Union.Read[A],
        QueryParameter.Union.Write[A]

  object Union:
    sealed trait Read[+A] extends QueryParameter.Read[A]:
      def self: Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]]
      ): QueryParameter.Union.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]] = annotation

      given Functor[QueryParameter.Union.Read] =
        Functor[[a] =>> Annotation[Self.Union.Read[QueryParameter.Branch.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Union.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Union.Read[A]] =
        Annotated[Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]]].imap(Read.apply)(_.self)

      given UnionOperation.Read[QueryParameter.Union.Read, QueryParameter.Branch.Read] = UnionOperation
        .Read[
          [a] =>> Annotation[Self.Union.Read[QueryParameter.Branch.Read, a]],
          QueryParameter.Branch.Read
        ]
        .imapK([A] => (self: Annotation[Self.Union.Read[QueryParameter.Branch.Read, A]]) => Read(self))([A] =>
          (parameter: QueryParameter.Union.Read[A]) => parameter.self
        )

    sealed trait Write[-A] extends QueryParameter.Write[A]:
      def self: Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]]
      ): QueryParameter.Union.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]] = annotation

      given Contravariant[QueryParameter.Union.Write] =
        Contravariant[[a] =>> Annotation[Self.Union.Write[QueryParameter.Branch.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]]) => Write(annotation)
        )([A] => (parameter: QueryParameter.Union.Write[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Union.Write[A]] =
        Annotated[Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]]].imap(Write.apply)(_.self)

      given UnionOperation.Write[QueryParameter.Union.Write, QueryParameter.Branch.Write] = UnionOperation
        .Write[
          [a] =>> Annotation[Self.Union.Write[QueryParameter.Branch.Write, a]],
          QueryParameter.Branch.Write
        ]
        .imapK([A] => (annotation: Annotation[Self.Union.Write[QueryParameter.Branch.Write, A]]) => Write(annotation))(
          [A] => (parameter: QueryParameter.Union.Write[A]) => parameter.self
        )

    given Invariant[QueryParameter.Union] =
      Invariant[[a] =>> Annotation[Self.Union[QueryParameter.Branch, a]]].imapK([A] =>
        (annotation: Annotation[Self.Union[QueryParameter.Branch, A]]) => Union(annotation)
      )([A] => (parameter: QueryParameter.Union[A]) => parameter.self)

    given [A] => Annotated[QueryParameter.Union[A]] =
      Annotated[Annotation[Self.Union[QueryParameter.Branch, A]]].imap(Union.apply)(_.self)

    given UnionOperation[QueryParameter.Union, QueryParameter.Branch] = UnionOperation[
      [a] =>> Annotation[Self.Union[QueryParameter.Branch, a]],
      QueryParameter.Branch
    ].imapK([A] => (annotation: Annotation[Self.Union[QueryParameter.Branch, A]]) => Union(annotation))([A] =>
      (parameter: QueryParameter.Union[A]) => parameter.self
    )

  final case class Optional[A](self: Annotation[Self.Optional[QueryParameter, A]])
      extends QueryParameter[A],
        QueryParameter.Optional.Read[A],
        QueryParameter.Optional.Write[A]

  object Optional:
    sealed trait Read[+A] extends QueryParameter.Read[A]:
      def self: Annotation[Self.Optional.Read[QueryParameter.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Optional.Read[QueryParameter.Read, A]]
      ): QueryParameter.Optional.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Optional.Read[QueryParameter.Read, A]] = annotation

      given Functor[QueryParameter.Optional.Read] =
        Functor[[a] =>> Annotation[Self.Optional.Read[QueryParameter.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Optional.Read[QueryParameter.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Optional.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Optional.Read[A]] =
        Annotated[Annotation[Self.Optional.Read[QueryParameter.Read, A]]].imap(Read.apply)(_.self)

      given OptionalOperation.Read[QueryParameter.Optional.Read, QueryParameter.Read] = OptionalOperation
        .Read[[a] =>> Annotation[Self.Optional.Read[QueryParameter.Read, a]], QueryParameter.Read]
        .imapK([A] => (self: Annotation[Self.Optional.Read[QueryParameter.Read, A]]) => Read(self))([A] =>
          (parameter: QueryParameter.Optional.Read[A]) => parameter.self
        )

    sealed trait Write[-A] extends QueryParameter.Write[A]:
      def self: Annotation[Self.Optional.Write[QueryParameter.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Optional.Write[QueryParameter.Write, A]]
      ): QueryParameter.Optional.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Optional.Write[QueryParameter.Write, A]] = annotation

      given Contravariant[QueryParameter.Optional.Write] =
        Contravariant[[a] =>> Annotation[Self.Optional.Write[QueryParameter.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Optional.Write[QueryParameter.Write, A]]) => Write(annotation)
        )([A] => (parameter: QueryParameter.Optional.Write[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Optional.Write[A]] =
        Annotated[Annotation[Self.Optional.Write[QueryParameter.Write, A]]].imap(Write.apply)(_.self)

      given OptionalOperation.Write[QueryParameter.Optional.Write, QueryParameter.Write] = OptionalOperation
        .Write[[a] =>> Annotation[Self.Optional.Write[QueryParameter.Write, a]], QueryParameter.Write]
        .imapK([A] => (annotation: Annotation[Self.Optional.Write[QueryParameter.Write, A]]) => Write(annotation))(
          [A] => (parameter: QueryParameter.Optional.Write[A]) => parameter.self
        )

    given Invariant[QueryParameter.Optional] =
      Invariant[[a] =>> Annotation[Self.Optional[QueryParameter, a]]].imapK([A] =>
        (annotation: Annotation[Self.Optional[QueryParameter, A]]) => Optional(annotation)
      )([A] => (parameter: QueryParameter.Optional[A]) => parameter.self)

    given [A] => Annotated[QueryParameter.Optional[A]] =
      Annotated[Annotation[Self.Optional[QueryParameter, A]]].imap(Optional.apply)(_.self)

    given OptionalOperation[QueryParameter.Optional, QueryParameter] =
      OptionalOperation[[a] =>> Annotation[Self.Optional[QueryParameter, a]], QueryParameter].imapK([A] =>
        (annotation: Annotation[Self.Optional[QueryParameter, A]]) => Optional(annotation)
      )([A] => (parameter: QueryParameter.Optional[A]) => parameter.self)

  final case class Collection[A](self: Annotation[Self.Collection[QueryParameter, A]])
      extends QueryParameter[A],
        QueryParameter.Collection.Read[A],
        QueryParameter.Collection.Write[A]

  object Collection:
    sealed trait Read[+A] extends QueryParameter.Read[A]:
      def self: Annotation[Self.Collection.Read[QueryParameter.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Collection.Read[QueryParameter.Read, A]]
      ): QueryParameter.Collection.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Collection.Read[QueryParameter.Read, A]] = annotation

      given Functor[QueryParameter.Collection.Read] =
        Functor[[a] =>> Annotation[Self.Collection.Read[QueryParameter.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Collection.Read[QueryParameter.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Collection.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Collection.Read[A]] =
        Annotated[Annotation[Self.Collection.Read[QueryParameter.Read, A]]].imap(Read.apply)(_.self)

      given CollectionOperation.Read[QueryParameter.Collection.Read, QueryParameter.Read] = CollectionOperation
        .Read[[a] =>> Annotation[Self.Collection.Read[QueryParameter.Read, a]], QueryParameter.Read]
        .imapK([A] => (self: Annotation[Self.Collection.Read[QueryParameter.Read, A]]) => Read(self))([A] =>
          (parameter: QueryParameter.Collection.Read[A]) => parameter.self
        )

    sealed trait Write[-A] extends QueryParameter.Write[A]:
      def self: Annotation[Self.Collection.Write[QueryParameter.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Collection.Write[QueryParameter.Write, A]]
      ): QueryParameter.Collection.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Collection.Write[QueryParameter.Write, A]] = annotation

      given Contravariant[QueryParameter.Collection.Write] =
        Contravariant[[a] =>> Annotation[Self.Collection.Write[QueryParameter.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Collection.Write[QueryParameter.Write, A]]) => Write(annotation)
        )([A] => (parameter: QueryParameter.Collection.Write[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Collection.Write[A]] =
        Annotated[Annotation[Self.Collection.Write[QueryParameter.Write, A]]].imap(Write.apply)(_.self)

      given CollectionOperation.Write[QueryParameter.Collection.Write, QueryParameter.Write] = CollectionOperation
        .Write[[a] =>> Annotation[Self.Collection.Write[QueryParameter.Write, a]], QueryParameter.Write]
        .imapK([A] => (annotation: Annotation[Self.Collection.Write[QueryParameter.Write, A]]) => Write(annotation))(
          [A] => (parameter: QueryParameter.Collection.Write[A]) => parameter.self
        )

    given Invariant[QueryParameter.Collection] =
      Invariant[[a] =>> Annotation[Self.Collection[QueryParameter, a]]].imapK([A] =>
        (annotation: Annotation[Self.Collection[QueryParameter, A]]) => Collection(annotation)
      )([A] => (parameter: QueryParameter.Collection[A]) => parameter.self)

    given [A] => Annotated[QueryParameter.Collection[A]] =
      Annotated[Annotation[Self.Collection[QueryParameter, A]]].imap(Collection.apply)(_.self)

    given CollectionOperation[QueryParameter.Collection, QueryParameter] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[QueryParameter, a]], QueryParameter].imapK([A] =>
        (annotation: Annotation[Self.Collection[QueryParameter, A]]) => Collection(annotation)
      )([A] => (parameter: QueryParameter.Collection[A]) => parameter.self)

  final case class Branch[A](self: Annotation[Self.Branch[QueryParameter, A]])
      extends QueryParameter.Branch.Read[A],
        QueryParameter.Branch.Write[A]

  object Branch:
    sealed trait Read[+A]:
      def self: Annotation[Self.Branch.Read[QueryParameter.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Branch.Read[QueryParameter.Read, A]]
      ): QueryParameter.Branch.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Branch.Read[QueryParameter.Read, A]] = annotation

      given Functor[QueryParameter.Branch.Read] =
        Functor[[a] =>> Annotation[Self.Branch.Read[QueryParameter.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Branch.Read[QueryParameter.Read, A]]) => Read(self)
        )([A] => (parameter: QueryParameter.Branch.Read[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Branch.Read[A]] =
        Annotated[Annotation[Self.Branch.Read[QueryParameter.Read, A]]].imap(Read.apply)(_.self)

    sealed trait Write[-A]:
      def self: Annotation[Self.Branch.Write[QueryParameter.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Branch.Write[QueryParameter.Write, A]]
      ): QueryParameter.Branch.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Branch.Write[QueryParameter.Write, A]] = annotation

      given Contravariant[QueryParameter.Branch.Write] =
        Contravariant[[a] =>> Annotation[Self.Branch.Write[QueryParameter.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Branch.Write[QueryParameter.Write, A]]) => Write(annotation)
        )([A] => (parameter: QueryParameter.Branch.Write[A]) => parameter.self)

      given [A] => Annotated[QueryParameter.Branch.Write[A]] =
        Annotated[Annotation[Self.Branch.Write[QueryParameter.Write, A]]].imap(Write.apply)(_.self)

    given Invariant[QueryParameter.Branch] =
      Invariant[[a] =>> Annotation[Self.Branch[QueryParameter, a]]].imapK([A] =>
        (annotation: Annotation[Self.Branch[QueryParameter, A]]) => QueryParameter.Branch(annotation)
      )([A] => (parameter: QueryParameter.Branch[A]) => parameter.self)

    given [A] => Annotated[QueryParameter.Branch[A]] =
      Annotated[Annotation[Self.Branch[QueryParameter, A]]].imap(QueryParameter.Branch.apply)(_.self)
