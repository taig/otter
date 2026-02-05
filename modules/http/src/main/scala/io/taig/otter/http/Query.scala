package io.taig.otter.http

import io.taig.otter.Reference
import cats.Eval
import io.taig.otter.http.operation.QueryOperation
import io.taig.otter.syntax.all.*
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Functor
import cats.Invariant
import cats.Contravariant

type Query[A] = Annotation[Query.Schema[Query.Parameter, A]]

object Query:
  type Read[A] = Annotation[Query.Schema.Read[Query.Parameter.Read, A]]

  type Write[A] = Annotation[Query.Schema.Write[Query.Parameter.Write, A]]

  abstract class Schema[+F[_], A] extends Schema.Read[F, A], Schema.Write[F, A]:
    final def imap[B](f: A => B)(g: B => A): Schema[F, B] = Schema.Modify(self = this, f, g)

    final override def optional: Schema[F, Option[A]] = Schema.Optional(self = this)

    final def optional(default: Eval[A]): Schema[F, A] = Schema.Default(self = this, value = default)

    override def parameter: Reference[F, ?]

  object Schema:
    trait Read[+F[_], +A]:
      final def map[B](f: A => B): Schema.Read[F, B] = Read.Modify(self = this, f)

      def name: String

      def optional: Schema.Read[F, Option[A]] = Read.Optional(self = this)

      def optional[A1 >: A](default: Eval[A1]): Schema.Read[F, A1] = Read.Default(self = this, value = default)

      def parameter: Reference[F, ?]

    object Read:
      final case class Default[F[_], A](self: Schema.Read[F, A], value: Eval[A]) extends Schema.Read[F, A]:
        export self.{name, parameter}

      final case class Modify[F[_], A, B](self: Schema.Read[F, A], f: A => B) extends Schema.Read[F, B]:
        export self.{name, parameter}

      final case class Optional[F[_], A](self: Schema.Read[F, A]) extends Schema.Read[F, Option[A]]:
        export self.{name, parameter}

      given [F[_]] => Functor[Schema.Read[F, *]]:
        override def map[A, B](query: Schema.Read[F, A])(f: A => B): Schema.Read[F, B] = query.map(f)

      given [F[_]] => QueryOperation.Read[Schema.Read[F, *], F]:
        override def lift[A](name: String, parameter: Reference[F, A]): Schema.Read[F, A] =
          Root(name, parameter)

    trait Write[+F[_], -A]:
      final def contramap[B](f: B => A): Schema.Write[F, B] = Write.Modify(self = this, f)

      def name: String

      def optional: Schema.Write[F, Option[A]] = Write.Optional(self = this)

      def parameter: Reference[F, ?]

    object Write:
      final case class Modify[F[_], A, B](self: Schema.Write[F, A], f: B => A) extends Schema.Write[F, B]:
        export self.{name, parameter}

      final case class Optional[F[_], A](self: Schema.Write[F, A]) extends Schema.Write[F, Option[A]]:
        export self.{name, parameter}

      final case class Root[F[_], A](name: String, parameter: Reference[F, A]) extends Schema.Write[F, A]

      given [F[_]] => Contravariant[Schema.Write[F, *]]:
        override def contramap[A, B](query: Schema.Write[F, A])(f: B => A): Schema.Write[F, B] = query.contramap(f)

      given [F[_]] => QueryOperation.Write[Schema.Write[F, *], F]:
        override def lift[A](name: String, parameter: Reference[F, A]): Schema.Write[F, A] =
          Root(name, parameter)

    final case class Default[F[_], A](self: Schema[F, A], value: Eval[A]) extends Schema[F, A]:
      export self.{name, parameter}

    final case class Modify[F[_], A, B](self: Schema[F, A], f: A => B, g: B => A) extends Schema[F, B]:
      export self.{name, parameter}

    final case class Optional[F[_], A](self: Schema[F, A]) extends Schema[F, Option[A]]:
      export self.{name, parameter}

    final case class Root[F[_], A](name: String, parameter: Reference[F, A]) extends Schema[F, A]

    given [F[_]] => Invariant[Schema[F, *]]:
      override def imap[A, B](fa: Schema[F, A])(f: A => B)(g: B => A): Schema[F, B] = fa.imap(f)(g)

    given [F[_]] => QueryOperation[Schema[F, *], F]:
      override def lift[A](name: String, parameter: Reference[F, A]): Schema[F, A] =
        Root(name, parameter)

  sealed abstract class Parameter[A] extends Query.Parameter.Read[A], Query.Parameter.Write[A]:
    override def self: Annotation[
      Self.Constant[Query.Parameter.Primitive.Text, A] | Self.Enumeration[Query.Parameter.Primitive.Text, A] |
        Self.Primitive.Text[A] | Self.Union[Query.Parameter.Branch, A] | Self.Optional[Query.Parameter, A] |
        Self.Collection[Query.Parameter, A]
    ]

  object Parameter:
    sealed trait Read[+A]:
      def self: Annotation[
        Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A] |
          Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A] | Self.Primitive.Text.Read[A] |
          Self.Union.Read[Query.Parameter.Branch.Read, A] | Self.Optional.Read[Query.Parameter.Read, A] |
          Self.Collection.Read[Query.Parameter.Read, A]
      ]

    sealed trait Write[-A]:
      def self: Annotation[
        Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A] |
          Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, A] | Self.Primitive.Text.Write[A] |
          Self.Union.Write[Query.Parameter.Branch.Write, A] | Self.Optional.Write[Query.Parameter.Write, A] |
          Self.Collection.Write[Query.Parameter.Write, A]
      ]

    final case class Constant[A](self: Annotation[Self.Constant[Query.Parameter.Primitive.Text, A]])
        extends Query.Parameter[A],
          Query.Parameter.Constant.Read[A],
          Query.Parameter.Constant.Write[A]

    object Constant:
      sealed trait Read[+A] extends Query.Parameter.Read[A]:
        def self: Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]]
        ): Query.Parameter.Constant.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]] = annotation

        given Functor[Query.Parameter.Constant.Read] =
          Functor[[a] =>> Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Constant.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Constant.Read[A]] =
          Annotated[Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

        given ConstantOperation.Read[Query.Parameter.Constant.Read, Query.Parameter.Primitive.Text.Read] =
          ConstantOperation
            .Read[
              [a] =>> Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, a]],
              Query.Parameter.Primitive.Text.Read
            ]
            .imapK([A] => (self: Annotation[Self.Constant.Read[Query.Parameter.Primitive.Text.Read, A]]) => Read(self))(
              [A] => (parameter: Query.Parameter.Constant.Read[A]) => parameter.self
            )

      sealed trait Write[-A] extends Query.Parameter.Write[A]:
        def self: Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]]
        ): Query.Parameter.Constant.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]] =
            annotation

        given Contravariant[Query.Parameter.Constant.Write] =
          Contravariant[[a] =>> Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]]) => Write(annotation)
          )([A] => (parameter: Query.Parameter.Constant.Write[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Constant.Write[A]] =
          Annotated[Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]]]
            .imap(Write.apply)(_.self)

        given ConstantOperation.Write[Query.Parameter.Constant.Write, Query.Parameter.Primitive.Text.Write] =
          ConstantOperation
            .Write[
              [a] =>> Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, a]],
              Query.Parameter.Primitive.Text.Write
            ]
            .imapK([A] =>
              (annotation: Annotation[Self.Constant.Write[Query.Parameter.Primitive.Text.Write, A]]) => Write(annotation)
            )([A] => (parameter: Query.Parameter.Constant.Write[A]) => parameter.self)

      given Invariant[Query.Parameter.Constant] =
        Invariant[[a] =>> Annotation[Self.Constant[Query.Parameter.Primitive.Text, a]]].imapK([A] =>
          (self: Annotation[Self.Constant[Query.Parameter.Primitive.Text, A]]) => Constant(self)
        )([A] => (parameter: Query.Parameter.Constant[A]) => parameter.self)

      given [A] => Annotated[Query.Parameter.Constant[A]] =
        Annotated[Annotation[Self.Constant[Query.Parameter.Primitive.Text, A]]].imap(Constant.apply)(_.self)

      given ConstantOperation[Query.Parameter.Constant, Query.Parameter.Primitive.Text] = ConstantOperation[
        [a] =>> Annotation[Self.Constant[Query.Parameter.Primitive.Text, a]],
        Query.Parameter.Primitive.Text
      ].imapK([A] => Constant(_))([A] => _.self)

    final case class Enumeration[A](self: Annotation[Self.Enumeration[Query.Parameter.Primitive.Text, A]])
        extends Query.Parameter[A],
          Query.Parameter.Enumeration.Read[A],
          Query.Parameter.Enumeration.Write[A]

    object Enumeration:
      sealed trait Read[+A] extends Query.Parameter.Read[A]:
        override def self: Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A]]
        ): Query.Parameter.Enumeration.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A]] =
            annotation

        given Functor[Query.Parameter.Enumeration.Read] =
          Functor[[a] =>> Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Enumeration.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Enumeration.Read[A]] =
          Annotated[Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, A]]]
            .imap(Read.apply)(_.self)

        given EnumerationOperation.Read[Query.Parameter.Enumeration.Read, Query.Parameter.Primitive.Text.Read] =
          EnumerationOperation
            .Read[
              [a] =>> Annotation[Self.Enumeration.Read[Query.Parameter.Primitive.Text.Read, a]],
              Query.Parameter.Primitive.Text.Read
            ]
            .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends Query.Parameter.Write[A]:
        override def self: Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, A]]
        ): Query.Parameter.Enumeration.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, A]] =
            annotation

        given Contravariant[Query.Parameter.Enumeration.Write] =
          Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[Query.Parameter.Enumeration.Write[A]] =
          Annotated[Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, A]]]
            .imap(Write.apply)(_.self)

        given EnumerationOperation.Write[Query.Parameter.Enumeration.Write, Query.Parameter.Primitive.Text.Write] =
          EnumerationOperation
            .Write[
              [a] =>> Annotation[Self.Enumeration.Write[Query.Parameter.Primitive.Text.Write, a]],
              Query.Parameter.Primitive.Text.Write
            ]
            .imapK([A] => Write(_))([A] => _.self)

      given Invariant[Query.Parameter.Enumeration] =
        Invariant[[a] =>> Annotation[Self.Enumeration[Query.Parameter.Primitive.Text, a]]]
          .imapK([A] => Enumeration(_))([A] => _.self)

      given [A] => Annotated[Query.Parameter.Enumeration[A]] =
        Annotated[Annotation[Self.Enumeration[Query.Parameter.Primitive.Text, A]]].imap(Enumeration.apply)(_.self)

      given EnumerationOperation[Query.Parameter.Enumeration, Query.Parameter.Primitive.Text] =
        EnumerationOperation[
          [a] =>> Annotation[Self.Enumeration[Query.Parameter.Primitive.Text, a]],
          Query.Parameter.Primitive.Text
        ].imapK([A] => Enumeration(_))([A] => _.self)

    sealed abstract class Primitive[A]
        extends Query.Parameter[A],
          Query.Parameter.Primitive.Read[A],
          Query.Parameter.Primitive.Write[A]

    object Primitive:
      sealed trait Read[+A] extends Query.Parameter.Read[A]

      sealed trait Write[-A] extends Query.Parameter.Write[A]

      final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
          extends Query.Parameter.Primitive.Boolean.Read[A],
            Query.Parameter.Primitive.Boolean.Write[A]

      object Boolean:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Boolean.Read[A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Boolean.Read[A]]
          ): Query.Parameter.Primitive.Boolean.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Read[A]] = annotation

          given Functor[Query.Parameter.Primitive.Boolean.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Boolean.Read[A]] =
            Annotated[Annotation[Self.Primitive.Boolean.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Boolean.Read[Query.Parameter.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
            .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Boolean.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Boolean.Write[A]]
          ): Query.Parameter.Primitive.Boolean.Write[A] = new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Write[A]] = annotation

          given Contravariant[Query.Parameter.Primitive.Boolean.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Boolean.Write[A]] =
            Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Boolean.Write[Query.Parameter.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
            .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Query.Parameter.Primitive.Boolean] =
          Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK([A] => Primitive.Boolean(_))([A] => _.self)

        given [A] => Annotated[Query.Parameter.Primitive.Boolean[A]] =
          Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

        given PrimitiveOperation.Boolean[Query.Parameter.Primitive.Boolean] = PrimitiveOperation
          .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
          .imapK([A] => Primitive.Boolean(_))([A] => _.self)

      final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
          extends Query.Parameter.Primitive.Number.Read[A],
            Query.Parameter.Primitive.Number.Write[A]

      object Number:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Number.Read[A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Number.Read[A]]
          ): Query.Parameter.Primitive.Number.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Read[A]] = annotation

          given Functor[Query.Parameter.Primitive.Number.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Number.Read[A]] =
            Annotated[Annotation[Self.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Number.Read[Query.Parameter.Primitive.Number.Read] = PrimitiveOperation.Number
            .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Number.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Number.Write[A]]
          ): Query.Parameter.Primitive.Number.Write[A] = new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Write[A]] = annotation

          given Contravariant[Query.Parameter.Primitive.Number.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Number.Write[A]] =
            Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Number.Write[Query.Parameter.Primitive.Number.Write] = PrimitiveOperation.Number
            .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Query.Parameter.Primitive.Number] =
          Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] => Primitive.Number(_))([A] => _.self)

        given [A] => Annotated[Query.Parameter.Primitive.Number[A]] =
          Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

        given PrimitiveOperation.Number[Query.Parameter.Primitive.Number] = PrimitiveOperation
          .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
          .imapK([A] => Primitive.Number(_))([A] => _.self)

      final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
          extends Query.Parameter.Primitive[A],
            Query.Parameter.Primitive.Text.Read[A],
            Query.Parameter.Primitive.Text.Write[A]

      object Text:
        sealed trait Read[+A] extends Query.Parameter.Read[A]:
          def self: Annotation[Self.Primitive.Text.Read[A]]

        object Read:
          def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Query.Parameter.Primitive.Text.Read[A] =
            new Read[A]:
              override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

          given Functor[Query.Parameter.Primitive.Text.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Text.Read[A]] =
            Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Text.Read[Query.Parameter.Primitive.Text.Read] = PrimitiveOperation.Text
            .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A] extends Query.Parameter.Write[A]:
          def self: Annotation[Self.Primitive.Text.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Text.Write[A]]
          ): Query.Parameter.Primitive.Text.Write[A] =
            new Write[A]:
              override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

          given Contravariant[Query.Parameter.Primitive.Text.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Query.Parameter.Primitive.Text.Write[A]] =
            Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Text.Write[Query.Parameter.Primitive.Text.Write] = PrimitiveOperation.Text
            .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Query.Parameter.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

        given [A] => Annotated[Query.Parameter.Primitive.Text[A]] =
          Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

        given PrimitiveOperation.Text[Query.Parameter.Primitive.Text] = PrimitiveOperation
          .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

    final case class Union[A](self: Annotation[Self.Union[Query.Parameter.Branch, A]])
        extends Query.Parameter[A],
          Query.Parameter.Union.Read[A],
          Query.Parameter.Union.Write[A]

    object Union:
      sealed trait Read[+A] extends Query.Parameter.Read[A]:
        def self: Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]]
        ): Query.Parameter.Union.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]] = annotation

        given Functor[Query.Parameter.Union.Read] =
          Functor[[a] =>> Annotation[Self.Union.Read[Query.Parameter.Branch.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Union.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Union.Read[A]] =
          Annotated[Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]]].imap(Read.apply)(_.self)

        given UnionOperation.Read[Query.Parameter.Union.Read, Query.Parameter.Branch.Read] = UnionOperation
          .Read[
            [a] =>> Annotation[Self.Union.Read[Query.Parameter.Branch.Read, a]],
            Query.Parameter.Branch.Read
          ]
          .imapK([A] => (self: Annotation[Self.Union.Read[Query.Parameter.Branch.Read, A]]) => Read(self))([A] =>
            (parameter: Query.Parameter.Union.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Query.Parameter.Write[A]:
        def self: Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]]
        ): Query.Parameter.Union.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]] = annotation

        given Contravariant[Query.Parameter.Union.Write] =
          Contravariant[[a] =>> Annotation[Self.Union.Write[Query.Parameter.Branch.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]]) => Write(annotation)
          )([A] => (parameter: Query.Parameter.Union.Write[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Union.Write[A]] =
          Annotated[Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]]].imap(Write.apply)(_.self)

        given UnionOperation.Write[Query.Parameter.Union.Write, Query.Parameter.Branch.Write] = UnionOperation
          .Write[
            [a] =>> Annotation[Self.Union.Write[Query.Parameter.Branch.Write, a]],
            Query.Parameter.Branch.Write
          ]
          .imapK([A] => (annotation: Annotation[Self.Union.Write[Query.Parameter.Branch.Write, A]]) => Write(annotation))(
            [A] => (parameter: Query.Parameter.Union.Write[A]) => parameter.self
          )

      given Invariant[Query.Parameter.Union] =
        Invariant[[a] =>> Annotation[Self.Union[Query.Parameter.Branch, a]]].imapK([A] =>
          (annotation: Annotation[Self.Union[Query.Parameter.Branch, A]]) => Union(annotation)
        )([A] => (parameter: Query.Parameter.Union[A]) => parameter.self)

      given [A] => Annotated[Query.Parameter.Union[A]] =
        Annotated[Annotation[Self.Union[Query.Parameter.Branch, A]]].imap(Union.apply)(_.self)

      given UnionOperation[Query.Parameter.Union, Query.Parameter.Branch] = UnionOperation[
        [a] =>> Annotation[Self.Union[Query.Parameter.Branch, a]],
        Query.Parameter.Branch
      ].imapK([A] => (annotation: Annotation[Self.Union[Query.Parameter.Branch, A]]) => Union(annotation))([A] =>
        (parameter: Query.Parameter.Union[A]) => parameter.self
      )

    final case class Optional[A](self: Annotation[Self.Optional[Query.Parameter, A]])
        extends Query.Parameter[A],
          Query.Parameter.Optional.Read[A],
          Query.Parameter.Optional.Write[A]

    object Optional:
      sealed trait Read[+A] extends Query.Parameter.Read[A]:
        def self: Annotation[Self.Optional.Read[Query.Parameter.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Optional.Read[Query.Parameter.Read, A]]
        ): Query.Parameter.Optional.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Optional.Read[Query.Parameter.Read, A]] = annotation

        given Functor[Query.Parameter.Optional.Read] =
          Functor[[a] =>> Annotation[Self.Optional.Read[Query.Parameter.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Optional.Read[Query.Parameter.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Optional.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Optional.Read[A]] =
          Annotated[Annotation[Self.Optional.Read[Query.Parameter.Read, A]]].imap(Read.apply)(_.self)

        given OptionalOperation.Read[Query.Parameter.Optional.Read, Query.Parameter.Read] = OptionalOperation
          .Read[[a] =>> Annotation[Self.Optional.Read[Query.Parameter.Read, a]], Query.Parameter.Read]
          .imapK([A] => (self: Annotation[Self.Optional.Read[Query.Parameter.Read, A]]) => Read(self))([A] =>
            (parameter: Query.Parameter.Optional.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Query.Parameter.Write[A]:
        def self: Annotation[Self.Optional.Write[Query.Parameter.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Optional.Write[Query.Parameter.Write, A]]
        ): Query.Parameter.Optional.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Optional.Write[Query.Parameter.Write, A]] = annotation

        given Contravariant[Query.Parameter.Optional.Write] =
          Contravariant[[a] =>> Annotation[Self.Optional.Write[Query.Parameter.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Optional.Write[Query.Parameter.Write, A]]) => Write(annotation)
          )([A] => (parameter: Query.Parameter.Optional.Write[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Optional.Write[A]] =
          Annotated[Annotation[Self.Optional.Write[Query.Parameter.Write, A]]].imap(Write.apply)(_.self)

        given OptionalOperation.Write[Query.Parameter.Optional.Write, Query.Parameter.Write] = OptionalOperation
          .Write[[a] =>> Annotation[Self.Optional.Write[Query.Parameter.Write, a]], Query.Parameter.Write]
          .imapK([A] => (annotation: Annotation[Self.Optional.Write[Query.Parameter.Write, A]]) => Write(annotation))(
            [A] => (parameter: Query.Parameter.Optional.Write[A]) => parameter.self
          )

      given Invariant[Query.Parameter.Optional] =
        Invariant[[a] =>> Annotation[Self.Optional[Query.Parameter, a]]].imapK([A] =>
          (annotation: Annotation[Self.Optional[Query.Parameter, A]]) => Optional(annotation)
        )([A] => (parameter: Query.Parameter.Optional[A]) => parameter.self)

      given [A] => Annotated[Query.Parameter.Optional[A]] =
        Annotated[Annotation[Self.Optional[Query.Parameter, A]]].imap(Optional.apply)(_.self)

      given OptionalOperation[Query.Parameter.Optional, Query.Parameter] =
        OptionalOperation[[a] =>> Annotation[Self.Optional[Query.Parameter, a]], Query.Parameter].imapK([A] =>
          (annotation: Annotation[Self.Optional[Query.Parameter, A]]) => Optional(annotation)
        )([A] => (parameter: Query.Parameter.Optional[A]) => parameter.self)

    final case class Collection[A](self: Annotation[Self.Collection[Query.Parameter, A]])
        extends Query.Parameter[A],
          Query.Parameter.Collection.Read[A],
          Query.Parameter.Collection.Write[A]

    object Collection:
      sealed trait Read[+A] extends Query.Parameter.Read[A]:
        def self: Annotation[Self.Collection.Read[Query.Parameter.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Collection.Read[Query.Parameter.Read, A]]
        ): Query.Parameter.Collection.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Collection.Read[Query.Parameter.Read, A]] = annotation

        given Functor[Query.Parameter.Collection.Read] =
          Functor[[a] =>> Annotation[Self.Collection.Read[Query.Parameter.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Collection.Read[Query.Parameter.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Collection.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Collection.Read[A]] =
          Annotated[Annotation[Self.Collection.Read[Query.Parameter.Read, A]]].imap(Read.apply)(_.self)

        given CollectionOperation.Read[Query.Parameter.Collection.Read, Query.Parameter.Read] = CollectionOperation
          .Read[[a] =>> Annotation[Self.Collection.Read[Query.Parameter.Read, a]], Query.Parameter.Read]
          .imapK([A] => (self: Annotation[Self.Collection.Read[Query.Parameter.Read, A]]) => Read(self))([A] =>
            (parameter: Query.Parameter.Collection.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Query.Parameter.Write[A]:
        def self: Annotation[Self.Collection.Write[Query.Parameter.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Collection.Write[Query.Parameter.Write, A]]
        ): Query.Parameter.Collection.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Collection.Write[Query.Parameter.Write, A]] = annotation

        given Contravariant[Query.Parameter.Collection.Write] =
          Contravariant[[a] =>> Annotation[Self.Collection.Write[Query.Parameter.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Collection.Write[Query.Parameter.Write, A]]) => Write(annotation)
          )([A] => (parameter: Query.Parameter.Collection.Write[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Collection.Write[A]] =
          Annotated[Annotation[Self.Collection.Write[Query.Parameter.Write, A]]].imap(Write.apply)(_.self)

        given CollectionOperation.Write[Query.Parameter.Collection.Write, Query.Parameter.Write] = CollectionOperation
          .Write[[a] =>> Annotation[Self.Collection.Write[Query.Parameter.Write, a]], Query.Parameter.Write]
          .imapK([A] => (annotation: Annotation[Self.Collection.Write[Query.Parameter.Write, A]]) => Write(annotation))(
            [A] => (parameter: Query.Parameter.Collection.Write[A]) => parameter.self
          )

      given Invariant[Query.Parameter.Collection] =
        Invariant[[a] =>> Annotation[Self.Collection[Query.Parameter, a]]].imapK([A] =>
          (annotation: Annotation[Self.Collection[Query.Parameter, A]]) => Collection(annotation)
        )([A] => (parameter: Query.Parameter.Collection[A]) => parameter.self)

      given [A] => Annotated[Query.Parameter.Collection[A]] =
        Annotated[Annotation[Self.Collection[Query.Parameter, A]]].imap(Collection.apply)(_.self)

      given CollectionOperation[Query.Parameter.Collection, Query.Parameter] =
        CollectionOperation[[a] =>> Annotation[Self.Collection[Query.Parameter, a]], Query.Parameter].imapK([A] =>
          (annotation: Annotation[Self.Collection[Query.Parameter, A]]) => Collection(annotation)
        )([A] => (parameter: Query.Parameter.Collection[A]) => parameter.self)

    final case class Branch[A](self: Annotation[Self.Branch[Query.Parameter, A]])
        extends Query.Parameter.Branch.Read[A],
          Query.Parameter.Branch.Write[A]

    object Branch:
      sealed trait Read[+A]:
        def self: Annotation[Self.Branch.Read[Query.Parameter.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Branch.Read[Query.Parameter.Read, A]]
        ): Query.Parameter.Branch.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Branch.Read[Query.Parameter.Read, A]] = annotation

        given Functor[Query.Parameter.Branch.Read] =
          Functor[[a] =>> Annotation[Self.Branch.Read[Query.Parameter.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Branch.Read[Query.Parameter.Read, A]]) => Read(self)
          )([A] => (parameter: Query.Parameter.Branch.Read[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Branch.Read[A]] =
          Annotated[Annotation[Self.Branch.Read[Query.Parameter.Read, A]]].imap(Read.apply)(_.self)

      sealed trait Write[-A]:
        def self: Annotation[Self.Branch.Write[Query.Parameter.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Branch.Write[Query.Parameter.Write, A]]
        ): Query.Parameter.Branch.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Branch.Write[Query.Parameter.Write, A]] = annotation

        given Contravariant[Query.Parameter.Branch.Write] =
          Contravariant[[a] =>> Annotation[Self.Branch.Write[Query.Parameter.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Branch.Write[Query.Parameter.Write, A]]) => Write(annotation)
          )([A] => (parameter: Query.Parameter.Branch.Write[A]) => parameter.self)

        given [A] => Annotated[Query.Parameter.Branch.Write[A]] =
          Annotated[Annotation[Self.Branch.Write[Query.Parameter.Write, A]]].imap(Write.apply)(_.self)

      given Invariant[Query.Parameter.Branch] =
        Invariant[[a] =>> Annotation[Self.Branch[Query.Parameter, a]]].imapK([A] =>
          (annotation: Annotation[Self.Branch[Query.Parameter, A]]) => Query.Parameter.Branch(annotation)
        )([A] => (parameter: Query.Parameter.Branch[A]) => parameter.self)

      given [A] => Annotated[Query.Parameter.Branch[A]] =
        Annotated[Annotation[Self.Branch[Query.Parameter, A]]].imap(Query.Parameter.Branch.apply)(_.self)