package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Annotation
import cats.Functor
import cats.Contravariant
import cats.Invariant
import io.taig.otter.Annotated
import io.taig.otter.operation.EnumerationOperation
import io.taig.otter.operation.PrimitiveOperation
import Self.operation.ConstantOperation

sealed abstract class Parameter[A] extends Parameter.Read[A], Parameter.Write[A]:
  override def self: Annotation[
    Self.Constant[Parameter.Primitive.Text, A] | Self.Enumeration[Parameter.Primitive.Text, A] |
      Self.Primitive.Coerce[
        [a] =>> Parameter.Primitive.Boolean[a] | Parameter.Primitive.Number[a] | Parameter.Primitive.Text[a],
        A
      ] | Self.Primitive.Text[A] | Self.Union[Parameter.Branch, A]
  ]

object Parameter:
  sealed trait Read[+A]:
    def self: Annotation[
      Self.Constant.Read[Parameter.Primitive.Text.Read, A] | Self.Enumeration.Read[Parameter.Primitive.Text.Read, A] |
        Self.Primitive.Coerce.Read[
          [a] =>> Parameter.Primitive.Boolean.Read[a] | Parameter.Primitive.Number.Read[a] |
            Parameter.Primitive.Text.Read[a],
          A
        ] | Self.Primitive.Text.Read[A] | Self.Union.Read[Parameter.Branch.Read, A]
    ]

  sealed trait Write[-A]:
    def self: Annotation[
      Self.Constant.Write[Parameter.Primitive.Text.Write, A] |
        Self.Enumeration.Write[Parameter.Primitive.Text.Write, A] |
        Self.Primitive.Coerce.Write[
          [a] =>> Parameter.Primitive.Boolean.Write[a] | Parameter.Primitive.Number.Write[a] |
            Parameter.Primitive.Text.Write[a],
          A
        ] | Self.Primitive.Text.Write[A] | Self.Union.Write[Parameter.Branch.Write, A]
    ]

  final case class Constant[A](self: Annotation[Self.Constant[Parameter.Primitive.Text, A]])
      extends Parameter[A],
        Parameter.Constant.Read[A],
        Parameter.Constant.Write[A]

  object Constant:
    sealed trait Read[+A] extends Parameter.Read[A]:
      def self: Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]]
      ): Parameter.Constant.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]] = annotation

      given Functor[Parameter.Constant.Read] =
        Functor[[a] =>> Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]]) => Read(self)
        )([A] => (parameter: Parameter.Constant.Read[A]) => parameter.self)

      given [A] => Annotated[Parameter.Constant.Read[A]] =
        Annotated[Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

      given ConstantOperation.Read[Parameter.Constant.Read, Parameter.Primitive.Text.Read] = ConstantOperation
        .Read[
          [a] =>> Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, a]],
          Parameter.Primitive.Text.Read
        ]
        .imapK([A] => (self: Annotation[Self.Constant.Read[Parameter.Primitive.Text.Read, A]]) => Read(self))([A] =>
          (parameter: Parameter.Constant.Read[A]) => parameter.self
        )

    sealed trait Write[-A] extends Parameter.Write[A]:
      def self: Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]]
      ): Parameter.Constant.Write[A] =
        new Write[A]:
          override def self: Self.Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]] = annotation

      given Contravariant[Parameter.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (parameter: Parameter.Constant.Write[A]) => parameter.self)

      given [A] => Annotated[Parameter.Constant.Write[A]] =
        Annotated[Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]]].imap(Write.apply)(_.self)

      given ConstantOperation.Write[Parameter.Constant.Write, Parameter.Primitive.Text.Write] = ConstantOperation
        .Write[
          [a] =>> Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, a]],
          Parameter.Primitive.Text.Write
        ]
        .imapK([A] =>
          (annotation: Annotation[Self.Constant.Write[Parameter.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (parameter: Parameter.Constant.Write[A]) => parameter.self)

    given Invariant[Parameter.Constant] =
      Invariant[[a] =>> Annotation[Self.Constant[Parameter.Primitive.Text, a]]].imapK([A] =>
        (self: Annotation[Self.Constant[Parameter.Primitive.Text, A]]) => Constant(self)
      )([A] => (parameter: Parameter.Constant[A]) => parameter.self)

    given [A] => Annotated[Parameter.Constant[A]] =
      Annotated[Annotation[Self.Constant[Parameter.Primitive.Text, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Parameter.Constant, Parameter.Primitive.Text] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[Parameter.Primitive.Text, a]], Parameter.Primitive.Text].imapK(
        [A] => (self: Annotation[Self.Constant[Parameter.Primitive.Text, A]]) => Constant(self)
      )([A] => (parameter: Parameter.Constant[A]) => parameter.self)

  sealed abstract class Enumeration[A]
      extends Parameter[A],
        Parameter.Enumeration.Read[A],
        Parameter.Enumeration.Write[A]:
    override def self: Annotation[Self.Enumeration[Parameter.Primitive.Text, A]]

  object Enumeration:
    sealed trait Read[+A] extends Parameter.Read[A]:
      override def self: Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, A]]
      ): Parameter.Enumeration.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, A]] = annotation

      given Functor[Parameter.Enumeration.Read] =
        Functor[[a] =>> Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, A]]) => Read(self)
        )([A] => (parameter: Parameter.Enumeration.Read[A]) => parameter.self)

      given EnumerationOperation.Read[Parameter.Enumeration.Read, Parameter.Primitive.Text.Read] =
        EnumerationOperation
          .Read[[a] =>> Annotation[
            Self.Enumeration.Read[Parameter.Primitive.Text.Read, a]
          ], Parameter.Primitive.Text.Read]
          .imapK([A] => (self: Annotation[Self.Enumeration.Read[Parameter.Primitive.Text.Read, A]]) => Read(self))(
            [A] => (parameter: Parameter.Enumeration.Read[A]) => parameter.self
          )

    sealed trait Write[-A] extends Parameter.Write[A]:
      override def self: Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, A]]
      ): Parameter.Enumeration.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, A]] = annotation

      given Contravariant[Parameter.Enumeration.Write] =
        Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (parameter: Parameter.Enumeration.Write[A]) => parameter.self)

      given EnumerationOperation.Write[Parameter.Enumeration.Write, Parameter.Primitive.Text.Write] =
        EnumerationOperation
          .Write[
            [a] =>> Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, a]],
            Parameter.Primitive.Text.Write
          ]
          .imapK([A] =>
            (annotation: Annotation[Self.Enumeration.Write[Parameter.Primitive.Text.Write, A]]) => Write(annotation)
          )([A] => (parameter: Parameter.Enumeration.Write[A]) => parameter.self)

    def apply[A](annotation: Annotation[Self.Enumeration[Parameter.Primitive.Text, A]]): Parameter.Enumeration[A] =
      new Enumeration[A]:
        override def self: Self.Annotation[Self.Enumeration[Parameter.Primitive.Text, A]] = annotation

    given Invariant[Parameter.Enumeration] =
      Invariant[[a] =>> Annotation[Self.Enumeration[Parameter.Primitive.Text, a]]].imapK([A] =>
        (annotation: Annotation[Self.Enumeration[Parameter.Primitive.Text, A]]) => Enumeration(annotation)
      )([A] => (parameter: Parameter.Enumeration[A]) => parameter.self)

    given EnumerationOperation[Parameter.Enumeration, Parameter.Primitive.Text] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Parameter.Primitive.Text, a]], Parameter.Primitive.Text]
        .imapK([A] =>
          (annotation: Annotation[Self.Enumeration[Parameter.Primitive.Text, A]]) => Enumeration(annotation)
        )([A] => (parameter: Parameter.Enumeration[A]) => parameter.self)

  sealed abstract class Primitive[A] extends Parameter[A], Parameter.Primitive.Read[A], Parameter.Primitive.Write[A]

  object Primitive:
    sealed trait Read[+A] extends Parameter.Read[A]

    sealed trait Write[-A] extends Parameter.Write[A]

    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
        extends Parameter.Primitive.Boolean.Read[A],
          Parameter.Primitive.Boolean.Write[A]

    object Boolean:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Boolean.Read[A]]

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Boolean.Write[A]]

    final case class Coerce[A](self: Annotation[Self.Primitive.Coerce.Text[Parameter.Primitive.Text, A]])
        extends Parameter.Primitive[A],
          Parameter.Primitive.Read[A],
          Parameter.Primitive.Write[A]

    object Coerce:
      sealed trait Read[+A] extends Parameter.Primitive.Read[A]:
        def self: Annotation[Self.Primitive.Coerce.Text.Read[Parameter.Primitive.Text.Read, A]]

      sealed trait Write[-A] extends Parameter.Primitive.Write[A]:
        def self: Annotation[Self.Primitive.Coerce.Text.Write[Parameter.Primitive.Text.Write, A]]

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
        extends Parameter.Primitive.Number.Read[A],
          Parameter.Primitive.Number.Write[A]

    object Number:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Number.Read[A]]

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Number.Write[A]]

    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends Parameter.Primitive[A],
          Parameter.Primitive.Text.Read[A],
          Parameter.Primitive.Text.Write[A]

    object Text:
      sealed trait Read[+A] extends Parameter.Read[A]:
        def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Parameter.Primitive.Text.Read[A] =
          new Read[A]:
            override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[Parameter.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] =>
            (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self)
          )([A] => (parameter: Parameter.Primitive.Text.Read[A]) => parameter.self)

        given [A] => Annotated[Parameter.Primitive.Text.Read[A]] =
          Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Text.Read[Parameter.Primitive.Text.Read] =
          PrimitiveOperation.Text
            .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
              (parameter: Parameter.Primitive.Text.Read[A]) => parameter.self
            )

      sealed trait Write[-A] extends Parameter.Write[A]:
        def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Parameter.Primitive.Text.Write[A] =
          new Write[A]:
            override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[Parameter.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] =>
            (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self)
          )([A] => (parameter: Parameter.Primitive.Text.Write[A]) => parameter.self)

        given [A] => Annotated[Parameter.Primitive.Text.Write[A]] =
          Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Text.Write[Parameter.Primitive.Text.Write] = PrimitiveOperation.Text
          .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self))([A] =>
            (parameter: Parameter.Primitive.Text.Write[A]) => parameter.self
          )

      given Invariant[Parameter.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
          (parameter: Parameter.Primitive.Text[A]) => parameter.self
        )

      given [A] => Annotated[Parameter.Primitive.Text[A]] =
        Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

      given PrimitiveOperation.Text[Parameter.Primitive.Text] =
        PrimitiveOperation
          .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
            (parameter: Parameter.Primitive.Text[A]) => parameter.self
          )

  final case class Union[A](self: Annotation[Self.Union[Parameter.Branch, A]])
      extends Parameter[A],
        Parameter.Union.Read[A],
        Parameter.Union.Write[A]

  object Union:
    sealed trait Read[+A] extends Parameter.Read[A]:
      def self: Annotation[Self.Union.Read[Parameter.Branch.Read, A]]

    sealed trait Write[-A] extends Parameter.Write[A]:
      def self: Annotation[Self.Union.Write[Parameter.Branch.Write, A]]

  final case class Branch[A](self: Annotation[Self.Branch[Parameter, A]])
      extends Parameter.Branch.Read[A],
        Parameter.Branch.Write[A]

  object Branch:
    sealed trait Read[+A]:
      def self: Annotation[Self.Branch.Read[Parameter.Read, A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Branch.Write[Parameter.Write, A]]
