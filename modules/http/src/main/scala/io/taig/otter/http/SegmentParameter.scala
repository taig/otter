package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Functor
import cats.Invariant
import cats.Contravariant

sealed abstract class SegmentParameter[A] extends SegmentParameter.Read[A], SegmentParameter.Write[A]:
  override def self: Annotation[
    Self.Coerce[SegmentParameter.Primitive, A] | Self.Constant[SegmentParameter.Primitive.Text, A] |
      Self.Enumeration[SegmentParameter.Primitive.Text, A] | Self.Primitive.Text[A] |
      Self.Union[SegmentParameter.Branch, A]
  ]

object SegmentParameter:
  sealed trait Read[+A]:
    def self: Annotation[
      Self.Coerce.Read[SegmentParameter.Primitive.Read, A] | Self.Constant.Read[SegmentParameter.Primitive.Text, A] |
        Self.Enumeration.Read[SegmentParameter.Primitive.Text, A] | Self.Primitive.Text.Read[A] |
        Self.Union.Read[SegmentParameter.Branch.Read, A]
    ]

  sealed trait Write[-A]:
    def self: Annotation[
      Self.Coerce.Write[SegmentParameter.Primitive.Write, A] |
        Self.Constant.Write[SegmentParameter.Primitive.Text.Write, A] |
        Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, A] | Self.Primitive.Text.Write[A] |
        Self.Union.Write[SegmentParameter.Branch.Write, A]
    ]

  final case class Coerce[A](self: Annotation[Self.Coerce[SegmentParameter.Primitive, A]])
      extends SegmentParameter[A],
        SegmentParameter.Coerce.Read[A],
        SegmentParameter.Coerce.Write[A]

  object Coerce:
    sealed trait Read[+A] extends SegmentParameter.Read[A]:
      def self: Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, A]]
      ): SegmentParameter.Coerce.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, A]] = annotation

      given Functor[SegmentParameter.Coerce.Read] =
        Functor[[a] =>> Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, a]]]
          .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Coerce.Read[A]] =
        Annotated[Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, A]]].imap(Read.apply)(_.self)

      given CoerceOperation.Read[SegmentParameter.Coerce.Read, SegmentParameter.Primitive.Read] = CoerceOperation
        .Read[
          [a] =>> Annotation[Self.Coerce.Read[SegmentParameter.Primitive.Read, a]],
          SegmentParameter.Primitive.Read
        ]
        .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A] extends SegmentParameter.Write[A]:
      def self: Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, A]]
      ): SegmentParameter.Coerce.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, A]] = annotation

      given Contravariant[SegmentParameter.Coerce.Write] =
        Contravariant[[a] =>> Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Coerce.Write[A]] =
        Annotated[Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, A]]].imap(Write.apply)(_.self)

      given CoerceOperation.Write[SegmentParameter.Coerce.Write, SegmentParameter.Primitive.Write] =
        CoerceOperation
          .Write[
            [a] =>> Annotation[Self.Coerce.Write[SegmentParameter.Primitive.Write, a]],
            SegmentParameter.Primitive.Write
          ]
          .imapK([A] => Write(_))([A] => _.self)

    given Invariant[SegmentParameter.Coerce] =
      Invariant[[a] =>> Annotation[Self.Coerce[SegmentParameter.Primitive, a]]]
        .imapK([A] => Coerce(_))([A] => _.self)

    given [A] => Annotated[SegmentParameter.Coerce[A]] =
      Annotated[Annotation[Self.Coerce[SegmentParameter.Primitive, A]]].imap(Coerce.apply)(_.self)

    given CoerceOperation[SegmentParameter.Coerce, SegmentParameter.Primitive] = CoerceOperation[
      [a] =>> Annotation[Self.Coerce[SegmentParameter.Primitive, a]],
      SegmentParameter.Primitive
    ].imapK([A] => Coerce(_))([A] => _.self)

  final case class Constant[A](self: Annotation[Self.Constant[SegmentParameter.Primitive.Text, A]])
      extends SegmentParameter[A],
        SegmentParameter.Constant.Read[A],
        SegmentParameter.Constant.Write[A]

  object Constant:
    sealed trait Read[+A] extends SegmentParameter.Read[A]:
      def self: Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, A]]
      ): SegmentParameter.Constant.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, A]] = annotation

      given Functor[SegmentParameter.Constant.Read] =
        Functor[[a] =>> Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, a]]]
          .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Constant.Read[A]] =
        Annotated[Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, A]]].imap(Read.apply)(_.self)

      given ConstantOperation.Read[SegmentParameter.Constant.Read, SegmentParameter.Primitive.Text] =
        ConstantOperation
          .Read[
            [a] =>> Annotation[Self.Constant.Read[SegmentParameter.Primitive.Text, a]],
            SegmentParameter.Primitive.Text
          ]
          .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A] extends SegmentParameter.Write[A]:
      def self: Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, A]]
      ): SegmentParameter.Constant.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, A]] =
          annotation

      given Contravariant[SegmentParameter.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, a]]].imapK([A] =>
          Write(_)
        )([A] => _.self)

      given [A] => Annotated[SegmentParameter.Constant.Write[A]] =
        Annotated[Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, A]]]
          .imap(Write.apply)(_.self)

      given ConstantOperation.Write[SegmentParameter.Constant.Write, SegmentParameter.Primitive.Text.Write] =
        ConstantOperation
          .Write[
            [a] =>> Annotation[Self.Constant.Write[SegmentParameter.Primitive.Text.Write, a]],
            SegmentParameter.Primitive.Text.Write
          ]
          .imapK([A] => Write(_))([A] => _.self)

    given Invariant[SegmentParameter.Constant] =
      Invariant[[a] =>> Annotation[Self.Constant[SegmentParameter.Primitive.Text, a]]]
        .imapK([A] => Constant(_))([A] => _.self)

    given [A] => Annotated[SegmentParameter.Constant[A]] =
      Annotated[Annotation[Self.Constant[SegmentParameter.Primitive.Text, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[SegmentParameter.Constant, SegmentParameter.Primitive.Text] = ConstantOperation[
      [a] =>> Annotation[Self.Constant[SegmentParameter.Primitive.Text, a]],
      SegmentParameter.Primitive.Text
    ].imapK([A] => Constant(_))([A] => _.self)

  final case class Enumeration[A](self: Annotation[Self.Enumeration[SegmentParameter.Primitive.Text, A]])
      extends SegmentParameter[A],
        SegmentParameter.Enumeration.Read[A],
        SegmentParameter.Enumeration.Write[A]

  object Enumeration:
    sealed trait Read[+A] extends SegmentParameter.Read[A]:
      override def self: Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, A]]
      ): SegmentParameter.Enumeration.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, A]] =
          annotation

      given Functor[SegmentParameter.Enumeration.Read] =
        Functor[[a] =>> Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, a]]]
          .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Enumeration.Read[A]] =
        Annotated[Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, A]]]
          .imap(Read.apply)(_.self)

      given EnumerationOperation.Read[SegmentParameter.Enumeration.Read, SegmentParameter.Primitive.Text] =
        EnumerationOperation
          .Read[
            [a] =>> Annotation[Self.Enumeration.Read[SegmentParameter.Primitive.Text, a]],
            SegmentParameter.Primitive.Text
          ]
          .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A] extends SegmentParameter.Write[A]:
      override def self: Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, A]]
      ): SegmentParameter.Enumeration.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, A]] =
          annotation

      given Contravariant[SegmentParameter.Enumeration.Write] =
        Contravariant[[a] =>> Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Enumeration.Write[A]] =
        Annotated[Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, A]]]
          .imap(Write.apply)(_.self)

      given EnumerationOperation.Write[SegmentParameter.Enumeration.Write, SegmentParameter.Primitive.Text.Write] =
        EnumerationOperation
          .Write[
            [a] =>> Annotation[Self.Enumeration.Write[SegmentParameter.Primitive.Text.Write, a]],
            SegmentParameter.Primitive.Text.Write
          ]
          .imapK([A] => Write(_))([A] => _.self)

    given Invariant[SegmentParameter.Enumeration] =
      Invariant[[a] =>> Annotation[Self.Enumeration[SegmentParameter.Primitive.Text, a]]]
        .imapK([A] => Enumeration(_))([A] => _.self)

    given [A] => Annotated[SegmentParameter.Enumeration[A]] =
      Annotated[Annotation[Self.Enumeration[SegmentParameter.Primitive.Text, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[SegmentParameter.Enumeration, SegmentParameter.Primitive.Text] =
      EnumerationOperation[
        [a] =>> Annotation[Self.Enumeration[SegmentParameter.Primitive.Text, a]],
        SegmentParameter.Primitive.Text
      ].imapK([A] => Enumeration(_))([A] => _.self)

  sealed trait Primitive[A] extends SegmentParameter.Primitive.Read[A], SegmentParameter.Primitive.Write[A]:
    override def self: Annotation[Self.Primitive[A]]

  object Primitive:
    sealed trait Read[+A]:
      def self: Annotation[Self.Primitive.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Primitive.Write[A]]

    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
        extends SegmentParameter.Primitive[A],
          SegmentParameter.Primitive.Boolean.Read[A],
          SegmentParameter.Primitive.Boolean.Write[A]

    object Boolean:
      sealed trait Read[+A] extends SegmentParameter.Primitive.Read[A]:
        def self: Annotation[Self.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Primitive.Boolean.Read[A]]
        ): SegmentParameter.Primitive.Boolean.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Primitive.Boolean.Read[A]] = annotation

        given Functor[SegmentParameter.Primitive.Boolean.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Boolean.Read[SegmentParameter.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
          .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends SegmentParameter.Primitive.Write[A]:
        def self: Annotation[Self.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Boolean.Write[A]]
        ): SegmentParameter.Primitive.Boolean.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Primitive.Boolean.Write[A]] = annotation

        given Contravariant[SegmentParameter.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Boolean.Write[SegmentParameter.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
          .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[SegmentParameter.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK([A] => Primitive.Boolean(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Primitive.Boolean[A]] =
        Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

      given PrimitiveOperation.Boolean[SegmentParameter.Primitive.Boolean] = PrimitiveOperation
        .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => Primitive.Boolean(_))([A] => _.self)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
        extends SegmentParameter.Primitive[A],
          SegmentParameter.Primitive.Number.Read[A],
          SegmentParameter.Primitive.Number.Write[A]

    object Number:
      sealed trait Read[+A] extends SegmentParameter.Primitive.Read[A]:
        def self: Annotation[Self.Primitive.Number.Read[A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Primitive.Number.Read[A]]
        ): SegmentParameter.Primitive.Number.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Primitive.Number.Read[A]] = annotation

        given Functor[SegmentParameter.Primitive.Number.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Number.Read[A]] =
          Annotated[Annotation[Self.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Number.Read[SegmentParameter.Primitive.Number.Read] = PrimitiveOperation.Number
          .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends SegmentParameter.Primitive.Write[A]:
        def self: Annotation[Self.Primitive.Number.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Number.Write[A]]
        ): SegmentParameter.Primitive.Number.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Primitive.Number.Write[A]] = annotation

        given Contravariant[SegmentParameter.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Number.Write[A]] =
          Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Number.Write[SegmentParameter.Primitive.Number.Write] = PrimitiveOperation.Number
          .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[SegmentParameter.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] => Primitive.Number(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Primitive.Number[A]] =
        Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

      given PrimitiveOperation.Number[SegmentParameter.Primitive.Number] = PrimitiveOperation
        .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => Primitive.Number(_))([A] => _.self)

    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends SegmentParameter[A],
          SegmentParameter.Primitive[A],
          SegmentParameter.Primitive.Text.Read[A],
          SegmentParameter.Primitive.Text.Write[A]

    object Text:
      sealed trait Read[+A] extends SegmentParameter.Read[A], SegmentParameter.Primitive.Read[A]:
        def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): SegmentParameter.Primitive.Text.Read[A] =
          new Read[A]:
            override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[SegmentParameter.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] => Read(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Text.Read[A]] =
          Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Text.Read[SegmentParameter.Primitive.Text.Read] = PrimitiveOperation.Text
          .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends SegmentParameter.Write[A], SegmentParameter.Primitive.Write[A]:
        def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Primitive.Text.Write[A]]
        ): SegmentParameter.Primitive.Text.Write[A] =
          new Write[A]:
            override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[SegmentParameter.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[SegmentParameter.Primitive.Text.Write[A]] =
          Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Text.Write[SegmentParameter.Primitive.Text.Write] = PrimitiveOperation.Text
          .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given Invariant[SegmentParameter.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => Text(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Primitive.Text[A]] =
        Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

      given PrimitiveOperation.Text[SegmentParameter.Primitive.Text] = PrimitiveOperation
        .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => Text(_))([A] => _.self)

  final case class Union[A](self: Annotation[Self.Union[SegmentParameter.Branch, A]])
      extends SegmentParameter[A],
        SegmentParameter.Union.Read[A],
        SegmentParameter.Union.Write[A]

  object Union:
    sealed trait Read[+A] extends SegmentParameter.Read[A]:
      def self: Annotation[Self.Union.Read[SegmentParameter.Branch.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Union.Read[SegmentParameter.Branch.Read, A]]
      ): SegmentParameter.Union.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Union.Read[SegmentParameter.Branch.Read, A]] = annotation

      given Functor[SegmentParameter.Union.Read] =
        Functor[[a] =>> Annotation[Self.Union.Read[SegmentParameter.Branch.Read, a]]]
          .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Union.Read[A]] =
        Annotated[Annotation[Self.Union.Read[SegmentParameter.Branch.Read, A]]].imap(Read.apply)(_.self)

      given UnionOperation.Read[SegmentParameter.Union.Read, SegmentParameter.Branch.Read] = UnionOperation
        .Read[
          [a] =>> Annotation[Self.Union.Read[SegmentParameter.Branch.Read, a]],
          SegmentParameter.Branch.Read
        ]
        .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A] extends SegmentParameter.Write[A]:
      def self: Annotation[Self.Union.Write[SegmentParameter.Branch.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Union.Write[SegmentParameter.Branch.Write, A]]
      ): SegmentParameter.Union.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Union.Write[SegmentParameter.Branch.Write, A]] = annotation

      given Contravariant[SegmentParameter.Union.Write] =
        Contravariant[[a] =>> Annotation[Self.Union.Write[SegmentParameter.Branch.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Union.Write[A]] =
        Annotated[Annotation[Self.Union.Write[SegmentParameter.Branch.Write, A]]].imap(Write.apply)(_.self)

      given UnionOperation.Write[SegmentParameter.Union.Write, SegmentParameter.Branch.Write] = UnionOperation
        .Write[
          [a] =>> Annotation[Self.Union.Write[SegmentParameter.Branch.Write, a]],
          SegmentParameter.Branch.Write
        ]
        .imapK([A] => Write(_))([A] => _.self)

  given Invariant[SegmentParameter.Union] = Invariant[[a] =>> Annotation[Self.Union[SegmentParameter.Branch, a]]]
    .imapK([A] => Union(_))([A] => _.self)

  given [A] => Annotated[SegmentParameter.Union[A]] =
    Annotated[Annotation[Self.Union[SegmentParameter.Branch, A]]].imap(Union.apply)(_.self)

  given UnionOperation[SegmentParameter.Union, SegmentParameter.Branch] = UnionOperation[
    [a] =>> Annotation[Self.Union[SegmentParameter.Branch, a]],
    SegmentParameter.Branch
  ].imapK([A] => Union(_))([A] => _.self)

  final case class Branch[A](self: Annotation[Self.Branch[SegmentParameter, A]])
      extends SegmentParameter.Branch.Read[A],
        SegmentParameter.Branch.Write[A]

  object Branch:
    sealed trait Read[+A]:
      def self: Annotation[Self.Branch.Read[SegmentParameter.Read, A]]

    object Read:
      def apply[A](
          annotation: Annotation[Self.Branch.Read[SegmentParameter.Read, A]]
      ): SegmentParameter.Branch.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Branch.Read[SegmentParameter.Read, A]] = annotation

      given Functor[SegmentParameter.Branch.Read] =
        Functor[[a] =>> Annotation[Self.Branch.Read[SegmentParameter.Read, a]]]
          .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Branch.Read[A]] =
        Annotated[Annotation[Self.Branch.Read[SegmentParameter.Read, A]]].imap(Read.apply)(_.self)

    sealed trait Write[-A]:
      def self: Annotation[Self.Branch.Write[SegmentParameter.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Branch.Write[SegmentParameter.Write, A]]
      ): SegmentParameter.Branch.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Branch.Write[SegmentParameter.Write, A]] = annotation

      given Contravariant[SegmentParameter.Branch.Write] =
        Contravariant[[a] =>> Annotation[Self.Branch.Write[SegmentParameter.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[SegmentParameter.Branch.Write[A]] =
        Annotated[Annotation[Self.Branch.Write[SegmentParameter.Write, A]]].imap(Write.apply)(_.self)

    given Invariant[SegmentParameter.Branch] =
      Invariant[[a] =>> Annotation[Self.Branch[SegmentParameter, a]]].imapK([A] => Branch(_))([A] => _.self)

    given [A] => Annotated[SegmentParameter.Branch[A]] =
      Annotated[Annotation[Self.Branch[SegmentParameter, A]]].imap(Branch.apply)(_.self)
