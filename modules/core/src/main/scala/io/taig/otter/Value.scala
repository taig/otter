package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.syntax.all.*

sealed abstract class Value[A] extends Value.Read[A], Value.Write[A]:
  override def self: Annotation[Value.Of[A]]

object Value:
  sealed trait Read[+A]:
    def self: Annotation[Value.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Constant.Read[Value.Primitive.Text.Read, A] | Self.Primitive.Read[?, A] |
      Self.Union.Read[Value.Branch.Read, A]

    given Functor[Value.Read]:
      override def map[A, B](value: Value.Read[A])(f: A => B): Value.Read[B] = value match
        case value: Value.Constant.Read[A]     => value.map(f)
        case value: Value.Primitive.Read[?, A] => value.map(f)
        case value: Value.Union.Read[A]        => value.map(f)

  sealed trait Write[-A]:
    def self: Annotation[Value.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Constant.Write[Value.Primitive.Text.Write, A] | Self.Primitive.Write[?, A] |
      Self.Union.Write[Value.Branch.Write, A]

    given Contravariant[Value.Write]:
      override def contramap[A, B](value: Value.Write[A])(f: B => A): Value.Write[B] = value match
        case value: Value.Constant.Write[A]     => value.contramap(f)
        case value: Value.Primitive.Write[?, A] => value.contramap(f)
        case value: Value.Union.Write[A]        => value.contramap(f)

  sealed abstract class Constant[A] extends Value[A], Value.Constant.Read[A], Value.Constant.Write[A]:
    override def self: Annotation[Self.Constant[Value.Primitive.Text, A]]

  object Constant:
    sealed trait Read[+A] extends Value.Read[A]:
      override def self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]): Value.Constant.Read[A] =
        new Read[A]:
          override def self: Self.Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]] = annotation

      given Functor[Value.Constant.Read] =
        Functor[[a] =>> Annotation[Self.Constant.Read[Value.Primitive.Text.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]) => Read(self)
        )([A] => (value: Value.Constant.Read[A]) => value.self)

      given ConstantOperation.Read[Value.Constant.Read, Value.Primitive.Text.Read] = ConstantOperation
        .Read[[a] =>> Annotation[Self.Constant.Read[Value.Primitive.Text.Read, a]], Value.Primitive.Text.Read]
        .imapK([A] => (self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]) => Read(self))([A] =>
          (value: Value.Constant.Read[A]) => value.self
        )

    sealed trait Write[-A] extends Value.Write[A]:
      override def self: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]

    object Write:
      def apply[A](
          annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]
      ): Value.Constant.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]] = annotation

      given Contravariant[Value.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (value: Value.Constant.Write[A]) => value.self)

      given ConstantOperation.Write[Value.Constant.Write, Value.Primitive.Text.Write] =
        ConstantOperation
          .Write[[a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]], Value.Primitive.Text.Write]
          .imapK([A] =>
            (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
          )([A] => (value: Value.Constant.Write[A]) => value.self)

    def apply[A](annotation: Annotation[Self.Constant[Value.Primitive.Text, A]]): Value.Constant[A] =
      new Constant[A]:
        override def self: Self.Annotation[Self.Constant[Value.Primitive.Text, A]] = annotation

    given Invariant[Value.Constant] =
      Invariant[[a] =>> Annotation[Self.Constant[Value.Primitive.Text, a]]].imapK([A] =>
        (annotation: Annotation[Self.Constant[Value.Primitive.Text, A]]) => Constant(annotation)
      )([A] => (value: Value.Constant[A]) => value.self)

    given ConstantOperation[Value.Constant, Value.Primitive.Text] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[Value.Primitive.Text, a]], Value.Primitive.Text].imapK([A] =>
        (annotation: Annotation[Self.Constant[Value.Primitive.Text, A]]) => Constant(annotation)
      )([A] => (value: Value.Constant[A]) => value.self)

  sealed abstract class Primitive[+F[_], A] extends Value[A], Value.Primitive.Read[F, A], Value.Primitive.Write[F, A]:
    override def self: Annotation[Self.Primitive[F, A]]

  object Primitive:
    sealed trait Read[+F[_], +A] extends Value.Read[A]:
      override def self: Annotation[Self.Primitive.Read[F, A]]

    object Read:
      def apply[F[_], A](annotation: Annotation[Self.Primitive.Read[F, A]]): Value.Primitive.Read[F, A] =
        new Read[F, A]:
          override def self: Self.Annotation[Self.Primitive.Read[F, A]] = annotation

      given [F[_]] => Functor[[a] =>> Value.Primitive.Read[F, a]] =
        Functor[[a] =>> Annotation[Self.Primitive.Read[F, a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Read[F, A]]) => Read(self)
        )([A] => (value: Value.Primitive.Read[F, A]) => value.self)

    sealed trait Write[+F[_], -A] extends Value.Write[A]:
      override def self: Annotation[Self.Primitive.Write[F, A]]

    object Write:
      def apply[F[_], A](annotation: Annotation[Self.Primitive.Write[F, A]]): Value.Primitive.Write[F, A] =
        new Write[F, A]:
          override def self: Self.Annotation[Self.Primitive.Write[F, A]] = annotation

      given [F[_]] => Contravariant[[a] =>> Value.Primitive.Write[F, a]] =
        Contravariant[[a] =>> Annotation[Self.Primitive.Write[F, a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Write[F, A]]) => Write(annotation)
        )([A] => (value: Value.Primitive.Write[F, A]) => value.self)

    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
        extends Value.Primitive.Boolean.Read[A],
          Value.Primitive.Boolean.Write[A]

    object Boolean:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Boolean.Read[A]]): Value.Primitive.Boolean.Read[A] =
          new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Read[A]] = annotation

        given Functor[Value.Primitive.Boolean.Read] = Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Boolean.Read[A]) => value.self
          )

        given [A] => Annotated[Value.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Boolean.Read[Value.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
          .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Boolean.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Boolean.Read[A]) => value.self
          )

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Boolean.Write[A]]): Value.Primitive.Boolean.Write[A] =
          new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Write[A]] = annotation

        given Contravariant[Value.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]].imapK([A] =>
            (annotation: Annotation[Self.Primitive.Boolean.Write[A]]) => Write(annotation)
          )([A] => (value: Value.Primitive.Boolean.Write[A]) => value.self)

        given [A] => Annotated[Value.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Boolean.Write[Value.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
          .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
          .imapK([A] => (annotation: Annotation[Self.Primitive.Boolean.Write[A]]) => Write(annotation))([A] =>
            (value: Value.Primitive.Boolean.Write[A]) => value.self
          )

      given Invariant[Value.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Boolean[A]]) => Boolean(annotation)
        )([A] => (value: Value.Primitive.Boolean[A]) => value.self)

      given [A] => Annotated[Value.Primitive.Boolean[A]] =
        Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

      given PrimitiveOperation.Boolean[Value.Primitive.Boolean] = PrimitiveOperation
        .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
        .imapK([A] => (annotation: Annotation[Self.Primitive.Boolean[A]]) => Boolean(annotation))([A] =>
          (value: Value.Primitive.Boolean[A]) => value.self
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
        extends Value.Primitive.Number.Read[A],
          Value.Primitive.Number.Write[A]

    object Number:
      sealed trait Read[+A]:
        def self: Annotation[Self.Primitive.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Number.Read[A]]): Value.Primitive.Number.Read[A] =
          new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Read[A]] = annotation

        given Functor[Value.Primitive.Number.Read] = Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Number.Read[A]) => value.self
          )

        given [A] => Annotated[Value.Primitive.Number.Read[A]] =
          Annotated[Annotation[Self.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Number.Read[Value.Primitive.Number.Read] = PrimitiveOperation.Number
          .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Number.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Number.Read[A]) => value.self
          )

      sealed trait Write[-A]:
        def self: Annotation[Self.Primitive.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Number.Write[A]]): Value.Primitive.Number.Write[A] =
          new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Write[A]] = annotation

        given Contravariant[Value.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]].imapK([A] =>
            (annotation: Annotation[Self.Primitive.Number.Write[A]]) => Write(annotation)
          )([A] => (value: Value.Primitive.Number.Write[A]) => value.self)

        given [A] => Annotated[Value.Primitive.Number.Write[A]] =
          Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Number.Write[Value.Primitive.Number.Write] = PrimitiveOperation.Number
          .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
          .imapK([A] => (annotation: Annotation[Self.Primitive.Number.Write[A]]) => Write(annotation))([A] =>
            (value: Value.Primitive.Number.Write[A]) => value.self
          )

      given Invariant[Value.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Number[A]]) => Number(annotation)
        )([A] => (value: Value.Primitive.Number[A]) => value.self)

      given [A] => Annotated[Value.Primitive.Number[A]] =
        Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

      given PrimitiveOperation.Number[Value.Primitive.Number] = PrimitiveOperation
        .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
        .imapK([A] => (annotation: Annotation[Self.Primitive.Number[A]]) => Number(annotation))([A] =>
          (value: Value.Primitive.Number[A]) => value.self
        )

    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends Value.Primitive[Nothing, A],
          Value.Primitive.Text.Read[A],
          Value.Primitive.Text.Write[A]

    object Text:
      sealed trait Read[+A] extends Value.Primitive.Read[Nothing, A]:
        def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Value.Primitive.Text.Read[A] =
          new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[Value.Primitive.Text.Read] = Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Text.Read[A]) => value.self
          )

        given [A] => Annotated[Value.Primitive.Text.Read[A]] =
          Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

        given PrimitiveOperation.Text.Read[Value.Primitive.Text.Read] = PrimitiveOperation.Text
          .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Text.Read[A]) => value.self
          )

      sealed trait Write[-A] extends Value.Primitive.Write[Nothing, A]:
        def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Value.Primitive.Text.Write[A] =
          new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[Value.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Self.Primitive.Text.Write[A]]) => Write(annotation)
          )([A] => (value: Value.Primitive.Text.Write[A]) => value.self)

        given [A] => Annotated[Value.Primitive.Text.Write[A]] =
          Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

        given PrimitiveOperation.Text.Write[Value.Primitive.Text.Write] = PrimitiveOperation.Text
          .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
          .imapK([A] => (annotation: Annotation[Self.Primitive.Text.Write[A]]) => Write(annotation))([A] =>
            (value: Value.Primitive.Text.Write[A]) => value.self
          )

      given Invariant[Value.Primitive.Text] =
        Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Text[A]]) => Text(annotation)
        )([A] => (value: Value.Primitive.Text[A]) => value.self)

      given [A] => Annotated[Value.Primitive.Text[A]] =
        Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

      given PrimitiveOperation.Text[Value.Primitive.Text] = PrimitiveOperation
        .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
        .imapK([A] => (annotation: Annotation[Self.Primitive.Text[A]]) => Text(annotation))([A] =>
          (value: Value.Primitive.Text[A]) => value.self
        )

    given [F[_]] => Invariant[[a] =>> Value.Primitive[F, a]]:
      override def imap[A, B](value: Value.Primitive[F, A])(f: A => B)(g: B => A): Value.Primitive[F, B] =
        value match
          case value: Value.Primitive.Text[A] => value.imap(f)(g)

  sealed abstract class Union[A] extends Value[A], Value.Union.Read[A], Value.Union.Write[A]:
    override def self: Annotation[Self.Union[Value.Branch, A]]

  object Union:
    sealed trait Read[+A] extends Value.Read[A]:
      override def self: Annotation[Self.Union.Read[Value.Branch.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Union.Read[Value.Branch.Read, A]]): Value.Union.Read[A] =
        new Read[A]:
          override def self: Self.Annotation[Self.Union.Read[Value.Branch.Read, A]] = annotation

      given Functor[Value.Union.Read] =
        Functor[[a] =>> Annotation[Self.Union.Read[Value.Branch.Read, a]]].imapK([A] =>
          (self: Annotation[Self.Union.Read[Value.Branch.Read, A]]) => Read(self)
        )([A] => (value: Value.Union.Read[A]) => value.self)

    sealed trait Write[-A] extends Value.Write[A]:
      override def self: Annotation[Self.Union.Write[Value.Branch.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Union.Write[Value.Branch.Write, A]]): Value.Union.Write[A] =
        new Write[A]:
          override def self: Self.Annotation[Self.Union.Write[Value.Branch.Write, A]] = annotation

      given Contravariant[Value.Union.Write] =
        Contravariant[[a] =>> Annotation[Self.Union.Write[Value.Branch.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Union.Write[Value.Branch.Write, A]]) => Write(annotation)
        )([A] => (value: Value.Union.Write[A]) => value.self)

    def apply[A](annotation: Annotation[Self.Union[Value.Branch, A]]): Value.Union[A] =
      new Union[A]:
        override def self: Self.Annotation[Self.Union[Value.Branch, A]] = annotation

    given Invariant[Value.Union] =
      Invariant[[a] =>> Annotation[Self.Union[Value.Branch, a]]].imapK([A] =>
        (annotation: Annotation[Self.Union[Value.Branch, A]]) => Union(annotation)
      )([A] => (value: Value.Union[A]) => value.self)

  final case class Branch[A](self: Annotation[Self.Branch[Value, A]])
      extends Value.Branch.Read[A],
        Value.Branch.Write[A]

  object Branch:
    sealed trait Read[+A]:
      def self: Annotation[Self.Branch.Read[Value.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Branch.Read[Value.Read, A]]): Value.Branch.Read[A] = new Read[A]:
        override def self: Self.Annotation[Self.Branch.Read[Value.Read, A]] = annotation

      given Functor[Value.Branch.Read] = Functor[[a] =>> Annotation[Self.Branch.Read[Value.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Branch.Read[Value.Read, A]]) => Read(self))([A] =>
          (value: Value.Branch.Read[A]) => value.self
        )

    sealed trait Write[-A]:
      def self: Annotation[Self.Branch.Write[Value.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Branch.Write[Value.Write, A]]): Value.Branch.Write[A] = new Write[A]:
        override def self: Self.Annotation[Self.Branch.Write[Value.Write, A]] = annotation

      given Contravariant[Value.Branch.Write] =
        Contravariant[[a] =>> Annotation[Self.Branch.Write[Value.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Branch.Write[Value.Write, A]]) => Write(annotation)
        )([A] => (value: Value.Branch.Write[A]) => value.self)

    given Invariant[Value.Branch] = Invariant[[a] =>> Annotation[Self.Branch[Value, a]]]
      .imapK([A] => (annotation: Annotation[Self.Branch[Value, A]]) => Branch(annotation))([A] =>
        (value: Value.Branch[A]) => value.self
      )

  type Of[A] = Self.Constant[Value.Primitive.Text, A] | Self.Primitive[?, A] | Self.Union[Value.Branch, A]

  given Invariant[Value]:
    override def imap[A, B](value: Value[A])(f: A => B)(g: B => A): Value[B] = value match
      case value: Value.Constant[A] => value.imap(f)(g)
      case value: Value.Union[A]    => value.imap(f)(g)
