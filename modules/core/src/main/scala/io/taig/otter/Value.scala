package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.syntax.all.*
import Self.operation.ConstantOperation

sealed abstract class Value[A] extends Value.Read[A], Value.Write[A]:
  override def self: Annotation[Value.Of[A]]

object Value:
  sealed trait Read[+A]:
    def self: Annotation[Value.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Constant.Read[Value.Primitive.Text.Read, A] | Self.Primitive.Read[Value, A]

    given Functor[Value.Read]:
      override def map[A, B](value: Value.Read[A])(f: A => B): Value.Read[B] = value match
        case value: Value.Constant.Read[A]       => value.map(f)
        case value: Value.Primitive.Text.Read[A] => value.map(f)

  sealed trait Write[-A]:
    def self: Annotation[Value.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Constant.Write[Value.Primitive.Text.Write, A] | Self.Primitive.Write[Value.Write, A]

    given Contravariant[Value.Write]:
      override def contramap[A, B](value: Value.Write[A])(f: B => A): Value.Write[B] = value match
        case value: Value.Constant.Write[A]       => value.contramap(f)
        case value: Value.Primitive.Text.Write[A] => value.contramap(f)

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
      def apply[A](annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]): Value.Constant.Write[A] =
        new Write[A]:
          override def self: Self.Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]] = annotation

      given Contravariant[Value.Constant.Write] =
        Contravariant[[a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]]].imapK([A] =>
          (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
        )([A] => (value: Value.Constant.Write[A]) => value.self)

      given ConstantOperation.Write[Value.Constant.Write, Value.Primitive.Text.Write] =
        ConstantOperation
          .Write[[a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]], Value.Primitive.Text.Write]
          .imapK([A] => (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation))([A] =>
            (value: Value.Constant.Write[A]) => value.self
          )

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

  object Primitive:
    final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
        extends Value[A],
          Value.Primitive.Text.Read[A],
          Value.Primitive.Text.Write[A]

    object Text:
      sealed trait Read[+A] extends Value.Read[A]:
        override def self: Annotation[Self.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Value.Primitive.Text.Read[A] =
          new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Text.Read[A]] = annotation

        given Functor[Value.Primitive.Text.Read] = Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
          .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
            (value: Value.Primitive.Text.Read[A]) => value.self
          )

      sealed trait Write[-A] extends Value.Write[A]:
        override def self: Annotation[Self.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Value.Primitive.Text.Write[A] =
          new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Text.Write[A]] = annotation

        given Contravariant[Value.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Self.Primitive.Text.Write[A]]) => Write(annotation)
          )([A] => (value: Value.Primitive.Text.Write[A]) => value.self)

      given Invariant[Value.Primitive.Text] =
        Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Text[A]]) => Text(annotation)
        )([A] => (value: Value.Primitive.Text[A]) => value.self)

  type Of[A] = Self.Constant[Value.Primitive.Text, A] | Self.Primitive[Value, A]

  given Invariant[Value]:
    override def imap[A, B](value: Value[A])(f: A => B)(g: B => A): Value[B] = value match
      case value: Value.Constant[A]       => value.imap(f)(g)
      case value: Value.Primitive.Text[A] => value.imap(f)(g)
