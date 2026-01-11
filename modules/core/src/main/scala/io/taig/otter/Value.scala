package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.syntax.all.*

sealed abstract class Value[A] extends Value.Read[A], Value.Write[A]:
  override def self: Annotation[Value.Of[A]]

object Value:
  sealed trait Read[+A]:
    def self: Annotation[Value.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Primitive.Read[A]

    given Functor[Value.Read]:
      override def map[A, B](value: Value.Read[A])(f: A => B): Value.Read[B] = value match
        case value: Value.Primitive.Text.Read[A] => value.map(f)

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

        def unapply[A](value: Value.Primitive.Text.Read[A]): Annotation[Self.Primitive.Text.Read[A]] =
          value.self

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

        def unapply[A](value: Value.Primitive.Text.Write[A]): Annotation[Self.Primitive.Text.Write[A]] =
          value.self

        given Contravariant[Value.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Self.Primitive.Text.Write[A]]) => Write(annotation)
          )([A] => (value: Value.Primitive.Text.Write[A]) => value.self)

      given Invariant[Value.Primitive.Text] =
        Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]].imapK([A] =>
          (annotation: Annotation[Self.Primitive.Text[A]]) => Text(annotation)
        )([A] => (value: Value.Primitive.Text[A]) => value.self)

  sealed trait Write[-A]:
    def self: Annotation[Value.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Primitive.Write[A]

    given Contravariant[Value.Write]:
      override def contramap[A, B](value: Value.Write[A])(f: B => A): Value.Write[B] = value match
        case value: Value.Primitive.Text.Write[A] => value.contramap(f)

  type Of[A] = Self.Primitive[A]

  given Invariant[Value]:
    override def imap[A, B](value: Value[A])(f: A => B)(g: B => A): Value[B] = value match
      case value: Value.Primitive.Text[A] => value.imap(f)(g)
