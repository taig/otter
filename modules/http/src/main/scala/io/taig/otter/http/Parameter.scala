package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Annotation
import cats.Functor
import cats.Contravariant
import cats.Invariant
import Self.Annotated
import Self.operation.PrimitiveOperation

sealed abstract class Parameter[A] extends Parameter.Read[A], Parameter.Write[A]:
  override def self: Annotation[
    Self.Primitive.Coerce[
      [a] =>> Parameter.Primitive.Boolean[a] | Parameter.Primitive.Number[a] | Parameter.Primitive.Text[a],
      A
    ] | Self.Primitive.Text[A]
  ]

object Parameter:
  sealed trait Read[+A]:
    def self: Annotation[
      Self.Primitive.Coerce.Read[
        [a] =>> Parameter.Primitive.Boolean.Read[a] | Parameter.Primitive.Number.Read[a] |
          Parameter.Primitive.Text.Read[a],
        A
      ] | Self.Primitive.Text.Read[A]
    ]

  sealed trait Write[-A]:
    def self: Annotation[
      Self.Primitive.Coerce.Write[
        [a] =>> Parameter.Primitive.Boolean.Write[a] | Parameter.Primitive.Number.Write[a] |
          Parameter.Primitive.Text.Write[a],
        A
      ] | Self.Primitive.Text.Write[A]
    ]

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

        given PrimitiveOperation.Text.Write[Parameter.Primitive.Text.Write] =
          PrimitiveOperation.Text
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
