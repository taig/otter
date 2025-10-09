package io.taig.otter

import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import cats.syntax.all.*

sealed abstract class Text[+S[_], A] extends Product with Serializable

object Text:
  sealed trait Primitive[A] extends Product with Serializable

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A] derives Invariant

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A] derives Invariant

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[Nothing, A], Text.Primitive[A]
        derives Invariant

    given invariant: Invariant[Text.Primitive] with
      override def imap[A, B](fa: Text.Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa match
        case schema: Text.Primitive.Boolean[?] => schema.imap(f)(g)
        case schema: Text.Primitive.Number[?]  => schema.imap(f)(g)
        case schema: Text.Primitive.String[?]  => schema.imap(f)(g)

  final case class Coerce[+S[a] <: Text.Primitive[a], A](self: Annotation[Self.Coerce[S, A]]) extends Text[S, A]
      derives Invariant

  final case class Constant[+S[a] <: Text[?, a], A](self: Self.Constant[S, A]) extends Text[S, A] derives Invariant

  final case class Enumeration[A](self: Annotation[Self.Enumeration[Text.Primitive.String, A]]) extends Text[Nothing, A]
      derives Invariant

  final case class Union[+S[a] <: Text[?, a], A](self: Annotation[Self.Union[S, A]]) extends Text[S, A]
      derives Invariant

  given invariant[S[a] <: Text[?, a]]: Invariant[Text[S, *]] with
    override def imap[A, B](fa: Text[S, A])(f: A => B)(g: B => A): Text[S, B] = fa match
      case schema: Text.Primitive[?]   => schema.imap(f)(g)
      case schema: Text.Coerce[?, ?]   => schema.imap(f)(g)
      case schema: Text.Constant[?, ?] => schema.imap(f)(g)
      case schema: Text.Enumeration[?] => schema.imap(f)(g)
      case schema: Text.Union[?, ?]    => schema.imap(f)(g)
