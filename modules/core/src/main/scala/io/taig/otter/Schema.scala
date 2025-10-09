package io.taig.otter

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.*
import cats.Invariant
import cats.derived.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Product with Serializable

object Schema:
  final case class Collection[+S[a] <: Schema[?, a], A](self: Annotation[Self.Collection[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Constant[+S[a] <: Schema[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Dictionary[+S[a] <: Schema[?, a], A](self: Annotation[Self.Dictionary[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Enumeration[+S[a] <: Schema[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Record[+S[a] <: Schema[?, a], A](self: Annotation[Self.Record[Schema.Field[S, *], A]])
      extends Schema[S, A] derives Invariant

  final case class Tuple[+S[a] <: Schema[?, a], A](self: Annotation[Self.Tuple[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Union[+S[a] <: Schema[?, a], A](self: Annotation[Self.Union[S, A]]) extends Schema[S, A]
      derives Invariant

  final case class Field[+S[a] <: Schema[?, a], A](self: Annotation[Self.Field[S, A]]) derives Invariant

  sealed abstract class Primitive[A] extends Schema[Nothing, A]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]
        derives Invariant

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A] derives Invariant

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A] derives Invariant

    given invariant[A]: Invariant[Schema.Primitive] with
      override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa match
        case schema: Primitive.Boolean[A] => schema.imap(f)(g)
        case schema: Primitive.Number[A]  => schema.imap(f)(g)
        case schema: Primitive.String[A]  => schema.imap(f)(g)

  given invariant[S[a] <: Schema[?, a]]: Invariant[Schema[S, *]] with
    override def imap[A, B](fa: Schema[S, A])(f: A => B)(g: B => A): Schema[S, B] = fa match
      case schema: Schema.Collection[S, A]  => schema.imap(f)(g)
      case schema: Schema.Constant[S, A]    => schema.imap(f)(g)
      case schema: Schema.Dictionary[S, A]  => schema.imap(f)(g)
      case schema: Schema.Enumeration[S, A] => schema.imap(f)(g)
      case schema: Schema.Record[S, A]      => schema.imap(f)(g)
      case schema: Schema.Tuple[S, A]       => schema.imap(f)(g)
      case schema: Schema.Union[S, A]       => schema.imap(f)(g)
      case schema: Schema.Primitive[A]      => schema.imap(f)(g)
