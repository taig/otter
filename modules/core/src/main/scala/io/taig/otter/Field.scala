package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.FieldOperation

sealed abstract class Field[+S[_], A] extends Field.Read[S, A], Field.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Field[S, B] = Field.Modify(self = this, f, g)

object Field:
  sealed trait Read[+S[_], +A]:
    def name: String

    def schema: Reference[S, ?]

    final def map[B](f: A => B): Field.Read[S, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: Field.Read[S, A], f: A => B) extends Field.Read[S, B]:
      export self.{name, schema}

    given [S[_]] => Functor[Field.Read[S, *]]:
      override def map[A, B](fa: Field.Read[S, A])(f: A => B): Field.Read[S, B] = fa.map(f)

    given [S[_]] => FieldOperation.Read[Field.Read[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Field.Read[S, A] = Root(name, schema)

      extension [A](fa: Field.Read[S, A]) override def schema: Reference[S, ?] = fa.schema

  sealed trait Write[+S[_], -A]:
    def name: String

    def schema: Reference[S, ?]

    final def contramap[B](f: B => A): Field.Write[S, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Field.Write[S, A], f: B => A) extends Field.Write[S, B]:
      export self.{name, schema}

    given [S[_]] => Contravariant[Field.Write[S, *]]:
      override def contramap[A, B](fa: Field.Write[S, A])(f: B => A): Field.Write[S, B] = fa.contramap(f)

    given [S[_]] => FieldOperation.Write[Field.Write[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Field.Write[S, A] = Root(name, schema)

      extension [A](fa: Field.Write[S, A]) override def schema: Reference[S, ?] = fa.schema

  final case class Modify[S[_], A, B](self: Field[S, A], f: A => B, g: B => A) extends Field[S, B]:
    export self.{name, schema}

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Field[S, A]

  given [S[_]] => Invariant[Field[S, *]]:
    override def imap[A, B](fa: Field[S, A])(f: A => B)(g: B => A): Field[S, B] = fa.imap(f)(g)

  given [S[_]] => FieldOperation[Field[S, *], S]:
    override def lift[A](name: String, schema: Reference[S, A]): Field[S, A] = Root(name, schema)

    extension [A](fa: Field[S, A]) override def schema: Reference[S, ?] = fa.schema
