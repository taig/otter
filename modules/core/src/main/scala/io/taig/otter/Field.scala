package io.taig.otter

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.FieldOperation

type Field[+S[_], A] = Field.Read[S, A] & Field.Write[S, A]

object Field:
  sealed trait Read[+S[_], +A]:
    def isOptional: Boolean

    def name: String

    def optional: Field.Read[S, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): Field.Read[S, A1] = Read.Default(self = this, value = default)

    def schema: Reference[S, ?]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Read[G, A]

    final def map[B](f: A => B): Field.Read[S, B] = Read.Modify(self = this, f)

  object Read:
    final case class Default[S[_], A](self: Field.Read[S, A], value: Eval[A]) extends Field.Read[S, A]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Read[G, A] = copy(self = self.mapK(fK))

    final case class Modify[S[_], A, B](self: Field.Read[S, A], f: A => B) extends Field.Read[S, B]:
      export self.{isOptional, name, schema}

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Read[G, B] = copy(self = self.mapK(fK))

    final case class Optional[S[_], A](self: Field.Read[S, A]) extends Field.Read[S, Option[A]]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Read[G, Option[A]] = copy(self = self.mapK(fK))

    given [S[_]] => Functor[Field.Read[S, *]]:
      override def map[A, B](fa: Field.Read[S, A])(f: A => B): Field.Read[S, B] = fa.map(f)

    given [S[_]] => FieldOperation.Read[Field.Read[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Field.Read[S, A] = Root(name, schema)

      extension [A](fa: Field.Read[S, A])
        override def isOptional: Boolean = fa.isOptional

        override def name: String = fa.name

        override def optional: Field.Read[S, Option[A]] = fa.optional

        override def optional(default: => A): Field.Read[S, A] = fa.optional(default = Eval.later(default))

        override def schema: Reference[S, ?] = fa.schema

  sealed trait Write[+S[_], -A]:
    def isOptional: Boolean

    def name: String

    def optional: Field.Write[S, Option[A]] = Write.Optional(self = this)

    def schema: Reference[S, ?]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Write[G, A]

    final def contramap[B](f: B => A): Field.Write[S, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Field.Write[S, A], f: B => A) extends Field.Write[S, B]:
      export self.{isOptional, name, schema}

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Write[G, B] = copy(self = self.mapK(fK))

    final case class Optional[S[_], A](self: Field.Write[S, A]) extends Field.Write[S, Option[A]]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field.Write[G, Option[A]] = copy(self = self.mapK(fK))

    given [S[_]] => Contravariant[Field.Write[S, *]]:
      override def contramap[A, B](fa: Field.Write[S, A])(f: B => A): Field.Write[S, B] = fa.contramap(f)

    given [S[_]] => FieldOperation.Write[Field.Write[S, *], S]:
      override def lift[A](name: String, schema: Reference[S, A]): Field.Write[S, A] = Root(name, schema)

      extension [A](fa: Field.Write[S, A])
        override def isOptional: Boolean = fa.isOptional

        override def name: String = fa.name

        override def optional: Field.Write[S, Option[A]] = fa.optional

        override def schema: Reference[S, ?] = fa.schema

  final case class Default[S[_], A](self: Field[S, A], value: Eval[A]) extends Field.Read[S, A], Field.Write[S, A]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def optional: Field[S, Option[A]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field[G, A] = copy(self = self.mapK(fK))

  final case class Modify[S[_], A, B](self: Field[S, A], f: A => B, g: B => A)
      extends Field.Read[S, B],
        Field.Write[S, B]:
    export self.{isOptional, name, schema}

    override def optional: Field[S, Option[B]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field[G, B] = copy(self = self.mapK(fK))

  final case class Optional[S[_], A](self: Field[S, A]) extends Field.Read[S, Option[A]], Field.Write[S, Option[A]]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def optional: Field[S, Option[Option[A]]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field[G, Option[A]] = copy(self = self.mapK(fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Field.Read[S, A], Field.Write[S, A]:
    override def isOptional: Boolean = false

    override def optional: Field[S, Option[A]] = Optional(self = this)

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Field[G, A] = copy(schema = schema.mapK[S, G](fK))

  given [S[_]] => Invariant[Field[S, *]]:
    override def imap[A, B](fa: Field[S, A])(f: A => B)(g: B => A): Field[S, B] = Modify(fa, f, g)

  given [S[_]] => FieldOperation[Field[S, *], S]:
    override def lift[A](name: String, schema: Reference[S, A]): Field[S, A] = Root(name, schema)

    extension [A](fa: Field[S, A])
      override def isOptional: Boolean = fa.isOptional

      override def name: String = fa.name

      override def optional: Field[S, Option[A]] = Optional(fa)

      override def optional(default: => A): Field[S, A] = Default(fa, value = Eval.later(default))

      override def schema: Reference[S, ?] = fa.schema
