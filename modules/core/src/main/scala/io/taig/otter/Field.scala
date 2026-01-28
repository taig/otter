package io.taig.otter

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.FieldOperation

sealed abstract class Field[+F[_], A] extends Field.Read[F, A], Field.Write[F, A]:
  override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field[G, A]

  final override def optional: Field[F, Option[A]] = Field.Optional(self = this)

object Field:
  sealed trait Read[+F[_], +A]:
    def isOptional: Boolean

    def name: String

    def optional: Field.Read[F, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): Field.Read[F, A1] = Read.Default(self = this, value = default)

    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Read[G, A]

    final def map[B](f: A => B): Field.Read[F, B] = Read.Modify(self = this, f)

  object Read:
    final case class Default[F[_], A](self: Field.Read[F, A], value: Eval[A]) extends Field.Read[F, A]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Read[G, A] = copy(self = self.mapK(fK))

    final case class Modify[F[_], A, B](self: Field.Read[F, A], f: A => B) extends Field.Read[F, B]:
      export self.{isOptional, name, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Read[G, B] = copy(self = self.mapK(fK))

    final case class Optional[F[_], A](self: Field.Read[F, A]) extends Field.Read[F, Option[A]]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Read[G, Option[A]] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Field.Read[F, *]]:
      override def map[A, B](fa: Field.Read[F, A])(f: A => B): Field.Read[F, B] = fa.map(f)

    given [F[_]] => FieldOperation.Read[Field.Read[F, *], F]:
      override def lift[A](name: String, schema: Reference[F, A]): Field.Read[F, A] = Root(name, schema)

      extension [A](fa: Field.Read[F, A])
        override def isOptional: Boolean = fa.isOptional

        override def name: String = fa.name

        override def optional: Field.Read[F, Option[A]] = fa.optional

        override def optional(default: => A): Field.Read[F, A] = fa.optional(default = Eval.later(default))

        override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+F[_], -A]:
    def isOptional: Boolean

    def name: String

    def optional: Field.Write[F, Option[A]] = Write.Optional(self = this)

    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Write[G, A]

    final def contramap[B](f: B => A): Field.Write[F, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[F[_], A, B](self: Field.Write[F, A], f: B => A) extends Field.Write[F, B]:
      export self.{isOptional, name, schema}

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Write[G, B] = copy(self = self.mapK(fK))

    final case class Optional[F[_], A](self: Field.Write[F, A]) extends Field.Write[F, Option[A]]:
      export self.{name, schema}

      override def isOptional: Boolean = true

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field.Write[G, Option[A]] = copy(self = self.mapK(fK))

    given [F[_]] => Contravariant[Field.Write[F, *]]:
      override def contramap[A, B](fa: Field.Write[F, A])(f: B => A): Field.Write[F, B] = fa.contramap(f)

    given [F[_]] => FieldOperation.Write[Field.Write[F, *], F]:
      override def lift[A](name: String, schema: Reference[F, A]): Field.Write[F, A] = Root(name, schema)

      extension [A](fa: Field.Write[F, A])
        override def isOptional: Boolean = fa.isOptional

        override def name: String = fa.name

        override def optional: Field.Write[F, Option[A]] = fa.optional

        override def schema: Reference[F, ?] = fa.schema

  final case class Default[F[_], A](self: Field[F, A], value: Eval[A]) extends Field[F, A]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field[G, A] = copy(self = self.mapK(fK))

  final case class Modify[F[_], A, B](self: Field[F, A], f: A => B, g: B => A) extends Field[F, B]:
    export self.{isOptional, name, schema}

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field[G, B] = copy(self = self.mapK(fK))

  final case class Optional[F[_], A](self: Field[F, A]) extends Field[F, Option[A]]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field[G, Option[A]] = copy(self = self.mapK(fK))

  final case class Root[F[_], A](name: String, schema: Reference[F, A]) extends Field[F, A]:
    override def isOptional: Boolean = false

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Field[G, A] = copy(schema = schema.mapK[F, G](fK))

  given [F[_]] => Invariant[Field[F, *]]:
    override def imap[A, B](fa: Field[F, A])(f: A => B)(g: B => A): Field[F, B] = Modify(fa, f, g)

  given [F[_]] => FieldOperation[Field[F, *], F]:
    override def lift[A](name: String, schema: Reference[F, A]): Field[F, A] = Root(name, schema)

    extension [A](fa: Field[F, A])
      override def isOptional: Boolean = fa.isOptional

      override def name: String = fa.name

      override def optional: Field[F, Option[A]] = Optional(fa)

      override def optional(default: => A): Field[F, A] = Default(fa, value = Eval.later(default))

      override def schema: Reference[F, ?] = fa.schema
