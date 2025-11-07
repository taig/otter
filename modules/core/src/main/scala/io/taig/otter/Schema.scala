package io.taig.otter

import io.taig.otter as Self
import io.taig.validation.Validation
import cats.data.Chain
import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A] with Schema.Write[S, A]:
  override def modify(f: Metadata => Metadata): Schema[S, A]

  def imap[T](f: A => T)(g: T => A): Schema[S, T]

object Schema:
  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def metadata: Metadata

    def modify(f: Metadata => Metadata): Schema.Read[S, A]

    def map[T](f: A => T): Schema.Read[S, T]

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def metadata: Metadata

    def modify(f: Metadata => Metadata): Schema.Write[S, A]

    def contramap[T](f: T => A): Schema.Write[S, T]

  sealed abstract class Coerce[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Coerce.Read[S, A],
        Schema.Coerce.Write[S, A]:
    def modify(f: Metadata => Metadata): Schema.Coerce[S, A]

    final override def imap[T](f: A => T)(g: T => A): Schema.Coerce[S, T] = ???

  object Coerce:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def modify(f: Metadata => Metadata): Schema.Coerce.Read[S, A]

      final override def map[T](f: A => T): Schema.Coerce.Read[S, T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[S[a] <: Schema.Read[?, a], A, B](self: Schema.Coerce.Read[S, A], f: A => B)
          extends Schema.Coerce.Read[S, B]:
        export self.metadata

        override def modify(f: Metadata => Metadata): Schema.Coerce.Read[S, B] =
          copy(self = self.modify(f))

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def modify(f: Metadata => Metadata): Schema.Coerce.Write[S, A]

      final override def contramap[T](f: T => A): Schema.Coerce.Write[S, T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[S[a] <: Schema.Write[?, a], A, B](self: Schema.Coerce.Write[S, A], f: B => A)
          extends Schema.Coerce.Write[S, B]:
        export self.metadata

        override def modify(f: Metadata => Metadata): Schema.Coerce.Write[S, B] =
          copy(self = self.modify(f))

    final case class Modify[+S[a] <: Schema[?, a], A, B](self: Schema.Coerce[S, A], f: A => B, g: B => A)
        extends Schema.Coerce[S, B]:
      export self.metadata

      override def modify(f: Metadata => Metadata): Schema.Coerce[S, B] =
        copy(self = self.modify(f))

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def modify(f: Metadata => Metadata): Schema.Collection[S, A]

    final override def imap[T](f: A => T)(g: T => A): Schema.Collection[S, T] =
      Collection.Modify(self = this, f, g)

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Collection.Read[S, A]

      final override def map[T](f: A => T): Schema.Collection.Read[S, T] = Read.Modify(self = this, f)

    object Read:
      final case class Chained[S[a] <: Schema.Read[?, a], A](
          schema: Reference[S, A],
          validation: Validation[Constraint.Collection, List[A]],
          metadata: Metadata
      ) extends Schema.Collection.Read[S, Chain[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Read[S, Chain[A]] =
          copy(metadata = f(metadata))

      final case class Indexed[S[a] <: Schema.Read[?, a], A](
          schema: Reference[S, A],
          metadata: Metadata
      ) extends Schema.Collection.Read[S, Vector[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Read[S, Vector[A]] =
          copy(metadata = f(metadata))

      final case class Linked[S[a] <: Schema.Read[?, a], A](
          schema: Reference[S, A],
          validation: Validation[Constraint.Collection, List[A]],
          metadata: Metadata
      ) extends Schema.Collection.Read[S, List[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Read[S, List[A]] =
          copy(metadata = f(metadata))

      final case class Modify[S[a] <: Schema.Read[?, a], A, B](self: Schema.Collection.Read[S, A], f: A => B)
          extends Schema.Collection.Read[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Collection.Read[S, B] =
          copy(self = self.modify(f))

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Collection.Write[S, A]

      final override def contramap[T](f: T => A): Schema.Collection.Write[S, T] = Write.Modify(self = this, f)

    object Write:
      final case class Chained[S[a] <: Schema.Write[?, a], A](
          schema: Reference[S, A],
          metadata: Metadata
      ) extends Schema.Collection.Write[S, Chain[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Write[S, Chain[A]] =
          copy(metadata = f(metadata))

      final case class Indexed[S[a] <: Schema.Write[?, a], A](
          schema: Reference[S, A],
          metadata: Metadata
      ) extends Schema.Collection.Write[S, Vector[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Write[S, Vector[A]] =
          copy(metadata = f(metadata))

      final case class Linked[S[a] <: Schema.Write[?, a], A](
          schema: Reference[S, A],
          metadata: Metadata
      ) extends Schema.Collection.Write[S, List[A]]:
        override def modify(f: Metadata => Metadata): Schema.Collection.Write[S, List[A]] =
          copy(metadata = f(metadata))

      final case class Modify[S[a] <: Schema.Write[?, a], A, B](self: Schema.Collection.Write[S, A], f: B => A)
          extends Schema.Collection.Write[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Collection.Write[S, B] =
          copy(self = self.modify(f))

    final case class Chained[S[a] <: Schema[?, a], A](
        schema: Reference[S, A],
        validation: Validation[Constraint.Collection, List[A]],
        metadata: Metadata
    ) extends Schema.Collection[S, Chain[A]]:
      override def modify(f: Metadata => Metadata): Schema.Collection[S, Chain[A]] = copy(metadata = f(metadata))

    final case class Indexed[S[a] <: Schema[?, a], A](
        schema: Reference[S, A],
        metadata: Metadata
    ) extends Schema.Collection[S, Vector[A]]:
      override def modify(f: Metadata => Metadata): Schema.Collection[S, Vector[A]] = copy(metadata = f(metadata))

    final case class Linked[S[a] <: Schema[?, a], A](
        schema: Reference[S, A],
        validation: Validation[Constraint.Collection, List[A]],
        metadata: Metadata
    ) extends Schema.Collection[S, List[A]]:
      override def modify(f: Metadata => Metadata): Schema.Collection[S, List[A]] = copy(metadata = f(metadata))

    final case class Modify[S[a] <: Schema[?, a], A, B](self: Schema.Collection[S, A], f: A => B, g: B => A)
        extends Schema.Collection[S, B]:
      export self.{metadata, schema}

      override def modify(f: Metadata => Metadata): Schema.Collection[S, B] =
        copy(self = self.modify(f))

  sealed abstract class Constant[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Constant.Read[S, A],
        Schema.Constant.Write[S, A]:
    override def modify(f: Metadata => Metadata): Schema.Constant[S, A]

    final override def imap[T](f: A => T)(g: T => A): Schema.Constant[S, T] =
      Constant.Modify(self = this, f, g)

  object Constant:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Constant.Read[S, A]

      final override def map[T](f: A => T): Schema.Constant.Read[S, T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[S[a] <: Schema.Read[?, a], A, B](self: Schema.Constant.Read[S, A], f: A => B)
          extends Schema.Constant.Read[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Constant.Read[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Read[?, a], A](schema: Reference[S, A], metadata: Metadata)
          extends Constant.Read[S, A]:
        override def modify(f: Metadata => Metadata): Schema.Constant.Read[S, A] =
          copy(metadata = f(metadata))

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Constant.Write[S, A]

      final override def contramap[T](f: T => A): Schema.Constant.Write[S, T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[S[a] <: Schema.Write[?, a], A, B](self: Schema.Constant.Write[S, A], f: B => A)
          extends Schema.Constant.Write[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Constant.Write[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Write[?, a], A](schema: Reference[S, A], metadata: Metadata)
          extends Constant.Write[S, A]:
        override def modify(f: Metadata => Metadata): Schema.Constant.Write[S, A] =
          copy(metadata = f(metadata))

    final case class Modify[S[a] <: Schema[?, a], A, B](self: Schema.Constant[S, A], f: A => B, g: B => A)
        extends Schema.Constant[S, B]:
      export self.{metadata, schema}

      override def modify(f: Metadata => Metadata): Schema.Constant[S, B] = copy(self = self.modify(f))

    final case class Root[S[a] <: Schema[?, a], A](schema: Reference[S, A], metadata: Metadata) extends Constant[S, A]:
      override def modify(f: Metadata => Metadata): Schema.Constant[S, A] =
        copy(metadata = f(metadata))

  sealed abstract class Dictionary[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Dictionary.Read[S, A],
        Schema.Dictionary.Write[S, A]:
    override def modify(f: Metadata => Metadata): Schema.Dictionary[S, A]

    final override def imap[T](f: A => T)(g: T => A): Schema.Dictionary[S, T] =
      Dictionary.Modify(self = this, f, g)

  object Dictionary:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Dictionary.Read[S, A]

      final override def map[T](f: A => T): Schema.Dictionary.Read[S, T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[S[a] <: Schema.Read[?, a], A, B](self: Schema.Dictionary.Read[S, A], f: A => B)
          extends Schema.Dictionary.Read[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Dictionary.Read[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Read[?, a], A](schema: Reference[S, A], metadata: Metadata)
          extends Dictionary.Read[S, List[(String, A)]]:
        override def modify(f: Metadata => Metadata): Schema.Dictionary.Read[S, List[(String, A)]] =
          copy(metadata = f(metadata))

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Dictionary.Write[S, A]

      final override def contramap[T](f: T => A): Schema.Dictionary.Write[S, T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[S[a] <: Schema.Write[?, a], A, B](self: Schema.Dictionary.Write[S, A], f: B => A)
          extends Schema.Dictionary.Write[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Dictionary.Write[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Write[?, a], A](schema: Reference[S, A], metadata: Metadata)
          extends Dictionary.Write[S, List[(String, A)]]:
        override def modify(f: Metadata => Metadata): Schema.Dictionary.Write[S, List[(String, A)]] =
          copy(metadata = f(metadata))

    final case class Modify[S[a] <: Schema[?, a], A, B](self: Schema.Dictionary[S, A], f: A => B, g: B => A)
        extends Schema.Dictionary[S, B]:
      export self.{metadata, schema}

      override def modify(f: Metadata => Metadata): Schema.Dictionary[S, B] = copy(self = self.modify(f))

    final case class Root[S[a] <: Schema[?, a], A](schema: Reference[S, A], metadata: Metadata)
        extends Dictionary[S, List[(String, A)]]:
      override def modify(f: Metadata => Metadata): Schema.Dictionary[S, List[(String, A)]] =
        copy(metadata = f(metadata))

  sealed abstract class Enumeration[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Enumeration.Read[S, A],
        Schema.Enumeration.Write[S, A]:
    override def modify(f: Metadata => Metadata): Schema.Enumeration[S, A]

    final override def imap[T](f: A => T)(g: T => A): Schema.Enumeration[S, T] =
      Enumeration.Modify(self = this, f, g)

  object Enumeration:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      def schema: Reference[S, ?]

      def values: NonEmptyList[A]

      override def modify(f: Metadata => Metadata): Schema.Enumeration.Read[S, A]

      final override def map[T](f: A => T): Schema.Enumeration.Read[S, T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[S[a] <: Schema.Read[?, a], A, B](self: Schema.Enumeration.Read[S, A], f: A => B)
          extends Schema.Enumeration.Read[S, B]:
        export self.{metadata, schema}

        override def values: NonEmptyList[B] = self.values.map(f)

        override def modify(f: Metadata => Metadata): Schema.Enumeration.Read[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Read[?, a], A, B](
          schema: Reference[S, A],
          mapping: A => B,
          metadata: Metadata
      ) extends Enumeration.Read[S, B]:
        override def values: NonEmptyList[B] = mapping.values

        override def modify(f: Metadata => Metadata): Schema.Enumeration.Read[S, B] =
          copy(metadata = f(metadata))

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      def schema: Reference[S, ?]

      override def modify(f: Metadata => Metadata): Schema.Enumeration.Write[S, A]

      final override def contramap[T](f: T => A): Schema.Enumeration.Write[S, T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[S[a] <: Schema.Write[?, a], A, B](self: Schema.Enumeration.Write[S, A], f: B => A)
          extends Schema.Enumeration.Write[S, B]:
        export self.{metadata, schema}

        override def modify(f: Metadata => Metadata): Schema.Enumeration.Write[S, B] =
          copy(self = self.modify(f))

      final case class Root[S[a] <: Schema.Write[?, a], A, B](
          schema: Reference[S, A],
          mapping: B => A,
          metadata: Metadata
      ) extends Enumeration.Write[S, B]:
        override def modify(f: Metadata => Metadata): Schema.Enumeration.Write[S, B] =
          copy(metadata = f(metadata))

    final case class Modify[S[a] <: Schema[?, a], A, B](self: Schema.Enumeration[S, A], f: A => B, g: B => A)
        extends Schema.Enumeration[S, B]:
      export self.{metadata, schema}

      override def values: NonEmptyList[B] = self.values.map(f)

      override def modify(f: Metadata => Metadata): Schema.Enumeration[S, B] = copy(self = self.modify(f))

    final case class Root[S[a] <: Schema[?, a], A, B](
        schema: Reference[S, A],
        mapping: Mapping[B, A],
        metadata: Metadata
    ) extends Enumeration[S, B]:
      override def values: NonEmptyList[B] = mapping.values

      override def modify(f: Metadata => Metadata): Schema.Enumeration[S, B] = copy(metadata = f(metadata))
