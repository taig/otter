package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.base as Base
import io.taig.otter.base.Collection

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Schema.Of[S, A]]

object Schema:
  type Of[+S[_], A] = Base.Coerce[S, A] | Base.Collection[S, A] | Base.Constant[S, A] | Base.Dictionary[S, A] |
    Base.Enumeration[S, A] | Base.Nullable[S, A] | Base.Primitive[A]

  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Schema.Read.Of[S, A]]

  object Read:
    type Of[+S[_], +A] = Base.Coerce.Read[S, A] | Base.Collection.Read[S, A] | Base.Constant.Read[S, A] |
      Base.Dictionary.Read[S, A] | Base.Enumeration.Read[S, A] | Base.Nullable.Read[S, A] | Base.Primitive.Read[A]

    def apply[S[a] <: Schema.Read[?, a], A](annotation: Annotation[Schema.Read.Of[S, A]]): Schema.Read[S, A] =
      annotation.self match
        case self: Base.Coerce.Read[S, A]      => Schema.Coerce.Read(annotation.copy(self = self))
        case self: Base.Collection.Read[S, A]  => Schema.Collection.Read(annotation.copy(self = self))
        case self: Base.Constant.Read[S, A]    => Schema.Constant.Read(annotation.copy(self = self))
        case self: Base.Dictionary.Read[S, A]  => Schema.Dictionary.Read(annotation.copy(self = self))
        case self: Base.Enumeration.Read[S, A] => Schema.Enumeration.Read(annotation.copy(self = self))
        case self: Base.Nullable.Read[S, A]    => Schema.Nullable.Read(annotation.copy(self = self))
        case self: Base.Primitive.Read[A]      => Schema.Primitive.Read(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Read[S, A]): Annotation[Schema.Read.Of[S, A]] = schema.self

    given [S[a] <: Schema.Read[?, a]]: Wrapper[Schema.Read, [s[a] <: S[a], a] =>> Annotation[Schema.Read.Of[s, a]], S]
    with
      override def extract[I[a] <: S[a], A](schema: Schema.Read[I, A]): Annotation[Schema.Read.Of[I, A]] = schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Schema.Read.Of[I, A]]): Schema.Read[I, A] =
        Schema.Read(annotation)

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Schema.Write.Of[S, A]]

  object Write:
    type Of[+S[_], -A] = Base.Coerce.Write[S, A] | Base.Collection.Write[S, A] | Base.Constant.Write[S, A] |
      Base.Dictionary.Write[S, A] | Base.Enumeration.Write[S, A] | Base.Nullable.Write[S, A] | Base.Primitive.Write[A]

    def apply[S[a] <: Schema.Write[?, a], A](annotation: Annotation[Schema.Write.Of[S, A]]): Schema.Write[S, A] =
      annotation.self match
        case self: Base.Coerce.Write[S, A]      => Schema.Coerce.Write(annotation.copy(self = self))
        case self: Base.Collection.Write[S, A]  => Schema.Collection.Write(annotation.copy(self = self))
        case self: Base.Constant.Write[S, A]    => Schema.Constant.Write(annotation.copy(self = self))
        case self: Base.Dictionary.Write[S, A]  => Schema.Dictionary.Write(annotation.copy(self = self))
        case self: Base.Enumeration.Write[S, A] => Schema.Enumeration.Write(annotation.copy(self = self))
        case self: Base.Nullable.Write[S, A]    => Schema.Nullable.Write(annotation.copy(self = self))
        case self: Base.Primitive.Write[A]      => Schema.Primitive.Write(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Write[?, a], A](schema: Schema.Write[S, A]): Annotation[Schema.Write.Of[S, A]] =
      schema.self

    given [S[a] <: Schema.Write[?, a]]
        : Wrapper[Schema.Write, [s[a] <: S[a], a] =>> Annotation[Schema.Write.Of[s, a]], S] with
      override def extract[I[a] <: S[a], A](schema: Schema.Write[I, A]): Annotation[Schema.Write.Of[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Schema.Write.Of[I, A]]): Schema.Write[I, A] =
        Schema.Write(annotation)

  sealed abstract class Coerce[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Coerce.Read[S, A],
        Schema.Coerce.Write[S, A]:
    override def self: Annotation[Base.Coerce[S, A]]

  object Coerce:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Coerce.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Coerce.Read[S, A]]
      ): Schema.Coerce.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Coerce.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Coerce.Read[S, A]): Annotation[Base.Coerce.Read[S, A]] =
        schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Coerce.Read, [s[a] <: S[a], a] =>> Annotation[Base.Coerce.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](schema: Schema.Coerce.Read[I, A]): Annotation[Base.Coerce.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Coerce.Read[I, A]]): Schema.Coerce.Read[I, A] =
          Schema.Coerce.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Coerce.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Coerce.Write[S, A]]
      ): Schema.Coerce.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Coerce.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Coerce.Write[S, A]
      ): Annotation[Base.Coerce.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Coerce.Write, [s[a] <: S[a], a] =>> Annotation[Base.Coerce.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](schema: Schema.Coerce.Write[I, A]): Annotation[Base.Coerce.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Coerce.Write[I, A]]
        ): Schema.Coerce.Write[I, A] =
          Schema.Coerce.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Coerce[S, A]]): Schema.Coerce[S, A] =
      new Coerce[S, A]:
        override def self: Annotation[Base.Coerce[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Coerce[S, A]): Annotation[Base.Coerce[S, A]] = schema.self

    given [S[a] <: Schema[?, a]]: Wrapper[Schema.Coerce, [s[a] <: S[a], a] =>> Annotation[Base.Coerce[s, a]], S] with
      override def extract[I[a] <: S[a], A](schema: Schema.Coerce[I, A]): Annotation[Base.Coerce[I, A]] = schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Coerce[I, A]]): Schema.Coerce[I, A] =
        Schema.Coerce(annotation)

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def self: Annotation[Base.Collection[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Collection.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Collection.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Collection.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Collection.Read[S, A]
      ): Annotation[Base.Collection.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Collection.Read, [s[a] <: S[a], a] =>> Annotation[Base.Collection.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Collection.Read[I, A]
        ): Annotation[Base.Collection.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Collection.Read[I, A]]
        ): Schema.Collection.Read[I, A] =
          Schema.Collection.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Collection.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Collection.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Collection.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Collection.Write[S, A]
      ): Annotation[Base.Collection.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Collection.Write, [s[a] <: S[a], a] =>> Annotation[Base.Collection.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Collection.Write[I, A]
        ): Annotation[Base.Collection.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Collection.Write[I, A]]
        ): Schema.Collection.Write[I, A] =
          Schema.Collection.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Collection[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Base.Collection[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[Base.Collection[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Wrapper[Schema.Collection, [s[a] <: S[a], a] =>> Annotation[Base.Collection[s, a]], S]
    with
      override def extract[I[a] <: S[a], A](schema: Schema.Collection[I, A]): Annotation[Base.Collection[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Collection[I, A]]): Schema.Collection[I, A] =
        Schema.Collection(annotation)

  sealed abstract class Constant[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Constant.Read[S, A],
        Schema.Constant.Write[S, A]:
    override def self: Annotation[Base.Constant[S, A]]

  object Constant:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Constant.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Constant.Read[S, A]]
      ): Schema.Constant.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Constant.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Constant.Read[S, A]
      ): Annotation[Base.Constant.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Constant.Read, [s[a] <: S[a], a] =>> Annotation[Base.Constant.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Constant.Read[I, A]
        ): Annotation[Base.Constant.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Constant.Read[I, A]]
        ): Schema.Constant.Read[I, A] =
          Schema.Constant.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Constant.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Constant.Write[S, A]]
      ): Schema.Constant.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Constant.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Constant.Write[S, A]
      ): Annotation[Base.Constant.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Constant.Write, [s[a] <: S[a], a] =>> Annotation[Base.Constant.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Constant.Write[I, A]
        ): Annotation[Base.Constant.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Constant.Write[I, A]]
        ): Schema.Constant.Write[I, A] =
          Schema.Constant.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Constant[S, A]]): Schema.Constant[S, A] =
      new Constant[S, A]:
        override def self: Annotation[Base.Constant[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Constant[S, A]): Annotation[Base.Constant[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Wrapper[Schema.Constant, [s[a] <: S[a], a] =>> Annotation[Base.Constant[s, a]], S]
    with
      override def extract[I[a] <: S[a], A](schema: Schema.Constant[I, A]): Annotation[Base.Constant[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Constant[I, A]]): Schema.Constant[I, A] =
        Schema.Constant(annotation)

  sealed abstract class Dictionary[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Dictionary.Read[S, A],
        Schema.Dictionary.Write[S, A]:
    override def self: Annotation[Base.Dictionary[S, A]]

  object Dictionary:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Dictionary.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Dictionary.Read[S, A]]
      ): Schema.Dictionary.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Dictionary.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Dictionary.Read[S, A]
      ): Annotation[Base.Dictionary.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Dictionary.Read, [s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Dictionary.Read[I, A]
        ): Annotation[Base.Dictionary.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Dictionary.Read[I, A]]
        ): Schema.Dictionary.Read[I, A] =
          Schema.Dictionary.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Dictionary.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Dictionary.Write[S, A]]
      ): Schema.Dictionary.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Dictionary.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Dictionary.Write[S, A]
      ): Annotation[Base.Dictionary.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Dictionary.Write, [s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Dictionary.Write[I, A]
        ): Annotation[Base.Dictionary.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Dictionary.Write[I, A]]
        ): Schema.Dictionary.Write[I, A] =
          Schema.Dictionary.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Dictionary[S, A]]): Schema.Dictionary[S, A] =
      new Dictionary[S, A]:
        override def self: Annotation[Base.Dictionary[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Dictionary[S, A]): Annotation[Base.Dictionary[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Wrapper[Schema.Dictionary, [s[a] <: S[a], a] =>> Annotation[Base.Dictionary[s, a]], S]
    with
      override def extract[I[a] <: S[a], A](schema: Schema.Dictionary[I, A]): Annotation[Base.Dictionary[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Dictionary[I, A]]): Schema.Dictionary[I, A] =
        Schema.Dictionary(annotation)

  sealed abstract class Enumeration[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Enumeration.Read[S, A],
        Schema.Enumeration.Write[S, A]:
    override def self: Annotation[Base.Enumeration[S, A]]

  object Enumeration:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Enumeration.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Enumeration.Read[S, A]]
      ): Schema.Enumeration.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Enumeration.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Enumeration.Read[S, A]
      ): Annotation[Base.Enumeration.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Enumeration.Read, [s[a] <: S[a], a] =>> Annotation[Base.Enumeration.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Enumeration.Read[I, A]
        ): Annotation[Base.Enumeration.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Enumeration.Read[I, A]]
        ): Schema.Enumeration.Read[I, A] =
          Schema.Enumeration.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Enumeration.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Enumeration.Write[S, A]]
      ): Schema.Enumeration.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Enumeration.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Enumeration.Write[S, A]
      ): Annotation[Base.Enumeration.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Enumeration.Write, [s[a] <: S[a], a] =>> Annotation[Base.Enumeration.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Enumeration.Write[I, A]
        ): Annotation[Base.Enumeration.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Enumeration.Write[I, A]]
        ): Schema.Enumeration.Write[I, A] =
          Schema.Enumeration.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Enumeration[S, A]]): Schema.Enumeration[S, A] =
      new Enumeration[S, A]:
        override def self: Annotation[Base.Enumeration[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Enumeration[S, A]): Annotation[Base.Enumeration[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]
        : Wrapper[Schema.Enumeration, [s[a] <: S[a], a] =>> Annotation[Base.Enumeration[s, a]], S] with
      override def extract[I[a] <: S[a], A](schema: Schema.Enumeration[I, A]): Annotation[Base.Enumeration[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Enumeration[I, A]]): Schema.Enumeration[I, A] =
        Schema.Enumeration(annotation)

  sealed abstract class Nullable[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Nullable.Read[S, A],
        Schema.Nullable.Write[S, A]:
    override def self: Annotation[Base.Nullable[S, A]]

  object Nullable:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Nullable.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Nullable.Read[S, A]]
      ): Schema.Nullable.Read[S, A] = new Schema.Nullable.Read[S, A]:
        override def self: Annotation[Base.Nullable.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Nullable.Read[S, A]
      ): Annotation[Base.Nullable.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]
          : Wrapper[Schema.Nullable.Read, [s[a] <: S[a], a] =>> Annotation[Base.Nullable.Read[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Nullable.Read[I, A]
        ): Annotation[Base.Nullable.Read[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Nullable.Read[I, A]]
        ): Schema.Nullable.Read[I, A] =
          Schema.Nullable.Read(annotation)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Nullable.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Nullable.Write[S, A]]
      ): Schema.Nullable.Write[S, A] = new Schema.Nullable.Write[S, A]:
        override def self: Annotation[Base.Nullable.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Nullable.Write[S, A]
      ): Annotation[Base.Nullable.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]
          : Wrapper[Schema.Nullable.Write, [s[a] <: S[a], a] =>> Annotation[Base.Nullable.Write[s, a]], S] with
        override def extract[I[a] <: S[a], A](
            schema: Schema.Nullable.Write[I, A]
        ): Annotation[Base.Nullable.Write[I, A]] =
          schema.self
        override def inject[I[a] <: S[a], A](
            annotation: Annotation[Base.Nullable.Write[I, A]]
        ): Schema.Nullable.Write[I, A] =
          Schema.Nullable.Write(annotation)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Nullable[S, A]]): Schema.Nullable[S, A] =
      new Nullable[S, A]:
        override def self: Annotation[Base.Nullable[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Nullable[S, A]): Annotation[Base.Nullable[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Wrapper[Schema.Nullable, [s[a] <: S[a], a] =>> Annotation[Base.Nullable[s, a]], S]
    with
      override def extract[I[a] <: S[a], A](schema: Schema.Nullable[I, A]): Annotation[Base.Nullable[I, A]] =
        schema.self
      override def inject[I[a] <: S[a], A](annotation: Annotation[Base.Nullable[I, A]]): Schema.Nullable[I, A] =
        Schema.Nullable(annotation)

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[Base.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[Base.Primitive.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Base.Primitive.Read[A]]): Schema.Primitive.Read[A] = annotation.self match
        case self: Base.Primitive.Boolean.Read[A] => Boolean.Read(annotation.copy(self = self))
        case self: Base.Primitive.Number.Read[A]  => Number.Read(annotation.copy(self = self))
        case self: Base.Primitive.Text.Read[A]    => Text.Read(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Read[A]): Annotation[Base.Primitive.Read[A]] = schema.self

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[Base.Primitive.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Base.Primitive.Write[A]]): Schema.Primitive.Write[A] = annotation.self match
        case self: Base.Primitive.Boolean.Write[A] => Boolean.Write(annotation.copy(self = self))
        case self: Base.Primitive.Number.Write[A]  => Number.Write(annotation.copy(self = self))
        case self: Base.Primitive.Text.Write[A]    => Text.Write(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Write[A]): Annotation[Base.Primitive.Write[A]] = schema.self

    sealed abstract class Boolean[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Boolean.Read[A],
          Schema.Primitive.Boolean.Write[A]:
      override def self: Annotation[Base.Primitive.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Boolean.Read[A]]): Schema.Primitive.Boolean.Read[A] =
          new Schema.Primitive.Boolean.Read[A]:
            override def self: Annotation[Base.Primitive.Boolean.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Read[A]): Annotation[Base.Primitive.Boolean.Read[A]] =
          schema.self

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Boolean.Write[A]]): Schema.Primitive.Boolean.Write[A] =
          new Schema.Primitive.Boolean.Write[A]:
            override def self: Annotation[Base.Primitive.Boolean.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Write[A]): Annotation[Base.Primitive.Boolean.Write[A]] =
          schema.self

      def apply[A](annotation: Annotation[Base.Primitive.Boolean[A]]): Schema.Primitive.Boolean[A] = new Boolean[A]:
        override def self: Annotation[Base.Primitive.Boolean[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Boolean[A]): Annotation[Base.Primitive.Boolean[A]] = schema.self

    sealed abstract class Number[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Number.Read[A],
          Schema.Primitive.Number.Write[A]:
      override def self: Annotation[Base.Primitive.Number[A]]

    object Number:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Number.Read[A]]): Schema.Primitive.Number.Read[A] =
          new Schema.Primitive.Number.Read[A]:
            override def self: Annotation[Base.Primitive.Number.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Read[A]): Annotation[Base.Primitive.Number.Read[A]] =
          schema.self

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Number.Write[A]]): Schema.Primitive.Number.Write[A] =
          new Schema.Primitive.Number.Write[A]:
            override def self: Annotation[Base.Primitive.Number.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Write[A]): Annotation[Base.Primitive.Number.Write[A]] =
          schema.self

      def apply[A](annotation: Annotation[Base.Primitive.Number[A]]): Schema.Primitive.Number[A] = new Number[A]:
        override def self: Annotation[Base.Primitive.Number[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Number[A]): Annotation[Base.Primitive.Number[A]] = schema.self

    sealed abstract class Text[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Text.Read[A],
          Schema.Primitive.Text.Write[A]:
      override def self: Annotation[Base.Primitive.Text[A]]

    object Text:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Text.Read[A]]): Schema.Primitive.Text.Read[A] =
          new Schema.Primitive.Text.Read[A]:
            override def self: Annotation[Base.Primitive.Text.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Read[A]): Annotation[Base.Primitive.Text.Read[A]] = schema.self

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Text.Write[A]]): Schema.Primitive.Text.Write[A] =
          new Schema.Primitive.Text.Write[A]:
            override def self: Annotation[Base.Primitive.Text.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Write[A]): Annotation[Base.Primitive.Text.Write[A]] = schema.self

      def apply[A](annotation: Annotation[Base.Primitive.Text[A]]): Schema.Primitive.Text[A] =
        new Schema.Primitive.Text[A]:
          override def self: Annotation[Base.Primitive.Text[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Text[A]): Annotation[Base.Primitive.Text[A]] = schema.self

    def apply[A](annotation: Annotation[Base.Primitive[A]]): Schema.Primitive[A] = annotation.self match
      case self: Base.Primitive.Boolean[A] => Schema.Primitive.Boolean(annotation.copy(self = self))
      case self: Base.Primitive.Number[A]  => Schema.Primitive.Number(annotation.copy(self = self))
      case self: Base.Primitive.Text[A]    => Schema.Primitive.Text(annotation.copy(self = self))

    def unapply[A](schema: Schema.Primitive[A]): Annotation[Base.Primitive[A]] = schema.self

  def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Schema.Of[S, A]]): Schema[S, A] = annotation.self match
    case self: Base.Coerce[S, A]      => Schema.Coerce(annotation.copy(self = self))
    case self: Base.Collection[S, A]  => Schema.Collection(annotation.copy(self = self))
    case self: Base.Constant[S, A]    => Schema.Constant(annotation.copy(self = self))
    case self: Base.Dictionary[S, A]  => Schema.Dictionary(annotation.copy(self = self))
    case self: Base.Enumeration[S, A] => Schema.Enumeration(annotation.copy(self = self))
    case self: Base.Nullable[S, A]    => Schema.Nullable(annotation.copy(self = self))
    case self: Base.Primitive[A]      => Schema.Primitive(annotation.copy(self = self))

  def unapply[S[a] <: Schema[?, a], A](schema: Schema[S, A]): Annotation[Schema.Of[S, A]] = schema.self

  given [S[a] <: Schema[?, a]]: Wrapper[Schema, [s[a] <: S[a], a] =>> Annotation[Schema.Of[s, a]], S] with
    override def extract[I[a] <: S[a], A](schema: Schema[I, A]): Annotation[Schema.Of[I, A]] = schema.self
    override def inject[I[a] <: S[a], A](annotation: Annotation[Schema.Of[I, A]]): Schema[I, A] = Schema(annotation)

  given [S[a] <: Schema[?, a]]: Coerceable[Schema.Coerce, S] with
    override val coerce: Self.Coerce[Schema.Coerce, S] = Self.Coerce[Schema.Coerce, S]
