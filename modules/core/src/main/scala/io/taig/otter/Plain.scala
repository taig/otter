package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.Collection.Root
import io.taig.otter.Isomorphic.Root

object Plain extends Dsl:
  final override type AsSchema[+A] = A
  final override type AsCollection[+A] = A
  final override type AsPrimitive[+A] = A
  final override type AsTuple[+A] = A

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Base.Isomorphic.Root(Base.Required(Base.Primitive(tpe)))

  override def collection[F[a] <: Parent.Isomorphic[a], A](schema: F[A]): Collection.Of[F[A], Vector[A]] =
    Base.Isomorphic.Root(Base.Required(Base.Collection.Root(schema)))

  override def collectionReader[F[a] <: Parent.Reader[a], A](schema: F[A]): Collection.Reader.Of[F[A], Vector[A]] =
    Base.Reader.Root(Base.Required(Base.Collection.Root(???)))

  override def collectionWriter[F[a] <: Parent.Writer[a], A](schema: F[A]): Collection.Writer.Of[F[A], Vector[A]] =
    Base.Writer.Root(Base.Required(Base.Collection.Root(schema)))
