package io.taig.otter

import io.taig.otter as Base
import scala.annotation.targetName

trait Schemas extends Types:
  protected def asPrimitive[A](a: A): AsPrimitive[A]
  protected def asCollection[A](a: A): AsCollection[A]
  protected def asTuple[A](a: A): AsTuple[A]

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    asPrimitive(Base.Isomorphic.Root(Base.Required(Base.Primitive(tpe))))

  final val string: Primitive.Required[String] = primitive(Type.String)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)

  object collection:
    @targetName("isomorphic")
    def apply[F[a] <: Isomorphic[a], A](schema: F[A]): Collection.Of[F[A], Vector[A]] =
      asCollection(Base.Isomorphic.Root(Base.Required(Base.Collection.Root(schema))))

    @targetName("reader")
    def apply[F[a] <: Reader[a], A](schema: F[A]): Collection.Reader.Of[F[A], Vector[A]] =
      asCollection(Base.Reader.Root(Base.Required(Base.Collection.Root(schema))))

    @targetName("writer")
    def apply[F[a] <: Writer[a], A](schema: F[A]): Collection.Writer.Of[F[A], Vector[A]] =
      asCollection(Base.Writer.Root(Base.Required(Base.Collection.Root(schema))))

  extension [F[a] <: Isomorphic[a], A](schema: F[A])
    def toTuple: Tuple.Of[F[A], A] =
      asTuple(Base.Isomorphic.Root(Base.Required(Base.Tuple.One(schema))))
