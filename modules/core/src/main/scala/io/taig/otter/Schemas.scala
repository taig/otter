package io.taig.otter

import io.taig.otter as Base
import scala.annotation.targetName
import io.taig.otter.Isomorphic

trait Schemas extends Types:
  protected def asPrimitive[A](a: A): AsPrimitive[A]
  protected def asCollection[A](a: A): AsCollection[A]
  protected def asTuple[A](a: A): AsTuple[A]
  protected def extract[A](a: AsSchema[A]): A = ???

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
    @targetName("isomorphic")
    def toTuple: Tuple.Of[F[A], A] = asTuple(Base.Isomorphic.Root(Base.Required(Base.Tuple.One(schema))))
    def product[G[a] <: Isomorphic[a], B](tuple: G[B]): Tuple.Of[F[A] | G[B], (A, B)] =
      asTuple(Base.Isomorphic.Root(Base.Required(Base.Tuple.Product(schema, tuple))))

  extension [F[a] <: Reader[a], A](schema: F[A])
    @targetName("reader")
    def toTuple: Tuple.Reader.Of[F[A], A] = asTuple(Base.Reader.Root(Base.Required(Base.Tuple.One(schema))))

  extension [F[a] <: Writer[a], A](schema: F[A])
    @targetName("writer")
    def toTuple: Tuple.Writer.Of[F[A], A] = asTuple(Base.Writer.Root(Base.Required(Base.Tuple.One(schema))))
