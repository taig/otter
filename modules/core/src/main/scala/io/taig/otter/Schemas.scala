package io.taig.otter

import io.taig.otter as Base
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait Schemas[F[+_]] extends Types[F]:
  def primitive[A](tpe: Type[A]): Primitive.Required[A]
  final val bigDecimal: Primitive[JBigDecimal] = primitive(Type.BigDecimal)
  final val bigInteger: Primitive[JBigInteger] = primitive(Type.BigInteger)
  final val int: Primitive[Int] = primitive(Type.Int)
  final val long: Primitive[Long] = primitive(Type.Long)
  final val string: Primitive[String] = primitive(Type.String)

  // protected def collection[S[a] <: Schema[a], A](schema: S[A]): Collection.Of[S[A], Chain[A]]
  // protected def collectionReader[S[a] <: Schema.Reader[a], A](schema: S[A]): Collection.Reader.Of[S[A], Chain[A]]
  // protected def collectionWriter[S[a] <: Schema.Writer[a], A](schema: S[A]): Collection.Writer.Of[S[A], Chain[A]]

  // object collection:
  //   def chain[S[a] <: Schema[a], A](schema: S[A]): Collection.Of[S[A], Chain[A]] = collection(schema)

  //   @targetName("chainR")
  //   def chain[S[a] <: Schema.Reader[a], A](schema: S[A]): Collection.Reader.Of[S[A], Chain[A]] =
  //     collectionReader(schema)

  //   @targetName("chainW")
  //   def chain[S[a] <: Schema.Writer[a], A](schema: S[A]): Collection.Writer.Of[S[A], Chain[A]] =
  //     collectionWriter(schema)

  //   def list[S[a] <: Schema[a], A](schema: S[A]): Collection.Of[S[A], List[A]] = ???

  //   @targetName("listR")
  //   def list[S[a] <: Schema.Reader[a], A](schema: S[A]): Collection.Reader.Of[S[A], List[A]] = ???

  //   @targetName("listW")
  //   def list[S[a] <: Schema.Writer[a], A](schema: S[A]): Collection.Writer.Of[S[A], List[A]] = ???

  //   def set[S[a] <: Schema[a], A](schema: S[A]): Collection.Of[S[A], Set[A]] = ???

  //   @targetName("setR")
  //   def set[S[a] <: Schema.Reader[a], A](schema: S[A]): Collection.Reader.Of[S[A], Set[A]] = ???

  //   @targetName("setW")
  //   def set[S[a] <: Schema.Writer[a], A](schema: S[A]): Collection.Writer.Of[S[A], Set[A]] = ???
