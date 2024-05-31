package io.taig.otter

import io.taig.otter as Base

trait Schemas extends Types:
  def primitive[A](tpe: Type[A]): Primitive.Required[A]

  final val string: Primitive.Required[String] = primitive(Type.String)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)

  def collection[F[a] <: AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, a]], A](
      schema: F[A]
  ): Collection.Of[F[A], Vector[A]]

  // def collection2[X[F[_], O[_[_], _], S[_, _], A], A](
  //     schema: AsSchema[X[AsSchema, Base.Optional, Base.Schema, A]]
  // ): AsSchema[X[AsCollection, Base.Required, Base.Schema, A]]

  def collectionReader[F[a] <: AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, a]], A](
      schema: F[A]
  ): Collection.Reader.Of[F[A], Vector[A]]

  def collectionWriter[F[a] <: AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, a]], A](
      schema: F[A]
  ): Collection.Writer.Of[F[A], Vector[A]]
