package io.taig.otter

import cats.Id as Identity
import io.taig.otter as Base
import cats.data.Chain
import scala.annotation.targetName

object Plain extends Dsl[Identity]:
  override def primitive[A](tpe: Type[A]): Primitive[A] = Base.Primitive.Root(tpe)

//   override protected def collection[S[a] <: Schema[a], A](schema: S[A]): Collection.Of[S[A], Chain[A]] =
//     Fix(Base.Collection.Root(schema, _.unfix))

//   override protected def collectionReader[S[a] <: Schema.Reader[a], A](
//       schema: S[A]
//   ): Collection.Reader.Of[S[A], Chain[A]] = Fix(Base.Collection.Reader.Root(schema, _.unfix))

//   override protected def collectionWriter[S[a] <: Schema.Writer[a], A](
//       schema: S[A]
//   ): Collection.Writer.Of[S[A], Chain[A]] = Fix(Base.Collection.Writer.Root(schema, _.unfix))
