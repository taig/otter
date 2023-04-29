package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation}

sealed abstract class Collection[A] extends Schema[A]:
  self =>

  type Of <: OpenApi
  final override type Self[a] = Collection.Of[a, Of]
  final override type Codec = OpenApi.Array[Of]

  def schema: Eval[Schema[?]]

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Collection.Of[B, Of] =
    Collection.Validate(this, validation, g)

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Array[?] => decode(openapi)
    case openapi                   => typeViolations("Array", openapi).invalid

  def decode(openapi: OpenApi.Array[?]): Validated[Violations, A]

object Collection:
  type Of[A, B <: OpenApi] = Collection[A] { type Of = B }

  final private case class Root[A, B <: OpenApi](
      description: Option[String],
      example: Option[Vector[A]],
      schema: Eval[Schema.Of[A, B]]
  ) extends Collection[Vector[A]]:
    override type Of = B
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Collection.Of[Vector[A], B] =
      copy(description = f(description))
    override def modifyExample(f: Option[Vector[A]] => Option[Vector[A]]): Collection.Of[Vector[A], B] =
      copy(example = f(example))
    override def decode(openapi: OpenApi.Array[?]): Validated[Violations, Vector[A]] =
      openapi.toVector.zipWithIndex.traverse: (openapi, index) =>
        schema.value.decode(openapi).leftMap(_.modifyHistory(index /: _))
    override def encode(as: Vector[A]): OpenApi.Array[B] = OpenApi.fromVector(as.map(schema.value.encode))

  final private case class Validate[A, B <: OpenApi, C](
      collection: Collection.Of[A, B],
      validation: Validation[A, A, A, C],
      g: C => A
  ) extends Collection[C]:
    override type Of = B
    override def example: Option[C] = collection.example.flatMap(validation.run(_).toOption)
    override def schema: Eval[Schema[?]] = collection.schema
    override def constraints: Chain[Constraint[OpenApi]] =
      collection.constraints ++ validation.constraints.map(_.map(collection.encode))
    override def description: Option[String] = collection.description
    override def modifyDescription(f: Option[String] => Option[String]): Collection.Of[C, B] =
      copy(collection = collection.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Collection.Of[C, B] =
      copy(collection = collection.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
    override def decode(openapi: OpenApi.Array[?]): Validated[Violations, C] =
      collection.decode(openapi).andThen(andThenValidate(validation, collection.encode))
    override def encode(c: C): OpenApi.Array[B] = collection.encode(g(c))

  def apply[A, B <: OpenApi](schema: Eval[Schema.Of[A, B]]): Collection.Of[Vector[A], B] =
    Root(none, none, schema)
