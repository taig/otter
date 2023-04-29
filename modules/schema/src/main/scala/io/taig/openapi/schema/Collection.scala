package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Collection[A](
    val constraints: Chain[Constraint[OpenApi]],
    val description: Option[String],
    val example: Option[A],
    val schema: Eval[Schema[?]]
) extends Schema[A]:
  self =>

  type Of <: OpenApi
  final override type Self[a] = Collection.Of[a, Of]
  final override type Codec = OpenApi.Array[Of]

  override def modifyDescription(f: Option[String] => Option[String]): Collection.Of[A, Of] =
    new Collection[A](constraints, f(description), example, schema) { export self.{decode, encode, Of} }

  override def modifyExample(f: Option[A] => Option[A]): Collection.Of[A, Of] =
    new Collection[A](constraints, description, f(example), schema) { export self.{decode, encode, Of} }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Collection.Of[B, Of] =
    new Collection[B](
      constraints ++ validation.constraints.map(_.map(self.encode)),
      description,
      example.flatMap(validation.run(_).toOption),
      schema
    ):
      override type Of = self.Of
      override def decode(openapi: OpenApi.Array[?]): Validated[Violations, B] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: B): OpenApi.Array[self.Of] = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Array[?] => decode(openapi)
    case openapi                   => typeViolations("Array", openapi).invalid

  def decode(openapi: OpenApi.Array[?]): Validated[Violations, A]

object Collection:
  type Of[A, B <: OpenApi] = Collection[A] { type Of = B }

  def apply[A, B <: OpenApi](of: Eval[Schema.Of[A, B]]): Collection.Of[Vector[A], B] =
    new Collection[Vector[A]](Chain.empty, none, none, of):
      override type Of = B
      override def decode(openapi: OpenApi.Array[?]): Validated[Violations, Vector[A]] =
        openapi.toVector.zipWithIndex.traverse: (openapi, index) =>
          of.value.decode(openapi).leftMap(_.modifyHistory(index /: _))
      override def encode(as: Vector[A]): OpenApi.Array[B] = OpenApi.fromVector(as.map(of.value.encode))
