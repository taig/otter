package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.validation.{identifiers, Constraint, Validation, Violation}

sealed abstract class Collection[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val name: Option[String],
    val schema: Eval[Schema[?]]
) extends Value[A]:
  self =>

  type Of <: OpenApi
  final override type Self[a] = Collection.Of[a, Of]
  final override type Codec = OpenApi.Array[Of]

  override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Collection.Of[A, Of] =
    new Collection[A](constraints, default, description, example, name, schema):
      export self.{decode, encode, Of}

  override def imap[B](f: A => B)(g: B => A): Collection.Of[B, Of] =
    new Collection[B](constraints, default.map(f), description, example.map(f), name, schema):
      export self.Of
      override def decode(openapi: OpenApi.Array[?]): Validated[Violations, B] = self.decode(openapi).map(f)
      override def encode(b: B): OpenApi.Array[Of] = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Collection.Of[C, Of] =
    new Collection[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      example.flatMap(validation.run(_).toOption),
      name,
      schema
    ):
      export self.Of
      override def decode(openapi: OpenApi.Array[?]): Validated[Violations, C] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: C): OpenApi.Array[self.Of] = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null              => default.toValid(nonNullViolations("Array"))
    case openapi: OpenApi.Array[?] => decode(openapi)
    case openapi                   => typeViolations("Array", openapi).invalid

  def decode(openapi: OpenApi.Array[?]): Validated[Violations, A]

  override def encode(a: A): OpenApi.Array[Of]

object Collection:
  type Of[A, B <: OpenApi] = Collection[A] { type Of = B }

  def apply[A, B <: OpenApi](of: Eval[Schema.Of[A, B]]): Collection.Of[Chain[A], B] =
    new Collection[Chain[A]](Chain.empty, none, none, none, none, of):
      override type Of = B

      override def decode(openapi: OpenApi.Array[?]): Validated[Violations, Chain[A]] =
        openapi.toChain.zipWithIndex.traverse { (openapi, index) =>
          of.value.decode(openapi).leftMap(_.modifyHistory(index /: _))
        }

      override def encode(as: Chain[A]): OpenApi.Array[B] = OpenApi.fromVector(as.map(of.value.encode).toVector)
