package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation}

import scala.collection.immutable.{SeqMap, VectorMap}

sealed abstract class Dictionary[A] extends Schema[A]:
  self =>

  final override type Self[a] = Dictionary[a]
  final override type Codec = OpenApi.Object

  def key: Eval[Value[?]]
  def schema: Eval[Schema[?]]

  final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Dictionary[C] =
    Dictionary.Validate(this, validation, g)

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Object => decode(openapi)
    case _                       => typeViolations("Object", openapi).invalid

  def decode(openapi: OpenApi.Object): Validated[Violations, A]

object Dictionary:
  final private case class Root[A, B](
      description: Option[String],
      example: Option[SeqMap[A, B]],
      key: Eval[Value[A]],
      schema: Eval[Schema[B]]
  ) extends Dictionary[SeqMap[A, B]]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Dictionary[SeqMap[A, B]] =
      copy(description = f(description))
    override def modifyExample(f: Option[SeqMap[A, B]] => Option[SeqMap[A, B]]): Dictionary[SeqMap[A, B]] =
      copy(example = f(example))
    override def decode(openapi: OpenApi.Object): Validated[Violations, SeqMap[A, B]] = openapi.toChain
      .traverse { case (k, v) =>
        (key.value.parse(k), schema.value.decode(v)).tupled.leftMap(_.modifyHistory(k /: _))
      }
      .map(chain => SeqMap.from(chain.iterator))
    override def encode(abs: SeqMap[A, B]): OpenApi.Object =
      OpenApi.Object(abs.map { case (k, v) => (key.value.encode(k).render, schema.value.encode(v)) }.to(VectorMap))

  final private case class Validate[A, B: Encoder, C](
      dictionary: Dictionary[A],
      validation: Validation[B, A, A, C],
      g: C => A
  ) extends Dictionary[C]:
    override def constraints: Chain[Constraint[OpenApi]] =
      dictionary.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
    override def description: Option[String] = dictionary.description
    override def example: Option[C] = dictionary.example.flatMap(validation.run(_).toOption)
    override def key: Eval[Value[?]] = dictionary.key
    override def schema: Eval[Schema[?]] = dictionary.schema
    override def modifyDescription(f: Option[String] => Option[String]): Dictionary[C] =
      copy(dictionary = dictionary.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Dictionary[C] =
      copy(dictionary = dictionary.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
    override def decode(openapi: OpenApi.Object): Validated[Violations, C] =
      dictionary.decode(openapi).andThen(andThenValidate(validation, dictionary.encode))
    override def encode(b: C): OpenApi.Object = dictionary.encode(g(b))

  def apply[A, B](key: Eval[Value[A]], schema: Eval[Schema[B]]): Dictionary[SeqMap[A, B]] =
    Root(none, none, key, schema)
