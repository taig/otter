package io.taig.openapi.schema

import cats.Eq
import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Sum[A, B] extends Schema[B]:
  self =>

  final override type Self[a] = Sum[A, a]
  final override type Codec = OpenApi

  def branches: NonEmptyChain[Branch[A, ?]]
  def discriminator: Sum.Discriminator

  final def orElse[C](sum: Sum[A, C]): Sum[A, B + C] =
    Sum.OrElse(this, sum, none, discriminator, none)

  final def :+[C](branch: Branch[A, C]): Sum[A, B + C] = orElse(branch.toSum)

  final override def ivalidate[C](validation: Validation[B, B, B, C])(g: C => B): Sum[A, C] =
    Sum.Validate(this, validation, g)

  // TODO gimap / as

  final override def decode(openapi: OpenApi): Validated[Violations, B] =
    decodeOption(openapi).andThen(_.toValid(typeViolations("Sum", openapi)))

  def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]]

object Sum:
  enum Discriminator:
    case Nested(identifier: String, value: String)
    case Merged(identifier: String)
    case Keyed
    case None

  object Discriminator:
    object Nested:
      val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

    object Merged:
      val Default: Discriminator.Merged = Merged(identifier = "type")

    val Default: Discriminator = Nested.Default

    given Eq[Discriminator] = Eq.fromUniversalEquals

  final private case class Root[A, B](
      branch: Branch[A, B],
      description: Option[String],
      discriminator: Sum.Discriminator,
      example: Option[B]
  ) extends Sum[A, B]:
    override def branches: NonEmptyChain[Branch[A, ?]] = NonEmptyChain.one(branch)
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Sum[A, B] = copy(description = f(description))
    override def modifyExample(f: Option[B] => Option[B]): Sum[A, B] = copy(example = f(example))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]] =
      branch.decode(openapi, discriminator)
    override def encode(b: B): OpenApi = branch.encode(b, discriminator)

  final private case class OrElse[A, B, C](
      left: Sum[A, B],
      right: Sum[A, C],
      description: Option[String],
      discriminator: Sum.Discriminator,
      example: Option[B + C]
  ) extends Sum[A, B + C]:
    override def constraints: Chain[Constraint[OpenApi]] = left.constraints ++ right.constraints
    override def branches: NonEmptyChain[Branch[A, ?]] = left.branches ++ right.branches
    override def modifyDescription(f: Option[String] => Option[String]): Sum[A, B + C] =
      copy(description = f(description))
    override def modifyExample(f: Option[B + C] => Option[B + C]): Sum[A, B + C] = copy(example = f(example))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B + C]] =
      left.decodeOption(openapi).map(_.map(_.asLeft)).orElse(right.decodeOption(openapi).map(_.map(_.asRight)))
    override def encode(bc: B + C): OpenApi = bc.fold(left.encode, right.encode)

  final private case class Validate[A, B, C](sum: Sum[A, B], validation: Validation[B, B, B, C], g: C => B)
      extends Sum[A, C]:
    override def branches: NonEmptyChain[Branch[A, ?]] = sum.branches
    override def constraints: Chain[Constraint[OpenApi]] =
      sum.constraints ++ validation.constraints.map(_.map(sum.encode))
    override def description: Option[String] = sum.description
    override def discriminator: Discriminator = sum.discriminator
    override def example: Option[C] = sum.example.flatMap(validation.run(_).toOption)
    override def modifyDescription(f: Option[String] => Option[String]): Sum[A, C] =
      copy(sum = sum.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Sum[A, C] =
      copy(sum = sum.modifyExample(b => f(b.flatMap(validation.run(_).toOption)).map(g)))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[C]] =
      sum.decodeOption(openapi).andThen(_.traverse(andThenValidate(validation, sum.encode)))
    override def encode(c: C): OpenApi = sum.encode(g(c))

  def apply[A, B](branch: Branch[A, B]): Sum[A, B] = Root(branch, none, Discriminator.Default, none)
