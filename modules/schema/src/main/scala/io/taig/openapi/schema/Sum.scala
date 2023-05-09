package io.taig.openapi.schema

import cats.Eq
import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation}

sealed abstract class Sum[A, B] extends Schema[B]:
  self =>

  final override type Self[a] = Sum[A, a]
  final override type Codec = OpenApi

  def branches: NonEmptyChain[Branch[A, ?]]
  def discriminator: Sum.Discriminator
  def modifyDiscriminator(f: Sum.Discriminator => Sum.Discriminator): Self[B]
  final def withDiscriminator(discriminator: Sum.Discriminator): Self[B] = modifyDiscriminator(_ => discriminator)
  final def withNestedDiscriminator(identifier: String, value: String): Self[B] =
    withDiscriminator(Sum.Discriminator.Nested(identifier, value))
  final def withMergedDiscriminator(identifier: String): Self[B] = withDiscriminator(
    Sum.Discriminator.Merged(identifier)
  )
  final def withKeyedDiscriminator: Self[B] = withDiscriminator(Sum.Discriminator.Keyed)
  final def withoutDiscriminator: Self[B] = withDiscriminator(Sum.Discriminator.None)

  final infix def orElse[C](sum: Sum[A, C]): Sum[A, B + C] =
    Sum.OrElse(this, sum.withDiscriminator(discriminator), none, none)

  final infix def :+[C](branch: Branch[A, C]): Sum[A, B + C] = orElse(branch.toSum)

  final override def ivalidate[C: Encoder, D](validation: Validation[C, B, B, D])(g: D => B): Sum[A, D] =
    Sum.Validate(this, validation, g)

  final def to[C](using evidence: Evidence.Sum.Aux[C, B]): Sum[A, C] = imap(evidence.from)(evidence.to)

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
    override def modifyDiscriminator(f: Sum.Discriminator => Sum.Discriminator): Sum[A, B] =
      copy(discriminator = f(discriminator))
    override def modifyExample(f: Option[B] => Option[B]): Sum[A, B] = copy(example = f(example))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]] =
      branch.decode(openapi, discriminator)
    override def encode(b: B): OpenApi = branch.encode(b, discriminator)

  final private case class OrElse[A, B, C](
      left: Sum[A, B],
      right: Sum[A, C],
      description: Option[String],
      example: Option[B + C]
  ) extends Sum[A, B + C]:
    override def constraints: Chain[Constraint[OpenApi]] = left.constraints ++ right.constraints
    override def discriminator: Discriminator = left.discriminator
    override def branches: NonEmptyChain[Branch[A, ?]] = left.branches ++ right.branches
    override def modifyDescription(f: Option[String] => Option[String]): Sum[A, B + C] =
      copy(description = f(description))
    override def modifyDiscriminator(f: Sum.Discriminator => Sum.Discriminator): Sum[A, B + C] =
      copy(left = left.modifyDiscriminator(f), right = right.modifyDiscriminator(f))
    override def modifyExample(f: Option[B + C] => Option[B + C]): Sum[A, B + C] = copy(example = f(example))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B + C]] =
      left.decodeOption(openapi).map(_.map(_.asLeft)).orElse(right.decodeOption(openapi).map(_.map(_.asRight)))
    override def encode(bc: B + C): OpenApi = bc.fold(left.encode, right.encode)

  final private case class Validate[A, B, C: Encoder, D](sum: Sum[A, B], validation: Validation[C, B, B, D], g: D => B)
      extends Sum[A, D]:
    override def branches: NonEmptyChain[Branch[A, ?]] = sum.branches
    override def constraints: Chain[Constraint[OpenApi]] =
      sum.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
    override def description: Option[String] = sum.description
    override def discriminator: Discriminator = sum.discriminator
    override def example: Option[D] = sum.example.flatMap(validation.run(_).toOption)
    override def modifyDescription(f: Option[String] => Option[String]): Sum[A, D] =
      copy(sum = sum.modifyDescription(f))
    override def modifyDiscriminator(f: Sum.Discriminator => Sum.Discriminator): Sum[A, D] =
      copy(sum = sum.modifyDiscriminator(f))
    override def modifyExample(f: Option[D] => Option[D]): Sum[A, D] =
      copy(sum = sum.modifyExample(b => f(b.flatMap(validation.run(_).toOption)).map(g)))
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[D]] =
      sum.decodeOption(openapi).andThen(_.traverse(applyValidation(validation, sum.encode)))
    override def encode(c: D): OpenApi = sum.encode(g(c))

  def apply[A, B](branch: Branch[A, B]): Sum[A, B] = Root(branch, none, Discriminator.Default, none)
