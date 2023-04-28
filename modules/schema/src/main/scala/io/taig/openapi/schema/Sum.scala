package io.taig.openapi.schema

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Sum[A, B](
    val branches: NonEmptyChain[Branch[A, ?]],
    val constraints: Chain[Constraint[OpenApi]],
    val metadata: Sum.Metadata[B]
) extends Schema[B]:
  self =>

  final override type Self[a] = Sum[A, a]
  final override type Codec = OpenApi
  final override type Metadata[a] = Sum.Metadata[a]

  object discriminator extends Attribute[Discriminator](metadata.discriminator):
    override def updated(f: Discriminator => Discriminator): Sum.Metadata[B] = metadata.copy(discriminator = f(value))

  final def orElse[C](sum: Sum[A, C]): Sum[A, B + C] = new Sum[A, B + C](
    branches ++ sum.branches,
    constraints ++ sum.constraints,
    Sum.Metadata(
      none,
      metadata.discriminator,
      metadata.example.map(_.asLeft).orElse(sum.metadata.example.map(_.asRight))
    )
  ):
    override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B + C]] =
      self.decodeOption(openapi).map(_.map(_.asLeft)).orElse(sum.decodeOption(openapi).map(_.map(_.asRight)))
    override def encode(bc: B + C): OpenApi = bc.fold(self.encode, sum.encode)

  final def :+[C](branch: Branch[A, C]): Sum[A, B + C] = orElse(branch.toSum)

  final override def copy(metadata: Sum.Metadata[B]): Sum[A, B] =
    new Sum[A, B](branches, constraints, metadata) { export self.{decodeOption, encode} }

  final override def ivalidate[C](validation: Validation[B, B, B, C])(g: C => B): Sum[A, C] = ???

  final override def decode(openapi: OpenApi): Validated[Violations, B] =
    decodeOption(openapi).andThen(_.toValid(typeViolations("Sum", openapi)))

  def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]]

object Sum:
  final case class Metadata[A](description: Option[String], discriminator: Discriminator, example: Option[A])
      extends Schema.Metadata[A]:
    override type Self[a] = Sum.Metadata[a]
    override def map[B](f: A => B): Sum.Metadata[B] = copy(example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Sum.Metadata[B] = copy(example = example.flatMap(f))
    override def updated(description: Option[String], example: Option[A]): Sum.Metadata[A] =
      Metadata(description, discriminator, example)

  object Metadata:
    def empty[A]: Sum.Metadata[A] = Metadata(none, Discriminator.Default, none)

  def apply[A, B](branch: Branch[A, B]): Sum[A, B] =
    new Sum[A, B](NonEmptyChain.one(branch), Chain.empty, Metadata.empty):
      override def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]] =
        branch.decode(openapi, metadata.discriminator)
      override def encode(b: B): OpenApi = branch.encode(b, metadata.discriminator)
