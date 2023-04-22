package io.taig.openapi.schema

import cats.Eval
import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.screening.{Constraint, Validation}

sealed abstract class Sum[A, B](
    val branches: NonEmptyChain[Branch[A, ?]],
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[B],
    val description: Option[String],
    val discriminator: Option[Discriminator],
    val example: Option[B],
    val name: Option[String]
) extends Value[B]:
  self =>

  override type Self[a] = Sum[A, a]
  override type Codec = OpenApi

  final def modifyDiscriminator(f: Option[Discriminator] => Option[Discriminator]): Sum[A, B] =
    copy(default, description, f(discriminator), example, name)

  final def copy(
      default: Option[B],
      description: Option[String],
      discriminator: Option[Discriminator],
      example: Option[B],
      name: Option[String]
  ): Sum[A, B] = new Sum[A, B](branches, constraints, default, description, discriminator, example, name):
    export self.{decodeOption, encode}

  final override def copy(
      default: Option[B],
      description: Option[String],
      example: Option[B],
      name: Option[String]
  ): Sum[A, B] = copy(default, description, discriminator, example, name)

  final def gimap[C](using evidence: Evidence.Sum.Aux[C, B]): Sum[A, C] = imap(evidence.from)(evidence.to)

  final override def imap[C](f: B => C)(g: C => B): Sum[A, C] =
    new Sum[A, C](branches, constraints, default.map(f), description, discriminator, example.map(f), name):
      override def decodeOption(
          openapi: OpenApi,
          discriminator: Option[Discriminator]
      ): Validated[Violations, Option[C]] =
        self.decodeOption(openapi, discriminator).map(_.map(f))
      override def encode(c: C, discriminator: Option[Discriminator]): OpenApi = self.encode(g(c), discriminator)

  final override def ivalidate[C: Encoder, D](validation: Validation[C, B, B, D])(g: D => B): Sum[A, D] =
    new Sum[A, D](
      branches,
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      discriminator,
      example.flatMap(validation.run(_).toOption),
      name
    ):
      override def decodeOption(
          openapi: OpenApi,
          discriminator: Option[Discriminator]
      ): Validated[Violations, Option[D]] = self
        .decodeOption(openapi, discriminator)
        .andThen(_.traverse(andThenValidate(validation, self.encode)))
      override def encode(c: D, discriminator: Option[Discriminator]): OpenApi = self.encode(g(c), discriminator)

  final infix def orElse[C](sum: Sum[A, C]): Sum[A, Either[B, C]] = new Sum[A, Either[B, C]](
    self.branches ++ sum.branches,
    self.constraints ++ sum.constraints,
    self.default.map(_.asLeft).orElse(sum.default.map(_.asRight)),
    none,
    self.discriminator |+| sum.discriminator,
    self.example.map(_.asLeft).orElse(sum.example.map(_.asRight)),
    none
  ):
    override def decodeOption(
        openapi: OpenApi,
        discriminator: Option[Discriminator]
    ): Validated[Violations, Option[Either[B, C]]] = self.decodeOption(openapi, discriminator).andThen {
      case Some(b) => b.asLeft[C].some.valid
      case None    => sum.decodeOption(openapi, discriminator).map(_.map(_.asRight))
    }

    override def encode(cb: Either[B, C], discriminator: Option[Discriminator]): OpenApi = cb match
      case Left(b)  => self.encode(b)
      case Right(c) => sum.encode(c)

  final def :+[C](branch: Branch[A, C]): Sum[A, Either[B, C]] = self orElse branch.toSum

  final override def decode(openapi: OpenApi): Validated[Violations, B] = openapi match
    case OpenApi.Null if discriminator.nonEmpty => default.toValid(nonNullViolations("Object"))
    case _ => decodeOption(openapi).andThen(_.toValid(typeViolations("Sum", openapi)))

  final def decodeOption(openapi: OpenApi): Validated[Violations, Option[B]] = decodeOption(openapi, discriminator)

  protected def decodeOption(openapi: OpenApi, discriminator: Option[Discriminator]): Validated[Violations, Option[B]]

  final override def encode(B: B): OpenApi = encode(B, discriminator)
  protected def encode(b: B, discriminator: Option[Discriminator]): OpenApi

object Sum:
  def one[A, B](branch: Branch[A, B]): Sum[A, B] = new Sum[A, B](
    NonEmptyChain.one(branch),
    Chain.empty,
    none,
    none,
    Discriminator.Default.some,
    none,
    none
  ):
    override def decodeOption(
        openapi: OpenApi,
        discriminator: Option[Discriminator]
    ): Validated[Violations, Option[B]] = branch.decode(openapi, discriminator)
    override def encode(b: B, discriminator: Option[Discriminator]): OpenApi = branch.encode(b, discriminator)

  def of[A, B](branch: Branch[A, B], schema: Eval[Schema[B]]): Sum[A, B] = new Sum[A, B](
    NonEmptyChain.one(branch),
    Chain.empty,
    none,
    none,
    Discriminator.Default.some,
    none,
    none
  ):
    override def decodeOption(
        openapi: OpenApi,
        discriminator: Option[Discriminator]
    ): Validated[Violations, Option[B]] = branch.decode(openapi, discriminator)
    override def encode(a: B, discriminator: Option[Discriminator]): OpenApi = schema.value.encode(a)
