package io.taig.otter.schema

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation}

sealed abstract class Coproduct[A] extends Schema[A]:
  self =>
  final override type Self[a] = Coproduct[a]
  final override type Codec = OpenApi.Value
  final override type Properties[a] = Coproduct.Properties[a]
  def toNonEmptyChain: NonEmptyChain[Branch[?]]

  final def discriminator = new Property[Discriminator]:
    override def value: Discriminator = properties.discriminator
    override def modify(f: Discriminator => Discriminator): Coproduct[A] = copy(properties.modifyDiscriminator(f))
    def nested(identifier: String, value: String): Coproduct[A] = apply(Discriminator.Nested(identifier, value))
    def merged(identifier: String): Coproduct[A] = apply(Discriminator.Merged(identifier))
    def keyed: Coproduct[A] = apply(Discriminator.Keyed)
    def none: Coproduct[A] = apply(Discriminator.None)

  final override def copy(properties: Coproduct.Properties[A]): Coproduct[A] = new Coproduct[A] with Copy(properties):
    export self.{decode, encode, toNonEmptyChain}

  final override def optional: Coproduct[Option[A]] = new Coproduct[Option[A]] with Optional:
    export self.toNonEmptyChain
    override def decode(
        openapi: Option[OpenApi.Value],
        discriminator: Discriminator
    ): Validated[Violations, Option[A]] = self.decode(openapi, discriminator).map(_.some)
    override def encode(a: Option[A], discriminator: Discriminator): Option[OpenApi.Value] =
      a.flatMap(self.encode(_, discriminator))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] = new Coproduct[B]
    with Validate[B](validation):
    export self.toNonEmptyChain
    override def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, B] =
      self.decode(openapi, discriminator).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B, discriminator: Discriminator): Option[OpenApi.Value] = self.encode(g(b), discriminator)

  def orElse[B](coproduct: Coproduct[B]): Coproduct[A + B] = new Coproduct[A + B]:
    override def toNonEmptyChain: NonEmptyChain[Branch[?]] = self.toNonEmptyChain `combine` coproduct.toNonEmptyChain
    override def isOptional: Boolean = self.isOptional && coproduct.isOptional
    override def properties: Coproduct.Properties[A + B] = Coproduct.Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, A + B] =
      ???
    override def encode(a: A + B, discriminator: Discriminator): Option[OpenApi.Value] = ???

  def :+[B](branch: Branch[B]): Coproduct[A + B] = orElse(branch.toCoproduct)
  def +:[B](branch: Branch[B]): Coproduct[B + A] = branch.toCoproduct.orElse(this)

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Coproduct[B] = imap(evidence.from)(evidence.to)

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] =
    decode(openapi, properties.discriminator)
  def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, A]

  final override def encode(a: A): Option[OpenApi.Value] = encode(a, properties.discriminator)
  def encode(a: A, discriminator: Discriminator): Option[OpenApi.Value]

object Coproduct:
  extension [A <: Matchable](self: Coproduct[A])
    inline def |[B <: Matchable](branch: Branch[B]): Coproduct[A | B] = (self :+ branch).imap[A | B] {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

  final case class Properties[+A](description: Option[String], discriminator: Discriminator, example: Option[A])
      extends Schema.Properties[A]:
    override type Self[a] = Coproduct.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Coproduct.Properties[A] =
      copy(description = f(description))
    def modifyDiscriminator(f: Discriminator => Discriminator): Coproduct.Properties[A] =
      copy(discriminator = f(discriminator))
    override def modifyExample[B](f: Option[A] => Option[B]): Coproduct.Properties[B] = copy(example = f(example))
    override def flatMap[B](f: A => Option[B]): Coproduct.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Coproduct.Properties[Nothing] = Properties(None, Discriminator.Default, None)

  def apply[A](branch: Branch[A]): Coproduct[A] = new Coproduct[A]:
    override def toNonEmptyChain: NonEmptyChain[Branch[A]] = NonEmptyChain.one(branch)
    override def properties: Coproduct.Properties[A] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, A] = ???
    override def encode(a: A, discriminator: Discriminator): Option[OpenApi.Value] = branch.encode(a, discriminator)
