package io.taig.crock.schema

import cats.Eq
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import io.taig.crock.validation.{Constraint, Validation}

// TODO add null property
sealed abstract class Coproduct[A] extends Schema[A]:
  final override type Self[a] = Coproduct[a]

  def toNonEmptyChain: NonEmptyChain[Branch[?, ?]]

  abstract class Discriminators extends Property[Coproduct.Discriminator]:
    final def nested(identifier: String, value: String): Coproduct[A] =
      apply(Coproduct.Discriminator.Nested(identifier, value))
    final def merged(identifier: String): Coproduct[A] = apply(Coproduct.Discriminator.Merged(identifier))
    final def keyed: Coproduct[A] = apply(Coproduct.Discriminator.Keyed)
    final def none: Coproduct[A] = apply(Coproduct.Discriminator.None)

  object Discriminators:
    def apply(
        discriminator: Coproduct.Discriminator,
        g: (Coproduct.Discriminator => Coproduct.Discriminator) => Self[A]
    ): Discriminators = new Discriminators:
      override def value: Coproduct.Discriminator = discriminator
      override def modify(f: Coproduct.Discriminator => Coproduct.Discriminator): Self[A] = g(f)

  def discriminator: Discriminators

  final infix def orElse[B](other: Coproduct[B]): Coproduct[A + B] =
    Coproduct.OrElse(this, other, Coproduct.Properties.Empty)
  final def :+[B, C](branch: Branch[B, C]): Coproduct[A + C] = orElse(branch.toCoproduct)
  final def +:[B, C](branch: Branch[B, C]): Coproduct[C + A] = branch.toCoproduct.orElse(this)

  final override def optional: Coproduct[Option[A]] = Coproduct.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] =
    Coproduct.Validate(this, validation, g)
  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Coproduct[B] = imap(evidence.from)(evidence.to)

//  final override def decode(crock: OpenApi): Validated[Violations, B] = tryDecode(crock) match
//    case Ior.Left(violations) => violations.invalid
//    case Ior.Right(Some(b))   => b.valid
//    case Ior.Right(None) =>
//      renderDiscriminator(crock) match
//        case Some(discriminator) =>
//          val names = toNonEmptyChain.map(branch => OpenApi.fromString(branch.renderName)).toNonEmptyVector.toVector
//          Violations.rootNec(Constraint.collection.oneOf(OpenApi.Array(names)).toViolation(discriminator)).invalid
//        case None => typeViolations("Sum", crock).invalid
//    case Ior.Both(violations, b) => b.toValid(violations)
//  final private def renderDiscriminator(crock: OpenApi): Option[OpenApi.Primitive] = discriminator match
//    case Discriminator.Nested(identifier, _) =>
//      crock.asObject.flatMap(_.get(identifier)).flatMap(_.asPrimitive)
//    case Discriminator.Merged(identifier) =>
//      crock.asObject.flatMap(_.get(identifier)).flatMap(_.asPrimitive)
//    case Discriminator.Keyed => crock.asObject.flatMap(_.keys.headOption.map(OpenApi.fromString))
//    case Discriminator.None  => None

object Coproduct:
  extension [A <: Matchable](self: Coproduct[A])
    inline def |[B <: Matchable](other: Coproduct[B]): Coproduct[A | B] = self
      .orElse(other)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

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

  final private[crock] case class Properties[+A](
      description: Option[String],
      discriminator: Discriminator,
      example: Option[A]
  )

  object Properties:
    val Empty: Coproduct.Properties[Nothing] = Properties(None, Discriminator.Default, None)

  final private[crock] case class Root[A, B](
      branch: Branch[A, B],
      properties: Coproduct.Properties[B]
  ) extends Coproduct[B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def toNonEmptyChain: NonEmptyChain[Branch[A, B]] = NonEmptyChain.one(branch)
    override def isOptional: Boolean = false
    override def discriminator: Discriminators = Discriminators(
      properties.discriminator,
      f => copy(properties = properties.copy(discriminator = f(properties.discriminator)))
    )
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class OrElse[A, B](left: Coproduct[A], right: Coproduct[B], properties: Properties[A + B])
      extends Coproduct[A + B] {
    override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
    override def toNonEmptyChain: NonEmptyChain[Branch[?, ?]] = left.toNonEmptyChain ++ right.toNonEmptyChain
    override def isOptional: Boolean = left.isOptional && right.isOptional

    override def discriminator: Discriminators = Discriminators(
      properties.discriminator,
      f => copy(properties = properties.copy(discriminator = f(properties.discriminator)))
    )

    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[A + B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )
  }

//    override def tryDecode(crock: OpenApi): Ior[Violations, Option[B + C]] = left.tryDecode(crock) match
//      case Ior.Right(Some(b)) => b.asLeft.some.rightIor
//      case Ior.Right(None) =>
//        right.tryDecode(crock) match
//          case Ior.Left(right)    => right.leftIor
//          case Ior.Right(c)       => c.map(_.asRight).rightIor
//          case Ior.Both(right, c) => right.leftIor.putRight(c.map(_.asRight))
//      case Ior.Left(left)          => Ior.Left(left)
//      case Ior.Both(left, Some(b)) => left.leftIor.putRight(b.asLeft.some)
//      case Ior.Both(left, None) =>
//        right.tryDecode(crock) match
//          case Ior.Left(right)    => (left merge right).leftIor
//          case Ior.Right(c)       => left.leftIor.putRight(c.map(_.asRight))
//          case Ior.Both(right, c) => (left merge right).leftIor.putRight(c.map(_.asRight))

  final private[crock] case class Validate[A, B](self: Coproduct[A], validation: Validation[A, B], g: B => A)
      extends Coproduct[B]:
    export self.{isOptional, toNonEmptyChain}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def discriminator: Discriminators =
      Discriminators(self.discriminator.value, f => copy(self = self.discriminator.modify(f)))
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  final private[crock] case class Optional[A](self: Coproduct[A]) extends Coproduct[Option[A]]:
    export self.{constraints, toNonEmptyChain}
    override def isOptional: Boolean = true
    override def discriminator: Discriminators =
      Discriminators(self.discriminator.value, f => copy(self = self.discriminator.modify(f)))
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  def apply[A, B](branch: Branch[A, B]): Coproduct[B] = Root(branch, Properties.Empty)
