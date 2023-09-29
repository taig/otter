package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, NonEmptyChain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Coproduct[A](description: Option[String], val discriminator: Discriminator)
    extends Schema[A](description):
  self =>
  final override type Self[a] = Coproduct[a]

  def toNonEmptyChain: NonEmptyChain[Branch[?]]

  final override def description(f: Option[String] => Option[String]): Coproduct[A] =
    Coproduct(this, f(description), discriminator)
  final def discriminator(f: Discriminator => Discriminator): Coproduct[A] =
    Coproduct(this, description, f(discriminator))

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Coproduct[B] = imap(evidence.from)(evidence.to)

  final override def optional: Coproduct[Option[A]] = new Coproduct[Option[A]](None, Discriminator.Default):
    export self.{constraints, toNonEmptyChain}
    override def isOptional: Boolean = true
    override def decode(
        data: Option[Data.Object],
        discriminator: Discriminator
    ): Validated[Violations, Option[Option[A]]] =
      data match
        case Some(_) => self.decode(data, discriminator).map(_.some)
        case None    => none.valid
    override def encode(a: Option[A], discriminator: Discriminator): Data =
      a.map(self.encode(_, discriminator)).getOrElse(Data.Null)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] =
    new Coproduct[B](description, discriminator):
      export self.{isOptional, toNonEmptyChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decode(data: Option[Data.Object], discriminator: Discriminator): Validated[Violations, Option[B]] =
        self.decode(data, discriminator).andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encode(b: B, discriminator: Discriminator): Data = self.encode(g(b), discriminator)

  final def orElse[B](schema: Coproduct[B]): Coproduct[Either[A, B]] =
    new Coproduct[Either[A, B]](None, Discriminator.Default):
      override def toNonEmptyChain: NonEmptyChain[Branch[?]] = self.toNonEmptyChain ++ schema.toNonEmptyChain
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decode(
          data: Option[Data.Object],
          discriminator: Discriminator
      ): Validated[Violations, Option[Either[A, B]]] = self
        .decode(data, discriminator)
        .map(_.map(_.asLeft))
        .andThen:
          case a @ Some(_) => a.valid
          case None        => schema.decode(data, discriminator).map(_.map(_.asRight))
      override def encode(ab: Either[A, B], discriminator: Discriminator): Data = ab.fold(self.encode, schema.encode)

  final def :+[B](branch: Branch[B]): Coproduct[Either[A, B]] = self.orElse(branch.toCoproduct)
  final def +:[B](branch: Branch[B]): Coproduct[Either[B, A]] = branch.toCoproduct.orElse(self)

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(obj: Data.Object) =>
      decode(data.asInstanceOf[Option[Data.Object]], discriminator).andThen:
        case Some(a) => a.valid
        case None =>
          val values = toNonEmptyChain.toChain.map(_.name)
          Violations.rootNec(Violation(Constraint.OneOf(values), actual = obj)).invalid
    case Some(data) => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
    case None =>
      decode(None, discriminator).andThen:
        case Some(a) => a.valid
        case None =>
          val values = toNonEmptyChain.toChain.map(_.name)
          Violations.rootNec(Violation(Constraint.OneOf(values), actual = data.getOrElse(Data.Null))).invalid
  def decode(data: Option[Data.Object], discriminator: Discriminator): Validated[Violations, Option[A]]

  final override def encode(a: A): Data = encode(a, discriminator)
  def encode(a: A, discriminator: Discriminator): Data

object Coproduct:
  def apply[A](schema: Coproduct[A], description: Option[String], discriminator: Discriminator): Coproduct[A] =
    new Coproduct[A](description, discriminator) { export schema.* }

  def apply[A](branch: Branch[A]): Coproduct[A] = new Coproduct[A](None, Discriminator.Default):
    override def toNonEmptyChain: NonEmptyChain[Branch[?]] = NonEmptyChain.one(branch)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Option[Data.Object], discriminator: Discriminator): Validated[Violations, Option[A]] =
      data match
        case Some(data) => branch.decode(data, discriminator)
        case None       => Violations.rootNec(Violation.required).invalid
    override def encode(a: A, discriminator: Discriminator): Data = branch.encode(a, discriminator)
