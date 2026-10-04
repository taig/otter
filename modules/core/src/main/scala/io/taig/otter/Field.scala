package io.taig.otter

import cats.Eval
import cats.arrow.Profunctor

/** A named member of a [[Record]]. `F` is the type of the schema the field holds. */
sealed trait Field[+F[-_, +_], -W, +R]:
  def name: String

  def isOptional: Boolean

  def schema: Reference[F, ?, ?]

object Field:
  /** The wire forms that supply an absent value, independently of the payload's own empty values. */
  enum Absent:
    case Missing, Empty, MissingOrEmpty

    def acceptsMissing: Boolean = this != Empty

    def matches[T](value: Option[T], isEmpty: T => Boolean): Boolean = this match
      case Missing        => value.isEmpty
      case Empty          => value.exists(isEmpty)
      case MissingOrEmpty => value.forall(isEmpty)

  /** A complete read/write contract. The first form in a lenient contract is its written form. */
  enum Presence(val absent: Absent, val writesEmpty: Boolean):
    case Omitted extends Presence(Absent.Missing, false)
    case Empty extends Presence(Absent.Empty, true)
    case OmittedOrEmpty extends Presence(Absent.MissingOrEmpty, false)
    case EmptyOrMissing extends Presence(Absent.MissingOrEmpty, true)

  private def contract(field: Field[?, ?, ?]): Option[String] = field match
    case Root(_, _)            => None
    case Modify(self, _, _)    => contract(self)
    case Optional(_, presence) => Some(s"optional($presence)")
    case Default(_, _, absent) => Some(s"defaulted($absent)")

  private def check(field: Field[?, ?, ?], requested: String): Unit =
    require(
      contract(field).isEmpty,
      s"Field '${field.name}' already has ${contract(field).getOrElse("")}; cannot apply $requested. " +
        "Choose one field absence contract. Put payload nullability inside field(name, schema.nullable)."
    )

  /** Whether the wire name must be present, on the specified side. */
  def required(field: Field[?, ?, ?], side: Side): Boolean = field match
    case Root(_, _)            => true
    case Modify(self, _, _)    => required(self, side)
    case Optional(_, presence) =>
      side match
        case Side.Read  => !presence.absent.acceptsMissing
        case Side.Write => presence.writesEmpty
    case Default(_, _, absent) => side == Side.Write || !absent.acceptsMissing

  final case class Root[F[-_, +_], W, R](name: String, reference: Reference[F, W, R]) extends Field[F, W, R]:
    override def isOptional: Boolean = false

    override def schema: Reference[F, ?, ?] = reference

  final case class Optional[F[-_, +_], W, R](self: Field[F, W, R], presence: Presence)
      extends Field[F, Option[W], Option[R]]:
    check(self, s"optional($presence)")
    export self.{name, schema}

    override def isOptional: Boolean = true

  final case class Default[F[-_, +_], W, R](self: Field[F, W, R], value: Eval[R], absent: Absent)
      extends Field[F, W, R]:
    check(self, s"defaulted($absent)")
    export self.{name, schema}

    override def isOptional: Boolean = true

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Field[F, W0, R0], f: R0 => R, g: W => W0)
      extends Field[F, W, R]:
    export self.{isOptional, name, schema}

  given [F[-_, +_]] => Profunctor[Field[F, *, *]]:
    override def dimap[W0, R0, W, R](self: Field[F, W0, R0])(f: W => W0)(g: R0 => R): Field[F, W, R] =
      Field.Modify(self, g, f)
